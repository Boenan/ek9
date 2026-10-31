;;; ek9-list-test.el --- Tests for ek9-list -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for resource list buffers.  The tests use a fake kubectl
;; script and do not need a cluster.

;;; Code:

(require 'ert)
(require 'ek9-list)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defmacro ek9-test--with-pods-buffer (&rest body)
  "Open the pods list with the fake kubectl, wait for it, and run BODY.
The pods buffer is current in BODY.  Kill it after BODY."
  (declare (indent 0))
  `(ek9-test--with-clean-context
     (let ((ek9-context-changed-hook (list #'ek9-list--context-changed))
           (buffer (ek9-list-open 'pods)))
       (unwind-protect
           (with-current-buffer buffer
             (ek9-test--wait-until (lambda () ek9-list--loaded))
             ,@body)
         (kill-buffer buffer)
         (when (timerp ek9-list--timer)
           (cancel-timer ek9-list--timer)
           (setq ek9-list--timer nil))))))

(defun ek9-test--row-ids ()
  "Return the IDs of the rows in the current list buffer."
  (mapcar #'car tabulated-list-entries))

(ert-deftest ek9-list-open-shows-pods ()
  (ek9-test--with-pods-buffer
    (should (eq major-mode 'ek9-list-mode))
    (should (equal (buffer-name) "*ek9 pods*"))
    (should (= (length tabulated-list-entries) 8))
    (should (member "default/api-5f9c-xyz" (ek9-test--row-ids)))
    (should (string-match-p "CrashLoopBackOff" (buffer-string)))
    (should (equal (car (aref tabulated-list-format 0)) "NAME"))
    (should (timerp ek9-list--timer))))

(ert-deftest ek9-list-tab-line-shows-state ()
  (ek9-test--with-pods-buffer
    (let ((line (substring-no-properties (ek9-list--tab-line))))
      (should (string-match-p "Context: dev" line))
      (should (string-match-p "Namespace: team-a" line))
      (should (string-match-p "View: pods \\[8\\]" line)))
    (setq ek9-list--error "100% broken")
    (should (string-match-p "Error: 100%% broken"
                            (substring-no-properties (ek9-list--tab-line))))))

(ert-deftest ek9-list-filter-matches-any-column ()
  (ek9-test--with-pods-buffer
    (ek9-list-filter "crashloop")
    (should (equal (ek9-test--row-ids) '("default/api-5f9c-xyz")))
    (ek9-list-filter "")
    (should (= (length tabulated-list-entries) 8))))

(ert-deftest ek9-list-filter-labels-and-annotations ()
  (ek9-test--with-pods-buffer
    ;; A label filter is a regexp on "KEY=VALUE".
    (ek9-list-filter "-l app=nginx")
    (should (equal (ek9-test--row-ids) '("default/nginx-7c5b8d65b8-abcde")))
    (ek9-list-filter "-l ^team=team-[ab]$")
    (should (equal (ek9-test--row-ids)
                   '("default/nginx-7c5b8d65b8-abcde" "default/api-5f9c-xyz")))
    ;; The match ignores case.
    (ek9-list-filter "-l APP=API")
    (should (equal (ek9-test--row-ids) '("default/api-5f9c-xyz")))
    ;; The key alone also matches.
    (ek9-list-filter "-a ^note=")
    (should (equal (ek9-test--row-ids) '("default/api-5f9c-xyz")))
    (ek9-list-filter "-a owner=.*@example\\.com")
    (should (= (length (ek9-test--row-ids)) 2))
    ;; A label filter does not look at annotations, and the reverse.
    (ek9-list-filter "-l owner")
    (should-not (ek9-test--row-ids))
    (should (string-match-p "No pods match the filter \"-l owner\""
                            (overlay-get ek9-list--empty-overlay 'after-string)))
    ;; A value with line breaks is one line.
    (ek9-list-filter "-a ^note=line one line two$")
    (should (equal (ek9-test--row-ids) '("default/api-5f9c-xyz")))
    ;; Spaces before the form and more spaces after it work.
    (ek9-list-filter "  -l   app=api")
    (should (equal (ek9-test--row-ids) '("default/api-5f9c-xyz")))
    ;; "-l" alone shows the rows with a label.
    (ek9-list-filter "-l")
    (should (= (length (ek9-test--row-ids)) 2))
    ;; Other text is a plain text filter on the columns, not a regexp.
    (ek9-list-filter "-q")
    (should-not (ek9-test--row-ids))
    (ek9-list-filter "nginx.*")
    (should-not (ek9-test--row-ids))
    (ek9-list-filter "")
    (should (= (length (ek9-test--row-ids)) 8))))

(ert-deftest ek9-list-filter-invalid-regexp ()
  (ek9-test--with-pods-buffer
    (ek9-list-filter "-l app=nginx")
    (should-error (ek9-list-filter "-l app=[") :type 'user-error)
    ;; The old filter stays.
    (should (equal ek9-list--filter "-l app=nginx"))
    ;; A saved view can have an invalid regexp.  Then no row matches.
    (setq ek9-list--filter "-a [")
    (ek9-list--render)
    (should-not (ek9-test--row-ids))))

(ert-deftest ek9-list-all-namespaces-adds-column ()
  (ek9-test--with-pods-buffer
    (setq ek9-context--namespace 'all)
    (ek9-list--render)
    (should (equal (car (aref tabulated-list-format 0)) "NAMESPACE"))
    (should (string-match-p "kube-system" (buffer-string)))))

(ert-deftest ek9-list-refresh-error-keeps-rows ()
  (ek9-test--with-pods-buffer
    (setq ek9-context--current "forbidden")
    (ek9-list-refresh)
    (ek9-test--wait-until (lambda () ek9-list--error))
    (should (string-match-p "Forbidden" ek9-list--error))
    (should (= (length tabulated-list-entries) 8))))

(ert-deftest ek9-list-ignores-old-results ()
  (ek9-test--with-pods-buffer
    (let ((old-generation ek9-list--generation))
      (ek9-list--reset)
      (ek9-list--handle (current-buffer) old-generation
                        #'ek9-list--receive
                        (ek9-test--table "pods"))
      (should (null tabulated-list-entries))
      (should-not ek9-list--loaded))))

(ert-deftest ek9-list-context-change-reloads ()
  (ek9-test--with-pods-buffer
    (let ((old-generation ek9-list--generation))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
        (ek9-test--switch-with-answer #'ek9-context-switch-namespace
                                      "kube-system"))
      (should (> ek9-list--generation old-generation)))
    (ek9-test--wait-until (lambda () ek9-list--loaded))
    (should (string-match-p "Namespace: kube-system"
                            (substring-no-properties (ek9-list--tab-line))))
    (should (= (length tabulated-list-entries) 8))))

(ert-deftest ek9-list-sort-by-restarts ()
  (ek9-test--with-pods-buffer
    (setq tabulated-list-sort-key (cons "RESTARTS" t))
    (tabulated-list-print)
    (should (equal (car (ek9-test--row-ids)) "default/api-5f9c-xyz"))))

(ert-deftest ek9-list-natural-sort ()
  (should (equal (ek9-list--duration-seconds "2d3h") (+ (* 2 86400) (* 3 3600))))
  (should-not (ek9-list--duration-seconds "<unknown>"))
  (should (equal (sort (list "web" "10" "1h" "2" "30m" "3 (5m ago)" "<none>" "")
                       #'ek9-list-natural-less)
                 '("30m" "1h" "2" "3 (5m ago)" "10" "" "<none>" "web")))
  ;; The order is transitive, also when a column mixes the classes.
  (let ((values '("30m" "1h" "2" "10" "x")))
    (dolist (a values)
      (dolist (b values)
        (dolist (c values)
          (when (and (ek9-list-natural-less a b) (ek9-list-natural-less b c))
            (should (ek9-list-natural-less a c))))))))

(ert-deftest ek9-list-resource-choices-include-aliases ()
  (let ((choices (ek9-list--resource-choices)))
    (should (eq (cdr (assoc "deployments" choices)) 'deployments))
    (should (eq (cdr (assoc "deploy" choices)) 'deployments))
    (should (eq (cdr (assoc "po" choices)) 'pods))))

(ert-deftest ek9-list-read-resource-empty-input ()
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
    (should-error (ek9-list--read-command) :type 'user-error)))

(defun ek9-test--open-with-answer (answer)
  "Run `ek9-list-select-resource' with ANSWER and wait for the rows.
Return the list buffer."
  (let (buffer)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) answer)))
      (setq buffer (call-interactively #'ek9-list-select-resource)))
    (with-current-buffer buffer
      (ek9-test--wait-until (lambda () ek9-list--loaded)))
    buffer))

(ert-deftest ek9-list-select-resource-by-alias ()
  (ek9-test--with-pods-buffer
    (let ((buffer (ek9-test--open-with-answer "deploy")))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal (buffer-name) "*ek9 deployments*"))
            (should (equal (ek9-test--row-ids) '("team-a/web")))
            (should (string-match-p "3/3" (buffer-string))))
        (kill-buffer buffer)))))

(ert-deftest ek9-list-events-newest-first ()
  (ek9-test--with-pods-buffer
    (let ((buffer (ek9-test--open-with-answer "ev")))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal tabulated-list-sort-key '("LAST SEEN")))
            (should (equal (ek9-test--row-ids)
                           '("team-a/new.2" "team-a/old.1"))))
        (kill-buffer buffer)))))

;;;; Drill-down

(defun ek9-test--goto-id (id)
  "Move point to the row with ID in the current list buffer."
  (goto-char (or (ek9-list--find-id id) (error "No row %s" id))))

(defun ek9-test--kill-list-buffers ()
  "Kill all list buffers and stop the refresh timer."
  (mapc #'kill-buffer (ek9-list--buffers))
  (when (timerp ek9-list--timer)
    (cancel-timer ek9-list--timer)
    (setq ek9-list--timer nil)))

(ert-deftest ek9-list-drill-down-deployment-to-pods ()
  (ek9-test--with-pods-buffer
    (let* ((deployments (ek9-test--open-with-answer "deploy"))
           replicasets pods)
      (unwind-protect
          (progn
            ;; Deployment -> its replica sets.
            (with-current-buffer deployments
              (ek9-test--goto-id "team-a/web")
              (setq replicasets (ek9-list-drill-down)))
            (with-current-buffer replicasets
              (ek9-test--wait-until (lambda () ek9-list--loaded))
              (should (equal (buffer-name)
                             "*ek9 replicasets team-a deployment/web*"))
              (should (eq ek9-list--parent deployments))
              ;; Only the replica sets that the deployment owns.
              (should (equal (mapcar #'car tabulated-list-entries)
                             '("team-a/web-7c5b8d65b8" "team-a/web-6d4f9c7b5a")))
              (should (string-match-p "Owner: deployment/web"
                                      (substring-no-properties
                                       (ek9-list--tab-line))))
              ;; Replica set -> its pods, by selector and by owner.
              (ek9-test--goto-id "team-a/web-7c5b8d65b8")
              (setq pods (ek9-list-drill-down)))
            (with-current-buffer pods
              (should (eq ek9-list--parent replicasets))
              (should (equal ek9-list--owner-uid "rs-1"))
              (should (equal (ek9-list--request-path)
                             (concat "/api/v1/namespaces/team-a/pods"
                                     "?labelSelector=app%3Dweb%2Cpod-template-hash%3D7c5b8d65b8"
                                     "&includeObject=Object")))
              (ek9-list-up))
            (with-current-buffer replicasets
              (ek9-list-up)
              (should (eq (current-buffer) deployments))
              (should (eq (window-buffer) deployments))))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-replicaset-faces ()
  (with-temp-buffer
    (ek9-list-mode)
    (setq ek9-list--resource (ek9-resource-get 'replicasets))
    (ek9-list--receive (ek9-test--table "replicasets"))
    (should (equal (mapcar (lambda (item)
                             (ek9-resource-replicaset-face item))
                           ek9-list--items)
                   '(ek9-status-ok ek9-status-done ek9-status-warning)))
    (should (string-match-p "web-7c5b8d65b8 +3 +3 +3" (buffer-string)))))

(ert-deftest ek9-list-drill-down-keeps-own-namespace ()
  (ek9-test--with-pods-buffer
    (let ((buffer (ek9-list-open 'pods
                                 '(:args ("--field-selector" "spec.nodeName=n1")
                                   :label "node=n1" :namespace all)
                                 (current-buffer))))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal (buffer-name) "*ek9 pods node=n1*"))
            (should (equal (ek9-list--request-path)
                           (concat "/api/v1/pods?fieldSelector=spec.nodeName%3Dn1"
                                   "&includeObject=Object")))
            (ek9-test--wait-until (lambda () ek9-list--loaded))
            (should (equal (car (aref tabulated-list-format 0)) "NAMESPACE"))
            (should (string-match-p
                     "Namespace: <all>"
                     (substring-no-properties (ek9-list--tab-line))))
            ;; A namespace switch does not change a drill-down buffer.
            (setq ek9-context--namespace "kube-system")
            (should (string-prefix-p "/api/v1/pods?" (ek9-list--request-path))))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-drill-down-namespace-switches ()
  (ek9-test--with-pods-buffer
    (let ((namespaces (ek9-test--open-with-answer "ns")))
      (unwind-protect
          (with-current-buffer namespaces
            (ek9-test--goto-id "kube-system")
            (let ((pods (ek9-list-drill-down)))
              (should (equal ek9-context--namespace "kube-system"))
              (should (equal (buffer-name pods) "*ek9 pods*"))
              (with-current-buffer pods
                (should (eq ek9-list--parent namespaces))
                (should-not ek9-list--namespace-override)
                (should (string-prefix-p "/api/v1/namespaces/kube-system/pods?"
                                         (ek9-list--request-path))))))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-drill-down-reuses-buffer ()
  (ek9-test--with-pods-buffer
    (let* ((top (current-buffer))
           (spec '(:args ("--selector" "app=web") :label "app=web"
                   :namespace "team-a"))
           (first (ek9-list-open 'pods spec top))
           (other (get-buffer-create "*ek9 test parent*")))
      (unwind-protect
          (let ((second (ek9-list-open 'pods spec other)))
            (should (eq first second))
            (should (eq (buffer-local-value 'ek9-list--parent second) other)))
        (kill-buffer other)
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-drill-down-keeps-wide-of-parent ()
  (ek9-test--with-pods-buffer
    (let ((top (current-buffer)))
      (unwind-protect
          (progn
            (should (eq (keymap-lookup ek9-list-mode-map "w") #'ek9-list-toggle-wide))
            (cl-letf (((symbol-function 'message) #'ignore))
              (ek9-list-toggle-wide))
            (should-not (buffer-local-value
                         'ek9-list--wide
                         (ek9-list-open 'pods '(:args ("--selector" "app=web")
                                                :label "app=web")
                                        top)))
            ;; A new top-level buffer uses the option.
            (should (buffer-local-value 'ek9-list--wide
                                        (ek9-list-open 'deployments))))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-context-switch-kills-drill-down ()
  (ek9-test--with-pods-buffer
    (let* ((top (current-buffer))
           (child (ek9-list-open 'pods '(:args ("--selector" "app=web")
                                         :label "app=web" :namespace "team-a")
                                 top)))
      (unwind-protect
          (progn
            (should (equal (buffer-local-value 'ek9-list--context child) "dev"))
            ;; A namespace switch keeps the drill-down buffer.
            (ek9-context-set-namespace "default")
            (should (buffer-live-p child))
            (should (eq (window-buffer) child))
            ;; A context switch kills it and shows the parent.
            (ek9-context--set "prod" "default")
            (should-not (buffer-live-p child))
            (should (eq (window-buffer) top))
            (should (buffer-live-p top)))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-up-after-namespace-drill-down-keeps-row ()
  (ek9-test--with-pods-buffer
    (let ((namespaces (ek9-test--open-with-answer "ns")))
      (unwind-protect
          (progn
            (with-current-buffer namespaces
              (ek9-test--goto-id "kube-system")
              (cl-letf (((symbol-function 'get-buffer-window)
                         (lambda (&rest _) t)))
                (ek9-list-drill-down)))
            (with-current-buffer namespaces
              (ek9-test--wait-until (lambda () ek9-list--loaded)))
            (with-current-buffer (window-buffer)
              (ek9-list-up))
            (should (eq (current-buffer) namespaces))
            (should (equal (tabulated-list-get-id) "kube-system")))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-drill-down-current-namespace-keeps-rows ()
  (ek9-test--with-pods-buffer
    (let ((namespaces (ek9-test--open-with-answer "ns"))
          (pods (current-buffer)))
      (unwind-protect
          (with-current-buffer namespaces
            (ek9-test--goto-id "team-a")
            (ek9-list-drill-down)
            (should (buffer-local-value 'ek9-list--loaded namespaces))
            (should (buffer-local-value 'ek9-list--loaded pods)))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-owner-filter ()
  (ek9-test--with-pods-buffer
    (let ((buffer (ek9-list-open 'pods
                                 '(:args ("--selector" "app=db") :label "app=db"
                                   :namespace "a" :owner-uid "u1"
                                   :owner "statefulset/db")
                                 (current-buffer))))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal (buffer-name) "*ek9 pods a statefulset/db app=db*"))
            (ek9-list--receive
             (ek9-test--name-table
              "{\"items\": [
                 {\"metadata\": {\"name\": \"db-0\", \"namespace\": \"a\",
                   \"ownerReferences\": [{\"uid\": \"u1\", \"controller\": true}]}},
                 {\"metadata\": {\"name\": \"other\", \"namespace\": \"a\",
                   \"ownerReferences\": [{\"uid\": \"u2\", \"controller\": true}]}},
                 {\"metadata\": {\"name\": \"lone\", \"namespace\": \"a\"}}]}"))
            (should (equal (ek9-test--row-ids) '("a/db-0")))
            (should (string-match-p "Owner: statefulset/db"
                                    (substring-no-properties
                                     (ek9-list--tab-line)))))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-namespace-switch-blocked-in-drill-down ()
  (ek9-test--with-pods-buffer
    (let ((buffer (ek9-list-open 'pods '(:args ("--selector" "app=web")
                                         :label "app=web" :namespace "team-a")
                                 (current-buffer))))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal (cadr (should-error (ek9-list-switch-namespace)
                                               :type 'user-error))
                           (concat "ek9: This view always shows namespace "
                                   "team-a.  Press ^ to go back"))))
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-drill-down-keeps-one-child ()
  (ek9-test--with-pods-buffer
    (let ((deployments (ek9-test--open-with-answer "deploy"))
          first second)
      (unwind-protect
          (progn
            (with-current-buffer deployments
              (ek9-test--goto-id "team-a/web")
              (setq first (ek9-list-drill-down)))
            (with-current-buffer first
              (ek9-list-up))
            (with-current-buffer deployments
              ;; Change the name, so that RET opens a different buffer.
              (setcdr (assq 'name (alist-get 'metadata
                                             (ek9-list-item-at-point)))
                      "web-2")
              (setq second (ek9-list-drill-down)))
            (should-not (eq first second))
            (should-not (buffer-live-p first))
            (should (eq (buffer-local-value 'ek9-list--child deployments)
                        second)))
        (ek9-test--kill-list-buffers)))))

(defun ek9-test--drill-twice (setup)
  "Drill down from deployments two times with different owners.
Call SETUP with the first child before the second drill-down.
Return the first child."
  (let ((deployments (ek9-test--open-with-answer "deploy"))
        first)
    (with-current-buffer deployments
      (ek9-test--goto-id "team-a/web")
      (setq first (ek9-list-drill-down)))
    (with-current-buffer first
      (ek9-list-up))
    (funcall setup first)
    (with-current-buffer deployments
      (setcdr (assq 'name (alist-get 'metadata (ek9-list-item-at-point)))
              "web-2")
      (ek9-list-drill-down))
    first))

(ert-deftest ek9-list-drill-down-keeps-visible-child ()
  (ek9-test--with-pods-buffer
    (unwind-protect
        (let ((first (ek9-test--drill-twice
                      (lambda (first)
                        (set-window-buffer (split-window) first)))))
          (should (buffer-live-p first)))
      (delete-other-windows)
      (ek9-test--kill-list-buffers))))

(ert-deftest ek9-list-drill-down-keeps-child-of-other-parent ()
  (ek9-test--with-pods-buffer
    (let ((other (get-buffer-create "*ek9 test parent*")))
      (unwind-protect
          (let ((first (ek9-test--drill-twice
                        (lambda (first)
                          (with-current-buffer first
                            (setq ek9-list--parent other))))))
            (should (buffer-live-p first)))
        (kill-buffer other)
        (ek9-test--kill-list-buffers)))))

(ert-deftest ek9-list-drill-down-errors ()
  (ek9-test--with-pods-buffer
    (should-error (ek9-list-up) :type 'user-error)
    ;; Events have no level below.
    (setq ek9-list--resource (ek9-resource-get 'events))
    (ek9-list--receive (ek9-test--table "events"))
    (goto-char (point-min))
    (should-error (ek9-list-drill-down) :type 'user-error)))

(ert-deftest ek9-list-drill-down-keys ()
  (should (eq (keymap-lookup ek9-list-mode-map "RET") #'ek9-list-drill-down))
  (should (eq (keymap-lookup ek9-list-mode-map "^") #'ek9-list-up)))

(provide 'ek9-list-test)

;;; ek9-list-test.el ends here
