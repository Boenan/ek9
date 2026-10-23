;;; ek9-containers-test.el --- Tests for the ek9 container list -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the drill-down from a pod to its containers, and
;; from a cron job to its jobs.  The pod fixture "pods-web-1.json" has
;; an init container, a sidecar, and two normal containers.

;;; Code:

(require 'ert)
(require 'ek9)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--web-1 ()
  "Return the pod in the fixture pods-web-1.json."
  (ek9-test--read-fixture "pods-web-1.json"))

(defmacro ek9-test--in-containers (&rest body)
  "Drill down to the containers of pod web-1 and run BODY in that list.
Kill all list buffers and stop the timer after BODY."
  (declare (indent 0))
  `(ek9-test--with-clean-context
     (ek9-test--load-contexts)
     (unwind-protect
         (let ((buffer (ek9-list-open
                        'containers (ek9-resource-drill-pod (ek9-test--web-1)))))
           (with-current-buffer buffer
             (ek9-test--wait-until (lambda () ek9-list--loaded))
             ,@body))
       (mapc #'kill-buffer (ek9-list--buffers))
       (when (timerp ek9-list--timer)
         (cancel-timer ek9-list--timer)
         (setq ek9-list--timer nil)))))

(defun ek9-test--container-row (name)
  "Return the alist of (TITLE . VALUE) of the container NAME."
  (let ((item (seq-find (lambda (item) (equal (ek9-resource-item-name item) name))
                        (ek9-resource-container-items (ek9-test--web-1)))))
    (mapcar (lambda (column)
              (cons (car column) (ek9-resource-column-string column item)))
            (ek9-resource-columns (ek9-resource-get 'containers)))))

(defun ek9-test--interactive-args (command)
  "Return the arguments that the interactive form of COMMAND gives."
  (advice-eval-interactive-spec (cadr (interactive-form command))))

(ert-deftest ek9-containers-items ()
  (let ((items (ek9-resource-container-items (ek9-test--web-1))))
    (should (equal (mapcar #'ek9-resource-item-name items)
                   '("migrate" "proxy" "app" "worker")))
    (should (equal (mapcar (lambda (item) (alist-get 'ek9-type item)) items)
                   '("init" "sidecar" "container" "container")))
    (should (equal (ek9-resource-item-id (car items)) "team-a/migrate"))))

(ert-deftest ek9-containers-columns ()
  (let ((app (ek9-test--container-row "app"))
        (worker (ek9-test--container-row "worker"))
        (migrate (ek9-test--container-row "migrate")))
    (should (equal (cdr (assoc "IMAGE" app)) "web:2"))
    (should (equal (cdr (assoc "READY" app)) "true"))
    (should (equal (cdr (assoc "STATE" app)) "Running"))
    (should (equal (cdr (assoc "PORTS" app)) "8080/TCP,8125/UDP"))
    (should (equal (cdr (assoc "RESTARTS" app)) "0"))
    (should (equal (cdr (assoc "STATE" worker)) "CrashLoopBackOff"))
    (should (string-match-p "\\`4 (.* ago)\\'" (cdr (assoc "RESTARTS" worker))))
    (should (equal (cdr (assoc "STATE" migrate)) "Completed"))
    (should (equal (cdr (assoc "PORTS" migrate)) ""))))

(ert-deftest ek9-containers-faces ()
  (let ((faces (mapcar (lambda (item)
                         (cons (ek9-resource-item-name item)
                               (ek9-resource-container-face item)))
                       (ek9-resource-container-items (ek9-test--web-1)))))
    (should (equal faces '(("migrate" . ek9-status-done)
                           ("proxy" . ek9-status-ok)
                           ("app" . ek9-status-ok)
                           ("worker" . ek9-status-error))))))

(ert-deftest ek9-containers-drill-down-from-pod ()
  (ek9-test--in-containers
    (should (equal (buffer-name) "*ek9 containers team-a pod/web-1*"))
    (should (equal (ek9-list--request-path)
                   "/api/v1/namespaces/team-a/pods/web-1"))
    (should (equal (mapcar #'car tabulated-list-entries)
                   '("team-a/migrate" "team-a/proxy" "team-a/app"
                     "team-a/worker")))
    (should (string-match-p "Owner: pod/web-1"
                            (substring-no-properties (ek9-list--tab-line))))))

(ert-deftest ek9-containers-hidden-from-command ()
  (should-not (assoc "containers" (ek9-list--resource-choices)))
  (should (assoc "pods" (ek9-list--resource-choices))))

(ert-deftest ek9-containers-actions-use-pod ()
  (ek9-test--in-containers
    (goto-char (ek9-list--find-id "team-a/worker"))
    (pcase-let ((`(,resource ,object ,container)
                 (ek9-list-target-for-action 'logs)))
      (should (eq (ek9-resource-name resource) 'pods))
      (should (equal (ek9-resource-item-name object) "web-1"))
      (should (equal container "worker")))
    ;; Logs: no question about the container.
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "Unexpected question"))))
      (should (equal (cadr (ek9-test--interactive-args #'ek9-logs)) "worker")))
    ;; Describe and YAML: the pod.
    (let (args)
      (cl-letf (((symbol-function 'ek9-view-open)
                 (lambda (&rest a) (setq args a))))
        (call-interactively #'ek9-describe))
      (should (equal (car args) "*ek9 describe pods team-a/web-1*")))
    ;; Edit and delete are not available for a container.
    (should-error (call-interactively #'ek9-delete) :type 'user-error)))

(ert-deftest ek9-containers-port-forward-offers-container-ports ()
  (ek9-test--in-containers
    (goto-char (ek9-list--find-id "team-a/app"))
    (let (choices)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest args)
                   (setq choices collection)
                   (nth 4 args))))
        (pcase-let ((`(,resource ,item ,ports)
                     (ek9-test--interactive-args #'ek9-port-forward)))
          (should (eq (ek9-resource-name resource) 'pods))
          (should (equal (ek9-resource-item-name item) "web-1"))
          (should (equal ports "8080:8080"))))
      (should (equal choices '("8080:8080" "8125:8125"))))))

(ert-deftest ek9-containers-shell-uses-container ()
  (ek9-test--in-containers
    (goto-char (ek9-list--find-id "team-a/app"))
    (should (equal (cadr (ek9-test--interactive-args #'ek9-shell)) "app"))))

(ert-deftest ek9-containers-not-a-saved-view ()
  (ek9-test--in-containers
    (should-error (ek9-saved-view-save "c") :type 'user-error)))

(ert-deftest ek9-cronjob-drill-down-to-jobs ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (unwind-protect
        (let ((buffer (ek9-list-open
                       'jobs
                       (ek9-resource-drill-cronjob
                        '((kind . "CronJob")
                          (metadata (name . "backup") (namespace . "team-a")
                                    (uid . "cj-1")))))))
          (with-current-buffer buffer
            (ek9-test--wait-until (lambda () ek9-list--loaded))
            (should (equal (buffer-name) "*ek9 jobs team-a cronjob/backup*"))
            ;; Only the job that the cron job owns.
            (should (equal (mapcar #'car tabulated-list-entries)
                           '("team-a/backup-1")))
            (should-error (ek9-saved-view-save "j") :type 'user-error)))
      (mapc #'kill-buffer (ek9-list--buffers))
      (when (timerp ek9-list--timer)
        (cancel-timer ek9-list--timer)
        (setq ek9-list--timer nil)))))

;;;; Events of an item

(ert-deftest ek9-events-of-namespaced-item ()
  (should (equal (ek9-resource-events-of
                  '((kind . "Pod")
                    (metadata (name . "web-1") (namespace . "team-a")
                              (uid . "u-1"))))
                 '(:resource events
                   :args ("--field-selector"
                          "involvedObject.name=web-1,involvedObject.kind=Pod,involvedObject.uid=u-1")
                   :label "pod/web-1"
                   :namespace "team-a"))))

(ert-deftest ek9-events-of-cluster-item ()
  ;; The kubelet writes node events with the node name as the UID, so
  ;; ek9 does not filter by the real UID.
  (let ((spec (ek9-resource-events-of
               '((kind . "Node") (metadata (name . "n1") (uid . "real-uid"))))))
    (should (eq (plist-get spec :namespace) 'all))
    (should (equal (cadr (plist-get spec :args))
                   "involvedObject.name=n1,involvedObject.kind=Node"))))

(ert-deftest ek9-events-key-from-container ()
  (ek9-test--in-containers
    (let ((containers (current-buffer)))
      (goto-char (ek9-list--find-id "team-a/app"))
      (let ((events (ek9-list-events)))
        (with-current-buffer events
          (should (equal (buffer-name) "*ek9 events team-a pod/web-1*"))
          (should (eq ek9-list--parent containers))
          (should (string-search
                   (url-hexify-string
                    (concat "involvedObject.name=web-1,involvedObject.kind=Pod,"
                            "involvedObject.uid=pod-uid-1"))
                   (ek9-list--request-path)))
          (should (string-match-p "Selector: pod/web-1"
                                  (substring-no-properties
                                   (ek9-list--tab-line)))))
        (should (eq (buffer-local-value 'ek9-list--child containers) events))))))

(ert-deftest ek9-events-key-availability ()
  (should (eq (keymap-lookup ek9-list-mode-map "E") #'ek9-list-events))
  (should-not (memq 'events (ek9-resource-actions (ek9-resource-get 'events))))
  (dolist (name '(pods containers deployments replicasets nodes namespaces))
    (should (memq 'events (ek9-resource-actions (ek9-resource-get name))))))

;;;; More cases

(ert-deftest ek9-containers-ephemeral ()
  (let* ((pod (ek9-test--web-1))
         (spec (alist-get 'spec pod)))
    (setf (alist-get 'ephemeralContainers spec)
          '(((name . "debugger") (image . "busybox"))))
    (setf (alist-get 'spec pod) spec)
    (let ((last (car (last (ek9-resource-container-items pod)))))
      (should (equal (ek9-resource-item-name last) "debugger"))
      (should (equal (alist-get 'ek9-type last) "ephemeral")))))

(ert-deftest ek9-containers-shell-needs-running-container ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (should (equal (cadr (should-error (ek9-shell (ek9-test--web-1) "migrate")
                                       :type 'user-error))
                   "ek9: Container migrate is not running"))
    (should-error (ek9-shell (ek9-test--web-1) "worker") :type 'user-error)))

(ert-deftest ek9-containers-not-in-saved-view ()
  (let ((ek9-saved-views '(("c" :resource containers))))
    (should-error (ek9-saved-view--get "c") :type 'user-error)))

(ert-deftest ek9-containers-pod-not-found ()
  (ek9-test--in-containers
    (let ((rows tabulated-list-entries))
      (setq ek9-list--args '("gone"))
      (ek9-list-refresh)
      (ek9-test--wait-until (lambda () ek9-list--error))
      (should (equal tabulated-list-entries rows))
      (should (string-match-p "Error:" (substring-no-properties
                                        (ek9-list--tab-line)))))))

(ert-deftest ek9-containers-menu-keys ()
  (ek9-test--in-containers
    (goto-char (ek9-list--find-id "team-a/app"))
    (let ((inapt (seq-remove
                  (lambda (key)
                    (let* ((plist (nth 2 (transient-get-suffix 'ek9-dispatch key)))
                           (if-not (plist-get plist :inapt-if-not)))
                      (or (null if-not) (funcall if-not))))
                  '("RET" "e" "D" "S" "R" "x" "l" "s" "f" "d" "y" "E"))))
      (should (equal inapt '("RET" "e" "D" "S" "R" "x"))))))

(ert-deftest ek9-events-key-from-pod-row ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (unwind-protect
        (with-current-buffer (ek9-list-open 'pods)
          (ek9-test--wait-until (lambda () ek9-list--loaded))
          (goto-char (ek9-list--find-id "default/api-5f9c-xyz"))
          (with-current-buffer (ek9-list-events)
            (should (equal (buffer-name)
                           "*ek9 events default pod/api-5f9c-xyz*"))))
      (mapc #'kill-buffer (ek9-list--buffers))
      (when (timerp ek9-list--timer)
        (cancel-timer ek9-list--timer)
        (setq ek9-list--timer nil)))))

(provide 'ek9-containers-test)

;;; ek9-containers-test.el ends here
