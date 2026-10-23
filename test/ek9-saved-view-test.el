;;; ek9-saved-view-test.el --- Tests for ek9 saved views -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for saved views.  The tests use the fake kubectl and the
;; fake kubeconfig, with the contexts "dev" and "prod".

;;; Code:

(require 'ert)
(require 'ek9)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defmacro ek9-test--with-views (views &rest body)
  "Run BODY with the saved VIEWS in context dev and namespace team-a.
Kill all list buffers and stop the timer after BODY."
  (declare (indent 1))
  `(ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (let ((ek9-saved-views ,views)
          (ek9-context-changed-hook (list #'ek9-list--context-changed)))
      (unwind-protect
          (progn ,@body)
        (mapc #'kill-buffer (ek9-list--buffers))
        (when (timerp ek9-list--timer)
          (cancel-timer ek9-list--timer)
          (setq ek9-list--timer nil))))))

(defun ek9-test--open-view (name)
  "Open the saved view NAME, wait for its rows, and return its buffer."
  (let ((buffer (ek9-saved-view-open name)))
    (with-current-buffer buffer
      (ek9-test--wait-until (lambda () ek9-list--loaded)))
    buffer))

(defconst ek9-test--views
  '(("argo" :resource pods :namespace "argocd" :filter "nginx")
    ("here" :resource pods)
    ("prod-nodes" :resource nodes :context "prod")
    ("broken" :resource gadgets))
  "Saved views for the tests.")

(ert-deftest ek9-saved-view-opens-own-buffer ()
  (ek9-test--with-views ek9-test--views
                        (with-current-buffer (ek9-test--open-view "argo")
                          (should (equal (buffer-name) "*ek9 view argo*"))
                          (should (equal ek9-list--filter "nginx"))
                          (should (equal (ek9-list--namespace) "argocd"))
                          (should (string-prefix-p "/api/v1/namespaces/argocd/pods?"
                                                   (ek9-list--request-path)))
                          (should (string-match-p "Namespace: argocd .*Saved view: argo"
                                                  (substring-no-properties (ek9-list--tab-line)))))
                        ;; The current namespace does not change.
                        (should (equal (ek9-context-namespace) "team-a"))
                        (should-not (get-buffer "*ek9 pods*"))))

(ert-deftest ek9-saved-view-without-namespace-follows-current ()
  (ek9-test--with-views ek9-test--views
                        (with-current-buffer (ek9-test--open-view "here")
                          (should (equal (ek9-list--namespace) "team-a"))
                          (setq ek9-context--namespace "kube-system")
                          (should (equal (ek9-list--namespace) "kube-system")))))

(ert-deftest ek9-saved-view-filter-is-local ()
  (ek9-test--with-views ek9-test--views
                        (let ((pods (ek9-list-open 'pods)))
                          (with-current-buffer (ek9-test--open-view "argo")
                            (ek9-list-filter "other"))
                          (should-not (buffer-local-value 'ek9-list--filter pods))
                          ;; Opening the view again gives the saved filter.
                          (with-current-buffer (ek9-test--open-view "argo")
                            (should (equal ek9-list--filter "nginx"))))))

(ert-deftest ek9-saved-view-errors ()
  (ek9-test--with-views ek9-test--views
                        (should-error (ek9-saved-view-open "nothing") :type 'user-error)
                        ;; With the CRD types in the cache, the error comes at once.
                        (ek9-test--load-types)
                        (should-error (ek9-saved-view-open "broken") :type 'user-error))
  (ek9-test--with-views '(("x" :resource pods :context "staging"))
                        (should-error (ek9-saved-view-open "x") :type 'user-error)
                        (should (equal (ek9-context-current) "dev")))
  (ek9-test--with-views nil
                        (should-error (ek9-saved-view--read "Saved view") :type 'user-error)))

(ert-deftest ek9-saved-view-unknown-type-after-crd-load ()
  ;; Without the CRD types, the view waits for them.  Then an unknown
  ;; type is a message, not an error.
  (ek9-test--with-views ek9-test--views
                        (let (messages)
                          (cl-letf (((symbol-function 'message)
                                     (lambda (format &rest args)
                                       (push (apply #'format format args) messages))))
                            (ek9-saved-view-open "broken")
                            (ek9-test--wait-until
                             (lambda () (member "ek9: Saved view broken has an unknown resource type: gadgets"
                                                messages)))))))

(ert-deftest ek9-saved-view-opens-crd-type ()
  (ek9-test--with-views '(("w" :resource widgets.ek9.example.com :namespace "ek9-itest"))
                        (ek9-saved-view-open "w")
                        (ek9-test--wait-until (lambda () (get-buffer "*ek9 view w*")))
                        (with-current-buffer "*ek9 view w*"
                          (ek9-test--wait-until (lambda () ek9-list--loaded))
                          (should (eq (ek9-resource-name ek9-list--resource)
                                      'widgets.ek9.example.com))
                          (should (equal (length ek9-list--items) 4)))))

(ert-deftest ek9-saved-view-namespace-key-goes-to-normal-list ()
  (ek9-test--with-views ek9-test--views
                        (let ((view (ek9-test--open-view "argo")))
                          (with-current-buffer view
                            (ek9-test--switch-with-answer #'ek9-list-switch-namespace "kube-system"))
                          (should (equal (ek9-context-namespace) "kube-system"))
                          (should (equal (buffer-name (window-buffer)) "*ek9 pods*"))
                          ;; The view keeps its namespace and its filter.
                          (with-current-buffer view
                            (should (equal (ek9-list--namespace) "argocd"))
                            (should (equal ek9-list--filter "nginx"))))))

(ert-deftest ek9-saved-view-drill-down-still-blocks-namespace ()
  (ek9-test--with-views ek9-test--views
                        (with-current-buffer (ek9-list-open 'pods '(:args ("--selector" "a=b")
                                                                          :label "a=b"
                                                                          :namespace "team-a"))
                          (should-error (ek9-list-switch-namespace) :type 'user-error))))

(ert-deftest ek9-saved-view-context-key-without-view-context ()
  (ek9-test--with-views ek9-test--views
                        (let ((view (ek9-test--open-view "argo")))
                          (with-current-buffer view
                            (ek9-test--switch-with-answer #'ek9-list-switch-context "prod"))
                          (should (equal (ek9-context-current) "prod"))
                          ;; The view stays, shows the new context, and keeps its namespace.
                          (should (buffer-live-p view))
                          (should (eq (window-buffer) view))
                          (with-current-buffer view
                            (should (equal (ek9-list--namespace) "argocd"))))))

(ert-deftest ek9-saved-view-with-context ()
  (ek9-test--with-views ek9-test--views
                        (let (view messages)
                          (cl-letf (((symbol-function 'message)
                                     (lambda (format &rest args)
                                       (push (apply #'format-message format args) messages))))
                            (setq view (ek9-test--open-view "prod-nodes")))
                          ;; Opening the view changes the current context.
                          (should (equal (ek9-context-current) "prod"))
                          (should (member "ek9: Context changed to prod" messages))
                          (with-current-buffer view
                            (should (= (ek9-test--row-count) 1)))
                          ;; c in the view goes to the normal list.  The view pauses.
                          (with-current-buffer view
                            (ek9-test--switch-with-answer #'ek9-list-switch-context "dev"))
                          (should (equal (ek9-context-current) "dev"))
                          (should (equal (buffer-name (window-buffer)) "*ek9 nodes*"))
                          (with-current-buffer view
                            (ek9-list-refresh)
                            (should-not ek9-list--items)
                            (should-not (ek9-proxy-live-p ek9-list--process))
                            (should (string-match-p "This view uses context prod\\.  Push v"
                                                    (ek9-list--empty-message))))
                          ;; v opens the view again in its context.
                          (ek9-test--open-view "prod-nodes")
                          (should (equal (ek9-context-current) "prod"))
                          (with-current-buffer view
                            (should-not (ek9-list--paused-p))))))

(defun ek9-test--row-count ()
  "Return the number of rows in the current list buffer."
  (length tabulated-list-entries))

(defmacro ek9-test--with-custom-file (&rest body)
  "Run BODY with a fake custom file, so that a save is possible."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'custom-file)
              (lambda (&rest _) "/nonexistent/custom.el")))
     ,@body))

(ert-deftest ek9-saved-view-save ()
  (ek9-test--with-views nil
                        (ek9-test--with-custom-file
                         (let (saved)
                           (cl-letf (((symbol-function 'customize-save-variable)
                                      (lambda (_variable value) (setq saved value))))
                             (with-current-buffer (ek9-list-open 'pods)
                               (ek9-test--wait-until (lambda () ek9-list--loaded))
                               (ek9-list-filter "web")
                               (ek9-saved-view-save "mine"))
                             (should (equal saved '(("mine" :resource pods :namespace "team-a"
                                                     :filter "web" :wide t))))
                             (should (equal ek9-saved-views saved))
                             ;; A drill-down list cannot become a view.
                             (with-current-buffer (ek9-list-open 'pods '(:args ("--selector" "a=b")
                                                                               :label "a=b"))
                               (should-error (ek9-saved-view-save "drill") :type 'user-error))
                             ;; A replace asks first.
                             (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
                               (with-current-buffer "*ek9 pods*"
                                 (should-error (ek9-saved-view-save "mine") :type 'user-error))))))))

(ert-deftest ek9-saved-view-save-refuses-type-names ()
  (ek9-test--with-views nil
    (ek9-test--load-types)
    (with-current-buffer (ek9-list-open 'pods)
      ;; ":" would open the type, not the view.
      (dolist (name '("pods" "po" "ingresses" "wdg"))
        (should (equal (cadr (should-error (ek9-saved-view-save name)
                                           :type 'user-error))
                       (format "ek9: %s is the name of a resource type.  Use another name"
                               name)))))
    (should-not ek9-saved-views)))

(ert-deftest ek9-saved-view-save-keeps-view-context ()
  (ek9-test--with-views '(("p" :resource nodes :context "prod"))
                        (ek9-test--with-custom-file
                         (let (saved)
                           (cl-letf (((symbol-function 'customize-save-variable)
                                      (lambda (_variable value) (setq saved value)))
                                     ((symbol-function 'y-or-n-p) (lambda (_) t)))
                             (with-current-buffer (ek9-test--open-view "p")
                               (ek9-saved-view-save "p")))
                           (should (equal saved '(("p" :resource nodes :context "prod" :wide t))))))))

(ert-deftest ek9-saved-view-save-keeps-following-namespace ()
  (ek9-test--with-views (copy-tree '(("here" :resource pods)))
                        (ek9-test--with-custom-file
                         (let (saved)
                           (cl-letf (((symbol-function 'customize-save-variable)
                                      (lambda (_variable value) (setq saved value)))
                                     ((symbol-function 'y-or-n-p) (lambda (_) t)))
                             (with-current-buffer (ek9-test--open-view "here")
                               (ek9-saved-view-save "here")))
                           (should (equal saved '(("here" :resource pods :wide t))))))))

(ert-deftest ek9-saved-view-reopen-after-change ()
  (ek9-test--with-views (copy-tree '(("v" :resource pods :namespace "argocd")))
                        (let ((buffer (ek9-test--open-view "v"))
                              old-generation)
                          (with-current-buffer buffer
                            (setq old-generation ek9-list--generation))
                          (setq ek9-saved-views '(("v" :resource deployments :namespace "team-a")))
                          (should (eq (ek9-test--open-view "v") buffer))
                          (with-current-buffer buffer
                            (should (eq (ek9-resource-name ek9-list--resource) 'deployments))
                            (should (equal (ek9-list--namespace) "team-a"))
                            (should (> ek9-list--generation old-generation))
                            (should (equal (mapcar #'car tabulated-list-entries) '("team-a/web")))))))

(ert-deftest ek9-saved-view-menu-allows-namespace ()
  (ek9-test--with-views ek9-test--views
                        (with-current-buffer (ek9-test--open-view "argo")
                          (let ((if (plist-get (nth 2 (transient-get-suffix 'ek9-dispatch "n"))
                                               :inapt-if)))
                            (should-not (funcall if))))))

(ert-deftest ek9-saved-view-paused-blocks-actions ()
  (ek9-test--with-views ek9-test--views
                        (let ((view (ek9-test--open-view "prod-nodes")))
                          ;; The context changes in another buffer.  The view is not visible.
                          (switch-to-buffer (get-buffer-create " *ek9 other*"))
                          (ek9-context-set "dev")
                          (with-current-buffer view
                            (ek9-list-refresh)
                            (should-not tabulated-list-entries)
                            (should (string-match-p "This view uses context prod"
                                                    (ek9-list--empty-message)))
                            (let (ran)
                              (cl-letf (((symbol-function 'ek9-process-run)
                                         (lambda (&rest _) (setq ran t))))
                                (should-error (call-interactively #'ek9-delete) :type 'user-error))
                              (should-not ran))))))

(ert-deftest ek9-saved-view-keeps-row-while-paused ()
  (ek9-test--with-views ek9-test--views
                        (let ((view (ek9-test--open-view "prod-nodes")))
                          (with-current-buffer view
                            (goto-char (ek9-list--find-id "kind-control-plane")))
                          (ek9-context-set "dev")
                          (with-current-buffer view
                            (ek9-list-refresh)
                            (should (equal ek9-list--restore-id "kind-control-plane"))))))

(ert-deftest ek9-saved-view-drill-down-then-context ()
  (ek9-test--with-views '(("dep" :resource deployments :namespace "team-a"
                           :context "dev"))
                        (let* ((view (ek9-test--open-view "dep"))
                               (child (with-current-buffer view
                                        (goto-char (ek9-list--find-id "team-a/web"))
                                        (ek9-list-drill-down))))
                          (should (eq (buffer-local-value 'ek9-list--parent child) view))
                          (with-current-buffer child
                            (ek9-test--switch-with-answer #'ek9-list-switch-context "prod"))
                          ;; The drill-down buffer of the old context is gone.  The view
                          ;; stays, but it shows no rows.
                          (should-not (buffer-live-p child))
                          (should (buffer-live-p view))
                          (with-current-buffer view
                            (should (ek9-list--paused-p))))))

(ert-deftest ek9-saved-view-opens-before-contexts-load ()
  (ek9-test--with-clean-context
   (let ((ek9-saved-views '(("p" :resource nodes :context "prod")))
         (ek9-context-changed-hook (list #'ek9-list--context-changed)))
     (unwind-protect
         (progn
           (cl-letf (((symbol-function 'message) #'ignore))
             (ek9-saved-view-open "p"))
           (ek9-test--wait-until (lambda () (get-buffer "*ek9 view p*")))
           (should (equal (ek9-context-current) "prod"))
           (with-current-buffer "*ek9 view p*"
             (ek9-test--wait-until (lambda () ek9-list--loaded))))
       (mapc #'kill-buffer (ek9-list--buffers))
       (when (timerp ek9-list--timer)
         (cancel-timer ek9-list--timer)
         (setq ek9-list--timer nil))))))

(ert-deftest ek9-saved-view-delete-broken-view ()
  (ek9-test--with-views (copy-tree ek9-test--views)
                        (ek9-test--with-custom-file
                         (cl-letf (((symbol-function 'customize-save-variable) #'ignore)
                                   ((symbol-function 'y-or-n-p) (lambda (_) t)))
                           (ek9-saved-view-delete "broken")))
                        (should-not (assoc "broken" ek9-saved-views))))

(ert-deftest ek9-saved-view-save-without-custom-file ()
  (ek9-test--with-views nil
                        (let (messages)
                          ;; In "emacs -Q", Emacs has no file for customizations.
                          (cl-letf (((symbol-function 'custom-file) (lambda (&rest _) nil))
                                    ((symbol-function 'customize-save-variable)
                                     (lambda (&rest _) (error "Unexpected save")))
                                    ((symbol-function 'message)
                                     (lambda (format &rest args)
                                       (push (apply #'format-message format args) messages))))
                            (with-current-buffer (ek9-list-open 'nodes)
                              (ek9-saved-view-save "n")))
                          (should (equal ek9-saved-views '(("n" :resource nodes :wide t))))
                          (should (seq-some (lambda (m) (string-match-p "this session only" m))
                                            messages)))))

(ert-deftest ek9-saved-view-delete ()
  (ek9-test--with-views (copy-tree ek9-test--views)
                        (cl-letf (((symbol-function 'customize-save-variable) #'ignore)
                                  ((symbol-function 'y-or-n-p) (lambda (_) t)))
                          (ek9-saved-view-delete "argo"))
                        (should-not (assoc "argo" ek9-saved-views))
                        (should (assoc "here" ek9-saved-views))))

(ert-deftest ek9-saved-view-describe ()
  (should (equal (ek9-saved-view-describe
                  '(:resource pods :namespace "argocd" :filter "x"
                              :context "prod"))
                 "pods in argocd, filter \"x\", context prod"))
  (should (equal (ek9-saved-view-describe '(:resource pods :namespace all))
                 "pods in all namespaces"))
  (should (equal (ek9-saved-view-describe '(:resource pods))
                 "pods in the current namespace"))
  (should (equal (ek9-saved-view-describe '(:resource nodes)) "nodes"))
  (should (equal (ek9-saved-view-describe '(:resource nodes :wide t)) "nodes"))
  (should (equal (ek9-saved-view-describe '(:resource nodes :wide nil))
                 "nodes, no wide columns")))

(ert-deftest ek9-saved-view-wide ()
  (ek9-test--with-views (copy-tree '(("narrow" :resource pods :wide nil)
                                     ("old" :resource pods)))
    (with-current-buffer (ek9-test--open-view "narrow")
      (should-not ek9-list--wide)
      ;; The toggle changes only the buffer.  The next open uses the
      ;; value of the view again.
      (cl-letf (((symbol-function 'message) #'ignore))
        (ek9-list-toggle-wide))
      (should ek9-list--wide)
      (should-not (plist-get (cdr (assoc "narrow" ek9-saved-views)) :wide))
      (ek9-test--open-view "narrow")
      (should-not ek9-list--wide))
    ;; A view without :wide uses the option.
    (with-current-buffer (ek9-test--open-view "old")
      (should ek9-list--wide))
    (let ((ek9-list-wide nil))
      (with-current-buffer (ek9-test--open-view "old")
        (should-not ek9-list--wide)))))

(ert-deftest ek9-saved-view-save-narrow ()
  (ek9-test--with-views nil
    (ek9-test--with-custom-file
      (let (saved)
        (cl-letf (((symbol-function 'customize-save-variable)
                   (lambda (_variable value) (setq saved value)))
                  ((symbol-function 'message) #'ignore))
          (with-current-buffer (ek9-list-open 'nodes)
            (ek9-list-toggle-wide)
            (ek9-saved-view-save "n")))
        (should (equal saved '(("n" :resource nodes :wide nil))))))))

(ert-deftest ek9-saved-view-key ()
  (should (eq (keymap-lookup ek9-list-mode-map "v") #'ek9-saved-view-open))
  (should (eq (keymap-lookup ek9-list-mode-map "c") #'ek9-list-switch-context)))

(provide 'ek9-saved-view-test)

;;; ek9-saved-view-test.el ends here
