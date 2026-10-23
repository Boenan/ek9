;;; ek9-command-test.el --- Tests for the ek9 resource command -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for ":" with a namespace, with "-A", and with the name
;; of a saved view.

;;; Code:

(require 'ert)
(require 'ek9)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defmacro ek9-test--with-command (&rest body)
  "Run BODY in context dev and namespace team-a with two saved views.
Kill all list buffers and stop the timer after BODY."
  (declare (indent 0))
  `(ek9-test--with-clean-context
     (ek9-test--load-contexts)
     (let ((ek9-saved-views '(("argo" :resource pods :namespace "argocd")
                              ("pods" :resource nodes)))
           (ek9-context-changed-hook (list #'ek9-list--context-changed)))
       (unwind-protect
           (progn ,@body)
         (mapc #'kill-buffer (ek9-list--buffers))
         (when (timerp ek9-list--timer)
           (cancel-timer ek9-list--timer)
           (setq ek9-list--timer nil))))))

(defun ek9-test--run-command (input)
  "Run `ek9-list-select-resource' with the minibuffer INPUT.
Return the buffer that the command shows."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) input)))
    (call-interactively #'ek9-list-select-resource))
  (window-buffer))

(ert-deftest ek9-command-parse ()
  (ek9-test--with-command
    (should (equal (ek9-list-parse-command "pods") '(resource pods nil)))
    (should (equal (ek9-list-parse-command " po  argocd ")
                   '(resource pods "argocd")))
    (should (equal (ek9-list-parse-command "deploy -A")
                   '(resource deployments all)))
    (should (equal (ek9-list-parse-command "argo") '(view "argo")))
    ;; A resource type wins over a view with the same name.
    (should (equal (ek9-list-parse-command "pods") '(resource pods nil)))
    (dolist (input '("" "gadgets" "pods a b" "nodes argocd" "no -A"))
      (should-error (ek9-list-parse-command input) :type 'user-error))))

(ert-deftest ek9-command-namespace-error-text ()
  (ek9-test--with-command
    (should (equal (cadr (should-error (ek9-list-parse-command "nodes argocd")
                                       :type 'user-error))
                   "ek9: nodes are not in a namespace"))))

(ert-deftest ek9-command-with-namespace ()
  (ek9-test--with-command
    (let ((buffer (ek9-test--run-command "po argocd")))
      (should (equal (buffer-name buffer) "*ek9 pods*"))
      (should (equal (ek9-context-namespace) "argocd"))
      (with-current-buffer buffer
        (should (string-prefix-p "/api/v1/namespaces/argocd/pods?"
                                 (ek9-list--request-path)))))))

(ert-deftest ek9-command-all-namespaces ()
  (ek9-test--with-command
    (ek9-test--run-command "deploy -A")
    (should (eq (ek9-context-namespace) 'all))))

(ert-deftest ek9-command-without-namespace-keeps-namespace ()
  (ek9-test--with-command
    (ek9-test--run-command "svc")
    (should (equal (ek9-context-namespace) "team-a"))))

(ert-deftest ek9-command-opens-view ()
  (ek9-test--with-command
    (should (equal (buffer-name (ek9-test--run-command "argo"))
                   "*ek9 view argo*"))
    (should (equal (ek9-context-namespace) "team-a"))))

(ert-deftest ek9-command-completion ()
  (ek9-test--with-command
    (puthash "dev" '("argocd" "default" "kube-system")
             ek9-context--namespace-cache)
    (let ((table (ek9-list--command-table (ek9-list--resource-choices))))
      ;; The first word: resource types, short names, and views.
      (should (member "deploy" (all-completions "de" table)))
      (should (member "argo" (all-completions "ar" table)))
      ;; After a space: the typed word first, then namespaces and -A.
      (should (equal (all-completions "pods a" table) '("a" "argocd")))
      (should (equal (car (all-completions "pods team-b" table)) "team-b"))
      (should (member "-A" (all-completions "pods -" table)))
      ;; TAB extends the typed word.
      (should (equal (try-completion "pods kube" table) "pods kube-system"))
      ;; An empty word has no candidates, so RET keeps "pods ".
      (should-not (all-completions "pods " table))
      (should (equal (completion-boundaries "pods ku" table nil "")
                     '(5 . 0))))))

(ert-deftest ek9-command-annotations ()
  (ek9-test--with-command
    (puthash "dev" '("argo" "cm") ek9-context--namespace-cache)
    (let* ((table (ek9-list--command-table (ek9-list--resource-choices)))
           (annotate (alist-get 'annotation-function
                                (cdr (funcall table "de" nil 'metadata)))))
      (should (equal (funcall annotate "deploy") "  deployments"))
      (should-not (funcall annotate "deployments"))
      (should (equal (funcall annotate "argo") "  saved view"))
      ;; Namespaces get no annotation, also when a view or a short
      ;; name has the same name.
      (should (equal (funcall table "pods ar" nil 'metadata) '(metadata))))))

(ert-deftest ek9-command-before-context-loads ()
  (ek9-test--with-clean-context
    (let ((ek9-context-changed-hook (list #'ek9-list--context-changed)))
      (unwind-protect
          (progn
            (ek9-list-select-resource '(resource pods "argocd"))
            (ek9-test--wait-until (lambda () (get-buffer "*ek9 pods*")))
            (should (equal (ek9-context-current) "dev"))
            (should (equal (ek9-context-namespace) "argocd")))
        (mapc #'kill-buffer (ek9-list--buffers))
        (when (timerp ek9-list--timer)
          (cancel-timer ek9-list--timer)
          (setq ek9-list--timer nil))))))

(ert-deftest ek9-command-symbol-form ()
  (ek9-test--with-command
    (should (equal (buffer-name (ek9-list-select-resource 'nodes))
                   "*ek9 nodes*"))))

(ert-deftest ek9-command-set-namespace-needs-context ()
  (ek9-test--with-clean-context
    (should-error (ek9-context-set-namespace "argocd") :type 'user-error)))

(ert-deftest ek9-command-view-name-without-space ()
  (ek9-test--with-command
    (with-current-buffer (ek9-list-open 'pods)
      (should-error (ek9-saved-view-save "my view") :type 'user-error))))

(ert-deftest ek9-command-space-inserts-space ()
  (ek9-test--with-command
    (let (binding)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (run-hooks 'minibuffer-setup-hook)
                   (setq binding (keymap-lookup (current-local-map) "SPC"))
                   "pods")))
        (with-temp-buffer
          (use-local-map minibuffer-local-completion-map)
          (ek9-list--read-command)))
      (should (eq binding #'self-insert-command)))))

(ert-deftest ek9-command-prefetches-namespaces ()
  (ek9-test--with-command
    (should-not (ek9-context-cached-namespaces))
    (ek9-context-prefetch-namespaces)
    (ek9-test--wait-until #'ek9-context-cached-namespaces)
    (should (member "kube-system" (ek9-context-cached-namespaces))))
  ;; A failure shows no message.
  (ek9-test--with-clean-context
    (setq ek9-context--current "forbidden")
    (let (messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args) (push args messages))))
        (let ((process (ek9-context-prefetch-namespaces)))
          (ek9-test--wait process)))
      (should-not messages)
      (should-not (ek9-context-cached-namespaces)))))

(provide 'ek9-command-test)

;;; ek9-command-test.el ends here
