;;; ek9-exec-test.el --- Tests for ek9-exec -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for container shells.  The fake kubectl prints its
;; arguments for "exec", so the terminal buffer shows the command.

;;; Code:

(require 'ert)
(require 'ek9-exec)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--exec-pod (&optional phase)
  "Return a pod with two containers and an init container.
PHASE is the pod phase, \"Running\" by default."
  `((kind . "Pod")
    (metadata (name . "web-1") (namespace . "team-a"))
    (spec (containers ((name . "app")) ((name . "proxy")))
          (initContainers ((name . "init"))))
    (status (phase . ,(or phase "Running")))))

(defmacro ek9-test--with-shell (&rest body)
  "Run BODY in context dev with `term' as the terminal.
Kill all shell buffers after BODY."
  (declare (indent 0))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev")
     (let ((ek9-exec-terminal 'term))
       (unwind-protect
           (progn ,@body)
         (dolist (buffer (buffer-list))
           (when (string-prefix-p "*ek9 shell " (buffer-name buffer))
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buffer))))))))

(ert-deftest ek9-exec-args ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (should (equal (ek9-exec--args (ek9-test--exec-pod) "proxy")
                   '("exec" "-it" "web-1" "--container" "proxy"
                     "--namespace" "team-a" "--context" "dev" "--"
                     "sh" "-c"
                     "command -v bash >/dev/null && exec bash || exec sh")))))

(ert-deftest ek9-exec-opens-terminal ()
  (ek9-test--with-shell
    (let ((buffer (ek9-shell (ek9-test--exec-pod) "app")))
      (with-current-buffer buffer
        (should (equal (buffer-name) "*ek9 shell dev team-a/web-1 app*"))
        (should (derived-mode-p 'term-mode))
        (ek9-test--wait-until
         (lambda () (string-match-p "exec -it web-1" (buffer-string))))
        (should (string-match-p "--container app --namespace team-a --context dev"
                                (buffer-string)))
        (should (string-match-p
                 "Context: dev  Shell: team-a/web-1 app"
                 (substring-no-properties (ek9-exec--header-line))))))))

(ert-deftest ek9-exec-reuses-live-shell ()
  (ek9-test--with-shell
    (let ((ek9-exec-command '("sleep-forever")))
      (cl-letf (((symbol-function 'ek9-exec--start)
                 (lambda (name _args)
                   (let ((buffer (get-buffer-create (concat "*" name "*"))))
                     (make-process :name "ek9-test-sleep" :buffer buffer
                                   :command '("sleep" "30") :noquery t)
                     buffer))))
        (let* ((first (ek9-shell (ek9-test--exec-pod) "app"))
               (process (get-buffer-process first))
               (second (ek9-shell (ek9-test--exec-pod) "app")))
          (should (eq first second))
          (should (eq (get-buffer-process second) process))
          (delete-process process)
          ;; After the process ends, a new shell replaces the buffer.
          (let ((third (ek9-shell (ek9-test--exec-pod) "app")))
            (should-not (buffer-live-p first))
            (should (process-live-p (get-buffer-process third)))
            (delete-process (get-buffer-process third))))))))

(ert-deftest ek9-exec-uses-eat ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (let ((default-directory temporary-file-directory)
          call directory)
      (cl-letf (((symbol-function 'ek9-exec--terminal) (lambda () 'eat))
                ((symbol-function 'eat-make)
                 (lambda (&rest args)
                   (setq call args
                         directory default-directory)
                   (get-buffer-create (concat "*" (car args) "*")))))
        (let ((buffer (ek9-shell (ek9-test--exec-pod) "app")))
          (unwind-protect
              (progn
                (should (equal (car call) "ek9 shell dev team-a/web-1 app"))
                (should (equal (nth 1 call) ek9-kubectl-executable))
                (should (equal (nthcdr 3 call)
                               (ek9-exec--args (ek9-test--exec-pod) "app")))
                ;; kubectl must not start in a TRAMP directory.
                (should (equal directory (expand-file-name "~/")))
                (should (with-current-buffer buffer header-line-format)))
            (kill-buffer buffer)))))))

(ert-deftest ek9-exec-eat-missing ()
  (let ((ek9-exec-terminal 'eat))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (ek9-exec--terminal) :type 'user-error))))

(ert-deftest ek9-exec-read-only-context-blocks ()
  (ek9-test--with-shell
    (setq ek9-read-only-contexts '("dev"))
    (should-error (ek9-shell (ek9-test--exec-pod) "app") :type 'user-error)
    (should-not (get-buffer "*ek9 shell dev team-a/web-1 app*"))))

(ert-deftest ek9-exec-pod-not-running ()
  (ek9-test--with-shell
    (should (equal (cadr (should-error
                          (ek9-shell (ek9-test--exec-pod "Pending") "app")
                          :type 'user-error))
                   "ek9: Pod team-a/web-1 is not running (phase Pending)"))))

(ert-deftest ek9-exec-reads-container-without-init ()
  (let (choices)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq choices collection)
                 "proxy")))
      (should (equal (ek9-list-read-container (ek9-test--exec-pod) t) "proxy"))
      (should (equal choices '("app" "proxy"))))))

(ert-deftest ek9-exec-no-container ()
  (should-error (ek9-list-read-container
                 '((metadata (name . "x") (namespace . "a")) (spec)) t)
                :type 'user-error))

(ert-deftest ek9-exec-default-container-ignores-unknown-annotation ()
  (let ((pod (ek9-test--exec-pod)))
    (push '(annotations (kubectl.kubernetes.io/default-container . "init"))
          (alist-get 'metadata pod))
    (should (equal (ek9-resource-pod-default-container pod t) "app"))
    (should (equal (ek9-resource-pod-default-container pod) "init"))))

(ert-deftest ek9-exec-key-and-action ()
  (should (eq (keymap-lookup ek9-list-mode-map "s") #'ek9-shell))
  (should (memq 'shell (ek9-resource-actions (ek9-resource-get 'pods))))
  (should-not (memq 'shell (ek9-resource-actions
                            (ek9-resource-get 'deployments)))))

(provide 'ek9-exec-test)

;;; ek9-exec-test.el ends here
