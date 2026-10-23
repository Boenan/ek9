;;; ek9-edit-test.el --- Tests for ek9-edit -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the edit buffer.  The fake kubectl prints a small
;; YAML document for "get" and simulates "replace".

;;; Code:

(require 'ert)
(require 'ek9-edit)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defconst ek9-test--pod
  "{\"kind\": \"Pod\", \"metadata\": {\"name\": \"web-1\", \"namespace\": \"team-a\"}}"
  "JSON of a pod for the tests.")

(defmacro ek9-test--with-edit (answer &rest body)
  "Open an edit buffer for a pod in context dev and run BODY in it.
ANSWER is the answer to each `yes-or-no-p' prompt.  In BODY,
`prompts' is the list of prompts and `messages' is the list of
messages, newest first.  Kill the buffer after BODY."
  (declare (indent 1))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev")
     (let (prompts messages buffer)
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (prompt) (push prompt prompts) ,answer))
                 ((symbol-function 'message)
                  (lambda (format &rest args)
                    (push (apply #'format-message format args) messages))))
         (setq buffer (ek9-edit (ek9-resource-get 'pods)
                                (json-parse-string ek9-test--pod
                                                   :object-type 'alist)))
         (unwind-protect
             (with-current-buffer buffer
               (ek9-test--wait-until (lambda () (> (buffer-size) 0)))
               ,@body)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defun ek9-test--wait-apply ()
  "Wait until the kubectl processes of the edit buffer are done."
  (ek9-test--wait-until (lambda () (not (ek9-process-live-p ek9-edit--process))))
  ;; The dry run starts the apply in its callback.
  (accept-process-output nil 0.1)
  (ek9-test--wait-until (lambda () (not (ek9-process-live-p ek9-edit--process)))))

(ert-deftest ek9-edit-opens-yaml ()
  (ek9-test--with-edit t
    (should (equal (buffer-name) "*ek9 edit dev pods team-a/web-1*"))
    (should (equal (buffer-string) "kind: Pod\nmetadata:\n  name: web-1\n"))
    (should ek9-edit-mode)
    (should-not (buffer-modified-p))
    (should-not buffer-read-only)
    (let ((line (substring-no-properties (ek9-edit--header-line))))
      (should (string-match-p "Context: dev" line))
      (should (string-match-p "Edit: Pod team-a/web-1" line))
      (should (string-match-p "C-c C-c apply" line)))
    (should (eq (keymap-lookup ek9-edit-mode-map "<remap> <save-buffer>") #'ek9-edit-apply))))

(ert-deftest ek9-edit-apply-runs-dry-run-then-apply ()
  (ek9-test--with-edit t
    (goto-char (point-max))
    (insert "spec: {}\n")
    (ek9-edit-apply)
    (should (equal prompts
                   '("Apply changes to Pod team-a/web-1 in context dev? ")))
    (ek9-test--wait-apply)
    (should (member (concat "ek9: applied 4 lines (spec: {}): replace -f - "
                            "--namespace team-a --context dev")
                    messages))
    (should-not ek9-edit--status)
    ;; After the apply, ek9 gets the YAML again.
    (ek9-test--wait-until (lambda () (not (buffer-modified-p))))
    (should (equal (buffer-string) "kind: Pod\nmetadata:\n  name: web-1\n"))))

(defmacro ek9-test--recording-runs (&rest body)
  "Run BODY and record each call of `ek9-process-run'.
In BODY, `runs' is the list of (ARGS INPUT) of each call, newest
first."
  (declare (indent 0))
  (let ((run (make-symbol "run")))
    `(let ((runs nil)
           (,run (symbol-function 'ek9-process-run)))
       (cl-letf (((symbol-function 'ek9-process-run)
                  (lambda (args on-success &optional on-error input)
                    (push (list args input) runs)
                    (funcall ,run args on-success on-error input))))
         ,@body))))

(defun ek9-test--last-applied-runs (runs)
  "Return the set-last-applied calls in RUNS."
  (seq-filter (lambda (run) (equal (take 2 (car run))
                                   '("apply" "set-last-applied")))
              runs))

(ert-deftest ek9-edit-last-applied-cleans-server-fields ()
  (let* ((output (concat
                  "{\"kind\": \"Widget\", \"metadata\": {\"name\": \"w\", "
                  "\"resourceVersion\": \"7\", \"uid\": \"u-1\", \"generation\": 2, "
                  "\"creationTimestamp\": \"2026-01-01T00:00:00Z\", "
                  "\"managedFields\": [], \"annotations\": {"
                  "\"kubectl.kubernetes.io/last-applied-configuration\": \"{}\", "
                  "\"note\": \"\"}}, "
                  "\"spec\": {\"on\": false, \"none\": null, \"empty\": {}, \"list\": []}}"))
         (clean (ek9-edit--last-applied output)))
    (should (equal clean
                   (concat "{\"kind\":\"Widget\",\"metadata\":{\"name\":\"w\","
                           "\"annotations\":{\"note\":\"\"}},"
                           "\"spec\":{\"on\":false,\"none\":null,\"empty\":{},"
                           "\"list\":[]}}")))))

(ert-deftest ek9-edit-last-applied-nil-without-annotation ()
  (should-not (ek9-edit--last-applied
               "{\"kind\": \"Pod\", \"metadata\": {\"name\": \"p\"}}"))
  (should-not (ek9-edit--last-applied
               (concat "{\"kind\": \"Pod\", \"metadata\": {\"name\": \"p\", "
                       "\"annotations\": {\"note\": \"x\"}}}"))))

(ert-deftest ek9-edit-apply-without-annotation-skips-set-last-applied ()
  (ek9-test--with-edit t
    (ek9-test--recording-runs
      (insert "# change\n")
      (ek9-edit-apply)
      (ek9-test--wait-apply)
      (ek9-test--wait-until (lambda () (not (buffer-modified-p))))
      (should-not (ek9-test--last-applied-runs runs)))))

(ert-deftest ek9-edit-apply-sets-clean-last-applied ()
  (ek9-test--with-edit t
    (ek9-test--recording-runs
      (goto-char (point-max))
      (insert "  annotations:\n"
              "    kubectl.kubernetes.io/last-applied-configuration: '{}'\n")
      (ek9-edit-apply)
      (ek9-test--wait-until (lambda () (ek9-test--last-applied-runs runs)))
      (ek9-test--wait-until (lambda () (not (buffer-modified-p))))
      (ek9-test--wait-until (lambda () (not buffer-read-only)))
      (pcase-let ((`((,args ,input)) (ek9-test--last-applied-runs runs)))
        (should (equal args '("apply" "set-last-applied" "-f" "-"
                              "--namespace" "team-a" "--context" "dev")))
        (let ((object (json-parse-string input :object-type 'alist)))
          (should (equal (alist-get 'metadata object)
                         '((name . "web-1") (labels) (annotations)))))
        (should (string-match-p "\"on\":false" input)))
      ;; The replace runs before the annotation update.
      (should (equal (car (nth 1 (member (car (ek9-test--last-applied-runs runs))
                                         runs)))
                     '("replace" "-f" "-" "--namespace" "team-a"
                       "--context" "dev")))
      (should-not ek9-edit--status))))

(ert-deftest ek9-edit-kill-during-replace-sets-last-applied ()
  (ek9-test--with-edit t
    (let ((buffer (current-buffer))
          (runs nil)
          (run (symbol-function 'ek9-process-run)))
      (cl-letf (((symbol-function 'ek9-process-run)
                 (lambda (args on-success &optional on-error input)
                   (push args runs)
                   (prog1 (funcall run args on-success on-error input)
                     ;; The user kills the buffer while the replace runs.
                     (when (equal (car args) "replace")
                       (unless (member "--dry-run=client" args)
                         (kill-buffer buffer)))))))
        (goto-char (point-max))
        (insert "  annotations:\n"
                "    kubectl.kubernetes.io/last-applied-configuration: '{}'\n"
                "# annotation-fail\n")
        (ek9-edit-apply)
        (ek9-test--wait-until (lambda () (not (buffer-live-p buffer))))
        (ek9-test--wait-until
         (lambda () (seq-some (lambda (m) (string-match-p "annotation conflict" m))
                              messages)))
        (should (equal (take 4 (car runs)) '("apply" "set-last-applied" "-f" "-")))
        (should (string-match-p
                 (concat "\\`ek9: applied .*: replace .*\\.  Applied, but the "
                         "last-applied annotation is not clean\\.")
                 (car messages)))))))

(ert-deftest ek9-edit-apply-last-applied-start-error-warns ()
  (ek9-test--with-edit t
    (let ((run (symbol-function 'ek9-process-run)))
      (cl-letf (((symbol-function 'ek9-process-run)
                 (lambda (args on-success &optional on-error input)
                   (if (equal (car args) "apply")
                       (error "Cannot start kubectl")
                     (funcall run args on-success on-error input)))))
        (goto-char (point-max))
        (insert "  annotations:\n"
                "    kubectl.kubernetes.io/last-applied-configuration: '{}'\n")
        (ek9-edit-apply)
        (ek9-test--wait-until (lambda () (not (buffer-modified-p))))
        (ek9-test--wait-until (lambda () (not buffer-read-only)))
        (should (equal ek9-edit--status
                       (concat "Applied, but the last-applied annotation is not clean.  "
                               "kubectl apply can fail: Cannot start kubectl")))))))

(ert-deftest ek9-edit-apply-last-applied-error-warns ()
  (ek9-test--with-edit t
    (goto-char (point-max))
    (insert "  annotations:\n"
            "    kubectl.kubernetes.io/last-applied-configuration: '{}'\n"
            "# annotation-fail\n")
    (ek9-edit-apply)
    (ek9-test--wait-until (lambda () (not (buffer-modified-p))))
    (ek9-test--wait-until (lambda () (not buffer-read-only)))
    (should (equal ek9-edit--status
                   (concat "Applied, but the last-applied annotation is not clean.  "
                           "kubectl apply can fail: error: annotation conflict")))
    ;; The warning shows one time, not again after the load.
    (should (= 1 (seq-count (lambda (m) (string-match-p "annotation conflict" m))
                            messages)))))

(ert-deftest ek9-edit-apply-no-changes ()
  (ek9-test--with-edit t
    (ek9-edit-apply)
    (should-not prompts)
    (should (equal (car messages) "ek9: No changes to apply"))))

(ert-deftest ek9-edit-apply-cancelled ()
  (ek9-test--with-edit nil
    (insert "# change\n")
    (ek9-edit-apply)
    (should (equal (car messages) "ek9: Cancelled"))
    (should-not (ek9-process-live-p ek9-edit--process))
    (should (buffer-modified-p))))

(ert-deftest ek9-edit-apply-blocks-other-name ()
  (ek9-test--with-edit t
    (goto-char (point-min))
    (search-forward "web-1")
    (replace-match "web-2")
    (ek9-edit-apply)
    (ek9-test--wait-apply)
    (should (equal ek9-edit--status
                   (concat "The YAML is for Pod web-2, not for Pod team-a/web-1."
                           "  Nothing was applied")))
    (should-not (seq-some (lambda (m) (string-prefix-p "ek9: applied" m))
                          messages))
    (should (buffer-modified-p))))

(ert-deftest ek9-edit-apply-blocks-many-documents ()
  (ek9-test--with-edit t
    (goto-char (point-max))
    (insert "---\nkind: Pod\nmetadata:\n  name: other\n")
    (ek9-edit-apply)
    (ek9-test--wait-apply)
    (should (equal ek9-edit--status
                   (concat "The buffer must contain exactly one resource."
                           "  Nothing was applied")))))

(ert-deftest ek9-edit-context-change-during-dry-run ()
  (ek9-test--with-edit t
    (insert "# change\n")
    (ek9-edit-apply)
    ;; The user switches the context while the dry run runs.
    (setq ek9-context--current "prod")
    (ek9-test--wait-apply)
    (should (equal ek9-edit--status "The context changed.  Nothing was done"))
    (should-not (seq-some (lambda (m) (string-prefix-p "ek9: applied" m))
                          messages))
    (should (buffer-modified-p))))

(ert-deftest ek9-edit-uses-buffer-context ()
  (ek9-test--with-edit t
    (setq ek9-context--current "prod")
    (should (equal (ek9-edit--replace-args)
                   '("replace" "-f" "-" "--namespace" "team-a"
                     "--context" "dev")))
    (should (member "dev" (ek9-edit--get-args)))))

(ert-deftest ek9-edit-kill-during-dry-run ()
  (ek9-test--with-edit t
    (insert "# change\n")
    (ek9-edit-apply)
    (set-buffer-modified-p nil)
    (kill-buffer)
    (accept-process-output nil 0.3)
    (should-not (seq-some (lambda (m) (string-prefix-p "ek9: applied" m))
                          messages))))

(ert-deftest ek9-edit-read-only-while-loading ()
  (ek9-test--with-edit t
    (ek9-edit--load)
    (should buffer-read-only)
    (should-error (insert "x") :type 'buffer-read-only)
    (ek9-test--wait-until (lambda () (not buffer-read-only)))
    (should (equal (buffer-string) "kind: Pod\nmetadata:\n  name: web-1\n"))))

(ert-deftest ek9-edit-sends-utf-8 ()
  (ek9-test--with-edit t
    (goto-char (point-max))
    (insert "# café\n")
    (ek9-edit-apply)
    (ek9-test--wait-apply)
    (should (seq-some (lambda (m) (string-match-p "(# café)" m)) messages))))

(ert-deftest ek9-edit-survives-major-mode-change ()
  (ek9-test--with-edit t
    (fundamental-mode)
    (should ek9-edit-mode)
    (should (equal ek9-edit--context "dev"))
    (should header-line-format)
    (should (eq (key-binding (kbd "C-x C-s")) #'ek9-edit-apply))
    (should (memq #'ek9-edit--query-kill kill-buffer-query-functions))))

(ert-deftest ek9-edit-kill-asks-when-modified ()
  (ek9-test--with-edit nil
    (insert "# change\n")
    (should-not (kill-buffer (current-buffer)))
    (should (equal prompts
                   '("The changes are not applied.  Kill the buffer? ")))
    (should (buffer-live-p (current-buffer)))))

(ert-deftest ek9-edit-apply-error-keeps-changes ()
  (ek9-test--with-edit t
    (goto-char (point-max))
    (insert "fail: true\n")
    (ek9-edit-apply)
    (ek9-test--wait-apply)
    (should (equal ek9-edit--status "Apply failed: error: apply conflict"))
    (should (string-match-p "Apply failed"
                            (substring-no-properties (ek9-edit--header-line))))
    (should (buffer-modified-p))
    (should (string-match-p "fail: true" (buffer-string)))))

(ert-deftest ek9-edit-read-only-context-blocks ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev"
          ek9-read-only-contexts '("dev"))
    (should-error (ek9-edit (ek9-resource-get 'pods)
                            (json-parse-string ek9-test--pod :object-type 'alist))
                  :type 'user-error)
    (should-not (get-buffer "*ek9 edit dev pods team-a/web-1*"))))

(ert-deftest ek9-edit-apply-checks-context ()
  (ek9-test--with-edit t
    (insert "# change\n")
    (setq ek9-context--current "prod")
    (should-error (ek9-edit-apply) :type 'user-error)
    (setq ek9-context--current "dev"
          ek9-read-only-contexts '("dev"))
    (should-error (ek9-edit-apply) :type 'user-error)
    (should-not prompts)))

(ert-deftest ek9-edit-cancel-asks-when-modified ()
  (ek9-test--with-edit nil
    (insert "# change\n")
    (ek9-edit-cancel)
    (should (equal prompts '("Discard the changes? ")))
    (should (buffer-live-p (current-buffer)))))

(ert-deftest ek9-edit-start-error-kills-buffer ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (let ((ek9-kubectl-executable "/nonexistent/kubectl"))
      (should-error (ek9-edit (ek9-resource-get 'pods)
                              (json-parse-string ek9-test--pod
                                                 :object-type 'alist)))
      (should-not (get-buffer "*ek9 edit dev pods team-a/web-1*")))))

(ert-deftest ek9-edit-load-error-kills-buffer ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (cl-letf (((symbol-function 'message) #'ignore))
      (let ((buffer (ek9-edit (ek9-resource-get 'pods)
                              (json-parse-string ek9-test--pod
                                                 :object-type 'alist))))
        ;; The first load already runs with context dev, so start a
        ;; second load that fails.
        (with-current-buffer buffer
          (ek9-test--wait-until (lambda () (> (buffer-size) 0)))
          (setq ek9-edit--context "forbidden")
          (ek9-edit--load (lambda () (kill-buffer buffer))))
        (ek9-test--wait-until (lambda () (not (buffer-live-p buffer))))))))

(provide 'ek9-edit-test)

;;; ek9-edit-test.el ends here
