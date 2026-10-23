;;; ek9-describe-test.el --- Tests for ek9-describe -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the describe and YAML views.  The tests use a fake
;; kubectl script and do not need a cluster.

;;; Code:

(require 'ert)
(require 'ek9-describe)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--fixture-pod (name)
  "Return the pod NAME from the pods fixture."
  (seq-find (lambda (item) (equal (ek9-resource-item-name item) name))
            (alist-get 'items (ek9-test--read-fixture "pods.json"))))

(defmacro ek9-test--with-view (form &rest body)
  "Run FORM to open a view, wait for its content, and run BODY.
The view buffer is current in BODY.  Kill it after BODY."
  (declare (indent 1))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev")
     (let ((buffer ,form))
       (unwind-protect
           (with-current-buffer buffer
             (ek9-test--wait-until (lambda () (> (buffer-size) 0)))
             ,@body)
         (kill-buffer buffer)))))

(ert-deftest ek9-describe-shows-output ()
  (ek9-test--with-view
      (ek9-describe (ek9-resource-get 'pods)
                    (ek9-test--fixture-pod "nginx-7c5b8d65b8-abcde"))
    (should (equal (buffer-name)
                   "*ek9 describe pods default/nginx-7c5b8d65b8-abcde*"))
    (should (eq major-mode 'ek9-describe-mode))
    (should ek9-view-mode)
    (should buffer-read-only)
    ;; The fake kubectl prints its arguments for "describe".
    (should (equal (buffer-string)
                   (concat "describe pods nginx-7c5b8d65b8-abcde"
                           " --namespace default --context dev\n")))
    (should (string-match-p "Context: dev" header-line-format))))

(ert-deftest ek9-yaml-shows-yaml ()
  (ek9-test--with-view
      (ek9-yaml (ek9-resource-get 'pods)
                (ek9-test--fixture-pod "setup-abc"))
    (should (equal (buffer-name) "*ek9 yaml pods kube-system/setup-abc*"))
    (should (string-match-p "name: setup-abc" (buffer-string)))
    (should buffer-read-only)
    (should ek9-view-mode)))

(ert-deftest ek9-view-refresh-keeps-line ()
  (ek9-test--with-view
      (ek9-yaml (ek9-resource-get 'pods)
                (ek9-test--fixture-pod "setup-abc"))
    (forward-line 2)
    (ek9-view-refresh)
    (ek9-test--wait-until (lambda () (not (ek9-process-live-p ek9-view--process))))
    (accept-process-output nil 0.05)
    (should (= (line-number-at-pos) 3))))

(ert-deftest ek9-view-shows-error ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "forbidden")
    (let ((buffer (ek9-describe (ek9-resource-get 'pods)
                                (ek9-test--fixture-pod "db-0"))))
      (unwind-protect
          (with-current-buffer buffer
            (ek9-test--wait-until (lambda () (> (buffer-size) 0)))
            (should (string-match-p "kubectl failed: .*Forbidden"
                                    (buffer-string))))
        (kill-buffer buffer)))))

(ert-deftest ek9-list-item-for-action-checks-actions ()
  (with-temp-buffer
    (ek9-list-mode)
    (setq ek9-list--resource (ek9-resource-create :name 'things :actions nil))
    (should-error (ek9-list-item-for-action 'logs) :type 'user-error)
    (setf (ek9-resource-actions ek9-list--resource) '(logs))
    ;; The action exists, but there is no row at point.
    (should-error (ek9-list-item-for-action 'logs) :type 'user-error)))

;;;; Secrets

(ert-deftest ek9-secret-decode-value ()
  (should (equal (ek9-secret--decode-value "YWRtaW4=") "admin"))
  (should (equal (ek9-secret--decode-value "cMOkc3N3b3Jk") "pässword"))
  (should (equal (ek9-secret--decode-value "AAEC/w==")
                 "<binary data, 4 bytes>")))

(ert-deftest ek9-secret-decode-shows-values ()
  (ek9-test--with-view
      (ek9-secret-decode (ek9-resource-get 'secrets)
                         '((metadata (name . "db-creds")
                                     (namespace . "default"))))
    (should (equal (buffer-name) "*ek9 decode secrets default/db-creds*"))
    (should buffer-read-only)
    (should (equal (buffer-string)
                   (concat "# Decoded data of secret default/db-creds\n"
                           "username: admin\n"
                           "password: pässword\n"
                           "config: |\n"
                           "  host: db\n"
                           "  port: 5432\n"
                           "cert: <binary data, 4 bytes>\n")))
    ;; "g" decodes the values again.
    (ek9-view-refresh)
    (ek9-test--wait-until (lambda () (not (ek9-process-live-p ek9-view--process))))
    (accept-process-output nil 0.05)
    (should (string-match-p "username: admin" (buffer-string)))))

(ert-deftest ek9-secret-decode-only-for-secrets ()
  (with-temp-buffer
    (ek9-list-mode)
    (setq ek9-list--resource (ek9-resource-get 'pods))
    (should-error (call-interactively #'ek9-secret-decode) :type 'user-error)))

(provide 'ek9-describe-test)

;;; ek9-describe-test.el ends here
