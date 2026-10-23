;;; ek9-process-test.el --- Tests for ek9-process -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the async kubectl runner.  The tests use a fake
;; kubectl script and do not need a cluster.

;;; Code:

(require 'ert)
(require 'ek9-process)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(ert-deftest ek9-process-run-returns-stdout ()
  (ek9-test--with-fake-kubectl
    (let (result)
      (ek9-test--wait
       (ek9-process-run '("version" "--client")
                        (lambda (out) (setq result out))))
      (should (equal result "version --client\n")))))

(ert-deftest ek9-process-run-calls-error-callback ()
  (ek9-test--with-fake-kubectl
    (let (code stderr success)
      (ek9-test--wait
       (ek9-process-run '("fail")
                        (lambda (_) (setq success t))
                        (lambda (c e) (setq code c stderr e))))
      (should-not success)
      (should (equal code 1))
      (should (equal stderr "error: fake failure")))))

(ert-deftest ek9-process-run-json-parses-output ()
  (ek9-test--with-fake-kubectl
    (let (result)
      (ek9-test--wait
       (ek9-process-run-json '("get" "pods")
                             (lambda (json) (setq result json))))
      (let ((pod (car (alist-get 'items result))))
        (should (equal (alist-get 'name (alist-get 'metadata pod))
                       "nginx-7c5b8d65b8-abcde"))
        (should (equal (alist-get 'phase (alist-get 'status pod))
                       "Running"))))))

(ert-deftest ek9-process-error-summary-returns-last-line ()
  (should (equal (ek9-process-error-summary
                  "E1021 log line 1\nE1021 log line 2\nUnable to connect\n\n")
                 "Unable to connect"))
  (should (equal (ek9-process-error-summary "") "")))

(ert-deftest ek9-process-run-kills-temp-buffers ()
  (ek9-test--with-fake-kubectl
    (let ((before (length (buffer-list))))
      (ek9-test--wait (ek9-process-run '("x") #'ignore))
      (should (= before (length (buffer-list)))))))

(provide 'ek9-process-test)

;;; ek9-process-test.el ends here
