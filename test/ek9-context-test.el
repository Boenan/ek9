;;; ek9-context-test.el --- Tests for ek9-context -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for contexts and namespaces.  The tests use a fake
;; kubectl script and do not need a cluster.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ek9-context)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

;;;; Arguments

(ert-deftest ek9-context-args-without-context ()
  (ek9-test--with-clean-context
    (should (null (ek9-context-args)))
    (should (null (ek9-context-args t)))))

(ert-deftest ek9-context-args-with-namespace ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev"
          ek9-context--namespace "team-a")
    (should (equal (ek9-context-args) '("--context" "dev")))
    (should (equal (ek9-context-args t)
                   '("--context" "dev" "--namespace" "team-a")))))

(ert-deftest ek9-context-args-with-all-namespaces ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev"
          ek9-context--namespace 'all)
    (should (equal (ek9-context-args t)
                   '("--context" "dev" "--all-namespaces")))))

;;;; Loading

(ert-deftest ek9-context-load-uses-kubeconfig-current-context ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (should (equal (ek9-context-current) "dev"))
    (should (equal (ek9-context-namespace) "team-a"))
    (should (equal ek9-context--contexts
                   '(("dev" . "team-a") ("prod") ("forbidden") ("nocluster"))))))

(ert-deftest ek9-context-load-keeps-valid-current-context ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "prod"
          ek9-context--namespace "web")
    (ek9-test--load-contexts)
    (should (equal (ek9-context-current) "prod"))
    (should (equal (ek9-context-namespace) "web"))))

(ert-deftest ek9-context-namespaces-uses-cache ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (let (result)
      (ek9-context-namespaces (lambda (names) (setq result names)))
      (ek9-test--wait-until (lambda () result))
      (should (equal result '("default" "kube-system" "team-a")))
      ;; The second call must not start kubectl.
      (let ((ek9-kubectl-executable "/nonexistent/kubectl"))
        (setq result nil)
        (ek9-context-namespaces (lambda (names) (setq result names)))
        (should (equal result '("default" "kube-system" "team-a")))))))

(ert-deftest ek9-context-namespaces-returns-nil-on-error ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "forbidden")
    (let ((result 'unset))
      (ek9-context-namespaces (lambda (names) (setq result names)))
      (ek9-test--wait-until (lambda () (not (eq result 'unset))))
      (should (null result)))))

;;;; Commands

(ert-deftest ek9-context-switch-namespace-sets-namespace ()
  (ek9-test--with-clean-context
    (let ((hook-runs 0))
      (add-hook 'ek9-context-changed-hook (lambda () (cl-incf hook-runs)))
      (ek9-test--switch-with-answer #'ek9-context-switch-namespace
                                    "kube-system")
      (should (equal (ek9-context-current) "dev"))
      (should (equal (ek9-context-namespace) "kube-system"))
      (should (= hook-runs 1)))))

(ert-deftest ek9-context-switch-namespace-to-all ()
  (ek9-test--with-clean-context
    (ek9-test--switch-with-answer #'ek9-context-switch-namespace
                                  ek9-context-all-namespaces-label)
    (should (eq (ek9-context-namespace) 'all))
    (should (member "--all-namespaces" (ek9-context-args t)))))

(ert-deftest ek9-context-switch-namespace-accepts-unknown-name ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (setq ek9-context--current "forbidden")
    (ek9-test--switch-with-answer #'ek9-context-switch-namespace "my-team")
    (should (equal (ek9-context-namespace) "my-team"))))

(ert-deftest ek9-context-switch-remembers-namespace-per-context ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (ek9-test--switch-with-answer #'ek9-context-switch-namespace
                                  "kube-system")
    (ek9-test--switch-with-answer #'ek9-context-switch "prod")
    (should (equal (ek9-context-current) "prod"))
    (should (equal (ek9-context-namespace) "default"))
    (ek9-test--switch-with-answer #'ek9-context-switch "dev")
    (should (equal (ek9-context-current) "dev"))
    (should (equal (ek9-context-namespace) "kube-system"))))

;;;; Read-only contexts

(ert-deftest ek9-context-read-only ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "prod"
          ek9-read-only-contexts '("prod"))
    (should (ek9-context-read-only-p))
    (should-not (ek9-context-read-only-p "dev"))
    (should-error (ek9-context-check-writable) :type 'user-error)
    (setq ek9-context--current "dev")
    (should-not (ek9-context-check-writable))))

(ert-deftest ek9-context-header-line-shows-state ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "prod"
          ek9-context--namespace 'all
          ek9-read-only-contexts '("prod"))
    (let ((line (substring-no-properties (ek9-context-header-line))))
      (should (string-match-p "Context: prod" line))
      (should (string-match-p "Namespace: <all>" line))
      (should (string-match-p "\\[read-only\\]" line)))))

(provide 'ek9-context-test)

;;; ek9-context-test.el ends here
