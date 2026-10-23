;;; ek9-transient-test.el --- Tests for ek9-transient -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the ek9 action menu.  The tests do not open the
;; menu.  They read the suffix definitions with the public function
;; `transient-get-suffix' and call their predicates.

;;; Code:

(require 'ert)
(require 'ek9)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defmacro ek9-test--in-list-of (name &rest body)
  "Run BODY in a list buffer of the resource type NAME in context dev."
  (declare (indent 1))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev"
           ek9-context--namespace "team-a")
     (with-temp-buffer
       (ek9-list-mode)
       (setq ek9-list--resource (ek9-resource-get ,name))
       ,@body)))

(defconst ek9-test--menu-keys
  '(":" "RET" "^" "n" "c" "v" "/" "o" "w" "g" "Q" "d" "y" "l" "x" "E" "F"
    "e" "s" "f" "S" "R" "D")
  "All keys of `ek9-dispatch'.")

(defun ek9-test--apt-p (key)
  "Return non-nil if the menu key KEY is available in the current buffer."
  (let* ((plist (nth 2 (transient-get-suffix 'ek9-dispatch key)))
         (if-not (plist-get plist :inapt-if-not))
         (if (plist-get plist :inapt-if)))
    (and (or (null if-not) (funcall if-not))
         (not (and if (funcall if))))))

(defun ek9-test--inapt-keys ()
  "Return the sorted menu keys that are not available."
  (sort (seq-remove #'ek9-test--apt-p ek9-test--menu-keys) #'string<))

(ert-deftest ek9-transient-keys-match-keymap ()
  (should (eq (keymap-lookup ek9-list-mode-map "?") #'ek9-dispatch))
  (dolist (key ek9-test--menu-keys)
    (should (eq (keymap-lookup ek9-list-mode-map key)
                (plist-get (nth 2 (transient-get-suffix 'ek9-dispatch key))
                           :command)))))

(ert-deftest ek9-transient-title-shows-context ()
  (ek9-test--in-list-of 'pods
    (should (string-match-p "Context: dev  Namespace: team-a  View: pods"
                            (substring-no-properties (ek9-transient--title))))
    (setq ek9-read-only-contexts '("dev"))
    (should (string-match-p "\\[read-only\\]"
                            (substring-no-properties (ek9-transient--title))))))

(ert-deftest ek9-transient-inapt-for-pods ()
  (ek9-test--in-list-of 'pods
    (should (equal (ek9-test--inapt-keys) '("R" "S" "^" "x")))))

(ert-deftest ek9-transient-inapt-for-deployments ()
  (ek9-test--in-list-of 'deployments
    (should (equal (ek9-test--inapt-keys) '("^" "l" "s" "x")))
    (should (ek9-test--apt-p "RET"))))

(ert-deftest ek9-transient-drill-down-buffer ()
  (ek9-test--in-list-of 'pods
    (let ((parent (generate-new-buffer " *ek9 test parent*")))
      (unwind-protect
          (progn
            (setq ek9-list--parent parent
                  ek9-list--namespace-override "team-a")
            (should (ek9-test--apt-p "^"))
            (should-not (ek9-test--apt-p "n")))
        (kill-buffer parent)))))

(ert-deftest ek9-transient-read-only-context ()
  (ek9-test--in-list-of 'deployments
    (setq ek9-read-only-contexts '("dev"))
    (let ((inapt (ek9-test--inapt-keys)))
      (dolist (key '("D" "R" "S" "e"))
        (should (member key inapt)))
      ;; Port-forward and the views stay available.
      (dolist (key '("f" "d" "y"))
        (should-not (member key inapt))))))

(ert-deftest ek9-transient-no-context ()
  (ek9-test--in-list-of 'deployments
    (setq ek9-context--current nil)
    (dolist (key '("D" "R" "S" "e"))
      (should-not (ek9-test--apt-p key)))))

(provide 'ek9-transient-test)

;;; ek9-transient-test.el ends here
