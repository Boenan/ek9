;;; ek9-resource-test.el --- Tests for ek9-resource -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for resource definitions, durations, and table cells.

;;; Code:

(require 'ert)
(require 'ek9-resource)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

;;;; Registry

(ert-deftest ek9-resource-define-and-get ()
  (let ((ek9-resource--registry nil))
    (ek9-define-resource things
      :kind "things"
      :namespaced nil
      :columns '(("NAME" 10 (metadata name))))
    (let ((resource (ek9-resource-get 'things)))
      (should (equal (ek9-resource-kind resource) "things"))
      (should-not (ek9-resource-namespaced resource)))
    (should-error (ek9-resource-get 'unknown))))

(ert-deftest ek9-resource-item-id ()
  (should (equal (ek9-resource-item-id
                  '((metadata (name . "a") (namespace . "ns"))))
                 "ns/a"))
  (should (equal (ek9-resource-item-id '((metadata (name . "node-1"))))
                 "node-1")))

(ert-deftest ek9-resource-column-string-handles-nil-and-numbers ()
  (let ((item '((metadata (name . "a")) (count . 3))))
    (should (equal (ek9-resource-column-string '("X" 5 (count)) item) "3"))
    (should (equal (ek9-resource-column-string '("X" 5 (missing)) item) ""))))

;;;; Age

(ert-deftest ek9-resource-human-duration-matches-kubectl ()
  (dolist (case '((-5 . "<invalid>") (-2 . "<invalid>") (-1.5 . "0s")
                  (-1 . "0s") (0 . "0s") (59 . "59s") (119 . "119s")
                  (120 . "2m") (330 . "5m30s") (600 . "10m") (10740 . "179m")
                  (10800 . "3h") (12600 . "3h30m") (28800 . "8h")
                  (172800 . "2d") (183600 . "2d3h") (691200 . "8d")
                  (63072000 . "2y") (65664000 . "2y30d")
                  (252460800 . "8y")))
    (should (equal (ek9-resource-human-duration (car case)) (cdr case)))))

;;;; Table cells

(ert-deftest ek9-resource-cell-by-title ()
  (let ((item (car (ek9-proxy-table-items (ek9-test--table "pods")))))
    (should (equal (ek9-resource-cell item "STATUS") "Running"))
    (should (equal (ek9-resource-cell item "READY") "1/1"))
    (should (equal (ek9-resource-cell item "RESTARTS") "0"))
    (should-not (ek9-resource-cell item "MISSING"))
    ;; The item is also the object of the row.
    (should (equal (ek9-resource-item-id item) "default/nginx-7c5b8d65b8-abcde"))
    ;; An item without a table has no cells.
    (should-not (ek9-resource-cell (ek9-test--pod "db-0") "STATUS"))))

(provide 'ek9-resource-test)

;;; ek9-resource-test.el ends here
