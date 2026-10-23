;;; ek9-watch-test.el --- Tests for the watch of ek9 list buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the live updates of list buffers.  The fake proxy
;; sends the watch events that a test puts in a directory, see
;; test/fixtures/fake-proxy.el.

;;; Code:

(require 'ert)
(require 'ek9-list)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defmacro ek9-test--with-watched-pods (dir &rest body)
  "Open the pods list with a watch, and run BODY in the pods buffer.
DIR is bound to the watch directory of the fake proxy.  BODY starts
after the first watch request.  The timer does not tick in BODY: call
`ek9-list--tick' to make a tick."
  (declare (indent 1))
  `(ek9-test--with-watch-dir ,dir
     (ek9-test--with-clean-context
       (when (timerp ek9-list--timer)
         (cancel-timer ek9-list--timer)
         (setq ek9-list--timer nil))
       (let* ((ek9-watch t)
              (ek9-refresh-interval 3600)
              (ek9-context-changed-hook (list #'ek9-list--context-changed))
              (buffer (ek9-list-open 'pods)))
         (unwind-protect
             (with-current-buffer buffer
               (ek9-test--wait-until (lambda () ek9-list--loaded) 10)
               (ek9-test--wait-until (lambda () (ek9-test--watch-requests ,dir)) 10)
               ,@body)
           (kill-buffer buffer)
           (when (timerp ek9-list--timer)
             (cancel-timer ek9-list--timer)
             (setq ek9-list--timer nil)))))))

(defun ek9-test--new-pod (name &optional phase)
  "Return a small pod object NAME in namespace team-a with PHASE."
  `((metadata (name . ,name) (namespace . "team-a") (uid . ,(concat "uid-" name)))
    (spec (containers . [((name . "main"))]))
    (status (phase . ,(or phase "Pending")))))

(defun ek9-test--status-of (id)
  "Return the STATUS cell of the row ID in the current list buffer."
  (when-let* ((entry (assoc id tabulated-list-entries))
              (index (seq-position (mapcar #'car tabulated-list-format) "STATUS")))
    (substring-no-properties (aref (cadr entry) index))))

(ert-deftest ek9-watch-applies-events ()
  (ek9-test--with-watched-pods dir
    (should (ek9-proxy-live-p ek9-list--watch))
    (should (equal ek9-list--resource-version "100"))
    ;; The watch starts from the resourceVersion of the list.
    (should (string-match-p "&resourceVersion=100&"
                            (car (ek9-test--watch-requests dir))))
    (goto-char (ek9-list--find-id "default/api-5f9c-xyz"))
    (ek9-test--put-watch-file
     dir "1.json" (ek9-test--watch-event "ADDED" (ek9-test--new-pod "new-1") "101"
                                         ["new-1" "0/1" "Pending" "0" "1s"]))
    (ek9-test--wait-until (lambda () (assoc "team-a/new-1" tabulated-list-entries)))
    (should (= (length tabulated-list-entries) 9))
    (should (equal (ek9-test--status-of "team-a/new-1") "Pending"))
    (should (equal ek9-list--resource-version "101"))
    ;; Point stays on its row.
    (should (equal (tabulated-list-get-id) "default/api-5f9c-xyz"))
    (ek9-test--put-watch-file
     dir "2.json" (ek9-test--watch-event "MODIFIED" (ek9-test--new-pod "new-1" "Running")
                                         "102" ["new-1" "1/1" "Running" "0" "2s"]))
    (ek9-test--wait-until (lambda () (equal (ek9-test--status-of "team-a/new-1") "Running")))
    (should (equal (ek9-resource-path (gethash "team-a/new-1" ek9-list--index)
                                      '(status phase))
                   "Running"))
    (ek9-test--put-watch-file
     dir "3.json" (ek9-test--watch-event "DELETED" (ek9-test--new-pod "new-1") "103"))
    (ek9-test--wait-until (lambda () (not (assoc "team-a/new-1" tabulated-list-entries))))
    (should (= (length tabulated-list-entries) 8))
    (should-not (gethash "team-a/new-1" ek9-list--index))
    ;; A bookmark changes only the resourceVersion.
    (ek9-test--put-watch-file
     dir "4.json" (ek9-test--watch-event "BOOKMARK" '((metadata (name . ""))) "150"
                                         ["" "" "" "" "<unknown>"]))
    (ek9-test--wait-until (lambda () (equal ek9-list--resource-version "150")))
    (should (= (length ek9-list--items) 8))))

(ert-deftest ek9-watch-continues-after-normal-end ()
  (ek9-test--with-watched-pods dir
    (let ((ek9-list--min-watch-time 0))
      (ek9-test--put-watch-file
       dir "1.json" (ek9-test--watch-event "ADDED" (ek9-test--new-pod "new-1") "205"))
      (ek9-test--wait-until (lambda () (assoc "team-a/new-1" tabulated-list-entries)))
      (ek9-test--put-watch-file dir "2.close")
      (ek9-test--wait-until (lambda () (= (length (ek9-test--watch-requests dir)) 2)))
      ;; The new watch continues from the last event.  It does not read
      ;; the full list again.
      (should (string-match-p "&resourceVersion=205&"
                              (nth 1 (ek9-test--watch-requests dir))))
      (should (assoc "team-a/new-1" tabulated-list-entries))
      (should (= ek9-list--watch-failures 0)))))

(ert-deftest ek9-watch-quick-end-is-a-failure ()
  (ek9-test--with-watched-pods dir
    (ek9-test--put-watch-file dir "1.close")
    (ek9-test--wait-until (lambda () (= ek9-list--watch-failures 1)))
    (should-not ek9-list--watch)
    (should (equal ek9-list--error "The API server ended the watch at once"))
    ;; The next tick reads the full list, and starts a new watch.
    (ek9-list--tick)
    (ek9-test--wait-until (lambda () (= (length (ek9-test--watch-requests dir)) 2)))
    (should-not ek9-list--error)))

(ert-deftest ek9-watch-gone-reads-list-again ()
  (ek9-test--with-watched-pods dir
    (ek9-test--put-watch-file
     dir "1.json" (ek9-test--watch-event "ADDED" (ek9-test--new-pod "new-1") "300"))
    (ek9-test--wait-until (lambda () (equal ek9-list--resource-version "300")))
    (ek9-test--put-watch-file dir "2.gone")
    (ek9-test--wait-until (lambda () (= (length (ek9-test--watch-requests dir)) 2)))
    ;; The new watch starts from the resourceVersion of the new list.
    (should (string-match-p "&resourceVersion=100&"
                            (nth 1 (ek9-test--watch-requests dir))))
    ;; The full list does not have the pod of the event.
    (ek9-test--wait-until (lambda () (not (assoc "team-a/new-1" tabulated-list-entries))))
    ;; A 410 counts as a failure, but it does not show an error.
    (should (= ek9-list--watch-failures 1))
    (should (ek9-proxy-live-p ek9-list--watch))
    (should-not ek9-list--error)))

(ert-deftest ek9-watch-failures-stop-the-watch ()
  (ek9-test--with-watch-dir dir
    (ek9-test--put-watch-file dir "fail")
    (ek9-test--with-clean-context
      (when (timerp ek9-list--timer)
        (cancel-timer ek9-list--timer)
        (setq ek9-list--timer nil))
      (let* ((ek9-watch t)
             (ek9-refresh-interval 3600)
             (buffer (ek9-list-open 'pods)))
        (unwind-protect
            (with-current-buffer buffer
              (ek9-test--wait-until (lambda () (= ek9-list--watch-failures 1)) 10)
              (should (string-match-p "fake watch failure" ek9-list--error))
              ;; Each tick reads the full list and tries the watch again.
              (dolist (count '(2 3))
                (ek9-list--tick)
                (ek9-test--wait-until (lambda () (= ek9-list--watch-failures count))))
              (should (ek9-list--watch-stopped-p))
              ;; Now a tick reads the full list, without a watch.
              (ek9-list--tick)
              (ek9-test--wait-until (lambda () (not (ek9-proxy-live-p ek9-list--process))))
              (accept-process-output nil 0.5)
              (should (= (length (ek9-test--watch-requests dir)) 3))
              (should-not ek9-list--error)
              (should (string-match-p "Watch failed"
                                      (substring-no-properties (ek9-list--tab-line))))
              ;; g tries the watch again.
              (delete-file (expand-file-name "fail" dir))
              (ek9-list-refresh nil t)
              (ek9-test--wait-until (lambda () (ek9-proxy-live-p ek9-list--watch)))
              (should (= ek9-list--watch-failures 0))
              (should-not (ek9-list--watch-stopped-p)))
          (kill-buffer buffer)
          (when (timerp ek9-list--timer)
            (cancel-timer ek9-list--timer)
            (setq ek9-list--timer nil)))))))

(ert-deftest ek9-watch-stops-in-hidden-buffer ()
  (ek9-test--with-watched-pods dir
    (should (ek9-proxy-live-p ek9-list--watch))
    (cl-letf (((symbol-function 'get-buffer-window) #'ignore))
      (ek9-list--tick))
    (should-not ek9-list--watch)
    ;; A tick reads the full list of the visible buffer, and starts a
    ;; new watch.
    (ek9-list--tick)
    (ek9-test--wait-until (lambda () (ek9-proxy-live-p ek9-list--watch)))
    (should (= (length (ek9-test--watch-requests dir)) 2))))

(ert-deftest ek9-watch-reloads-after-interval ()
  (ek9-test--with-watched-pods dir
    (let ((ek9-watch-reload-interval 30))
      (ek9-list--tick)
      (should-not (ek9-proxy-live-p ek9-list--process))
      (setq ek9-list--loaded-time (- (float-time) 31))
      (ek9-list--tick)
      (should (ek9-proxy-live-p ek9-list--process))
      ;; The new list starts a new watch.
      (ek9-test--wait-until (lambda () (= (length (ek9-test--watch-requests dir)) 2))))))

(ert-deftest ek9-watch-option-off-polls ()
  (ek9-test--with-watch-dir dir
    (ek9-test--with-clean-context
      (let* ((ek9-watch nil)
             (buffer (ek9-list-open 'pods)))
        (unwind-protect
            (with-current-buffer buffer
              (ek9-test--wait-until (lambda () ek9-list--loaded) 10)
              (accept-process-output nil 0.5)
              (should-not ek9-list--watch)
              (should-not (ek9-test--watch-requests dir))
              (should-not (string-match-p "Watch"
                                          (substring-no-properties (ek9-list--tab-line)))))
          (kill-buffer buffer)
          (when (timerp ek9-list--timer)
            (cancel-timer ek9-list--timer)
            (setq ek9-list--timer nil)))))))

(ert-deftest ek9-watch-kill-buffer-stops-watch ()
  (let (watch)
    (ek9-test--with-watched-pods dir
      (setq watch ek9-list--watch)
      (should (ek9-proxy-live-p watch)))
    (should-not (ek9-proxy-live-p watch))))

(ert-deftest ek9-watch-change-item-keeps-owner-filter ()
  (with-temp-buffer
    (let* ((owned (lambda (name uid)
                    `((metadata (name . ,name) (namespace . "a")
                                (ownerReferences ((controller . t) (uid . ,uid)))))))
           (a (funcall owned "a" "rs-1"))
           (b (funcall owned "b" "rs-1")))
      (setq ek9-list--owner-uid "rs-1"
            ek9-list--items (list a b)
            ek9-list--index (make-hash-table :test #'equal))
      (puthash "a/a" a ek9-list--index)
      (puthash "a/b" b ek9-list--index)
      ;; A pod of another owner does not show.
      (ek9-list--change-item "ADDED" (funcall owned "c" "rs-2"))
      (should (equal (mapcar #'ek9-resource-item-id ek9-list--items) '("a/a" "a/b")))
      ;; A pod that changes its owner goes away.
      (ek9-list--change-item "MODIFIED" (funcall owned "a" "rs-2"))
      (should (equal (mapcar #'ek9-resource-item-id ek9-list--items) '("a/b")))
      (should-not (gethash "a/a" ek9-list--index))
      ;; A change replaces the item in its position.
      (let ((new-b (funcall owned "b" "rs-1")))
        (ek9-list--change-item "ADDED" (funcall owned "d" "rs-1"))
        (ek9-list--change-item "MODIFIED" new-b)
        (should (equal (mapcar #'ek9-resource-item-id ek9-list--items) '("a/b" "a/d")))
        (should (eq (car ek9-list--items) new-b))
        (should (eq (gethash "a/b" ek9-list--index) new-b))))))

(provide 'ek9-watch-test)

;;; ek9-watch-test.el ends here
