;;; ek9-kill-test.el --- Tests for ek9-kill-all-buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the command that kills all ek9 buffers.

;;; Code:

(require 'ert)
(require 'ek9)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--make-buffer (name setup)
  "Make the buffer NAME, call SETUP in it, and return the buffer."
  (with-current-buffer (get-buffer-create name)
    (funcall setup)
    (current-buffer)))

(defmacro ek9-test--with-kill (answer &rest body)
  "Run BODY with ANSWER for each question.
In BODY, `messages' is the list of messages, newest first."
  (declare (indent 1))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev")
     (let ((ek9-portfwd--forwards nil)
           messages)
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) ,answer))
                 ((symbol-function 'yes-or-no-p) (lambda (_) ,answer))
                 ((symbol-function 'message)
                  (lambda (format &rest args)
                    (push (apply #'format-message format args) messages))))
         ,@body))))

(ert-deftest ek9-kill-all-buffers-kills-ek9-buffers ()
  (ek9-test--with-kill t
    (let* ((list (ek9-list-open 'pods))
           (describe (ek9-test--make-buffer
                      "*ek9 describe x*"
                      (lambda () (ek9-describe-mode) (ek9-view-mode 1))))
           (yaml (ek9-test--make-buffer
                  "*ek9 yaml x*" (lambda () (fundamental-mode) (ek9-view-mode 1))))
           (logs (ek9-test--make-buffer "*ek9 logs x*" #'ek9-logs-mode))
           (shell (ek9-test--make-buffer
                   "*ek9 shell x*" (lambda () (setq ek9-exec--context "dev"))))
           (manager (ek9-port-forwards))
           (other (ek9-test--make-buffer "*ek9 notes*" #'ignore)))
      (unwind-protect
          (progn
            (ek9-kill-all-buffers)
            (dolist (buffer (list list describe yaml logs shell manager))
              (should-not (buffer-live-p buffer)))
            ;; A buffer that only has a similar name stays.
            (should (buffer-live-p other))
            (should-not ek9-list--timer)
            (should (equal (car messages) "ek9: Killed 6 buffers")))
        (kill-buffer other)))))

(ert-deftest ek9-kill-all-buffers-keeps-refused-edit ()
  (ek9-test--with-kill nil
    (let ((edit (ek9-test--make-buffer
                 "*ek9 edit x*"
                 (lambda ()
                   (setq ek9-edit--item '((kind . "Pod") (metadata (name . "x"))))
                   (ek9-edit-mode 1)
                   (insert "change")))))
      (unwind-protect
          (progn
            (ek9-kill-all-buffers)
            (should (buffer-live-p edit))
            (should (equal (car messages) "ek9: Killed 0 buffers, 1 kept")))
        (with-current-buffer edit
          (set-buffer-modified-p nil))
        (kill-buffer edit)))))

(ert-deftest ek9-kill-all-buffers-port-forwards ()
  (dolist (answer '(t nil))
    (ek9-test--with-kill answer
      (let ((forward (ek9-port-forward
                      (ek9-resource-get 'services)
                      '((metadata (name . "web") (namespace . "a"))) "8080:80")))
        (unwind-protect
            (progn
              (ek9-kill-all-buffers)
              (if answer
                  (should-not (ek9-portfwd--running-p forward))
                (should (ek9-portfwd--running-p forward))))
          (when (ek9-portfwd--running-p forward)
            (delete-process (ek9-portfwd--forward-process forward))))))))

;;;; More cases

(ert-deftest ek9-kill-all-buffers-keeps-running-shell-on-no ()
  (ek9-test--with-kill nil
    (let ((shell (ek9-test--make-buffer
                  "*ek9 shell y*" (lambda () (setq ek9-exec--context "dev")))))
      (let ((process (make-process :name "ek9-test-shell" :buffer shell
                                   :command '("sleep" "30"))))
        (set-process-query-on-exit-flag process t)
        (unwind-protect
            (progn
              (ek9-kill-all-buffers)
              (should (buffer-live-p shell))
              (should (string-match-p "1 kept" (car messages))))
          (delete-process process)
          (kill-buffer shell))))))

(ert-deftest ek9-kill-all-buffers-quit-keeps-one ()
  (ek9-test--with-kill t
    (let ((first (ek9-test--make-buffer "*ek9 logs a*" #'ek9-logs-mode))
          (second (ek9-test--make-buffer "*ek9 logs b*" #'ek9-logs-mode)))
      (with-current-buffer first
        (add-hook 'kill-buffer-query-functions (lambda () (signal 'quit nil))
                  nil t))
      (unwind-protect
          (progn
            (ek9-kill-all-buffers)
            (should (buffer-live-p first))
            (should-not (buffer-live-p second))
            (should (equal (car messages) "ek9: Killed 1 buffer, 1 kept")))
        (with-current-buffer first
          (setq kill-buffer-query-functions nil))
        (kill-buffer first)))))

(ert-deftest ek9-kill-all-buffers-ignores-dead-buffers ()
  (ek9-test--with-kill t
    (let ((logs (ek9-test--make-buffer "*ek9 logs c*" #'ek9-logs-mode)))
      ;; The buffer dies while the port-forward question is open.
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) (kill-buffer logs) t)))
        (ek9-port-forward (ek9-resource-get 'services)
                          '((metadata (name . "web") (namespace . "a"))) "8081:80")
        (ek9-kill-all-buffers))
      (should (equal (car messages) "ek9: Killed 0 buffers")))))

(ert-deftest ek9-kill-all-buffers-keeps-kubectl-output-buffers ()
  (ek9-test--with-kill t
    (ek9-test--with-fake-kubectl
      (let* ((done nil)
             (process (ek9-process-run '("version") (lambda (_) (setq done t)))))
        (ek9-kill-all-buffers)
        (ek9-test--wait-until (lambda () done))
        (should-not (ek9-process-live-p process))))))

(ert-deftest ek9-edit-query-kill-while-apply-runs ()
  (let (prompt)
    (with-temp-buffer
      (ek9-edit-mode 1)
      (setq ek9-edit--process (make-process :name "ek9-test-apply"
                                            :command '("sleep" "30")))
      (unwind-protect
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (text) (setq prompt text) nil)))
            (should-not (ek9-edit--query-kill))
            (should (equal prompt
                           "The kubectl command still applies the changes.  Kill the buffer? ")))
        (delete-process ek9-edit--process)))))

(ert-deftest ek9-exec-shell-asks-before-kill ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (let* ((ek9-exec-terminal 'term)
           (buffer (ek9-shell '((metadata (name . "p") (namespace . "a"))
                                (spec (containers ((name . "c"))))
                                (status (phase . "Running")))
                              "c")))
      (unwind-protect
          (when-let* ((process (get-buffer-process buffer)))
            (should (process-query-on-exit-flag process)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(ert-deftest ek9-buffer-p-dead-buffer ()
  ;; A loop over a buffer list can meet a buffer that died meanwhile.
  (let ((buffer (generate-new-buffer "*ek9 test dead*")))
    (with-current-buffer buffer
      (ek9-list-mode))
    (should (ek9--buffer-p buffer))
    (kill-buffer buffer)
    (should-not (ek9--buffer-p buffer))))

(provide 'ek9-kill-test)

;;; ek9-kill-test.el ends here
