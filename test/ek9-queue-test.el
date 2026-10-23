;;; ek9-queue-test.el --- Tests for the ek9 process queue -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the queue of short kubectl commands.  The fake
;; kubectl has a "sleep SECONDS" command, so that a test can control
;; how long a job runs.

;;; Code:

(require 'ert)
(require 'ek9)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defvar ek9-test--log nil
  "Events of the queue tests, oldest first.")

(defmacro ek9-test--with-queue (max &rest body)
  "Run BODY with an empty queue and MAX processes at the same time.
In BODY, `ek9-test--log' is a list of events, oldest first.  Wait
until all jobs are done after BODY."
  (declare (indent 1))
  `(ek9-test--with-fake-kubectl
     (let ((ek9-process--queue nil)
           (ek9-process--running nil)
           (ek9-process-max-processes ,max)
           (ek9-process-timeout 30)
           (ek9-test--log nil))
       (unwind-protect
           (progn ,@body)
         (ek9-test--wait-until
          (lambda () (and (null ek9-process--queue) (null ek9-process--running)))
          10)))))

(defun ek9-test--sleep-job (seconds name)
  "Start a job that sleeps SECONDS.  Add NAME to `ek9-test--log' at its end.
After an error, add (NAME CODE TEXT)."
  (ek9-process-run (list "sleep" (number-to-string seconds))
                   (lambda (_)
                     (setq ek9-test--log (append ek9-test--log (list name))))
                   (lambda (code text)
                     (setq ek9-test--log
                           (append ek9-test--log (list (list name code text)))))))

(ert-deftest ek9-queue-limit ()
  (ek9-test--with-queue 2
    (let* ((a (ek9-test--sleep-job 0.3 'a))
           (b (ek9-test--sleep-job 0.3 'b))
           (c (ek9-test--sleep-job 0.1 'c)))
      (should (= (length ek9-process--running) 2))
      (should (equal ek9-process--queue (list c)))
      (should (ek9-process-queued-p c))
      (should (ek9-process-live-p c))
      (should-not (ek9-process-queued-p a))
      (ek9-test--wait-until (lambda () (= (length ek9-test--log) 3)))
      ;; c starts after a or b ends, so it ends last.
      (should (eq (nth 2 ek9-test--log) 'c))
      (dolist (job (list a b c))
        (should-not (ek9-process-live-p job))))))

(ert-deftest ek9-queue-user-before-background ()
  (ek9-test--with-queue 1
    (ek9-test--sleep-job 0.3 'first)
    (let ((ek9-process-priority 'background))
      (ek9-test--sleep-job 0 'background-1)
      (ek9-test--sleep-job 0 'background-2))
    (ek9-test--sleep-job 0 'user-1)
    (ek9-test--sleep-job 0 'user-2)
    (ek9-test--wait-until (lambda () (= (length ek9-test--log) 5)))
    (should (equal ek9-test--log '(first user-1 user-2 background-1 background-2)))))

(ert-deftest ek9-queue-prioritize ()
  (ek9-test--with-queue 1
    (ek9-test--sleep-job 0.3 'first)
    (let ((waiting (let ((ek9-process-priority 'background))
                     (ek9-test--sleep-job 0 'refresh))))
      (ek9-test--sleep-job 0 'user)
      (ek9-process-prioritize waiting)
      (ek9-test--wait-until (lambda () (= (length ek9-test--log) 3)))
      ;; The refresh is now a user job after the user job before it.
      (should (equal ek9-test--log '(first user refresh))))))

(ert-deftest ek9-queue-cancel-waiting-job ()
  (ek9-test--with-queue 1
    (ek9-test--sleep-job 0.2 'first)
    (let ((waiting (ek9-test--sleep-job 0 'cancelled)))
      (ek9-process-cancel waiting)
      (should-not (ek9-process-live-p waiting))
      (should-not ek9-process--queue)
      (ek9-test--wait-until (lambda () (null ek9-process--running)))
      (accept-process-output nil 0.2)
      (should (equal ek9-test--log '(first))))))

(ert-deftest ek9-queue-cancel-running-job ()
  (ek9-test--with-queue 1
    (let ((running (ek9-test--sleep-job 5 'cancelled)))
      (ek9-test--sleep-job 0 'next)
      (ek9-process-cancel running)
      (ek9-test--wait-until (lambda () (equal ek9-test--log '(next))))
      (should-not (ek9-process-live-p running)))))

(ert-deftest ek9-queue-timeout ()
  (ek9-test--with-queue 1
    (let ((ek9-process-timeout 0.3))
      (ek9-test--sleep-job 5 'slow)
      (ek9-test--sleep-job 0 'next))
    (ek9-test--wait-until (lambda () (= (length ek9-test--log) 2)))
    (should (equal (car ek9-test--log) '(slow timeout "Timeout after 0.3 s")))
    (should (eq (cadr ek9-test--log) 'next))))

(ert-deftest ek9-queue-start-error-of-waiting-job ()
  (ek9-test--with-queue 1
    (let ((copy (make-temp-file "ek9-kubectl-")))
      (copy-file ek9-kubectl-executable copy t)
      (set-file-modes copy #o755)
      (ek9-test--sleep-job 0.2 'first)
      (let ((waiting (let ((ek9-kubectl-executable copy))
                       (ek9-test--sleep-job 0 'broken))))
        ;; kubectl is gone when the waiting job starts.
        (delete-file copy)
        (ek9-test--wait-until (lambda () (= (length ek9-test--log) 2)))
        (should (eq (car (cadr ek9-test--log)) 'broken))
        (should-not (cadr (cadr ek9-test--log)))
        (should-not (ek9-process-live-p waiting))))))

(ert-deftest ek9-queue-start-error-of-own-job ()
  (ek9-test--with-queue 1
    (let ((ek9-kubectl-executable "/nonexistent/kubectl"))
      (should-error (ek9-process-run '("version") #'ignore))
      (should-not ek9-process--queue)
      (should-not ek9-process--running))))

(ert-deftest ek9-queue-accepts-processes ()
  (let ((process (make-process :name "ek9-test-sleep" :command '("sleep" "5"))))
    (should (ek9-process-live-p process))
    (should-not (ek9-process-queued-p process))
    (ek9-process-cancel process)
    (should-not (ek9-process-live-p process))))

(ert-deftest ek9-queue-killed-buffer-cancels-request ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (let ((buffer (get-buffer-create "*ek9 test queue*"))
          request)
      ;; `with-temp-buffer' does not run `kill-buffer-hook'.
      (with-current-buffer buffer
        (ek9-list-mode)
        (setq ek9-list--resource (ek9-resource-get 'pods))
        (ek9-list-refresh)
        (setq request ek9-list--process))
      (should (ek9-proxy-live-p request))
      (kill-buffer buffer)
      (should-not (ek9-proxy-live-p request)))))

(ert-deftest ek9-queue-waiting-job-keeps-environment ()
  (ek9-test--with-queue 1
    (let (output)
      (ek9-test--sleep-job 0.2 'first)
      (let ((process-environment (cons "KUBECONFIG=/caller" process-environment)))
        (ek9-process-run '("kubeconfig") (lambda (text) (setq output text))))
      ;; The job starts from a sentinel, in another environment.
      (let ((process-environment (cons "KUBECONFIG=/other" process-environment)))
        (ek9-test--wait-until (lambda () output)))
      (should (equal output "KUBECONFIG=/caller\n")))))

(ert-deftest ek9-queue-callback-error-does-not-stop-queue ()
  (ek9-test--with-queue 1
    (ek9-process-run '("sleep" "0.1") (lambda (_) (error "Broken callback")))
    (ek9-test--sleep-job 0 'next)
    (ek9-test--wait-until (lambda () (equal ek9-test--log '(next))))))

(ert-deftest ek9-queue-callback-submits-and-cancels ()
  (ek9-test--with-queue 1
    (let (waiting)
      (setq waiting (ek9-test--sleep-job 0.3 'cancelled))
      (ek9-process-run '("sleep" "0.1")
                       (lambda (_)
                         ;; A callback can start and cancel jobs.
                         (ek9-test--sleep-job 0 'from-callback)
                         (ek9-process-cancel waiting)))
      (ek9-test--wait-until (lambda () (equal ek9-test--log '(cancelled))) 2)
      (ek9-test--wait-until
       (lambda () (equal ek9-test--log '(cancelled from-callback)))))))

(ert-deftest ek9-queue-failed-input-calls-error-once ()
  (ek9-test--with-queue 1
    (let ((calls 0))
      ;; "fail" exits at once and does not read its input.
      (ek9-process-run '("fail") #'ignore
                       (lambda (&rest _) (setq calls (1+ calls)))
                       (make-string 300000 ?x))
      (ek9-test--wait-until (lambda () (null ek9-process--running)))
      (accept-process-output nil 0.2)
      (should (= calls 1)))))

(ert-deftest ek9-queue-timeout-text-for-writes ()
  (let ((ek9-process-timeout 60))
    (should (equal (ek9-process-timeout-text "Delete Pod a/b")
                   (concat "Delete Pod a/b: no answer after 60 s.  The change "
                           "can be done or not.  Refresh to check it")))))

(provide 'ek9-queue-test)

;;; ek9-queue-test.el ends here
