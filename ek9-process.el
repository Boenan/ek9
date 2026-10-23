;;; ek9-process.el --- Async kubectl runner for ek9 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Fabian Waxin

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file runs `kubectl' as an asynchronous subprocess.  The caller
;; gives a success callback and an optional error callback.  Emacs does
;; not block while `kubectl' runs.
;;
;; Short commands go into a queue.  At most `ek9-process-max-processes'
;; of them run at the same time.  A command that the user started goes
;; before the background refreshes.  `ek9-process-timeout' stops a
;; command that hangs.  Log streams use `ek9-process-stream' and are
;; not in the queue.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defcustom ek9-kubectl-executable "kubectl"
  "Name or path of the kubectl executable."
  :type 'string
  :group 'ek9)

(defun ek9-process-check-kubectl ()
  "Signal a `user-error' if Emacs cannot find `ek9-kubectl-executable'."
  (unless (executable-find ek9-kubectl-executable)
    (user-error "ek9: Cannot find kubectl (%s).  Install kubectl, or set `ek9-kubectl-executable'"
                ek9-kubectl-executable)))

(defun ek9-process-timeout-text (question)
  "Return the text for a write QUESTION that got no answer in time.
The API server can still do the change after ek9 stops kubectl."
  (format "%s: no answer after %s s.  The change can be done or not.  Refresh to check it"
          question ek9-process-timeout))

(defun ek9-process-error-summary (stderr)
  "Return the last non-empty line of the kubectl error text STDERR.
kubectl can write many log lines before the main error message.  The
last line usually contains the main error message."
  (or (car (last (split-string stderr "\n" t "[ \t\r]+")))
      ""))

(defun ek9-process--default-error (exit-code stderr)
  "Show a message for a kubectl failure with EXIT-CODE and STDERR."
  (message "ek9: kubectl failed (exit %s): %s"
           exit-code (ek9-process-error-summary stderr)))

(defun ek9-process--drain (proc)
  "Read all remaining output of the pipe process PROC."
  (when proc
    (while (accept-process-output proc 0.05))))

(defcustom ek9-process-max-processes 4
  "Maximum number of short kubectl commands that run at the same time.
More commands wait in a queue.  Log streams, port-forwards, and shells
are not in the queue, because they run for a long time."
  :type 'natnum
  :group 'ek9)

(defcustom ek9-process-timeout 60
  "Number of seconds after which ek9 stops a short kubectl command.
A command that hangs, for example when the network is down, keeps
its place in the queue.  The value must be long enough for a login
plugin of kubectl.  If nil, ek9 does not stop a command."
  :type '(choice (number :tag "Seconds") (const :tag "No timeout" nil))
  :group 'ek9)

(defvar ek9-process-priority 'user
  "Priority of the kubectl commands that start in this dynamic scope.
The value `user' is for a command that the user started.  It goes to
the front of the queue.  The value `background' is for a command
that a timer started, for example a refresh.  It goes to the back.")

(cl-defstruct (ek9-process-job (:constructor ek9-process--make-job)
                               (:copier nil))
  "A short kubectl command in the queue."
  args parser on-success on-error input priority
  process timer timeout
  (environment nil :documentation "The `process-environment' of the call.")
  (path nil :documentation "The `exec-path' of the call.")
  (directory nil :documentation "The `default-directory' of the call.")
  (executable nil :documentation "The `ek9-kubectl-executable' of the call.")
  (state 'queued :documentation "One of queued, running, done, or cancelled.")
  timed-out)

(defvar ek9-process--queue nil
  "Jobs that wait, in the order in which they start.")

(defvar ek9-process--running nil
  "Jobs that run now.")

;;;; Queue

(defun ek9-process--enqueue (job)
  "Add JOB to the queue in the place of its priority.
A user job goes after the other user jobs, but before the background
jobs.  A background job goes to the end."
  (if (eq (ek9-process-job-priority job) 'background)
      (setq ek9-process--queue (append ek9-process--queue (list job)))
    (let ((before (seq-take-while
                   (lambda (other)
                     (not (eq (ek9-process-job-priority other) 'background)))
                   ek9-process--queue)))
      (setq ek9-process--queue
            (append before (list job)
                    (nthcdr (length before) ek9-process--queue))))))

(defun ek9-process--next (&optional caller-job)
  "Start the waiting jobs while fewer than the maximum run.
If the start of a job fails, end the job and call its error callback.
For CALLER-JOB, signal the error again, so that the caller sees it."
  (while (and ek9-process--queue
              (< (length ek9-process--running)
                 (max 1 ek9-process-max-processes)))
    (let ((job (pop ek9-process--queue)))
      (condition-case err
          (ek9-process--start job)
        (error
         (setf (ek9-process-job-state job) 'done)
         (setq ek9-process--running (delq job ek9-process--running))
         (if (eq job caller-job)
             (signal (car err) (cdr err))
           ;; An error in the callback must not stop the queue.
           (with-demoted-errors "ek9: Error in a kubectl callback: %S"
             (funcall (or (ek9-process-job-on-error job)
                          #'ek9-process--default-error)
                      nil (error-message-string err)))))))))

(defun ek9-process--finish (job)
  "Call the callback of the JOB that stopped, then start the next job."
  (let ((proc (ek9-process-job-process job))
        (stdout (process-buffer (ek9-process-job-process job)))
        (stderr (process-get (ek9-process-job-process job) 'ek9-stderr)))
    (setq ek9-process--running (delq job ek9-process--running))
    (when (timerp (ek9-process-job-timer job))
      (cancel-timer (ek9-process-job-timer job)))
    (unwind-protect
        (unless (memq (ek9-process-job-state job) '(cancelled done))
          (setf (ek9-process-job-state job) 'done)
          ;; An error in a callback is a message, as for other process
          ;; sentinels.  It must not stop the queue.
          (with-demoted-errors "ek9: Error in a kubectl callback: %S"
            (ek9-process--drain (get-buffer-process stderr))
            (cond
             ((ek9-process-job-timed-out job)
              (funcall (or (ek9-process-job-on-error job)
                           #'ek9-process--default-error)
                       'timeout (format "Timeout after %s s"
                                        (ek9-process-job-timeout job))))
             ((and (eq (process-status proc) 'exit)
                   (zerop (process-exit-status proc)))
              (funcall (ek9-process-job-on-success job)
                       (with-current-buffer stdout
                         (goto-char (point-min))
                         (funcall (ek9-process-job-parser job)))))
             (t
              (funcall (or (ek9-process-job-on-error job)
                           #'ek9-process--default-error)
                       (process-exit-status proc)
                       (with-current-buffer stderr
                         (string-trim (buffer-string))))))))
      (kill-buffer stdout)
      (kill-buffer stderr)
      (ek9-process--next))))

(defun ek9-process--start (job)
  "Start the kubectl process of JOB.
Use the environment, the path, the directory, and the executable of
the call, not of the buffer that is current now: a job that waited
starts from the sentinel of another job.  With direnv, for example,
another buffer can have another KUBECONFIG."
  (let* ((process-environment (ek9-process-job-environment job))
         (exec-path (ek9-process-job-path job))
         (default-directory (ek9-process-job-directory job))
         (stdout (generate-new-buffer " *ek9-stdout*" t))
         (stderr (generate-new-buffer " *ek9-stderr*" t))
         (proc
          (condition-case err
              (make-process
               :name "ek9-kubectl"
               :buffer stdout
               :stderr stderr
               :command (cons (ek9-process-job-executable job)
                              (ek9-process-job-args job))
               :connection-type 'pipe
               :coding 'utf-8
               :noquery t
               :sentinel (lambda (proc _event)
                           (unless (process-live-p proc)
                             (ek9-process--finish job))))
            (error (kill-buffer stdout)
                   (kill-buffer stderr)
                   (signal (car err) (cdr err))))))
    (process-put proc 'ek9-stderr stderr)
    (setf (ek9-process-job-process job) proc
          (ek9-process-job-state job) 'running)
    (push job ek9-process--running)
    (when ek9-process-timeout
      (setf (ek9-process-job-timeout job) ek9-process-timeout
            (ek9-process-job-timer job)
            (run-with-timer ek9-process-timeout nil
                            (lambda ()
                              (when (process-live-p proc)
                                (setf (ek9-process-job-timed-out job) t)
                                (delete-process proc))))))
    ;; A list buffer shows "Queued..." while its job waits.
    (force-mode-line-update t)
    (when-let* ((input (ek9-process-job-input job)))
      ;; If kubectl stops before it reads all input, the send fails.
      ;; The sentinel then reports the failure one time.
      (ignore-errors
        (process-send-string proc input)
        (process-send-eof proc)))))

(defun ek9-process--submit (args parser on-success on-error input)
  "Put a kubectl job with ARGS into the queue and return the job.
PARSER, ON-SUCCESS, ON-ERROR, and INPUT have the meaning of
`ek9-process-run'.  The job gets the priority `ek9-process-priority'."
  (let ((job (ek9-process--make-job
              :args args :parser parser :on-success on-success
              :on-error on-error :input input
              :priority ek9-process-priority
              :environment process-environment
              :path exec-path
              :directory default-directory
              :executable ek9-kubectl-executable)))
    (ek9-process--enqueue job)
    ;; Start the job if the queue has space.  If its start fails, the
    ;; caller gets the error.
    (ek9-process--next job)
    job))

;;;; Jobs

(defun ek9-process-live-p (job)
  "Return non-nil if JOB waits in the queue or is in progress.
JOB can also be a process, for example of `ek9-process-stream'."
  (cond ((processp job) (process-live-p job))
        ((ek9-process-job-p job)
         (memq (ek9-process-job-state job) '(queued running)))))

(defun ek9-process-queued-p (job)
  "Return non-nil if JOB waits in the queue."
  (and (ek9-process-job-p job)
       (eq (ek9-process-job-state job) 'queued)))

(defun ek9-process-cancel (job)
  "Stop JOB.  Do not call its callbacks.
Remove a job that waits from the queue.  Stop the process of a job
that runs.  JOB can also be a process."
  (cond
   ((processp job)
    (when (process-live-p job)
      (delete-process job)))
   ((ek9-process-job-p job)
    (pcase (ek9-process-job-state job)
      ('queued
       (setf (ek9-process-job-state job) 'cancelled)
       (setq ek9-process--queue (delq job ek9-process--queue)))
      ('running
       (setf (ek9-process-job-state job) 'cancelled)
       (delete-process (ek9-process-job-process job)))))))

(defun ek9-process-prioritize (job)
  "Move JOB to the user part of the queue, if it waits."
  (when (and (ek9-process-queued-p job)
             (eq (ek9-process-job-priority job) 'background))
    (setq ek9-process--queue (delq job ek9-process--queue))
    (setf (ek9-process-job-priority job) 'user)
    (ek9-process--enqueue job)))

;;;; Commands

(defun ek9-process-run (args on-success &optional on-error input)
  "Run kubectl with ARGS asynchronously and return the job.
Call ON-SUCCESS with the standard output as a string.  When kubectl
fails, call ON-ERROR with the exit code and the standard error text.
After a timeout, the exit code is the symbol `timeout'.  If the
start fails, the exit code is nil.  If ON-ERROR is nil, show a
message.  If INPUT is non-nil, send this string to the standard input
of kubectl.  The job waits in a queue if many commands run, see
`ek9-process-max-processes' and `ek9-process-priority'."
  (ek9-process--submit args #'buffer-string on-success on-error input))

(defun ek9-process-run-json (args on-success &optional on-error input exact)
  "Run kubectl with ARGS asynchronously and parse the output as JSON.
Return the job.  Call ON-SUCCESS with the parsed JSON.  Objects
become alists with symbol keys, and arrays become lists.  JSON null
and false become nil.  If EXACT is non-nil, arrays become vectors,
null becomes `:null', and false becomes `:false'.  ON-ERROR and INPUT
have the same meaning as in `ek9-process-run'."
  (ek9-process--submit
   args
   (if exact
       (lambda ()
         (json-parse-buffer :object-type 'alist
                            :array-type 'array
                            :null-object :null
                            :false-object :false))
     (lambda ()
       (json-parse-buffer :object-type 'alist
                          :array-type 'list
                          :null-object nil
                          :false-object nil)))
   on-success on-error input))

(defun ek9-process-stream (args on-output on-exit)
  "Run kubectl with ARGS and give its output while it arrives.
Return the process.  Call ON-OUTPUT with the process and each new
text.  The text includes the standard error output.  Call ON-EXIT
with the process when the process stops."
  (make-process
   :name "ek9-kubectl-stream"
   :command (cons ek9-kubectl-executable args)
   :connection-type 'pipe
   :coding 'utf-8
   :noquery t
   :filter on-output
   :sentinel (lambda (proc _event)
               (unless (process-live-p proc)
                 (funcall on-exit proc)))))

(provide 'ek9-process)

;;; ek9-process.el ends here
