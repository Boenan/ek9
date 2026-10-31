;;; ek9-logs.el --- Pod logs for ek9 -*- lexical-binding: t; -*-

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

;; This file streams the logs of a pod container into a buffer.  The
;; buffer has one stream at a time.  When point is at the end of the
;; buffer, point stays at the end while new lines arrive.  The user can
;; stop the stream and start it again.  The old lines stay in the
;; buffer, and the new stream adds the lines after the last line.
;;
;; ek9 always asks kubectl for timestamps.  The timestamps come from
;; the node, so ek9 does not use the local clock.  A new stream after
;; a stop starts at the timestamp of the last line, and ek9 drops the
;; lines that it showed before.  ek9 removes the timestamps from the
;; text if the user did not turn them on.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'seq)
(require 'subr-x)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)

(defcustom ek9-logs-tail-lines 200
  "Number of old log lines that ek9 shows when the stream starts.
The value -1 shows all old lines."
  :type 'integer
  :group 'ek9)

(defcustom ek9-logs-max-lines 10000
  "Maximum number of lines in a logs buffer.
ek9 removes the oldest lines when there are more lines."
  :type 'integer
  :group 'ek9)

(defvar-local ek9-logs--pod nil
  "The pod item of the logs buffer.")

(defvar-local ek9-logs--container nil
  "The container name of the logs buffer.")

(defvar-local ek9-logs--context nil
  "The context name that was current when the logs buffer opened.")

(defvar-local ek9-logs--context-args nil
  "The context arguments that were current when the logs buffer opened.")

(defvar-local ek9-logs--timestamps nil
  "Non-nil to show a timestamp on each log line.")

(defvar-local ek9-logs--previous nil
  "Non-nil to show the logs of the previous container instance.")

(defvar-local ek9-logs--process nil
  "The kubectl logs process, or nil.")

(defvar-local ek9-logs--line-count 0
  "Number of lines in the logs buffer.")

(defvar-local ek9-logs--stopped nil
  "Non-nil if the user stopped the log stream.")

(defvar-local ek9-logs--last-time nil
  "The time key of the last log line in the buffer, or nil.
See `ek9-logs--time-key'.")

(defvar-local ek9-logs--skip-until nil
  "Drop the log lines up to this time key, or nil.
A new stream after a stop starts with lines that the buffer has.
ek9 drops them until the first newer line arrives.")

(defvar-local ek9-logs--partial ""
  "The start of a log line that did not end yet.")

(defconst ek9-logs--timestamp-regexp
  "\\`\\([0-9]+-[0-9]+-[0-9]+T[0-9]+:[0-9]+:[0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?Z "
  "Regexp for the timestamp at the start of a log line.
Group 1 is the time in whole seconds, and group 2 is the fraction.")

;;;; Stream

(defun ek9-logs--args (&optional since)
  "Return the kubectl arguments for the logs buffer.
If SINCE is a time key, get only the lines from the second of this
time, and not the last `ek9-logs-tail-lines' lines."
  (append (list "logs" (ek9-resource-item-name ek9-logs--pod)
                "--container" ek9-logs--container)
          (when-let* ((namespace (ek9-resource-item-namespace ek9-logs--pod)))
            (list "--namespace" namespace))
          (if ek9-logs--previous
              (list "--previous")
            (list "--follow"))
          ;; The API accepts only whole seconds.  The stream gives
          ;; the lines of the full second again, and
          ;; `ek9-logs--skip-until' drops them.
          (if since
              (list (format "--since-time=%sZ"
                            (substring since 0 (string-search "." since))))
            (list (format "--tail=%d" ek9-logs-tail-lines)))
          (list "--timestamps")
          ek9-logs--context-args))

(defun ek9-logs--time-key (seconds fraction)
  "Return a time key for SECONDS and the FRACTION of a timestamp.
SECONDS is the text up to the whole seconds, and FRACTION the digits
after the point, or nil.  Keys have a fixed width, so `string<'
compares them as times."
  (concat seconds "." (substring (concat fraction "000000000") 0 9)))

(defun ek9-logs--line (line)
  "Return the buffer text for the log LINE, or nil.
LINE has no newline.  Remove the timestamp of LINE, unless the user
turned timestamps on.  Return nil for a line to drop, see
`ek9-logs--skip-until'.  A line without a timestamp, for example an
error of kubectl, stays as it is."
  (setq line (string-remove-suffix "\r" line))
  (if (not (string-match ek9-logs--timestamp-regexp line))
      line
    (let ((key (ek9-logs--time-key (match-string 1 line) (match-string 2 line)))
          (text (if ek9-logs--timestamps line (substring line (match-end 0)))))
      (unless (and ek9-logs--skip-until
                   (not (string< ek9-logs--skip-until key)))
        (setq ek9-logs--skip-until nil
              ek9-logs--last-time key)
        text))))

(defun ek9-logs--output (text)
  "Add the complete log lines of the stream TEXT to the buffer.
Keep the start of a line that did not end in `ek9-logs--partial'."
  (let* ((text (concat ek9-logs--partial text))
         (end (cl-position ?\n text :from-end t)))
    (setq ek9-logs--partial (substring text (if end (1+ end) 0)))
    (when end
      (ek9-logs--insert
       (mapconcat (lambda (line)
                    (if-let* ((shown (ek9-logs--line line)))
                        (concat shown "\n")
                      ""))
                  (split-string (substring text 0 end) "\n")
                  "")))))

(defun ek9-logs--trim ()
  "Remove the oldest lines if there are more than `ek9-logs-max-lines'."
  (when (> ek9-logs--line-count ek9-logs-max-lines)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (forward-line (- ek9-logs--line-count ek9-logs-max-lines))
        (delete-region (point-min) (point))))
    (setq ek9-logs--line-count ek9-logs-max-lines)))

(defun ek9-logs--insert (text)
  "Add TEXT at the end of the logs buffer.
Change ANSI color codes in TEXT to faces.  Keep point at the end in
the windows where point is at the end."
  (let* ((text (ansi-color-apply text))
         (at-end (= (point) (point-max)))
         (windows (seq-filter (lambda (window)
                                (= (window-point window) (point-max)))
                              (get-buffer-window-list nil nil t))))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))
    (when at-end
      (goto-char (point-max)))
    (dolist (window windows)
      (set-window-point window (point-max)))
    (cl-incf ek9-logs--line-count (cl-count ?\n text))
    (ek9-logs--trim)))

(defun ek9-logs--stop ()
  "Stop the log stream of the current buffer, if there is one."
  (when (process-live-p ek9-logs--process)
    (delete-process ek9-logs--process))
  (setq ek9-logs--process nil))

(defun ek9-logs--insert-note (text)
  "Add the note TEXT on a new line at the end of the logs buffer."
  (ek9-logs--insert
   (concat (unless (memq (char-before (point-max)) '(nil ?\n)) "\n")
           (propertize (format "[ek9: %s]\n" text) 'face 'shadow))))

(defun ek9-logs--start ()
  "Remove the old content and start a new log stream."
  (ek9-logs--stop)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq ek9-logs--line-count 0
        ek9-logs--last-time nil
        ansi-color-context nil)
  (ek9-logs--stream))

(defun ek9-logs--stream (&optional since)
  "Start a log stream into the logs buffer.
If SINCE is a time key, start after the line of this time."
  ;; The new stream gives the unfinished line of the old stream again,
  ;; so drop it.  The colors of the dropped lines are unknown.
  (setq ek9-logs--stopped nil
        ek9-logs--skip-until since
        ek9-logs--partial ""
        ansi-color-context nil)
  (let ((buffer (current-buffer)))
    (setq ek9-logs--process
          (ek9-process-stream
           (ek9-logs--args since)
           (lambda (proc text)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq proc ek9-logs--process)
                   (ek9-logs--output text)))))
           (lambda (proc)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq proc ek9-logs--process)
                   (unless (string-empty-p ek9-logs--partial)
                     (ek9-logs--output "\n"))
                   (ek9-logs--insert-note "Log stream ended")
                   (force-mode-line-update)))))))
    (force-mode-line-update)))

(defun ek9-logs--tab-line ()
  "Return the tab line text of a logs buffer."
  (string-replace
   "%" "%%"
   (concat " "
           (propertize "Context: " 'face 'bold) (or ek9-logs--context "<none>")
           "  " (propertize "Pod: " 'face 'bold)
           (ek9-resource-item-id ek9-logs--pod)
           "  " (propertize "Container: " 'face 'bold) ek9-logs--container
           (when ek9-logs--timestamps "  [timestamps]")
           (when ek9-logs--previous "  [previous]")
           (if (process-live-p ek9-logs--process)
               "  Streaming..."
             (propertize "  Stopped" 'face 'shadow)))))

;;;; Commands

(defun ek9-logs-restart ()
  "Remove the content of the logs buffer and start the stream again."
  (interactive nil ek9-logs-mode)
  (ek9-logs--start))

(defun ek9-logs-toggle-stream ()
  "Stop the log stream, or start it again.
A stop keeps the lines in the buffer.  A start after a stop adds the
lines after the last line in the buffer.  For previous logs, or after
the stream ended, a start removes the content and starts a new
stream."
  (interactive nil ek9-logs-mode)
  (cond
   ((process-live-p ek9-logs--process)
    (ek9-logs--stop)
    (setq ek9-logs--stopped t)
    (ek9-logs--insert-note "Log stream stopped")
    (force-mode-line-update))
   ((and ek9-logs--stopped (not ek9-logs--previous))
    (ek9-logs--insert-note "Log stream started again")
    (ek9-logs--stream ek9-logs--last-time))
   (t
    (ek9-logs--start))))

(defun ek9-logs-toggle-timestamps ()
  "Turn timestamps on or off, then start the stream again."
  (interactive nil ek9-logs-mode)
  (setq ek9-logs--timestamps (not ek9-logs--timestamps))
  (ek9-logs--start))

(defun ek9-logs-toggle-previous ()
  "Switch between current and previous container logs.
The previous logs come from the container instance before the last
restart.  They do not stream."
  (interactive nil ek9-logs-mode)
  (setq ek9-logs--previous (not ek9-logs--previous))
  (ek9-logs--start))

(defun ek9-logs-select-container ()
  "Select another container of the pod, then start the stream again."
  (interactive nil ek9-logs-mode)
  (setq ek9-logs--container (ek9-list-read-container ek9-logs--pod))
  (ek9-logs--start))

(defun ek9-logs-quit ()
  "Stop the log stream and kill the logs buffer."
  (interactive nil ek9-logs-mode)
  (quit-window t))

(defvar-keymap ek9-logs-mode-map
  :doc "Keymap for `ek9-logs-mode'."
  :parent special-mode-map
  "g" #'ek9-logs-restart
  "s" #'ek9-logs-toggle-stream
  "t" #'ek9-logs-toggle-timestamps
  "P" #'ek9-logs-toggle-previous
  "c" #'ek9-logs-select-container
  "q" #'ek9-logs-quit)

(define-derived-mode ek9-logs-mode special-mode "ek9-logs"
  "Major mode for pod logs.

\\{ek9-logs-mode-map}"
  (setq-local ansi-color-context nil)
  (setq-local tab-line-format '(:eval (ek9-logs--tab-line)))
  (add-hook 'kill-buffer-hook #'ek9-logs--stop nil t))

(defun ek9-logs (pod container)
  "Show the logs of CONTAINER in POD.
There is one logs buffer for each pod.  If the buffer exists and
streams the same container, show it without a restart."
  (interactive (pcase-let ((`(,_ ,pod ,container)
                            (ek9-list-target-for-action 'logs)))
                 (list pod (or container (ek9-list-read-container pod))))
               ek9-list-mode)
  (let ((buffer (get-buffer-create
                 (format "*ek9 logs %s*" (ek9-resource-item-id pod)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ek9-logs-mode)
        (ek9-logs-mode))
      (unless (and (process-live-p ek9-logs--process)
                   (equal container ek9-logs--container))
        (setq ek9-logs--pod pod
              ek9-logs--container container
              ek9-logs--context (ek9-context-current)
              ek9-logs--context-args (ek9-context-args))
        (ek9-logs--start)))
    (pop-to-buffer buffer)
    buffer))

(provide 'ek9-logs)

;;; ek9-logs.el ends here
