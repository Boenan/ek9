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
;; buffer, point stays at the end while new lines arrive.

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

;;;; Stream

(defun ek9-logs--args ()
  "Return the kubectl arguments for the logs buffer."
  (append (list "logs" (ek9-resource-item-name ek9-logs--pod)
                "--container" ek9-logs--container)
          (when-let* ((namespace (ek9-resource-item-namespace ek9-logs--pod)))
            (list "--namespace" namespace))
          (if ek9-logs--previous
              (list "--previous")
            (list "--follow"))
          (list (format "--tail=%d" ek9-logs-tail-lines))
          (when ek9-logs--timestamps
            (list "--timestamps"))
          ek9-logs--context-args))

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

(defun ek9-logs--start ()
  "Remove the old content and start a new log stream."
  (ek9-logs--stop)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq ek9-logs--line-count 0
        ansi-color-context nil)
  (let ((buffer (current-buffer)))
    (setq ek9-logs--process
          (ek9-process-stream
           (ek9-logs--args)
           (lambda (proc text)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq proc ek9-logs--process)
                   (ek9-logs--insert text)))))
           (lambda (proc)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq proc ek9-logs--process)
                   (ek9-logs--insert
                    (propertize "\n[ek9: Log stream ended]\n"
                                'face 'shadow))
                   (force-mode-line-update)))))))))

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
