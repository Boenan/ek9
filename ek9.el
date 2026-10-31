;;; ek9.el --- Kubernetes client similar to k9s -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Fabian Waxin

;; Author: Fabian Waxin <fabian.waxin@gmail.com>
;; URL: https://github.com/Boenan/ek9
;; Version: 0.1.1
;; Package-Requires: ((emacs "30.2"))
;; Keywords: tools
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

;; ek9 lets you work with Kubernetes clusters from Emacs.  It uses
;; `kubectl' as its backend and shows resources in list buffers that
;; update automatically.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'ek9-process)
(require 'ek9-proxy)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)
(require 'ek9-types)
(require 'ek9-describe)
(require 'ek9-logs)
(require 'ek9-action)
(require 'ek9-edit)
(require 'ek9-exec)
(require 'ek9-portfwd)
(require 'ek9-transient)
(require 'ek9-saved-view)

(defgroup ek9 nil
  "Kubernetes client similar to k9s."
  :group 'tools
  :prefix "ek9-")

;;;###autoload
(defun ek9 ()
  "Open the ek9 pods list."
  (interactive)
  (ek9-process-check-kubectl)
  (ek9-list-open 'pods))

(defun ek9--buffer-p (buffer)
  "Return non-nil if BUFFER is a live ek9 buffer.
Use the modes and the variables of the buffer, not its name.  A
buffer can die while a caller loops over a buffer list: the kill of
a list buffer also kills the buffer of its proxy request."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (or (derived-mode-p 'ek9-list-mode 'ek9-logs-mode 'ek9-portfwd-mode
                             'ek9-describe-mode)
             ek9-view-mode
             ek9-edit-mode
             ek9-exec--context))))

;;;###autoload
(defun ek9-kill-all-buffers ()
  "Kill all ek9 buffers.
An edit buffer with changes that are not applied, and a shell with a
running process, ask first.  If you answer no, or push \\[keyboard-quit],
ek9 keeps that buffer.  If port-forwards run, ask if they must stop
too: they run without a buffer."
  (interactive)
  (let ((forwards (seq-filter #'ek9-portfwd--running-p ek9-portfwd--forwards))
        (killed 0)
        (kept 0))
    (when (and forwards
               (y-or-n-p (format "Stop %d running port-forward%s too? "
                                 (length forwards)
                                 (if (cdr forwards) "s" ""))))
      (mapc #'ek9-portfwd-stop forwards))
    ;; Make the list after the question: a buffer can die meanwhile.
    (dolist (buffer (seq-filter #'ek9--buffer-p (buffer-list)))
      (when (buffer-live-p buffer)
        (if (condition-case nil (kill-buffer buffer) (quit nil))
            (cl-incf killed)
          (cl-incf kept))))
    (when (timerp ek9-list--timer)
      (cancel-timer ek9-list--timer)
      (setq ek9-list--timer nil))
    ;; Only list buffers use the proxies, and they cannot refuse.
    (ek9-proxy-stop-all)
    (message "ek9: Killed %d buffer%s%s"
             killed (if (= killed 1) "" "s")
             (if (> kept 0) (format ", %d kept" kept) ""))))

(provide 'ek9)

;;; ek9.el ends here
