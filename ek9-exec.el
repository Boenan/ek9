;;; ek9-exec.el --- Shell into a container for ek9 -*- lexical-binding: t; -*-

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

;; This file opens a shell in a pod container with "kubectl exec" in
;; a terminal buffer.  The terminal is Eat if it is installed, or the
;; built-in `term' if not.  See `ek9-exec-terminal'.
;;
;; A shell can change the container, so ek9 treats it as a write
;; action: it blocks a shell in a context of `ek9-read-only-contexts'.

;;; Code:

(require 'subr-x)
(require 'term)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)
(require 'ek9-action)

(declare-function eat-make "ext:eat")

(defcustom ek9-exec-terminal 'auto
  "Terminal emulator for container shells.
The value `auto' uses Eat if it is installed, and `term' if not."
  :type '(choice (const :tag "Eat if installed, else term" auto)
                 (const :tag "Eat" eat)
                 (const :tag "Built-in term" term))
  :group 'ek9)

(defcustom ek9-exec-command
  '("sh" "-c" "command -v bash >/dev/null && exec bash || exec sh")
  "Command that ek9 runs in the container.
The default command starts bash if the container has it, and sh if
not."
  :type '(repeat string)
  :group 'ek9)

(defvar-local ek9-exec--context nil
  "Context of the shell buffer.")
(put 'ek9-exec--context 'permanent-local t)

(defvar-local ek9-exec--title nil
  "Text that describes the pod and the container of the shell buffer.")
(put 'ek9-exec--title 'permanent-local t)

(defun ek9-exec--terminal ()
  "Return the terminal emulator to use: the symbol `eat' or `term'."
  (pcase ek9-exec-terminal
    ('auto (if (require 'eat nil t) 'eat 'term))
    ('eat (unless (require 'eat nil t)
            (user-error "ek9: Eat is not installed.  Change `ek9-exec-terminal'"))
          'eat)
    (_ 'term)))

(defun ek9-exec--header-line ()
  "Return the header line text of a shell buffer."
  (string-replace
   "%" "%%"
   (concat " " (propertize "Context: " 'face 'bold) ek9-exec--context
           "  " (propertize "Shell: " 'face 'bold) ek9-exec--title)))

(defun ek9-exec--args (pod container)
  "Return the kubectl arguments that open a shell in CONTAINER of POD."
  (append (list "exec" "-it" (ek9-resource-item-name pod)
                "--container" container)
          (ek9-list-item-args pod)
          (list "--")
          ek9-exec-command))

(defun ek9-exec--start (name args)
  "Start kubectl with ARGS in a terminal buffer *NAME* and return it."
  ;; Eat uses file handlers to start the process.  Use a local
  ;; directory, so that kubectl never runs on a TRAMP host with a
  ;; different kubeconfig.
  (let* ((default-directory (expand-file-name "~/"))
         (buffer
          (pcase (ek9-exec--terminal)
            ('eat (apply #'eat-make name ek9-kubectl-executable nil args))
            (_ (let ((buffer (apply #'make-term name ek9-kubectl-executable
                                    nil args)))
                 ;; `make-term' already turns on `term-mode'.
                 (with-current-buffer buffer
                   (term-char-mode))
                 buffer)))))
    ;; Ask before Emacs kills a running shell, with all terminals.
    (when-let* ((process (get-buffer-process buffer)))
      (set-process-query-on-exit-flag process t))
    buffer))

(defun ek9-shell (pod container)
  "Open a shell in CONTAINER of POD.
If a shell buffer for CONTAINER already runs, show it."
  (interactive (pcase-let ((`(,_ ,pod ,container)
                            (ek9-list-target-for-action 'shell)))
                 (ek9-action-check (ek9-context-current))
                 (list pod (or container (ek9-list-read-container pod t))))
               ek9-list-mode)
  (let ((context (ek9-context-current))
        (phase (ek9-resource-path pod '(status phase))))
    (ek9-action-check context)
    (unless (equal phase "Running")
      (user-error "ek9: Pod %s is not running (phase %s)"
                  (ek9-resource-item-id pod) (or phase "unknown")))
    ;; A completed init container is in a running pod, but it cannot
    ;; run a shell.
    (when-let* ((status (ek9-resource-pod-container-status pod container))
                ((not (ek9-resource-path status '(state running)))))
      (user-error "ek9: Container %s is not running" container))
    (let* ((title (format "%s %s" (ek9-resource-item-id pod) container))
           (name (format "ek9 shell %s %s" context title))
           (old (get-buffer (concat "*" name "*"))))
      (if (and old (process-live-p (get-buffer-process old)))
          (pop-to-buffer old)
        (when old
          (kill-buffer old))
        (let ((buffer (ek9-exec--start name (ek9-exec--args pod container))))
          (with-current-buffer buffer
            (setq ek9-exec--context context
                  ek9-exec--title title)
            (setq-local header-line-format '(:eval (ek9-exec--header-line))))
          (pop-to-buffer buffer))))))

(provide 'ek9-exec)

;;; ek9-exec.el ends here
