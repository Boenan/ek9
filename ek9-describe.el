;;; ek9-describe.el --- Describe and YAML views for ek9 -*- lexical-binding: t; -*-

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

;; This file shows the "kubectl describe" output and the YAML of a
;; resource in a read-only buffer.  Each view buffer keeps the kubectl
;; arguments that made it, including the context.  "g" runs kubectl
;; again with the same arguments.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'treesit)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)

(declare-function yaml-mode "ext:yaml-mode")

(defvar-local ek9-view--args nil
  "The kubectl arguments that make the content of the view buffer.")

(defvar-local ek9-view--process nil
  "The kubectl process of the running refresh, or nil.")

(defvar-local ek9-view--transform nil
  "Function that changes the kubectl output before ek9 shows it, or nil.")

;;;; View buffers

(defvar-keymap ek9-view-mode-map
  :doc "Keymap for `ek9-view-mode'."
  "g" #'ek9-view-refresh
  "q" #'quit-window)

(define-minor-mode ek9-view-mode
  "Minor mode for ek9 describe and YAML buffers.

\\{ek9-view-mode-map}"
  :lighter " ek9")

(defun ek9-view--window-lines ()
  "Return (WINDOW POINT-LINE START-LINE) for each window of the buffer."
  (mapcar (lambda (window)
            (list window
                  (line-number-at-pos (window-point window))
                  (line-number-at-pos (window-start window))))
          (get-buffer-window-list nil nil t)))

(defun ek9-view--line-position (line)
  "Return the position of the start of LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun ek9-view--set-content (text)
  "Replace the content of the view buffer with TEXT.
Keep the line of point and of the window start in each window."
  (let ((windows (ek9-view--window-lines))
        (line (line-number-at-pos))
        (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (goto-char (ek9-view--line-position line))
    (pcase-dolist (`(,window ,point-line ,start-line) windows)
      (set-window-start window (ek9-view--line-position start-line))
      (set-window-point window (ek9-view--line-position point-line)))
    (set-buffer-modified-p nil)))

(defun ek9-view-refresh (&optional buffer)
  "Run kubectl again and show the new output in the view buffer.
If BUFFER is nil, use the current buffer.  Do nothing if a refresh
is already running."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (ek9-process-live-p ek9-view--process)
      (let ((buffer (current-buffer)))
        (setq ek9-view--process
              (ek9-process-run
               ek9-view--args
               (lambda (output)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (ek9-view--set-content
                      (if ek9-view--transform
                          (funcall ek9-view--transform output)
                        output)))))
               (lambda (_exit-code stderr)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (ek9-view--set-content
                      (concat "ek9: kubectl failed: "
                              (ek9-process-error-summary stderr)
                              "\n")))))))))))

(defun ek9-view-open (buffer-name args mode title &optional transform)
  "Show the output of kubectl ARGS in the buffer BUFFER-NAME.
MODE is the major mode function for the buffer.  TITLE is the text
in the header line after the context.  If TRANSFORM is non-nil, it is
a function that takes the kubectl output and returns the text to
show.  Return the buffer."
  (let ((buffer (get-buffer-create buffer-name))
        (context (ek9-context-current)))
    (with-current-buffer buffer
      (funcall mode)
      (setq ek9-view--args args
            ek9-view--transform transform)
      (setq-local header-line-format
                  (string-replace
                   "%" "%%"
                   (concat " " (propertize "Context: " 'face 'bold)
                           (or context "<none>") "  " title)))
      (setq buffer-read-only t)
      (ek9-view-mode 1))
    (pop-to-buffer buffer)
    (ek9-view-refresh buffer)
    buffer))

;;;; Describe

(defvar ek9-describe-font-lock-keywords
  '(("^\\s-*\\([A-Za-z][A-Za-z0-9 ./-]*\\):" 1 'font-lock-keyword-face))
  "Font lock keywords for `ek9-describe-mode'.")

(define-derived-mode ek9-describe-mode special-mode "ek9-describe"
  "Major mode for the \"kubectl describe\" output."
  (setq-local font-lock-defaults '(ek9-describe-font-lock-keywords t)))

(defun ek9-describe (resource item)
  "Show the \"kubectl describe\" output for ITEM of type RESOURCE."
  (interactive (seq-take (ek9-list-target-for-action 'describe) 2) ek9-list-mode)
  (let ((kind (ek9-resource-kind resource))
        (id (ek9-resource-item-id item)))
    (ek9-view-open (format "*ek9 describe %s %s*" kind id)
                   (append (list "describe" kind (ek9-resource-item-name item))
                           (ek9-list-item-args item))
                   #'ek9-describe-mode
                   (format "describe %s %s" kind id))))

;;;; YAML

(defun ek9-yaml-set-mode ()
  "Turn on the best YAML major mode that is available."
  (cond ((and (fboundp 'yaml-ts-mode) (treesit-ready-p 'yaml t))
         (yaml-ts-mode))
        ((fboundp 'yaml-mode)
         (yaml-mode))
        (t
         (fundamental-mode))))

(defun ek9-yaml (resource item)
  "Show the YAML of ITEM of type RESOURCE."
  (interactive (seq-take (ek9-list-target-for-action 'yaml) 2) ek9-list-mode)
  (let ((kind (ek9-resource-kind resource))
        (id (ek9-resource-item-id item)))
    (ek9-view-open (format "*ek9 yaml %s %s*" kind id)
                   (append (list "get" kind (ek9-resource-item-name item)
                                 "-o" "yaml")
                           (ek9-list-item-args item))
                   #'ek9-yaml-set-mode
                   (format "yaml %s %s" kind id))))

;;;; Secrets

(defun ek9-secret--decode-value (encoded)
  "Return the base64 text ENCODED as decoded text.
Return \"<binary data, N bytes>\" if the decoded value is not text."
  (let* ((bytes (base64-decode-string encoded))
         (text (decode-coding-string bytes 'utf-8-unix t)))
    (if (or (string-match-p "[\0-\10\16-\37\177]" text)
            (seq-some (lambda (char) (eq (char-charset char) 'eight-bit)) text))
        (format "<binary data, %d bytes>" (length bytes))
      text)))

(defun ek9-secret--format-value (key value)
  "Return KEY and the decoded VALUE as YAML text."
  (if (string-search "\n" value)
      (concat key ": |\n"
              (mapconcat (lambda (line) (concat "  " line))
                         (split-string (string-trim-right value "\n") "\n")
                         "\n")
              "\n")
    (format "%s: %s\n" key value)))

(defun ek9-secret--format-decoded (output)
  "Return the decoded values of the secret JSON in OUTPUT as text."
  (let* ((secret (json-parse-string output :object-type 'alist
                                    :null-object nil :false-object nil))
         (data (alist-get 'data secret)))
    (concat (format "# Decoded data of secret %s\n"
                    (ek9-resource-item-id secret))
            (if data
                (mapconcat (lambda (entry)
                             (ek9-secret--format-value
                              (symbol-name (car entry))
                              (ek9-secret--decode-value (cdr entry))))
                           data "")
              "# The secret has no data.\n"))))

(defun ek9-secret-decode (resource item)
  "Show the decoded data values of the secret ITEM of type RESOURCE."
  (interactive (list ek9-list--resource (ek9-list-item-for-action 'decode))
               ek9-list-mode)
  (let ((kind (ek9-resource-kind resource))
        (id (ek9-resource-item-id item)))
    (ek9-view-open (format "*ek9 decode %s %s*" kind id)
                   (append (list "get" kind (ek9-resource-item-name item)
                                 "-o" "json")
                           (ek9-list-item-args item))
                   #'ek9-yaml-set-mode
                   (format "decoded %s %s" kind id)
                   #'ek9-secret--format-decoded)))

(provide 'ek9-describe)

;;; ek9-describe.el ends here
