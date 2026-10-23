;;; ek9-edit.el --- Edit and apply resources for ek9 -*- lexical-binding: t; -*-

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

;; This file lets the user edit the YAML of a resource in a buffer.
;; A save (C-x C-s or C-c C-c) applies the YAML to the cluster.
;; C-c C-k discards the changes.
;;
;; ek9 uses "kubectl replace", not "kubectl apply".  The YAML comes
;; from "kubectl get", so it is the full object.  "kubectl replace"
;; does a full update, as "kubectl edit" does: a field that the user
;; removes is also removed in the cluster.  The resourceVersion in the
;; YAML makes the update fail if the object changed in the meantime.
;; "kubectl apply" would also copy the full object, including secret
;; values, into an annotation.
;;
;; Before ek9 applies the YAML, it does these steps:
;;
;; 1. Stop if the context of the buffer is not the current context,
;;    or if the context is read-only.
;; 2. Ask for confirmation.
;; 3. Run a client dry run and make sure that the YAML contains only
;;    the resource of the buffer.  This stops an update of a different
;;    resource, for example after a change of the name.
;; 4. Do the check of step 1 again, then update the resource.
;; 5. If the YAML has the last-applied annotation of "kubectl apply",
;;    write a clean copy of the YAML into the annotation.
;;
;; Step 5 is necessary because "kubectl replace" writes the full YAML
;; into the last-applied annotation if the annotation exists.  The
;; YAML contains server fields, for example resourceVersion.  After
;; that, "kubectl apply" removes resourceVersion in its patch, and the
;; patch of a custom resource fails.  ek9 cannot remove resourceVersion
;; before the replace, because the replace needs it to find conflicts.
;;
;; All kubectl commands of an edit buffer use the context of the
;; buffer, not the current context.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)
(require 'ek9-describe)
(require 'ek9-action)

;; The variables are permanent, so that a change of the major mode,
;; for example with M-x yaml-mode, does not remove the edit state.

(defvar-local ek9-edit--resource nil
  "Resource type of the item in the edit buffer.")
(put 'ek9-edit--resource 'permanent-local t)

(defvar-local ek9-edit--item nil
  "The resource item in the edit buffer.")
(put 'ek9-edit--item 'permanent-local t)

(defvar-local ek9-edit--context nil
  "Context of the item in the edit buffer.")
(put 'ek9-edit--context 'permanent-local t)

(defvar-local ek9-edit--process nil
  "The running kubectl process of the edit buffer, or nil.")
(put 'ek9-edit--process 'permanent-local t)

(defvar-local ek9-edit--status nil
  "Status text for the header line, or nil.")
(put 'ek9-edit--status 'permanent-local t)

;;;; Header line

(defun ek9-edit--header-line ()
  "Return the header line text of an edit buffer."
  (string-replace
   "%" "%%"
   (concat " " (propertize "Context: " 'face 'bold)
           (or ek9-edit--context "<none>")
           "  " (propertize "Edit: " 'face 'bold)
           (ek9-action-label ek9-edit--item)
           (substitute-command-keys
            "  \\<ek9-edit-mode-map>\\[ek9-edit-apply] apply, \\[ek9-edit-cancel] cancel")
           (when ek9-edit--status
             (concat "  " (propertize ek9-edit--status 'face 'error))))))

(defun ek9-edit--set-status (status)
  "Show STATUS in the header line and as a message.
STATUS is an error text, or nil to remove the old text."
  (setq ek9-edit--status status)
  (when status
    (message "ek9: %s" status))
  (force-mode-line-update))

;;;; kubectl arguments

(defun ek9-edit--item-args ()
  "Return the namespace and context arguments of the edit buffer.
Use the context of the buffer, not the current context."
  (append (when-let* ((namespace (ek9-resource-item-namespace ek9-edit--item)))
            (list "--namespace" namespace))
          (list "--context" ek9-edit--context)))

(defun ek9-edit--get-args ()
  "Return the kubectl arguments that get the YAML of the edit buffer."
  (append (list "get" (ek9-resource-kind ek9-edit--resource)
                (ek9-resource-item-name ek9-edit--item) "-o" "yaml")
          (ek9-edit--item-args)))

(defun ek9-edit--replace-args (&optional dry-run)
  "Return the kubectl arguments that update the item from stdin.
If DRY-RUN is non-nil, return the arguments of a client dry run."
  (append (list "replace" "-f" "-")
          (when dry-run
            (list "--dry-run=client" "-o" "json"))
          (ek9-edit--item-args)))

(defun ek9-edit--set-last-applied-args ()
  "Return the kubectl arguments that set the last-applied annotation.
The annotation gets the object from stdin."
  (append (list "apply" "set-last-applied" "-f" "-")
          (ek9-edit--item-args)))

;;;; Last-applied annotation

(defconst ek9-edit--last-applied-annotation
  "kubectl.kubernetes.io/last-applied-configuration"
  "Name of the annotation that \"kubectl apply\" uses.")

(defconst ek9-edit--server-fields
  '("resourceVersion" "uid" "creationTimestamp" "generation"
    "managedFields" "selfLink" "deletionTimestamp"
    "deletionGracePeriodSeconds")
  "Metadata fields that the API server sets.
The last-applied annotation must not contain these fields.")

(defun ek9-edit--last-applied (output)
  "Return the clean last-applied JSON for the dry-run OUTPUT, or nil.
OUTPUT is the JSON text of a client dry run with \"-o json\".  Return
nil if the object has no last-applied annotation.  Otherwise, return
the object without the server fields and without the annotation."
  ;; Hash tables keep null, false, and empty objects, so that the
  ;; annotation gets the same values as the YAML.
  (let* ((object (json-parse-string output :object-type 'hash-table))
         (metadata (gethash "metadata" object))
         (annotations (and (hash-table-p metadata)
                           (gethash "annotations" metadata))))
    (when (and (hash-table-p annotations)
               (gethash ek9-edit--last-applied-annotation annotations))
      (remhash ek9-edit--last-applied-annotation annotations)
      (dolist (field ek9-edit--server-fields)
        (remhash field metadata))
      (json-serialize object))))

;;;; Load

(defun ek9-edit--insert (text)
  "Replace the content of the edit buffer with TEXT.
Mark the buffer as not modified and clear the undo list."
  (let ((line (line-number-at-pos))
        (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (forward-line (1- line))
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)))

(defun ek9-edit--load (&optional on-error status)
  "Get the YAML of the edit buffer from the cluster and show it.
The buffer is read-only while kubectl runs, so that the new YAML
cannot replace text that the user types.  After a successful load,
show STATUS in the header line, or no status if STATUS is nil.  If
kubectl fails, show the error.  Then call ON-ERROR with no arguments
if it is non-nil."
  (let ((buffer (current-buffer)))
    (setq ek9-edit--process
          (ek9-process-run
           (ek9-edit--get-args)
           (lambda (output)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (ek9-edit--insert output)
                 (setq buffer-read-only nil)
                 (ek9-edit--set-status status))))
           (lambda (_exit-code stderr)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq buffer-read-only nil)
                 (ek9-edit--set-status
                  (concat "Cannot get the YAML: "
                          (ek9-process-error-summary stderr)))
                 (when on-error
                   (funcall on-error)))))))
    ;; Set the flag only after kubectl started.  If `make-process'
    ;; signals an error, no callback would clear the flag.
    (setq buffer-read-only t)))

;;;; Apply

(defun ek9-edit--check-dry-run (output)
  "Signal an error if the dry-run OUTPUT is not the item of the buffer.
OUTPUT is the JSON text of a client dry run with \"-o json\"."
  (let* ((object (condition-case nil
                     (json-parse-string output :object-type 'alist
                                        :array-type 'list
                                        :null-object nil :false-object nil)
                   (json-error nil)))
         (kind (alist-get 'kind object))
         (name (ek9-resource-item-name object))
         (namespace (ek9-resource-item-namespace object))
         (expected-kind (alist-get 'kind ek9-edit--item))
         (expected-name (ek9-resource-item-name ek9-edit--item))
         (expected-namespace (ek9-resource-item-namespace ek9-edit--item)))
    ;; kubectl prints a List when the YAML has more than one document.
    (when (or (null object) (equal kind "List"))
      (error "The buffer must contain exactly one resource"))
    (unless (and (equal kind expected-kind)
                 (equal name expected-name)
                 (or (null namespace) (equal namespace expected-namespace)))
      (error "The YAML is for %s %s, not for %s"
             kind (if namespace (concat namespace "/" name) name)
             (ek9-action-label ek9-edit--item)))))

(defun ek9-edit--applied (output tick &optional warning)
  "Show the result OUTPUT of a successful update.
TICK is the value of `buffer-chars-modified-tick' when the update
started.  If the buffer did not change since then, get the new YAML,
so that the next update uses the new resource version.  WARNING is a
text for the header line, or nil."
  (message "ek9: %s" (string-trim output))
  (if (/= tick (buffer-chars-modified-tick))
      (ek9-edit--set-status warning)
    (set-buffer-modified-p nil)
    ;; Keep the old status until the load is done, so that the
    ;; warning shows only one time.
    (ek9-edit--load nil warning)))

(defun ek9-edit--last-applied-warning (exit-code stderr)
  "Return the warning for a failed update of the last-applied annotation.
EXIT-CODE and STDERR are the result of kubectl."
  (concat "Applied, but the last-applied annotation is not clean.  "
          (if (eq exit-code 'timeout)
              (ek9-process-timeout-text "Update of the annotation")
            (concat "kubectl apply can fail: "
                    (ek9-process-error-summary stderr)))))

(defun ek9-edit--run-apply (input tick)
  "Update the item of the buffer with the YAML text INPUT.
First make sure with a dry run that INPUT contains only the item of
the buffer.  After the update, clean the last-applied annotation.
TICK is the value of `buffer-chars-modified-tick' when the user
saved."
  (let ((buffer (current-buffer))
        (label (ek9-action-label ek9-edit--item))
        (dry-run-args (ek9-edit--replace-args t))
        (replace-args (ek9-edit--replace-args))
        (last-applied-args (ek9-edit--set-last-applied-args)))
    (cl-flet ((in-buffer (function &optional when-dead)
                (lambda (&rest args)
                  (cond ((buffer-live-p buffer)
                         (with-current-buffer buffer
                           (apply function args)))
                        (when-dead (message "ek9: %s" (apply when-dead args))))))
              (failed (exit-code stderr)
                (ek9-edit--set-status
                 (if (eq exit-code 'timeout)
                     (ek9-process-timeout-text "Apply")
                   (concat "Apply failed: "
                           (ek9-process-error-summary stderr)))))
              ;; The user can kill the buffer while kubectl runs.  Then
              ;; show the result as a message.
              (failed-text (exit-code stderr)
                (if (eq exit-code 'timeout)
                    (ek9-process-timeout-text (concat "Apply to " label))
                  (format "Apply to %s failed: %s"
                          label (ek9-process-error-summary stderr))))
              (set-process (job)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq ek9-edit--process job)))))
      (setq ek9-edit--process
            (ek9-process-run
             dry-run-args
             (in-buffer
              (lambda (output)
                (condition-case err
                    (let ((last-applied (progn
                                          (ek9-edit--check-dry-run output)
                                          (ek9-edit--last-applied output)))
                          (applied
                           (in-buffer
                            (lambda (output &optional warning)
                              (ek9-edit--applied output tick warning))
                            (lambda (output &optional warning)
                              (concat (string-trim output)
                                      (and warning (concat ".  " warning)))))))
                      ;; The context can change during the dry run.
                      (ek9-action-check ek9-edit--context)
                      (setq ek9-edit--process
                            (ek9-process-run
                             replace-args
                             ;; Clean the annotation also if the user
                             ;; killed the buffer in the meantime.
                             (lambda (output)
                               (if (not last-applied)
                                   (funcall applied output)
                                 (condition-case err
                                     (set-process
                                      (ek9-process-run
                                       last-applied-args
                                       (lambda (_)
                                         (funcall applied output))
                                       (lambda (exit-code stderr)
                                         (funcall applied output
                                                  (ek9-edit--last-applied-warning
                                                   exit-code stderr)))
                                       last-applied))
                                   ;; The replace is done, so show its
                                   ;; result also if kubectl cannot start.
                                   (error
                                    (funcall applied output
                                             (ek9-edit--last-applied-warning
                                              nil (error-message-string err)))))))
                             (in-buffer #'failed #'failed-text)
                             input)))
                  (user-error
                   (ek9-edit--set-status
                    (string-remove-prefix "ek9: " (cadr err))))
                  (error
                   (ek9-edit--set-status
                    (concat (error-message-string err)
                            ".  Nothing was applied")))))
              (lambda (_output)
                (format "Nothing was applied to %s, because its edit buffer is gone"
                        label)))
             (in-buffer #'failed #'failed-text)
             input)))))

(defun ek9-edit-apply ()
  "Apply the YAML in the edit buffer to the cluster."
  (interactive nil ek9-edit-mode)
  (when (ek9-process-live-p ek9-edit--process)
    (user-error "ek9: A kubectl command is still running.  Try again later"))
  (ek9-action-check ek9-edit--context)
  (cond
   ((not (buffer-modified-p))
    (message "ek9: No changes to apply"))
   ((not (yes-or-no-p (format "Apply changes to %s in context %s? "
                              (ek9-action-label ek9-edit--item)
                              ek9-edit--context)))
    (message "ek9: Cancelled"))
   (t
    (ek9-action-check ek9-edit--context)
    (ek9-edit--run-apply (buffer-substring-no-properties (point-min) (point-max))
                         (buffer-chars-modified-tick)))))

;;;; Cancel

(defun ek9-edit-cancel ()
  "Kill the edit buffer without an apply.
Ask the user first if the buffer has changes."
  (interactive nil ek9-edit-mode)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard the changes? "))
    (set-buffer-modified-p nil)
    (kill-buffer)))

(defun ek9-edit--query-kill ()
  "Return non-nil if ek9 can kill the edit buffer.
If the buffer has changes that are not applied, ask the user."
  (cond ((not ek9-edit-mode) t)
        ((ek9-process-live-p ek9-edit--process)
         (yes-or-no-p "The kubectl command still applies the changes.  Kill the buffer? "))
        ((not (buffer-modified-p)) t)
        (t (yes-or-no-p "The changes are not applied.  Kill the buffer? "))))
(put 'ek9-edit--query-kill 'permanent-local-hook t)

;;;; Mode

(defvar-keymap ek9-edit-mode-map
  :doc "Keymap for `ek9-edit-mode'."
  "C-c C-c" #'ek9-edit-apply
  "C-c C-k" #'ek9-edit-cancel
  "<remap> <save-buffer>" #'ek9-edit-apply)

(define-minor-mode ek9-edit-mode
  "Minor mode for ek9 buffers that edit the YAML of a resource.

\\{ek9-edit-mode-map}"
  :lighter " ek9-edit"
  (if ek9-edit-mode
      (progn
        (setq-local header-line-format '(:eval (ek9-edit--header-line)))
        (add-hook 'kill-buffer-query-functions #'ek9-edit--query-kill nil t)
        (add-hook 'after-change-major-mode-hook #'ek9-edit--restore nil t))
    (kill-local-variable 'header-line-format)
    (remove-hook 'kill-buffer-query-functions #'ek9-edit--query-kill t)
    (remove-hook 'after-change-major-mode-hook #'ek9-edit--restore t)))
(put 'ek9-edit-mode 'permanent-local t)

(defun ek9-edit--restore ()
  "Turn on `ek9-edit-mode' again after a change of the major mode."
  (when ek9-edit-mode
    (ek9-edit-mode 1)))
(put 'ek9-edit--restore 'permanent-local-hook t)

(defun ek9-edit (resource item)
  "Edit the YAML of ITEM of type RESOURCE in a buffer.
Save the buffer to apply the changes.  If an edit buffer for ITEM
already exists, show it and do not discard its changes."
  (interactive (list ek9-list--resource (ek9-list-item-for-action 'edit))
               ek9-list-mode)
  (let ((context (ek9-context-current)))
    (ek9-action-check context)
    (let* ((name (format "*ek9 edit %s %s %s*" context
                         (ek9-resource-kind resource)
                         (ek9-resource-item-id item)))
           (existing (get-buffer name))
           (buffer (or existing (get-buffer-create name))))
      (unless existing
        (condition-case err
            (with-current-buffer buffer
              (ek9-yaml-set-mode)
              (setq ek9-edit--resource resource
                    ek9-edit--item item
                    ek9-edit--context context
                    ek9-edit--status nil)
              (ek9-edit-mode 1)
              (ek9-edit--load (lambda () (kill-buffer buffer))))
          ;; Do not keep an empty buffer that a new `e' would show.
          (error (kill-buffer buffer)
                 (signal (car err) (cdr err)))))
      (pop-to-buffer buffer)
      buffer)))

(provide 'ek9-edit)

;;; ek9-edit.el ends here
