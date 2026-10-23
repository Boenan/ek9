;;; ek9-action.el --- Write actions for ek9 -*- lexical-binding: t; -*-

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

;; This file defines the write actions of the list buffers: delete,
;; scale, and restart.  Each action does these steps:
;;
;; 1. Stop if the current context is in `ek9-read-only-contexts'.
;; 2. Ask for confirmation.  The prompt shows the resource and the
;;    context.  For a resource that is not in a namespace, for example
;;    a node or a namespace, the user must type its name.
;; 3. Run kubectl asynchronously, show the result, and refresh the
;;    list buffer.

;;; Code:

(require 'subr-x)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)

;;;; Common steps

(defun ek9-action-label (item)
  "Return a short description of ITEM, for example \"Pod a/b\"."
  (concat (or (alist-get 'kind item) "Resource") " "
          (ek9-resource-item-id item)))

(defun ek9-action--target (resource item)
  "Return the kubectl argument \"KIND/NAME\" for ITEM of type RESOURCE."
  (concat (ek9-resource-kind resource) "/" (ek9-resource-item-name item)))

(defun ek9-action--confirm (resource item question)
  "Ask the user to confirm QUESTION about ITEM of type RESOURCE.
QUESTION is a sentence without the context, for example \"Delete Pod
a/b\".  Return non-nil if the user confirms.  For a resource that is
not in a namespace, the user must type the name of ITEM."
  (let ((prompt (format "%s in context %s? " question (ek9-context-current))))
    (if (ek9-resource-namespaced resource)
        (yes-or-no-p prompt)
      (let ((answer (read-string (concat prompt "Type the name to confirm: "))))
        (or (equal answer (ek9-resource-item-name item))
            (progn
              (unless (string-empty-p answer)
                (user-error "ek9: The name does not match.  Nothing was done"))
              nil))))))

(defun ek9-action-check (context)
  "Signal a `user-error' if a write action in CONTEXT is not permitted.
CONTEXT is the context of the item.  It must be the current context,
and the current context must not be read-only."
  (unless (ek9-context-current)
    (user-error "ek9: No context is selected.  Nothing was done"))
  (unless (equal context (ek9-context-current))
    (user-error "ek9: The context changed.  Nothing was done"))
  (ek9-context-check-writable))

(defun ek9-action--run (resource item question args &optional context)
  "Ask to confirm QUESTION about ITEM of type RESOURCE, then run kubectl.
ARGS are the kubectl arguments without the namespace and the context.
CONTEXT is the context of ITEM.  If nil, use the current context.
Return the process, or nil if the user does not confirm."
  (let ((context (or context (ek9-context-current)))
        (buffer (current-buffer)))
    (ek9-action-check context)
    (if (not (ek9-action--confirm resource item question))
        (progn (message "ek9: Cancelled") nil)
      ;; The context can change while the prompt is open.  Do not run
      ;; the action in a different context.
      (ek9-action-check context)
      (ek9-process-run
       (append args (ek9-list-item-args item))
       (lambda (output)
         (message "ek9: %s" (string-trim output))
         (when (and (buffer-live-p buffer)
                    (eq (buffer-local-value 'major-mode buffer) 'ek9-list-mode))
           (ek9-list-refresh buffer)))
       (lambda (exit-code stderr)
         (if (eq exit-code 'timeout)
             (message "ek9: %s" (ek9-process-timeout-text question))
           (message "ek9: %s failed: %s"
                    question (ek9-process-error-summary stderr))))))))

;;;; Delete

(defun ek9-delete (resource item)
  "Delete ITEM of type RESOURCE.
Do not wait until Kubernetes removes ITEM.  The list shows the
progress."
  (interactive (list ek9-list--resource (ek9-list-item-for-action 'delete))
               ek9-list-mode)
  (ek9-action--run resource item
                   (concat "Delete " (ek9-action-label item))
                   (list "delete" (ek9-action--target resource item)
                         "--wait=false")))

;;;; Scale

(defun ek9-action--read-replicas (current)
  "Ask for a number of replicas and return it.
CURRENT is the number of replicas now.  Signal a `user-error' if the
answer is not a natural number."
  (let ((replicas (read-number (format "Replicas (now %d): " current)
                               current)))
    (unless (natnump replicas)
      (user-error "ek9: Replicas must be a whole number of 0 or more"))
    replicas))

(defun ek9-scale (resource item replicas &optional context)
  "Set the number of replicas of ITEM of type RESOURCE to REPLICAS.
CONTEXT is the context of ITEM.  If nil, use the current context."
  (interactive
   (let ((item (ek9-list-item-for-action 'scale))
         (context (ek9-context-current)))
     (ek9-action-check context)
     (list ek9-list--resource item
           (ek9-action--read-replicas
            (or (ek9-resource-path item '(spec replicas)) 0))
           context))
   ek9-list-mode)
  (ek9-action--run resource item
                   (format "Scale %s from %d to %d replicas"
                           (ek9-action-label item)
                           (or (ek9-resource-path item '(spec replicas)) 0)
                           replicas)
                   (list "scale" (ek9-action--target resource item)
                         (format "--replicas=%d" replicas))
                   context))

;;;; Restart

(defun ek9-restart (resource item)
  "Restart the pods of ITEM of type RESOURCE with a rolling restart."
  (interactive (list ek9-list--resource (ek9-list-item-for-action 'restart))
               ek9-list-mode)
  (ek9-action--run resource item
                   (concat "Restart " (ek9-action-label item))
                   (list "rollout" "restart"
                         (ek9-action--target resource item))))

(provide 'ek9-action)

;;; ek9-action.el ends here
