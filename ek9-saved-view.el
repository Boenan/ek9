;;; ek9-saved-view.el --- Saved views for ek9 -*- lexical-binding: t; -*-

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

;; A saved view is a name for a list that the user opens often.  It
;; keeps a resource type, the wide columns setting, and optionally a
;; namespace, a filter, and a context.  Each view opens in its own
;; buffer, "*ek9 view NAME*".  The other list buffers do not change,
;; except when the view has a context: then the view changes the
;; current context.
;;
;; In a view buffer:
;; - "/" changes the filter of the view buffer only.
;; - "n" changes the current namespace and shows the normal list of
;;   the same type.  The view keeps its namespace.
;; - "c" changes the current context.  A view without a context shows
;;   the new context.  A view with a context keeps it: ek9 shows the
;;   normal list of the same type, and the view buffer shows no rows
;;   until the user opens the view again.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)

(declare-function ek9-types-ensure "ek9-types")

(declare-function custom-file "cus-edit")
(declare-function custom-variable-state "cus-edit")

(defcustom ek9-saved-views nil
  "List of saved views.
Each entry has the form (NAME . PROPERTIES).  NAME is a string
without spaces, because \":\" uses a space before a namespace.
PROPERTIES is a plist with these keys:
  :resource   The resource type, for example `pods'.  Necessary.
  :namespace  A namespace name, or the symbol `all'.  If nil, the
              view shows the current namespace.
  :filter     The filter text, or nil.
  :context    A context name.  If nil, the view shows the current
              context.
  :wide       Non-nil to show the wide columns, nil to hide them.
              If the key is missing, use `ek9-list-wide'.
Use `ek9-saved-view-save' or Customize to change this option.  Do not
use `setq' in your init file: Customize saves the views in
`custom-set-variables', and then one of the two values is lost."
  :type '(alist :key-type (string :tag "Name")
                :value-type (plist :tag "Properties"))
  :group 'ek9)

(defvar ek9-saved-view-history nil
  "Minibuffer history for saved view names.")

;;;; Reading

(defun ek9-saved-view-describe (view)
  "Return a short description of the saved VIEW properties."
  (let* ((name (plist-get view :resource))
         (resource (ek9-resource-get name))
         (namespace (plist-get view :namespace))
         (filter (plist-get view :filter))
         (context (plist-get view :context)))
    (concat (symbol-name name)
            (when (ek9-resource-namespaced resource)
              (cond ((eq namespace 'all) " in all namespaces")
                    (namespace (concat " in " namespace))
                    (t " in the current namespace")))
            (when filter (format ", filter \"%s\"" filter))
            (when context (concat ", context " context))
            (when (and (plist-member view :wide) (not (plist-get view :wide)))
              ", no wide columns"))))

(defun ek9-saved-view--get (name)
  "Return the properties of the saved view NAME.
Signal a `user-error' if there is no such view, or if its resource
type is not known."
  (let ((view (or (cdr (assoc name ek9-saved-views))
                  (user-error "ek9: No saved view %s" name))))
    (let ((resource (condition-case nil
                        (ek9-resource-get (plist-get view :resource))
                      (error (user-error "ek9: Saved view %s has an unknown resource type: %s"
                                         name (plist-get view :resource))))))
      (when (ek9-resource-hidden resource)
        (user-error "ek9: Saved view %s uses %s, which only a drill-down can show"
                    name (plist-get view :resource))))
    view))

(defun ek9-saved-view--read (prompt)
  "Ask for the name of a saved view with PROMPT and return it."
  (unless ek9-saved-views
    (user-error "ek9: No saved views.  Use `ek9-saved-view-save' to add one"))
  (let ((completion-extra-properties
         (list :annotation-function
               (lambda (name)
                 (when-let* ((view (cdr (assoc name ek9-saved-views))))
                   (concat "  " (ignore-errors
                                  (ek9-saved-view-describe view))))))))
    (completing-read (concat prompt ": ") ek9-saved-views nil t nil
                     'ek9-saved-view-history)))

(defun ek9-saved-view--store (views)
  "Make VIEWS the saved views and save them in `custom-file'.
Return non-nil if Emacs saved them in a file.  If Emacs cannot save
them, for example in \"emacs -Q\", keep them for this session only
and return nil."
  (require 'cus-edit)
  (when (eq (custom-variable-state 'ek9-saved-views ek9-saved-views) 'rogue)
    (display-warning
     'ek9 "`ek9-saved-views' has a value from `setq'.  Remove the `setq' from your init file, or saved views can be lost"))
  (customize-set-variable 'ek9-saved-views views)
  (and (custom-file t)
       (condition-case nil
           (progn (customize-save-variable 'ek9-saved-views views) t)
         (error nil))))

(defun ek9-saved-view--stored-text (saved)
  "Return a note for a message if SAVED is nil, or an empty string."
  (if saved "" " (for this session only: Emacs cannot save it in a file)"))

;;;; Commands

;;;###autoload
(defun ek9-saved-view-open (name)
  "Open the saved view NAME in its own list buffer.
If the view has a context, make it the current context first.  A
view of a discovered type, for example a CRD type, opens when ek9 has
the discovered types of the context, see `ek9-types-ensure'."
  (interactive (list (ek9-saved-view--read "Saved view")))
  ;; This command can be the first ek9 command.  Load all of ek9, so
  ;; that all keys of the list buffer work.
  (require 'ek9)
  (ek9-process-check-kubectl)
  (let* ((view (or (cdr (assoc name ek9-saved-views))
                   (user-error "ek9: No saved view %s" name)))
         (context (plist-get view :context))
         (registered (assq (plist-get view :resource) ek9-resource--registry))
         (open (lambda ()
                 (ek9-saved-view--get name)
                 (ek9-list-open (plist-get view :resource)
                                (append
                                 (list :view name
                                       :namespace (plist-get view :namespace)
                                       :filter (plist-get view :filter)
                                       :context context)
                                 (when (plist-member view :wide)
                                   (list :wide (plist-get view :wide))))))))
    ;; Check a view of a registry type before the context changes.
    (when registered
      (ek9-saved-view--get name))
    (ek9-context-ensure
     (lambda ()
       (when (and context (not (equal context (ek9-context-current))))
         (ek9-context-set context)
         (message "ek9: Context changed to %s" context))
       (if registered
           (funcall open)
         ;; The type can be a discovered type of the context.
         (let ((now t))
           (prog1 (ek9-types-ensure
                   (lambda ()
                     (if now
                         (funcall open)
                       ;; After a wait, an error is a message.
                       (condition-case err
                           (funcall open)
                         (user-error
                          (message "%s" (error-message-string err)))))))
             (setq now nil))))))))

(defun ek9-saved-view-save (name)
  "Save the current list as the view NAME.
The view keeps the resource type, the namespace, the filter, and the
wide columns setting of the list.  A view buffer also keeps its
context.  A drill-down list cannot become a view, because a view
does not keep a selector."
  (interactive (list (completing-read "Save view as: " ek9-saved-views
                                      nil nil nil 'ek9-saved-view-history))
               ek9-list-mode)
  (when (string-empty-p name)
    (user-error "ek9: No name given"))
  (when (string-match-p "[[:space:]]" name)
    (user-error "ek9: A view name cannot contain a space"))
  ;; ":" opens the type, not the view, if both have the name.
  (when (assoc name (ek9-list--resource-choices))
    (user-error "ek9: %s is the name of a resource type.  Use another name"
                name))
  (when (or ek9-list--args ek9-list--owner)
    (user-error "ek9: A drill-down list cannot become a saved view"))
  (let* ((resource ek9-list--resource)
         (view (append
                (list :resource (ek9-resource-name resource))
                ;; A view that follows the current namespace keeps
                ;; this rule.
                (when-let* (((ek9-resource-namespaced resource))
                            (namespace (if ek9-list--view
                                           ek9-list--namespace-override
                                         (ek9-list--namespace))))
                  (list :namespace namespace))
                (when ek9-list--filter
                  (list :filter ek9-list--filter))
                (when ek9-list--view-context
                  (list :context ek9-list--view-context))
                (list :wide ek9-list--wide)))
         (views (copy-tree ek9-saved-views)))
    (when (and (assoc name views)
               (not (y-or-n-p (format "Replace the saved view %s? " name))))
      (user-error "ek9: Cancelled"))
    (setf (alist-get name views nil nil #'equal) view)
    (message "ek9: Saved view %s: %s%s" name (ek9-saved-view-describe view)
             (ek9-saved-view--stored-text (ek9-saved-view--store views)))))

;;;###autoload
(defun ek9-saved-view-delete (name)
  "Delete the saved view NAME."
  (interactive (list (ek9-saved-view--read "Delete saved view")))
  (unless (assoc name ek9-saved-views)
    (user-error "ek9: No saved view %s" name))
  (when (y-or-n-p (format "Delete the saved view %s? " name))
    (message "ek9: Deleted saved view %s%s" name
             (ek9-saved-view--stored-text
              (ek9-saved-view--store
               (seq-remove (lambda (entry) (equal (car entry) name))
                           ek9-saved-views))))))

(provide 'ek9-saved-view)

;;; ek9-saved-view.el ends here
