;;; ek9-transient.el --- Action menus for ek9 -*- lexical-binding: t; -*-

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

;; This file defines `ek9-dispatch', the menu that "?" opens in a list
;; buffer.  The menu shows all keys of the list buffer.  An action that
;; the resource type does not have is shown, but it is not available.
;; In a read-only context, the write actions are not available.
;;
;; The menu uses only features of transient 0.7.2, the version in
;; Emacs 30.2.

;;; Code:

(require 'transient)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)

(declare-function ek9-describe "ek9-describe")
(declare-function ek9-yaml "ek9-describe")
(declare-function ek9-secret-decode "ek9-describe")
(declare-function ek9-logs "ek9-logs")
(declare-function ek9-edit "ek9-edit")
(declare-function ek9-shell "ek9-exec")
(declare-function ek9-port-forward "ek9-portfwd")
(declare-function ek9-port-forwards "ek9-portfwd")
(declare-function ek9-delete "ek9-action")
(declare-function ek9-scale "ek9-action")
(declare-function ek9-restart "ek9-action")
(declare-function ek9-saved-view-open "ek9-saved-view")
(declare-function ek9-kill-all-buffers "ek9")

;;;; Predicates

(defun ek9-transient--has-action-p (action)
  "Return non-nil if the resource type of the buffer has ACTION."
  (and ek9-list--resource
       (memq action (ek9-resource-actions ek9-list--resource))))

(defun ek9-transient--can-write-p (action)
  "Return non-nil if the write ACTION is available in the buffer.
The resource type must have ACTION, a context must be selected, and
the context must not be read-only.  These are the same rules as in
`ek9-action-check'."
  (and (ek9-transient--has-action-p action)
       (ek9-context-current)
       (not (ek9-context-read-only-p))))

(defun ek9-transient--can-drill-down-p ()
  "Return non-nil if the resource type of the buffer has a level below."
  (and ek9-list--resource
       (ek9-resource-drill-down ek9-list--resource)))

(defun ek9-transient--title ()
  "Return the title of the menu, with the context and the namespace."
  (concat (propertize "ek9  " 'face 'transient-heading)
          (ek9-context-header-line (ek9-list--namespace))
          (when ek9-list--resource
            (concat "  " (propertize "View: " 'face 'bold)
                    (symbol-name (ek9-resource-name ek9-list--resource))))))

;;;; Menu

;; No autoload: the menu works only in a list buffer, where "?" opens it.
(transient-define-prefix ek9-dispatch ()
  "Show the ek9 keys for the list buffer."
  [:description ek9-transient--title
   ["Navigate"
    (":" "Resource type" ek9-list-select-resource)
    ("RET" "Go down" ek9-list-drill-down
     :inapt-if-not ek9-transient--can-drill-down-p)
    ("^" "Go up" ek9-list-up
     :inapt-if-not (lambda () (buffer-live-p ek9-list--parent)))
    ("n" "Namespace" ek9-list-switch-namespace
     :inapt-if (lambda () (and ek9-list--namespace-override
                               (not ek9-list--view))))
    ("c" "Context" ek9-list-switch-context)
    ("v" "Saved view" ek9-saved-view-open)
    ("/" "Filter" ek9-list-filter)
    ("o" "Sort by column" tabulated-list-sort)
    ("w" "Wide columns" ek9-list-toggle-wide)
    ("g" "Refresh" ek9-list-refresh)
    ("Q" "Kill all ek9 buffers" ek9-kill-all-buffers)]
   ["View"
    ("d" "Describe" ek9-describe
     :inapt-if-not (lambda () (ek9-transient--has-action-p 'describe)))
    ("y" "YAML" ek9-yaml
     :inapt-if-not (lambda () (ek9-transient--has-action-p 'yaml)))
    ("l" "Logs" ek9-logs
     :inapt-if-not (lambda () (ek9-transient--has-action-p 'logs)))
    ("x" "Decode secret" ek9-secret-decode
     :inapt-if-not (lambda () (ek9-transient--has-action-p 'decode)))
    ("E" "Events" ek9-list-events
     :inapt-if-not (lambda () (ek9-transient--has-action-p 'events)))
    ("F" "Port-forwards" ek9-port-forwards)]
   ["Act"
    ("e" "Edit" ek9-edit
     :inapt-if-not (lambda () (ek9-transient--can-write-p 'edit)))
    ("s" "Shell" ek9-shell
     :inapt-if-not (lambda () (ek9-transient--can-write-p 'shell)))
    ("f" "Port-forward" ek9-port-forward
     :inapt-if-not (lambda () (ek9-transient--has-action-p 'port-forward)))
    ("S" "Scale" ek9-scale
     :inapt-if-not (lambda () (ek9-transient--can-write-p 'scale)))
    ("R" "Restart" ek9-restart
     :inapt-if-not (lambda () (ek9-transient--can-write-p 'restart)))
    ("D" "Delete" ek9-delete
     :inapt-if-not (lambda () (ek9-transient--can-write-p 'delete)))]])

(provide 'ek9-transient)

;;; ek9-transient.el ends here
