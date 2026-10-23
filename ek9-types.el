;;; ek9-types.el --- Discovered resource types for ek9 -*- lexical-binding: t; -*-

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

;; This file finds the resource types of a cluster that ek9 does not
;; define with `ek9-define-resource', and makes a resource type for each
;; of them.  These are the custom resource types (CRDs), and built-in
;; types such as ingresses or persistentvolumeclaims.  ek9 keeps the
;; types of each context in a cache.
;;
;; The type list comes from "kubectl api-resources --verbs=list".  This
;; is the small discovery data, without the schemas of the CRDs.
;;
;; The columns come from the server table, see `ek9-proxy'.  Thus, a
;; discovered type needs no code of its own.  The API server applies
;; the additionalPrinterColumns of a CRD.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)

(defconst ek9-types--builtin-groups
  '("" "admissionregistration.k8s.io" "apiextensions.k8s.io"
    "apiregistration.k8s.io" "apps" "authentication.k8s.io"
    "authorization.k8s.io" "autoscaling" "batch" "certificates.k8s.io"
    "coordination.k8s.io" "discovery.k8s.io" "events.k8s.io" "extensions"
    "flowcontrol.apiserver.k8s.io" "internal.apiserver.k8s.io"
    "metrics.k8s.io" "networking.k8s.io" "node.k8s.io" "policy"
    "rbac.authorization.k8s.io" "resource.k8s.io" "scheduling.k8s.io"
    "storage.k8s.io" "storagemigration.k8s.io")
  "API groups of Kubernetes itself.
A type of these groups gets its plural as its name, if the name is
free.  A CRD type gets the name PLURAL.GROUP.")

(defvar ek9-types--cache (make-hash-table :test #'equal)
  "Hash table that maps a context name to its discovered types.
Each value is an alist of (NAME . RESOURCE), sorted by NAME.  A
context that is not in the table has no loaded types.  A context
with the value nil has no discovered types, or the load failed.")

(defun ek9-types--loaded-p (context)
  "Return non-nil if `ek9-types--cache' has an entry for CONTEXT."
  (not (eq (gethash context ek9-types--cache 'none) 'none)))

(defvar ek9-types--jobs (make-hash-table :test #'equal)
  "Hash table that maps a context name to the job that loads its types.")

(defvar ek9-types--callbacks (make-hash-table :test #'equal)
  "Hash table that maps a context name to a list of functions.
The functions wait for the discovered types of the context.")

(defvar ek9-saved-views)

;;;; Types

(defun ek9-types--group (entry)
  "Return the API group of the discovery ENTRY, \"\" for the core group."
  (or (alist-get 'group entry) ""))

(defun ek9-types--builtin-p (entry)
  "Return non-nil if the discovery ENTRY is a type of Kubernetes itself."
  (member (ek9-types--group entry) ek9-types--builtin-groups))

(defun ek9-types--registry-p (entry)
  "Return non-nil if a registry type has the API type of ENTRY.
The version does not count, so that a registry type also covers the
other versions of its type."
  (let ((group (ek9-types--group entry))
        (plural (alist-get 'name entry)))
    (seq-some (lambda (pair)
                (pcase (ek9-resource-api (cdr pair))
                  (`(,g ,_ ,p) (and (equal g group) (equal p plural)))))
              ek9-resource--registry)))

(defun ek9-types--actions (entry)
  "Return the actions for the discovery ENTRY.
Edit needs the verb \"update\", and delete needs the verb \"delete\"."
  (let ((verbs (alist-get 'verbs entry)))
    (append '(describe yaml)
            (when (member "update" verbs) '(edit))
            (when (member "delete" verbs) '(delete))
            '(events))))

(defun ek9-types--resource (entry name)
  "Return the resource type NAME for the discovery ENTRY.
ENTRY is one element of the resources of \"kubectl api-resources\".
`ek9-types-parse' sets the aliases."
  (let* ((plural (alist-get 'name entry))
         (group (ek9-types--group entry))
         (version (alist-get 'version entry)))
    (ek9-resource-create
     :name name
     ;; kubectl reads the type of the core group without a group.
     :kind (if (string-empty-p group)
               plural
             (concat plural "." version "." group))
     :api (list group version plural)
     :namespaced (alist-get 'namespaced entry)
     :actions (ek9-types--actions entry))))

(defun ek9-types--entry-aliases (entry)
  "Return the short names of the discovery ENTRY, without duplicates."
  (delete-dups (delq nil (append (list (alist-get 'name entry)
                                       (ek9-resource--non-empty
                                        (alist-get 'singularName entry)))
                                 (alist-get 'shortNames entry)))))

(defun ek9-types--registry-names ()
  "Return the names and the short names of the registry types."
  (let (names)
    (pcase-dolist (`(,name . ,resource) ek9-resource--registry)
      (push (symbol-name name) names)
      (setq names (append (ek9-resource-aliases resource) names)))
    names))

(defun ek9-types--names (entries)
  "Return the type name for each discovery entry in ENTRIES.
A built-in type gets its plural as its name, unless a registry type
uses the plural as its name or short name, or an earlier built-in
entry has the same plural.  The order of ENTRIES is the order of the
API server, so the result is the same each time.  Other types get the
name PLURAL.GROUP, or PLURAL.core for the core group.  The names do
not depend on the CRDs or on the saved views, so that a saved view of
a built-in type works in each context."
  (let ((used (ek9-types--registry-names)))
    (mapcar (lambda (entry)
              (let ((plural (alist-get 'name entry))
                    (group (ek9-types--group entry)))
                (intern
                 (cond ((and (ek9-types--builtin-p entry)
                             (not (member plural used)))
                        (push plural used)
                        plural)
                       ((string-empty-p group) (concat plural ".core"))
                       (t (concat plural "." group))))))
            entries)))

(defun ek9-types-parse (json)
  "Return the discovered types in the \"kubectl api-resources\" JSON.
The value is an alist of (NAME . RESOURCE), sorted by NAME.  Skip the
types that the registry defines.  See `ek9-types--names' for the names.
A type gets its plural, singular, and short names as aliases, unless
a type name, a registry short name, a saved view, or another type
uses the same name.  A type whose plural a registry type uses gets
no aliases, so that for example \"event\" does not open another type
than \"events\"."
  (let* ((entries (seq-remove #'ek9-types--registry-p (alist-get 'resources json)))
         (registry (ek9-types--registry-names))
         (names (ek9-types--names entries))
         ;; A saved view with the name of a type cannot open with
         ;; ":", because the type wins.
         (taken (append (mapcar #'symbol-name names)
                        registry
                        (mapcar #'car (bound-and-true-p ek9-saved-views))))
         (counts (make-hash-table :test #'equal))
         (types nil))
    (dolist (entry entries)
      (dolist (alias (ek9-types--entry-aliases entry))
        (puthash alias (1+ (gethash alias counts 0)) counts)))
    (cl-loop
     for entry in entries
     for name in names
     do (let ((resource (ek9-types--resource entry name)))
          (setf (ek9-resource-aliases resource)
                (unless (member (alist-get 'name entry) registry)
                  (seq-filter (lambda (alias)
                                (and (= (gethash alias counts) 1)
                                     (not (member alias taken))))
                              (ek9-types--entry-aliases entry))))
          (push (cons name resource) types)))
    (sort types (lambda (a b) (string< (car a) (car b))))))

(defun ek9-types-current ()
  "Return the discovered types of the current context.
The value is an alist of (NAME . RESOURCE).  Return nil if ek9 did
not load them yet."
  (and (ek9-context-current)
       (gethash (ek9-context-current) ek9-types--cache)))

(defun ek9-types--lookup (name)
  "Return the discovered type NAME of the current context, or nil."
  (alist-get name (ek9-types-current)))

(add-hook 'ek9-resource-lookup-functions #'ek9-types--lookup)

(defun ek9-types--finish (context jobs waiting)
  "Remove the load job of CONTEXT from JOBS, and call its functions.
WAITING has the format of `ek9-types--callbacks'.  An error in one
function does not stop the others."
  (remhash context jobs)
  (let ((callbacks (reverse (gethash context waiting))))
    (remhash context waiting)
    (dolist (callback callbacks)
      (with-demoted-errors "ek9: Error after the types loaded: %S"
        (funcall callback)))))

(defun ek9-types--load (refresh priority)
  "Load the discovered types of the current context with PRIORITY.
PRIORITY is a value of `ek9-process-priority'.  Do nothing if the
types are in the cache, unless REFRESH is non-nil.  If a load for
the context runs, do not start another one.  Return the job, or nil."
  (let* ((context (ek9-context-current))
         ;; The callbacks must use these tables, also if they run later.
         (table ek9-types--cache)
         (jobs ek9-types--jobs)
         (waiting ek9-types--callbacks)
         (job (and context (gethash context jobs))))
    (cond
     ((null context) nil)
     ((ek9-process-live-p job)
      (when (eq priority 'user)
        (ek9-process-prioritize job))
      job)
     ((and (not refresh) (ek9-types--loaded-p context)) nil)
     (t
      (let* ((ek9-process-priority priority)
             (failed
              (lambda (text)
                (puthash context nil table)
                (message "ek9: Cannot load the types of the cluster: %s.  C-u : tries again"
                         text)
                (ek9-types--finish context jobs waiting))))
        (puthash
         context
         (ek9-process-run-json
          (append '("api-resources" "--verbs=list" "-o" "json")
                  (ek9-context-args))
          (lambda (json)
            (pcase (condition-case err
                       (cons 'ok (ek9-types-parse json))
                     (error (cons 'error (error-message-string err))))
              (`(error . ,text) (funcall failed text))
              (`(ok . ,types)
               (puthash context types table)
               (when refresh
                 ;; The open lists still have the old type objects.
                 (when (equal context (ek9-context-current))
                   (ek9-types--update-buffers t))
                 (message "ek9: Loaded %d types" (length types)))
               (ek9-types--finish context jobs waiting))))
          (lambda (_exit-code stderr)
            (funcall failed (ek9-process-error-summary stderr))))
         jobs))))))

(defun ek9-types-prefetch (&optional refresh)
  "Load the discovered types of the current context in the background.
Do nothing if they are in the cache, unless REFRESH is non-nil.  A
REFRESH load does not wait behind the timer refreshes.  If kubectl
fails, show a message and keep an empty list, so that ek9 does not
try again until a refresh.  Return the job, or nil."
  (ek9-types--load refresh (if refresh 'user 'background)))

(defun ek9-types-ensure (callback)
  "Call CALLBACK with no arguments when the discovered types are loaded.
Load the types of the current context if they are not in the cache.
The load does not wait behind the timer refreshes.  If the load
fails, call CALLBACK too.  If the types are in the cache, return the
value of CALLBACK."
  (let ((context (ek9-context-current)))
    (if (or (null context) (ek9-types--loaded-p context))
        (funcall callback)
      (push callback (gethash context ek9-types--callbacks))
      (ek9-types--load nil 'user)
      nil)))

(defun ek9-types--update-buffers (refresh)
  "Give the list buffers of discovered types the types of this context.
A type of another context can have another version and other
columns.  If the current context does not have the type, keep the
old one: then kubectl shows the error in the tab line.  If REFRESH is
non-nil, also remove the old rows and refresh the visible buffers.
Else `ek9-list--context-changed' does this.  Skip the paused views
and the drill-down buffers of an old context."
  (dolist (buffer (ek9-list--buffers))
    (with-demoted-errors "ek9: Cannot update a list: %S"
      (with-current-buffer buffer
        (let* ((old ek9-list--resource)
               (name (ek9-resource-name old))
               (new (and (not (assq name ek9-resource--registry))
                         (not (ek9-list--paused-p))
                         (not (ek9-list--stale-p buffer))
                         (ek9-types--lookup name))))
          (when (and new (not (eq new old)))
            (setq ek9-list--resource new
                  tabulated-list-sort-key (ek9-resource-sort new))
            (when refresh
              (ek9-list--reset))
            (when (and refresh (get-buffer-window buffer 'visible))
              (ek9-list-refresh))))))))

(defun ek9-types--context-changed ()
  "Load the types of the new context, and update the lists of them.
If the types are in the cache, only give the lists the new types:
`ek9-list--context-changed' runs after this function and refreshes
them.  If ek9 must load the types first, refresh the lists after the
load."
  (let ((now t))
    (ek9-types-ensure (lambda () (ek9-types--update-buffers (not now))))
    (setq now nil)))

;; Run before `ek9-list--context-changed', see above.
(add-hook 'ek9-context-changed-hook #'ek9-types--context-changed -50)

(provide 'ek9-types)

;;; ek9-types.el ends here
