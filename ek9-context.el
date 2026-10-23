;;; ek9-context.el --- Contexts and namespaces for ek9 -*- lexical-binding: t; -*-

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

;; This file keeps the current kubectl context and namespace for ek9.
;; ek9 does not change the current context in kubeconfig.  It gives
;; "--context" and "--namespace" to each kubectl call instead.  Other
;; tools that use the same kubeconfig are not affected.

;;; Code:

(require 'subr-x)
(require 'ek9-process)

(defcustom ek9-read-only-contexts nil
  "List of kubectl context names where ek9 blocks write actions."
  :type '(repeat string)
  :group 'ek9)

(defvar ek9-context-changed-hook nil
  "Hook that runs after the context or the namespace changes.")

(defconst ek9-context-all-namespaces-label "<all>"
  "Completion label for all namespaces.
Namespace names cannot contain \"<\" or \">\", so this label cannot
be the same as a real namespace name.")

(defvar ek9-context--current nil
  "Name of the current context, or nil if ek9 did not load it.")

(defvar ek9-context--namespace nil
  "Current namespace: a string, or the symbol `all'.")

(defvar ek9-context--contexts nil
  "Alist of (NAME . DEFAULT-NAMESPACE) for each context in kubeconfig.
DEFAULT-NAMESPACE is nil when kubeconfig does not set one.")

(defvar ek9-context--clusters nil
  "Alist of (CONTEXT . CLUSTER) for each context in kubeconfig.")

(defvar ek9-context--namespace-cache (make-hash-table :test #'equal)
  "Hash table that maps a context name to its namespace names.")

(defvar ek9-context--last-namespace (make-hash-table :test #'equal)
  "Hash table that maps a context name to its last namespace.")

(defvar ek9-context-namespace-history nil
  "Minibuffer history for namespace names.")

;;;; State

(defun ek9-context-current ()
  "Return the name of the current context, or nil."
  ek9-context--current)

(defun ek9-context-namespace ()
  "Return the current namespace: a string, or the symbol `all'."
  ek9-context--namespace)

(defun ek9-context--namespace-label (&optional namespace)
  "Return NAMESPACE as a string for display.
If NAMESPACE is nil, use the current namespace."
  (let ((namespace (or namespace ek9-context--namespace)))
    (cond ((eq namespace 'all) ek9-context-all-namespaces-label)
          (namespace)
          (t "<none>"))))

(defun ek9-context-namespace-args (namespace)
  "Return the kubectl arguments for NAMESPACE.
NAMESPACE is a string, the symbol `all', or nil.  Return nil for nil."
  (cond ((eq namespace 'all) (list "--all-namespaces"))
        (namespace (list "--namespace" namespace))))

(defun ek9-context-args (&optional namespaced)
  "Return the global kubectl arguments for the current context.
When NAMESPACED is non-nil, also return the namespace argument."
  (append (when ek9-context--current
            (list "--context" ek9-context--current))
          (when namespaced
            (ek9-context-namespace-args ek9-context--namespace))))

(defun ek9-context--default-namespace (context)
  "Return the namespace to use when CONTEXT becomes current.
Use the last namespace of CONTEXT in this session.  If there is none,
use the kubeconfig namespace of CONTEXT.  If there is none, use
\"default\"."
  (or (gethash context ek9-context--last-namespace)
      (cdr (assoc context ek9-context--contexts))
      "default"))

(defun ek9-context--set (context namespace)
  "Set CONTEXT and NAMESPACE, then run `ek9-context-changed-hook'."
  (setq ek9-context--current context
        ek9-context--namespace namespace)
  (puthash context namespace ek9-context--last-namespace)
  (run-hooks 'ek9-context-changed-hook))

(defun ek9-context-set-namespace (namespace)
  "Make NAMESPACE the current namespace of the current context.
NAMESPACE is a string or the symbol `all'.  Signal a `user-error' if
there is no current context."
  (unless ek9-context--current
    (user-error "ek9: No context is selected"))
  (ek9-context--set ek9-context--current namespace))

;;;; Loading

(defun ek9-context--parse-config (config)
  "Return (CURRENT . CONTEXTS) from the parsed kubeconfig CONFIG.
CURRENT is the kubeconfig current context, or nil.  CONTEXTS has the
format of `ek9-context--contexts'."
  (let ((current (alist-get 'current-context config)))
    (cons (unless (string-empty-p (or current "")) current)
          (mapcar (lambda (entry)
                    (cons (alist-get 'name entry)
                          (alist-get 'namespace (alist-get 'context entry))))
                  (alist-get 'contexts config)))))

(defun ek9-context--parse-clusters (config)
  "Return an alist of (CONTEXT . CLUSTER) from the parsed kubeconfig CONFIG.
CLUSTER is nil if the context has no cluster.  kubectl writes an
empty string in this case."
  (mapcar (lambda (entry)
            (let ((cluster (alist-get 'cluster (alist-get 'context entry))))
              (cons (alist-get 'name entry)
                    (unless (string-empty-p (or cluster "")) cluster))))
          (alist-get 'contexts config)))

(defun ek9-context-cluster (&optional context)
  "Return the cluster name of CONTEXT, or nil.
If CONTEXT is nil, use the current context."
  (cdr (assoc (or context ek9-context--current) ek9-context--clusters)))

(defun ek9-context-load (&optional callback)
  "Load the contexts from kubeconfig asynchronously.
If ek9 has no valid current context, use the kubeconfig current
context.  Then call CALLBACK with no arguments."
  (ek9-process-check-kubectl)
  ;; "kubectl config view" removes certificate and token data from its
  ;; output, so no secrets come into Emacs.
  (ek9-process-run-json
   '("config" "view" "-o" "json")
   (lambda (config)
     (let ((parsed (ek9-context--parse-config config)))
       (setq ek9-context--contexts (cdr parsed)
             ek9-context--clusters (ek9-context--parse-clusters config))
       (unless (assoc ek9-context--current ek9-context--contexts)
         (setq ek9-context--current (car parsed)
               ek9-context--namespace
               (when (car parsed)
                 (ek9-context--default-namespace (car parsed))))))
     (when callback
       (funcall callback)))))

(defun ek9-context-ensure (callback)
  "Call CALLBACK with no arguments when ek9 has a current context.
Load the contexts first if necessary."
  (if ek9-context--current
      (funcall callback)
    (ek9-context-load callback)))

(defun ek9-context-cached-namespaces ()
  "Return the cached namespace names of the current context, or nil."
  (and ek9-context--current
       (gethash ek9-context--current ek9-context--namespace-cache)))

(defvar ek9-context--prefetch-process nil
  "The kubectl job of `ek9-context-prefetch-namespaces', or nil.")

(defun ek9-context-prefetch-namespaces ()
  "Load the namespace names of the current context again in the background.
Keep the old names in the cache until the new names arrive.  Do
nothing if there is no current context, or if a load already runs.
Show no message if kubectl fails, for example because of RBAC.
Return the job, or nil."
  (let ((context ek9-context--current)
        ;; The user waits for nothing: completion uses the cache.
        (ek9-process-priority 'background))
    (when (and context (not (ek9-process-live-p ek9-context--prefetch-process)))
      (setq ek9-context--prefetch-process
            (ek9-process-run-json
             (append '("get" "namespaces" "-o" "json") (ek9-context-args))
             (lambda (json)
               (puthash context
                        (mapcar (lambda (item)
                                  (alist-get 'name (alist-get 'metadata item)))
                                (alist-get 'items json))
                        ek9-context--namespace-cache))
             #'ignore)))))

(defun ek9-context-namespaces (callback &optional refresh)
  "Call CALLBACK with the namespace names of the current context.
Use the cache, unless REFRESH is non-nil or the cache is empty.
If kubectl fails, for example when RBAC does not let the user list
namespaces, show a message and call CALLBACK with nil."
  (let* ((context ek9-context--current)
         (cached (unless refresh
                   (gethash context ek9-context--namespace-cache))))
    (if cached
        (funcall callback cached)
      (ek9-process-run-json
       (append '("get" "namespaces" "-o" "json") (ek9-context-args))
       (lambda (json)
         (let ((names (mapcar (lambda (item)
                                (alist-get 'name (alist-get 'metadata item)))
                              (alist-get 'items json))))
           (puthash context names ek9-context--namespace-cache)
           (funcall callback names)))
       (lambda (_exit-code stderr)
         (message "ek9: Cannot list namespaces: %s"
                  (ek9-process-error-summary stderr))
         (funcall callback nil))))))

;;;; Read-only contexts

(defun ek9-context-read-only-p (&optional context)
  "Return non-nil if CONTEXT is in `ek9-read-only-contexts'.
If CONTEXT is nil, use the current context."
  (and (member (or context ek9-context--current) ek9-read-only-contexts)
       t))

(defun ek9-context-check-writable ()
  "Signal a `user-error' if the current context is read-only.
Call this function before each write action."
  (when (ek9-context-read-only-p)
    (user-error "ek9: Context %s is read-only" ek9-context--current)))

;;;; Display

(defun ek9-context-header-line (&optional namespace)
  "Return a header line string with the current context and namespace.
If NAMESPACE is non-nil, show it in place of the current namespace."
  (concat (propertize "Context: " 'face 'bold)
          (or ek9-context--current "<none>")
          (when-let* ((cluster (ek9-context-cluster)))
            (concat "  " (propertize "Cluster: " 'face 'bold) cluster))
          "  "
          (propertize "Namespace: " 'face 'bold)
          (ek9-context--namespace-label namespace)
          (when (ek9-context-read-only-p)
            (propertize "  [read-only]" 'face 'warning))))

;;;; Commands

(defun ek9-context-set (context)
  "Make CONTEXT the current context, with its default namespace.
Signal a `user-error' if CONTEXT is not in kubeconfig.  Do nothing if
CONTEXT is already the current context."
  (unless (assoc context ek9-context--contexts)
    (user-error "ek9: Context %s is not in kubeconfig" context))
  (unless (equal context ek9-context--current)
    (ek9-context--set context (ek9-context--default-namespace context))))

(defun ek9-context--read-and-switch (&optional callback)
  "Ask for a context and make it the current context.
If the context changes, call CALLBACK with no arguments after the
change."
  (unless ek9-context--contexts
    (user-error "ek9: No contexts in kubeconfig"))
  (let ((context (completing-read
                  (format-prompt "Context" ek9-context--current)
                  (mapcar #'car ek9-context--contexts)
                  nil t nil nil ek9-context--current)))
    (unless (equal context ek9-context--current)
      (ek9-context-set context)
      (when callback
        (funcall callback)))))

;;;###autoload
(defun ek9-context-switch (&optional refresh callback)
  "Select the kubectl context for ek9.
This command does not change the current context in kubeconfig.
With prefix argument REFRESH, load the contexts from kubeconfig again.
If the context changes, call CALLBACK with no arguments after the
change."
  (interactive "P")
  (if (and ek9-context--contexts (not refresh))
      (ek9-context--read-and-switch callback)
    (ek9-context-load (lambda () (ek9-context--read-and-switch callback)))))

(defun ek9-context--read-and-switch-namespace (names &optional callback)
  "Ask for a namespace from NAMES and make it the current namespace.
The user can also type a name that is not in NAMES.  Call CALLBACK
with no arguments after the change, if it is non-nil."
  (let* ((default (ek9-context--namespace-label))
         (choice (completing-read
                  (format-prompt "Namespace" default)
                  (cons ek9-context-all-namespaces-label names)
                  nil nil nil 'ek9-context-namespace-history default)))
    (when (string-empty-p choice)
      (user-error "ek9: No namespace given"))
    (ek9-context--set ek9-context--current
                      (if (equal choice ek9-context-all-namespaces-label)
                          'all
                        choice))
    (when callback
      (funcall callback))))

;;;###autoload
(defun ek9-context-switch-namespace (&optional refresh callback)
  "Select the namespace for ek9 in the current context.
With prefix argument REFRESH, get the namespace list from the cluster
again.  Call CALLBACK with no arguments after the change, if it is
non-nil."
  (interactive "P")
  (ek9-context-ensure
   (lambda ()
     (ek9-context-namespaces
      (lambda (names) (ek9-context--read-and-switch-namespace names callback))
      refresh))))

(provide 'ek9-context)

;;; ek9-context.el ends here
