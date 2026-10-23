;;; ek9-resource.el --- Resource definitions for ek9 -*- lexical-binding: t; -*-

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

;; This file defines the resource types that ek9 can show.  Each
;; resource type is data: a kubectl kind, a namespaced flag, a list of
;; columns, and a list of actions.  Use `ek9-define-resource' to add a
;; resource type.
;;
;; The pod status functions use the same rules as "kubectl get pods".

;;; Code:

(require 'cl-lib)
(require 'parse-time)
(require 'seq)
(require 'subr-x)

(cl-defstruct (ek9-resource (:constructor ek9-resource-create)
                            (:copier nil))
  "A resource type that ek9 can show."
  (name nil :documentation "Symbol that identifies the resource type.")
  (kind nil :documentation "Resource name for kubectl, for example \"pods\".")
  (api nil :documentation "The API type: (GROUP VERSION PLURAL), or nil.
GROUP is \"\" for the core group, for example (\"apps\" \"v1\"
\"deployments\").  ek9 reads lists through `ek9-proxy'.  For a type
without :items, ek9 reads the server table: the table gives the
columns, so the type has no :columns.  A type with :items reads the
JSON object and uses its own :columns.")
  (namespaced t :documentation "Non-nil if the resources are in a namespace.")
  (columns nil :documentation "List of column definitions, for a type without :api.
Each column has the form (TITLE WIDTH GETTER . PROPERTIES).
TITLE is a string.  WIDTH is the column width in characters.
WIDTH is the default width.  A column gets wider when a value is
longer, up to `ek9-list-max-column-width'.  If the window is too
narrow, a column can get narrower, see `ek9-list--widths'.
GETTER is a list of JSON keys, for example (metadata name), or a
function that takes the resource item and returns the value.
PROPERTIES is a plist with these keys:
  :sort-key     A function that takes the item and returns a number.
                ek9 sorts the column by this number.
  :right-align  Non-nil to align the column to the right.
  :wide         Non-nil if only \"kubectl get -o wide\" shows the
                column.  See `ek9-list-wide'.
  :default      The value to show when the value is nil or \"\".")
  (actions nil :documentation "List of action symbols for this type.")
  (aliases nil :documentation "List of short names, for example (\"po\").")
  (sort nil :documentation "The default sort, or nil for the kubectl order.
The value has the form of `tabulated-list-sort-key'.")
  (drill-down nil :documentation "Function for the next level, or nil.
The function takes a resource item and returns a plist with these keys:
  :resource          The name of the resource type to show.
  :args              Extra arguments: \"--selector\" and a label
                     selector, \"--field-selector\" and a field
                     selector, or an object name.  They become the
                     API path, see `ek9-list--request-path'.
  :label             Short text that describes :args, for the tab line.
  :namespace         The namespace to show: a string or the symbol `all'.
                     If nil, use the current namespace.
  :owner-uid         Show only items with this controller owner UID.
  :owner             Short text that describes :owner-uid.
  :switch-namespace  A namespace that becomes the current namespace.")
  (row-face nil :documentation "Function for the face of a row, or nil.
The function takes a resource item and returns a face, or nil for
no face.  Usually the face is one of `ek9-status-error',
`ek9-status-warning', `ek9-status-done', and `ek9-status-ok'.")
  (items nil :documentation "Function that makes the rows, or nil.
The function takes the JSON object that ek9 reads through
`ek9-proxy', and returns a list of items.  If nil, ek9 reads the
server table, and each row is an item.")
  (target nil :documentation "Function for the object of an action, or nil.
The function takes an item and returns (RESOURCE OBJECT CONTAINER).
RESOURCE is the name of the resource type of OBJECT.  OBJECT is the
Kubernetes object that actions such as describe use.  CONTAINER is a
container name, or nil.  If nil, actions use the item itself.")
  (hidden nil :documentation "Non-nil to not offer this type in \":\".
Use it for a type that only a drill-down can show.")
  (metrics nil :documentation "The type in the metrics API, or nil.
The value is \"pods\" or \"nodes\".  The list then has CPU and memory
columns from the metrics API, see `ek9-metrics-columns'.  These are
the only columns of an API type that do not come from the server
table."))

(defvar ek9-resource--registry nil
  "Alist of (NAME . RESOURCE) for each defined resource type.")

(defvar ek9-resource-lookup-functions nil
  "Functions that find resource types that are not in the registry.
`ek9-resource-get' calls each function with the name of the type.
The first function that returns a resource type wins.  For example,
the discovered types of the current context use this hook, see
`ek9-types-current'.")

(defun ek9-resource-register (resource)
  "Add RESOURCE to the resource types and return it.
If a resource type with the same name exists, replace it."
  (setf (alist-get (ek9-resource-name resource) ek9-resource--registry)
        resource)
  resource)

(defmacro ek9-define-resource (name &rest properties)
  "Define the resource type NAME with PROPERTIES.
PROPERTIES is a plist with the keys :kind, :api, :namespaced,
:columns, :actions, :aliases, :sort, :drill-down, :row-face, :items, :target,
:hidden, and :metrics.  See `ek9-resource' for the meaning of each key."
  (declare (indent 1))
  `(ek9-resource-register (ek9-resource-create :name ',name ,@properties)))

(defun ek9-resource-get (name)
  "Return the resource type NAME.
If NAME is not in the registry, use `ek9-resource-lookup-functions'.
Signal an error if NAME is not defined."
  (or (alist-get name ek9-resource--registry)
      (run-hook-with-args-until-success 'ek9-resource-lookup-functions name)
      (error "ek9: Unknown resource type %s" name)))

;;;; Column values

(defun ek9-resource-path (item path)
  "Return the value at PATH in ITEM.
PATH is a list of JSON keys."
  (let ((value item))
    (dolist (key path value)
      (setq value (alist-get key value)))))

(defun ek9-resource-column-value (column item)
  "Return the raw value of COLUMN for ITEM."
  (let ((getter (nth 2 column)))
    (if (functionp getter)
        (funcall getter item)
      (ek9-resource-path item getter))))

(defun ek9-resource-column-string (column item)
  "Return the value of COLUMN for ITEM as a string.
Use the :default property of COLUMN when the value is nil or \"\".
Replace line breaks with spaces, so that each row is one line."
  (let ((value (ek9-resource-column-value column item)))
    (when (or (null value) (equal value ""))
      (setq value (plist-get (nthcdr 3 column) :default)))
    (if value
        (replace-regexp-in-string "[\n\r]+" " " (format "%s" value))
      "")))

(defun ek9-resource-item-name (item)
  "Return the name of the resource ITEM."
  (ek9-resource-path item '(metadata name)))

(defun ek9-resource-item-namespace (item)
  "Return the namespace of the resource ITEM, or nil."
  (ek9-resource-path item '(metadata namespace)))

(defun ek9-resource-item-id (item)
  "Return a unique ID for the resource ITEM.
The ID is \"NAMESPACE/NAME\", or \"NAME\" for resources that are
not in a namespace."
  (let ((namespace (ek9-resource-item-namespace item)))
    (if namespace
        (concat namespace "/" (ek9-resource-item-name item))
      (ek9-resource-item-name item))))

;;;; Durations

(defun ek9-resource--non-empty (string)
  "Return STRING if it is a non-empty string, or nil."
  (and (stringp string) (not (string-empty-p string)) string))

(defun ek9-resource-human-duration (seconds)
  "Return SECONDS as a short duration string, such as \"5m30s\" or \"3d\".
The format is the same as the AGE column of kubectl.  A time up to
one second in the future gives \"0s\", because clocks can differ a
little.  A time more in the future gives \"<invalid>\"."
  (let* ((s (truncate seconds))
         (minutes (/ s 60))
         (hours (/ s 3600))
         (days (/ hours 24)))
    (cond ((< s -1) "<invalid>")
          ((< s 0) "0s")
          ((< s 120) (format "%ds" s))
          ((< minutes 10)
           (if (zerop (% s 60))
               (format "%dm" minutes)
             (format "%dm%ds" minutes (% s 60))))
          ((< minutes 180) (format "%dm" minutes))
          ((< hours 8)
           (if (zerop (% minutes 60))
               (format "%dh" hours)
             (format "%dh%dm" hours (% minutes 60))))
          ((< hours 48) (format "%dh" hours))
          ((< hours (* 24 8))
           (if (zerop (% hours 24))
               (format "%dd" days)
             (format "%dd%dh" days (% hours 24))))
          ((< hours (* 24 365 2)) (format "%dd" days))
          ((< hours (* 24 365 8))
           (if (zerop (% days 365))
               (format "%dy" (/ days 365))
             (format "%dy%dd" (/ days 365) (% days 365))))
          (t (format "%dy" (/ days 365))))))

(defun ek9-resource-seconds-since (timestamp)
  "Return the number of seconds since the RFC 3339 TIMESTAMP, or nil."
  (when (ek9-resource--non-empty timestamp)
    (- (float-time) (float-time (parse-iso8601-time-string timestamp)))))

(defun ek9-resource-since (timestamp)
  "Return the time since the RFC 3339 TIMESTAMP as a string, or nil."
  (when-let* ((seconds (ek9-resource-seconds-since timestamp)))
    (ek9-resource-human-duration seconds)))

;;;; Table cells

(defun ek9-resource-cell (item title)
  "Return the text of the column TITLE in the table row of ITEM, or nil.
ITEM comes from a server table, see `ek9-proxy-table-items'.  Return
nil if the table has no column TITLE, or the row has no cell for it."
  (when-let* ((titles (alist-get 'ek9-titles item))
              (index (seq-position titles title))
              (cells (alist-get 'ek9-cells item))
              ((< index (length cells))))
    (aref cells index)))

;;;; Container states

(defun ek9-resource--terminated-reason (terminated)
  "Return the reason text for the TERMINATED container state."
  (or (ek9-resource--non-empty (alist-get 'reason terminated))
      (let ((signal (alist-get 'signal terminated)))
        (if (and signal (/= signal 0))
            (format "Signal:%d" signal)
          (format "ExitCode:%d" (or (alist-get 'exitCode terminated) 0))))))

(defun ek9-resource--restart-info (statuses)
  "Return (COUNT . TIME) for the container STATUSES.
COUNT is the sum of the restart counts.  TIME is the latest finish
time of a last termination state, as a Lisp time value, or nil."
  (let ((count 0) last)
    (dolist (status statuses)
      (cl-incf count (or (alist-get 'restartCount status) 0))
      (when-let* ((finished (ek9-resource-path
                             status '(lastState terminated finishedAt)))
                  (time (ignore-errors (parse-iso8601-time-string finished))))
        (when (or (null last) (time-less-p last time))
          (setq last time))))
    (cons count last)))

;;;; Conditions

(defun ek9-resource--condition-true-p (item type)
  "Return non-nil if ITEM has the status condition TYPE=True."
  (cl-some (lambda (condition)
             (and (equal (alist-get 'type condition) type)
                  (equal (alist-get 'status condition) "True")))
           (ek9-resource-path item '(status conditions))))

;;;; Status faces

(defface ek9-status-error
  '((t :inherit error))
  "Face for rows with a problem, for example a pod in CrashLoopBackOff."
  :group 'ek9)

(defface ek9-status-warning
  '((t :inherit warning))
  "Face for rows that are not ready yet, for example a pending pod."
  :group 'ek9)

(defface ek9-status-done
  '((t :inherit shadow))
  "Face for rows that are complete, for example a completed job."
  :group 'ek9)

(defface ek9-status-ok
  '((t :inherit success))
  "Face for rows that are in a normal state, for example a running pod."
  :group 'ek9)

(defun ek9-resource--ratio-short-p (ratio)
  "Return non-nil if RATIO, for example \"1/3\", has fewer than all."
  (and (string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)\\'" ratio)
       (< (string-to-number (match-string 1 ratio))
          (string-to-number (match-string 2 ratio)))))

(defconst ek9-resource--pod-error-regexp
  (rx (or "Error" "BackOff" "ErrImage" "Failed" "OOMKilled" "Evicted"
          "Unknown" "Invalid" "CannotRun" "Unavailable" "DeadlineExceeded"
          "Signal:" (seq "ExitCode:" (not (in "0")))))
  "Regexp that matches the pod STATUS values that show a problem.")

(defconst ek9-resource--pod-warning-regexp
  (rx (or "Pending" "ContainerCreating" "PodInitializing" "Terminating"
          "NotReady" "SchedulingGated" (seq "Init:" digit)))
  "Regexp that matches the pod STATUS values of a pod that starts or stops.")

(defun ek9-resource-pod-face (item)
  "Return the row face of the pod ITEM.
Use the STATUS and READY cells of the server table."
  (let ((status (or (ek9-resource-cell item "STATUS") ""))
        (phase (ek9-resource-path item '(status phase)))
        (case-fold-search nil))
    (cond ((equal status "Terminating") 'ek9-status-warning)
          ;; A failed pod is an error, whatever reason kubectl shows.
          ((equal phase "Failed") 'ek9-status-error)
          ((or (equal phase "Succeeded")
               (member status '("Completed" "ExitCode:0")))
           'ek9-status-done)
          ((string-match-p ek9-resource--pod-error-regexp status)
           'ek9-status-error)
          ((string-match-p ek9-resource--pod-warning-regexp status)
           'ek9-status-warning)
          ((ek9-resource--ratio-short-p (or (ek9-resource-cell item "READY") ""))
           'ek9-status-warning)
          (t 'ek9-status-ok))))

(defun ek9-resource-replicas-face (item)
  "Return the row face of the deployment or stateful set ITEM.
Fewer ready replicas than desired replicas is a warning."
  (if (< (or (ek9-resource-path item '(status readyReplicas)) 0)
         (or (ek9-resource-path item '(spec replicas)) 0))
      'ek9-status-warning
    'ek9-status-ok))

(defun ek9-resource-replicaset-face (item)
  "Return the row face of the replica set ITEM.
An old replica set with 0 replicas uses `ek9-status-done'."
  (if (zerop (or (ek9-resource-path item '(spec replicas)) 0))
      'ek9-status-done
    (ek9-resource-replicas-face item)))

(defun ek9-resource-daemonset-face (item)
  "Return the row face of the daemon set ITEM."
  (if (< (or (ek9-resource-path item '(status numberReady)) 0)
         (or (ek9-resource-path item '(status desiredNumberScheduled)) 0))
      'ek9-status-warning
    'ek9-status-ok))

(defun ek9-resource-cronjob-face (item)
  "Return the row face of the cron job ITEM."
  (if (ek9-resource-path item '(spec suspend))
      'ek9-status-done
    'ek9-status-ok))

(defun ek9-resource-event-face (item)
  "Return the row face of the event ITEM."
  (if (equal (alist-get 'type item) "Warning")
      'ek9-status-warning
    'ek9-status-ok))

(defun ek9-resource-namespace-face (item)
  "Return the row face of the namespace ITEM."
  (if (equal (ek9-resource-path item '(status phase)) "Terminating")
      'ek9-status-warning
    'ek9-status-ok))

(defun ek9-resource-job-face (item)
  "Return the row face of the job ITEM.
Use the conditions in the same order as the STATUS column of kubectl."
  (cl-flet ((true-p (type) (ek9-resource--condition-true-p item type)))
    (cond ((true-p "Complete") 'ek9-status-done)
          ((true-p "Failed") 'ek9-status-error)
          ((ek9-resource-path item '(metadata deletionTimestamp))
           'ek9-status-warning)
          ((true-p "Suspended") 'ek9-status-warning)
          ((true-p "FailureTarget") 'ek9-status-error)
          ((true-p "SuccessCriteriaMet") 'ek9-status-done)
          (t 'ek9-status-ok))))

(defun ek9-resource-node-face (item)
  "Return the row face of the node ITEM.
A node that is not ready is an error.  A node that does not accept
new pods is a warning."
  (cond ((not (ek9-resource--condition-true-p item "Ready")) 'ek9-status-error)
        ((ek9-resource-path item '(spec unschedulable)) 'ek9-status-warning)
        (t 'ek9-status-ok)))

;;;; Containers

(defun ek9-resource-pod-containers (pod &optional no-init)
  "Return the container names of POD.
The list starts with the normal containers.  The init containers
come after them.  If NO-INIT is non-nil, do not include the init
containers."
  (mapcar (lambda (container) (alist-get 'name container))
          (append (ek9-resource-path pod '(spec containers))
                  (unless no-init
                    (ek9-resource-path pod '(spec initContainers))))))

(defun ek9-resource-pod-default-container (pod &optional no-init)
  "Return the default container name of POD.
Use the annotation \"kubectl.kubernetes.io/default-container\" if
POD has it and it names a container.  If not, use the first
container.  NO-INIT has the same meaning as in
`ek9-resource-pod-containers'."
  (let ((names (ek9-resource-pod-containers pod no-init))
        (annotation (alist-get (intern "kubectl.kubernetes.io/default-container")
                               (ek9-resource-path pod '(metadata annotations)))))
    (if (member annotation names)
        annotation
      (car names))))

;;;; Container rows

(defun ek9-resource-container-items (pod)
  "Return one item for each container of the POD object.
The list has the init containers first, then the normal containers,
then the ephemeral containers of \"kubectl debug\".
Each item has the keys kind, metadata (name and namespace), spec,
status, and the ek9 keys ek9-type and ek9-pod."
  (let* ((spec (alist-get 'spec pod))
         (status (alist-get 'status pod))
         (namespace (ek9-resource-item-namespace pod)))
    (cl-flet ((rows (specs statuses group)
                (mapcar
                 (lambda (container)
                   (let ((name (alist-get 'name container)))
                     `((kind . "Container")
                       (metadata (name . ,name) (namespace . ,namespace))
                       (spec . ,container)
                       (status . ,(seq-find (lambda (status)
                                              (equal (alist-get 'name status)
                                                     name))
                                            statuses))
                       (ek9-type . ,(cond ((not (eq group 'init))
                                           (symbol-name group))
                                          ((equal (alist-get 'restartPolicy
                                                             container)
                                                  "Always")
                                           "sidecar")
                                          (t "init")))
                       (ek9-pod . ,pod))))
                 specs)))
      (append (rows (alist-get 'initContainers spec)
                    (alist-get 'initContainerStatuses status) 'init)
              (rows (alist-get 'containers spec)
                    (alist-get 'containerStatuses status) 'container)
              (rows (alist-get 'ephemeralContainers spec)
                    (alist-get 'ephemeralContainerStatuses status)
                    'ephemeral)))))

(defun ek9-resource-pod-container-status (pod container)
  "Return the status of the CONTAINER in POD, or nil."
  (let ((status (alist-get 'status pod)))
    (seq-find (lambda (entry) (equal (alist-get 'name entry) container))
              (append (alist-get 'initContainerStatuses status)
                      (alist-get 'containerStatuses status)
                      (alist-get 'ephemeralContainerStatuses status)))))

(defun ek9-resource-container-target (item)
  "Return the action target of the container ITEM: its pod."
  (list 'pods (alist-get 'ek9-pod item) (ek9-resource-item-name item)))

(defun ek9-resource-container-state (item)
  "Return the STATE column of the container ITEM."
  (let ((state (ek9-resource-path item '(status state))))
    (cond ((alist-get 'running state) "Running")
          ((alist-get 'waiting state)
           (or (ek9-resource--non-empty
                (alist-get 'reason (alist-get 'waiting state)))
               "Waiting"))
          ((alist-get 'terminated state)
           (ek9-resource--terminated-reason (alist-get 'terminated state)))
          (t "Waiting"))))

(defun ek9-resource-container-ready (item)
  "Return the READY column of the container ITEM."
  (if (ek9-resource-path item '(status ready)) "true" "false"))

(defun ek9-resource-container-restarts (item)
  "Return the number of restarts of the container ITEM."
  (or (ek9-resource-path item '(status restartCount)) 0))

(defun ek9-resource-container-restarts-column (item)
  "Return the RESTARTS column of the container ITEM.
Use the same format as the RESTARTS column of the pods."
  (pcase-let ((`(,count . ,time)
               (ek9-resource--restart-info
                (delq nil (list (alist-get 'status item))))))
    (if (and (> count 0) time)
        (format "%d (%s ago)" count
                (ek9-resource-human-duration
                 (float-time (time-subtract nil time))))
      (number-to-string count))))

(defun ek9-resource-container-ports (item)
  "Return the PORTS column of the container ITEM, for example \"80/TCP\"."
  (mapconcat (lambda (port)
               (format "%s/%s" (alist-get 'containerPort port)
                       (or (alist-get 'protocol port) "TCP")))
             (ek9-resource-path item '(spec ports))
             ","))

(defun ek9-resource-container-started (item)
  "Return the start time of the container ITEM as a timestamp, or nil."
  (let ((state (ek9-resource-path item '(status state))))
    (or (ek9-resource-path state '(running startedAt))
        (ek9-resource-path state '(terminated startedAt)))))

(defun ek9-resource-container-age (item)
  "Return the AGE column of the container ITEM."
  (ek9-resource-since (ek9-resource-container-started item)))

(defun ek9-resource-container-age-seconds (item)
  "Return the age of the container ITEM in seconds, or nil."
  (ek9-resource-seconds-since (ek9-resource-container-started item)))

(defun ek9-resource-container-face (item)
  "Return the row face of the container ITEM."
  (let ((state (ek9-resource-container-state item))
        (case-fold-search nil))
    (cond ((member state '("Completed" "ExitCode:0")) 'ek9-status-done)
          ((string-match-p ek9-resource--pod-error-regexp state)
           'ek9-status-error)
          ((not (equal state "Running")) 'ek9-status-warning)
          ((and (not (ek9-resource-path item '(status ready)))
                (not (equal (alist-get 'ek9-type item) "init")))
           'ek9-status-warning)
          (t 'ek9-status-ok))))

;;;; Drill-down

(defun ek9-resource--selector-expression (expression)
  "Return the label selector EXPRESSION in kubectl syntax."
  (let ((key (alist-get 'key expression))
        (values (string-join (alist-get 'values expression) ",")))
    (pcase (alist-get 'operator expression)
      ("In" (format "%s in (%s)" key values))
      ("NotIn" (format "%s notin (%s)" key values))
      ("Exists" key)
      ("DoesNotExist" (concat "!" key))
      ;; Do not drop an unknown operator: the selector would get wider.
      (operator (user-error "ek9: Unknown selector operator %s" operator)))))

(defun ek9-resource-label-selector (selector)
  "Return the label SELECTOR object in kubectl syntax.
SELECTOR has the keys matchLabels and matchExpressions.  Return nil if
SELECTOR is empty."
  (let ((parts (append (mapcar (lambda (entry)
                                 (format "%s=%s" (car entry) (cdr entry)))
                               (alist-get 'matchLabels selector))
                       (mapcar #'ek9-resource--selector-expression
                               (alist-get 'matchExpressions selector)))))
    (when parts
      (string-join parts ","))))

(defun ek9-resource--pods-with-selector (item selector)
  "Return the drill-down plist for the pods of ITEM with SELECTOR.
SELECTOR is a selector string or nil.  Signal a `user-error' if it is
nil, because an empty selector selects all pods."
  (unless selector
    (user-error "ek9: %s has no pod selector" (ek9-resource-item-id item)))
  (list :resource 'pods
        :args (list "--selector" selector)
        :label selector
        :namespace (ek9-resource-item-namespace item)))

(defun ek9-resource-drill-workload (item)
  "Return the drill-down plist for the pods of the workload ITEM.
Use the label selector in spec.selector.  The list can also contain
pods of other workloads with the same labels.  Use
`ek9-resource-drill-owned' to show only the pods that ITEM owns."
  (ek9-resource--pods-with-selector
   item (ek9-resource-label-selector (ek9-resource-path item '(spec selector)))))

(defun ek9-resource-drill-owned (item)
  "Return the drill-down plist for the pods that the workload ITEM owns.
Use the label selector in spec.selector, and show only the pods
whose controller is ITEM.  This is the same rule that
\"kubectl describe\" uses for statefulsets, daemonsets, and jobs."
  (append (ek9-resource-drill-workload item)
          (list :owner-uid (ek9-resource-path item '(metadata uid))
                :owner (concat (downcase (or (alist-get 'kind item) "owner"))
                               "/" (ek9-resource-item-name item)))))

(defun ek9-resource--owned-children (item resource)
  "Return the drill-down plist for the RESOURCE items that ITEM owns.
Show only the items whose controller is ITEM."
  (list :resource resource
        :namespace (ek9-resource-item-namespace item)
        :owner-uid (ek9-resource-path item '(metadata uid))
        :owner (concat (downcase (or (alist-get 'kind item) "owner"))
                       "/" (ek9-resource-item-name item))))

(defun ek9-resource-drill-deployment (item)
  "Return the drill-down plist for the replica set list of deployment ITEM.
A deployment owns replica sets, and each replica set owns pods."
  (ek9-resource--owned-children item 'replicasets))

(defun ek9-resource-drill-cronjob (item)
  "Return the drill-down plist for the jobs of the cron job ITEM."
  (ek9-resource--owned-children item 'jobs))

(defun ek9-resource-drill-pod (item)
  "Return the drill-down plist for the containers of the pod ITEM."
  (list :resource 'containers
        :args (list (ek9-resource-item-name item))
        :namespace (ek9-resource-item-namespace item)
        :owner (concat "pod/" (ek9-resource-item-name item))))

(defun ek9-resource-events-of (item)
  "Return the drill-down plist for the events of the object ITEM.
Use the same filter as \"kubectl describe\": the name, the kind, and
the UID of ITEM.  For a node, do not use the UID: the kubelet writes
node events with the node name as the UID.  Events of an object that
is not in a namespace, for example a node, can be in any namespace."
  (let* ((name (ek9-resource-item-name item))
         (kind (alist-get 'kind item))
         (uid (unless (equal kind "Node")
                (ek9-resource-path item '(metadata uid))))
         (namespace (ek9-resource-item-namespace item)))
    (list :resource 'events
          :args (list "--field-selector"
                      (string-join
                       (delq nil (list (concat "involvedObject.name=" name)
                                       (when kind
                                         (concat "involvedObject.kind=" kind))
                                       (when uid
                                         (concat "involvedObject.uid=" uid))))
                       ","))
          :label (concat (downcase (or kind "object")) "/" name)
          :namespace (or namespace 'all))))

(defun ek9-resource-controller-uid (item)
  "Return the UID of the controller that owns ITEM, or nil."
  (alist-get 'uid (seq-find (lambda (owner) (alist-get 'controller owner))
                            (ek9-resource-path item '(metadata ownerReferences)))))

(defun ek9-resource-drill-service (item)
  "Return the drill-down plist for the pods of the service ITEM."
  (ek9-resource--pods-with-selector
   item (ek9-resource-label-selector
         `((matchLabels . ,(ek9-resource-path item '(spec selector)))))))

(defun ek9-resource-drill-node (item)
  "Return the drill-down plist for the pods on the node ITEM."
  (let ((name (ek9-resource-item-name item)))
    (list :resource 'pods
          :args (list "--field-selector" (concat "spec.nodeName=" name))
          :label (concat "node=" name)
          :namespace 'all)))

(defun ek9-resource-drill-namespace (item)
  "Return the drill-down plist for the namespace ITEM.
The plist makes ITEM the current namespace and shows its pods."
  (list :resource 'pods
        :switch-namespace (ek9-resource-item-name item)))

;;;; Resource types

(ek9-define-resource pods
  :kind "pods"
  :api '("" "v1" "pods")
  :namespaced t
  :drill-down #'ek9-resource-drill-pod
  :row-face #'ek9-resource-pod-face
  :metrics "pods"
  :actions '(describe yaml edit logs shell delete port-forward events)
  :aliases '("po"))

(ek9-define-resource containers
  :kind "pods"
  :api '("" "v1" "pods")
  :hidden t
  :columns '(("NAME" 30 (metadata name))
             ("TYPE" 9 (ek9-type))
             ("IMAGE" 40 (spec image))
             ("READY" 5 ek9-resource-container-ready)
             ("STATE" 20 ek9-resource-container-state)
             ("RESTARTS" 14 ek9-resource-container-restarts-column
              :sort-key ek9-resource-container-restarts :right-align t)
             ("PORTS" 14 ek9-resource-container-ports)
             ("AGE" 8 ek9-resource-container-age
              :sort-key ek9-resource-container-age-seconds :right-align t))
  :items #'ek9-resource-container-items
  :target #'ek9-resource-container-target
  :row-face #'ek9-resource-container-face
  :actions '(describe yaml logs shell port-forward events))

(ek9-define-resource deployments
  :kind "deployments"
  :api '("apps" "v1" "deployments")
  :aliases '("deploy" "dp")
  :drill-down #'ek9-resource-drill-deployment
  :row-face #'ek9-resource-replicas-face
  :actions '(describe yaml edit delete scale restart port-forward events))

(ek9-define-resource replicasets
  :kind "replicasets"
  :api '("apps" "v1" "replicasets")
  :aliases '("rs")
  :drill-down #'ek9-resource-drill-owned
  :row-face #'ek9-resource-replicaset-face
  :actions '(describe yaml edit delete events))

(ek9-define-resource statefulsets
  :kind "statefulsets"
  :api '("apps" "v1" "statefulsets")
  :aliases '("sts")
  :drill-down #'ek9-resource-drill-owned
  :row-face #'ek9-resource-replicas-face
  :actions '(describe yaml edit delete scale restart port-forward events))

(ek9-define-resource daemonsets
  :kind "daemonsets"
  :api '("apps" "v1" "daemonsets")
  :aliases '("ds")
  :drill-down #'ek9-resource-drill-owned
  :row-face #'ek9-resource-daemonset-face
  :actions '(describe yaml edit delete restart events))

(ek9-define-resource jobs
  :kind "jobs"
  :api '("batch" "v1" "jobs")
  :aliases '("job")
  :drill-down #'ek9-resource-drill-owned
  :row-face #'ek9-resource-job-face
  :actions '(describe yaml edit delete events))

(ek9-define-resource cronjobs
  :kind "cronjobs"
  :api '("batch" "v1" "cronjobs")
  :aliases '("cj")
  :row-face #'ek9-resource-cronjob-face
  :drill-down #'ek9-resource-drill-cronjob
  :actions '(describe yaml edit delete events))

(ek9-define-resource services
  :kind "services"
  :api '("" "v1" "services")
  :aliases '("svc")
  :drill-down #'ek9-resource-drill-service
  :actions '(describe yaml edit delete port-forward events))

(ek9-define-resource configmaps
  :kind "configmaps"
  :api '("" "v1" "configmaps")
  :aliases '("cm")
  :actions '(describe yaml edit delete events))

(ek9-define-resource secrets
  :kind "secrets"
  :api '("" "v1" "secrets")
  :aliases '("sec")
  :actions '(describe yaml edit decode delete events))

(ek9-define-resource events
  :kind "events"
  :api '("" "v1" "events")
  :aliases '("ev")
  :sort '("LAST SEEN")
  :row-face #'ek9-resource-event-face
  :actions '(describe yaml edit delete))

(ek9-define-resource nodes
  :kind "nodes"
  :api '("" "v1" "nodes")
  :namespaced nil
  :aliases '("no")
  :drill-down #'ek9-resource-drill-node
  :row-face #'ek9-resource-node-face
  :metrics "nodes"
  :actions '(describe yaml edit delete events))

(ek9-define-resource namespaces
  :kind "namespaces"
  :api '("" "v1" "namespaces")
  :namespaced nil
  :aliases '("ns")
  :drill-down #'ek9-resource-drill-namespace
  :row-face #'ek9-resource-namespace-face
  :actions '(describe yaml edit delete events))

(provide 'ek9-resource)

;;; ek9-resource.el ends here
