;;; ek9-metrics.el --- CPU and memory columns for ek9 -*- lexical-binding: t; -*-

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

;; The top mode of the lists of pods, containers, and nodes shows the
;; CPU and memory use, as "kubectl top" does.  The server table does
;; not have these values.  ek9 reads them from the metrics API
;; (metrics.k8s.io) through the proxy, and shows them in columns.  A
;; resource type declares the metrics type with :metrics in
;; `ek9-define-resource'.
;;
;; The metrics API is not a built-in API.  A cluster has it only with
;; metrics-server or a similar service.  Without it, the top mode
;; goes off.
;;
;; The texts are the same as in "kubectl top": CPU in millicores, for
;; example "250m", and memory in MiB, for example "64Mi".  The columns
;; with "/R", "/L", and "/A" show the use as a percentage of the
;; requests, the limits, and the allocatable resources of a node.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ek9-resource)
(require 'ek9-proxy)

(defcustom ek9-metrics t
  "Non-nil means that lists have a top mode with CPU and memory columns.
The values come from the metrics API, for example from
metrics-server.  A resource type has a top mode if it has :metrics,
see `ek9-resource' and `ek9-list-toggle-top'."
  :type 'boolean
  :group 'ek9)

(defcustom ek9-metrics-interval 15
  "Seconds between two reads of the metrics of a visible list in top mode.
metrics-server gets new values about every 15 seconds."
  :type 'number
  :group 'ek9)

(defconst ek9-metrics-api '("metrics.k8s.io" "v1beta1")
  "Group and version of the metrics API.")

;;;; Quantities

(defconst ek9-metrics--suffixes
  '(("n" . 1e-9) ("u" . 1e-6) ("m" . 1e-3) ("" . 1)
    ("k" . 1e3) ("M" . 1e6) ("G" . 1e9) ("T" . 1e12) ("P" . 1e15) ("E" . 1e18)
    ("Ki" . 1024) ("Mi" . 1048576) ("Gi" . 1073741824)
    ("Ti" . 1099511627776) ("Pi" . 1125899906842624)
    ("Ei" . 1152921504606846976))
  "Factors of the suffixes of a Kubernetes quantity.")

(defun ek9-metrics-parse-quantity (text)
  "Return the Kubernetes quantity TEXT as a number, or nil.
For example, \"250m\" gives 0.25 and \"1Ki\" gives 1024.  The unit is
cores for CPU and bytes for memory."
  (when (and (stringp text)
             (string-match "\\`\\([+-]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\)\\([eE][+-]?[0-9]+\\|[a-zA-Z]*\\)\\'"
                           text))
    (let ((number (match-string 1 text))
          (suffix (match-string 2 text)))
      (if (string-match-p "\\`[eE][+-]?[0-9]+\\'" suffix)
          (string-to-number (concat number suffix))
        (when-let* ((factor (cdr (assoc suffix ek9-metrics--suffixes))))
          (* (string-to-number number) factor))))))

(defun ek9-metrics--milli (cores)
  "Return CORES in millicores, rounded up, as kubectl does.
Return nil if CORES is nil."
  ;; Round to whole nanocores first, so that a float error, for
  ;; example in 0.1 * 1000, does not round up.
  (when cores
    (ceiling (round (* cores 1e9)) 1000000)))

(defun ek9-metrics--mebi (bytes)
  "Return BYTES in whole MiB, as kubectl does."
  (floor (ceiling (- bytes 1e-6)) 1048576))

;;;; Values

(defun ek9-metrics--sum (resources key)
  "Return the sum of the quantity KEY in the list RESOURCES, or nil.
Each element of RESOURCES is an alist, for example the usage of a
container.  Return nil if no element has KEY."
  (let ((values (delq nil (mapcar (lambda (resource)
                                    (ek9-metrics-parse-quantity
                                     (alist-get key resource)))
                                  resources))))
    (and values (apply #'+ values))))

(defun ek9-metrics--usage (metrics key)
  "Return the use of KEY (cpu or memory) in METRICS, or nil.
METRICS is a PodMetrics or a NodeMetrics object."
  (if-let* ((containers (alist-get 'containers metrics)))
      (ek9-metrics--sum (mapcar (lambda (container) (alist-get 'usage container))
                                containers)
                        key)
    (ek9-metrics-parse-quantity (alist-get key (alist-get 'usage metrics)))))

(defun ek9-metrics--pod-containers (pod)
  "Return the containers of POD that metrics-server measures.
These are the containers and the sidecar containers, that is, the
init containers with the restart policy Always."
  (append (ek9-resource-path pod '(spec containers))
          (seq-filter (lambda (container)
                        (equal (alist-get 'restartPolicy container) "Always"))
                      (ek9-resource-path pod '(spec initContainers)))))

(defun ek9-metrics--pod-resource (pod kind key)
  "Return the sum of the resource KEY of KIND in the containers of POD.
KIND is `requests' or `limits', and KEY is `cpu' or `memory'.  Return
nil if no container has a value.  For limits, also return nil if one
container has no limit: then the pod has no limit."
  (let ((values (mapcar (lambda (container)
                          (ek9-metrics-parse-quantity
                           (alist-get key (alist-get kind (alist-get 'resources container)))))
                        (ek9-metrics--pod-containers pod))))
    (unless (or (null (delq nil (copy-sequence values)))
                (and (eq kind 'limits) (memq nil values)))
      (apply #'+ (delq nil values)))))

(defun ek9-metrics--container-resource (container kind key)
  "Return the resource KEY of KIND of the item CONTAINER, or nil.
CONTAINER is an item of `ek9-resource-container-items'.  KIND and KEY
are as in `ek9-metrics--pod-resource'."
  (ek9-metrics-parse-quantity
   (alist-get key (ek9-resource-path container (list 'spec 'resources kind)))))

(defun ek9-metrics--node-resource (node key)
  "Return the allocatable resource KEY of NODE, or nil."
  (ek9-metrics-parse-quantity
   (alist-get key (ek9-resource-path node '(status allocatable)))))

(defun ek9-metrics--amount (key value)
  "Return VALUE of KEY as kubectl compares it, or nil if VALUE is nil.
KEY is `cpu' or `memory'.  CPU is in millicores, memory in bytes."
  (if (eq key 'cpu) (ek9-metrics--milli value) value))

(defun ek9-metrics--percent (usage total)
  "Return USAGE as a percentage of TOTAL, as an integer, or nil."
  (when (and usage total (> total 0))
    (floor (* usage 100) total)))

(defun ek9-metrics--text (key value)
  "Return the text for the VALUE of KEY (cpu or memory), or nil."
  (when value
    (if (eq key 'cpu)
        (format "%dm" (ek9-metrics--milli value))
      (format "%dMi" (ek9-metrics--mebi value)))))

(defun ek9-metrics--percent-text (percent)
  "Return the text for PERCENT, or \"n/a\" if PERCENT is nil."
  (if percent (format "%d%%" percent) "n/a"))

;;;; Columns

(defun ek9-metrics--usage-column (title key)
  "Return the column TITLE for the use of KEY, see `ek9-metrics-columns'."
  (list title 6
        (lambda (metrics _item) (ek9-metrics--text key (ek9-metrics--usage metrics key)))
        (lambda (metrics _item) (ek9-metrics--usage metrics key))))

(defun ek9-metrics--resource-column (title key kind resource)
  "Return the column TITLE for the use of KEY as a percentage of KIND.
KIND is `requests' or `limits'.  RESOURCE is a function that takes
the item, KIND, and KEY, and returns the request or the limit, for
example `ek9-metrics--pod-resource'."
  (let ((percent (lambda (metrics item)
                   (ek9-metrics--percent
                    (ek9-metrics--amount key (ek9-metrics--usage metrics key))
                    (ek9-metrics--amount key (funcall resource item kind key))))))
    (list title 5
          (lambda (metrics item)
            (ek9-metrics--percent-text (funcall percent metrics item)))
          percent)))

(defun ek9-metrics--node-column (title key)
  "Return the column TITLE for the use of KEY as a percentage of allocatable."
  (let ((percent (lambda (metrics item)
                   (ek9-metrics--percent
                    (ek9-metrics--amount key (ek9-metrics--usage metrics key))
                    (ek9-metrics--amount key (ek9-metrics--node-resource item key))))))
    (list title 5
          (lambda (metrics item)
            (ek9-metrics--percent-text (funcall percent metrics item)))
          percent)))

(defun ek9-metrics--resource-columns (resource)
  "Return the column definitions for a pod or a container.
RESOURCE is the function for `ek9-metrics--resource-column'."
  (list (ek9-metrics--usage-column "CPU" 'cpu)
        (ek9-metrics--resource-column "CPU/R" 'cpu 'requests resource)
        (ek9-metrics--resource-column "CPU/L" 'cpu 'limits resource)
        (ek9-metrics--usage-column "MEM" 'memory)
        (ek9-metrics--resource-column "MEM/R" 'memory 'requests resource)
        (ek9-metrics--resource-column "MEM/L" 'memory 'limits resource)))

(defun ek9-metrics--definitions (type)
  "Return the column definitions for the metrics TYPE.
TYPE is \"pods\", \"containers\", or \"nodes\".  Each definition
is (TITLE WIDTH TEXT SORT).  TEXT and SORT are functions that take
the metrics object and the item.  TEXT returns the cell text, and
SORT a number or nil."
  (pcase type
    ("pods" (ek9-metrics--resource-columns #'ek9-metrics--pod-resource))
    ("containers"
     (ek9-metrics--resource-columns #'ek9-metrics--container-resource))
    ("nodes"
     (list (ek9-metrics--usage-column "CPU" 'cpu)
           (ek9-metrics--node-column "CPU/A" 'cpu)
           (ek9-metrics--usage-column "MEM" 'memory)
           (ek9-metrics--node-column "MEM/A" 'memory)))))

(defun ek9-metrics-columns (type lookup)
  "Return the ek9 columns for the metrics TYPE.
TYPE is the value of :metrics of the resource type.  LOOKUP is a
function that takes an item and returns its metrics object, or nil.
A column of an item without metrics is empty.  See `ek9-resource' for
the form of a column."
  (mapcar
   (pcase-lambda (`(,title ,width ,text ,sort))
     (list title width
           (lambda (item)
             (when-let* ((metrics (funcall lookup item)))
               (funcall text metrics item)))
           :right-align t
           :sort-key (lambda (item)
                       (when-let* ((metrics (funcall lookup item)))
                         (funcall sort metrics item)))))
   (ek9-metrics--definitions type)))

(defun ek9-metrics-path (type namespace query &optional name)
  "Return the API path for the metrics TYPE in NAMESPACE.
NAMESPACE is a namespace name, or nil for all namespaces or for
nodes.  QUERY is the query alist of the list, see `ek9-proxy-path'.
The metrics API accepts only the label selector of QUERY.  NAME is an
object name, or nil for a list.  For the type \"containers\", NAME
is the name of the pod, and the path gives the metrics of that pod."
  (ek9-proxy-path (append ek9-metrics-api
                          (list (if (equal type "containers") "pods" type)))
                  namespace name
                  (seq-filter (lambda (entry) (equal (car entry) "labelSelector"))
                              query)))

(defun ek9-metrics-index (type json)
  "Return a hash table that maps an item ID to its metrics object.
JSON is the reply of the metrics API for the metrics TYPE.  For the
type \"containers\", JSON is one PodMetrics object, and the hash
table has the metrics of each container.  For the other types, JSON
is a list."
  (let ((index (make-hash-table :test #'equal)))
    (if (equal type "containers")
        ;; The same ID as an item of `ek9-resource-container-items'.
        (let ((namespace (ek9-resource-item-namespace json)))
          (dolist (container (alist-get 'containers json))
            (puthash (concat namespace "/" (alist-get 'name container))
                     container index)))
      (dolist (metrics (alist-get 'items json))
        (puthash (ek9-resource-item-id metrics) metrics index)))
    index))

(provide 'ek9-metrics)

;;; ek9-metrics.el ends here
