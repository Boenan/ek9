;;; ek9-metrics-test.el --- Tests for the ek9 metric columns -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the CPU and memory columns.  The fake proxy serves
;; the metrics fixtures pods.v1beta1.metrics.k8s.io.json,
;; nodes.v1beta1.metrics.k8s.io.json, and
;; pods-web-1.v1beta1.metrics.k8s.io.json.

;;; Code:

(require 'ert)
(require 'ek9-list)
(require 'ek9-metrics)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defmacro ek9-test--with-list (name top &rest body)
  "Open the list of the resource type NAME, and run BODY in it.
If TOP is non-nil, set the top mode on.  BODY starts after the rows
arrived, and in top mode also the metrics.  The timer does not tick
in BODY."
  (declare (indent 2))
  `(ek9-test--with-clean-context
     (when (timerp ek9-list--timer)
       (cancel-timer ek9-list--timer)
       (setq ek9-list--timer nil))
     (let* ((ek9-refresh-interval 3600)
            (ek9-list-wide t)
            (buffer (ek9-list-open ,name)))
       (unwind-protect
           (with-current-buffer buffer
             (ek9-test--wait-until (lambda () ek9-list--loaded) 10)
             (when ,top
               (ek9-list-toggle-top)
               (ek9-test--wait-until (lambda () ek9-list--metrics) 10))
             ,@body)
         (kill-buffer buffer)
         (when (timerp ek9-list--timer)
           (cancel-timer ek9-list--timer)
           (setq ek9-list--timer nil))))))

(defmacro ek9-test--with-metrics-list (name &rest body)
  "Open the list of NAME in top mode, and run BODY in it.
See `ek9-test--with-list'."
  (declare (indent 1))
  `(ek9-test--with-list ,name t ,@body))

(defmacro ek9-test--with-plain-list (name &rest body)
  "Open the list of NAME without top mode, and run BODY in it.
See `ek9-test--with-list'."
  (declare (indent 1))
  `(ek9-test--with-list ,name nil ,@body))

(defun ek9-test--cells (id)
  "Return an alist of (TITLE . TEXT) for the row ID of the current buffer."
  (let ((entry (assoc id tabulated-list-entries)))
    (cl-mapcar (lambda (column value)
                 (cons (car column) (substring-no-properties value)))
               tabulated-list-format (cadr entry))))

(ert-deftest ek9-metrics-parse-quantity-forms ()
  (dolist (case '(("0" . 0) ("2" . 2) ("250m" . 0.25) ("1500u" . 0.0015)
                  ("655225767n" . 0.655225767) ("1.5" . 1.5)
                  ("1Ki" . 1024) ("17Mi" . 17825792) ("1Gi" . 1073741824)
                  ("1k" . 1000.0) ("1M" . 1e6) ("1e3" . 1000.0) ("12E" . 1.2e19)
                  ("100e-3" . 0.1) ("1.5Gi" . 1610612736) (".5" . 0.5)))
    (should (= (ek9-metrics-parse-quantity (car case)) (cdr case))))
  (should-not (ek9-metrics-parse-quantity "1Xi"))
  (should-not (ek9-metrics-parse-quantity ""))
  (should-not (ek9-metrics-parse-quantity nil)))

(ert-deftest ek9-metrics-texts-as-kubectl ()
  ;; kubectl rounds millicores up and MiB down.
  (should (equal (ek9-metrics--text 'cpu 0.655225767) "656m"))
  (should (equal (ek9-metrics--text 'cpu 0.1) "100m"))
  (should (equal (ek9-metrics--text 'cpu 0) "0m"))
  ;; One nanocore more than 1m rounds up.
  (should (equal (ek9-metrics--text 'cpu (ek9-metrics-parse-quantity "1000001n")) "2m"))
  (should (equal (ek9-metrics--text 'cpu (ek9-metrics-parse-quantity "1000000n")) "1m"))
  (should (equal (ek9-metrics--text 'memory (* 826220 1024.0)) "806Mi"))
  (should (equal (ek9-metrics--text 'memory 0) "0Mi")))

(defun ek9-test--metric-texts (type metrics item)
  "Return an alist of (TITLE . TEXT) for the metrics TYPE of ITEM.
METRICS is the metrics object of ITEM."
  (mapcar (lambda (column)
            (cons (car column) (ek9-resource-column-string column item)))
          (ek9-metrics-columns type (lambda (_item) metrics))))

(ert-deftest ek9-metrics-pod-limits-and-sidecars ()
  (let* ((pod '((metadata (name . "p") (namespace . "a"))
                (spec (containers ((name . "main")
                                   (resources (requests (cpu . "100m") (memory . "100Mi"))
                                              (limits (cpu . "200m")))))
                      (initContainers ((name . "init")
                                       (resources (requests (cpu . "500m"))))
                                      ((name . "sidecar")
                                       (restartPolicy . "Always")
                                       (resources (requests (cpu . "100m"))))))))
         (metrics '((metadata (name . "p") (namespace . "a"))
                    (containers ((name . "main") (usage (cpu . "50m") (memory . "50Mi")))
                                ((name . "sidecar") (usage (cpu . "50m") (memory . "0"))))))
         (texts (ek9-test--metric-texts "pods" metrics pod)))
    ;; The sidecar counts, the normal init container does not.
    (should (equal (cdr (assoc "CPU" texts)) "100m"))
    (should (equal (cdr (assoc "CPU/R" texts)) "50%"))
    (should (equal (cdr (assoc "MEM/R" texts)) "50%"))
    ;; The sidecar has no CPU limit, so the pod has no CPU limit.
    (should (equal (cdr (assoc "CPU/L" texts)) "n/a"))
    (should (equal (cdr (assoc "MEM/L" texts)) "n/a"))))

(ert-deftest ek9-metrics-without-usage ()
  ;; A metrics object without containers must not break the render.
  (let ((texts (ek9-test--metric-texts
                "pods" '((metadata (name . "p")) (containers))
                '((metadata (name . "p"))
                  (spec (containers ((resources (requests (cpu . "1"))))))))))
    (should (equal (cdr (assoc "CPU" texts)) ""))
    (should (equal (cdr (assoc "CPU/R" texts)) "n/a")))
  (let ((texts (ek9-test--metric-texts
                "nodes" '((metadata (name . "n")))
                '((metadata (name . "n")) (status (allocatable (cpu . "4")))))))
    (should (equal (cdr (assoc "CPU/A" texts)) "n/a"))))

(ert-deftest ek9-metrics-path-keeps-label-selector ()
  (should (equal (ek9-metrics-path "pods" "team-a"
                                   '(("labelSelector" . "app=web")
                                     ("fieldSelector" . "spec.nodeName=n1")
                                     ("includeObject" . "Object")))
                 "/apis/metrics.k8s.io/v1beta1/namespaces/team-a/pods?labelSelector=app%3Dweb"))
  (should (equal (ek9-metrics-path "nodes" nil nil)
                 "/apis/metrics.k8s.io/v1beta1/nodes")))

(ert-deftest ek9-metrics-normal-list-has-no-metrics ()
  ;; Only the top mode shows and reads the metrics.
  (ek9-test--with-plain-list 'pods
    (should (equal (mapcar #'car tabulated-list-format)
                   '("NAME" "READY" "STATUS" "RESTARTS" "AGE" "IP" "NODE")))
    (should-not ek9-list--metrics-request)
    (ek9-list--tick)
    (should-not (ek9-proxy-live-p ek9-list--metrics-request))))

(ert-deftest ek9-metrics-pod-columns ()
  (ek9-test--with-metrics-list 'pods
    (should (equal (mapcar #'car tabulated-list-format)
                   '("NAME" "CPU" "CPU/R" "CPU/L" "MEM" "MEM/R" "MEM/L")))
    (let ((nginx (ek9-test--cells "default/nginx-7c5b8d65b8-abcde"))
          (api (ek9-test--cells "default/api-5f9c-xyz"))
          (db (ek9-test--cells "default/db-0")))
      (should (equal (mapcar (lambda (title) (cdr (assoc title nginx)))
                             '("CPU" "MEM" "CPU/R" "MEM/R" "CPU/L" "MEM/L"))
                     '("25m" "32Mi" "25%" "50%" "2%" "12%")))
      ;; Two containers: 1.5m + 2m is 4m, rounded up.  No requests.
      (should (equal (mapcar (lambda (title) (cdr (assoc title api)))
                             '("CPU" "MEM" "CPU/R" "MEM/L"))
                     '("4m" "10Mi" "n/a" "n/a")))
      ;; A pod without metrics has empty metric cells.
      (should (equal (cdr (assoc "CPU" db)) ""))
      (should (equal (cdr (assoc "CPU/R" db)) "")))
    ;; The top mode shows all metric columns, also without the wide
    ;; columns.
    (ek9-list-toggle-wide)
    (should (equal (mapcar #'car tabulated-list-format)
                   '("NAME" "CPU" "CPU/R" "CPU/L" "MEM" "MEM/R" "MEM/L")))))

(ert-deftest ek9-metrics-sort-by-cpu ()
  (ek9-test--with-metrics-list 'pods
    ;; The largest CPU use is first.
    (should (equal tabulated-list-sort-key '("CPU" . t)))
    (should (equal (seq-take (mapcar #'car tabulated-list-entries) 2)
                   '("default/nginx-7c5b8d65b8-abcde" "default/api-5f9c-xyz")))))

(ert-deftest ek9-metrics-sort-stays-after-reset ()
  (ek9-test--with-metrics-list 'pods
    (ek9-list--reset)
    ;; The metric columns stay, with empty cells.
    (should-not ek9-list--metrics)
    (should (assoc "CPU" (append tabulated-list-format nil)))
    (should (equal tabulated-list-sort-key '("CPU" . t)))
    (ek9-list-refresh)
    (ek9-test--wait-until (lambda () ek9-list--metrics) 10)
    (should (equal (car (mapcar #'car tabulated-list-entries))
                   "default/nginx-7c5b8d65b8-abcde"))))

(ert-deftest ek9-metrics-all-namespaces ()
  (ek9-test--with-metrics-list 'pods
    (setq ek9-context--namespace 'all)
    (ek9-list--reset)
    (ek9-list-refresh)
    (ek9-test--wait-until (lambda () ek9-list--metrics) 10)
    (should (equal (seq-take (mapcar #'car tabulated-list-format) 3)
                   '("NAMESPACE" "NAME" "CPU")))
    (should (equal (cdr (assoc "CPU" (ek9-test--cells "default/nginx-7c5b8d65b8-abcde")))
                   "25m"))))

(ert-deftest ek9-metrics-node-columns ()
  (ek9-test--with-metrics-list 'nodes
    (should (equal (mapcar #'car tabulated-list-format)
                   '("NAME" "CPU" "CPU/A" "MEM" "MEM/A")))
    ;; The same values as "kubectl top nodes".  The node has 4 cores and
    ;; 8Gi.
    (let ((node (ek9-test--cells "kind-control-plane")))
      (should (equal (mapcar (lambda (title) (cdr (assoc title node)))
                             '("CPU" "CPU/A" "MEM" "MEM/A"))
                     '("656m" "16%" "806Mi" "9%"))))))

(ert-deftest ek9-metrics-no-metrics-api ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (setq ek9-context--current "no-metrics")
    (let ((buffer (ek9-list-open 'pods)))
      (unwind-protect
          (with-current-buffer buffer
            (ek9-test--wait-until (lambda () ek9-list--loaded) 10)
            (ek9-list-toggle-top)
            (should ek9-list--top)
            ;; The 404 sets the top mode off.
            (ek9-test--wait-until (lambda () (not ek9-list--top)) 10)
            (should-not ek9-list--metrics)
            (should-not (assoc "CPU" (append tabulated-list-format nil)))
            ;; A cluster without metrics-server is not an error.
            (should-not ek9-list--error)
            (ek9-list--tick)
            (should-not (ek9-proxy-live-p ek9-list--metrics-request)))
        (kill-buffer buffer)
        (when (timerp ek9-list--timer)
          (cancel-timer ek9-list--timer)
          (setq ek9-list--timer nil))))))

(ert-deftest ek9-metrics-read-after-interval ()
  (ek9-test--with-metrics-list 'pods
    (let ((ek9-metrics-interval 15))
      (ek9-list--tick)
      (should-not (ek9-proxy-live-p ek9-list--metrics-request))
      (setq ek9-list--metrics-time (- (float-time) 16))
      (ek9-list--tick)
      (should (ek9-proxy-live-p ek9-list--metrics-request)))))

(ert-deftest ek9-metrics-option-off ()
  (let ((ek9-metrics nil))
    (ek9-test--with-plain-list 'pods
      (should-not ek9-list--metrics-request)
      (should-error (ek9-list-toggle-top) :type 'user-error)
      (should-not ek9-list--top)
      (should-not (assoc "CPU" (append tabulated-list-format nil))))))

(ert-deftest ek9-pods-omit-columns ()
  ;; The server table has the wide columns NOMINATED NODE and
  ;; READINESS GATES.  The pod list does not show them.
  (ek9-test--with-plain-list 'pods
    (let ((titles (mapcar #'car tabulated-list-format)))
      (should (member "NODE" titles))
      (should-not (member "NOMINATED NODE" titles))
      (should-not (member "READINESS GATES" titles)))))

(ert-deftest ek9-nodes-omit-columns ()
  (ek9-test--with-plain-list 'nodes
    (let ((titles (mapcar #'car tabulated-list-format)))
      (should (member "OS-IMAGE" titles))
      (should-not (member "KERNEL-VERSION" titles))
      (should-not (member "CONTAINER-RUNTIME" titles)))))

;;;; Containers

(defmacro ek9-test--with-container-metrics (&rest body)
  "Open the container list of the pod web-1 in top mode, and run BODY.
BODY starts after the rows and the metrics arrived."
  (declare (indent 0))
  `(ek9-test--with-clean-context
     (ek9-test--load-contexts)
     (let ((ek9-refresh-interval 3600)
           (ek9-list-wide t))
       (unwind-protect
           (with-current-buffer
               (ek9-list-open 'containers
                              (ek9-resource-drill-pod
                               (ek9-test--read-fixture "pods-web-1.json")))
             (ek9-test--wait-until (lambda () ek9-list--loaded) 10)
             (should-not (assoc "CPU" (append tabulated-list-format nil)))
             (ek9-list-toggle-top)
             (ek9-test--wait-until (lambda () ek9-list--metrics) 10)
             ,@body)
         (mapc #'kill-buffer (ek9-list--buffers))
         (when (timerp ek9-list--timer)
           (cancel-timer ek9-list--timer)
           (setq ek9-list--timer nil))))))

(ert-deftest ek9-metrics-path-of-containers ()
  (should (equal (ek9-metrics-path "containers" "team-a" nil "web-1")
                 "/apis/metrics.k8s.io/v1beta1/namespaces/team-a/pods/web-1")))

(ert-deftest ek9-metrics-container-columns ()
  (ek9-test--with-container-metrics
    (should (equal (mapcar #'car tabulated-list-format)
                   '("NAME" "CPU" "CPU/R" "CPU/L" "MEM" "MEM/R" "MEM/L")))
    (let ((app (ek9-test--cells "team-a/app"))
          (proxy (ek9-test--cells "team-a/proxy"))
          (worker (ek9-test--cells "team-a/worker")))
      ;; The requests are 100m and 64Mi.  Only memory has a limit.
      (should (equal (mapcar (lambda (title) (cdr (assoc title app)))
                             '("CPU" "MEM" "CPU/R" "MEM/R" "CPU/L" "MEM/L"))
                     '("50m" "32Mi" "50%" "50%" "n/a" "25%")))
      (should (equal (mapcar (lambda (title) (cdr (assoc title proxy)))
                             '("CPU" "MEM" "CPU/R"))
                     '("2m" "10Mi" "n/a")))
      ;; A container that does not run has no metrics.
      (should (equal (cdr (assoc "CPU" worker)) "")))))

(ert-deftest ek9-metrics-container-404-keeps-top ()
  (ek9-test--with-container-metrics
    (ek9-list--metrics-error 404)
    ;; The pod can have metrics at the next read.
    (should-not ek9-list--metrics)
    (should ek9-list--top)))

;;;; Top mode

(ert-deftest ek9-metrics-toggle-top ()
  (ek9-test--with-plain-list 'pods
    (let ((columns (mapcar #'car tabulated-list-format))
          (sort tabulated-list-sort-key))
      (ek9-list-toggle-top)
      (should (equal (mapcar #'car tabulated-list-format)
                     '("NAME" "CPU" "CPU/R" "CPU/L" "MEM" "MEM/R" "MEM/L")))
      (should (equal tabulated-list-sort-key '("CPU" . t)))
      (should (ek9-proxy-live-p ek9-list--metrics-request))
      (should (string-match-p "View: pods \\[[0-9]+\\]  Top"
                              (substring-no-properties (ek9-list--tab-line))))
      (ek9-test--wait-until (lambda () ek9-list--metrics) 10)
      ;; The columns and the sort come back, and the reads stop.
      (ek9-list-toggle-top)
      (should (equal (mapcar #'car tabulated-list-format) columns))
      (should (equal tabulated-list-sort-key sort))
      (should-not ek9-list--metrics)
      (should-not ek9-list--metrics-request)
      (should-not (string-match-p "Top" (substring-no-properties
                                         (ek9-list--tab-line)))))))

(ert-deftest ek9-metrics-top-needs-metrics-type ()
  (with-temp-buffer
    (ek9-list-mode)
    (setq ek9-list--resource (ek9-resource-get 'deployments))
    (should-error (ek9-list-toggle-top) :type 'user-error)
    (should-not ek9-list--top)))

(ert-deftest ek9-metrics-top-off-without-metrics-api ()
  (ek9-test--with-metrics-list 'pods
    (ek9-list--metrics-error 503)
    (should ek9-list--top)
    (should ek9-list--metrics)
    (ek9-list--metrics-error 404)
    (should-not ek9-list--top)
    (should-not ek9-list--metrics)
    (should (equal tabulated-list-sort-key (ek9-resource-sort ek9-list--resource)))
    (should (member "STATUS" (mapcar #'car tabulated-list-format)))))

(ert-deftest ek9-metrics-top-goes-to-containers ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (unwind-protect
        (let ((parent (generate-new-buffer " *ek9 test parent*"))
              (spec (ek9-resource-drill-pod
                     (ek9-test--read-fixture "pods-web-1.json"))))
          (with-current-buffer parent
            (ek9-list-mode)
            (setq ek9-list--resource (ek9-resource-get 'pods)
                  ek9-list--top t))
          (with-current-buffer (ek9-list-open 'containers spec parent)
            (should ek9-list--top)
            (should (equal tabulated-list-sort-key '("CPU" . t)))
            (ek9-test--wait-until (lambda () ek9-list--metrics) 10)
            (should (equal (seq-take (mapcar #'car tabulated-list-format) 3)
                           '("NAME" "CPU" "CPU/R")))
            (should (equal (car (mapcar #'car tabulated-list-entries))
                           "team-a/app"))))
      (mapc #'kill-buffer (ek9-list--buffers))
      (when (timerp ek9-list--timer)
        (cancel-timer ek9-list--timer)
        (setq ek9-list--timer nil)))))

(provide 'ek9-metrics-test)

;;; ek9-metrics-test.el ends here
