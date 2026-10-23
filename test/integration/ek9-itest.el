;;; ek9-itest.el --- Integration tests for ek9 with kind -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration tests against the local kind cluster "ek9-itest".  Run
;; them with "eask run script integration".  The script makes the
;; cluster, applies manifests.yaml, crds.yaml, and custom-resources.yaml,
;; and sets KUBECONFIG to the file test/integration/kubeconfig.
;;
;; Each test first calls `ek9-itest--check-safe'.  It stops the test
;; if KUBECONFIG is not that file, or if the API server is not on
;; 127.0.0.1.  Write actions use only the namespace ek9-itest.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
;; A package-vc rebuild can leave old .elc files.  Load the newer file.
(setq load-prefer-newer t)
(require 'ek9)

(defconst ek9-itest--dir (file-name-directory (macroexp-file-name))
  "Directory of the integration tests.")

(defconst ek9-itest--kubeconfig (expand-file-name "kubeconfig" ek9-itest--dir)
  "The kubeconfig file of the kind cluster.")

(defconst ek9-itest--context "kind-ek9-itest"
  "Context of the kind cluster.")

(defconst ek9-itest--namespace "ek9-itest"
  "Namespace of the test objects.")

;;;; Helpers

(defun ek9-itest--kubectl (&rest args)
  "Run kubectl with ARGS in the kind context and return its output.
Signal an error if kubectl fails.  Only the tests use this
synchronous call."
  (with-temp-buffer
    ;; Keep stderr out of the output: a warning line must not become
    ;; part of a table or a value.
    (let ((status (apply #'call-process "kubectl" nil '(t nil) nil
                         "--context" ek9-itest--context args)))
      (unless (eql status 0)
        (error "kubectl %s failed: %s" (string-join args " ") (buffer-string)))
      (buffer-string))))

(defun ek9-itest--check-safe ()
  "Signal an error unless the tests use the kind cluster only."
  (unless (equal (getenv "KUBECONFIG") ek9-itest--kubeconfig)
    (error "KUBECONFIG must be %s.  Use \"eask run script integration\""
           ek9-itest--kubeconfig))
  (let ((server (ek9-itest--kubectl
                 "config" "view" "--minify"
                 "-o" "jsonpath={.clusters[0].cluster.server}")))
    (unless (string-match-p "\\`https://127\\.0\\.0\\.1:" server)
      (error "The API server %s is not a local kind cluster" server)))
  ;; A tunnel to a real cluster can also use 127.0.0.1.  The kind
  ;; cluster has a node with a known name.
  (unless (member "node/ek9-itest-control-plane"
                  (split-string (ek9-itest--kubectl "get" "nodes" "-o" "name")
                                "\n" t))
    (error "The cluster has no node ek9-itest-control-plane")))

(defun ek9-itest--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, for TIMEOUT seconds at most.
TIMEOUT is 60 seconds by default.  Return the value of PREDICATE."
  (let ((end (+ (float-time) (or timeout 60)))
        value)
    (while (not (setq value (funcall predicate)))
      (when (> (float-time) end)
        (error "Timeout while waiting in an integration test"))
      (accept-process-output nil 0.1))
    value))

(defmacro ek9-itest--with-cluster (&rest body)
  "Run BODY with ek9 in the kind context and the test namespace.
Kill all ek9 buffers and stop all port-forwards after BODY."
  (declare (indent 0))
  `(progn
     (ek9-itest--check-safe)
     (let ((ek9-kubectl-executable "kubectl")
           (ek9-context--current ek9-itest--context)
           (ek9-context--namespace ek9-itest--namespace)
           (ek9-context--contexts nil)
           (ek9-context--clusters nil)
           (ek9-context--namespace-cache (make-hash-table :test #'equal))
           (ek9-context--last-namespace (make-hash-table :test #'equal))
           (ek9-types--cache (make-hash-table :test #'equal))
           (ek9-types--callbacks (make-hash-table :test #'equal))
           (ek9-types--jobs (make-hash-table :test #'equal))
           (ek9-context-changed-hook (list #'ek9-list--context-changed))
           (ek9-read-only-contexts nil)
           (ek9-saved-views nil)
           (ek9-portfwd--forwards nil))
       (let (done)
         (ek9-context-load (lambda () (setq done t)))
         (ek9-itest--wait-until (lambda () done)))
       (unwind-protect
           (progn ,@body)
         (dolist (forward ek9-portfwd--forwards)
           (when (ek9-portfwd--running-p forward)
             (delete-process (ek9-portfwd--forward-process forward))))
         (dolist (buffer (buffer-list))
           (when (ek9--buffer-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buffer))))
         (when (timerp ek9-list--timer)
           (cancel-timer ek9-list--timer)
           (setq ek9-list--timer nil))))))

(defun ek9-itest--wait-loaded (buffer)
  "Wait until the list BUFFER has its rows, and return BUFFER."
  (with-current-buffer buffer
    (ek9-itest--wait-until (lambda () (or ek9-list--loaded ek9-list--error)))
    (when ek9-list--error
      (error "The list %s has an error: %s" (buffer-name) ek9-list--error)))
  buffer)

(defun ek9-itest--open (name &optional spec)
  "Open the list of the resource type NAME with SPEC and wait for its rows."
  (ek9-itest--wait-loaded (ek9-list-open name spec)))

(defun ek9-itest--refresh (buffer)
  "Get the rows of the list BUFFER again and wait for them."
  (with-current-buffer buffer
    (ek9-itest--wait-until (lambda () (not (ek9-proxy-live-p ek9-list--process))))
    (ek9-list-refresh)
    (ek9-itest--wait-until (lambda () (not (ek9-proxy-live-p ek9-list--process))))
    (accept-process-output nil 0.1)))

;;;; kubectl tables

(defun ek9-itest--title-starts (header)
  "Return (TITLE . START) for each column title in the table HEADER.
A title can contain one space, for example \"LAST SEEN\"."
  (let ((position 0) starts)
    (while (string-match "[^ ]+\\(?: [^ ]+\\)*" header position)
      (push (cons (match-string 0 header) (match-beginning 0)) starts)
      (setq position (match-end 0)))
    (nreverse starts)))

(defun ek9-itest--table (&rest args)
  "Run \"kubectl get\" with ARGS and return its table.
Return a list of rows.  Each row is an alist of (TITLE . VALUE)."
  (ek9-itest--parse-table (apply #'ek9-itest--kubectl "get" args)))

(defun ek9-itest--parse-table (output)
  "Return the table in the kubectl OUTPUT as a list of rows.
Each row is an alist of (TITLE . VALUE)."
  (let* ((lines (split-string output "\n" t))
         (starts (ek9-itest--title-starts (car lines))))
    (mapcar (lambda (line)
              (cl-loop for ((title . start) . rest) on starts
                       for end = (if rest (cdar rest) (length line))
                       collect (cons title
                                     (string-trim
                                      (substring line (min start (length line))
                                                 (min end (length line)))))))
            (cdr lines))))

(defun ek9-itest--ek9-rows (buffer)
  "Return the rows of the list BUFFER as an alist of (ID . ROW).
Each ROW is an alist of (TITLE . VALUE)."
  (with-current-buffer buffer
    (let ((titles (mapcar #'car tabulated-list-format)))
      (mapcar (lambda (entry)
                (cons (car entry)
                      (cl-mapcar #'cons titles (append (cadr entry) nil))))
              tabulated-list-entries))))

(defconst ek9-itest--time-columns
  '("AGE" "DURATION" "LAST SCHEDULE" "LAST SEEN" "STARTED")
  "Columns that show a duration.")

(defun ek9-itest--normalize (title value)
  "Return VALUE of the column TITLE in a form that does not change with time.
In a duration, each number becomes N, so that \"7h18m\" equals
\"7h19m\", but not \"7h\".  In RESTARTS, the same is done for the time of
the last restart."
  (cond ((member title ek9-itest--time-columns)
         (replace-regexp-in-string "[0-9]+" "N" value))
        ((equal title "RESTARTS")
         (replace-regexp-in-string "(\\([0-9a-z]+\\) ago)"
                                   (lambda (match)
                                     (replace-regexp-in-string "[0-9]+" "N" match))
                                   value))
        (t value)))

(defun ek9-itest--differences (buffer table skip namespaced)
  "Return the differences between the list BUFFER and the kubectl TABLE.
SKIP is a list of column titles that change with time.  NAMESPACED
non-nil means that the row IDs of BUFFER have a namespace."
  (let* ((rows (ek9-itest--ek9-rows buffer))
         (titles (with-current-buffer buffer
                   (mapcar #'car tabulated-list-format)))
         (kubectl-titles (mapcar #'car (car table)))
         differences)
    (unless (equal titles kubectl-titles)
      (push (list 'titles titles kubectl-titles) differences))
    (unless (= (length rows) (length table))
      (push (list 'rows (mapcar #'car rows)
                  (mapcar (lambda (row) (cdr (assoc "NAME" row))) table))
            differences))
    (dolist (row table)
      ;; kubectl adds " (default)" to the name of the default
      ;; storage class.  A name has no spaces, so use the first word.
      (let* ((name (car (split-string (cdr (assoc "NAME" row)) " ")))
             (id (if namespaced (concat ek9-itest--namespace "/" name) name))
             (ek9-row (cdr (assoc id rows))))
        (if (not ek9-row)
            (push (list 'missing id) differences)
          (pcase-dolist (`(,title . ,value) row)
            (unless (member title skip)
              (let ((ours (cdr (assoc title ek9-row))))
                (unless (equal (ek9-itest--normalize title (or ours ""))
                               (ek9-itest--normalize title value))
                  (push (list id title ours value) differences))))))))
    differences))

(defun ek9-itest--check-columns (name skip &optional wide)
  "Check that the list of the type NAME shows the same as \"kubectl get\".
SKIP is a list of column titles that change with time.  If WIDE is
non-nil, show the wide columns and compare with \"kubectl get -o
wide\".  Else hide them and compare with \"kubectl get\".  Try again
for 30 seconds, because values can change between the two calls.
Return nil, or the differences of the last try.  The metric columns
are not in \"kubectl get\", so turn them off."
  (let* ((ek9-metrics nil)
         (resource (ek9-resource-get name))
         (namespaced (ek9-resource-namespaced resource))
         (buffer (ek9-itest--open name))
         differences)
    (with-current-buffer buffer
      (setq ek9-list--wide wide)
      (ek9-list--render))
    ;; Catch only the timeout, so that another error fails the test.
    (condition-case err
        (ek9-itest--wait-until
         (lambda ()
           (ek9-itest--refresh buffer)
           (setq differences
                 (ek9-itest--differences
                  buffer
                  (apply #'ek9-itest--table (ek9-resource-kind resource)
                         (append (when namespaced (list "-n" ek9-itest--namespace))
                                 (when wide (list "-o" "wide"))))
                  skip namespaced))
           (or (null differences) (progn (sleep-for 1) nil)))
         30)
      (error
       (unless (equal (cadr err) "Timeout while waiting in an integration test")
         (signal (car err) (cdr err)))))
    differences))

;;;; Watch

(ert-deftest ek9-itest-watch-shows-changes ()
  ;; The timer does not tick in this test, so only the watch can show
  ;; the changes.
  (ek9-itest--with-cluster
    (when (timerp ek9-list--timer)
      (cancel-timer ek9-list--timer)
      (setq ek9-list--timer nil))
    (let* ((ek9-refresh-interval 3600)
           (ek9-watch t)
           (name "ek9-watch-probe")
           (id (concat ek9-itest--namespace "/" name)))
      (ek9-itest--kubectl "-n" ek9-itest--namespace "delete" "configmap" name
                          "--ignore-not-found")
      (unwind-protect
          (let ((buffer (ek9-itest--open 'configmaps)))
            (cl-flet ((data ()
                        (cdr (assoc "DATA" (cdr (assoc id (ek9-itest--ek9-rows buffer)))))))
              (with-current-buffer buffer
                (ek9-itest--wait-until (lambda () (ek9-proxy-live-p ek9-list--watch)))
                (ek9-itest--kubectl "-n" ek9-itest--namespace "create" "configmap" name
                                    "--from-literal=a=1")
                (ek9-itest--wait-until (lambda () (equal (data) "1")) 20)
                (ek9-itest--kubectl "-n" ek9-itest--namespace "patch" "configmap" name
                                    "-p" "{\"data\":{\"b\":\"2\"}}")
                (ek9-itest--wait-until (lambda () (equal (data) "2")) 20)
                (ek9-itest--kubectl "-n" ek9-itest--namespace "delete" "configmap" name)
                (ek9-itest--wait-until
                 (lambda () (not (assoc id tabulated-list-entries))) 20)
                (should-not ek9-list--error)
                (should (ek9-proxy-live-p ek9-list--watch)))))
        (ek9-itest--kubectl "-n" ek9-itest--namespace "delete" "configmap" name
                            "--ignore-not-found")))))

;;;; Metrics

(defun ek9-itest--metric-differences (buffer table titles namespaced)
  "Return the differences between the metrics of BUFFER and TABLE.
TABLE is the table of \"kubectl top\".  TITLES is an alist of (TITLE
. KUBECTL-TITLE).  NAMESPACED non-nil means that the row IDs of
BUFFER have a namespace."
  (let ((rows (ek9-itest--ek9-rows buffer))
        differences)
    (dolist (row table)
      (let* ((name (cdr (assoc "NAME" row)))
             (id (if namespaced (concat ek9-itest--namespace "/" name) name))
             (ek9-row (cdr (assoc id rows))))
        (if (not ek9-row)
            (push (list 'missing id) differences)
          (pcase-dolist (`(,title . ,kubectl-title) titles)
            (let ((ours (cdr (assoc title ek9-row)))
                  (theirs (cdr (assoc kubectl-title row))))
              (unless (equal ours theirs)
                (push (list id title ours theirs) differences)))))))
    differences))

(defun ek9-itest--check-metrics (name top-args titles)
  "Check that the list of NAME shows the same metrics as \"kubectl top\".
TOP-ARGS are the arguments after \"top\".  TITLES is an alist of (TITLE
. KUBECTL-TITLE).  metrics-server changes the values every 15
seconds, so try again for 30 seconds.  Return nil, or the differences
of the last try."
  (let ((buffer (ek9-itest--open name))
        (namespaced (ek9-resource-namespaced (ek9-resource-get name)))
        differences)
    (with-current-buffer buffer
      (ek9-itest--wait-until (lambda () ek9-list--metrics)))
    (condition-case err
        (ek9-itest--wait-until
         (lambda ()
           (with-current-buffer buffer
             (setq ek9-list--metrics-time nil)
             (ek9-list--refresh-metrics)
             (ek9-itest--wait-until
              (lambda () (not (ek9-proxy-live-p ek9-list--metrics-request))))
             (accept-process-output nil 0.1))
           (setq differences
                 (ek9-itest--metric-differences
                  buffer
                  (ek9-itest--parse-table (apply #'ek9-itest--kubectl "top" top-args))
                  titles namespaced))
           (or (null differences) (progn (sleep-for 1) nil)))
         30)
      (error
       (unless (equal (cadr err) "Timeout while waiting in an integration test")
         (signal (car err) (cdr err)))))
    differences))

(ert-deftest ek9-itest-metrics-nodes ()
  (ek9-itest--with-cluster
    (should-not (ek9-itest--check-metrics
                 'nodes '("nodes")
                 '(("CPU" . "CPU(cores)") ("CPU/A" . "CPU(%)")
                   ("MEM" . "MEMORY(bytes)") ("MEM/A" . "MEMORY(%)"))))))

(ert-deftest ek9-itest-metrics-pods ()
  (ek9-itest--with-cluster
    (should-not (ek9-itest--check-metrics
                 'pods (list "pods" "-n" ek9-itest--namespace)
                 '(("CPU" . "CPU(cores)") ("MEM" . "MEMORY(bytes)"))))))

;;;; Columns

(ert-deftest ek9-itest-columns-pods ()
  (ek9-itest--with-cluster
    (should-not (ek9-itest--check-columns 'pods nil))
    (should-not (ek9-itest--check-columns 'pods nil t))))

(ert-deftest ek9-itest-columns-workloads ()
  (ek9-itest--with-cluster
    (dolist (name '(deployments replicasets statefulsets daemonsets))
      (should-not (ek9-itest--check-columns name nil))
      (should-not (ek9-itest--check-columns name nil t)))))

(ert-deftest ek9-itest-columns-jobs ()
  (ek9-itest--with-cluster
    (dolist (name '(jobs cronjobs))
      (should-not (ek9-itest--check-columns name nil))
      (should-not (ek9-itest--check-columns name nil t)))))

(ert-deftest ek9-itest-columns-config ()
  (ek9-itest--with-cluster
    (dolist (name '(services configmaps secrets))
      (should-not (ek9-itest--check-columns name nil))
      (should-not (ek9-itest--check-columns name nil t)))))

(ert-deftest ek9-itest-columns-cluster ()
  (ek9-itest--with-cluster
    (dolist (name '(nodes namespaces))
      (should-not (ek9-itest--check-columns name nil))
      (should-not (ek9-itest--check-columns name nil t)))))

;;;; CRD types

(defun ek9-itest--load-types ()
  "Load the discovered types of the kind context and wait."
  (let (done)
    (ek9-types-ensure (lambda () (setq done t)))
    (ek9-itest--wait-until (lambda () done))))

(ert-deftest ek9-itest-crd-types ()
  (ek9-itest--with-cluster
    (ek9-itest--load-types)
    (let ((names (mapcar #'car (ek9-types-current))))
      (dolist (name '(widgets.ek9.example.com gizmos.ek9.example.com
                      sprockets.ek9.example.com ingresses
                      persistentvolumeclaims roles events.events.k8s.io))
        (should (memq name names)))
      ;; The registry defines pods, so discovery skips them.  The
      ;; metrics type pods.metrics.k8s.io is another type.
      (should-not (memq 'pods names))
      (should (memq 'pods.metrics.k8s.io names)))
    (let ((widgets (ek9-resource-get 'widgets.ek9.example.com)))
      ;; v1 is the preferred version, not v1alpha1.
      (should (equal (ek9-resource-kind widgets) "widgets.v1.ek9.example.com"))
      (should (equal (ek9-resource-aliases widgets) '("widgets" "widget" "wdg"))))
    (should-not (ek9-resource-namespaced (ek9-resource-get 'gizmos.ek9.example.com)))))

(ert-deftest ek9-itest-columns-crds ()
  (ek9-itest--with-cluster
    (ek9-itest--load-types)
    (dolist (name '(widgets.ek9.example.com gizmos.ek9.example.com
                    sprockets.ek9.example.com))
      (should-not (ek9-itest--check-columns name nil))
      (should-not (ek9-itest--check-columns name nil t)))))

(defconst ek9-itest--unstable-types
  '(componentstatuses events.events.k8s.io leases)
  "Discovered types whose rows change too often for a comparison.
The row order of componentstatuses changes on each call.  Events and
leases change all the time.")

(ert-deftest ek9-itest-columns-discovered-types ()
  ;; Each discovered type with objects shows the same columns and
  ;; values as kubectl, in both modes.  The CRD types have their own
  ;; test.
  (ek9-itest--with-cluster
    (ek9-itest--load-types)
    (let ((checked nil))
      (pcase-dolist (`(,name . ,resource) (ek9-types-current))
        (unless (or (memq name ek9-itest--unstable-types)
                    (equal (car (ek9-resource-api resource)) "ek9.example.com")
                    (string-empty-p
                     (apply #'ek9-itest--kubectl "get" (ek9-resource-kind resource)
                            "-o" "name"
                            (when (ek9-resource-namespaced resource)
                              (list "-n" ek9-itest--namespace)))))
          (push name checked)
          (dolist (wide '(nil t))
            (let ((differences (ek9-itest--check-columns name nil wide)))
              (should-not (and differences (list name wide differences)))))))
      ;; The manifests have objects of these types.
      (dolist (name '(ingresses persistentvolumeclaims roles rolebindings
                      serviceaccounts networkpolicies horizontalpodautoscalers
                      poddisruptionbudgets resourcequotas priorityclasses
                      clusterroles))
        (should (memq name checked))))))

(ert-deftest ek9-itest-crd-describe-and-yaml ()
  (ek9-itest--with-cluster
    (ek9-itest--load-types)
    (let* ((resource (ek9-resource-get 'widgets.ek9.example.com))
           (item (ek9-itest--item 'widgets.ek9.example.com "ek9-itest/full")))
      (should (string-match-p "^Name: +full$"
                              (ek9-itest--view-text (ek9-describe resource item))))
      (should (string-match-p "^kind: Widget$"
                              (ek9-itest--view-text (ek9-yaml resource item)))))
    (let ((resource (ek9-resource-get 'gizmos.ek9.example.com)))
      (should (string-match-p "^Name: +gizmo-one$"
                              (ek9-itest--view-text
                               (ek9-describe resource
                                             (ek9-itest--item 'gizmos.ek9.example.com
                                                              "gizmo-one"))))))))

;;;; Drill-downs

(defun ek9-itest--goto (buffer id)
  "Move point in the list BUFFER to the row ID."
  (with-current-buffer buffer
    (goto-char (or (ek9-list--find-id id) (error "No row %s" id)))))

(defun ek9-itest--names (buffer)
  "Return the sorted names of the rows in the list BUFFER."
  (with-current-buffer buffer
    (sort (mapcar #'ek9-resource-item-name ek9-list--items) #'string<)))

(defun ek9-itest--kubectl-names (&rest args)
  "Return the sorted names of \"kubectl get ARGS -o name\", without kind."
  (sort (mapcar (lambda (line) (car (last (split-string line "/"))))
                (split-string (apply #'ek9-itest--kubectl "get" "-o" "name"
                                     "-n" ek9-itest--namespace args)
                              "\n" t))
        #'string<))

(ert-deftest ek9-itest-drill-deployment-to-pods ()
  (ek9-itest--with-cluster
    (let ((deployments (ek9-itest--open 'deployments)))
      (ek9-itest--goto deployments "ek9-itest/web")
      (let ((replicasets (with-current-buffer deployments
                           (ek9-itest--wait-loaded (ek9-list-drill-down)))))
        ;; The deployment owns all its replica sets, also old ones after
        ;; a restart.
        (should (equal (ek9-itest--names replicasets)
                       (ek9-itest--kubectl-names "replicasets" "-l" "app=web")))
        (let* ((rs (with-current-buffer replicasets
                     (seq-find (lambda (item)
                                 (> (or (ek9-resource-path item '(spec replicas)) 0)
                                    0))
                               ek9-list--items)))
               (hash (ek9-resource-path rs '(metadata labels pod-template-hash)))
               (pods (progn (ek9-itest--goto replicasets
                                             (ek9-resource-item-id rs))
                            (with-current-buffer replicasets
                              (ek9-itest--wait-loaded (ek9-list-drill-down))))))
          ;; Only the pods of this replica set.
          (should (equal (ek9-itest--names pods)
                         (ek9-itest--kubectl-names
                          "pods" "-l" (concat "pod-template-hash=" hash))))
          (should (ek9-itest--names pods)))))))

(defun ek9-itest--running-web-pod ()
  "Return a web pod item whose STATUS column is Running.
Other tests restart and scale the deployment.  A pod that terminates
still has the phase Running, so use the STATUS column, as kubectl
shows it."
  (ek9-itest--kubectl "-n" ek9-itest--namespace "rollout" "status"
                      "deployment/web" "--timeout=180s")
  (ek9-itest--wait-until
   (lambda ()
     (let ((buffer (ek9-itest--open 'pods)))
       (ek9-itest--refresh buffer)
       (with-current-buffer buffer
         (seq-find (lambda (item)
                     (and (string-prefix-p "web-" (ek9-resource-item-name item))
                          (equal (ek9-resource-cell item "STATUS") "Running")))
                   ek9-list--items))))))

(ert-deftest ek9-itest-drill-pod-to-containers ()
  (ek9-itest--with-cluster
    (let* ((item (ek9-itest--running-web-pod))
           (pods (ek9-itest--open 'pods)))
      (ek9-itest--goto pods (ek9-resource-item-id item))
      (let ((containers (with-current-buffer pods
                          (ek9-itest--wait-loaded (ek9-list-drill-down)))))
        (with-current-buffer containers
          (should (equal (mapcar (lambda (item)
                                   (list (ek9-resource-item-name item)
                                         (alist-get 'ek9-type item)
                                         (ek9-resource-container-state item)))
                                 ek9-list--items)
                         '(("init" "init" "Completed")
                           ("nginx" "container" "Running")))))))))

(ert-deftest ek9-itest-drill-cronjob-to-jobs ()
  (ek9-itest--with-cluster
    (let ((cronjobs (ek9-itest--open 'cronjobs)))
      (ek9-itest--goto cronjobs "ek9-itest/tick")
      (let ((jobs (with-current-buffer cronjobs
                    (ek9-itest--wait-loaded (ek9-list-drill-down)))))
        (should (equal (ek9-itest--names jobs) '("tick-manual")))))))

(ert-deftest ek9-itest-events-of-pod ()
  (ek9-itest--with-cluster
    (let ((pods (ek9-itest--open 'pods)))
      (ek9-itest--goto pods "ek9-itest/crasher")
      (let ((events (with-current-buffer pods
                      (ek9-itest--wait-loaded (ek9-list-events)))))
        (with-current-buffer events
          (should ek9-list--items)
          (should (seq-every-p
                   (lambda (item)
                     (let ((object (alist-get 'involvedObject item)))
                       (and (equal (alist-get 'name object) "crasher")
                            (equal (alist-get 'kind object) "Pod")
                            (equal (alist-get 'namespace object)
                                   ek9-itest--namespace))))
                   ek9-list--items))
          (should (seq-some (lambda (item) (equal (alist-get 'reason item)
                                                  "BackOff"))
                            ek9-list--items)))))))

(ert-deftest ek9-itest-drill-node-to-pods ()
  (ek9-itest--with-cluster
    (let ((nodes (ek9-itest--open 'nodes)))
      (ek9-itest--goto nodes "ek9-itest-control-plane")
      (let ((pods (with-current-buffer nodes
                    (ek9-itest--wait-loaded (ek9-list-drill-down)))))
        (with-current-buffer pods
          ;; The pods of the node are in all namespaces.
          (should (seq-some (lambda (item)
                              (equal (ek9-resource-item-namespace item)
                                     "kube-system"))
                            ek9-list--items))
          (should (seq-every-p
                   (lambda (item)
                     (equal (ek9-resource-path item '(spec nodeName))
                            "ek9-itest-control-plane"))
                   ek9-list--items)))))))

(ert-deftest ek9-itest-all-namespaces ()
  (ek9-itest--with-cluster
    (ek9-list-select-resource '(resource pods all))
    (let ((buffer (ek9-itest--wait-loaded (get-buffer "*ek9 pods*"))))
      (with-current-buffer buffer
        (should (equal (car (aref tabulated-list-format 0)) "NAMESPACE"))
        (should (seq-some (lambda (item)
                            (equal (ek9-resource-item-namespace item)
                                   "kube-system"))
                          ek9-list--items))))))

;;;; Views

(defun ek9-itest--view-text (buffer)
  "Wait until the view BUFFER has text, and return the text."
  (with-current-buffer buffer
    (ek9-itest--wait-until (lambda () (> (buffer-size) 0)))
    (buffer-string)))

(defun ek9-itest--item (name id)
  "Return the item ID from a new list of the resource type NAME."
  (let ((buffer (ek9-itest--open name)))
    (with-current-buffer buffer
      (or (gethash id ek9-list--index) (error "No item %s" id)))))

(ert-deftest ek9-itest-describe-and-yaml ()
  (ek9-itest--with-cluster
    (let ((item (ek9-itest--item 'deployments "ek9-itest/web")))
      (should (string-match-p "^Name: +web$"
                              (ek9-itest--view-text
                               (ek9-describe (ek9-resource-get 'deployments) item))))
      (should (string-match-p "^kind: Deployment$"
                              (ek9-itest--view-text
                               (ek9-yaml (ek9-resource-get 'deployments) item)))))))

(ert-deftest ek9-itest-secret-decode ()
  (ek9-itest--with-cluster
    (let ((item (ek9-itest--item 'secrets "ek9-itest/creds")))
      (should (string-match-p "^user: dummy-user$"
                              (ek9-itest--view-text
                               (ek9-secret-decode (ek9-resource-get 'secrets)
                                                  item)))))))

(ert-deftest ek9-itest-logs ()
  (ek9-itest--with-cluster
    (let* ((item (ek9-itest--item 'pods "ek9-itest/crasher"))
           (buffer (ek9-logs item "crasher")))
      (with-current-buffer buffer
        (ek9-itest--wait-until
         (lambda () (string-match-p "^crash$" (buffer-string))))))))

;;;; Write actions

(defmacro ek9-itest--confirming (&rest body)
  "Run BODY and answer yes to each confirmation question."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
             ((symbol-function 'y-or-n-p) (lambda (_) t)))
     ,@body))

(defun ek9-itest--jsonpath (path &rest args)
  "Return the PATH value of \"kubectl get ARGS\" in the test namespace."
  (apply #'ek9-itest--kubectl "get" "-n" ek9-itest--namespace
         (append args (list "-o" (concat "jsonpath=" path)))))

(ert-deftest ek9-itest-edit-replace ()
  (ek9-itest--with-cluster
    (unwind-protect
        (let* ((item (ek9-itest--item 'configmaps "ek9-itest/settings"))
               (buffer (ek9-edit (ek9-resource-get 'configmaps) item)))
          (with-current-buffer buffer
            (ek9-itest--wait-until (lambda () (string-match-p "mode: blue"
                                                              (buffer-string))))
            (goto-char (point-min))
            (search-forward "mode: blue")
            (replace-match "mode: green")
            (ek9-itest--confirming (ek9-edit-apply))
            (ek9-itest--wait-until (lambda () (not (buffer-modified-p))))
            (should-not ek9-edit--status))
          (should (equal (ek9-itest--jsonpath "{.data.mode}" "configmap" "settings")
                         "green")))
      (ek9-itest--kubectl "-n" ek9-itest--namespace "patch" "configmap" "settings"
                          "-p" "{\"data\":{\"mode\":\"blue\"}}"))))

(ert-deftest ek9-itest-crd-edit-replace ()
  (ek9-itest--with-cluster
    (ek9-itest--load-types)
    (unwind-protect
        (let* ((resource (ek9-resource-get 'widgets.ek9.example.com))
               (item (ek9-itest--item 'widgets.ek9.example.com "ek9-itest/full"))
               (buffer (ek9-edit resource item)))
          (with-current-buffer buffer
            (ek9-itest--wait-until (lambda () (string-match-p "owner: team-a"
                                                              (buffer-string))))
            (goto-char (point-min))
            (search-forward "owner: team-a")
            (replace-match "owner: team-b")
            (ek9-itest--confirming (ek9-edit-apply))
            (ek9-itest--wait-until (lambda () (not (buffer-modified-p))))
            (should-not ek9-edit--status))
          (should (equal (ek9-itest--jsonpath "{.spec.owner}" "widget" "full")
                         "team-b"))
          ;; ek9 writes the edit into the last-applied annotation,
          ;; without the server fields.
          (let ((last-applied
                 (json-parse-string
                  (ek9-itest--jsonpath
                   "{.metadata.annotations.kubectl\\.kubernetes\\.io/last-applied-configuration}"
                   "widget" "full")
                  :object-type 'alist)))
            (should (equal (alist-get 'owner (alist-get 'spec last-applied))
                           "team-b"))
            (should-not (alist-get 'resourceVersion
                                   (alist-get 'metadata last-applied)))
            (should-not (alist-get 'uid (alist-get 'metadata last-applied)))))
      ;; "kubectl apply" works after the edit and resets the widget.
      ;; If the annotation contains resourceVersion, the apply fails.
      ;; Then remove the annotation, so that run.sh can apply again.
      (condition-case err
          (ek9-itest--kubectl "apply" "-f" (expand-file-name "custom-resources.yaml"
                                                             ek9-itest--dir))
        (error
         (ek9-itest--kubectl "-n" ek9-itest--namespace "annotate" "widget" "full"
                             "kubectl.kubernetes.io/last-applied-configuration-")
         (signal (car err) (cdr err))))
      (should (equal (ek9-itest--jsonpath "{.spec.owner}" "widget" "full")
                     "team-a")))))

(ert-deftest ek9-itest-edit-blocks-other-name ()
  (ek9-itest--with-cluster
    (unwind-protect
        (let* ((item (ek9-itest--item 'configmaps "ek9-itest/settings"))
               (buffer (ek9-edit (ek9-resource-get 'configmaps) item)))
          (with-current-buffer buffer
            (ek9-itest--wait-until (lambda () (> (buffer-size) 0)))
            (goto-char (point-min))
            (re-search-forward "^  name: settings$")
            (replace-match "  name: settings-copy")
            (ek9-itest--confirming (ek9-edit-apply))
            (ek9-itest--wait-until (lambda () ek9-edit--status))
            (should (string-match-p "Nothing was applied" ek9-edit--status)))
          (should-error (ek9-itest--kubectl "-n" ek9-itest--namespace
                                            "get" "configmap" "settings-copy")))
      ;; If ek9 did not block it, remove the copy, so that the next run
      ;; does not fail.
      (ek9-itest--kubectl "-n" ek9-itest--namespace "delete" "configmap"
                          "settings-copy" "--ignore-not-found"))))

(ert-deftest ek9-itest-edit-conflict ()
  (ek9-itest--with-cluster
    (unwind-protect
        (let* ((item (ek9-itest--item 'configmaps "ek9-itest/settings"))
               (buffer (ek9-edit (ek9-resource-get 'configmaps) item)))
          (with-current-buffer buffer
            (ek9-itest--wait-until (lambda () (string-match-p "mode: blue"
                                                              (buffer-string))))
            ;; Another user changes the object while the buffer is open.
            (ek9-itest--kubectl "-n" ek9-itest--namespace "patch" "configmap"
                                "settings" "-p" "{\"data\":{\"mode\":\"red\"}}")
            (goto-char (point-min))
            (search-forward "mode: blue")
            (replace-match "mode: green")
            (ek9-itest--confirming (ek9-edit-apply))
            (ek9-itest--wait-until (lambda () ek9-edit--status))
            (should (string-match-p "Apply failed:.*modified" ek9-edit--status))
            (should (buffer-modified-p)))
          ;; The change of the other user stays.
          (should (equal (ek9-itest--jsonpath "{.data.mode}" "configmap" "settings")
                         "red")))
      (ek9-itest--kubectl "-n" ek9-itest--namespace "patch" "configmap" "settings"
                          "-p" "{\"data\":{\"mode\":\"blue\"}}"))))

(defun ek9-itest--run-action (function resource item &rest args)
  "Call the write action FUNCTION for ITEM of RESOURCE with ARGS.
Answer yes to the question, and wait until kubectl is done."
  (let ((process (ek9-itest--confirming
                   (apply function (ek9-resource-get resource) item args))))
    (ek9-itest--wait-until (lambda () (not (ek9-process-live-p process))))
    (accept-process-output nil 0.2)))

(ert-deftest ek9-itest-scale ()
  (ek9-itest--with-cluster
    (unwind-protect
        (progn
          (ek9-itest--run-action #'ek9-scale 'deployments
                                 (ek9-itest--item 'deployments "ek9-itest/web") 3)
          (should (equal (ek9-itest--jsonpath "{.spec.replicas}"
                                              "deployment" "web")
                         "3")))
      (ek9-itest--kubectl "-n" ek9-itest--namespace "scale" "deployment" "web"
                          "--replicas=2"))))

(ert-deftest ek9-itest-restart ()
  (ek9-itest--with-cluster
    (cl-flet ((restarted-at ()
                (ek9-itest--jsonpath
                 "{.spec.template.metadata.annotations.kubectl\\.kubernetes\\.io/restartedAt}"
                 "deployment" "web")))
      ;; An earlier run leaves the annotation.  It must change.
      (let ((before (restarted-at)))
        ;; The annotation has seconds.  Make sure the time is different.
        (sleep-for 1.1)
        (ek9-itest--run-action #'ek9-restart 'deployments
                               (ek9-itest--item 'deployments "ek9-itest/web"))
        (should-not (string-empty-p (restarted-at)))
        (should-not (equal (restarted-at) before))))
    (ek9-itest--kubectl "-n" ek9-itest--namespace "rollout" "status"
                        "deployment/web" "--timeout=180s")))

(defun ek9-itest--force-delete-doomed ()
  "Delete the test pod doomed now, if it exists.
Its sleep process ignores SIGTERM, so a normal delete takes 30 seconds."
  (ek9-itest--kubectl "-n" ek9-itest--namespace "delete" "pod" "doomed"
                      "--ignore-not-found" "--grace-period=0" "--force"))

(ert-deftest ek9-itest-delete ()
  (ek9-itest--with-cluster
    (ek9-itest--force-delete-doomed)
    (ek9-itest--kubectl "-n" ek9-itest--namespace "run" "doomed"
                        "--image=busybox:1.36" "--restart=Never"
                        "--" "sleep" "3600")
    (unwind-protect
        (progn
          (ek9-itest--run-action #'ek9-delete 'pods
                                 (ek9-itest--item 'pods "ek9-itest/doomed"))
          (ek9-itest--wait-until
           (lambda ()
             (not (member "doomed" (ek9-itest--kubectl-names "pods"))))
           120))
      (ignore-errors (ek9-itest--force-delete-doomed)))))

(ert-deftest ek9-itest-read-only-blocks-write ()
  (ek9-itest--with-cluster
    (let ((ek9-read-only-contexts (list ek9-itest--context))
          (item (ek9-itest--item 'deployments "ek9-itest/web")))
      (should-error (ek9-itest--confirming
                      (ek9-scale (ek9-resource-get 'deployments) item 5))
                    :type 'user-error)
      (should (equal (ek9-itest--jsonpath "{.spec.replicas}" "deployment" "web")
                     "2")))))

;;;; Shell and port-forward

(ert-deftest ek9-itest-shell ()
  (ek9-itest--with-cluster
    (let* ((ek9-exec-terminal 'term)
           (item (ek9-itest--running-web-pod))
           (buffer (ek9-shell item "nginx"))
           (process (get-buffer-process buffer)))
      (cl-flet* ((text ()
                   (with-current-buffer buffer
                     (buffer-substring-no-properties (point-min) (point-max))))
                 (prompts ()
                   (let ((count 0) (start 0) (text (text)))
                     (while (string-match "^/ # " text start)
                       (setq count (1+ count) start (match-end 0)))
                     count))
                 (wait (predicate what)
                   (condition-case nil
                       (ek9-itest--wait-until predicate 20)
                     (error (error "No %s.  The shell buffer has: %S"
                                   what (text))))))
        (wait (lambda () (>= (prompts) 1)) "first prompt")
        ;; In batch Emacs, BusyBox sh loses the first input line.  With
        ;; real typing in "emacs -nw", the first line works.  Thus send
        ;; empty lines until the shell shows a second prompt.
        (wait (lambda ()
                (or (>= (prompts) 2)
                    (progn (term-send-string process "\n")
                           (accept-process-output process 2)
                           (>= (prompts) 2))))
              "second prompt")
        (term-send-string process "expr 40 + 2\n")
        (wait (lambda () (string-match-p "^42\r?$" (text))) "result 42")))))

(ert-deftest ek9-itest-port-forward ()
  (ek9-itest--with-cluster
    (let* ((item (ek9-itest--item 'services "ek9-itest/web"))
           (forward (ek9-port-forward (ek9-resource-get 'services) item ":80")))
      (ek9-itest--wait-until
       (lambda () (equal (ek9-portfwd--forward-status forward) "Running")))
      (let* ((port (string-to-number (ek9-portfwd--forward-local-port forward)))
             (reply "")
             (stream (open-network-stream "ek9-itest-http" nil "127.0.0.1" port)))
        (set-process-filter stream (lambda (_ text) (setq reply (concat reply text))))
        (process-send-string stream "GET / HTTP/1.0\r\nHost: web\r\n\r\n")
        (ek9-itest--wait-until (lambda () (string-match-p "nginx" reply)))
        (delete-process stream))
      (ek9-portfwd-stop forward)
      (ek9-itest--wait-until
       (lambda () (equal (ek9-portfwd--forward-status forward) "Stopped"))))))

(provide 'ek9-itest)

;;; ek9-itest.el ends here
