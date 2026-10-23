;;; ek9-proxy-test.el --- Tests for ek9-proxy -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the proxy, the HTTP client, and the server tables.
;; The fake kubectl starts the fake proxy in test/fixtures.

;;; Code:

(require 'ert)
(require 'ek9-proxy)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--proxy-get (context path &optional accept exact)
  "Get PATH through the proxy of CONTEXT and wait for the reply.
ACCEPT and EXACT have the meaning of `ek9-proxy-get'.  Return
\(ok . JSON) or (error CODE . TEXT)."
  (let (result)
    (ek9-test--with-fake-kubectl
      (ek9-proxy-get context path
                     (lambda (json) (setq result (cons 'ok json)))
                     (lambda (code text) (setq result (cons 'error (cons code text))))
                     accept exact)
      (ek9-test--wait-until (lambda () result) 10))
    result))

;;;; Paths

(ert-deftest ek9-proxy-path-forms ()
  (should (equal (ek9-proxy-path '("" "v1" "pods") "team-a")
                 "/api/v1/namespaces/team-a/pods"))
  (should (equal (ek9-proxy-path '("apps" "v1" "deployments") nil)
                 "/apis/apps/v1/deployments"))
  (should (equal (ek9-proxy-path '("" "v1" "pods") "a" "web-1")
                 "/api/v1/namespaces/a/pods/web-1"))
  (should (equal (ek9-proxy-path '("" "v1" "pods") nil nil
                                 '(("labelSelector" . "app=web,tier in (a b)")
                                   ("includeObject" . "Object")))
                 (concat "/api/v1/pods?labelSelector=app%3Dweb%2Ctier%20in%20%28a%20b%29"
                         "&includeObject=Object"))))

;;;; Cells and tables

(ert-deftest ek9-proxy-format-float ()
  ;; The same texts as Go "%v" for a float64.
  (dolist (case '((1.5 . "1.5") (-2.5 . "-2.5") (0.1 . "0.1")
                  (123456.5 . "123456.5") (1234567.5 . "1.2345675e+06")
                  (0.0001 . "0.0001") (0.00001 . "1e-05")
                  (1.5e-7 . "1.5e-07") (1.5e300 . "1.5e+300")
                  (3.0 . "3") (1e6 . "1000000")))
    (should (equal (ek9-proxy--format-float (car case)) (cdr case)))))

(ert-deftest ek9-proxy-format-float-ignores-output-format ()
  (let ((float-output-format "%.2f"))
    (should (equal (ek9-proxy-cell-text 0.125) "0.125"))))

(ert-deftest ek9-proxy-cell-text-as-kubectl ()
  (should (equal (ek9-proxy-cell-text "Running") "Running"))
  (should (equal (ek9-proxy-cell-text 3) "3"))
  (should (equal (ek9-proxy-cell-text 1.5) "1.5"))
  (should (equal (ek9-proxy-cell-text t) "true"))
  (should (equal (ek9-proxy-cell-text :false) "false"))
  ;; kubectl prints a null cell as an empty string.
  (should (equal (ek9-proxy-cell-text :null) "")))

(ert-deftest ek9-proxy-table-columns-from-definitions ()
  (let* ((table (ek9-test--table "widgets.v1.ek9.example.com"))
         (columns (ek9-proxy-table-columns table)))
    (should (equal (mapcar #'car columns)
                   '("NAME" "PHASE" "REPLICAS" "RATIO" "ENABLED" "READY"
                     "HOSTS" "FIRST HOST" "LABELS" "OWNER" "STARTED" "AGE")))
    ;; Priority 1 is a wide column, as in "kubectl get -o wide".
    (should (equal (mapcar #'car (seq-filter (lambda (column)
                                               (plist-get (nthcdr 3 column) :wide))
                                             columns))
                   '("OWNER")))
    ;; Numbers align to the right.
    (should (equal (mapcar #'car (seq-filter (lambda (column)
                                               (plist-get (nthcdr 3 column)
                                                          :right-align))
                                             columns))
                   '("REPLICAS" "RATIO")))))

(ert-deftest ek9-proxy-table-items-have-cells-and-objects ()
  (let* ((items (ek9-proxy-table-items (ek9-test--table "widgets.v1.ek9.example.com")))
         (falsy (nth 1 items)))
    (should (= (length items) 4))
    (should (equal (alist-get 'ek9-cells falsy)
                   ["falsy" "false" "2" "0.1" "false" "" "only.example.com"
                    "only.example.com" "{\"a\":\"1\",\"b\":\"2\"}" "true"
                    "<unknown>" "17h"]))
    ;; All items share one titles vector.
    (should (eq (alist-get 'ek9-titles falsy)
                (alist-get 'ek9-titles (car items))))
    ;; The object is in the normal form: false is nil.
    (should (equal (ek9-resource-item-id falsy) "ek9-itest/falsy"))
    (should (null (ek9-resource-path falsy '(spec enabled))))
    (should (listp (ek9-resource-path falsy '(spec hosts))))))

;;;; HTTP

(ert-deftest ek9-proxy-get-table ()
  (pcase-let ((`(ok . ,table)
               (ek9-test--proxy-get "dev" "/api/v1/namespaces/a/pods"
                                    ek9-proxy-table-accept 'table)))
    (should (equal (alist-get 'kind table) "Table"))
    (should (= (length (alist-get 'rows table)) 8))
    ;; The cells are exact, and the objects are in the normal form.
    (should (vectorp (alist-get 'cells (aref (alist-get 'rows table) 0))))
    (should (listp (ek9-resource-path (alist-get 'object (aref (alist-get 'rows table) 0))
                                      '(spec containers))))
    ;; The reply uses many chunks.  They must join without loss.
    (should (equal (ek9-proxy-cell-text
                    (aref (alist-get 'cells (aref (alist-get 'rows table) 1)) 2))
                   "CrashLoopBackOff"))))

(ert-deftest ek9-proxy-get-object-plain ()
  (pcase-let ((`(ok . ,pod) (ek9-test--proxy-get "dev" "/api/v1/namespaces/a/pods/web-1")))
    (should (equal (ek9-resource-item-name pod) "web-1"))
    (should (listp (ek9-resource-path pod '(spec containers))))))

(ert-deftest ek9-proxy-get-non-ascii ()
  (pcase-let ((`(ok . ,list) (ek9-test--proxy-get "dev" "/api/v1/unicodes")))
    (should (equal (ek9-resource-item-name (car (alist-get 'items list)))
                   "café ✓"))))

(ert-deftest ek9-proxy-get-error-status ()
  (should (equal (ek9-test--proxy-get "dev" "/api/v1/namespaces/a/pods/ek9-missing")
                 '(error 404 . "Error from server (NotFound): pods \"ek9-missing\" not found"))))

(ert-deftest ek9-proxy-start-failure ()
  ;; The fake kubectl fails each command in the context "forbidden".
  (pcase-let ((`(error ,code . ,text)
               (ek9-test--proxy-get "forbidden" "/api/v1/pods")))
    (should-not code)
    (should (equal text "Error from server (Forbidden): access denied"))
    (should-not (gethash (ek9-test--with-fake-kubectl (ek9-proxy--key "forbidden"))
                         ek9-proxy--proxies))))

(ert-deftest ek9-proxy-timeout ()
  (let ((ek9-process-timeout 0.3))
    (should (equal (ek9-test--proxy-get "dev" "/api/v1/ek9-slow")
                   '(error timeout . "Timeout after 0.3 s")))))

(ert-deftest ek9-proxy-cancel-calls-nothing ()
  (ek9-test--with-fake-kubectl
    (let* (called
           (request (ek9-proxy-get "dev" "/api/v1/namespaces/a/pods"
                                   (lambda (_) (setq called t))
                                   (lambda (&rest _) (setq called t)))))
      (should (ek9-proxy-live-p request))
      (ek9-proxy-cancel request)
      (should-not (ek9-proxy-live-p request))
      (accept-process-output nil 0.3)
      (should-not called))))

(ert-deftest ek9-proxy-restarts-after-stop ()
  (should (eq (car (ek9-test--proxy-get "dev" "/api/v1/pods")) 'ok))
  (let* ((key (ek9-test--with-fake-kubectl (ek9-proxy--key "dev")))
         (proxy (gethash key ek9-proxy--proxies))
         (directory (ek9-proxy-directory proxy)))
    ;; Only the user can open the socket directory.
    (should (= (file-modes directory) #o700))
    (delete-process (ek9-proxy-process proxy))
    (ek9-test--wait-until (lambda () (not (file-exists-p directory))))
    (should (eq (car (ek9-test--proxy-get "dev" "/api/v1/pods")) 'ok))
    (should-not (eq (gethash key ek9-proxy--proxies) proxy))))

(ert-deftest ek9-proxy-stop-all-removes-sockets ()
  (should (eq (car (ek9-test--proxy-get "dev" "/api/v1/pods")) 'ok))
  (let ((proxies (hash-table-values ek9-proxy--proxies)))
    (should proxies)
    (ek9-proxy-stop-all)
    (should (= (hash-table-count ek9-proxy--proxies) 0))
    (dolist (proxy proxies)
      (should-not (process-live-p (ek9-proxy-process proxy)))
      (should-not (file-exists-p (ek9-proxy-directory proxy))))))

(ert-deftest ek9-proxy-bad-reply-calls-error ()
  (pcase-let ((`(error ,code . ,text) (ek9-test--proxy-get "dev" "/api/v1/ek9-badjson")))
    (should-not code)
    (should (string-prefix-p "Bad reply from kubectl proxy: " text))))

(ert-deftest ek9-proxy-502-starts-new-proxy ()
  (should (eq (car (ek9-test--proxy-get "dev" "/api/v1/pods")) 'ok))
  (let* ((key (ek9-test--with-fake-kubectl (ek9-proxy--key "dev")))
         (proxy (gethash key ek9-proxy--proxies)))
    (should (equal (ek9-test--proxy-get "dev" "/api/v1/ek9-502")
                   '(error 502 . "Error from server: dial tcp 127.0.0.1:1: connect: connection refused")))
    ;; New requests use a new proxy at once.  The old process stops
    ;; later, so that other requests on it can end.
    (should-not (gethash key ek9-proxy--proxies))
    (should (eq (car (ek9-test--proxy-get "dev" "/api/v1/pods")) 'ok))
    (should-not (eq (gethash key ek9-proxy--proxies) proxy))
    (ek9-proxy--stop proxy "test")))

(ert-deftest ek9-proxy-stops-idle-proxies ()
  (should (eq (car (ek9-test--proxy-get "dev" "/api/v1/pods")) 'ok))
  (let* ((key (ek9-test--with-fake-kubectl (ek9-proxy--key "dev")))
         (proxy (gethash key ek9-proxy--proxies)))
    (setf (ek9-proxy-last-used proxy) (- (float-time) ek9-proxy--idle-time 1))
    ;; A new proxy for another context stops the idle one.
    (should (eq (car (ek9-test--proxy-get "prod" "/api/v1/pods")) 'ok))
    (should-not (gethash key ek9-proxy--proxies))
    (should-not (process-live-p (ek9-proxy-process proxy)))))

(ert-deftest ek9-proxy-lost-socket-starts-new-proxy ()
  (should (eq (car (ek9-test--proxy-get "dev" "/api/v1/pods")) 'ok))
  (let* ((key (ek9-test--with-fake-kubectl (ek9-proxy--key "dev")))
         (proxy (gethash key ek9-proxy--proxies)))
    ;; For example, a cleanup of /tmp removes the socket.
    (delete-file (ek9-proxy-socket proxy))
    (pcase-let ((`(error ,code . ,text) (ek9-test--proxy-get "dev" "/api/v1/pods")))
      (should-not code)
      (should (string-prefix-p "Cannot connect to kubectl proxy: " text)))
    (should-not (gethash key ek9-proxy--proxies))
    (should (eq (car (ek9-test--proxy-get "dev" "/api/v1/pods")) 'ok))
    (ek9-proxy--stop proxy "test")))

(ert-deftest ek9-proxy-start-timeout ()
  (let ((ek9-process-timeout 0.3))
    ;; The request timer starts first, so it usually ends the request.
    (pcase-let ((`(error ,code . ,text) (ek9-test--proxy-get "silent" "/api/v1/pods")))
      (should (memq code '(nil timeout)))
      (should (member text '("kubectl proxy did not start in 0.3 s"
                             "Timeout after 0.3 s"))))
    ;; The start timeout stops the proxy that hangs.
    (ek9-test--wait-until
     (lambda () (not (gethash (ek9-test--with-fake-kubectl (ek9-proxy--key "silent"))
                              ek9-proxy--proxies))))))

(ert-deftest ek9-proxy-key-has-environment ()
  (ek9-test--with-fake-kubectl
    (let ((key (ek9-proxy--key "dev")))
      (let ((process-environment (cons "AWS_PROFILE=other" process-environment)))
        (should-not (equal (ek9-proxy--key "dev") key))))))

(ert-deftest ek9-proxy-null-rows ()
  (let ((table (ek9-test--parse-table
                '((kind . "Table") (columnDefinitions . :null) (rows . :null)))))
    (should-not (ek9-proxy-table-items table))
    (should-not (ek9-proxy-table-columns table))))

(ert-deftest ek9-proxy-incomplete-reply ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\n{\"a\":\r\n")
    (should (equal (ek9-proxy--parse-reply (ek9-proxy--make-request))
                   '(error nil . "Incomplete reply from kubectl proxy"))))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n{}")
    (should (equal (ek9-proxy--parse-reply (ek9-proxy--make-request))
                   '(error nil . "Incomplete reply from kubectl proxy")))))

;;;; Time columns

(ert-deftest ek9-proxy-time-columns-use-object-time ()
  (let* ((created (format-time-string "%FT%TZ" (time-subtract nil 330) t))
         (table (ek9-test--parse-table
                 `((kind . "Table")
                   (columnDefinitions
                    . [((name . "Name") (type . "string") (format . "name")
                        (priority . 0) (description . "Name must be unique"))
                       ((name . "Age") (type . "string") (format . "")
                        (priority . 0)
                        (description . "CreationTimestamp is a timestamp representing the server time"))
                       ((name . "Started") (type . "date") (format . "")
                        (priority . 0)
                        (description . "Custom resource definition column (in JSONPath format): .status.startTime"))
                       ;; Cluster roles show the creation time itself.
                       ((name . "Created At") (type . "date") (format . "")
                        (priority . 0)
                        (description . "CreationTimestamp is a timestamp representing the server time"))])
                   (rows . [((cells . ["a" "1s" "<unknown>" "2026-10-22T10:10:57Z"])
                             (object (metadata (name . "a")
                                               (creationTimestamp . ,created))
                                     (status)))]))))
         (columns (ek9-proxy-table-columns table))
         (item (car (ek9-proxy-table-items table))))
    (should (equal (mapcar (lambda (column) (plist-get (nthcdr 3 column) :time))
                           columns)
                   '(nil t t nil)))
    (should (equal (funcall (nth 2 (nth 3 columns)) item) "2026-10-22T10:10:57Z"))
    ;; The age comes from the creation time, not from the old cell.
    (should (string-match-p "\\`5m3[01]s\\'" (funcall (nth 2 (nth 1 columns)) item)))
    ;; Without a time in the object, the column shows the server cell.
    (should (equal (funcall (nth 2 (nth 2 columns)) item) "<unknown>"))))

;;;; Watch

(defun ek9-test--chunked (text size)
  "Return TEXT in the chunked transfer encoding, in chunks of SIZE bytes."
  (let ((start 0) (parts nil))
    (while (< start (length text))
      (let ((chunk (substring text start (min (length text) (+ start size)))))
        (push (format "%x\r\n%s\r\n" (length chunk) chunk) parts)
        (setq start (+ start size))))
    (apply #'concat (nreverse parts))))

(ert-deftest ek9-proxy-watch-reads-events-in-parts ()
  (let* (events
         end
         (request (ek9-proxy--make-request
                   :state 'running
                   :on-error (lambda (code text) (setq end (list code text)))
                   :on-event (lambda (type object) (push (cons type object) events))))
         (buffer (generate-new-buffer " *ek9-test-http*" t))
         (body (generate-new-buffer " *ek9-test-body*" t))
         (table (lambda (name)
                  `((kind . "Table") (metadata (resourceVersion . "7"))
                    (columnDefinitions . [])
                    (rows . [((cells . [,name])
                              (object (metadata (name . ,name))))]))))
         (payload (encode-coding-string
                   (concat (json-serialize `((type . "ADDED")
                                             (object . ,(funcall table "café ✓"))))
                           "\n\n"
                           (json-serialize `((type . "DELETED")
                                             (object . ,(funcall table "b"))))
                           "\n")
                   'utf-8))
         (reply (concat "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                        (ek9-test--chunked payload 7)
                        "0\r\n\r\n")))
    (unwind-protect
        (progn
          (dolist (b (list buffer body))
            (with-current-buffer b (set-buffer-multibyte nil)))
          (setf (ek9-proxy-request-body request) body)
          ;; The data arrives one byte at a time.
          (dotimes (i (length reply))
            (ek9-proxy--watch-receive request buffer (substring reply i (1+ i))))
          (setq events (nreverse events))
          (should (equal (mapcar #'car events) '("ADDED" "DELETED")))
          (let ((row (aref (alist-get 'rows (cdar events)) 0)))
            ;; The cells are exact, and the objects are in the normal form.
            (should (equal (alist-get 'cells row) ["café ✓"]))
            (should (equal (ek9-resource-item-name (alist-get 'object row)) "café ✓")))
          (should (ek9-proxy-request-ended request))
          ;; The last CR LF arrives alone.  It is not an error.
          (should-not end)
          (should (ek9-proxy-live-p request)))
      (kill-buffer buffer)
      (kill-buffer body))))

(ert-deftest ek9-proxy-watch-bad-chunk-ends-watch ()
  (let* (end
         (request (ek9-proxy--make-request
                   :state 'running :on-event #'ignore
                   :on-error (lambda (code text) (setq end (list code text)))))
         (buffer (generate-new-buffer " *ek9-test-http*" t))
         (body (generate-new-buffer " *ek9-test-body*" t)))
    (unwind-protect
        (progn
          (dolist (b (list buffer body))
            (with-current-buffer b (set-buffer-multibyte nil)))
          (setf (ek9-proxy-request-body request) body)
          ;; A part of a chunk size is not an error.
          (ek9-proxy--watch-receive
           request buffer "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n1")
          (should-not end)
          ;; The data of a chunk must end with CR LF.
          (ek9-proxy--watch-receive request buffer "\r\nXYZ")
          (should (equal end '(nil "Bad reply from kubectl proxy: Bad chunk in the watch")))
          (should-not (ek9-proxy-live-p request)))
      (kill-buffer buffer)
      (kill-buffer body))))

(ert-deftest ek9-proxy-idle-stop-keeps-proxy-with-watch ()
  (ek9-test--with-watch-dir dir
    (let* ((result (ek9-test--proxy-watch "dev" "/api/v1/pods" "1"))
           (key (ek9-test--with-fake-kubectl (ek9-proxy--key "dev")))
           (proxy nil))
      (ek9-test--wait-until (lambda () (ek9-test--watch-requests dir)) 10)
      (setq proxy (gethash key ek9-proxy--proxies))
      (setf (ek9-proxy-last-used proxy) (- (float-time) ek9-proxy--idle-time 1))
      ;; A new proxy for another context does not stop the proxy with
      ;; the watch.
      (should (eq (car (ek9-test--proxy-get "prod" "/api/v1/pods")) 'ok))
      (should (eq (gethash key ek9-proxy--proxies) proxy))
      (should (process-live-p (ek9-proxy-process proxy)))
      (should-not (cdr result)))))

(defun ek9-test--proxy-watch (context path version)
  "Start a watch of PATH from VERSION through the proxy of CONTEXT.
Return a cons cell (EVENTS . END).  The watch pushes each event as
\(TYPE . OBJECT) to EVENTS, and sets END to (CODE TEXT) at the end."
  (let ((result (cons nil nil)))
    (ek9-test--with-fake-kubectl
      (ek9-proxy-watch context path version
                       (lambda (type object)
                         (setcar result (append (car result)
                                                (list (cons type object)))))
                       (lambda (code text) (setcdr result (list code text)))))
    result))

(ert-deftest ek9-proxy-watch-events-from-proxy ()
  (ek9-test--with-watch-dir dir
    (let ((result (ek9-test--proxy-watch
                   "dev" "/api/v1/namespaces/a/pods?includeObject=Object" "42")))
      (ek9-test--wait-until (lambda () (ek9-test--watch-requests dir)) 10)
      (should (string-suffix-p
               "/pods?includeObject=Object&watch=1&allowWatchBookmarks=true&resourceVersion=42&timeoutSeconds=300"
               (car (ek9-test--watch-requests dir))))
      (let ((pod (let ((ek9-fake-proxy--dir
                        (file-name-directory ek9-test--fake-kubectl)))
                   (ek9-fake-proxy--read "pods-web-1.json"))))
        (ek9-test--put-watch-file dir "1.json" (ek9-test--watch-event "ADDED" pod "102"))
        (ek9-test--put-watch-file dir "2.json" (ek9-test--watch-event "MODIFIED" pod "103"))
        (ek9-test--put-watch-file dir "3.json" (ek9-test--watch-event "DELETED" pod "104")))
      (ek9-test--wait-until (lambda () (= (length (car result)) 3)))
      (pcase-let ((`((,type1 . ,table1) (,type2 . ,table2) (,type3 . ,_)) (car result)))
        (should (equal (list type1 type2 type3) '("ADDED" "MODIFIED" "DELETED")))
        ;; Only the first table has the column definitions of the list.
        (should (equal (aref (ek9-proxy-table-definitions table1) 1)
                       (aref (ek9-proxy-table-definitions (ek9-test--table "pods")) 1)))
        (should-not (ek9-proxy-table-definitions table2))
        (should (equal (alist-get 'resourceVersion (alist-get 'metadata table2)) "103"))
        (should (equal (ek9-resource-item-name
                        (car (ek9-proxy-table-items
                              table2 (ek9-proxy-table-definitions table1))))
                       "web-1")))
      (should-not (cdr result))
      (ek9-test--put-watch-file dir "4.close")
      (ek9-test--wait-until (lambda () (cdr result)))
      (should (equal (cdr result) '(nil nil))))))

(ert-deftest ek9-proxy-watch-410-event ()
  (ek9-test--with-watch-dir dir
    (let ((result (ek9-test--proxy-watch "dev" "/api/v1/pods" "1")))
      (ek9-test--wait-until (lambda () (ek9-test--watch-requests dir)) 10)
      (ek9-test--put-watch-file dir "1.gone")
      (ek9-test--wait-until (lambda () (cdr result)))
      (should (equal (caar (car result)) "ERROR"))
      (should (eql (alist-get 'code (cdar (car result))) 410))
      (should (equal (cdr result) '(nil nil))))))

(ert-deftest ek9-proxy-watch-lost-connection ()
  (ek9-test--with-watch-dir dir
    (let ((result (ek9-test--proxy-watch "dev" "/api/v1/pods" "1")))
      (ek9-test--wait-until (lambda () (ek9-test--watch-requests dir)) 10)
      (ek9-test--put-watch-file dir "1.cut")
      (ek9-test--wait-until (lambda () (cdr result)))
      (should (equal (cdr result) '(nil "kubectl proxy closed the watch"))))))

(ert-deftest ek9-proxy-watch-error-status ()
  (ek9-test--with-watch-dir dir
    (ek9-test--put-watch-file dir "fail")
    (let ((result (ek9-test--proxy-watch "dev" "/api/v1/pods" "1")))
      (ek9-test--wait-until (lambda () (cdr result)) 10)
      (should-not (car result))
      (should (equal (cdr result) '(500 "Error from server: fake watch failure"))))))

(ert-deftest ek9-proxy-watch-cancel-calls-nothing ()
  (ek9-test--with-watch-dir dir
    (let* ((called nil)
           (request (ek9-test--with-fake-kubectl
                      (ek9-proxy-watch "dev" "/api/v1/pods" "1"
                                       (lambda (&rest _) (setq called t))
                                       (lambda (&rest _) (setq called t))))))
      (ek9-test--wait-until (lambda () (ek9-test--watch-requests dir)) 10)
      (should (ek9-proxy-live-p request))
      (ek9-proxy-cancel request)
      (should-not (ek9-proxy-live-p request))
      (ek9-test--put-watch-file dir "1.close")
      (accept-process-output nil 0.3)
      (should-not called)
      ;; The cancel kills the buffers of the watch.
      (should-not (buffer-live-p (ek9-proxy-request-body request))))))

(provide 'ek9-proxy-test)

;;; ek9-proxy-test.el ends here
