;;; fake-proxy.el --- Fake kubectl proxy for ek9 unit tests -*- lexical-binding: t; -*-

;;; Commentary:

;; The fake kubectl runs this file for "kubectl proxy".  It serves the
;; JSON fixtures over HTTP on a Unix socket:
;;
;;   emacs -Q --batch -l fake-proxy.el -f ek9-fake-proxy-main \
;;     CONTEXT SOCKET FIXTURE-DIRECTORY
;;
;; A list path, for example /api/v1/namespaces/team-a/pods, gives the
;; fixture PLURAL.VERSION.GROUP.json or PLURAL.json.  An object path,
;; for example /api/v1/namespaces/team-a/pods/web-1, gives the fixture
;; PLURAL-NAME.json.  The fake ignores the namespace and the query, as
;; the fake kubectl does.
;;
;; With the Accept header of the server table, the reply is a table.
;; If the fixture BASE.table.json exists, the table has its column
;; definitions and the cells of its rows.  Else the table has one
;; column "Name".  The objects of the rows come from the list fixture.
;;
;; A path that contains "ek9-missing" gives a 404 Status.  A path that
;; contains "ek9-slow" gets no reply.  "ek9-502" gives a 502 Status, as
;; kubectl proxy gives when it cannot reach the API server.
;; "ek9-badjson" gives status 200 with a body that is not JSON.  The proxy
;; sends each list in small chunks, so that the tests use the chunked
;; transfer encoding.  A table has the resourceVersion "100".
;;
;; In the context "no-metrics", a path of the metrics API gives a 404
;; Status, as in a cluster without metrics-server.
;;
;; A path with "watch=1" is a watch.  Without the environment variable
;; EK9_FAKE_WATCH_DIR, the watch sends no events and stays open.  With
;; it, the proxy adds each watch path to the file "requests" in that
;; directory.  If the file "fail" is in the directory, each watch gets
;; a 500 Status.  Else the proxy reads the files in the directory in
;; the order of their names, sends each one to the oldest open watch,
;; and deletes it:
;;
;;   NAME.json   An event {"type": TYPE, "object": OBJECT}.  OBJECT
;;               is a plain object.  The proxy sends a table with one
;;               row for it.  The optional key "cells" gives the cells
;;               of the row, else the row has the name.  The optional
;;               key "rv" gives the resourceVersion of the table.  The
;;               first event of a watch has the column definitions of
;;               the list.
;;   NAME.gone   An ERROR event with code 410.  Then the watch ends.
;;   NAME.close  The watch ends normally.
;;   NAME.cut    The connection closes without the last chunk.

;;; Code:

(defvar ek9-fake-proxy--dir nil
  "Directory of the fixtures.")

(defvar ek9-fake-proxy--context nil
  "Context of the proxy.")

(defvar ek9-fake-proxy--watch-dir nil
  "Directory with the watch events of the test, or nil.")

(defvar ek9-fake-proxy--watches nil
  "Open watches, oldest first.
Each entry is (PROCESS BASE . FIRST).  BASE is the fixture name of the
list.  FIRST is non-nil until the watch sent its first table.")

(defun ek9-fake-proxy--read (name)
  "Return the exact JSON of the fixture NAME, or nil if it is missing."
  (let ((file (expand-file-name name ek9-fake-proxy--dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (json-parse-buffer :object-type 'alist :array-type 'array
                           :null-object :null :false-object :false)))))

(defun ek9-fake-proxy--parse-path (path)
  "Return (PLURAL VERSION GROUP NAME) for the API PATH."
  (let* ((parts (split-string (car (split-string path "?")) "/" t))
         (group "") version)
    (pcase (pop parts)
      ("api" (setq version (pop parts)))
      ("apis" (setq group (pop parts) version (pop parts))))
    ;; "namespaces/NS/PLURAL" is a namespaced path.  "namespaces" alone
    ;; is the list of namespaces.
    (when (and (equal (car parts) "namespaces") (cddr parts))
      (setq parts (cddr parts)))
    (list (nth 0 parts) version group (nth 1 parts))))

(defun ek9-fake-proxy--table (list base)
  "Return the server table for the kubectl LIST of the fixture BASE.
LIST is in the exact JSON form.  BASE is nil or the name of a fixture
without \".json\"."
  (let* ((objects (alist-get 'items list))
         (table (and base (ek9-fake-proxy--read (concat base ".table.json"))))
         (rows (and table (alist-get 'rows table))))
    `((kind . "Table")
      (apiVersion . "meta.k8s.io/v1")
      (metadata (resourceVersion . "100"))
      (columnDefinitions
       . ,(if table
              (alist-get 'columnDefinitions table)
            [((name . "Name") (type . "string") (format . "name")
              (priority . 0))]))
      (rows
       . ,(vconcat
           (seq-map-indexed
            (lambda (object index)
              `((cells . ,(if rows
                              (alist-get 'cells (aref rows index))
                            (vector (alist-get 'name
                                               (alist-get 'metadata object)))))
                (object . ,object)))
            objects))))))

(defun ek9-fake-proxy--reply (path accept)
  "Return (STATUS . BODY) for a GET of PATH with the ACCEPT header."
  (pcase-let ((`(,plural ,version ,group ,name)
               (ek9-fake-proxy--parse-path path)))
    (let* ((base (if (string-empty-p group)
                     plural
                   (format "%s.%s.%s" plural version group)))
           (list (or (ek9-fake-proxy--read (concat base ".json"))
                     (ek9-fake-proxy--read (concat plural ".json"))))
           (base (if (ek9-fake-proxy--read (concat base ".json")) base plural))
           (object (and name (ek9-fake-proxy--read
                              (format "%s-%s.json" plural name)))))
      (cond
       ((or (string-search "ek9-missing" path)
            (and name (not object))
            (and (not name) (not list)))
        (cons 404 `((kind . "Status") (status . "Failure") (reason . "NotFound")
                    (message . ,(format "%s \"%s\" not found" plural (or name "")))
                    (code . 404))))
       (name (cons 200 object))
       ((string-search "as=Table" accept)
        (cons 200 (ek9-fake-proxy--table list base)))
       (t (cons 200 list))))))

(defun ek9-fake-proxy--send (process status body)
  "Send the reply with STATUS and the JSON BODY to PROCESS, then close it."
  (let ((text (json-serialize body :null-object :null :false-object :false)))
    (if (/= status 200)
        (process-send-string
         process (format "HTTP/1.1 %d Error\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
                         status (length text) text))
      (process-send-string
       process "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nTransfer-Encoding: chunked\r\n\r\n")
      (let ((start 0))
        (while (< start (length text))
          (let ((chunk (substring text start (min (length text) (+ start 700)))))
            (process-send-string process (format "%x\r\n%s\r\n" (length chunk) chunk))
            (setq start (+ start 700)))))
      (process-send-string process "0\r\n\r\n"))
    (delete-process process)))

;;;; Watch

(defun ek9-fake-proxy--base (path)
  "Return the fixture name of the list PATH without \".json\", or nil."
  (pcase-let ((`(,plural ,version ,group ,_) (ek9-fake-proxy--parse-path path)))
    (let ((base (format "%s.%s.%s" plural version group)))
      (cond ((and (not (string-empty-p group))
                  (file-exists-p (expand-file-name (concat base ".json")
                                                   ek9-fake-proxy--dir)))
             base)
            ((file-exists-p (expand-file-name (concat plural ".json")
                                              ek9-fake-proxy--dir))
             plural)))))

(defun ek9-fake-proxy--send-chunked (process text)
  "Send TEXT to PROCESS in chunks of 50 bytes.
The small chunks cut each event in many parts."
  (let ((start 0))
    (while (< start (length text))
      (let ((chunk (substring text start (min (length text) (+ start 50)))))
        (process-send-string process (format "%x\r\n%s\r\n" (length chunk) chunk))
        (setq start (+ start 50))))))

(defun ek9-fake-proxy--send-event (process type object)
  "Send the watch event with TYPE and OBJECT to PROCESS."
  (ek9-fake-proxy--send-chunked
   process
   (concat (json-serialize `((type . ,type) (object . ,object))
                           :null-object :null :false-object :false)
           "\n")))

(defun ek9-fake-proxy--event-table (watch event)
  "Return the table for the EVENT of WATCH, see `ek9-fake-proxy--watches'."
  (let* ((object (alist-get 'object event))
         (base (cadr watch))
         (definitions
          (if (and (cddr watch) base)
              (alist-get 'columnDefinitions
                         (ek9-fake-proxy--table
                          (ek9-fake-proxy--read (concat base ".json")) base))
            [])))
    (setcdr (cdr watch) nil)
    `((kind . "Table")
      (apiVersion . "meta.k8s.io/v1")
      (metadata (resourceVersion . ,(or (alist-get 'rv event) "101")))
      (columnDefinitions . ,definitions)
      (rows . [((cells . ,(or (alist-get 'cells event)
                              (vector (alist-get 'name (alist-get 'metadata object)))))
                (object . ,object))]))))

(defun ek9-fake-proxy--end (process)
  "Send the last chunk to PROCESS and close it."
  (process-send-string process "0\r\n\r\n")
  (delete-process process))

(defun ek9-fake-proxy--watch-poll ()
  "Send the files of the watch directory to the open watches."
  (setq ek9-fake-proxy--watches
        (seq-filter (lambda (watch) (process-live-p (car watch)))
                    ek9-fake-proxy--watches))
  (when (and ek9-fake-proxy--watch-dir ek9-fake-proxy--watches)
    (dolist (file (directory-files ek9-fake-proxy--watch-dir t
                                   "\\`[^.].*\\.\\(json\\|gone\\|close\\|cut\\)\\'"))
      (when-let* ((watch (seq-find (lambda (watch) (process-live-p (car watch)))
                                   ek9-fake-proxy--watches)))
        (let ((process (car watch)))
          (pcase (file-name-extension file)
            ("json"
             (let ((event (ek9-fake-proxy--read file)))
               (ek9-fake-proxy--send-event process (alist-get 'type event)
                                           (ek9-fake-proxy--event-table watch event))))
            ("gone"
             (ek9-fake-proxy--send-event
              process "ERROR"
              '((kind . "Status") (apiVersion . "v1") (metadata)
                (status . "Failure") (reason . "Expired") (code . 410)
                (message . "too old resource version: 1 (60804)")))
             (ek9-fake-proxy--end process))
            ("close" (ek9-fake-proxy--end process))
            ("cut" (delete-process process)))
          (delete-file file))))))

(defun ek9-fake-proxy--watch (process path)
  "Start the watch of PATH on PROCESS.  See the Commentary."
  (when ek9-fake-proxy--watch-dir
    (write-region (concat path "\n") nil
                  (expand-file-name "requests" ek9-fake-proxy--watch-dir)
                  t 'silent))
  (if (and ek9-fake-proxy--watch-dir
           (file-exists-p (expand-file-name "fail" ek9-fake-proxy--watch-dir)))
      (ek9-fake-proxy--send process 500
                            '((kind . "Status") (status . "Failure")
                              (message . "fake watch failure") (code . 500)))
    (process-send-string
     process "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nTransfer-Encoding: chunked\r\n\r\n")
    (setq ek9-fake-proxy--watches
          (append ek9-fake-proxy--watches
                  (list (cons process (cons (ek9-fake-proxy--base path) t)))))))

(defun ek9-fake-proxy--filter (process text)
  "Read the request TEXT from PROCESS and answer it."
  (let ((request (concat (or (process-get process 'request) "") text)))
    (process-put process 'request request)
    (when (string-search "\r\n\r\n" request)
      (let ((path (and (string-match "\\`GET \\([^ ]+\\) HTTP" request)
                       (match-string 1 request)))
            (accept (if (string-match "^Accept: \\([^\r]*\\)" request)
                        (match-string 1 request)
                      "")))
        (cond
         ((string-search "ek9-slow" path))
         ((and (equal ek9-fake-proxy--context "no-metrics")
               (string-search "/metrics.k8s.io/" path))
          (ek9-fake-proxy--send process 404
                                '((kind . "Status") (status . "Failure")
                                  (reason . "NotFound") (code . 404)
                                  (message . "the server could not find the requested resource"))))
         ((string-search "watch=1" path)
          (ek9-fake-proxy--watch process path))
         ((string-search "ek9-502" path)
          (ek9-fake-proxy--send process 502
                                '((kind . "Status") (status . "Failure")
                                  (message . "dial tcp 127.0.0.1:1: connect: connection refused")
                                  (code . 502))))
         ((string-search "ek9-badjson" path)
          (process-send-string
           process "HTTP/1.1 200 OK\r\nContent-Length: 8\r\n\r\nnot json")
          (delete-process process))
         (t
          (pcase-let ((`(,status . ,body) (ek9-fake-proxy--reply path accept)))
            (ek9-fake-proxy--send process status body))))))))

(defun ek9-fake-proxy-main ()
  "Serve the fixtures.  See the Commentary."
  (let ((socket (nth 1 command-line-args-left)))
    (setq ek9-fake-proxy--context (nth 0 command-line-args-left)
          ek9-fake-proxy--dir (nth 2 command-line-args-left)
          ek9-fake-proxy--watch-dir (getenv "EK9_FAKE_WATCH_DIR")
          command-line-args-left nil)
    (run-with-timer 0.05 0.05 #'ek9-fake-proxy--watch-poll)
    (make-network-process :name "ek9-fake-proxy" :server t
                          :family 'local :service socket
                          :coding 'binary :noquery t
                          :filter #'ek9-fake-proxy--filter)
    ;; `princ' output waits in a buffer.  This output does not.
    (send-string-to-terminal (format "Starting to serve on %s\n" socket))
    (while t
      (accept-process-output nil 1))))

;;; fake-proxy.el ends here
