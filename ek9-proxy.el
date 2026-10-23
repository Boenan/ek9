;;; ek9-proxy.el --- Read lists through kubectl proxy for ek9 -*- lexical-binding: t; -*-

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

;; ek9 reads lists through "kubectl proxy".  All other commands, for
;; example describe, delete, and logs, run kubectl directly.
;;
;; Why a proxy: each kubectl command starts a process, reads the
;; kubeconfig, and opens a new TLS connection to the API server.  This
;; takes 80 to 300 ms for each list read.  A proxy that stays open
;; does these steps one time.  A read through the proxy takes 5 to
;; 50 ms.  The proxy also gives the server table, the same table that
;; "kubectl get" prints.  Thus, ek9 shows the same columns as kubectl
;; for each type, without code for each type.  kubectl still does the
;; authentication, so all kubeconfig types work.
;;
;; ek9 starts one proxy for each context and environment, when a list
;; first needs it.  The proxy listens on a Unix socket in a new
;; directory that only the user can open.  A TCP port would give the
;; permissions of the user to all local processes.  If a proxy stops,
;; the next request starts a new one.  ek9 stops a proxy that got no
;; request for some minutes, and all proxies when Emacs exits.
;;
;; The HTTP client is small: it sends one GET request for each
;; connection, with "Connection: close", and reads the reply until
;; the proxy closes the connection.
;;
;; A watch is a GET request that stays open.  The API server sends
;; each change of the list as one line of JSON, an event.  ek9 reads
;; the events while they arrive.  In a server table, each event has a
;; table with one row.  Only the first event has the column
;; definitions.
;;
;; The server calculates some cells from a time in the object, for
;; example AGE.  The text of such a cell becomes old while a watch
;; sends no change for the row.  Thus, ek9 calculates these cells
;; again from the time in the object, with the same format as kubectl.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'ek9-process)
(require 'ek9-resource)

(defconst ek9-proxy-table-accept
  "application/json;as=Table;v=v1;g=meta.k8s.io"
  "Accept header that requests the server table.")

(cl-defstruct (ek9-proxy (:constructor ek9-proxy--make)
                         (:copier nil))
  "A \"kubectl proxy\" process for one context."
  key context process socket directory
  (last-used nil :documentation "The `float-time' of the last request.")
  (state 'starting :documentation "One of starting, ready, or dead.")
  (waiters nil :documentation "Functions to call when the proxy is ready.
Each function takes nil, or an error text if the start failed.")
  (stdout "" :documentation "Standard output until the proxy is ready.")
  (stderr "" :documentation "The last part of the standard error output.")
  (watches nil :documentation "The watch requests that use the proxy."))

(cl-defstruct (ek9-proxy-request (:constructor ek9-proxy--make-request)
                                 (:copier nil))
  "An HTTP GET request through a proxy."
  path accept on-success on-error exact process timer proxy
  (state 'waiting :documentation "One of waiting, running, done, or cancelled.")
  (on-event nil :documentation "Function for each event of a watch.
The value is nil for a request that is not a watch.")
  (body nil :documentation "Buffer with the body of a watch that ek9 did not read.")
  (status nil :documentation "HTTP status of a watch after its headers arrived.")
  (chunked nil :documentation "Non-nil if the body of a watch uses chunks.")
  (ended nil :documentation "Non-nil if the last chunk of a watch arrived."))

(defvar ek9-proxy--proxies (make-hash-table :test #'equal)
  "Hash table that maps a proxy key to its `ek9-proxy'.")

(defconst ek9-proxy--idle-time 300
  "Seconds after which ek9 stops a proxy that got no request.
Each new environment, for example from direnv, gets its own proxy.
This limit stops the old ones.")

;;;; Proxy processes

(defun ek9-proxy--key (context)
  "Return the proxy key for CONTEXT in the current environment.
The proxy process gets the environment of its start.  Another
environment, for example another KUBECONFIG or AWS_PROFILE from
direnv, can give other credentials, so it needs its own proxy."
  (list context ek9-kubectl-executable process-environment exec-path))

(defun ek9-proxy--private-directory ()
  "Make a new directory that only the user can open, and return it."
  (with-file-modes #o700
    (make-temp-file (expand-file-name "ek9-proxy-"
                                      (or (getenv "XDG_RUNTIME_DIR")
                                          temporary-file-directory))
                    t)))

(defun ek9-proxy--finish-start (proxy error)
  "Mark PROXY as ready, or as dead if ERROR is non-nil.
Call the waiters of PROXY with ERROR."
  (when (eq (ek9-proxy-state proxy) 'starting)
    (setf (ek9-proxy-state proxy) (if error 'dead 'ready))
    (let ((waiters (reverse (ek9-proxy-waiters proxy))))
      (setf (ek9-proxy-waiters proxy) nil)
      (dolist (waiter waiters)
        (with-demoted-errors "ek9: Error in a proxy callback: %S"
          (funcall waiter error))))))

(defun ek9-proxy--cleanup (proxy)
  "Remove the socket directory of PROXY and forget PROXY."
  (when (eq (gethash (ek9-proxy-key proxy) ek9-proxy--proxies) proxy)
    (remhash (ek9-proxy-key proxy) ek9-proxy--proxies))
  (ignore-errors (delete-directory (ek9-proxy-directory proxy) t)))

(defun ek9-proxy--error-text (proxy)
  "Return the error text of PROXY after its process stopped."
  (let ((summary (ek9-process-error-summary (ek9-proxy-stderr proxy))))
    (if (string-empty-p summary)
        "kubectl proxy stopped"
      summary)))

(defun ek9-proxy--stop (proxy text)
  "Stop PROXY.  Call its waiters with the error TEXT."
  (ek9-proxy--finish-start proxy text)
  (setf (ek9-proxy-state proxy) 'dead)
  (when (process-live-p (ek9-proxy-process proxy))
    (delete-process (ek9-proxy-process proxy)))
  (ek9-proxy--cleanup proxy))

(defun ek9-proxy--retire (proxy)
  "Make new requests use a new proxy, not PROXY.
Stop PROXY after `ek9-process-timeout', so that the requests that use
it now can end."
  (when (eq (gethash (ek9-proxy-key proxy) ek9-proxy--proxies) proxy)
    (remhash (ek9-proxy-key proxy) ek9-proxy--proxies))
  (run-with-timer (or ek9-process-timeout 60) nil
                  #'ek9-proxy--stop proxy "kubectl proxy stopped"))

(defun ek9-proxy--stop-idle (now)
  "Stop the proxies that got no request since `ek9-proxy--idle-time'.
NOW is the current `float-time'."
  (dolist (proxy (hash-table-values ek9-proxy--proxies))
    ;; A watch of a quiet list can get no data for a long time.
    (setf (ek9-proxy-watches proxy)
          (seq-filter #'ek9-proxy-live-p (ek9-proxy-watches proxy)))
    (when (and (> (- now (ek9-proxy-last-used proxy)) ek9-proxy--idle-time)
               (null (ek9-proxy-watches proxy)))
      (ek9-proxy--stop proxy "kubectl proxy stopped"))))

(defun ek9-proxy--start (key context)
  "Start a proxy for CONTEXT with KEY and return it.
If CONTEXT is nil, use the current context of the kubeconfig."
  (let* ((directory (ek9-proxy--private-directory))
         ;; kubectl must run on this host, because the socket is here.
         (default-directory (if (file-remote-p default-directory)
                                (expand-file-name "~/")
                              default-directory))
         (socket (expand-file-name "proxy.sock" directory))
         (proxy (ek9-proxy--make :key key :context context
                                 :socket socket :directory directory))
         (stderr (make-pipe-process
                  :name "ek9-proxy-stderr" :noquery t
                  :coding 'utf-8
                  :filter (lambda (_process text)
                            ;; Keep only the end: a proxy can run for days.
                            (let ((all (concat (ek9-proxy-stderr proxy) text)))
                              (setf (ek9-proxy-stderr proxy)
                                    (substring all (max 0 (- (length all) 4000))))))))
         (process
          (condition-case err
              (make-process
               :name "ek9-kubectl-proxy"
               :command (append (list ek9-kubectl-executable "proxy")
                                (when context (list "--context" context))
                                (list (concat "--unix-socket=" socket)))
               :connection-type 'pipe
               :coding 'utf-8
               :noquery t
               :stderr stderr
               :filter (lambda (_process text)
                         (when (eq (ek9-proxy-state proxy) 'starting)
                           (setf (ek9-proxy-stdout proxy)
                                 (concat (ek9-proxy-stdout proxy) text))
                           ;; kubectl prints this line after the socket
                           ;; listens.
                           (when (string-match-p "^Starting to serve on "
                                                 (ek9-proxy-stdout proxy))
                             (ek9-proxy--finish-start proxy nil))))
               :sentinel (lambda (process _event)
                           (unless (process-live-p process)
                             ;; Read the error text that already
                             ;; arrived, without a wait.
                             (while (accept-process-output stderr 0 nil t))
                             (delete-process stderr)
                             (ek9-proxy--stop proxy (ek9-proxy--error-text proxy)))))
            (error (delete-process stderr)
                   (ignore-errors (delete-directory directory t))
                   (signal (car err) (cdr err))))))
    (setf (ek9-proxy-process proxy) process)
    (puthash key proxy ek9-proxy--proxies)
    ;; A proxy that hangs before it is ready, for example in a login
    ;; plugin, must not keep its waiters for ever.
    (run-with-timer (or ek9-process-timeout 60) nil
                    (lambda ()
                      (when (eq (ek9-proxy-state proxy) 'starting)
                        (ek9-proxy--stop
                         proxy (format "kubectl proxy did not start in %s s"
                                       (or ek9-process-timeout 60))))))
    proxy))

(defun ek9-proxy--ensure (context callback)
  "Call CALLBACK with the proxy of CONTEXT when it is ready.
Start the proxy if necessary.  If the proxy cannot start, call
CALLBACK with nil and the error text."
  (let* ((key (ek9-proxy--key context))
         (now (float-time))
         (proxy (gethash key ek9-proxy--proxies)))
    (unless (and proxy (process-live-p (ek9-proxy-process proxy)))
      (ek9-proxy--stop-idle now)
      (setq proxy (ek9-proxy--start key context)))
    (setf (ek9-proxy-last-used proxy) now)
    (if (eq (ek9-proxy-state proxy) 'ready)
        (funcall callback proxy nil)
      (push (lambda (error) (funcall callback (and (not error) proxy) error))
            (ek9-proxy-waiters proxy)))))

(defun ek9-proxy-stop-all ()
  "Stop all proxies and remove their sockets."
  (dolist (proxy (hash-table-values ek9-proxy--proxies))
    (ek9-proxy--stop proxy "kubectl proxy stopped"))
  (clrhash ek9-proxy--proxies))

(add-hook 'kill-emacs-hook #'ek9-proxy-stop-all)

;;;; HTTP

(defun ek9-proxy--dechunk ()
  "Decode the chunked body from point to the end of the buffer.
Return non-nil if the body is complete."
  (let (complete)
    (while (and (not complete)
                (looking-at "\\([0-9a-fA-F]+\\)[^\r\n]*\r\n"))
      (let ((size (string-to-number (match-string 1) 16)))
        (delete-region (match-beginning 0) (match-end 0))
        (cond ((zerop size)
               ;; Remove the trailer.
               (delete-region (point) (point-max))
               (setq complete t))
              ((> (+ (point) size 2) (1+ (point-max)))
               ;; The reply stopped in the chunk.
               (goto-char (point-max)))
              (t
               (forward-char size)
               (if (looking-at "\r\n")
                   (delete-region (point) (+ (point) 2))
                 (goto-char (point-max)))))))
    complete))

(defun ek9-proxy--status-text (status body)
  "Return the error text for the HTTP STATUS with BODY.
Use the same form as kubectl: \"Error from server (REASON): TEXT\"."
  (let ((object (ignore-errors
                  (json-parse-string body :object-type 'alist
                                     :null-object nil :false-object nil))))
    (if-let* (((equal (alist-get 'kind object) "Status"))
              (message (alist-get 'message object)))
        (if-let* ((reason (alist-get 'reason object))
                  ((not (string-empty-p reason))))
            (format "Error from server (%s): %s" reason message)
          (concat "Error from server: " message))
      (format "HTTP %d: %s" status
              (or (car (split-string body "\n" t "[ \t\r]+")) "")))))

(defun ek9-proxy--parse-reply (request)
  "Parse the HTTP reply of REQUEST in the current buffer.
Return (ok . JSON) or (error STATUS . TEXT)."
  (goto-char (point-min))
  (if (not (looking-at "HTTP/1\\.[01] \\([0-9]+\\)"))
      (cons 'error (cons nil (if (= (point-min) (point-max))
                                 "No reply from kubectl proxy"
                               "Not an HTTP reply from kubectl proxy")))
    (let ((status (string-to-number (match-string 1)))
          (complete t))
      (if (not (search-forward "\r\n\r\n" nil t))
          (cons 'error (cons nil "Incomplete reply from kubectl proxy"))
        (let ((headers (buffer-substring (point-min) (point)))
              (case-fold-search t))
          (delete-region (point-min) (point))
          (cond ((string-match "^Transfer-Encoding: *chunked" headers)
                 (setq complete (ek9-proxy--dechunk)))
                ((string-match "^Content-Length: *\\([0-9]+\\)" headers)
                 (setq complete (= (buffer-size)
                                   (string-to-number (match-string 1 headers)))))))
        (cond
         ((not complete)
          (cons 'error (cons nil "Incomplete reply from kubectl proxy")))
         ((not (<= 200 status 299))
          (cons 'error
                (cons status
                      (ek9-proxy--status-text
                       status (decode-coding-string (buffer-string) 'utf-8)))))
         (t
          (goto-char (point-min))
          (cons 'ok (pcase (ek9-proxy-request-exact request)
                      ('table (ek9-proxy-parse-table))
                      ('nil (json-parse-buffer :object-type 'alist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object nil))
                      (_ (json-parse-buffer :object-type 'alist
                                            :array-type 'array
                                            :null-object :null
                                            :false-object :false))))))))))

(defun ek9-proxy--finish (request buffer)
  "Call the callback of REQUEST with the reply in BUFFER."
  (when (timerp (ek9-proxy-request-timer request))
    (cancel-timer (ek9-proxy-request-timer request)))
  (unwind-protect
      (when (eq (ek9-proxy-request-state request) 'running)
        (setf (ek9-proxy-request-state request) 'done)
        (with-demoted-errors "ek9: Error in a proxy callback: %S"
          (pcase (condition-case err
                     (with-current-buffer buffer
                       (ek9-proxy--parse-reply request))
                   ;; Each failure must reach the caller, so that a list
                   ;; does not wait for ever.
                   (error
                    (cons 'error
                          (cons nil (format "Bad reply from kubectl proxy: %s"
                                            (error-message-string err))))))
            (`(ok . ,json) (funcall (ek9-proxy-request-on-success request) json))
            (`(error ,status . ,text)
             ;; kubectl proxy gives 502 when it cannot reach the API
             ;; server, for example after the cluster was made again
             ;; with a new port.  A new proxy reads the kubeconfig again.
             (when (and (eql status 502) (ek9-proxy-request-proxy request))
               (ek9-proxy--retire (ek9-proxy-request-proxy request)))
             (funcall (ek9-proxy-request-on-error request) status text)))))
    (kill-buffer buffer)))

(defun ek9-proxy--fail (request code text)
  "End REQUEST and call its error callback with CODE and TEXT."
  (when (memq (ek9-proxy-request-state request) '(waiting running))
    (setf (ek9-proxy-request-state request) 'done)
    (when (timerp (ek9-proxy-request-timer request))
      (cancel-timer (ek9-proxy-request-timer request)))
    (with-demoted-errors "ek9: Error in a proxy callback: %S"
      (funcall (ek9-proxy-request-on-error request) code text))))

(defun ek9-proxy--send (request proxy)
  "Send REQUEST to PROXY."
  (let* ((buffer (generate-new-buffer " *ek9-http*" t))
         (watch (ek9-proxy-request-on-event request))
         (process
          (condition-case err
              (make-network-process
               :name "ek9-http" :buffer buffer
               :family 'local :service (ek9-proxy-socket proxy)
               :coding 'binary :noquery t
               :filter (if watch
                           (lambda (_process text)
                             (ek9-proxy--watch-receive request buffer text))
                         #'internal-default-process-filter)
               :sentinel (lambda (process _event)
                           (unless (process-live-p process)
                             (if watch
                                 (ek9-proxy--watch-finish request buffer)
                               (ek9-proxy--finish request buffer)))))
            (error
             (kill-buffer buffer)
             ;; The socket is gone or broken.  The next request starts
             ;; a new proxy.
             (ek9-proxy--retire proxy)
             (ek9-proxy--fail request nil
                              (concat "Cannot connect to kubectl proxy: "
                                      (error-message-string err)))
             nil))))
    (when process
      (with-current-buffer buffer
        (set-buffer-multibyte nil))
      (when watch
        (let ((body (generate-new-buffer " *ek9-watch*" t)))
          (with-current-buffer body
            (set-buffer-multibyte nil))
          (setf (ek9-proxy-request-body request) body)
          (push request (ek9-proxy-watches proxy))))
      (setf (ek9-proxy-request-process request) process
            (ek9-proxy-request-proxy request) proxy
            (ek9-proxy-request-state request) 'running)
      (process-send-string
       process
       (concat "GET " (ek9-proxy-request-path request) " HTTP/1.1\r\n"
               "Host: localhost\r\n"
               "Accept: " (ek9-proxy-request-accept request) "\r\n"
               "User-Agent: ek9\r\n"
               "Connection: close\r\n\r\n")))))

(defun ek9-proxy-get (context path on-success on-error &optional accept exact)
  "Send a GET request for PATH through the proxy of CONTEXT.
Return the request.  Call ON-SUCCESS with the parsed JSON reply.
Objects become alists, arrays become lists, and null and false become
nil.  If EXACT is non-nil, arrays become vectors, null becomes
`:null', and false becomes `:false'.  If EXACT is `table', the reply
is a server table: see `ek9-proxy-parse-table'.  If the request
fails, call ON-ERROR with a code and an error text.  The code is the
HTTP status, the symbol `timeout' after `ek9-process-timeout', or
nil.  ACCEPT is the Accept header, by default \"application/json\"."
  (let ((request (ek9-proxy--make-request
                  :path path :accept (or accept "application/json")
                  :on-success on-success :on-error on-error :exact exact)))
    (when ek9-process-timeout
      (ek9-proxy--set-timeout request ek9-process-timeout
                              (format "Timeout after %s s" ek9-process-timeout)))
    (ek9-proxy--start-request request context)))

(defun ek9-proxy--set-timeout (request seconds text)
  "End REQUEST with the code `timeout' and TEXT after SECONDS."
  (setf (ek9-proxy-request-timer request)
        (run-with-timer
         seconds nil
         (lambda ()
           ;; End the request first: the sentinel of the stopped
           ;; connection must not report the reply as missing.
           (ek9-proxy--fail request 'timeout text)
           (when (process-live-p (ek9-proxy-request-process request))
             (delete-process (ek9-proxy-request-process request)))))))

(defun ek9-proxy--start-request (request context)
  "Send REQUEST through the proxy of CONTEXT and return REQUEST."
  (condition-case err
      (ek9-proxy--ensure
       context
       (lambda (proxy error)
         (when (eq (ek9-proxy-request-state request) 'waiting)
           (if proxy
               (ek9-proxy--send request proxy)
             (ek9-proxy--fail request nil error)))))
    (error (ek9-proxy--fail request nil (error-message-string err))))
  request)

(defun ek9-proxy-live-p (request)
  "Return non-nil if the state of REQUEST is waiting or running."
  (and (ek9-proxy-request-p request)
       (memq (ek9-proxy-request-state request) '(waiting running))))

(defun ek9-proxy-cancel (request)
  "Stop REQUEST.  Do not call its callbacks."
  (when (ek9-proxy-live-p request)
    (setf (ek9-proxy-request-state request) 'cancelled)
    (when (timerp (ek9-proxy-request-timer request))
      (cancel-timer (ek9-proxy-request-timer request)))
    (when (process-live-p (ek9-proxy-request-process request))
      (delete-process (ek9-proxy-request-process request)))))

;;;; Watch

(defconst ek9-proxy-watch-timeout 300
  "Seconds after which the API server ends a watch.
The server can end a watch earlier.  A new watch then continues from
the last resourceVersion.")

(defun ek9-proxy--take-chunks (body)
  "Move the complete chunks at the start of the current buffer to BODY.
Return non-nil if the last chunk arrived."
  (let ((more t) last)
    (goto-char (point-min))
    (while (and more (looking-at "\\([0-9a-fA-F]+\\)[^\r\n]*\r\n"))
      (let* ((size (string-to-number (match-string 1) 16))
             (start (match-end 0))
             (end (+ start size)))
        (cond ((zerop size)
               (delete-region (point-min) (point-max))
               (setq last t more nil))
              ((<= (+ end 2) (point-max))
               (unless (equal (buffer-substring-no-properties end (+ end 2)) "\r\n")
                 (error "Bad chunk in the watch"))
               (let ((data (buffer-substring-no-properties start end)))
                 (delete-region (point-min) (+ end 2))
                 (with-current-buffer body
                   (goto-char (point-max))
                   (insert data))))
              (t (setq more nil)))))
    ;; The rest must be the start of a chunk.
    (unless (or last
                (looking-at-p "[0-9a-fA-F]+[^\r\n]*\r\n")
                (looking-at-p "[0-9a-fA-F]*\\(?:;[^\r\n]*\\)?\r?\\'"))
      (error "Bad chunk in the watch"))
    last))

(defun ek9-proxy--parse-event ()
  "Parse the watch event in the current buffer.
Return (TYPE . OBJECT).  If OBJECT is a server table, it has the form
of `ek9-proxy-parse-table'.  Else OBJECT is in the exact JSON form."
  (let* ((event (json-parse-buffer :object-type 'alist :array-type 'array
                                   :null-object :null :false-object :false))
         (object (alist-get 'object event)))
    (when (equal (alist-get 'kind object) "Table")
      (goto-char (point-min))
      (ek9-proxy--use-plain-objects
       object
       (alist-get 'object (json-parse-buffer :object-type 'alist
                                             :array-type 'list
                                             :null-object nil
                                             :false-object nil))))
    (cons (alist-get 'type event) object)))

(defun ek9-proxy--next-event (body)
  "Remove the first complete event from the buffer BODY and parse it.
Return (TYPE . OBJECT), see `ek9-proxy--parse-event', or nil if no
complete event is in BODY.  Signal an error if the event is not JSON."
  (with-current-buffer body
    (let (event)
      (goto-char (point-min))
      (while (and (not event) (search-forward "\n" nil t))
        (let ((end (point)))
          (unwind-protect
              (save-restriction
                (narrow-to-region (point-min) (1- end))
                (goto-char (point-min))
                ;; Skip an empty line.
                (unless (looking-at-p "[ \t\r]*\\'")
                  (setq event (ek9-proxy--parse-event))))
            (delete-region (point-min) end))))
      event)))

(defun ek9-proxy--watch-headers (request)
  "Read the HTTP headers of the watch REQUEST in the current buffer.
If they are complete, set the status of REQUEST.  For a status from
200 to 299, remove the headers.  Keep another reply complete for
`ek9-proxy--parse-reply'."
  (goto-char (point-min))
  (when-let* ((status (and (looking-at "HTTP/1\\.[01] \\([0-9]+\\)")
                           (string-to-number (match-string 1))))
              ((search-forward "\r\n\r\n" nil t)))
    (when (<= 200 status 299)
      (let ((headers (buffer-substring (point-min) (point)))
            (case-fold-search t))
        (setf (ek9-proxy-request-chunked request)
              (string-match-p "^Transfer-Encoding: *chunked" headers))
        (delete-region (point-min) (point))))
    (setf (ek9-proxy-request-status request) status)))

(defun ek9-proxy--watch-read (request buffer)
  "Read the new data of the watch REQUEST in BUFFER.
Call the event function of REQUEST for each complete event."
  (let ((body (ek9-proxy-request-body request))
        (on-event (ek9-proxy-request-on-event request)))
    (cl-flet ((bad-reply (err)
                (ek9-proxy--fail request nil
                                 (format "Bad reply from kubectl proxy: %s"
                                         (error-message-string err)))
                (when (process-live-p (ek9-proxy-request-process request))
                  (delete-process (ek9-proxy-request-process request)))
                nil))
      (condition-case err
          (with-current-buffer buffer
            (unless (ek9-proxy-request-status request)
              (ek9-proxy--watch-headers request))
            (when-let* ((status (ek9-proxy-request-status request))
                        ((<= 200 status 299)))
              (cond
               ;; Ignore the data after the last chunk, for example
               ;; the last CR LF in a later packet.
               ((ek9-proxy-request-ended request) (erase-buffer))
               ((ek9-proxy-request-chunked request)
                (when (ek9-proxy--take-chunks body)
                  (setf (ek9-proxy-request-ended request) t)))
               (t
                (let ((data (buffer-string)))
                  (erase-buffer)
                  (with-current-buffer body
                    (goto-char (point-max))
                    (insert data)))))))
        (error (bad-reply err)))
      (let (event)
        (while (and (eq (ek9-proxy-request-state request) 'running)
                    (buffer-live-p body)
                    (setq event (condition-case err
                                    (ek9-proxy--next-event body)
                                  (error (bad-reply err)))))
          (with-demoted-errors "ek9: Error in a watch callback: %S"
            (funcall on-event (car event) (cdr event))))))))

(defun ek9-proxy--watch-receive (request buffer text)
  "Add TEXT from the connection of the watch REQUEST to BUFFER, and read it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert text))
    ;; A proxy with a watch is in use.  It must not stop as idle.
    (when-let* ((proxy (ek9-proxy-request-proxy request)))
      (setf (ek9-proxy-last-used proxy) (float-time)))
    (ek9-proxy--watch-read request buffer)))

(defun ek9-proxy--watch-finish (request buffer)
  "End the watch REQUEST after its connection closed.
BUFFER has the part of the reply that ek9 did not read."
  (when (timerp (ek9-proxy-request-timer request))
    (cancel-timer (ek9-proxy-request-timer request)))
  (unwind-protect
      (when (eq (ek9-proxy-request-state request) 'running)
        (setf (ek9-proxy-request-state request) 'done)
        (with-demoted-errors "ek9: Error in a watch callback: %S"
          (let ((status (ek9-proxy-request-status request)))
            (if (and status (<= 200 status 299))
                (funcall (ek9-proxy-request-on-error request)
                         nil
                         (unless (or (ek9-proxy-request-ended request)
                                     (not (ek9-proxy-request-chunked request)))
                           "kubectl proxy closed the watch"))
              (pcase (condition-case err
                         (with-current-buffer buffer
                           (ek9-proxy--parse-reply request))
                       (error
                        (cons 'error
                              (cons nil (format "Bad reply from kubectl proxy: %s"
                                                (error-message-string err))))))
                (`(error ,code . ,text)
                 ;; See `ek9-proxy--finish'.
                 (when (and (eql code 502) (ek9-proxy-request-proxy request))
                   (ek9-proxy--retire (ek9-proxy-request-proxy request)))
                 (funcall (ek9-proxy-request-on-error request) code text))
                (_
                 (funcall (ek9-proxy-request-on-error request)
                          nil "Bad reply from kubectl proxy")))))))
    (kill-buffer buffer)
    (when (buffer-live-p (ek9-proxy-request-body request))
      (kill-buffer (ek9-proxy-request-body request)))))

(defun ek9-proxy-watch (context path resource-version on-event on-end)
  "Watch the server table of the list PATH through the proxy of CONTEXT.
PATH is a list path, see `ek9-proxy-path'.  The watch sends the
changes after RESOURCE-VERSION, the resourceVersion of the list.
Return the request.  `ek9-proxy-live-p' and `ek9-proxy-cancel' accept
it.

Call ON-EVENT with TYPE and OBJECT for each event.  TYPE is \"ADDED\",
\"MODIFIED\", \"DELETED\", \"BOOKMARK\", or \"ERROR\".  For \"ERROR\",
OBJECT is a Status in the exact JSON form.  Else OBJECT is a server
table with one row, see `ek9-proxy-parse-table'.  Only the first table
has column definitions.  The row of a \"BOOKMARK\" table is empty: use
only the resourceVersion in the metadata of the table.

When the watch ends, call ON-END with a code and an error text, as for
the ON-ERROR function of `ek9-proxy-get'.  Both are nil if the server
ended the watch normally, for example after
`ek9-proxy-watch-timeout'."
  (let ((request (ek9-proxy--make-request
                  :path (concat path
                                (if (string-search "?" path) "&" "?")
                                "watch=1&allowWatchBookmarks=true"
                                "&resourceVersion=" (url-hexify-string resource-version)
                                (format "&timeoutSeconds=%d" ek9-proxy-watch-timeout))
                  :accept ek9-proxy-table-accept
                  :on-success #'ignore :on-error on-end :on-event on-event)))
    ;; The server ends the watch after its timeout.  This timer ends a
    ;; watch that hangs, for example after a network failure.
    (ek9-proxy--set-timeout request (+ ek9-proxy-watch-timeout 30)
                            "The watch got no reply")
    (ek9-proxy--start-request request context)))

;;;; API paths

(defun ek9-proxy-path (api namespace &optional name query)
  "Return the API path for the type API.
API is (GROUP VERSION PLURAL).  GROUP is \"\" for the core group.
NAMESPACE is a namespace name, or nil for all namespaces or for a type
that is not in a namespace.  NAME is an object name, or nil for a
list.  QUERY is an alist of (KEY . VALUE) strings for the query."
  (pcase-let ((`(,group ,version ,plural) api))
    (concat (if (string-empty-p group)
                (concat "/api/" version)
              (concat "/apis/" group "/" version))
            (when namespace
              (concat "/namespaces/" (url-hexify-string namespace)))
            "/" plural
            (when name
              (concat "/" (url-hexify-string name)))
            (when query
              (concat "?" (mapconcat (lambda (entry)
                                       (concat (car entry) "="
                                               (url-hexify-string (cdr entry))))
                                     query "&"))))))

;;;; Server tables

(defun ek9-proxy--vector (value)
  "Return VALUE if it is a sequence, else an empty vector.
An API server can send null for an empty list."
  (if (sequencep value) value []))

(defun ek9-proxy-parse-table ()
  "Parse the server table in the current buffer, from point.
Return the table in the exact JSON form, see `ek9-proxy-get', but
with the object of each row in the normal ek9 form.  The cells need
the exact form, because kubectl prints false and null differently.
The objects need the normal form for the rest of ek9.  Two parses in
C are much faster than a conversion of the objects in Lisp."
  (let* ((start (point))
         (table (json-parse-buffer :object-type 'alist :array-type 'array
                                   :null-object :null :false-object :false))
         (plain (progn
                  (goto-char start)
                  (json-parse-buffer :object-type 'alist :array-type 'list
                                     :null-object nil :false-object nil))))
    (ek9-proxy--use-plain-objects table plain)))

(defun ek9-proxy--use-plain-objects (table plain)
  "Put the row objects of PLAIN in the rows of TABLE, and return TABLE.
TABLE is a server table in the exact JSON form.  PLAIN is the same
table in the normal form."
  (cl-loop for row across (ek9-proxy--vector (alist-get 'rows table))
           for plain-row in (ek9-proxy--vector (alist-get 'rows plain))
           do (when-let* ((cell (assq 'object row)))
                (setcdr cell (alist-get 'object plain-row))))
  table)

(defun ek9-proxy--float-parts (number)
  "Return (DIGITS . POINT) for the absolute value of the float NUMBER.
DIGITS is the shortest digit string that gives NUMBER back, without
leading or trailing zeros.  The decimal point is after POINT digits."
  (let ((text (let ((float-output-format nil))
                (number-to-string (abs number)))))
    (unless (string-match "\\`\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?\\(?:e\\([-+]?[0-9]+\\)\\)?\\'"
                          text)
      (error "ek9: Cannot format the number %s" text))
    (let* ((whole (match-string 1 text))
           (digits (concat whole (or (match-string 2 text) "")))
           (point (+ (length whole)
                     (string-to-number (or (match-string 3 text) "0")))))
      (while (and (> (length digits) 1) (= (aref digits 0) ?0))
        (setq digits (substring digits 1)
              point (1- point)))
      (cons (replace-regexp-in-string "\\(.\\)0+\\'" "\\1" digits) point))))

(defun ek9-proxy--float-text (number exponent-form)
  "Return the float NUMBER as text, in the shortest form.
If EXPONENT-FORM is non-nil, use the exponent form with at least two
exponent digits, for example \"1.5e-07\".  Else use the decimal form."
  (pcase-let ((`(,digits . ,point) (ek9-proxy--float-parts number)))
    (concat
     (when (< number 0) "-")
     (if exponent-form
         (let ((exponent (1- point)))
           (format "%s%se%s%02d"
                   (substring digits 0 1)
                   (if (> (length digits) 1) (concat "." (substring digits 1)) "")
                   (if (< exponent 0) "-" "+")
                   (abs exponent)))
       (cond ((<= point 0)
              (concat "0." (make-string (- point) ?0) digits))
             ((>= point (length digits))
              (concat digits (make-string (- point (length digits)) ?0)))
             (t (concat (substring digits 0 point) "." (substring digits point))))))))

(defun ek9-proxy--whole-float-p (number)
  "Return non-nil if the float NUMBER has no fraction and fits in 64 bits.
The API server reads such a number as an integer."
  (and (= number (ftruncate number)) (< (abs number) 9.2e18)))

(defun ek9-proxy--format-float (number)
  "Return the float NUMBER as text, as kubectl prints it in a cell.
A float without a fraction prints as an integer, because the API
server reads it as an integer.  Other floats use the Go format
\"%v\": the exponent form when the exponent is less than -4 or at
least 6, else the decimal form."
  (if (ek9-proxy--whole-float-p number)
      (format "%d" (truncate number))
    (let ((exponent (1- (cdr (ek9-proxy--float-parts number)))))
      (ek9-proxy--float-text number (or (< exponent -4) (>= exponent 6))))))

(defun ek9-proxy-cell-text (value)
  "Return the text of the table cell VALUE, as kubectl prints it.
VALUE is in the exact JSON form."
  (cond ((stringp value) value)
        ((integerp value) (number-to-string value))
        ((floatp value) (ek9-proxy--format-float value))
        ((eq value t) "true")
        ((eq value :false) "false")
        ((eq value :null) "")
        (t (json-serialize value :null-object :null :false-object :false))))

(defun ek9-proxy-table-definitions (table)
  "Return the column definitions of the server TABLE, or nil.
In a watch, only the first table has column definitions."
  (let ((definitions (ek9-proxy--vector (alist-get 'columnDefinitions table))))
    (unless (seq-empty-p definitions)
      definitions)))

(defun ek9-proxy-table-items (table &optional definitions)
  "Return the items of the server TABLE.
TABLE has the form of `ek9-proxy-parse-table'.  Each item is the
object of a row, with two extra keys: `ek9-cells' is a vector with the
text of each cell, and `ek9-titles' is a vector with the column
titles.  All items share the same titles vector.  If TABLE has no
column definitions, use DEFINITIONS for the titles."
  (let ((titles (vconcat (mapcar (lambda (definition)
                                   (upcase (alist-get 'name definition)))
                                 (or (ek9-proxy-table-definitions table)
                                     (ek9-proxy--vector definitions))))))
    (mapcar (lambda (row)
              (let ((object (alist-get 'object row)))
                `((ek9-cells . ,(vconcat (mapcar #'ek9-proxy-cell-text
                                                 (ek9-proxy--vector
                                                  (alist-get 'cells row)))))
                  (ek9-titles . ,titles)
                  ,@object)))
            (ek9-proxy--vector (alist-get 'rows table)))))

(defun ek9-proxy--time-path (definition)
  "Return the path of the time for the column DEFINITION, or nil.
The server calculates the text of such a cell from a time in the
object, for example AGE from the creation time.  A built-in Age column
has the description of creationTimestamp.  Another built-in column
with this description can show the time itself, for example \"Created
At\" of cluster roles.  A CRD column of the type date has its JSON path
in the description.  A path is a list of JSON keys, see
`ek9-resource-path'."
  (let ((description (alist-get 'description definition)))
    (cond ((not (stringp description)) nil)
          ((and (equal (alist-get 'name definition) "Age")
                (string-prefix-p "CreationTimestamp is a timestamp" description))
           '(metadata creationTimestamp))
          ((and (equal (alist-get 'type definition) "date")
                (string-match "(in JSONPath format): \\(\\(?:\\.[A-Za-z0-9_-]+\\)+\\)[ \t]*\\'"
                              description))
           (mapcar #'intern (split-string (match-string 1 description) "\\." t))))))

(defun ek9-proxy-table-columns (table)
  "Return the ek9 columns for the column definitions of the server TABLE.
A column with a priority above 0 is a wide column.  The default width
of the name column is 40, and of the other columns the width of the
title, but at least 8.  A column grows to its default width when
there is space, so that its width changes less between refreshes.

A column that shows the time since a time in the object, for example
AGE, calculates its text at each render, see `ek9-proxy--time-path'.
This column has the property :time."
  (cl-loop
   for definition across (ek9-proxy--vector (alist-get 'columnDefinitions table))
   for index from 0
   collect
   (let* ((title (upcase (alist-get 'name definition)))
          (index index)
          (time-path (ek9-proxy--time-path definition))
          (cell (lambda (item)
                  (let ((cells (alist-get 'ek9-cells item)))
                    (if (< index (length cells)) (aref cells index) "")))))
     (append (list title
                   (if (equal (alist-get 'format definition) "name")
                       40
                     (max 8 (string-width title)))
                   (if time-path
                       (lambda (item)
                         (let ((time item))
                           (dolist (key time-path)
                             (setq time (alist-get key time)))
                           (or (and (stringp time)
                                    (ignore-errors (ek9-resource-since time)))
                               (funcall cell item))))
                     cell))
             (when (member (alist-get 'type definition) '("integer" "number"))
               (list :right-align t))
             (when (> (let ((priority (alist-get 'priority definition)))
                        (if (numberp priority) priority 0))
                      0)
               (list :wide t))
             (when time-path
               (list :time t))))))

(provide 'ek9-proxy)

;;; ek9-proxy.el ends here
