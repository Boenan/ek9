;;; ek9-portfwd-test.el --- Tests for ek9-portfwd -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the port-forward manager.  The fake kubectl prints
;; "Forwarding from" and waits, as the real command does.

;;; Code:

(require 'ert)
(require 'ek9-portfwd)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defconst ek9-test--service
  '((kind . "Service")
    (metadata (name . "web") (namespace . "team-a"))
    (spec (ports ((port . 80) (targetPort . 8080))
                 ((port . 443) (targetPort . 8443)))))
  "A service for the tests.")

(defmacro ek9-test--with-forwards (&rest body)
  "Run BODY in context dev with an empty port-forward list.
Stop all port-forwards and kill the manager buffer after BODY."
  (declare (indent 0))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev")
     (let ((ek9-portfwd--forwards nil)
           messages)
       (cl-letf (((symbol-function 'message)
                  (lambda (format &rest args)
                    (push (apply #'format-message format args) messages))))
         (unwind-protect
             (progn ,@body)
           (dolist (forward ek9-portfwd--forwards)
             (when (ek9-portfwd--running-p forward)
               (delete-process (ek9-portfwd--forward-process forward))))
           (when-let* ((buffer (get-buffer ek9-portfwd--buffer-name)))
             (kill-buffer buffer)))))))

(ert-deftest ek9-portfwd-item-ports ()
  (should (equal (ek9-portfwd--item-ports ek9-test--service) '(80 443)))
  (should (equal (ek9-portfwd--item-ports
                  '((spec (containers ((name . "a")
                                       (ports ((containerPort . 8080))))
                                      ((name . "b")
                                       (ports ((containerPort . 9090))
                                              ((containerPort . 8080))))))))
                 '(8080 9090)))
  (should (equal (ek9-portfwd--item-ports
                  '((spec (template
                           (spec (containers ((ports ((containerPort . 3000))))))))))
                 '(3000)))
  (should-not (ek9-portfwd--item-ports '((spec)))))

(ert-deftest ek9-portfwd-check-ports ()
  (dolist (ports '("8080:80" "80" ":80" "65535:1"))
    (should (equal (ek9-portfwd--check-ports ports) ports)))
  (dolist (ports '("" "abc" "8080:" "0:80" "70000:80" "1:2:3" "-1:80"))
    (should-error (ek9-portfwd--check-ports ports) :type 'user-error)))

(ert-deftest ek9-portfwd-read-ports-offers-item-ports ()
  (let (choices)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest args)
                 (setq choices collection)
                 (nth 4 args))))
      (should (equal (ek9-portfwd--read-ports ek9-test--service) "8080:80"))
      (should (equal choices '("8080:80" "80:80" "8443:443" "443:443"))))))

(ert-deftest ek9-portfwd-port-choices ()
  (should (equal (ek9-portfwd--port-choices 3000) '("3000:3000")))
  (should (equal (ek9-portfwd--port-choices 80) '("8080:80" "80:80"))))

(ert-deftest ek9-portfwd-args ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (should (equal (ek9-portfwd--args (ek9-resource-get 'services)
                                      ek9-test--service "8080:80")
                   '("port-forward" "services/web" "8080:80"
                     "--address" "localhost"
                     "--namespace" "team-a" "--context" "dev")))))

(ert-deftest ek9-portfwd-start-and-stop ()
  (ek9-test--with-forwards
    (let ((forward (ek9-port-forward (ek9-resource-get 'services)
                                     ek9-test--service "8080:80")))
      (ek9-test--wait-until
       (lambda () (equal (ek9-portfwd--forward-status forward) "Running")))
      (should (member (concat "ek9: Port-forward runs: services/web "
                              "8080:80 on localhost in context dev")
                      messages))
      (should (process-query-on-exit-flag (ek9-portfwd--forward-process forward)))
      ;; The same port-forward again does not start a second process.
      (should (eq (ek9-port-forward (ek9-resource-get 'services)
                                    ek9-test--service "8080:80")
                  forward))
      (should (= (length ek9-portfwd--forwards) 1))
      (with-current-buffer (ek9-port-forwards)
        (goto-char (point-min))
        (should (eq (tabulated-list-get-id) forward))
        (should (string-match-p "services/web +localhost +8080:80 +Running"
                                (buffer-string)))
        (should (string-match-p "1 running"
                                (substring-no-properties (ek9-portfwd--tab-line))))
        (ek9-portfwd-stop forward)
        (ek9-test--wait-until
         (lambda () (equal (ek9-portfwd--forward-status forward) "Stopped")))
        (should (string-match-p "Stopped" (buffer-string)))
        ;; D on a stopped port-forward removes it.
        (ek9-portfwd-stop forward)
        (should-not ek9-portfwd--forwards)))))

(ert-deftest ek9-portfwd-failure ()
  (ek9-test--with-forwards
    (let ((forward (ek9-port-forward (ek9-resource-get 'services)
                                     ek9-test--service "9999:9999")))
      (ek9-test--wait-until
       (lambda () (not (ek9-portfwd--running-p forward))))
      (accept-process-output nil 0.05)
      (should (equal (ek9-portfwd--forward-status forward)
                     (concat "Failed: error: unable to listen on any of the "
                             "requested ports: [{9999 9999}]")))
      (should (seq-some (lambda (m) (string-prefix-p "ek9: Port-forward failed" m))
                        messages)))))

(defun ek9-test--start-forward (ports &optional resource item)
  "Start a port-forward with PORTS to ITEM of RESOURCE and return it.
RESOURCE and ITEM are the test service by default."
  (ek9-port-forward (or resource (ek9-resource-get 'services))
                    (or item ek9-test--service) ports))

(ert-deftest ek9-portfwd-random-local-port ()
  (ek9-test--with-forwards
    (let ((forward (ek9-test--start-forward ":80")))
      (ek9-test--wait-until
       (lambda () (equal (ek9-portfwd--forward-status forward) "Running")))
      (should (equal (ek9-portfwd--ports-label forward) "43517:80"))
      (should (seq-some (lambda (m) (string-match-p "43517:80 on localhost" m))
                        messages)))))

(ert-deftest ek9-portfwd-error-while-running ()
  (ek9-test--with-forwards
    (let ((forward (ek9-test--start-forward "7777:7777")))
      (ek9-test--wait-until
       (lambda () (equal (ek9-portfwd--forward-status forward)
                         "Running (errors)")))
      (should (ek9-portfwd--running-p forward))
      (should (string-match-p "connection refused"
                              (ek9-portfwd--forward-last-line forward))))))

(ert-deftest ek9-portfwd-lost-connection ()
  (ek9-test--with-forwards
    (let ((forward (ek9-test--start-forward "8888:8888")))
      (ek9-test--wait-until
       (lambda () (string-prefix-p "Failed" (ek9-portfwd--forward-status forward))))
      (should (equal (ek9-portfwd--forward-status forward)
                     "Failed: error: lost connection to pod")))))

(ert-deftest ek9-portfwd-killed-by-signal ()
  (ek9-test--with-forwards
    (let ((forward (ek9-test--start-forward "8080:80")))
      (ek9-test--wait-until
       (lambda () (equal (ek9-portfwd--forward-status forward) "Running")))
      (signal-process (ek9-portfwd--forward-process forward) 'SIGTERM)
      (ek9-test--wait-until
       (lambda () (string-prefix-p "Failed" (ek9-portfwd--forward-status forward))))
      (should (equal (ek9-portfwd--forward-status forward)
                     "Failed: killed (signal 15)")))))

(ert-deftest ek9-portfwd-split-output ()
  (let ((forward (ek9-portfwd--make-forward :ports "8080:80")))
    (ek9-portfwd--output forward "Forwar")
    (should (equal (ek9-portfwd--forward-status forward) "Starting"))
    (ek9-portfwd--output forward "ding from 127.0.0.1:8080 -> 80\n")
    (should (equal (ek9-portfwd--forward-status forward) "Running"))
    (should (equal (ek9-portfwd--forward-last-line forward)
                   "Forwarding from 127.0.0.1:8080 -> 80"))))

(ert-deftest ek9-portfwd-duplicates ()
  (ek9-test--with-forwards
    (let ((first (ek9-test--start-forward "8080:80")))
      ;; A different namespace, context, or address is a new forward.
      (let ((other (copy-tree ek9-test--service)))
        (setf (alist-get 'namespace (alist-get 'metadata other)) "team-b")
        (should-not (eq (ek9-test--start-forward "8081:80" nil other) first)))
      (let ((ek9-portfwd-address "127.0.0.1"))
        (should-not (eq (ek9-test--start-forward "8080:80") first)))
      (setq ek9-context--current "prod")
      (should-not (eq (ek9-test--start-forward "8080:80") first))
      (should (= (length ek9-portfwd--forwards) 4)))))

(ert-deftest ek9-portfwd-targets ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (let ((item '((metadata (name . "web") (namespace . "a")))))
      (should (equal (nth 1 (ek9-portfwd--args (ek9-resource-get 'pods) item "1:1"))
                     "pods/web"))
      (should (equal (nth 1 (ek9-portfwd--args (ek9-resource-get 'deployments)
                                               item "1:1"))
                     "deployments/web")))))

(ert-deftest ek9-portfwd-manager-reopens ()
  (ek9-test--with-forwards
    (let ((forward (ek9-test--start-forward "8080:80")))
      (kill-buffer (ek9-port-forwards))
      (should (ek9-portfwd--running-p forward))
      (with-current-buffer (ek9-port-forwards)
        (goto-char (point-min))
        (should (eq (tabulated-list-get-id) forward))))))

(ert-deftest ek9-portfwd-interactive-from-list ()
  (ek9-test--with-forwards
    (with-temp-buffer
      (ek9-list-mode)
      (setq ek9-list--resource (ek9-resource-get 'services))
      (ek9-list--update (list ek9-test--service))
      (goto-char (ek9-list--find-id "team-a/web"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest args) (nth 6 args))))
        (let ((forward (call-interactively #'ek9-port-forward)))
          (should (equal (ek9-portfwd--forward-ports forward) "8080:80"))
          (should (equal (ek9-portfwd--forward-target forward) "services/web")))))))

(ert-deftest ek9-portfwd-read-only-context-allowed ()
  (ek9-test--with-forwards
    (setq ek9-read-only-contexts '("dev"))
    (let ((forward (ek9-port-forward (ek9-resource-get 'services)
                                     ek9-test--service "8080:80")))
      (should (ek9-portfwd--running-p forward)))))

(ert-deftest ek9-portfwd-no-context ()
  (ek9-test--with-forwards
    (setq ek9-context--current nil)
    (should-error (ek9-port-forward (ek9-resource-get 'services)
                                    ek9-test--service "8080:80")
                  :type 'user-error)))

(ert-deftest ek9-portfwd-key-and-actions ()
  (should (eq (keymap-lookup ek9-list-mode-map "f") #'ek9-port-forward))
  (dolist (name '(pods deployments statefulsets services))
    (should (memq 'port-forward (ek9-resource-actions (ek9-resource-get name)))))
  (should-not (memq 'port-forward (ek9-resource-actions (ek9-resource-get 'nodes)))))

(provide 'ek9-portfwd-test)

;;; ek9-portfwd-test.el ends here
