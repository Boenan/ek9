;;; ek9-logs-test.el --- Tests for ek9-logs -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for pod logs.  The tests use a fake kubectl script and do
;; not need a cluster.

;;; Code:

(require 'ert)
(require 'ek9-logs)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--logs-pod (name)
  "Return the pod NAME from the pods fixture."
  (seq-find (lambda (item) (equal (ek9-resource-item-name item) name))
            (alist-get 'items (ek9-test--read-fixture "pods.json"))))

(defun ek9-test--wait-for-stream-end ()
  "Wait until the log stream of the current buffer ended."
  (ek9-test--wait-until
   (lambda () (string-match-p "Log stream ended" (buffer-string)))))

(defmacro ek9-test--with-logs (pod-name container &rest body)
  "Open the logs of CONTAINER in POD-NAME, wait, and run BODY.
The logs buffer is current in BODY.  Kill it after BODY."
  (declare (indent 2))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev")
     (let ((buffer (ek9-logs (ek9-test--logs-pod ,pod-name) ,container)))
       (unwind-protect
           (with-current-buffer buffer
             (ek9-test--wait-for-stream-end)
             ,@body)
         (kill-buffer buffer)))))

(ert-deftest ek9-logs-shows-stream ()
  (ek9-test--with-logs "nginx-7c5b8d65b8-abcde" "nginx"
    (should (eq major-mode 'ek9-logs-mode))
    (should (equal (buffer-name) "*ek9 logs default/nginx-7c5b8d65b8-abcde*"))
    (should (string-match-p
             (concat "args: logs nginx-7c5b8d65b8-abcde --container nginx"
                     " --namespace default --follow --tail=200"
                     " --timestamps --context dev")
             (buffer-string)))
    ;; Timestamps are off, so the buffer does not show them.
    (should (string-match-p "^args: " (buffer-string)))
    (should (string-match-p "^line 1$" (buffer-string)))
    (should-not (string-match-p "2026-10-30T" (buffer-string)))
    (should buffer-read-only)))

(ert-deftest ek9-logs-applies-ansi-colors ()
  (ek9-test--with-logs "nginx-7c5b8d65b8-abcde" "nginx"
    (should-not (string-search "\e[" (buffer-string)))
    (goto-char (point-min))
    (search-forward "line 2")
    (should (get-text-property (match-beginning 0) 'font-lock-face))))

(ert-deftest ek9-logs-toggle-flags ()
  (ek9-test--with-logs "nginx-7c5b8d65b8-abcde" "nginx"
    (ek9-logs-toggle-timestamps)
    (ek9-test--wait-for-stream-end)
    (should (string-match-p "^2026-10-30T05:00:01Z line 1$" (buffer-string)))
    (ek9-logs-toggle-previous)
    (ek9-test--wait-for-stream-end)
    (should (string-match-p "--previous" (buffer-string)))
    (should-not (string-match-p "--follow" (buffer-string)))
    (should (string-match-p "\\[previous\\]"
                            (substring-no-properties (ek9-logs--tab-line))))))

(ert-deftest ek9-logs-toggle-stream ()
  (ek9-test--with-logs "nginx-7c5b8d65b8-abcde" "nginx"
    (should (equal ek9-logs--last-time "2026-10-30T05:00:01.000000002"))
    ;; The fake stream ended.  Make it look like a live stream.
    (let ((ek9-kubectl-executable "sleep"))
      (setq ek9-logs--process (ek9-process-stream '("10") #'ignore #'ignore)))
    ;; Stop: the process ends and the old lines stay.
    (let ((process ek9-logs--process))
      (ek9-logs-toggle-stream)
      (should-not (process-live-p process)))
    (should ek9-logs--stopped)
    (should (string-match-p "Log stream stopped" (buffer-string)))
    (should (string-match-p "Stopped"
                            (substring-no-properties (ek9-logs--tab-line))))
    ;; Start again: start at the second of the last line, and drop the
    ;; lines that the buffer has.
    (ek9-logs-toggle-stream)
    (should-not ek9-logs--stopped)
    (ek9-test--wait-until
     (lambda () (string-match-p "^line 3$" (buffer-string))))
    (should (string-match-p "Log stream started again" (buffer-string)))
    (should (string-match-p "--since-time=2026-10-30T05:00:01Z --timestamps"
                            (buffer-string)))
    (should (= (how-many "^line 2$" (point-min) (point-max)) 1))
    (should (equal ek9-logs--last-time "2026-10-30T05:00:02.000000000"))))

(ert-deftest ek9-logs-joins-split-lines ()
  (with-temp-buffer
    (ek9-logs-mode)
    (ek9-logs--output "2026-10-30T05:00:01.5Z first li")
    (should (equal (buffer-string) ""))
    (ek9-logs--output "ne\r\nerror without timestamp\n2026-10-30T05:00:02Z x")
    (should (equal (buffer-string) "first line\nerror without timestamp\n"))
    (should (equal ek9-logs--last-time "2026-10-30T05:00:01.500000000"))
    (should (equal ek9-logs--partial "2026-10-30T05:00:02Z x"))))

(ert-deftest ek9-logs-read-container ()
  ;; One container: no question.
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) (error "Unexpected question"))))
    (should (equal (ek9-list-read-container
                    (ek9-test--logs-pod "nginx-7c5b8d65b8-abcde"))
                   "nginx")))
  ;; Two containers: ask, with the first container as default.
  (let (choices default)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest args)
                 (setq choices collection
                       default (nth 4 args))
                 "proxy")))
      (should (equal (ek9-list-read-container
                      (ek9-test--logs-pod "api-5f9c-xyz"))
                     "proxy"))
      (should (equal choices '("app" "proxy")))
      (should (equal default "app")))))

(ert-deftest ek9-logs-default-container-annotation ()
  (let ((pod '((metadata
                (annotations
                 (kubectl.kubernetes.io/default-container . "proxy")))
               (spec (containers ((name . "app")) ((name . "proxy")))))))
    (should (equal (ek9-resource-pod-default-container pod) "proxy"))))

(ert-deftest ek9-logs-trims-old-lines ()
  (with-temp-buffer
    (ek9-logs-mode)
    (let ((ek9-logs-max-lines 2))
      (ek9-logs--insert "a\nb\nc\nd\n")
      (should (equal (buffer-string) "c\nd\n"))
      (should (= ek9-logs--line-count 2)))))

(ert-deftest ek9-logs-stops-process-on-kill ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (let* ((ek9-kubectl-executable "sleep")
           (buffer (get-buffer-create "*ek9 logs test*"))
           process)
      (with-current-buffer buffer
        (ek9-logs-mode)
        (setq ek9-logs--process
              (ek9-process-stream '("10") #'ignore #'ignore))
        (setq process ek9-logs--process))
      (kill-buffer buffer)
      (should-not (process-live-p process)))))

(provide 'ek9-logs-test)

;;; ek9-logs-test.el ends here
