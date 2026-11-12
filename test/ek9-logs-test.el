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
    (should (equal (ek9-logs--since) "2026-10-30T05:00:01.000000002"))
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
    (should (equal (ek9-logs--since) "2026-10-30T05:00:02.000000000"))))

(ert-deftest ek9-logs-joins-split-lines ()
  (with-temp-buffer
    (ek9-logs-mode)
    (ek9-logs--output "2026-10-30T05:00:01.5Z first li")
    (should (equal (buffer-string) ""))
    (ek9-logs--output "ne\r\nerror without timestamp\n2026-10-30T05:00:02Z x")
    (should (equal (buffer-string) "first line\nerror without timestamp\n"))
    (should (equal (ek9-logs--since) "2026-10-30T05:00:01.500000000"))
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

(defconst ek9-test--logs-deployment
  '((kind . "Deployment")
    (metadata (name . "web") (namespace . "default"))
    (spec (selector (matchLabels (app . "web")))
          (template (metadata (labels (app . "web")))
                    (spec (containers ((name . "app"))
                                      ((name . "sidecar")))))))
  "A deployment with two containers.")

(defmacro ek9-test--with-workload-logs (container &rest body)
  "Open the logs of CONTAINER in the test deployment, wait, run BODY."
  (declare (indent 1))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev")
     (let ((buffer (ek9-logs ek9-test--logs-deployment ,container)))
       (unwind-protect
           (with-current-buffer buffer
             (ek9-test--wait-for-stream-end)
             ,@body)
         (kill-buffer buffer)))))

(ert-deftest ek9-logs-workload-stream ()
  (ek9-test--with-workload-logs "app"
    (should (equal (buffer-name) "*ek9 logs deployment default/web*"))
    (should (string-match-p
             (concat "args: logs --selector app=web --max-log-requests=50"
                     " --container app --prefix --namespace default"
                     " --follow --tail=200 --timestamps --context dev")
             (buffer-string)))
    ;; Pod names are off by default, and one container has no label.
    (should (string-match-p "^line 1$" (buffer-string)))
    (should-not (string-search "web-1" (buffer-string)))
    (should (string-match-p "Workload: deployment default/web"
                            (substring-no-properties (ek9-logs--tab-line))))
    ;; Each source has its own last time.
    (should (equal (gethash "web-1/app" ek9-logs--last-times)
                   "2026-10-30T05:00:02.000000000"))
    (should (equal (gethash "web-2/sidecar" ek9-logs--last-times)
                   "2026-10-30T05:00:02.000000000"))
    (should (equal (ek9-logs--since) "2026-10-30T05:00:02.000000000"))))

(ert-deftest ek9-logs-workload-pod-names ()
  (ek9-test--with-workload-logs "app"
    (let ((process ek9-logs--process))
      ;; The toggle changes all lines, and does not start a new stream.
      (ek9-logs-toggle-pod-names)
      (should (eq ek9-logs--process process)))
    (should (string-match-p "\\[pod names\\]"
                            (substring-no-properties (ek9-logs--tab-line))))
    (should (= (how-many "args: logs" (point-min) (point-max)) 1))
    (should (string-match-p "^web-1 line 1$" (buffer-string)))
    (should (string-match-p "^web-2 line 2$" (buffer-string)))
    ;; Each pod has its own color.
    (goto-char (point-min))
    (search-forward "web-1 line 1")
    (let ((face-1 (get-text-property (match-beginning 0) 'font-lock-face)))
      (search-forward "web-2 line 2")
      (should face-1)
      (should-not (equal face-1 (get-text-property (match-beginning 0)
                                                   'font-lock-face))))
    ;; New lines get the names too.
    (ek9-logs--output "[pod/web-2/app] 2026-10-30T05:00:09Z line 9\n")
    (ek9-logs--flush)
    (should (string-match-p "^web-2 line 9$" (buffer-string)))
    ;; The toggle turns the names off again, on all lines.
    (ek9-logs-toggle-pod-names)
    (should-not ek9-logs--pod-names)
    (should (string-match-p "^line 1$" (buffer-string)))
    (should (string-match-p "^line 9$" (buffer-string)))
    (should-not (string-search "web-" (buffer-substring
                                       (line-beginning-position 2)
                                       (point-max))))))

(ert-deftest ek9-logs-relabel-keeps-point ()
  (ek9-test--with-workload-logs ek9-logs-all-containers
    (goto-char (point-min))
    (search-forward "line 3")
    (let ((text-at-point (buffer-substring (line-beginning-position)
                                           (line-end-position))))
      (should (equal (substring-no-properties text-at-point) "app line 3"))
      (ek9-logs-toggle-pod-names)
      (should (equal (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))
                     "web-1/app line 3"))
      (should (looking-back "line 3" (line-beginning-position)))
      (ek9-logs-toggle-pod-names)
      (should (equal (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))
                     "app line 3")))))

(ert-deftest ek9-logs-all-containers ()
  (ek9-test--with-workload-logs ek9-logs-all-containers
    (should (string-match-p "--all-containers --prefix" (buffer-string)))
    (should-not (string-match-p "--container" (buffer-string)))
    ;; The container name is always there, in bold and in color.
    (should (string-match-p "^app line 1$" (buffer-string)))
    (should (string-match-p "^sidecar line 2$" (buffer-string)))
    (goto-char (point-min))
    (search-forward "sidecar line 2")
    (let ((face (get-text-property (match-beginning 0) 'font-lock-face)))
      (should (eq (plist-get face :weight) 'bold))
      (should (plist-get face :foreground)))
    ;; The red color of web-2 does not go into the next line of web-1.
    (search-forward "app line 3")
    (should-not (get-text-property (+ (match-beginning 0) 4) 'font-lock-face))
    (should (string-match-p "Container: all"
                            (substring-no-properties (ek9-logs--tab-line))))
    ;; With pod names on, both names show.
    (ek9-logs-toggle-pod-names)
    (should (string-match-p "^web-2/sidecar line 2$" (buffer-string)))))

(ert-deftest ek9-logs-workload-start-again ()
  (ek9-test--with-workload-logs "app"
    (let ((ek9-kubectl-executable "sleep"))
      (setq ek9-logs--process (ek9-process-stream '("10") #'ignore #'ignore)))
    (ek9-logs-toggle-stream)
    (ek9-logs-toggle-stream)
    (ek9-test--wait-until
     (lambda () (= (how-many "Log stream ended" (point-min) (point-max)) 2)))
    ;; Each source drops the lines that it showed before.  web-1 has
    ;; line 3 at 05:00:03 as a new line.
    (should (= (how-many "^line 1$" (point-min) (point-max)) 1))
    (should (= (how-many "^line 2$" (point-min) (point-max)) 1))
    (should (= (how-many "^line 3$" (point-min) (point-max)) 2))))

(ert-deftest ek9-logs-read-workload-container ()
  (let (choices default)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest args)
                 (setq choices collection
                       default (nth 4 args))
                 ek9-logs-all-containers)))
      (should (equal (ek9-logs-read-container ek9-test--logs-deployment)
                     ek9-logs-all-containers))
      (should (equal choices (list "app" "sidecar" ek9-logs-all-containers)))
      (should (equal default "app")))))

(ert-deftest ek9-logs-workload-without-selector ()
  (should-error (ek9-logs '((kind . "Job")
                            (metadata (name . "x") (namespace . "default"))
                            (spec (template (spec (containers ((name . "a")))))))
                          "a")
                :type 'user-error))

(ert-deftest ek9-logs-pod-names-only-for-workloads ()
  (with-temp-buffer
    (ek9-logs-mode)
    (setq ek9-logs--object (ek9-test--logs-pod "nginx-7c5b8d65b8-abcde"))
    (should-error (ek9-logs-toggle-pod-names) :type 'user-error)))

(defmacro ek9-test--with-workload-buffer (&rest body)
  "Run BODY in a logs buffer for the test deployment, without a stream."
  (declare (indent 0))
  `(with-temp-buffer
     (ek9-logs-mode)
     (setq ek9-logs--object ek9-test--logs-deployment
           ek9-logs--container "app")
     (unwind-protect
         (progn ,@body)
       (ek9-logs--flush))))

(ert-deftest ek9-logs-sorts-pods-by-time ()
  (ek9-test--with-workload-buffer
    ;; kubectl gives a block of lines for each pod.
    (ek9-logs--output (concat "[pod/web-1/app] 2026-10-30T05:00:01Z a1\n"
                              "[pod/web-1/app] 2026-10-30T05:00:03Z a3\n"
                              "[pod/web-1/app] 2026-10-30T05:00:05Z a5\n"))
    ;; The lines wait for the sort.
    (should (equal (buffer-string) ""))
    (should (timerp ek9-logs--sort-timer))
    (ek9-logs--output (concat "[pod/web-2/app] 2026-10-30T05:00:02Z b2\n"
                              "[pod/web-2/app] 2026-10-30T05:00:04Z b4\n"))
    (ek9-logs--flush)
    (should (equal (buffer-string) "a1\nb2\na3\nb4\na5\n"))
    (should (= ek9-logs--line-count 5))
    ;; A later block of an old pod goes to its time position.
    (ek9-logs--output (concat "[pod/web-3/app] 2026-10-30T05:00:01.5Z c1\n"
                              "[pod/web-3/app] 2026-10-30T05:00:06Z c6\n"))
    (ek9-logs--flush)
    (should (equal (buffer-string) "a1\nc1\nb2\na3\nb4\na5\nc6\n"))
    ;; The labels do not change the order.
    (ek9-logs-toggle-pod-names)
    (ek9-logs-toggle-pod-names)
    (should (equal (buffer-string) "a1\nc1\nb2\na3\nb4\na5\nc6\n"))))

(ert-deftest ek9-logs-sort-keeps-notes ()
  (ek9-test--with-workload-buffer
    (ek9-logs--output "[pod/web-1/app] 2026-10-30T05:00:05Z a5\n")
    (ek9-logs--flush)
    (ek9-logs--insert-note "Log stream started again")
    ;; An older line does not go before the note.
    (ek9-logs--output "[pod/web-2/app] 2026-10-30T05:00:01Z b1\n")
    (ek9-logs--flush)
    (should (equal (buffer-string)
                   "a5\n[ek9: Log stream started again]\nb1\n"))))

(ert-deftest ek9-logs-sort-line-without-timestamp ()
  (ek9-test--with-workload-buffer
    (ek9-logs--output (concat "[pod/web-1/app] 2026-10-30T05:00:03Z a3\n"
                              "error: lost the connection\n"
                              "[pod/web-2/app] 2026-10-30T05:00:01Z b1\n"))
    (ek9-logs--flush)
    ;; The error gets the newest time, so it comes after a3.
    (should (equal (buffer-string) "b1\na3\nerror: lost the connection\n"))))

(ert-deftest ek9-logs-sort-keeps-point-at-end ()
  (ek9-test--with-workload-buffer
    (ek9-logs--output "[pod/web-1/app] 2026-10-30T05:00:03Z a3\n")
    (ek9-logs--flush)
    (goto-char (point-max))
    (ek9-logs--output "[pod/web-2/app] 2026-10-30T05:00:01Z b1\n")
    (ek9-logs--flush)
    (should (= (point) (point-max)))))

(ert-deftest ek9-logs-default-colors ()
  (dolist (dark '(t nil))
    (let ((colors (ek9-logs--default-colors 50 dark)))
      (should (= (length colors) 50))
      (should (= (length (delete-dups (copy-sequence colors))) 50))
      ;; Grey is the last color.
      (should (equal (car (last colors)) (if dark "#999999" "#666666")))
      (dolist (color colors)
        (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" color))))))

(ert-deftest ek9-logs-name-colors-option ()
  (with-temp-buffer
    (ek9-logs-mode)
    (let* ((ek9-logs-name-colors '("red" "blue" "green" "grey"))
           (names '("a" "b" "c"))
           (colors (mapcar #'ek9-logs--name-color names)))
      ;; Three names get the three colors before grey.
      (should (equal (sort (copy-sequence colors) #'string<)
                     '("blue" "green" "red")))
      ;; Grey is for the fourth name, when the other colors are in use.
      (should (equal (ek9-logs--name-color "d") "grey"))
      ;; A name keeps its color.
      (should (equal (ek9-logs--name-color "a") (car colors))))))

(ert-deftest ek9-logs-name-color-does-not-depend-on-order ()
  (let ((names (mapcar (lambda (i) (format "web-7c5b8d65b8-%05d" i))
                       (number-sequence 1 8)))
        first second)
    (with-temp-buffer
      (ek9-logs-mode)
      (setq first (mapcar (lambda (name)
                            (cons name (ek9-logs--name-color name)))
                          names)))
    (with-temp-buffer
      (ek9-logs-mode)
      (setq second (mapcar (lambda (name)
                             (cons name (ek9-logs--name-color name)))
                           (reverse names))))
    ;; A name without a conflict has the same color in both orders.
    (let ((same (seq-count (lambda (entry)
                             (equal (cdr entry)
                                    (cdr (assoc (car entry) second))))
                           first)))
      (should (>= same 6)))
    ;; All names in a buffer have different colors.
    (should (= (length (delete-dups (mapcar #'cdr first))) 8))
    ;; Grey is not used while other colors are free.
    (should-not (member "#999999" (mapcar #'cdr first)))
    (should-not (member "#666666" (mapcar #'cdr first)))))

(ert-deftest ek9-logs-name-colors-stay-after-restart ()
  (ek9-test--with-workload-logs ek9-logs-all-containers
    (ek9-logs-toggle-pod-names)
    (let ((before (ek9-logs--name-color "web-2")))
      (ek9-logs-toggle-timestamps)
      (ek9-test--wait-for-stream-end)
      (ek9-logs-toggle-timestamps)
      (ek9-test--wait-for-stream-end)
      (should (equal (ek9-logs--name-color "web-2") before))
      (goto-char (point-min))
      (search-forward "web-2/sidecar")
      (should (equal (plist-get (get-text-property (match-beginning 0)
                                                   'font-lock-face)
                                :foreground)
                     before)))))

(ert-deftest ek9-logs-workload-actions ()
  (dolist (name '(deployments statefulsets daemonsets jobs))
    (should (memq 'logs (ek9-resource-actions (ek9-resource-get name))))))

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
