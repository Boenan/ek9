;;; ek9-polish-test.el --- Tests for ek9 display details -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the column widths, the empty-list message, the
;; cluster name in the tab line, and the kubectl check at startup.

;;; Code:

(require 'ert)
(require 'ek9)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--pod-named (name)
  "Return a running pod with NAME in namespace argocd."
  `((metadata (name . ,name) (namespace . "argocd"))
    (spec (containers ((name . "c"))))
    (status (phase . "Running"))))

(defun ek9-test--empty-text ()
  "Return the empty-list message of the current list buffer, or nil."
  (and ek9-list--empty-overlay
       (substring-no-properties
        (or (overlay-get ek9-list--empty-overlay 'after-string) ""))))

(defun ek9-test--pods-table (items)
  "Return a server table with the pod columns for the pod ITEMS."
  `((kind . "Table")
    (columnDefinitions . ,(alist-get 'columnDefinitions (ek9-test--table "pods")))
    (rows . ,(vconcat
              (mapcar (lambda (item)
                        `((cells . [,(ek9-resource-item-name item)
                                    "1/1" "Running" 0 "1d" "<none>" "<none>"])
                          (object . ,item)))
                      items)))))

(defmacro ek9-test--in-pods-list (items &rest body)
  "Run BODY in a pods list buffer that shows ITEMS in context dev."
  (declare (indent 1))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev"
           ek9-context--namespace "argocd")
     (with-temp-buffer
       (ek9-list-mode)
       (setq ek9-list--resource (ek9-resource-get 'pods))
       ;; A wide window, unless the test sets another width.
       (setq ek9-list--last-width 200)
       (ek9-list--receive (ek9-test--pods-table ,items))
       ,@body)))

(ert-deftest ek9-polish-long-name-widens-column ()
  (let ((long "argocd-applicationset-controller-6c8b9d5f4-x2xkq"))
    (ek9-test--in-pods-list (list (ek9-test--pod-named long)
                                  (ek9-test--pod-named "short"))
      (should (= (cadr (aref tabulated-list-format 0)) (length long)))
      (should (string-match-p (regexp-quote long) (buffer-string))))))

(ert-deftest ek9-polish-short-names-keep-default-width ()
  (ek9-test--in-pods-list (list (ek9-test--pod-named "short"))
    ;; A wide window has space for the default widths.
    (setq ek9-list--last-width 200)
    (ek9-list--render)
    (should (= (cadr (aref tabulated-list-format 0)) 40))))

(defun ek9-test--format-widths ()
  "Return the column widths of the current list buffer."
  (mapcar #'cadr tabulated-list-format))

(ert-deftest ek9-polish-columns-fit-narrow-window ()
  (let ((long "argocd-applicationset-controller-6c8b9d5f4-x2xkq"))
    (ek9-test--in-pods-list (list (ek9-test--pod-named long))
      (dolist (width '(80 60 45))
        (setq ek9-list--last-width width)
        (ek9-list--render)
        ;; The row fits, with one character for the window edge.
        (should (<= (ek9-list--row-width (ek9-test--format-widths))
                    (1- width))))
      ;; The narrowest NAME is the width of its title.
      (setq ek9-list--last-width 10)
      (ek9-list--render)
      (should (= (car (ek9-test--format-widths)) 4)))))

(ert-deftest ek9-polish-free-space-goes-to-columns ()
  (ek9-test--in-pods-list (list (ek9-test--pod-named "short"))
    (setq ek9-list--last-width 80)
    (ek9-list--render)
    (let ((widths (ek9-test--format-widths)))
      (should (= (ek9-list--row-width widths) 79))
      ;; No column is wider than its default.
      (should (cl-every #'<= widths '(40 8 8 8 8 8 8))))))

(ert-deftest ek9-polish-fit-and-grow ()
  (let ((tabulated-list-padding 1))
    ;; The widest column gets narrower first.  The last column stays.
    (should (equal (ek9-list--fit '(30 10 100) '(4 4 4) 30) '(4 4 100)))
    (should (equal (ek9-list--fit '(20 10 5) '(4 4 4) 30) '(12 10 5)))
    ;; Free space goes to the columns, one character each in turn.
    (should (equal (ek9-list--grow '(5 5) '(10 6) 14) '(6 6)))
    (should (equal (ek9-list--grow '(5 5) '(10 6) 100) '(10 6)))))

(ert-deftest ek9-polish-narrowest-window-wins ()
  (ek9-test--in-pods-list (list (ek9-test--pod-named "short"))
    (let ((windows '(wide narrow)))
      (cl-letf (((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _) windows))
                ((symbol-function 'window-live-p) (lambda (_) t))
                ((symbol-function 'window-max-chars-per-line)
                 (lambda (window) (if (eq window 'wide) 160 40))))
        (should (= (ek9-list--available-width) 40))))))

(ert-deftest ek9-polish-height-change-does-not-render ()
  (ek9-test--in-pods-list (list (ek9-test--pod-named "short"))
    (let ((window (selected-window))
          (old (window-buffer (selected-window)))
          (renders 0))
      (set-window-buffer window (current-buffer))
      (unwind-protect
          (cl-letf (((symbol-function 'window-max-chars-per-line)
                     (lambda (&rest _) 70)))
            (setq ek9-list--last-width 70)
            (advice-add 'ek9-list--render :before (lambda () (cl-incf renders))
                        '((name . ek9-test-count)))
            (ek9-list--window-resized window)
            (should (= renders 0)))
        (advice-remove 'ek9-list--render 'ek9-test-count)
        (set-window-buffer window old)))))

(ert-deftest ek9-polish-global-line-numbers ()
  ;; A global mode turns on line numbers while `ek9-list-mode' starts,
  ;; before the buffer has a resource type.
  (ek9-test--with-clean-context
    (setq ek9-context--current "dev")
    (let ((global-display-line-numbers-mode nil))
      (unwind-protect
          (progn
            (global-display-line-numbers-mode 1)
            (let ((buffer (ek9-list-open 'nodes)))
              (with-current-buffer buffer
                (should display-line-numbers-mode)
                (should (eq (ek9-resource-name ek9-list--resource) 'nodes)))
              (kill-buffer buffer)))
        (global-display-line-numbers-mode -1)
        (when (timerp ek9-list--timer)
          (cancel-timer ek9-list--timer)
          (setq ek9-list--timer nil))))))

(ert-deftest ek9-polish-refit-hooks ()
  (with-temp-buffer
    (ek9-list-mode)
    (should (memq #'ek9-list--refit text-scale-mode-hook))
    (should (memq #'ek9-list--refit display-line-numbers-mode-hook))))

(ert-deftest ek9-polish-resize-fits-again ()
  (ek9-test--in-pods-list (list (ek9-test--pod-named "short"))
    (let ((window (selected-window))
          (old (window-buffer (selected-window))))
      (set-window-buffer window (current-buffer))
      (unwind-protect
          (progn
            (setq ek9-list--last-width nil)
            (cl-letf (((symbol-function 'window-max-chars-per-line)
                       (lambda (&rest _) 50)))
              (ek9-list--window-resized window))
            (should (<= (ek9-list--row-width (ek9-test--format-widths)) 49))
            ;; A dead window or t does nothing.
            (ek9-list--window-resized t))
        (set-window-buffer window old)))))

(ert-deftest ek9-polish-max-column-width ()
  (let ((ek9-list-max-column-width 50)
        (long (make-string 70 ?a)))
    (ek9-test--in-pods-list (list (ek9-test--pod-named long))
      (should (= (cadr (aref tabulated-list-format 0)) 50))
      ;; tabulated-list hides the end of the value with a display
      ;; property.  ek9 makes it show ASCII dots.
      (goto-char (point-min))
      (search-forward (make-string 48 ?a))
      (should (equal (get-text-property (1- (point)) 'display) "...")))))

(ert-deftest ek9-polish-widths-ignore-filter ()
  (let ((long "argocd-applicationset-controller-6c8b9d5f4-x2xkq"))
    (ek9-test--in-pods-list (list (ek9-test--pod-named long)
                                  (ek9-test--pod-named "short"))
      (ek9-list-filter "short")
      (should (= (cadr (aref tabulated-list-format 0)) (length long))))))

(ert-deftest ek9-polish-empty-messages ()
  (ek9-test--in-pods-list nil
    (should (string-match-p "No pods in namespace argocd\\." (ek9-test--empty-text)))
    (setq ek9-context--namespace 'all)
    (ek9-list--render)
    (should (string-match-p "No pods in all namespaces\\." (ek9-test--empty-text)))
    (setq ek9-list--label "app=web"
          ek9-list--owner "statefulset/db"
          ek9-list--namespace-override "team-a")
    (ek9-list--render)
    (should (string-match-p (concat "No pods with selector app=web owned by "
                                    "statefulset/db in namespace team-a\\.")
                            (ek9-test--empty-text))))
  (ek9-test--in-pods-list (list (ek9-test--pod-named "web"))
    (ek9-list-filter "zzz")
    (should (string-match-p "No pods match the filter \"zzz\"\\."
                            (ek9-test--empty-text)))
    ;; A sort by column reprints the buffer.  The message stays.
    (tabulated-list--sort-by-column-name "NAME")
    (should (string-match-p "No pods match" (ek9-test--empty-text)))
    (ek9-list-filter "")
    (should-not (string-match-p "No pods" (ek9-test--empty-text))))
  (ek9-test--with-clean-context
    (with-temp-buffer
      (ek9-list-mode)
      (setq ek9-list--resource (ek9-resource-get 'nodes))
      (ek9-list--receive (ek9-test--name-table "{\"items\": []}"))
      (should (equal (ek9-test--empty-text) " No nodes.\n"))
      (should (equal (buffer-string) "")))))

(ert-deftest ek9-polish-no-message-before-load ()
  (ek9-test--with-clean-context
    (with-temp-buffer
      (ek9-list-mode)
      (setq ek9-list--resource (ek9-resource-get 'pods))
      (ek9-list--render)
      (should-not (string-match-p "No pods" (ek9-test--empty-text))))))

(ert-deftest ek9-polish-tab-line-shows-cluster ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (should (equal (ek9-context-cluster) "kind-dev"))
    (should (equal (ek9-context-cluster "prod") "prod"))
    (should-not (ek9-context-cluster "nocluster"))
    (setq ek9-context--current "nocluster")
    (let ((case-fold-search nil))
      (should-not (string-match-p "Cluster:"
                                  (substring-no-properties
                                   (ek9-context-header-line)))))
    (setq ek9-context--current "dev")
    (should (string-match-p "Context: dev  Cluster: kind-dev  Namespace: team-a"
                            (substring-no-properties
                             (ek9-context-header-line))))))

(ert-deftest ek9-polish-kubectl-check ()
  (let ((ek9-kubectl-executable "/nonexistent/kubectl"))
    (should (string-match-p
             "Cannot find kubectl (/nonexistent/kubectl)"
             (cadr (should-error (ek9) :type 'user-error)))))
  (ek9-test--with-clean-context
    (let ((ek9-kubectl-executable "/nonexistent/kubectl"))
      (should-error (ek9-context-switch) :type 'user-error)))
  (ek9-test--with-fake-kubectl
    (should-not (ek9-process-check-kubectl))))

(ert-deftest ek9-polish-header-follows-width ()
  (let ((long "argocd-applicationset-controller-6c8b9d5f4-x2xkq"))
    (ek9-test--in-pods-list (list (ek9-test--pod-named "short"))
      (let ((old (format "%S" header-line-format)))
        (ek9-list--receive (ek9-test--pods-table (list (ek9-test--pod-named long))))
        (should-not (equal (format "%S" header-line-format) old))))))

(ert-deftest ek9-polish-running-pod-is-green ()
  (should (eq (face-attribute 'ek9-status-ok :inherit) 'success)))

(provide 'ek9-polish-test)

;;; ek9-polish-test.el ends here
