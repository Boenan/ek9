;;; ek9-test-helper.el --- Shared helpers for ek9 tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers that the ek9 test files share.

;;; Code:

(require 'cl-lib)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-proxy)
(require 'ek9-types)

(defconst ek9-test--fake-kubectl
  (expand-file-name "fixtures/fake-kubectl"
                    (file-name-directory
                     (macroexp-file-name)))
  "Path of the fake kubectl script.")

;; The fake kubectl runs the fake proxy in the same Emacs as the tests.
(setenv "EK9_TEST_EMACS" (expand-file-name invocation-name invocation-directory))

(defmacro ek9-test--with-fake-kubectl (&rest body)
  "Run BODY with `ek9-kubectl-executable' set to the fake script."
  (declare (indent 0))
  `(let ((ek9-kubectl-executable ek9-test--fake-kubectl))
     ,@body))

(defun ek9-test--wait (proc)
  "Wait until PROC and its sentinel are done.
PROC is a process or a job of `ek9-process-run'."
  (while (ek9-process-live-p proc)
    (accept-process-output (and (processp proc) proc) 0.05))
  ;; Let the sentinel run.
  (accept-process-output nil 0.05))

(defun ek9-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil, for TIMEOUT seconds at most.
TIMEOUT is 5 seconds by default.  Signal an error on timeout."
  (let ((end (+ (float-time) (or timeout 5))))
    (while (not (funcall predicate))
      (when (> (float-time) end)
        (error "Timeout while waiting in test"))
      (accept-process-output nil 0.05))))

(load (expand-file-name "fixtures/fake-proxy.el"
                       (file-name-directory (macroexp-file-name)))
      nil t)

(defun ek9-test--parse-table (table)
  "Return the exact JSON TABLE as `ek9-proxy-parse-table' gives it."
  (with-temp-buffer
    (insert (json-serialize table :null-object :null :false-object :false))
    (goto-char (point-min))
    (ek9-proxy-parse-table)))

(defun ek9-test--table (base)
  "Return the server table of the fixture BASE, as ek9 reads it.
The table is the same as the fake proxy gives.  BASE is a fixture name
without \".json\", for example \"pods\"."
  (let ((ek9-fake-proxy--dir (file-name-directory ek9-test--fake-kubectl)))
    (ek9-test--parse-table
     (ek9-fake-proxy--table (ek9-fake-proxy--read (concat base ".json")) base))))

(defun ek9-test--name-table (json)
  "Return a server table with a NAME column for the kubectl list JSON.
JSON is a string.  The table has the form that ek9 reads."
  (ek9-test--parse-table
   (ek9-fake-proxy--table (json-parse-string json :object-type 'alist
                                             :array-type 'array
                                             :null-object :null
                                             :false-object :false)
                          nil)))

(defun ek9-test--json (string)
  "Return STRING parsed as JSON, in the same format as ek9 uses."
  (json-parse-string string :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun ek9-test--pod (name)
  "Return the pod NAME from the pods fixture."
  (seq-find (lambda (item) (equal (ek9-resource-item-name item) name))
            (alist-get 'items (ek9-test--read-fixture "pods.json"))))

(defun ek9-test--read-fixture (name)
  "Return the parsed JSON of the fixture file NAME in test/fixtures."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name (file-name-directory ek9-test--fake-kubectl)))
    (json-parse-buffer :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object nil)))

(defmacro ek9-test--with-clean-context (&rest body)
  "Run BODY with empty ek9 context state and the fake kubectl.
`ek9-context-changed-hook' is empty in BODY.  The cache of the
discovered types is empty too."
  (declare (indent 0))
  `(let ((ek9-context--current nil)
         (ek9-context--namespace nil)
         (ek9-context--contexts nil)
         (ek9-context--clusters nil)
         (ek9-context--namespace-cache (make-hash-table :test #'equal))
         (ek9-context--last-namespace (make-hash-table :test #'equal))
         (ek9-types--cache (make-hash-table :test #'equal))
         (ek9-types--callbacks (make-hash-table :test #'equal))
         (ek9-types--jobs (make-hash-table :test #'equal))
         (ek9-context-changed-hook nil)
         (ek9-read-only-contexts nil))
     (ek9-test--with-fake-kubectl
       ,@body)))

(defun ek9-test--load-contexts ()
  "Load the contexts from the fake kubeconfig and wait."
  (let (done)
    (ek9-context-load (lambda () (setq done t)))
    (ek9-test--wait-until (lambda () done))))

(defun ek9-test--load-types ()
  "Load the discovered types of the current context from the fake kubectl."
  (let (done)
    (ek9-types-ensure (lambda () (setq done t)))
    (ek9-test--wait-until (lambda () done))))

(defmacro ek9-test--with-watch-dir (var &rest body)
  "Run BODY with VAR bound to a new directory for fake watch events.
The fake proxy reads the events from this directory, see
test/fixtures/fake-proxy.el.  The environment of BODY gives a new
proxy.  Stop this proxy and delete the directory after BODY."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "ek9-watch-" t))
          (process-environment (cons (concat "EK9_FAKE_WATCH_DIR=" ,var)
                                     process-environment)))
     (unwind-protect
         (progn ,@body)
       (dolist (proxy (hash-table-values ek9-proxy--proxies))
         (when (member (concat "EK9_FAKE_WATCH_DIR=" ,var)
                       (nth 2 (ek9-proxy-key proxy)))
           (ek9-proxy--stop proxy "test")))
       (delete-directory ,var t))))

(defun ek9-test--put-watch-file (dir name &optional content)
  "Put the file NAME with CONTENT in the watch directory DIR.
Write a hidden file first, then rename it, so that the fake proxy
never reads a part of the file."
  (let ((temp (expand-file-name (concat "." name) dir)))
    (write-region (or content "") nil temp nil 'silent)
    (rename-file temp (expand-file-name name dir))))

(defun ek9-test--watch-event (type object &optional rv cells)
  "Return the JSON text of a fake watch event of TYPE for OBJECT.
OBJECT is an alist.  RV is the resourceVersion of the event table.
CELLS is a vector with the cells of the row."
  (json-serialize `((type . ,type) (object . ,object)
                    ,@(when rv `((rv . ,rv)))
                    ,@(when cells `((cells . ,cells))))))

(defun ek9-test--watch-requests (dir)
  "Return the watch paths that the fake proxy got for the directory DIR."
  (let ((file (expand-file-name "requests" dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t)))))

(defun ek9-test--switch-with-answer (command answer)
  "Run COMMAND and answer ANSWER to the minibuffer prompt.
Wait until the prompt was answered."
  (let (asked)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (setq asked t) answer)))
      (funcall command)
      (ek9-test--wait-until (lambda () asked)))))

(provide 'ek9-test-helper)

;;; ek9-test-helper.el ends here
