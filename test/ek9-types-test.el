;;; ek9-types-test.el --- Tests for the ek9 discovered types -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the discovered types.  The fixtures come from the kind
;; cluster of the integration tests: test/integration/crds.yaml and
;; test/integration/custom-resources.yaml.  The expected cell texts
;; are the texts of "kubectl get" on that cluster.

;;; Code:

(require 'ert)
(require 'ek9)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defmacro ek9-test--with-crds (&rest body)
  "Run BODY in context dev with the CRD types of the fake kubectl.
Kill all list buffers and stop the timer after BODY."
  (declare (indent 0))
  `(ek9-test--with-clean-context
     (ek9-test--load-contexts)
     (ek9-test--load-types)
     (unwind-protect
         (progn ,@body)
       (mapc #'kill-buffer (ek9-list--buffers))
       (when (timerp ek9-list--timer)
         (cancel-timer ek9-list--timer)
         (setq ek9-list--timer nil)))))

(defun ek9-test--crd-fixture (name)
  "Return the fixture NAME in the exact JSON format."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name (file-name-directory ek9-test--fake-kubectl)))
    (json-parse-buffer :object-type 'alist :array-type 'array
                       :null-object :null :false-object :false)))

(defun ek9-test--open-crd-list (name)
  "Open the list of the CRD type NAME in namespace ek9-itest.
Wait until the rows are loaded.  Return the buffer."
  (ek9-context-set-namespace "ek9-itest")
  (let ((buffer (ek9-list-open name)))
    (ek9-test--wait-until
     (lambda () (buffer-local-value 'ek9-list--loaded buffer)))
    buffer))

(defun ek9-test--crd-row (name)
  "Return an alist of (TITLE . TEXT) for the row NAME of this buffer."
  (let ((entry (seq-find (lambda (entry) (equal (aref (cadr entry) 0) name))
                         tabulated-list-entries)))
    (cl-mapcar (lambda (column text) (cons (car column) (substring-no-properties text)))
               ek9-list--columns
               (append (cadr entry) nil))))

;;;; Types

(ert-deftest ek9-types-parse ()
  (let* ((types (ek9-types-parse (ek9-test--read-fixture "api-resources.json")))
         (widgets (alist-get 'widgets.ek9.example.com types))
         (gizmos (alist-get 'gizmos.ek9.example.com types)))
    ;; The registry defines pods and deployments.  The built-in types
    ;; get their plural as their name, if it is free.
    (should (equal (mapcar #'car types)
                   '(componentstatuses events.events.k8s.io
                     gizmos.ek9.example.com ingresses nodemetrics
                     persistentvolumeclaims sprockets.ek9.example.com
                     widgets.ek9.example.com widgets.other.example.com)))
    (should (equal (ek9-resource-kind widgets) "widgets.v1.ek9.example.com"))
    (should (ek9-resource-namespaced widgets))
    (should-not (ek9-resource-namespaced gizmos))
    (should (equal (ek9-resource-api widgets) '("ek9.example.com" "v1" "widgets")))
    (should (equal (ek9-resource-actions widgets)
                   '(describe yaml edit delete events)))
    ;; Two CRDs have the plural "widgets": neither gets it.
    (should (equal (ek9-resource-aliases widgets) '("wdg")))
    ;; "po" is a short name of pods.
    (should (equal (ek9-resource-aliases
                    (alist-get 'widgets.other.example.com types))
                   '("ow")))
    (should (equal (ek9-resource-aliases gizmos) '("gizmos" "gizmo")))))

(ert-deftest ek9-types-parse-builtin-types ()
  (let* ((types (ek9-types-parse (ek9-test--read-fixture "api-resources.json")))
         (pvc (alist-get 'persistentvolumeclaims types))
         (ingresses (alist-get 'ingresses types))
         (status (alist-get 'componentstatuses types))
         (events (alist-get 'events.events.k8s.io types)))
    ;; kubectl reads a type of the core group without a group.
    (should (equal (ek9-resource-kind pvc) "persistentvolumeclaims"))
    (should (equal (ek9-resource-api pvc) '("" "v1" "persistentvolumeclaims")))
    (should (equal (ek9-resource-aliases pvc) '("persistentvolumeclaim" "pvc")))
    (should (equal (ek9-resource-kind ingresses) "ingresses.v1.networking.k8s.io"))
    (should (equal (ek9-resource-aliases ingresses) '("ingress" "ing")))
    ;; The actions come from the verbs.
    (should (equal (ek9-resource-actions status) '(describe yaml events)))
    (should (equal (ek9-resource-actions pvc) '(describe yaml edit delete events)))
    ;; The registry has "events" and "ev": the other events type gets
    ;; its full name and no aliases.
    (should (equal (ek9-resource-api events) '("events.k8s.io" "v1" "events")))
    (should-not (ek9-resource-aliases events))))

(ert-deftest ek9-types-naming-rules ()
  (let* ((ek9-saved-views '(("ing" :resource pods)))
         (json (ek9-test--json "{\"resources\": [
  {\"name\": \"deployments\", \"group\": \"apps\", \"version\": \"v1beta1\",
   \"namespaced\": true, \"verbs\": [\"list\"]},
  {\"name\": \"ev\", \"version\": \"v1\", \"namespaced\": true, \"verbs\": [\"list\"]},
  {\"name\": \"ingresses\", \"group\": \"networking.k8s.io\", \"version\": \"v1\",
   \"namespaced\": true, \"singularName\": \"ingress\", \"shortNames\": [\"ing\"],
   \"verbs\": [\"list\"]},
  {\"name\": \"ingresses\", \"group\": \"extensions\", \"version\": \"v1beta1\",
   \"namespaced\": true, \"verbs\": [\"list\"]},
  {\"name\": \"ingresses\", \"group\": \"foo.example.com\", \"version\": \"v1\",
   \"namespaced\": true, \"shortNames\": [\"pvc2\", \"persistentvolumeclaims\"],
   \"verbs\": [\"list\"]},
  {\"name\": \"persistentvolumeclaims\", \"version\": \"v1\", \"namespaced\": true,
   \"singularName\": \"persistentvolumeclaim\", \"verbs\": [\"list\"]}]}"))
         (types (ek9-types-parse json)))
    ;; The registry defines deployments in all versions.  The first
    ;; built-in ingresses gets the plural.  A CRD never takes it, so
    ;; that a saved view keeps its type in each context.  A core plural
    ;; that a registry short name uses gets PLURAL.core.
    (should (equal (mapcar #'car types)
                   '(ev.core ingresses ingresses.extensions
                     ingresses.foo.example.com persistentvolumeclaims)))
    (should (equal (ek9-resource-kind (alist-get 'ev.core types)) "ev"))
    ;; "ing" is a saved view, and "ingresses" is a type name.
    (should (equal (ek9-resource-aliases (alist-get 'ingresses types)) '("ingress")))
    ;; An alias cannot be the name of another type.
    (should (equal (ek9-resource-aliases (alist-get 'ingresses.foo.example.com types))
                   '("pvc2")))
    ;; A type whose plural a registry type uses gets no aliases.
    (should-not (ek9-resource-aliases (alist-get 'ev.core types)))))

(ert-deftest ek9-types-resource-get-uses-current-context ()
  (ek9-test--with-crds
    (should (eq (ek9-resource-name (ek9-resource-get 'gizmos.ek9.example.com))
                'gizmos.ek9.example.com))
    (setq ek9-context--current "prod")
    (should-error (ek9-resource-get 'gizmos.ek9.example.com))))

(ert-deftest ek9-types-command-choices ()
  (ek9-test--with-crds
    (let ((choices (ek9-list--resource-choices)))
      (should (equal (cdr (assoc "wdg" choices)) 'widgets.ek9.example.com))
      (should (equal (cdr (assoc "widgets.ek9.example.com" choices))
                     'widgets.ek9.example.com))
      (should (equal (cdr (assoc "po" choices)) 'pods))
      (should-not (assoc "widgets" choices))
      (should (equal (cdr (assoc "ingresses" choices)) 'ingresses))
      (should (equal (cdr (assoc "pvc" choices)) 'persistentvolumeclaims))
      (should (equal (cdr (assoc "events" choices)) 'events)))
    (should (equal (ek9-list-parse-command "wdg -A")
                   '(resource widgets.ek9.example.com all)))
    (should-error (ek9-list-parse-command "gizmo default") :type 'user-error)))

(ert-deftest ek9-types-completion-sees-new-types ()
  ;; The table without CHOICES reads the types on each call.
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (let ((table (ek9-list--command-table)))
      (should-not (all-completions "wd" table))
      (ek9-test--load-types)
      (should (equal (all-completions "wd" table) '("wdg"))))))

(ert-deftest ek9-types-load-failure ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (setq ek9-context--current "forbidden")
    (let (messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) messages))))
        (ek9-test--load-types))
      (should (string-match-p "Cannot load the types of the cluster: .*Forbidden"
                              (car messages)))
      ;; The empty result stays in the cache: no new load.
      (should (ek9-types--loaded-p "forbidden"))
      (should-not (ek9-types-prefetch))
      ;; A refresh loads them again.
      (cl-letf (((symbol-function 'message) #'ignore))
        (let ((job (ek9-types-prefetch t)))
          (should (ek9-process-live-p job))
          (ek9-test--wait job))))))

(ert-deftest ek9-types-no-types ()
  (ek9-test--with-clean-context
    (setq ek9-context--current "no-crds")
    (ek9-test--load-types)
    (should (ek9-types--loaded-p "no-crds"))
    (should-not (ek9-types-current))))

(ert-deftest ek9-types-refresh-replaces-types ()
  (ek9-test--with-crds
    (let ((old (ek9-resource-get 'widgets.ek9.example.com)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (ek9-test--wait (ek9-types-prefetch t)))
      (should-not (eq old (ek9-resource-get 'widgets.ek9.example.com))))))

;;;; Columns

(ert-deftest ek9-types-list-shows-kubectl-values ()
  (ek9-test--with-crds
    (with-current-buffer (ek9-test--open-crd-list 'widgets.ek9.example.com)
      (should (equal (ek9-list--request-path)
                     (concat "/apis/ek9.example.com/v1/namespaces/ek9-itest/widgets"
                             "?includeObject=Object")))
      (should (equal (length tabulated-list-entries) 4))
      (let ((full (ek9-test--crd-row "full"))
            (falsy (ek9-test--crd-row "falsy"))
            (mixed (ek9-test--crd-row "mixed"))
            (empty (ek9-test--crd-row "empty")))
        (should (equal (butlast full 2)
                       '(("NAME" . "full") ("PHASE" . "Running") ("REPLICAS" . "3")
                         ("RATIO" . "1.5") ("ENABLED" . "true") ("READY" . "True")
                         ("HOSTS" . "a.example.com") ("FIRST HOST" . "a.example.com")
                         ("LABELS" . "{\"tier\":\"front\"}") ("OWNER" . "team-a"))))
        (should (string-match-p "\\`[0-9]+d\\'" (cdr (assoc "STARTED" full))))
        (should (equal (butlast falsy 1)
                       '(("NAME" . "falsy") ("PHASE" . "false") ("REPLICAS" . "2")
                         ("RATIO" . "0.1") ("ENABLED" . "false") ("READY" . "")
                         ("HOSTS" . "only.example.com")
                         ("FIRST HOST" . "only.example.com")
                         ("LABELS" . "{\"a\":\"1\",\"b\":\"2\"}")
                         ("OWNER" . "true") ("STARTED" . "<unknown>"))))
        (should (equal (butlast mixed 1)
                       '(("NAME" . "mixed")
                         ("PHASE" . "{\"nested\":[1,2.5,null,false],\"z\":\"\\u003ca\\u0026b\\u003e\"}")
                         ("REPLICAS" . "") ("RATIO" . "2") ("ENABLED" . "")
                         ("READY" . "") ("HOSTS" . "") ("FIRST HOST" . "")
                         ("LABELS" . "") ("OWNER" . "42") ("STARTED" . "<invalid>"))))
        (should (equal (mapcar #'cdr (cdr (butlast empty 1)))
                       (make-list 10 "")))))))

(ert-deftest ek9-types-sort-by-number-column ()
  (ek9-test--with-crds
    (with-current-buffer (ek9-test--open-crd-list 'widgets.ek9.example.com)
      (setq tabulated-list-sort-key '("RATIO" . nil))
      (ek9-list--render)
      ;; An empty cell sorts after the numbers.
      (goto-char (point-min))
      (let (names)
        (while (not (eobp))
          (push (substring-no-properties (aref (tabulated-list-get-entry) 0)) names)
          (forward-line 1))
        (should (equal (nreverse names) '("falsy" "full" "mixed" "empty")))))))

;;;; Review fixes

(ert-deftest ek9-types-saved-view-names-are-not-aliases ()
  (let ((ek9-saved-views '(("wdg" :resource pods))))
    (should (equal (ek9-resource-aliases
                    (alist-get 'widgets.ek9.example.com
                               (ek9-types-parse
                                (ek9-test--read-fixture "api-resources.json"))))
                   nil))))

(ert-deftest ek9-types-callbacks-of-each-context ()
  (ek9-test--with-clean-context
    (ek9-test--load-contexts)
    (let (log)
      (setq ek9-context--current "prod")
      (ek9-types-ensure (lambda ()
                        (push (list 'prod (ek9-types--loaded-p "prod")) log)
                        (error "Broken callback")))
      (ek9-types-ensure (lambda () (push 'prod-2 log)))
      (setq ek9-context--current "dev")
      (ek9-types-ensure (lambda () (push (list 'dev (ek9-types--loaded-p "dev")) log)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (ek9-test--wait-until (lambda () (= (length log) 3))))
      ;; Each function runs after the load of its own context.  An
      ;; error does not stop the next function.
      (should (member '(prod t) log))
      (should (member '(dev t) log))
      (should (memq 'prod-2 log)))))

(ert-deftest ek9-types-context-switch-updates-list ()
  (ek9-test--with-crds
    (let* ((ek9-context-changed-hook (list #'ek9-types--context-changed
                                           #'ek9-list--context-changed))
           (buffer (ek9-test--open-crd-list 'widgets.ek9.example.com))
           (old (buffer-local-value 'ek9-list--resource buffer)))
      (ek9-context-set "prod")
      (ek9-test--wait-until (lambda () (ek9-types--loaded-p "prod")))
      (let ((new (buffer-local-value 'ek9-list--resource buffer)))
        (should-not (eq new old))
        (should (eq new (alist-get 'widgets.ek9.example.com
                                   (gethash "prod" ek9-types--cache)))))
      (with-current-buffer buffer
        (ek9-test--wait-until (lambda () ek9-list--loaded))
        ;; The list reads through the proxy of the new context.
        (should (gethash (ek9-proxy--key "prod") ek9-proxy--proxies))
        (should (equal (length ek9-list--columns) 12))))))

(ert-deftest ek9-types-context-switch-to-cached-types ()
  ;; The types of prod are in the cache: the switch only changes the
  ;; type object, and `ek9-list--context-changed' refreshes the list.
  (ek9-test--with-crds
    (setq ek9-context--current "prod")
    (ek9-test--load-types)
    (setq ek9-context--current "dev")
    (let* ((ek9-context-changed-hook (list #'ek9-types--context-changed
                                           #'ek9-list--context-changed))
           (buffer (ek9-test--open-crd-list 'widgets.ek9.example.com))
           (refreshes 0))
      (cl-letf* ((refresh (symbol-function 'ek9-list-refresh))
                 ((symbol-function 'ek9-list-refresh)
                  (lambda (&rest args)
                    (cl-incf refreshes)
                    (apply refresh args))))
        (ek9-context-set "prod"))
      (should (eq (buffer-local-value 'ek9-list--resource buffer)
                  (alist-get 'widgets.ek9.example.com (gethash "prod" ek9-types--cache))))
      (should (= refreshes 1)))))

(ert-deftest ek9-types-reload-with-sort-by-printer-column ()
  ;; A sort by a column that the new type does not have yet must not
  ;; break the reload.
  (ek9-test--with-crds
    (with-current-buffer (ek9-test--open-crd-list 'widgets.ek9.example.com)
      (setq tabulated-list-sort-key '("RATIO" . nil))
      (ek9-list--render)
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args)
                     (push (apply #'format format args) messages))))
          (ek9-test--wait (ek9-types-prefetch t)))
        (should (equal messages '("ek9: Loaded 9 types"))))
      (should (= (length (ek9-types-current)) 9))
      (should (eq ek9-list--resource (ek9-resource-get 'widgets.ek9.example.com)))
      (should-not tabulated-list-sort-key)
      (ek9-test--wait-until
       (lambda () (and ek9-list--loaded (= (length ek9-list--columns) 12)))))))

;;;; Wide columns

(ert-deftest ek9-types-toggle-wide-hides-wide-columns ()
  (ek9-test--with-crds
    (with-current-buffer (ek9-test--open-crd-list 'widgets.ek9.example.com)
      (should ek9-list--wide)
      (should (assoc "OWNER" ek9-list--columns))
      (cl-letf (((symbol-function 'message) #'ignore))
        (ek9-list-toggle-wide)
        (should-not ek9-list--wide)
        (should-not (assoc "OWNER" ek9-list--columns))
        (should (equal (length (cadr (car tabulated-list-entries))) 11))
        (ek9-list-toggle-wide))
      (should (assoc "OWNER" ek9-list--columns)))))

(ert-deftest ek9-types-toggle-wide-resets-sort-by-wide-column ()
  (ek9-test--with-crds
    (with-current-buffer (ek9-test--open-crd-list 'widgets.ek9.example.com)
      (setq tabulated-list-sort-key '("OWNER" . nil))
      (ek9-list--render)
      (cl-letf (((symbol-function 'message) #'ignore))
        (ek9-list-toggle-wide))
      (should-not tabulated-list-sort-key)
      (should (= (length tabulated-list-entries) 4)))))

(ert-deftest ek9-types-wide-option-sets-new-buffers ()
  (ek9-test--with-crds
    (let ((ek9-list-wide nil))
      (with-current-buffer (ek9-test--open-crd-list 'widgets.ek9.example.com)
        (should-not ek9-list--wide)
        (should-not (assoc "OWNER" ek9-list--columns))))))

(provide 'ek9-types-test)

;;; ek9-types-test.el ends here
