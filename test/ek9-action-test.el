;;; ek9-action-test.el --- Tests for ek9-action -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the write actions.  The fake kubectl prints its
;; arguments, so the tests can check the command without a cluster.

;;; Code:

(require 'ert)
(require 'ek9-action)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--item (json)
  "Return the resource item in the JSON string."
  (json-parse-string json :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defconst ek9-test--deployment
  "{\"kind\": \"Deployment\",
    \"metadata\": {\"name\": \"web\", \"namespace\": \"team-a\"},
    \"spec\": {\"replicas\": 3}}"
  "JSON of a deployment for the tests.")

(defmacro ek9-test--with-action (answers &rest body)
  "Run BODY in context dev with ANSWERS for the prompts.
ANSWERS is a plist with the keys :yes, :string, and :number.  In
BODY, `prompts' is the list of prompts and `messages' is the list of
messages, newest first."
  (declare (indent 1))
  `(ek9-test--with-clean-context
     (setq ek9-context--current "dev"
           ek9-context--namespace "team-a")
     (let (prompts messages)
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (prompt)
                    (push prompt prompts)
                    (plist-get ,answers :yes)))
                 ((symbol-function 'read-string)
                  (lambda (prompt &rest _)
                    (push prompt prompts)
                    (plist-get ,answers :string)))
                 ((symbol-function 'read-number)
                  (lambda (prompt &rest _)
                    (push prompt prompts)
                    (plist-get ,answers :number)))
                 ((symbol-function 'message)
                  (lambda (format &rest args)
                    (push (apply #'format-message format args) messages))))
         ,@body))))

(defun ek9-test--wait-message (messages-function)
  "Wait until MESSAGES-FUNCTION returns a non-nil list, and return it."
  (ek9-test--wait-until messages-function)
  (funcall messages-function))

(ert-deftest ek9-action-delete-runs-kubectl ()
  (ek9-test--with-action '(:yes t)
    (ek9-delete (ek9-resource-get 'deployments)
                (ek9-test--item ek9-test--deployment))
    (should (equal prompts
                   '("Delete Deployment team-a/web in context dev? ")))
    (should (equal (car (ek9-test--wait-message (lambda () messages)))
                   (concat "ek9: delete deployments/web --wait=false "
                           "--namespace team-a --context dev")))))

(ert-deftest ek9-action-delete-cancelled ()
  (ek9-test--with-action '(:yes nil)
    (should-not (ek9-delete (ek9-resource-get 'deployments)
                            (ek9-test--item ek9-test--deployment)))
    (should (equal messages '("ek9: Cancelled")))))

(ert-deftest ek9-action-read-only-context-blocks ()
  (ek9-test--with-action '(:yes t)
    (setq ek9-read-only-contexts '("dev"))
    (should-error (ek9-delete (ek9-resource-get 'deployments)
                              (ek9-test--item ek9-test--deployment))
                  :type 'user-error)
    (should-error (ek9-restart (ek9-resource-get 'deployments)
                               (ek9-test--item ek9-test--deployment))
                  :type 'user-error)
    (should-error (ek9-scale (ek9-resource-get 'deployments)
                             (ek9-test--item ek9-test--deployment) 1)
                  :type 'user-error)
    (should-not prompts)))

(ert-deftest ek9-action-cluster-resource-needs-name ()
  (let ((node (ek9-test--item
               "{\"kind\": \"Node\", \"metadata\": {\"name\": \"n1\"}}")))
    (ek9-test--with-action '(:string "n2")
      (should-error (ek9-delete (ek9-resource-get 'nodes) node)
                    :type 'user-error)
      (should (equal prompts
                     '("Delete Node n1 in context dev? Type the name to confirm: "))))
    (ek9-test--with-action '(:string "n1 ")
      (should-error (ek9-delete (ek9-resource-get 'nodes) node)
                    :type 'user-error))
    (ek9-test--with-action '(:string "n1")
      (ek9-delete (ek9-resource-get 'nodes) node)
      (should (equal (car (ek9-test--wait-message (lambda () messages)))
                     "ek9: delete nodes/n1 --wait=false --context dev")))))

(ert-deftest ek9-action-namespace-needs-name ()
  (ek9-test--with-action '(:string "")
    (should-not (ek9-delete (ek9-resource-get 'namespaces)
                            (ek9-test--item
                             "{\"kind\": \"Namespace\", \"metadata\": {\"name\": \"team-a\"}}")))
    (should (equal prompts
                   '("Delete Namespace team-a in context dev? Type the name to confirm: ")))
    (should (equal messages '("ek9: Cancelled")))))

(ert-deftest ek9-action-no-context-blocks ()
  (ek9-test--with-action '(:yes t)
    (setq ek9-context--current nil)
    (should-error (ek9-delete (ek9-resource-get 'deployments)
                              (ek9-test--item ek9-test--deployment))
                  :type 'user-error)
    (should-not prompts)))

(ert-deftest ek9-action-scale-in-old-context-blocks ()
  (ek9-test--with-action '(:yes t)
    (should-error (ek9-scale (ek9-resource-get 'deployments)
                             (ek9-test--item ek9-test--deployment) 1 "prod")
                  :type 'user-error)
    (should-not prompts)))

(defmacro ek9-test--in-deployments-list (&rest body)
  "Run BODY in a list buffer of deployments with point on team-a/web."
  (declare (indent 0))
  `(with-temp-buffer
     (ek9-list-mode)
     (setq ek9-list--resource (ek9-resource-get 'deployments))
     (ek9-list--receive
      (ek9-test--name-table (concat "{\"items\": [" ek9-test--deployment "]}")))
     (goto-char (ek9-list--find-id "team-a/web"))
     ,@body))

(ert-deftest ek9-action-interactive-read-only-asks-nothing ()
  (ek9-test--with-action '(:yes t :number 1)
    (setq ek9-read-only-contexts '("dev"))
    (ek9-test--in-deployments-list
      (should-error (call-interactively #'ek9-scale) :type 'user-error)
      (should-not prompts))))

(ert-deftest ek9-action-not-available-for-type ()
  (ek9-test--with-action '(:yes t :number 1)
    (ek9-test--in-deployments-list
      (setq ek9-list--resource (ek9-resource-get 'cronjobs))
      (should (equal (cadr (should-error (call-interactively #'ek9-scale)
                                         :type 'user-error))
                     "ek9: Action scale is not available for cronjobs"))
      (should-not prompts))))

(ert-deftest ek9-action-refreshes-list ()
  (ek9-test--with-action '(:yes t)
    (ek9-test--in-deployments-list
      (let (refreshed)
        (cl-letf (((symbol-function 'ek9-list-refresh)
                   (lambda (buffer) (setq refreshed buffer))))
          (call-interactively #'ek9-restart)
          (ek9-test--wait-until (lambda () refreshed))
          (should (eq refreshed (current-buffer))))))))

(ert-deftest ek9-action-context-change-during-prompt ()
  (ek9-test--with-action '(:yes t)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) (setq ek9-context--current "prod") t)))
      (should-error (ek9-delete (ek9-resource-get 'deployments)
                                (ek9-test--item ek9-test--deployment))
                    :type 'user-error))))

(ert-deftest ek9-action-scale-runs-kubectl ()
  (ek9-test--with-action '(:yes t)
    (ek9-scale (ek9-resource-get 'deployments)
               (ek9-test--item ek9-test--deployment) 0)
    (should (equal prompts
                   '("Scale Deployment team-a/web from 3 to 0 replicas in context dev? ")))
    (should (equal (car (ek9-test--wait-message (lambda () messages)))
                   (concat "ek9: scale deployments/web --replicas=0 "
                           "--namespace team-a --context dev")))))

(ert-deftest ek9-action-read-replicas ()
  (ek9-test--with-action '(:number 5)
    (should (= (ek9-action--read-replicas 3) 5))
    (should (equal prompts '("Replicas (now 3): "))))
  (ek9-test--with-action '(:number -1)
    (should-error (ek9-action--read-replicas 3) :type 'user-error))
  (ek9-test--with-action '(:number 1.5)
    (should-error (ek9-action--read-replicas 3) :type 'user-error)))

(ert-deftest ek9-action-restart-runs-kubectl ()
  (ek9-test--with-action '(:yes t)
    (ek9-restart (ek9-resource-get 'deployments)
                 (ek9-test--item ek9-test--deployment))
    (should (equal (car (ek9-test--wait-message (lambda () messages)))
                   (concat "ek9: rollout restart deployments/web "
                           "--namespace team-a --context dev")))))

(ert-deftest ek9-action-error-message ()
  (ek9-test--with-action '(:yes t)
    (setq ek9-context--current "forbidden")
    (ek9-delete (ek9-resource-get 'deployments)
                (ek9-test--item ek9-test--deployment))
    (should (equal (car (ek9-test--wait-message (lambda () messages)))
                   (concat "ek9: Delete Deployment team-a/web failed: "
                           "Error from server (Forbidden): access denied")))))

(ert-deftest ek9-action-availability ()
  (should (memq 'delete (ek9-resource-actions (ek9-resource-get 'pods))))
  (should-not (memq 'scale (ek9-resource-actions (ek9-resource-get 'pods))))
  (should-not (memq 'restart (ek9-resource-actions (ek9-resource-get 'jobs))))
  (dolist (name '(deployments statefulsets))
    (should (memq 'scale (ek9-resource-actions (ek9-resource-get name)))))
  (dolist (name '(deployments statefulsets daemonsets))
    (should (memq 'restart (ek9-resource-actions (ek9-resource-get name)))))
  (should (eq (keymap-lookup ek9-list-mode-map "o") #'tabulated-list-sort))
  (dolist (key '("D" "S" "R"))
    (should (keymap-lookup ek9-list-mode-map key))))

(provide 'ek9-action-test)

;;; ek9-action-test.el ends here
