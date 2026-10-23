;;; ek9-resource-types-test.el --- Tests for ek9 resource types -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the resource types and their drill-down functions.

;;; Code:

(require 'ert)
(require 'ek9-resource)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

;;;; Registry

(ert-deftest ek9-resource-types-are-defined ()
  (dolist (name '(pods deployments statefulsets daemonsets jobs cronjobs
                  services configmaps secrets events nodes namespaces))
    (should (ek9-resource-p (ek9-resource-get name))))
  (should-not (ek9-resource-namespaced (ek9-resource-get 'nodes)))
  (should-not (ek9-resource-namespaced (ek9-resource-get 'namespaces)))
  (should (member "deploy" (ek9-resource-aliases (ek9-resource-get 'deployments)))))

(ert-deftest ek9-resource-column-string-default-and-newlines ()
  (let ((column '("X" 5 (value) :default "<none>")))
    (should (equal (ek9-resource-column-string column '((value . ""))) "<none>"))
    (should (equal (ek9-resource-column-string column nil) "<none>"))
    (should (equal (ek9-resource-column-string column '((value . "a\nb")))
                   "a b"))))

;;;; Drill-down

(ert-deftest ek9-resource-label-selector-syntax ()
  (should (equal (ek9-resource-label-selector
                  (ek9-test--json "{\"matchLabels\": {\"app\": \"web\", \"tier\": \"fe\"},
                     \"matchExpressions\": [
                       {\"key\": \"env\", \"operator\": \"In\", \"values\": [\"a\", \"b\"]},
                       {\"key\": \"x\", \"operator\": \"NotIn\", \"values\": [\"c\"]},
                       {\"key\": \"y\", \"operator\": \"Exists\"},
                       {\"key\": \"z\", \"operator\": \"DoesNotExist\"}]}"))
                 "app=web,tier=fe,env in (a,b),x notin (c),y,!z"))
  (should (equal (ek9-resource-label-selector
                  (ek9-test--json "{\"matchLabels\": {\"app\": \"\"}}"))
                 "app="))
  (should (equal (ek9-resource-label-selector
                  (ek9-test--json "{\"matchExpressions\": [
                     {\"key\": \"y\", \"operator\": \"Exists\"}]}"))
                 "y"))
  (should-error (ek9-resource-label-selector
                 (ek9-test--json "{\"matchExpressions\": [
                    {\"key\": \"y\", \"operator\": \"Gt\"}]}"))
                :type 'user-error)
  (should-not (ek9-resource-label-selector nil))
  (should-not (ek9-resource-label-selector (ek9-test--json "{\"matchLabels\": {}}"))))

(ert-deftest ek9-resource-drill-workload-uses-selector ()
  (should (equal (ek9-resource-drill-workload
                  (ek9-test--json "{\"metadata\": {\"name\": \"web\", \"namespace\": \"team-a\"},
                     \"spec\": {\"selector\": {\"matchLabels\": {\"app\": \"web\"}}}}"))
                 '(:resource pods :args ("--selector" "app=web")
                   :label "app=web" :namespace "team-a"))))

(ert-deftest ek9-resource-drill-empty-selector-is-error ()
  (should-error (ek9-resource-drill-workload
                 (ek9-test--json "{\"metadata\": {\"name\": \"w\", \"namespace\": \"a\"},
                    \"spec\": {\"selector\": {}}}"))
                :type 'user-error)
  (should-error (ek9-resource-drill-service
                 (ek9-test--json "{\"metadata\": {\"name\": \"ext\", \"namespace\": \"a\"},
                    \"spec\": {\"type\": \"ExternalName\"}}"))
                :type 'user-error))

(ert-deftest ek9-resource-drill-down-functions ()
  (pcase-dolist (`(,name . ,function)
                 '((deployments . ek9-resource-drill-deployment)
                   (replicasets . ek9-resource-drill-owned)
                   (statefulsets . ek9-resource-drill-owned)
                   (daemonsets . ek9-resource-drill-owned)
                   (jobs . ek9-resource-drill-owned)
                   (services . ek9-resource-drill-service)
                   (nodes . ek9-resource-drill-node)
                   (namespaces . ek9-resource-drill-namespace)
                   (pods . ek9-resource-drill-pod)
                   (cronjobs . ek9-resource-drill-cronjob)
                   (events . nil)))
    (should (eq (ek9-resource-drill-down (ek9-resource-get name)) function))))

(ert-deftest ek9-resource-drill-owned-adds-owner ()
  (let ((spec (ek9-resource-drill-owned
               (ek9-test--json "{\"kind\": \"StatefulSet\",
                  \"metadata\": {\"name\": \"db\", \"namespace\": \"a\", \"uid\": \"u1\"},
                  \"spec\": {\"selector\": {\"matchLabels\": {\"app\": \"db\"}}}}"))))
    (should (equal (plist-get spec :args) '("--selector" "app=db")))
    (should (equal (plist-get spec :owner-uid) "u1"))
    (should (equal (plist-get spec :owner) "statefulset/db"))))

(ert-deftest ek9-resource-controller-uid ()
  (should (equal (ek9-resource-controller-uid
                  (ek9-test--json "{\"metadata\": {\"ownerReferences\": [
                     {\"uid\": \"other\", \"controller\": false},
                     {\"uid\": \"u1\", \"controller\": true}]}}"))
                 "u1"))
  (should-not (ek9-resource-controller-uid
               (ek9-test--json "{\"metadata\": {\"name\": \"p\"}}"))))

(ert-deftest ek9-resource-drill-service-uses-selector ()
  (should (equal (plist-get
                  (ek9-resource-drill-service
                   (ek9-test--json "{\"metadata\": {\"name\": \"web\", \"namespace\": \"a\"},
                      \"spec\": {\"selector\": {\"app\": \"web\"}}}"))
                  :args)
                 '("--selector" "app=web"))))

(ert-deftest ek9-resource-drill-node-uses-field-selector ()
  (should (equal (ek9-resource-drill-node
                  (ek9-test--json "{\"metadata\": {\"name\": \"n1\"}}"))
                 '(:resource pods :args ("--field-selector" "spec.nodeName=n1")
                   :label "node=n1" :namespace all))))

(ert-deftest ek9-resource-drill-namespace-switches ()
  (should (equal (ek9-resource-drill-namespace
                  (ek9-test--json "{\"metadata\": {\"name\": \"team-a\"}}"))
                 '(:resource pods :switch-namespace "team-a"))))

(provide 'ek9-resource-types-test)

;;; ek9-resource-types-test.el ends here
