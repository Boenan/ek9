;;; ek9-status-test.el --- Tests for the ek9 status colors -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the row faces of the resource types and for the
;; colors in list buffers.

;;; Code:

(require 'ert)
(require 'ek9-list)
(require 'ek9-test-helper
         (expand-file-name "ek9-test-helper.el"
                           (file-name-directory
                            (macroexp-file-name))))

(defun ek9-test--pod-row (status ready &optional phase)
  "Return a pod item of a server table with the STATUS and READY cells.
PHASE is the pod phase, \"Running\" by default."
  `((ek9-cells . [,status ,ready])
    (ek9-titles . ["STATUS" "READY"])
    (metadata (name . "p") (namespace . "a"))
    (status (phase . ,(or phase "Running")))))

(ert-deftest ek9-status-pod-faces ()
  (should (eq (ek9-resource-pod-face (ek9-test--pod-row "Running" "1/1"))
              'ek9-status-ok))
  (should (eq (ek9-resource-pod-face (ek9-test--pod-row "Running" "0/1"))
              'ek9-status-warning))
  (dolist (reason '("CrashLoopBackOff" "ImagePullBackOff" "ErrImagePull"
                    "CreateContainerConfigError"))
    (should (eq (ek9-resource-pod-face (ek9-test--pod-row reason "0/1"))
                'ek9-status-error)))
  (should (eq (ek9-resource-pod-face (ek9-test--pod-row "ContainerCreating" "0/1"))
              'ek9-status-warning))
  (should (eq (ek9-resource-pod-face
               (ek9-test--pod-row "Completed" "0/1" "Succeeded"))
              'ek9-status-done))
  (should (eq (ek9-resource-pod-face (ek9-test--pod-row "Error" "0/1" "Failed"))
              'ek9-status-error))
  ;; A pod without cells, for example outside a table, has no problem.
  (should (eq (ek9-resource-pod-face '((status (phase . "Running"))))
              'ek9-status-ok)))

(ert-deftest ek9-status-replicas-faces ()
  (should (eq (ek9-resource-replicas-face
               '((spec (replicas . 3)) (status (readyReplicas . 3))))
              'ek9-status-ok))
  (should (eq (ek9-resource-replicas-face
               '((spec (replicas . 3)) (status (readyReplicas . 1))))
              'ek9-status-warning))
  (should (eq (ek9-resource-replicas-face '((spec (replicas . 0))))
              'ek9-status-ok))
  (should (eq (ek9-resource-daemonset-face
               '((status (numberReady . 1) (desiredNumberScheduled . 2))))
              'ek9-status-warning)))

(ert-deftest ek9-status-job-faces ()
  (cl-flet ((job (type)
              `((status (conditions ((type . ,type) (status . "True")))))))
    (should (eq (ek9-resource-job-face (job "Complete")) 'ek9-status-done))
    (should (eq (ek9-resource-job-face (job "Failed")) 'ek9-status-error))
    (should (eq (ek9-resource-job-face (job "Suspended")) 'ek9-status-warning))
    (should (eq (ek9-resource-job-face '((status))) 'ek9-status-ok))))

(ert-deftest ek9-status-other-faces ()
  (should (eq (ek9-resource-cronjob-face '((spec (suspend . t))))
              'ek9-status-done))
  (should (eq (ek9-resource-event-face '((type . "Warning")))
              'ek9-status-warning))
  (should (eq (ek9-resource-event-face '((type . "Normal")))
              'ek9-status-ok))
  (should (eq (ek9-resource-node-face
               '((status (conditions ((type . "Ready") (status . "False"))))))
              'ek9-status-error))
  (should (eq (ek9-resource-node-face
               '((spec (unschedulable . t))
                 (status (conditions ((type . "Ready") (status . "True"))))))
              'ek9-status-warning))
  (should (eq (ek9-resource-namespace-face '((status (phase . "Terminating"))))
              'ek9-status-warning)))

(ert-deftest ek9-status-every-type-has-face ()
  (dolist (name '(pods deployments statefulsets daemonsets jobs cronjobs
                  events nodes namespaces))
    (should (functionp (ek9-resource-row-face (ek9-resource-get name))))))

(defun ek9-test--pod-face-for-status (status &optional phase)
  "Return the row face of a pod whose STATUS cell is STATUS.
PHASE is the pod phase, \"Running\" by default."
  (ek9-resource-pod-face (ek9-test--pod-row status "1/1" phase)))

(ert-deftest ek9-status-pod-status-values ()
  (pcase-dolist (`(,face . ,values)
                 '((ek9-status-warning "Init:0/1" "Terminating" "NotReady"
                                       "Pending" "ContainerCreating"
                                       "SchedulingGated")
                   (ek9-status-error "Init:CrashLoopBackOff" "Init:Error"
                                     "Signal:9" "ExitCode:1" "ExitCode:-1"
                                     "Evicted" "UnexpectedAdmissionError"
                                     "ContainerStatusUnknown" "Unknown"
                                     "OOMKilled" "ContainerCannotRun"
                                     "RegistryUnavailable")
                   (ek9-status-done "Completed" "ExitCode:0")
                   (ek9-status-ok "Running")))
    (dolist (value values)
      (should (equal (cons value (ek9-test--pod-face-for-status value))
                     (cons value face))))))

(ert-deftest ek9-status-pod-phase-wins ()
  ;; kubectl can show "Completed" or a scheduler reason for a failed pod.
  (should (eq (ek9-test--pod-face-for-status "NodeAffinity" "Failed")
              'ek9-status-error))
  (should (eq (ek9-test--pod-face-for-status "Completed" "Failed")
              'ek9-status-error))
  (should (eq (ek9-test--pod-face-for-status "Terminating" "Succeeded")
              'ek9-status-warning)))

(ert-deftest ek9-status-node-without-ready ()
  (should (eq (ek9-resource-node-face '((status))) 'ek9-status-error)))

(ert-deftest ek9-status-sorted-list-updates-colors ()
  (with-temp-buffer
    (ek9-list-mode)
    (setq ek9-list--resource (ek9-resource-get 'pods)
          tabulated-list-sort-key '("NAME"))
    (let ((table (ek9-test--table "pods")))
      (ek9-list--receive table)
      (should (text-property-any (point-min) (point-max)
                                 'face 'ek9-status-error))
      (let ((ek9-status-colors nil))
        (ek9-list--receive table)
        (should-not (text-property-any (point-min) (point-max)
                                       'face 'ek9-status-error)))
      ;; A status change of a row with the same ID shows the new text.
      (aset (alist-get 'cells (aref (alist-get 'rows table) 1)) 2 "Running")
      (ek9-list--receive table)
      (should-not (string-match-p "CrashLoopBackOff" (buffer-string))))))

(ert-deftest ek9-status-list-colors ()
  (with-temp-buffer
    (ek9-list-mode)
    (setq ek9-list--resource (ek9-resource-get 'pods))
    (ek9-list--receive (ek9-test--table "pods"))
    (should (eq (get-text-property
                 (string-match "CrashLoopBackOff" (buffer-string))
                 'face (buffer-string))
                'ek9-status-error))
    ;; The filter still matches the colored text.
    (ek9-list-filter "crashloop")
    (should (equal (mapcar #'car tabulated-list-entries)
                   '("default/api-5f9c-xyz")))
    ;; Without colors, the rows have no face.
    (ek9-list-filter "")
    (let ((ek9-status-colors nil))
      (ek9-list--render)
      (should-not (text-property-any (point-min) (point-max)
                                     'face 'ek9-status-error)))))

(provide 'ek9-status-test)

;;; ek9-status-test.el ends here
