;;; ek9-portfwd.el --- Port-forward manager for ek9 -*- lexical-binding: t; -*-

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

;; This file starts "kubectl port-forward" processes and shows them in
;; a manager buffer.  "f" in a list buffer starts a port-forward for
;; the item at point.  `ek9-port-forwards' shows all port-forwards.
;; In the manager buffer, "D" stops the port-forward at point, or
;; removes it from the list if it is already stopped.
;;
;; A port-forward does not change the cluster, so read-only contexts
;; do not block it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)

(defcustom ek9-portfwd-address "localhost"
  "Local address on which port-forwards listen.
The value goes to the \"--address\" option of kubectl.  Use
\"0.0.0.0\" only if other hosts must connect: then all hosts on your
network can reach the cluster port."
  :type 'string
  :group 'ek9)

(cl-defstruct (ek9-portfwd--forward
               (:constructor ek9-portfwd--make-forward)
               (:copier nil))
  "A port-forward process and its state."
  context namespace target ports address process
  (status "Starting")
  local-port
  last-line
  (pending ""))

(defvar ek9-portfwd--forwards nil
  "List of port-forwards, newest first.")

(defconst ek9-portfwd--buffer-name "*ek9 port-forwards*"
  "Name of the port-forward manager buffer.")

;;;; Ports

(defun ek9-portfwd--item-ports (item)
  "Return the ports of ITEM as a list of numbers.
Use spec.ports for a service.  Use the container ports for a pod,
and the container ports of the pod template for a workload."
  (delete-dups
   (delq nil
         (or (mapcar (lambda (port) (alist-get 'port port))
                     (ek9-resource-path item '(spec ports)))
             (mapcan (lambda (container)
                       (mapcar (lambda (port) (alist-get 'containerPort port))
                               (alist-get 'ports container)))
                     (append (ek9-resource-path item '(spec containers))
                             (ek9-resource-path
                              item '(spec template spec containers))))))))

(defun ek9-portfwd--container-only (pod container)
  "Return a pod item that has only the CONTAINER of POD.
Use it to offer only the ports of CONTAINER."
  `((spec (containers
           ,@(seq-filter (lambda (spec) (equal (alist-get 'name spec) container))
                         (append (ek9-resource-path pod '(spec containers))
                                 (ek9-resource-path
                                  pod '(spec initContainers))))))))

(defun ek9-portfwd--valid-port-p (string &optional allow-empty)
  "Return non-nil if STRING is a port number from 1 to 65535.
If ALLOW-EMPTY is non-nil, also accept an empty STRING."
  (or (and allow-empty (string-empty-p string))
      (and (string-match-p "\\`[0-9]+\\'" string)
           (<= 1 (string-to-number string) 65535))))

(defun ek9-portfwd--check-ports (ports)
  "Return PORTS if it is a valid kubectl port argument.
PORTS has the form \"REMOTE\", \"LOCAL:REMOTE\", or \":REMOTE\".  An
empty LOCAL makes kubectl select a free local port.  Signal a
`user-error' if PORTS is not valid."
  (let ((parts (split-string ports ":")))
    (unless (and (<= 1 (length parts) 2)
                 (ek9-portfwd--valid-port-p (car (last parts)))
                 (or (null (cdr parts))
                     (ek9-portfwd--valid-port-p (car parts) t)))
      (user-error "ek9: Use the form LOCAL:REMOTE with ports from 1 to 65535, for example 8080:80"))
    ports))

(defun ek9-portfwd--port-choices (port)
  "Return the port choices for the remote PORT, the best choice first.
A normal user cannot listen on a local port below 1024, so for such a
PORT the first choice uses the local port PORT + 8000."
  (if (< port 1024)
      (list (format "%d:%d" (+ port 8000) port) (format "%d:%d" port port))
    (list (format "%d:%d" port port))))

(defun ek9-portfwd--read-ports (item)
  "Ask for the ports of a port-forward to ITEM and return them."
  (let* ((choices (mapcan #'ek9-portfwd--port-choices
                          (ek9-portfwd--item-ports item)))
         (default (car choices)))
    (ek9-portfwd--check-ports
     (completing-read (format-prompt "Ports (LOCAL:REMOTE)" default)
                      choices nil nil nil nil default))))

;;;; Processes

(defun ek9-portfwd--running-p (forward)
  "Return non-nil if the process of FORWARD is live."
  (process-live-p (ek9-portfwd--forward-process forward)))

(defun ek9-portfwd--ports-label (forward)
  "Return the ports of FORWARD as text.
When kubectl selected the local port, show the selected port."
  (let ((ports (ek9-portfwd--forward-ports forward))
        (local (ek9-portfwd--forward-local-port forward)))
    (if (and local (string-prefix-p ":" ports))
        (concat local ports)
      ports)))

(defun ek9-portfwd--label (forward)
  "Return a short description of FORWARD for messages."
  (format "%s %s on %s in context %s"
          (ek9-portfwd--forward-target forward)
          (ek9-portfwd--ports-label forward)
          (ek9-portfwd--forward-address forward)
          (ek9-portfwd--forward-context forward)))

(defun ek9-portfwd--line (forward line)
  "Handle one complete output LINE of the process of FORWARD."
  (setf (ek9-portfwd--forward-last-line forward) line)
  (let ((status (ek9-portfwd--forward-status forward)))
    (cond
     ((string-match "\\`Forwarding from [^ ]*:\\([0-9]+\\) -> " line)
      (unless (ek9-portfwd--forward-local-port forward)
        (setf (ek9-portfwd--forward-local-port forward) (match-string 1 line)))
      (when (equal status "Starting")
        (setf (ek9-portfwd--forward-status forward) "Running")
        (message "ek9: Port-forward runs: %s" (ek9-portfwd--label forward))))
     ;; kubectl keeps running after some errors, for example when the
     ;; container refuses a connection.
     ((and (equal status "Running")
           (string-match-p "\\`E[0-9]\\{4\\} \\|error" line))
      (setf (ek9-portfwd--forward-status forward) "Running (errors)")))))

(defun ek9-portfwd--output (forward text)
  "Handle the output TEXT of the process of FORWARD.
TEXT can end in the middle of a line.  Keep that part until the rest
of the line arrives."
  (let ((lines (split-string (concat (ek9-portfwd--forward-pending forward) text)
                             "\n")))
    (setf (ek9-portfwd--forward-pending forward) (car (last lines)))
    (dolist (line (butlast lines))
      (let ((line (string-trim line)))
        (unless (string-empty-p line)
          (ek9-portfwd--line forward line)))))
  (ek9-portfwd--refresh-manager))

(defun ek9-portfwd--exit (forward process)
  "Handle the end of PROCESS of FORWARD."
  (let ((rest (string-trim (ek9-portfwd--forward-pending forward))))
    (unless (string-empty-p rest)
      (ek9-portfwd--line forward rest)))
  (setf (ek9-portfwd--forward-status forward)
        (cond
         ((or (process-get process 'ek9-stopped)
              (and (eq (process-status process) 'exit)
                   (zerop (process-exit-status process))))
          "Stopped")
         (t
          (let ((reason (if (eq (process-status process) 'signal)
                            (format "killed (signal %d)"
                                    (process-exit-status process))
                          (or (ek9-portfwd--forward-last-line forward)
                              "no output"))))
            (message "ek9: Port-forward failed: %s: %s"
                     (ek9-portfwd--label forward) reason)
            (concat "Failed: " reason)))))
  (ek9-portfwd--refresh-manager))

(defun ek9-portfwd--args (resource item ports)
  "Return the kubectl arguments of a port-forward to ITEM of RESOURCE.
PORTS is the port argument, for example \"8080:80\"."
  (append (list "port-forward"
                (concat (ek9-resource-kind resource) "/"
                        (ek9-resource-item-name item))
                ports
                "--address" ek9-portfwd-address)
          (ek9-list-item-args item)))

(defun ek9-port-forward (resource item ports)
  "Start a port-forward to ITEM of type RESOURCE with PORTS.
PORTS has the form \"LOCAL:REMOTE\", for example \"8080:80\".
Return the port-forward."
  (interactive (pcase-let ((`(,resource ,item ,container)
                            (ek9-list-target-for-action 'port-forward)))
                 (list resource item
                       (ek9-portfwd--read-ports
                        (if container
                            (ek9-portfwd--container-only item container)
                          item))))
               ek9-list-mode)
  (let* ((context (or (ek9-context-current)
                      (user-error "ek9: No context is selected")))
         (target (concat (ek9-resource-kind resource) "/"
                         (ek9-resource-item-name item)))
         (namespace (ek9-resource-item-namespace item))
         (address ek9-portfwd-address)
         (old (seq-find (lambda (forward)
                          (and (ek9-portfwd--running-p forward)
                               (equal (ek9-portfwd--forward-address forward)
                                      address)
                               (equal (ek9-portfwd--forward-context forward)
                                      context)
                               (equal (ek9-portfwd--forward-namespace forward)
                                      namespace)
                               (equal (ek9-portfwd--forward-target forward)
                                      target)
                               (equal (ek9-portfwd--forward-ports forward)
                                      ports)))
                        ek9-portfwd--forwards)))
    (if old
        (progn (message "ek9: Port-forward already runs: %s"
                        (ek9-portfwd--label old))
               old)
      (let* ((forward (ek9-portfwd--make-forward
                       :context context :namespace namespace
                       :target target :ports ports :address address))
             (process (ek9-process-stream
                       (ek9-portfwd--args resource item ports)
                       (lambda (_process text) (ek9-portfwd--output forward text))
                       (lambda (process) (ek9-portfwd--exit forward process)))))
        ;; Ask before Emacs exits and stops the port-forward.
        (set-process-query-on-exit-flag process t)
        (setf (ek9-portfwd--forward-process forward) process)
        (push forward ek9-portfwd--forwards)
        (ek9-portfwd--refresh-manager)
        (message "ek9: Port-forward starts: %s" (ek9-portfwd--label forward))
        forward))))

;;;; Manager buffer

(defun ek9-portfwd--entries ()
  "Return the `tabulated-list-entries' of the manager buffer."
  (mapcar (lambda (forward)
            (list forward
                  (vector (ek9-portfwd--forward-context forward)
                          (or (ek9-portfwd--forward-namespace forward) "")
                          (ek9-portfwd--forward-target forward)
                          (ek9-portfwd--forward-address forward)
                          (ek9-portfwd--ports-label forward)
                          (ek9-portfwd--forward-status forward)
                          (or (ek9-portfwd--forward-last-line forward) ""))))
          ek9-portfwd--forwards))

(defun ek9-portfwd--tab-line ()
  "Return the tab line text of the manager buffer."
  (string-replace
   "%" "%%"
   (concat " " (propertize "Port-forwards: " 'face 'bold)
           (format "%d running"
                   (seq-count #'ek9-portfwd--running-p ek9-portfwd--forwards))
           "  " (propertize "Current context: " 'face 'bold)
           (or (ek9-context-current) "<none>"))))

(defun ek9-portfwd--refresh-manager ()
  "Show the current port-forwards in the manager buffer, if it exists."
  (when-let* ((buffer (get-buffer ek9-portfwd--buffer-name)))
    (with-current-buffer buffer
      (setq tabulated-list-entries (ek9-portfwd--entries))
      (tabulated-list-print t)
      (force-mode-line-update))))

(defun ek9-portfwd-stop (forward)
  "Stop the port-forward FORWARD.
If FORWARD is already stopped, remove it from the list."
  (interactive (list (or (tabulated-list-get-id)
                         (user-error "ek9: No port-forward at point")))
               ek9-portfwd-mode)
  (let ((process (ek9-portfwd--forward-process forward)))
    (if (process-live-p process)
        (progn
          (process-put process 'ek9-stopped t)
          (delete-process process)
          (message "ek9: Port-forward stopped: %s" (ek9-portfwd--label forward)))
      (setq ek9-portfwd--forwards (delq forward ek9-portfwd--forwards))))
  (ek9-portfwd--refresh-manager))

(defvar-keymap ek9-portfwd-mode-map
  :doc "Keymap for `ek9-portfwd-mode'."
  :parent tabulated-list-mode-map
  "D" #'ek9-portfwd-stop)

(define-derived-mode ek9-portfwd-mode tabulated-list-mode "ek9-portfwd"
  "Major mode for the ek9 port-forward manager.

\\{ek9-portfwd-mode-map}"
  (setq tabulated-list-format [("CONTEXT" 20 t)
                               ("NAMESPACE" 16 t)
                               ("TARGET" 30 t)
                               ("ADDRESS" 10 t)
                               ("PORTS" 12 t)
                               ("STATUS" 17 t)
                               ("LAST OUTPUT" 0 nil)]
        tabulated-list-padding 1)
  (setq-local tab-line-format '(:eval (ek9-portfwd--tab-line)))
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (ek9-portfwd--entries)))
            nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun ek9-port-forwards ()
  "Show all ek9 port-forwards."
  (interactive)
  (let ((buffer (get-buffer-create ek9-portfwd--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ek9-portfwd-mode)
        (ek9-portfwd-mode)))
    (ek9-portfwd--refresh-manager)
    (pop-to-buffer buffer)
    buffer))

(provide 'ek9-portfwd)

;;; ek9-portfwd.el ends here
