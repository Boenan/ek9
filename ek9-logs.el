;;; ek9-logs.el --- Pod logs for ek9 -*- lexical-binding: t; -*-

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

;; This file streams the logs of a pod container into a buffer.  The
;; logs of a workload (a deployment, statefulset, daemonset, or job)
;; come from all pods that match the label selector of the workload.
;; The buffer has one stream at a time.  When point is at the end of the
;; buffer, point stays at the end while new lines arrive.  The user can
;; stop the stream and start it again.  The old lines stay in the
;; buffer, and the new stream adds the lines after the last line.
;;
;; ek9 always asks kubectl for timestamps.  The timestamps come from
;; the node, so ek9 does not use the local clock.  A new stream after
;; a stop starts at the timestamp of the last line, and ek9 drops the
;; lines that it showed before.  ek9 removes the timestamps from the
;; text if the user did not turn them on.
;;
;; For a workload, or for all containers of a pod, ek9 asks kubectl to
;; put "[pod/POD/CONTAINER]" before each line.  Each pod and container
;; is a source with its own timestamps and ANSI color state.  ek9
;; removes the prefix and shows the pod name and the container name in
;; color when the user wants them.  Each line keeps its pod name and
;; container name in the text property `ek9-logs-source', and the label
;; has the text property `ek9-logs-label'.  So a change of the labels
;; does not need a new stream.
;;
;; kubectl writes the lines of the pods in the order they arrive, not
;; in time order.  So ek9 keeps the lines of a stream with prefixes for
;; `ek9-logs-sort-delay' seconds, sorts them by timestamp, and merges
;; them into the buffer.  Each line keeps its time key in the text
;; property `ek9-logs-time' of its newline.  A note of ek9 has no time
;; key, and lines do not move before it.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-list)

(defcustom ek9-logs-tail-lines 200
  "Number of old log lines that ek9 shows when the stream starts.
The value -1 shows all old lines."
  :type 'integer
  :group 'ek9)

(defcustom ek9-logs-max-lines 10000
  "Maximum number of lines in a logs buffer.
ek9 removes the oldest lines when there are more lines."
  :type 'integer
  :group 'ek9)

(defcustom ek9-logs-max-requests 50
  "Maximum number of log streams for the logs of a workload.
kubectl opens one stream for each container of each pod.  kubectl
stops with an error if there are more streams."
  :type 'integer
  :group 'ek9)

(defcustom ek9-logs-name-colors nil
  "Colors for the pod names and container names in a logs buffer.
The color of a name comes from a hash of the name, so a name always
gets the same color.  The last color of the list is only for the case
that all other colors are in use.  If the value is nil,
ek9 makes `ek9-logs-name-color-count' colors that fit the background
of the frame, see `ek9-logs--default-colors'."
  :type '(choice (const :tag "Make colors for the background" nil)
                 (repeat color))
  :group 'ek9)

(defcustom ek9-logs-name-color-count 50
  "Number of colors that ek9 makes when `ek9-logs-name-colors' is nil.
The last color is grey.  The other colors have different hues."
  :type 'integer
  :group 'ek9)

(defcustom ek9-logs-sort-delay 0.1
  "Seconds that ek9 keeps new lines of workload logs before it shows them.
ek9 sorts the lines by time in this period.  A longer period gives
fewer lines to merge, but the lines show later."
  :type 'number
  :group 'ek9)

(defconst ek9-logs-all-containers "<all containers>"
  "The container choice for the logs of all containers.")

(defvar-local ek9-logs--object nil
  "The pod or workload item of the logs buffer.")

(defvar-local ek9-logs--container nil
  "The container name of the logs buffer.
The value `ek9-logs-all-containers' gives the logs of all containers.")

(defvar-local ek9-logs--pod-names nil
  "Non-nil to show the pod name on each line of workload logs.")

(defvar-local ek9-logs--context nil
  "The context name that was current when the logs buffer opened.")

(defvar-local ek9-logs--context-args nil
  "The context arguments that were current when the logs buffer opened.")

(defvar-local ek9-logs--timestamps nil
  "Non-nil to show a timestamp on each log line.")

(defvar-local ek9-logs--previous nil
  "Non-nil to show the logs of the previous container instance.")

(defvar-local ek9-logs--process nil
  "The kubectl logs process, or nil.")

(defvar-local ek9-logs--line-count 0
  "Number of lines in the logs buffer.")

(defvar-local ek9-logs--stopped nil
  "Non-nil if the user stopped the log stream.")

(defvar-local ek9-logs--last-times nil
  "Hash table from a source to the time key of its last line.
A source is \"POD/CONTAINER\", or \"\" for a stream without
prefixes.  See `ek9-logs--time-key'.")

(defvar-local ek9-logs--skip-until nil
  "Hash table from a source to a time key, or nil.
A new stream after a stop starts with lines that the buffer has.
ek9 drops the lines of a source up to its time key, until the first
newer line of the source arrives.")

(defvar-local ek9-logs--ansi-contexts nil
  "Hash table from a source to its `ansi-color-context'.
An open color of one pod does not go into the lines of another pod.")

(defvar-local ek9-logs--name-colors nil
  "Hash table from a pod or container name to its color.")

(defvar-local ek9-logs--name-faces nil
  "Hash table from (NAME . BOLD) to the face of a label.
All labels of a name use the same face object.")

(defvar-local ek9-logs--partial ""
  "The start of a log line that did not end yet.")

(defvar-local ek9-logs--pending nil
  "The lines that wait for the sort, newest first.
Each line is a string with the text property `ek9-logs-time'.")

(defvar-local ek9-logs--sort-timer nil
  "The timer that adds `ek9-logs--pending' to the buffer, or nil.")

(defvar-local ek9-logs--newest ""
  "The newest time key of the stream.
A line without a timestamp gets this time key.")

(defconst ek9-logs--timestamp-regexp
  "\\`\\([0-9]+-[0-9]+-[0-9]+T[0-9]+:[0-9]+:[0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?Z "
  "Regexp for the timestamp at the start of a log line.
Group 1 is the time in whole seconds, and group 2 is the fraction.")

(defconst ek9-logs--prefix-regexp
  "\\`\\[pod/\\([^]/]+\\)/\\([^]/]+\\)\\] "
  "Regexp for the prefix that kubectl puts before a log line.
Group 1 is the pod name, and group 2 is the container name.")

(defun ek9-logs--workload-p (object)
  "Return non-nil if OBJECT is a workload and not a pod.
A workload has a pod template."
  (ek9-resource-path object '(spec template)))

(defun ek9-logs--pod-spec-owner (object)
  "Return the object with the pod spec of OBJECT.
For a workload, return its pod template.  For a pod, return OBJECT."
  (or (ek9-logs--workload-p object) object))

(defun ek9-logs--selector (object)
  "Return the label selector of the workload OBJECT.
Signal a `user-error' if it is empty, because an empty selector
selects all pods."
  (or (ek9-resource-label-selector (ek9-resource-path object '(spec selector)))
      (user-error "ek9: %s has no pod selector" (ek9-resource-item-id object))))

(defun ek9-logs--all-p ()
  "Return non-nil for a logs buffer of all containers."
  (equal ek9-logs--container ek9-logs-all-containers))

(defun ek9-logs--prefix-p ()
  "Return non-nil if kubectl puts a prefix before each line."
  (or (ek9-logs--workload-p ek9-logs--object) (ek9-logs--all-p)))

(defun ek9-logs-read-container (object)
  "Return a container name of the pod or workload OBJECT.
Ask the user only if there is more than one container.  The choices
also have `ek9-logs-all-containers'.  Signal a `user-error' if there
is no container."
  (let* ((owner (ek9-logs--pod-spec-owner object))
         (names (ek9-resource-pod-containers owner)))
    (cond ((null names)
           (user-error "ek9: %s has no container" (ek9-resource-item-id object)))
          ((cdr names)
           (let ((default (ek9-resource-pod-default-container owner)))
             (completing-read (format-prompt "Container" default)
                              (append names (list ek9-logs-all-containers))
                              nil t nil nil default)))
          (t (car names)))))

;;;; Stream

(defun ek9-logs--args (&optional since)
  "Return the kubectl arguments for the logs buffer.
If SINCE is a time key, get only the lines from the second of this
time, and not the last `ek9-logs-tail-lines' lines."
  (append (list "logs")
          (if (ek9-logs--workload-p ek9-logs--object)
              (list "--selector" (ek9-logs--selector ek9-logs--object)
                    (format "--max-log-requests=%d" ek9-logs-max-requests))
            (list (ek9-resource-item-name ek9-logs--object)))
          (if (ek9-logs--all-p)
              (list "--all-containers")
            (list "--container" ek9-logs--container))
          (when (ek9-logs--prefix-p)
            (list "--prefix"))
          (when-let* ((namespace (ek9-resource-item-namespace ek9-logs--object)))
            (list "--namespace" namespace))
          (if ek9-logs--previous
              (list "--previous")
            (list "--follow"))
          ;; The API accepts only whole seconds.  The stream gives
          ;; the lines of the full second again, and
          ;; `ek9-logs--skip-until' drops them.
          (if since
              (list (format "--since-time=%sZ"
                            (substring since 0 (string-search "." since))))
            (list (format "--tail=%d" ek9-logs-tail-lines)))
          (list "--timestamps")
          ek9-logs--context-args))

(defun ek9-logs--time-key (seconds fraction)
  "Return a time key for SECONDS and the FRACTION of a timestamp.
SECONDS is the text up to the whole seconds, and FRACTION the digits
after the point, or nil.  Keys have a fixed width, so `string<'
compares them as times."
  (concat seconds "." (substring (concat fraction "000000000") 0 9)))

(defun ek9-logs--default-colors (count dark)
  "Return COUNT colors for names, the last color grey.
If DARK is non-nil, the colors are light, for a dark background.
Each hue is the golden angle after the hue before it, so that the
colors that come one after the other are very different.  The
lightness changes in three steps, to make more colors different."
  (append
   (mapcar (lambda (i)
             (let ((hue (mod (* i 0.618033988749895) 1.0))
                   (lightness (+ (if dark 0.68 0.38)
                                 (nth (mod i 3) '(0.0 -0.08 0.08)))))
               (apply #'color-rgb-to-hex
                      (append (color-hsl-to-rgb hue 0.75 lightness) '(2)))))
           (number-sequence 0 (- count 2)))
   (list (if dark "#999999" "#666666"))))

(defun ek9-logs--name-color-list ()
  "Return the list of colors for the names."
  (or ek9-logs-name-colors
      (ek9-logs--default-colors
       (max 1 ek9-logs-name-color-count)
       (eq (frame-parameter nil 'background-mode) 'dark))))

(defun ek9-logs--name-color (name)
  "Return the color of the pod or container NAME.
The MD5 hash of NAME selects a color of `ek9-logs--name-color-list',
so the color does not depend on the order of the lines.  If another
name of the buffer has that color, use the next free color.  Use the
last color only if all other colors are in use."
  (or (gethash name ek9-logs--name-colors)
      (let* ((colors (ek9-logs--name-color-list))
             (main (or (butlast colors) colors))
             (start (mod (string-to-number (substring (md5 name) 0 7) 16)
                         (length main)))
             (used (hash-table-values ek9-logs--name-colors)))
        (puthash name
                 (or (seq-find (lambda (color) (not (member color used)))
                               (append (nthcdr start main)
                                       (take start main)
                                       (last colors)))
                     (nth start main))
                 ek9-logs--name-colors))))

(defun ek9-logs--name-face (name bold)
  "Return the face for NAME in a label.  If BOLD is non-nil, make it bold."
  (let ((key (cons name bold)))
    (or (gethash key ek9-logs--name-faces)
        (puthash key
                 (append (list :foreground (ek9-logs--name-color name))
                         (when bold (list :weight 'bold)))
                 ek9-logs--name-faces))))

(defun ek9-logs--label (source)
  "Return the colored label of a line from SOURCE, or nil.
SOURCE is (POD . CONTAINER).  Show the pod name if the user turned pod
names on.  Show the container name if the buffer shows all
containers."
  (let ((parts (delq nil
                     (list (when (and ek9-logs--pod-names
                                      (ek9-logs--workload-p ek9-logs--object))
                             (propertize (car source) 'font-lock-face
                                         (ek9-logs--name-face (car source) nil)))
                           (when (ek9-logs--all-p)
                             (propertize (cdr source) 'font-lock-face
                                         (ek9-logs--name-face (cdr source) t)))))))
    (when parts
      (let ((label (concat (string-join parts "/") " ")))
        (add-text-properties 0 (length label)
                             (list 'ek9-logs-label t 'ek9-logs-source source)
                             label)
        label))))

(defun ek9-logs--relabel ()
  "Change the labels of all lines in the buffer to the current settings.
Use the text property `ek9-logs-source' of each line.  Point and the
window points stay on the same text."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((source (get-text-property (point) 'ek9-logs-source)))
          (when (get-text-property (point) 'ek9-logs-label)
            (delete-region (point) (next-single-property-change
                                    (point) 'ek9-logs-label nil
                                    (line-end-position))))
          (when-let* ((label (ek9-logs--label source)))
            (insert label)))
        (forward-line 1)))))

(defun ek9-logs--colors (source text)
  "Change the ANSI color codes in TEXT of SOURCE to faces.
Each source keeps its own color state."
  (let ((ansi-color-context (gethash source ek9-logs--ansi-contexts)))
    (prog1 (ansi-color-apply text)
      (puthash source ansi-color-context ek9-logs--ansi-contexts))))

(defun ek9-logs--line (line)
  "Return the buffer text for the log LINE, or nil.
LINE has no newline, and the text has one.  The newline has the time
key of the line in the text property `ek9-logs-time'.  Remove the
prefix of kubectl, and add the label of `ek9-logs--label'.  Remove
the timestamp of LINE, unless the user turned timestamps on.  Return
nil for a line to drop, see `ek9-logs--skip-until'.  A line without a
timestamp, for example an error of kubectl, stays as it is, and gets
the time key `ek9-logs--newest'."
  (setq line (string-remove-suffix "\r" line))
  (let ((key "") source shown time)
    (when (string-match ek9-logs--prefix-regexp line)
      (setq source (cons (match-string 1 line) (match-string 2 line))
            key (concat (car source) "/" (cdr source))
            line (substring line (match-end 0))))
    (if (not (string-match ek9-logs--timestamp-regexp line))
        (setq shown line
              time ek9-logs--newest)
      (setq time (ek9-logs--time-key (match-string 1 line) (match-string 2 line)))
      (let ((text (if ek9-logs--timestamps line (substring line (match-end 0))))
            (skip (and ek9-logs--skip-until
                       (gethash key ek9-logs--skip-until))))
        (unless (and skip (not (string< skip time)))
          (when skip
            (remhash key ek9-logs--skip-until))
          (puthash key time ek9-logs--last-times)
          (when (string< ek9-logs--newest time)
            (setq ek9-logs--newest time))
          (setq shown text))))
    (when shown
      (let ((text (concat (ek9-logs--colors key shown)
                          (propertize "\n" 'ek9-logs-time time))))
        (when source
          (put-text-property 0 (length text) 'ek9-logs-source source text))
        (concat (when source (ek9-logs--label source)) text)))))

(defun ek9-logs--since ()
  "Return the time key for a new stream after a stop, or nil.
Use the oldest last line of all sources, so that no source misses a
line."
  (let (since)
    (maphash (lambda (_source key)
               (when (or (null since) (string< key since))
                 (setq since key)))
             ek9-logs--last-times)
    since))

(defun ek9-logs--output (text)
  "Add the complete log lines of the stream TEXT to the buffer.
Keep the start of a line that did not end in `ek9-logs--partial'."
  (let* ((text (concat ek9-logs--partial text))
         (end (cl-position ?\n text :from-end t)))
    (setq ek9-logs--partial (substring text (if end (1+ end) 0)))
    (when end
      (let ((lines (delq nil (mapcar #'ek9-logs--line
                                     (split-string (substring text 0 end) "\n")))))
        (if (not (ek9-logs--prefix-p))
            ;; One source: kubectl gives the lines in time order.
            (ek9-logs--insert (apply #'concat lines))
          (setq ek9-logs--pending (nconc (nreverse lines) ek9-logs--pending))
          (unless ek9-logs--sort-timer
            (setq ek9-logs--sort-timer
                  (run-with-timer ek9-logs-sort-delay nil
                                  #'ek9-logs--flush-buffer
                                  (current-buffer)))))))))

(defun ek9-logs--text-time (text)
  "Return the time key of the line TEXT.  TEXT ends with a newline."
  (get-text-property (1- (length text)) 'ek9-logs-time text))

(defun ek9-logs--line-time ()
  "Return the time key of the line at point, or nil.
A note of ek9 has no time key."
  (get-text-property (line-end-position) 'ek9-logs-time))

(defun ek9-logs--merge (lines)
  "Insert LINES at their time positions in the buffer.
LINES is a list of line texts in time order, see `ek9-logs--line'.
Do not move a line before a line without a time key."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; Go back to the first line that is newer than the oldest line of
      ;; LINES.  Then go forward, and insert each line before the first
      ;; newer line.  LINES is in time order, so point only goes forward.
      (goto-char (point-max))
      (let ((oldest (ek9-logs--text-time (car lines))))
        (while (and (not (bobp))
                    (save-excursion
                      (forward-line -1)
                      (let ((time (ek9-logs--line-time)))
                        (and time (string< oldest time)))))
          (forward-line -1)))
      (dolist (line lines)
        (let ((new (ek9-logs--text-time line)))
          (while (and (not (eobp))
                      (let ((time (ek9-logs--line-time)))
                        (and time (not (string< new time)))))
            (forward-line 1))
          (insert line))))))

(defun ek9-logs--flush ()
  "Sort the lines of `ek9-logs--pending' and add them to the buffer.
Keep point at the end in the windows where point is at the end."
  (when (timerp ek9-logs--sort-timer)
    (cancel-timer ek9-logs--sort-timer))
  (setq ek9-logs--sort-timer nil)
  (when ek9-logs--pending
    ;; `sort' is stable: lines with the same time keep their order.
    (let ((lines (sort (nreverse ek9-logs--pending)
                       :key #'ek9-logs--text-time :lessp #'string<))
          (at-end (= (point) (point-max)))
          (windows (seq-filter (lambda (window)
                                 (= (window-point window) (point-max)))
                               (get-buffer-window-list nil nil t))))
      (setq ek9-logs--pending nil)
      (ek9-logs--merge lines)
      (when at-end
        (goto-char (point-max)))
      (dolist (window windows)
        (set-window-point window (point-max)))
      (cl-incf ek9-logs--line-count (length lines))
      (ek9-logs--trim))))

(defun ek9-logs--flush-buffer (buffer)
  "Run `ek9-logs--flush' in BUFFER if it is live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ek9-logs--flush))))

(defun ek9-logs--trim ()
  "Remove the oldest lines if there are more than `ek9-logs-max-lines'."
  (when (> ek9-logs--line-count ek9-logs-max-lines)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (forward-line (- ek9-logs--line-count ek9-logs-max-lines))
        (delete-region (point-min) (point))))
    (setq ek9-logs--line-count ek9-logs-max-lines)))

(defun ek9-logs--insert (text)
  "Add TEXT at the end of the logs buffer.
Keep point at the end in the windows where point is at the end."
  (let* ((at-end (= (point) (point-max)))
         (windows (seq-filter (lambda (window)
                                (= (window-point window) (point-max)))
                              (get-buffer-window-list nil nil t))))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))
    (when at-end
      (goto-char (point-max)))
    (dolist (window windows)
      (set-window-point window (point-max)))
    (cl-incf ek9-logs--line-count (cl-count ?\n text))
    (ek9-logs--trim)))

(defun ek9-logs--stop ()
  "Stop the log stream of the current buffer, if there is one.
Add the lines that wait for the sort."
  (when (process-live-p ek9-logs--process)
    (delete-process ek9-logs--process))
  (setq ek9-logs--process nil)
  (ek9-logs--flush))

(defun ek9-logs--insert-note (text)
  "Add the note TEXT on a new line at the end of the logs buffer."
  (ek9-logs--insert
   (concat (unless (memq (char-before (point-max)) '(nil ?\n)) "\n")
           (propertize (format "[ek9: %s]\n" text) 'face 'shadow))))

(defun ek9-logs--start ()
  "Remove the old content and start a new log stream."
  (ek9-logs--stop)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq ek9-logs--line-count 0
        ek9-logs--newest "")
  (clrhash ek9-logs--last-times)
  (ek9-logs--stream))

(defun ek9-logs--stream (&optional since)
  "Start a log stream into the logs buffer.
If SINCE is non-nil, start at `ek9-logs--since', and drop the lines
that the buffer has."
  ;; The new stream gives the unfinished line of the old stream again,
  ;; so drop it.  The colors of the dropped lines are unknown.
  (setq ek9-logs--stopped nil
        ek9-logs--skip-until (when since (copy-hash-table ek9-logs--last-times))
        ek9-logs--partial "")
  (clrhash ek9-logs--ansi-contexts)
  (let ((buffer (current-buffer)))
    (setq ek9-logs--process
          (ek9-process-stream
           (ek9-logs--args (when since (ek9-logs--since)))
           (lambda (proc text)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq proc ek9-logs--process)
                   (ek9-logs--output text)))))
           (lambda (proc)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq proc ek9-logs--process)
                   (unless (string-empty-p ek9-logs--partial)
                     (ek9-logs--output "\n"))
                   (ek9-logs--flush)
                   (ek9-logs--insert-note "Log stream ended")
                   (force-mode-line-update)))))))
    (force-mode-line-update)))

(defun ek9-logs--object-label (object)
  "Return the label of OBJECT for the tab line and the buffer name.
A pod has the label \"NAMESPACE/NAME\".  A workload has the label
\"KIND NAMESPACE/NAME\", for example \"deployment default/web\"."
  (if (ek9-logs--workload-p object)
      (concat (downcase (or (alist-get 'kind object) "workload"))
              " " (ek9-resource-item-id object))
    (ek9-resource-item-id object)))

(defun ek9-logs--tab-line ()
  "Return the tab line text of a logs buffer."
  (string-replace
   "%" "%%"
   (concat " "
           (propertize "Context: " 'face 'bold) (or ek9-logs--context "<none>")
           "  " (propertize (if (ek9-logs--workload-p ek9-logs--object)
                                "Workload: "
                              "Pod: ")
                            'face 'bold)
           (ek9-logs--object-label ek9-logs--object)
           "  " (propertize "Container: " 'face 'bold)
           (if (ek9-logs--all-p) "all" ek9-logs--container)
           (when ek9-logs--timestamps "  [timestamps]")
           (when (and ek9-logs--pod-names
                      (ek9-logs--workload-p ek9-logs--object))
             "  [pod names]")
           (when ek9-logs--previous "  [previous]")
           (if (process-live-p ek9-logs--process)
               "  Streaming..."
             (propertize "  Stopped" 'face 'shadow)))))

;;;; Commands

(defun ek9-logs-restart ()
  "Remove the content of the logs buffer and start the stream again."
  (interactive nil ek9-logs-mode)
  (ek9-logs--start))

(defun ek9-logs-toggle-stream ()
  "Stop the log stream, or start it again.
A stop keeps the lines in the buffer.  A start after a stop adds the
lines after the last line in the buffer.  For previous logs, or after
the stream ended, a start removes the content and starts a new
stream."
  (interactive nil ek9-logs-mode)
  (cond
   ((process-live-p ek9-logs--process)
    (ek9-logs--stop)
    (setq ek9-logs--stopped t)
    (ek9-logs--insert-note "Log stream stopped")
    (force-mode-line-update))
   ((and ek9-logs--stopped (not ek9-logs--previous))
    (ek9-logs--insert-note "Log stream started again")
    (ek9-logs--stream t))
   (t
    (ek9-logs--start))))

(defun ek9-logs-toggle-timestamps ()
  "Turn timestamps on or off, then start the stream again."
  (interactive nil ek9-logs-mode)
  (setq ek9-logs--timestamps (not ek9-logs--timestamps))
  (ek9-logs--start))

(defun ek9-logs-toggle-previous ()
  "Switch between current and previous container logs.
The previous logs come from the container instance before the last
restart.  They do not stream."
  (interactive nil ek9-logs-mode)
  (setq ek9-logs--previous (not ek9-logs--previous))
  (ek9-logs--start))

(defun ek9-logs-toggle-pod-names ()
  "Show or hide the pod name on each line.
Each pod name has its own color.  Only workload logs have pod names.
The change applies to all lines, and the stream does not start
again."
  (interactive nil ek9-logs-mode)
  (unless (ek9-logs--workload-p ek9-logs--object)
    (user-error "ek9: Pod names are only for the logs of a workload"))
  (setq ek9-logs--pod-names (not ek9-logs--pod-names))
  (ek9-logs--relabel)
  (force-mode-line-update))

(defun ek9-logs-select-container ()
  "Select another container, then start the stream again."
  (interactive nil ek9-logs-mode)
  (setq ek9-logs--container (ek9-logs-read-container ek9-logs--object))
  (ek9-logs--start))

(defun ek9-logs-quit ()
  "Stop the log stream and kill the logs buffer."
  (interactive nil ek9-logs-mode)
  (quit-window t))

(defvar-keymap ek9-logs-mode-map
  :doc "Keymap for `ek9-logs-mode'."
  :parent special-mode-map
  "g" #'ek9-logs-restart
  "s" #'ek9-logs-toggle-stream
  "t" #'ek9-logs-toggle-timestamps
  "P" #'ek9-logs-toggle-previous
  "p" #'ek9-logs-toggle-pod-names
  "c" #'ek9-logs-select-container
  "q" #'ek9-logs-quit)

(define-derived-mode ek9-logs-mode special-mode "ek9-logs"
  "Major mode for pod logs.

\\{ek9-logs-mode-map}"
  (setq-local ek9-logs--last-times (make-hash-table :test #'equal))
  (setq-local ek9-logs--ansi-contexts (make-hash-table :test #'equal))
  (setq-local ek9-logs--name-colors (make-hash-table :test #'equal))
  (setq-local ek9-logs--name-faces (make-hash-table :test #'equal))
  (setq-local tab-line-format '(:eval (ek9-logs--tab-line)))
  (add-hook 'kill-buffer-hook #'ek9-logs--stop nil t))

(defun ek9-logs (object container)
  "Show the logs of CONTAINER in OBJECT.
OBJECT is a pod or a workload, for example a deployment.  For a
workload, show the logs of all pods that match its label selector.
CONTAINER is a container name, or `ek9-logs-all-containers'.
There is one logs buffer for each object.  If the buffer exists and
streams the same container, show it without a restart."
  (interactive (pcase-let ((`(,_ ,object ,container)
                            (ek9-list-target-for-action 'logs)))
                 (list object (or container (ek9-logs-read-container object))))
               ek9-list-mode)
  (when (ek9-logs--workload-p object)
    (ek9-logs--selector object))
  (let ((buffer (get-buffer-create
                 (format "*ek9 logs %s*" (ek9-logs--object-label object)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ek9-logs-mode)
        (ek9-logs-mode))
      (unless (and (process-live-p ek9-logs--process)
                   (equal container ek9-logs--container))
        (setq ek9-logs--object object
              ek9-logs--container container
              ek9-logs--context (ek9-context-current)
              ek9-logs--context-args (ek9-context-args))
        (ek9-logs--start)))
    (pop-to-buffer buffer)
    buffer))

(provide 'ek9-logs)

;;; ek9-logs.el ends here
