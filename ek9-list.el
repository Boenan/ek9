;;; ek9-list.el --- Resource list buffers for ek9 -*- lexical-binding: t; -*-

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

;; This file defines `ek9-list-mode', the major mode for resource
;; lists.  Each list buffer shows one resource type.
;;
;; A list buffer reads the server table through `ek9-proxy'.  The table
;; has the same columns as "kubectl get", and the object of each row.
;;
;; Live updates: a visible list buffer reads the full list one time.
;; Then a watch sends each change after the resourceVersion of the
;; list.  If the server ends the watch normally, a new watch continues
;; from the last resourceVersion.  If the resourceVersion is too old
;; (HTTP 410), ek9 reads the full list again.  A timer does these steps
;; for the visible list buffers:
;;
;; - Read the full list of a buffer without a watch.
;; - Read the full list after `ek9-watch-reload-interval' seconds, for
;;   the cells with a time that the server calculates.
;; - Calculate the time columns again, for example AGE.
;;
;; A hidden buffer stops its watch.  If the watch fails many times,
;; the buffer reads the full list at each timer tick, as with
;; `ek9-watch' set to nil.
;;
;; The column titles use the header line.  The context, namespace,
;; filter, and errors use the tab line above it.
;;
;; RET goes down a level, for example from a deployment to its pods.
;; The new list buffer has extra kubectl arguments, such as a label
;; selector, and can have its own namespace.  ^ goes back to the list
;; buffer of the level above.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'ek9-process)
(require 'ek9-context)
(require 'ek9-resource)
(require 'ek9-proxy)
(require 'ek9-metrics)

(declare-function ek9-describe "ek9-describe")
(declare-function ek9-yaml "ek9-describe")
(declare-function ek9-logs "ek9-logs")
(declare-function ek9-secret-decode "ek9-describe")
(declare-function ek9-delete "ek9-action")
(declare-function ek9-edit "ek9-edit")
(declare-function ek9-shell "ek9-exec")
(declare-function ek9-port-forward "ek9-portfwd")
(declare-function ek9-port-forwards "ek9-portfwd")
(declare-function ek9-dispatch "ek9-transient")
(declare-function ek9-saved-view-open "ek9-saved-view")
(declare-function ek9-kill-all-buffers "ek9")
(defvar ek9-saved-views)
(declare-function ek9-scale "ek9-action")
(declare-function ek9-restart "ek9-action")
(declare-function ek9-types-current "ek9-types")
(declare-function ek9-types-prefetch "ek9-types")

(defvar ek9-list-resource-history nil
  "Minibuffer history for resource type names.")

(defcustom ek9-refresh-interval 3
  "Number of seconds between two ticks of the list timer.
At each tick, a visible list buffer without a watch reads its full
list, and a list buffer with a watch calculates its time columns
again.  A change has an effect for the next timer that ek9 starts."
  :type 'number
  :group 'ek9)

(defcustom ek9-watch t
  "Non-nil means that list buffers get each change from a watch.
The API server sends a change at once, and ek9 does not read the
full list every `ek9-refresh-interval' seconds.  If nil, a visible
list buffer reads its full list every `ek9-refresh-interval' seconds."
  :type 'boolean
  :group 'ek9)

(defcustom ek9-watch-reload-interval 30
  "Seconds between two full reads of a list buffer with a watch.
Some cells show a time that the server calculates, for example the
time of the last restart in RESTARTS.  A full read updates them.  ek9
calculates the AGE column itself, so it does not need a full read.
If nil, ek9 does not read the full list while the watch runs."
  :type '(choice (const :tag "Never" nil) number)
  :group 'ek9)

(defconst ek9-list--max-watch-failures 3
  "Number of watch failures after which a list buffer stops the watch.
Then the buffer reads its full list at each timer tick.  The command
`ek9-list-refresh' tries the watch again.")

(defconst ek9-list--min-watch-time 1
  "Minimum seconds of a watch that the server ends normally.
A shorter watch counts as a failure, so that a server that ends each
watch at once does not get new watches in a loop.")

(defconst ek9-list--render-delay 0.2
  "Seconds between a watch event and the render of the buffer.
All events in this time give one render.")

(defcustom ek9-status-colors t
  "Non-nil means that list buffers show the state of each row in color.
See the faces `ek9-status-error', `ek9-status-warning',
`ek9-status-done', and `ek9-status-ok'."
  :type 'boolean
  :group 'ek9)

(defvar-local ek9-list--resource nil
  "Resource type that this buffer shows.")

(defvar-local ek9-list--items nil
  "List of resource items, in the order that kubectl gives them.")

(defvar-local ek9-list--index nil
  "Hash table that maps an item ID to its item.")

(defcustom ek9-list-max-column-width 80
  "Maximum width of a list column, in characters.
A column gets wider than its default width when a value is longer,
for example a long pod name.  ek9 cuts values that are longer than
this width.  The last column has no maximum."
  :type 'natnum
  :group 'ek9)

(defcustom ek9-list-wide t
  "Non-nil means that new list buffers show the wide columns.
The wide columns are the extra columns of \"kubectl get -o wide\".
The command `ek9-list-toggle-wide' shows or hides them in one buffer.
A new drill-down buffer starts with the value of the buffer above it.
A saved view can have its own value, see `ek9-saved-views'."
  :type 'boolean
  :group 'ek9)

(defvar-local ek9-list--wide t
  "Non-nil if the buffer shows the wide columns.")

(defvar-local ek9-list--columns nil
  "Columns that the buffer shows at the moment.")

(defvar-local ek9-list--table-definitions nil
  "Column definitions of the last server table, or nil.")

(defvar-local ek9-list--table-columns nil
  "Columns of the last server table, or nil.
A type without :columns uses these columns.")

(defvar-local ek9-list--empty-overlay nil
  "Overlay that shows the message of a list buffer without rows.
An overlay stays when `tabulated-list-print' erases the buffer, for
example after a sort by column.")

(defvar-local ek9-list--last-width nil
  "Width of the last window that showed the buffer, or nil.")

(defvar-local ek9-list--widths nil
  "Column widths that the buffer shows at the moment.")

(defvar-local ek9-list--process nil
  "The `ek9-proxy' request of the refresh that waits or runs, or nil.")

(defvar-local ek9-list--watch nil
  "The `ek9-proxy-watch' request of the buffer, or nil.")

(defvar-local ek9-list--resource-version nil
  "The resourceVersion of the last full list or watch event, or nil.")

(defvar-local ek9-list--watch-failures 0
  "Number of watch failures in sequence.
A change event, or a watch that the server ends normally, sets the
value to 0.")

(defvar-local ek9-list--loaded-time nil
  "The `float-time' of the last full list, or nil.")

(defvar-local ek9-list--metrics nil
  "Hash table that maps an item ID to its metrics object, or nil.
The value is nil until the first metrics arrive.  Then the buffer
shows the metric columns, see `ek9-metrics-columns'.")

(defvar-local ek9-list--metrics-request nil
  "The `ek9-proxy' request that reads the metrics, or nil.")

(defvar-local ek9-list--metrics-time nil
  "The `float-time' of the last read of the metrics, or nil.")

(defvar-local ek9-list--metrics-columns nil
  "The metric columns of the buffer, or nil.
ek9 makes them one time, so that the columns stay `equal' between
renders.")

(defvar-local ek9-list--render-timer nil
  "Timer that shows the changes of watch events, or nil.")

(defvar-local ek9-list--generation 0
  "Counter that increases when the context or the namespace changes.
ek9 ignores the result of a refresh that started with a different
value.")

(defvar-local ek9-list--error nil
  "Error message of the last refresh, or nil.")

(defvar-local ek9-list--filter nil
  "Filter text for the rows, or nil.")

(defvar-local ek9-list--loaded nil
  "Non-nil after the first successful refresh.")

(defvar-local ek9-list--args nil
  "Extra kubectl arguments of a drill-down buffer, or nil.")

(defvar-local ek9-list--label nil
  "Short text that describes `ek9-list--args', or nil.")

(defvar-local ek9-list--namespace-override nil
  "Namespace of a drill-down buffer, or nil.
The value is a string or the symbol `all'.  If nil, the buffer shows
the current namespace.")

(defvar-local ek9-list--owner-uid nil
  "Controller owner UID of the items to show, or nil for all items.")

(defvar-local ek9-list--owner nil
  "Short text that describes `ek9-list--owner-uid', or nil.")

(defvar-local ek9-list--parent nil
  "List buffer of the level above, or nil.")

(defvar-local ek9-list--child nil
  "The last drill-down buffer that ek9 opened from this buffer, or nil.")

(defvar-local ek9-list--context nil
  "Context of a drill-down buffer, or nil for a top-level buffer.
ek9 kills a drill-down buffer when the current context changes.")

(defvar-local ek9-list--view nil
  "Name of the saved view that this buffer shows, or nil.")

(defvar-local ek9-list--view-context nil
  "Context of the saved view of this buffer, or nil.
If the view has a context and it is not the current context, the
buffer shows no rows.")

(defvar-local ek9-list--restore-id nil
  "ID of the row at point before the last reset, or nil.
The next successful refresh moves point back to this row.")

(defvar ek9-list--timer nil
  "Timer that refreshes the visible list buffers.")

;;;; Rendering

(defun ek9-list--namespace ()
  "Return the namespace of this buffer.
The value is a string, the symbol `all', or nil."
  (or ek9-list--namespace-override (ek9-context-namespace)))

(defun ek9-list--metrics-type ()
  "Return the metrics type of the buffer, or nil if it has no metrics."
  (and ek9-metrics (ek9-resource-metrics ek9-list--resource)))

(defun ek9-list--metric-columns ()
  "Return the metric columns of the buffer, or nil.
Return nil until the first metrics arrive."
  (when (and (ek9-list--metrics-type) ek9-list--metrics)
    (or ek9-list--metrics-columns
        (setq ek9-list--metrics-columns
              (ek9-metrics-columns
               (ek9-list--metrics-type)
               (lambda (item)
                 (and ek9-list--metrics
                      (gethash (ek9-resource-item-id item) ek9-list--metrics))))))))

(defun ek9-list--add-metric-columns (columns)
  "Return COLUMNS with the metric columns of the buffer.
Put the metric columns that are not wide before the AGE column, or
at the end.  Put the wide metric columns at the end."
  (if-let* ((metrics (ek9-list--metric-columns)))
      (let* ((wide (seq-filter (lambda (column) (plist-get (nthcdr 3 column) :wide))
                               metrics))
             (normal (seq-difference metrics wide #'eq))
             (age (seq-position columns "AGE"
                                (lambda (column title) (equal (car column) title)))))
        (append (if age
                    (append (seq-take columns age) normal (seq-drop columns age))
                  (append columns normal))
                wide))
    columns))

(defun ek9-list--current-columns ()
  "Return the columns for this buffer.
Remove the wide columns, unless the buffer shows them.  Add a
NAMESPACE column when the buffer shows all namespaces.  Add the
metric columns, see `ek9-list--add-metric-columns'."
  (let* ((all (ek9-list--add-metric-columns
               (or (ek9-resource-columns ek9-list--resource)
                   ek9-list--table-columns)))
         (columns (if ek9-list--wide
                      all
                    (seq-remove (lambda (column)
                                  (plist-get (nthcdr 3 column) :wide))
                                all))))
    (if (and (ek9-resource-namespaced ek9-list--resource)
             (eq (ek9-list--namespace) 'all))
        (cons '("NAMESPACE" 20 ek9-resource-item-namespace) columns)
      columns)))

(defun ek9-list--duration-seconds (text)
  "Return the seconds of the kubectl duration TEXT, or nil.
TEXT has the form of the AGE column, for example \"5d3h\"."
  (when (string-match-p "\\`\\(?:[0-9]+[smhdy]\\)+\\'" text)
    (let ((seconds 0) (start 0))
      (while (string-match "\\([0-9]+\\)\\([smhdy]\\)" text start)
        (cl-incf seconds (* (string-to-number (match-string 1 text))
                            (pcase (match-string 2 text)
                              ("s" 1) ("m" 60) ("h" 3600) ("d" 86400)
                              ("y" (* 365 86400)))))
        (setq start (match-end 0)))
      seconds)))

(defun ek9-list--leading-number (text)
  "Return the number at the start of TEXT, or nil.
For example, \"3 (5m ago)\" gives 3, and \"1/2\" gives 1."
  (when (string-match "\\`-?[0-9]+\\(?:\\.[0-9]+\\)?" text)
    (string-to-number (match-string 0 text))))

(defun ek9-list--sort-class (text)
  "Return (CLASS . NUMBER) for the cell TEXT.
CLASS is 0 for a duration, 1 for a text that starts with a number, and
2 for other texts.  NUMBER is the seconds of the duration, the number,
or nil."
  (if-let* ((seconds (ek9-list--duration-seconds text)))
      (cons 0 seconds)
    (if-let* ((number (ek9-list--leading-number text)))
        (cons 1 number)
      (cons 2 nil))))

(defun ek9-list-natural-less (a b)
  "Return non-nil if the cell text A is before the cell text B in a sort.
Durations come first, then texts that start with a number, then other
texts.  Compare two durations by their time, and two numbers by their
value.  Compare equal values and other texts as strings."
  (pcase-let ((`(,class-a . ,number-a) (ek9-list--sort-class a))
              (`(,class-b . ,number-b) (ek9-list--sort-class b)))
    (cond ((/= class-a class-b) (< class-a class-b))
          ((and number-a (/= number-a number-b)) (< number-a number-b))
          (t (string< a b)))))

(defun ek9-list--sort-predicate (column index)
  "Return the `tabulated-list-format' sort value for COLUMN.
INDEX is the position of COLUMN in the row."
  (if-let* ((key (plist-get (nthcdr 3 column) :sort-key)))
      (lambda (a b)
        (< (or (funcall key (gethash (car a) ek9-list--index)) 0)
           (or (funcall key (gethash (car b) ek9-list--index)) 0)))
    (lambda (a b)
      (ek9-list-natural-less (aref (cadr a) index) (aref (cadr b) index)))))

(defun ek9-list--available-width ()
  "Return the number of characters that a row of this buffer can use.
Use the narrowest window that shows the buffer, so that the rows fit
in each window.  If no window shows it, use the width of the last
window, or the width of the frame."
  (if-let* ((windows (seq-filter #'window-live-p
                                 (get-buffer-window-list nil nil t))))
      (setq ek9-list--last-width
            (apply #'min (mapcar #'window-max-chars-per-line windows)))
    (or ek9-list--last-width (frame-width))))

(defun ek9-list--row-width (widths)
  "Return the width of a row with the column WIDTHS.
The last column counts only up to 20 characters: `tabulated-list'
does not cut it, so a long last value can go past the window edge."
  (+ tabulated-list-padding
     (cl-loop for (width . rest) on widths
              sum (if rest (1+ width) (min width 20)))))

(defun ek9-list--fit (widths minimums available)
  "Return WIDTHS, made narrower so that a row fits in AVAILABLE characters.
Make the widest column narrower first, one character at a time.  No
column gets narrower than its value in MINIMUMS.  The last column
does not change."
  (let ((widths (copy-sequence widths)))
    (while (and (> (ek9-list--row-width widths) available)
                (let ((best nil) (best-room 0))
                  (cl-loop for width in (butlast widths)
                           for minimum in minimums
                           for index from 0
                           when (> (- width minimum) best-room)
                           do (setq best index
                                    best-room (- width minimum)))
                  (when best
                    (cl-decf (nth best widths))
                    t))))
    widths))

(defun ek9-list--grow (widths preferred available)
  "Return WIDTHS, made wider toward PREFERRED while a row fits.
AVAILABLE is the number of characters for a row.  Give one character
at a time to each column that is narrower than its PREFERRED width."
  (let ((widths (copy-sequence widths))
        (grew t))
    (while grew
      (setq grew nil)
      (cl-loop for cell on widths
               for target in preferred
               when (and (< (car cell) target)
                         (< (ek9-list--row-width widths) available))
               do (cl-incf (car cell))
               (setq grew t)))
    widths))

(defun ek9-list--widths (columns entries)
  "Return the column widths for COLUMNS and the rows in ENTRIES.
The widths fit in the window, see `ek9-list--available-width':
1. Each column is as wide as its longest value or its title, but not
   wider than `ek9-list-max-column-width'.
2. If there is space, a column grows toward its default width, so that
   its width does not change on each refresh.
3. If the row is still too wide, the widest columns get narrower, down
   to the width of their titles.  `tabulated-list' cuts long values."
  (let ((natural (mapcar (lambda (column) (string-width (car column))) columns))
        (available (1- (ek9-list--available-width))))
    (dolist (entry entries)
      (setq natural (cl-mapcar (lambda (width value)
                                 (max width (string-width value)))
                               natural (cadr entry))))
    (setq natural (cl-mapcar (lambda (width column)
                               (min width (max (nth 1 column)
                                               ek9-list-max-column-width)))
                             natural columns))
    (let ((preferred (cl-mapcar (lambda (width column) (max width (nth 1 column)))
                                natural columns)))
      (cond ((<= (ek9-list--row-width preferred) available) preferred)
            ((<= (ek9-list--row-width natural) available)
             (ek9-list--grow natural preferred available))
            (t (ek9-list--fit natural
                              (mapcar (lambda (column)
                                        (max 3 (string-width (car column))))
                                      columns)
                              available))))))

(defun ek9-list--window-resized (window)
  "Fit the columns of the list buffer in WINDOW to its new width.
Do nothing if WINDOW is not a live window, or if the width for the
rows did not change, for example when only the height changed."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'ek9-list-mode) ek9-list--resource)
        (let ((old ek9-list--last-width))
          (unless (equal old (ek9-list--available-width))
            (ek9-list--render)))))))

(defun ek9-list--refit ()
  "Fit the columns again, for example after a change of the text scale.
Do nothing while the mode starts: a global mode, for example
`global-display-line-numbers-mode', can run this hook before the
buffer has a resource type."
  (when (and (derived-mode-p 'ek9-list-mode) ek9-list--resource)
    (ek9-list--render)))

(defun ek9-list--format (columns widths)
  "Return the `tabulated-list-format' value for COLUMNS with WIDTHS."
  (vconcat
   (cl-loop for column in columns
            for width in widths
            for index from 0
            collect (append (list (nth 0 column)
                                  width
                                  (ek9-list--sort-predicate column index))
                            (when (plist-get (nthcdr 3 column) :right-align)
                              '(:right-align t))))))

(defun ek9-list--row-face (item)
  "Return the face for the row of ITEM, or nil."
  (when-let* ((ek9-status-colors)
              (function (ek9-resource-row-face ek9-list--resource)))
    (funcall function item)))

(defun ek9-list--entry (columns item)
  "Return the `tabulated-list-entries' entry for ITEM with COLUMNS."
  (let ((face (ek9-list--row-face item)))
    (list (ek9-resource-item-id item)
          (vconcat (mapcar (lambda (column)
                             (let ((value (ek9-resource-column-string
                                           column item)))
                               (if face
                                   (propertize value 'face face)
                                 value)))
                           columns)))))

(defun ek9-list--parse-filter (filter)
  "Return (KIND . TEXT) for the FILTER of a list buffer.
\"-l REGEXP\" gives (labels . REGEXP), and \"-a REGEXP\" gives
\(annotations . REGEXP).  Other text gives (cells . FILTER)."
  (if (string-match "\\`[ \t]*-\\([la]\\)\\(?:[ \t]+\\(.*\\)\\)?\\'" filter)
      (cons (if (equal (match-string 1 filter) "l") 'labels 'annotations)
            (or (match-string 2 filter) ""))
    (cons 'cells filter)))

(defun ek9-list--match-p (entry)
  "Return non-nil if ENTRY matches the filter of the buffer.
The match ignores case.  A text filter must be in one of the
columns.  A label or annotation filter is a regexp that must match
one \"KEY=VALUE\" of the labels or annotations of the item.  A
value with line breaks is one line, with spaces for the line
breaks.  See `ek9-list-filter'."
  (or (null ek9-list--filter)
      (let ((case-fold-search t))
        (pcase (ek9-list--parse-filter ek9-list--filter)
          (`(cells . ,text)
           (let ((regexp (regexp-quote text)))
             (seq-some (lambda (value) (string-match-p regexp value))
                       (cadr entry))))
          (`(,key . ,regexp)
           (let ((item (gethash (car entry) ek9-list--index)))
             (seq-some (lambda (pair)
                         ;; A saved view can have a regexp that is not
                         ;; valid.  Then no row matches.
                         (ignore-error invalid-regexp
                           (string-match-p
                            regexp
                            (replace-regexp-in-string
                             "[\n\r]+" " "
                             (format "%s=%s" (car pair) (cdr pair))))))
                       (ek9-resource-path item (list 'metadata key)))))))))

(defun ek9-list--find-id (id)
  "Return the position of the row with ID, or nil."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (equal (tabulated-list-get-id) id)))
      (forward-line 1))
    (unless (eobp)
      (point))))

(defun ek9-list--render ()
  "Show the current items in the buffer.
Keep the row at point in each window that shows the buffer."
  (let* ((columns (ek9-list--current-columns))
         (entries (mapcar (lambda (item) (ek9-list--entry columns item))
                          ek9-list--items))
         ;; Use all rows, so that the widths do not change with the filter.
         (widths (ek9-list--widths columns entries))
         (windows (mapcar (lambda (window)
                            (cons window
                                  (save-excursion
                                    (goto-char (window-point window))
                                    (tabulated-list-get-id))))
                          (get-buffer-window-list nil nil t))))
    (unless (and (equal columns ek9-list--columns)
                 (equal widths ek9-list--widths))
      ;; `tabulated-list-print' fails if the sort column is gone, for
      ;; example a wide column.  Then use the default sort.  Before
      ;; the first table arrives, the buffer has no columns: keep the
      ;; sort for the table.
      (when-let* ((title (car tabulated-list-sort-key))
                  (columns)
                  ((not (assoc title columns)))
                  ;; The metric columns go away for a short time, for
                  ;; example after a namespace change.  Keep their sort.
                  ((not (and (ek9-list--metrics-type)
                             (member title (ek9-metrics-titles
                                            (ek9-list--metrics-type)))))))
        (setq tabulated-list-sort-key
              (when-let* ((sort (ek9-resource-sort ek9-list--resource))
                          ((assoc (car sort) columns)))
                sort)))
      (setq ek9-list--columns columns
            ek9-list--widths widths
            tabulated-list-format (ek9-list--format columns widths))
      (tabulated-list-init-header))
    (setq tabulated-list-entries (seq-filter #'ek9-list--match-p entries))
    ;; Do not use the UPDATE argument: it keeps a row with the same ID
    ;; even when its text or its face changed.
    (let ((tabulated-list-sort-key (and (assoc (car tabulated-list-sort-key)
                                               columns)
                                        tabulated-list-sort-key)))
      (tabulated-list-print t))
    (unless (overlayp ek9-list--empty-overlay)
      (setq ek9-list--empty-overlay (make-overlay (point-min) (point-min))))
    (overlay-put ek9-list--empty-overlay 'after-string
                 (when (and ek9-list--loaded (null tabulated-list-entries))
                   (propertize (concat " " (ek9-list--empty-message) "\n")
                               'face 'shadow)))
    (pcase-dolist (`(,window . ,id) windows)
      (when-let* ((position (and id (ek9-list--find-id id))))
        (set-window-point window position)))))

(defun ek9-list--paused-p ()
  "Return non-nil if the buffer is a view of a context that is not current."
  (and ek9-list--view-context
       (not (equal ek9-list--view-context (ek9-context-current)))))

(defun ek9-list--empty-message ()
  "Return the text for a list buffer without rows."
  (let ((name (symbol-name (ek9-resource-name ek9-list--resource)))
        (namespace (ek9-list--namespace)))
    (cond
     ((ek9-list--paused-p)
      (format "This view uses context %s.  Push v to open it again."
              ek9-list--view-context))
     ((and ek9-list--filter ek9-list--items)
      (format "No %s match the filter \"%s\"." name ek9-list--filter))
     (t
      (concat "No " name
              (when ek9-list--label
                (concat " with selector " ek9-list--label))
              (when ek9-list--owner
                (concat " owned by " ek9-list--owner))
              (when (ek9-resource-namespaced ek9-list--resource)
                (if (eq namespace 'all)
                    " in all namespaces"
                  (concat " in namespace " (or namespace "<none>"))))
              ".")))))

(defun ek9-list--tab-line ()
  "Return the tab line text of a list buffer."
  (string-replace
   "%" "%%"
   (concat " "
           (ek9-context-header-line ek9-list--namespace-override)
           "  "
           (propertize "View: " 'face 'bold)
           (format "%s [%d]"
                   (ek9-resource-name ek9-list--resource)
                   (length tabulated-list-entries))
           (when ek9-list--view
             (concat "  " (propertize "Saved view: " 'face 'bold) ek9-list--view))
           (when ek9-list--label
             (concat "  " (propertize "Selector: " 'face 'bold) ek9-list--label))
           (when ek9-list--owner
             (concat "  " (propertize "Owner: " 'face 'bold) ek9-list--owner))
           (when ek9-list--filter
             (concat "  " (propertize "Filter: " 'face 'bold) ek9-list--filter))
           (cond (ek9-list--error
                  (concat "  " (propertize (concat "Error: " ek9-list--error)
                                           'face 'error)))
                 ((not ek9-list--loaded) "  Loading...")
                 ((ek9-list--watch-stopped-p)
                  (concat "  " (propertize "Watch failed, g tries again"
                                           'face 'warning)))))))

;;;; Refresh

(defun ek9-list--owned (items)
  "Return the ITEMS that `ek9-list--owner-uid' controls.
If `ek9-list--owner-uid' is nil, return all ITEMS."
  (if ek9-list--owner-uid
      (seq-filter (lambda (item)
                    (equal (ek9-resource-controller-uid item)
                           ek9-list--owner-uid))
                  items)
    items))

(defun ek9-list--table-p ()
  "Return non-nil if the rows of the buffer come from the server table.
A type with an :items function reads the JSON object instead."
  (not (ek9-resource-items ek9-list--resource)))

(defun ek9-list--set-table (table)
  "Use the columns of the server TABLE in this buffer.
Keep the old column objects if the definitions did not change, so
that the header does not change."
  (let ((definitions (alist-get 'columnDefinitions table)))
    (unless (equal definitions ek9-list--table-definitions)
      (setq ek9-list--table-definitions definitions
            ek9-list--table-columns (ek9-proxy-table-columns table)))))

(defun ek9-list--receive (json)
  "Show the reply JSON of a refresh.
JSON is a server table, or the JSON object for a type with :items.
After a server table, start a watch from its resourceVersion."
  (if-let* ((function (ek9-resource-items ek9-list--resource)))
      (ek9-list--update (funcall function json))
    (ek9-list--set-table json)
    (setq ek9-list--resource-version (ek9-list--table-version json)
          ek9-list--loaded-time (float-time))
    (ek9-list--update (ek9-proxy-table-items json))
    (ek9-list--start-watch)
    (ek9-list--refresh-metrics)))

(defun ek9-list--update (items)
  "Replace the items of the buffer with ITEMS."
  (let* ((items (ek9-list--owned items))
         (index (make-hash-table :test #'equal :size (length items))))
    (dolist (item items)
      (puthash (ek9-resource-item-id item) item index))
    (setq ek9-list--items items
          ek9-list--index index
          ek9-list--error nil
          ek9-list--loaded t)
    (ek9-list--render)
    (when-let* ((id (prog1 ek9-list--restore-id
                      (setq ek9-list--restore-id nil)))
                (position (ek9-list--find-id id)))
      (goto-char position)
      (dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window position)))))

(defun ek9-list--set-error (message)
  "Show the error MESSAGE in the tab line.  Keep the old rows."
  (setq ek9-list--error message)
  (force-mode-line-update))

(defun ek9-list--handle (buffer generation function argument)
  "Call FUNCTION with ARGUMENT in BUFFER.
Do nothing if BUFFER is dead or if GENERATION is not the current
value of `ek9-list--generation' in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation ek9-list--generation)
        (funcall function argument)))))

(defun ek9-list--parse-args ()
  "Return (NAME . QUERY) for the extra arguments of this buffer.
\"--selector\" and \"--field-selector\" take a value and go in the
QUERY alist.  Another argument is the NAME of one object."
  (let ((args ek9-list--args)
        name query)
    (while args
      (pcase (pop args)
        ("--selector" (push (cons "labelSelector" (pop args)) query))
        ("--field-selector" (push (cons "fieldSelector" (pop args)) query))
        (arg (setq name arg))))
    (cons name (nreverse query))))

(defun ek9-list--api-namespace ()
  "Return the namespace for the API path of this buffer, or nil.
Return nil for all namespaces, and for a type that is not in a
namespace."
  (when (ek9-resource-namespaced ek9-list--resource)
    (let ((namespace (ek9-list--namespace)))
      (unless (eq namespace 'all)
        (or namespace "default")))))

(defun ek9-list--request-path ()
  "Return the API path that gets the items of this buffer.
The extra arguments of a drill-down buffer become the query, see
`ek9-list--parse-args'."
  (pcase-let ((`(,name . ,query) (ek9-list--parse-args)))
    (ek9-proxy-path (ek9-resource-api ek9-list--resource)
                    (ek9-list--api-namespace)
                    name
                    (if (ek9-list--table-p)
                        (append query '(("includeObject" . "Object")))
                      query))))

;;;; Metrics

(defun ek9-list--metrics-due-p ()
  "Return non-nil if the buffer must read its metrics now."
  (and (ek9-list--metrics-type)
       (not (ek9-list--paused-p))
       (not (ek9-proxy-live-p ek9-list--metrics-request))
       (or (null ek9-list--metrics-time)
           (>= (- (float-time) ek9-list--metrics-time) ek9-metrics-interval))))

(defun ek9-list--receive-metrics (json)
  "Show the metrics JSON in the buffer."
  (setq ek9-list--metrics (ek9-metrics-index json))
  (ek9-list--render))

(defconst ek9-list--metrics-missing-delay 300
  "Seconds to wait for the next read after the metrics API was missing.
The command `ek9-list-refresh' reads the metrics at once.")

(defun ek9-list--metrics-error (code)
  "Continue after a failed read of the metrics, with the HTTP CODE.
Do not show an error: many clusters have no metrics API.  After a
404, remove the metric columns, and wait
`ek9-list--metrics-missing-delay' seconds more for the next read.
After another error, keep the last metrics, for example while
metrics-server starts again."
  (when (eql code 404)
    (setq ek9-list--metrics-time (+ (float-time) ek9-list--metrics-missing-delay))
    (when ek9-list--metrics
      (setq ek9-list--metrics nil)
      (ek9-list--render))))

(defun ek9-list--refresh-metrics ()
  "Read the metrics of the buffer, if `ek9-list--metrics-due-p'."
  (when (ek9-list--metrics-due-p)
    (pcase-let ((buffer (current-buffer))
                (generation ek9-list--generation)
                (`(,_ . ,query) (ek9-list--parse-args)))
      (setq ek9-list--metrics-time (float-time)
            ek9-list--metrics-request
            (ek9-proxy-get (ek9-context-current)
                           (ek9-metrics-path (ek9-list--metrics-type)
                                             (ek9-list--api-namespace)
                                             query)
                           (lambda (json)
                             (ek9-list--handle buffer generation
                                               #'ek9-list--receive-metrics json))
                           (lambda (code _text)
                             (ek9-list--handle buffer generation
                                               #'ek9-list--metrics-error code)))))))

;;;; Watch

(defun ek9-list--table-version (table)
  "Return the resourceVersion in the metadata of TABLE, or nil."
  (let ((version (alist-get 'resourceVersion (alist-get 'metadata table))))
    (and (stringp version) (not (string-empty-p version)) version)))

(defun ek9-list--watch-stopped-p ()
  "Return non-nil if the buffer stopped its watch after many failures."
  (and ek9-watch
       (>= ek9-list--watch-failures ek9-list--max-watch-failures)))

(defun ek9-list--watch-possible-p ()
  "Return non-nil if this buffer can start a watch now."
  (and ek9-watch
       (ek9-list--table-p)
       ek9-list--resource-version
       (not (ek9-list--watch-stopped-p))
       ;; A list of one object, for example the drill-down of a node.
       (not (car (ek9-list--parse-args)))
       (not (ek9-list--paused-p))
       (get-buffer-window (current-buffer) 'visible)))

(defun ek9-list--stop-watch ()
  "Stop the watch of the current buffer."
  (when ek9-list--watch
    (ek9-proxy-cancel ek9-list--watch))
  (setq ek9-list--watch nil))

(defun ek9-list--start-watch ()
  "Start a watch from `ek9-list--resource-version' for this buffer.
Stop the old watch first.  Do nothing if the buffer cannot start a
watch, see `ek9-list--watch-possible-p'."
  (ek9-list--stop-watch)
  (when (ek9-list--watch-possible-p)
    (let ((buffer (current-buffer))
          (generation ek9-list--generation)
          (start (float-time)))
      (condition-case err
          (setq ek9-list--watch
                (ek9-proxy-watch
                 (ek9-context-current)
                 (ek9-list--request-path)
                 ek9-list--resource-version
                 (lambda (type object)
                   (ek9-list--handle buffer generation
                                     #'ek9-list--watch-event (cons type object)))
                 (lambda (_code text)
                   (ek9-list--handle buffer generation
                                     #'ek9-list--watch-end (cons text start)))))
        (error (ek9-list--watch-failed (error-message-string err)))))))

(defun ek9-list--watch-failed (text)
  "Count a failure of the watch, and show the error TEXT.
Stop the watch.  The next timer tick reads the full list, and then
starts a new watch, unless the watch failed too many times."
  (ek9-list--stop-watch)
  (cl-incf ek9-list--watch-failures)
  (ek9-list--set-error text))

(defun ek9-list--watch-end (result)
  "Continue after the end of the watch.
RESULT is (TEXT . START).  TEXT is an error text, or nil if the
server ended the watch normally.  START is the `float-time' of the
start of the watch."
  (pcase-let ((`(,text . ,start) result))
    (setq ek9-list--watch nil)
    (cond (text (ek9-list--watch-failed text))
          ((< (- (float-time) start) ek9-list--min-watch-time)
           (ek9-list--watch-failed "The API server ended the watch at once"))
          (t
           (setq ek9-list--watch-failures 0)
           (ek9-list--start-watch)))))

(defun ek9-list--remove-item (item)
  "Remove ITEM from the items of the buffer."
  (remhash (ek9-resource-item-id item) ek9-list--index)
  (setq ek9-list--items (delq item ek9-list--items)))

(defun ek9-list--change-item (type item)
  "Add, replace, or remove ITEM after a watch event of TYPE."
  (let ((old (gethash (ek9-resource-item-id item) ek9-list--index)))
    (cond
     ;; An item can stop to match the owner of a drill-down buffer.
     ((or (equal type "DELETED") (null (ek9-list--owned (list item))))
      (when old
        (ek9-list--remove-item old)))
     (old
      (setcar (memq old ek9-list--items) item)
      (puthash (ek9-resource-item-id item) item ek9-list--index))
     (t
      (setq ek9-list--items (nconc ek9-list--items (list item)))
      (puthash (ek9-resource-item-id item) item ek9-list--index)))))

(defun ek9-list--schedule-render ()
  "Render the buffer after `ek9-list--render-delay' seconds.
All watch events in this time give one render."
  (unless (timerp ek9-list--render-timer)
    (let ((buffer (current-buffer)))
      (setq ek9-list--render-timer
            (run-with-timer ek9-list--render-delay nil
                            (lambda ()
                              (when (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (setq ek9-list--render-timer nil)
                                  (ek9-list--render)))))))))

(defun ek9-list--watch-event (event)
  "Apply the watch EVENT to the items of the buffer.
EVENT is (TYPE . OBJECT), see `ek9-proxy-watch'."
  (pcase-let ((`(,type . ,object) event))
    (if (equal type "ERROR")
        (if (eql (alist-get 'code object) 410)
            ;; The resourceVersion is too old.  A full list gives a new
            ;; one, and then starts a new watch.  This counts as a
            ;; failure, so that a server that always sends 410 does not
            ;; cause a loop.
            (progn
              (ek9-list--stop-watch)
              (cl-incf ek9-list--watch-failures)
              (ek9-list-refresh))
          (ek9-list--watch-failed
           (format "Watch: %s" (let ((message (alist-get 'message object)))
                                 (if (stringp message) message "unknown error")))))
      (when-let* ((version (ek9-list--table-version object)))
        (setq ek9-list--resource-version version))
      (unless (equal type "BOOKMARK")
        ;; A bookmark does not show that the watch works: a server can
        ;; send one and then end the watch at once.
        (setq ek9-list--watch-failures 0)
        (when (ek9-proxy-table-definitions object)
          (ek9-list--set-table object))
        (dolist (item (ek9-proxy-table-items object ek9-list--table-definitions))
          (ek9-list--change-item type item))
        (ek9-list--schedule-render)))))

(defun ek9-list--time-columns-p ()
  "Return non-nil if a column of the buffer has the property :time."
  (seq-some (lambda (column) (plist-get (nthcdr 3 column) :time))
            ek9-list--columns))

(defun ek9-list-refresh (&optional buffer retry)
  "Get the resources of the list buffer from the cluster again.
If BUFFER is nil, use the current buffer.  If a refresh already waits
or runs, do not start another one.  If RETRY is non-nil, try the
watch again after many failures.  Interactively, RETRY is t."
  (interactive (list nil t) ek9-list-mode)
  (with-current-buffer (or buffer (current-buffer))
    (when retry
      (setq ek9-list--watch-failures 0
            ek9-list--metrics-time nil))
    (if (ek9-list--paused-p)
        ;; Do not show rows of a context that the tab line does not show.
        (unless (and ek9-list--loaded (null ek9-list--items))
          (ek9-list--kill-process)
          ;; Keep the row of point for the time when the view opens again.
          (let ((restore-id ek9-list--restore-id))
            (ek9-list--update nil)
            (setq ek9-list--restore-id restore-id)))
      (unless (ek9-proxy-live-p ek9-list--process)
        (let ((buffer (current-buffer))
              (generation ek9-list--generation))
          (condition-case err
              (setq ek9-list--process
                    (ek9-proxy-get
                     (ek9-context-current)
                     (ek9-list--request-path)
                     (lambda (json)
                       (ek9-list--handle buffer generation
                                         #'ek9-list--receive json))
                     (lambda (_code text)
                       (ek9-list--handle buffer generation
                                         #'ek9-list--set-error text))
                     (if (ek9-list--table-p) ek9-proxy-table-accept)
                     (if (ek9-list--table-p) 'table)))
            (error (ek9-list--set-error (error-message-string err)))))))))

(defun ek9-list--kill-process ()
  "Stop the refresh and the watch of the current buffer."
  (when ek9-list--process
    (ek9-proxy-cancel ek9-list--process))
  (setq ek9-list--process nil)
  (when ek9-list--metrics-request
    (ek9-proxy-cancel ek9-list--metrics-request))
  (setq ek9-list--metrics-request nil)
  (ek9-list--stop-watch))

(defun ek9-list--reset ()
  "Remove all rows and ignore the result of the running refresh.
Remember the row at point for the next refresh."
  (setq ek9-list--restore-id (or (tabulated-list-get-id)
                                 ek9-list--restore-id))
  (cl-incf ek9-list--generation)
  (ek9-list--kill-process)
  (setq ek9-list--items nil
        ek9-list--index (make-hash-table :test #'equal)
        ek9-list--error nil
        ek9-list--loaded nil
        ek9-list--resource-version nil
        ek9-list--loaded-time nil
        ek9-list--watch-failures 0
        ek9-list--metrics nil
        ek9-list--metrics-time nil
        ek9-list--metrics-columns nil)
  (ek9-list--render))

(defun ek9-list--buffers ()
  "Return all live list buffers."
  (seq-filter (lambda (buffer)
                (eq (buffer-local-value 'major-mode buffer) 'ek9-list-mode))
              (buffer-list)))

(defun ek9-list--stale-p (buffer)
  "Return non-nil if BUFFER is a drill-down buffer of an old context."
  (let ((context (buffer-local-value 'ek9-list--context buffer)))
    (and context (not (equal context (ek9-context-current))))))

(defun ek9-list--kill (buffer)
  "Kill the list BUFFER.
Show its parent buffer in the windows that show BUFFER."
  (let ((parent (buffer-local-value 'ek9-list--parent buffer)))
    (when (buffer-live-p parent)
      (dolist (window (get-buffer-window-list buffer nil t))
        (set-window-buffer window parent)))
    (kill-buffer buffer)))

(defun ek9-list--context-changed ()
  "Clear all list buffers and refresh the visible list buffers.
Kill the drill-down buffers of an old context, because their
selectors refer to items of that context."
  (mapc #'ek9-list--kill (seq-filter #'ek9-list--stale-p (ek9-list--buffers)))
  (dolist (buffer (ek9-list--buffers))
    (with-current-buffer buffer
      (ek9-list--reset)
      (when (get-buffer-window buffer 'visible)
        (ek9-list-refresh)))))

(add-hook 'ek9-context-changed-hook #'ek9-list--context-changed)

(defun ek9-list--tick-buffer (buffer)
  "Update the visible list BUFFER at a tick of the timer.
Read the full list if the buffer has no watch, or after
`ek9-watch-reload-interval'.  Else calculate the time columns again.
Read the metrics after `ek9-metrics-interval'."
  (with-current-buffer buffer
    (ek9-list--refresh-metrics)
    (cond ((not (ek9-proxy-live-p ek9-list--watch))
           (ek9-list-refresh))
          ((and ek9-watch-reload-interval
                ek9-list--loaded-time
                (>= (- (float-time) ek9-list--loaded-time)
                    ek9-watch-reload-interval))
           (ek9-list-refresh))
          ((ek9-list--time-columns-p)
           (ek9-list--render)))))

(defun ek9-list--tick ()
  "Update the visible list buffers, see `ek9-list--tick-buffer'.
Stop the watch of each hidden list buffer.  Stop the timer when there
are no list buffers."
  (if-let* ((buffers (ek9-list--buffers)))
      (dolist (buffer buffers)
        (if (get-buffer-window buffer 'visible)
            (ek9-list--tick-buffer buffer)
          (with-current-buffer buffer
            (ek9-list--stop-watch))))
    (cancel-timer ek9-list--timer)
    (setq ek9-list--timer nil)))

(defun ek9-list--start-timer ()
  "Start the refresh timer if it does not run."
  (unless (timerp ek9-list--timer)
    (setq ek9-list--timer (run-with-timer ek9-refresh-interval
                                          ek9-refresh-interval
                                          #'ek9-list--tick))))

;;;; Commands

(defun ek9-list-item-at-point ()
  "Return the resource item at point.
Signal a `user-error' if there is no item at point."
  (or (and ek9-list--index
           (gethash (tabulated-list-get-id) ek9-list--index))
      (user-error "ek9: No resource at point")))

(defun ek9-list-item-args (item)
  "Return the namespace and context arguments for the resource ITEM.
Use the namespace of ITEM, so that the command also works when the
list shows all namespaces."
  (append (when-let* ((namespace (ek9-resource-item-namespace item)))
            (list "--namespace" namespace))
          (ek9-context-args)))

(defun ek9-list-read-container (pod &optional no-init)
  "Return a container name of POD.
Ask the user only if POD has more than one container.  If NO-INIT
is non-nil, do not offer the init containers.  Signal a `user-error'
if POD has no container."
  (let ((names (ek9-resource-pod-containers pod no-init)))
    (cond ((null names)
           (user-error "ek9: %s has no container" (ek9-resource-item-id pod)))
          ((cdr names)
           (let ((default (ek9-resource-pod-default-container pod no-init)))
             (completing-read (format-prompt "Container" default)
                              names nil t nil nil default)))
          (t (car names)))))

(defun ek9-list-item-for-action (action)
  "Return the resource item at point for ACTION.
Signal a `user-error' if the resource type does not have ACTION."
  (unless (memq action (ek9-resource-actions ek9-list--resource))
    (user-error "ek9: Action %s is not available for %s"
                action (ek9-resource-name ek9-list--resource)))
  (ek9-list-item-at-point))

(defun ek9-list-target-for-action (action)
  "Return (RESOURCE OBJECT CONTAINER) for ACTION on the row at point.
RESOURCE is a resource type, and OBJECT is the Kubernetes object for
the action.  CONTAINER is a container name, or nil.  For most types,
OBJECT is the item at point.  A type with a `:target' function, for
example containers, gives another object, for example the pod.
Signal a `user-error' if the resource type does not have ACTION."
  (let ((item (ek9-list-item-for-action action)))
    (if-let* ((target (ek9-resource-target ek9-list--resource)))
        (pcase-let ((`(,name ,object ,container) (funcall target item)))
          (list (ek9-resource-get name) object container))
      (list ek9-list--resource item nil))))

(defun ek9-list-events ()
  "Show the events of the object at point.
For a container, show the events of its pod.  \\[ek9-list-up] goes back."
  (interactive nil ek9-list-mode)
  (pcase-let ((`(,_ ,object ,_) (ek9-list-target-for-action 'events)))
    (ek9-list--open-child (ek9-resource-events-of object) (current-buffer))))

(defun ek9-list-filter (filter)
  "Show only the rows that match FILTER.
FILTER has one of these forms:
  TEXT          A column of the row contains TEXT.
  -l REGEXP     A label \"KEY=VALUE\" of the item matches REGEXP.
  -a REGEXP     An annotation \"KEY=VALUE\" of the item matches REGEXP.
For example, \"-l app=web\" or \"-a ^owner=.*@example\\.com$\".
The match ignores case.  An empty FILTER shows all rows."
  (interactive (list (read-string
                      "Filter (-l labels, -a annotations, empty for all): "
                      ek9-list--filter))
               ek9-list-mode)
  (pcase (ek9-list--parse-filter filter)
    (`(,(or 'labels 'annotations) . ,regexp)
     (condition-case err
         (string-match-p regexp "")
       (invalid-regexp
        (user-error "ek9: Invalid regexp %s: %s" regexp (cadr err))))))
  (setq ek9-list--filter (unless (string-empty-p filter) filter))
  (ek9-list--render))

(defun ek9-list--resource-choices ()
  "Return an alist of (STRING . NAME) for all resource types.
STRING is the name or a short name of the resource type NAME.  The
list has the types of the registry first, then the discovered types
of the current context, see `ek9-types-current'."
  (let (choices)
    (pcase-dolist (`(,name . ,resource) (append ek9-resource--registry
                                                (ek9-types-current)))
      (unless (ek9-resource-hidden resource)
        (push (cons (symbol-name name) name) choices)
        (dolist (alias (ek9-resource-aliases resource))
          (push (cons alias name) choices))))
    (nreverse choices)))

(defconst ek9-list-all-namespaces-word "-A"
  "Word that selects all namespaces in `ek9-list-select-resource'.")

(defun ek9-list--view-names ()
  "Return the names of the saved views."
  (mapcar #'car (bound-and-true-p ek9-saved-views)))

(defun ek9-list--command-annotation (choices)
  "Return the annotation function for the first word of `:'.
CHOICES is the alist of `ek9-list--resource-choices'."
  (lambda (string)
    (if-let* ((name (cdr (assoc string choices))))
        (unless (equal string (symbol-name name))
          (concat "  " (symbol-name name)))
      (when (member string (ek9-list--view-names))
        "  saved view"))))

(defun ek9-list--namespace-candidates (word action)
  "Return the namespace candidates for the typed namespace WORD.
ACTION is the completion action.  For the list of candidates, the
list starts with WORD, so that a completion UI that selects the first
candidate on RET (for example fido or vertico) keeps the typed name.
For TAB (ACTION nil), WORD is not a candidate, so that TAB can extend
it.  Return nil for an empty WORD: then RET does not select a
namespace that the user did not type."
  (unless (string-empty-p word)
    (delete-dups (append (when action (list word))
                         (ek9-context-cached-namespaces)
                         (list ek9-list-all-namespaces-word)))))

(defun ek9-list--command-table (&optional choices)
  "Return a completion table for `ek9-list-select-resource'.
CHOICES is the alist of `ek9-list--resource-choices'.  If CHOICES is
nil, get the choices again on each call, so that discovered types
that load while the prompt is open also show.  The first word
completes resource types and saved views, with annotations.  After a space, the
word completes namespaces, see `ek9-list--namespace-candidates'."
  (lambda (string pred action)
    (let ((choices (or choices (ek9-list--resource-choices))))
      (if-let* ((space (string-match-p " [^ ]*\\'" string)))
          (if (eq action 'metadata)
              '(metadata)
            (let ((word (substring string (1+ space))))
              (completion-table-with-context
               (substring string 0 (1+ space))
               (ek9-list--namespace-candidates word action)
               word pred action)))
        (if (eq action 'metadata)
            `(metadata (annotation-function
                        . ,(ek9-list--command-annotation choices)))
          (complete-with-action action
                                (append (mapcar #'car choices)
                                        (ek9-list--view-names))
                                string pred))))))

(defun ek9-list-parse-command (input)
  "Return what the `ek9-list-select-resource' INPUT asks for.
INPUT has the form \"TYPE\", \"TYPE NAMESPACE\", \"TYPE -A\", or
\"VIEW\".  TYPE is a resource type or a short name.  VIEW is the name
of a saved view.  If a view has the same name as a resource type, the
resource type wins.  Return (resource NAME NAMESPACE) or (view NAME).
NAMESPACE is a string, the symbol `all', or nil.  Signal a
`user-error' if INPUT is not valid."
  (let* ((input (string-trim input))
         (words (split-string input " +" t))
         (resource (cdr (assoc (car words) (ek9-list--resource-choices)))))
    (cond
     ((null words) (user-error "ek9: No resource type given"))
     ((and (not resource) (member input (ek9-list--view-names)))
      (list 'view input))
     ((not resource)
      (user-error "ek9: No resource type or saved view %s" input))
     ((cddr words)
      (user-error "ek9: Use the form TYPE or TYPE NAMESPACE, for example \"pods argocd\""))
     ((null (cdr words)) (list 'resource resource nil))
     ((not (ek9-resource-namespaced (ek9-resource-get resource)))
      (user-error "ek9: %s are not in a namespace" resource))
     (t (list 'resource resource
              (if (equal (cadr words) ek9-list-all-namespaces-word)
                  'all
                (cadr words)))))))

(defun ek9-list--read-command (&optional refresh)
  "Ask for a resource type, a namespace, or a saved view.
Return the value of `ek9-list-parse-command'.  If REFRESH is non-nil,
load the discovered types of the current context again."
  (ek9-context-prefetch-namespaces)
  (ek9-types-prefetch refresh)
  (let* ((input (minibuffer-with-setup-hook
                    (lambda ()
                      ;; SPC separates the namespace.  It must not
                      ;; complete a word.
                      (let ((map (make-sparse-keymap)))
                        (keymap-set map "SPC" #'self-insert-command)
                        (set-keymap-parent map (current-local-map))
                        (use-local-map map)))
                  (completing-read "Resource type [namespace] or saved view: "
                                   (ek9-list--command-table)
                                   nil nil nil 'ek9-list-resource-history))))
    (ek9-list-parse-command input)))

(defun ek9-list-select-resource (command)
  "Show the list or the saved view that COMMAND names.
Interactively, ask for a resource type, for example \"deploy\".  A
namespace after the type, for example \"pods argocd\", also makes it
the current namespace.  \"pods -A\" shows all namespaces.  The name
of a saved view opens the view.  With a prefix argument, load the
types of the current context again first, for example after the
install of an operator.  The new types show in the prompt when they
arrive.  COMMAND is a value of `ek9-list-parse-command', or a
resource type name."
  (interactive (list (ek9-list--read-command current-prefix-arg)))
  (pcase command
    (`(view ,name) (ek9-saved-view-open name))
    (`(resource ,name ,namespace)
     (ek9-process-check-kubectl)
     ;; Load the context first.  Else the load replaces the namespace.
     (ek9-context-ensure
      (lambda ()
        (when (and namespace (not (equal namespace (ek9-context-namespace))))
          (ek9-context-set-namespace namespace))
        (ek9-list-open name))))
    ((pred symbolp) (ek9-list-open command))))

(defun ek9-list-drill-down ()
  "Show the list of the level below the item at point.
For example, show the pods of the deployment at point."
  (interactive nil ek9-list-mode)
  (let* ((function (or (ek9-resource-drill-down ek9-list--resource)
                       (user-error "ek9: %s have no level below"
                                   (ek9-resource-name ek9-list--resource))))
         (spec (funcall function (ek9-list-item-at-point)))
         (parent (current-buffer)))
    (when-let* ((namespace (plist-get spec :switch-namespace)))
      (unless (equal namespace (ek9-context-namespace))
        (ek9-context-set-namespace namespace)))
    (ek9-list--open-child spec parent)))

(defun ek9-list--open-child (spec parent)
  "Show the drill-down SPEC as the child of the list buffer PARENT.
Return the new buffer.  Keep one drill-down buffer for each parent:
kill the old child, unless another parent uses it or a window or a
tab shows it."
  (let ((old (buffer-local-value 'ek9-list--child parent))
        (new (ek9-list-open (plist-get spec :resource) spec parent)))
    (with-current-buffer parent
      (setq ek9-list--child new))
    (when (and (buffer-live-p old)
               (not (eq old new))
               (buffer-local-value 'ek9-list--context old)
               (eq (buffer-local-value 'ek9-list--parent old) parent)
               (not (get-buffer-window old t))
               (not (tab-bar-get-buffer-tab old t)))
      (kill-buffer old))
    new))

(defun ek9-list-up ()
  "Go back to the list buffer of the level above."
  (interactive nil ek9-list-mode)
  (let ((parent ek9-list--parent))
    (unless (buffer-live-p parent)
      (user-error "ek9: There is no level above"))
    (pop-to-buffer-same-window parent)
    (ek9-list-refresh parent)))

(defun ek9-list--open-normal-list ()
  "Return a function that opens the normal list of this buffer's type."
  (let ((name (ek9-resource-name ek9-list--resource)))
    (lambda () (ek9-list-open name))))

(defun ek9-list-toggle-wide ()
  "Show or hide the wide columns in this list buffer.
The wide columns are the extra columns of \"kubectl get -o wide\".
The option `ek9-list-wide' gives the value for new list buffers."
  (interactive nil ek9-list-mode)
  (setq ek9-list--wide (not ek9-list--wide))
  (ek9-list--render)
  (message "ek9: Wide columns %s" (if ek9-list--wide "on" "off")))

(defun ek9-list-switch-namespace (&optional refresh)
  "Select the namespace for ek9 in the current context.
A drill-down buffer always shows its own namespace, so this command
does not work there.  In a saved view, the view keeps its namespace:
change the current namespace and show the normal list of the same
type.  With prefix argument REFRESH, get the namespace list from the
cluster again."
  (interactive "P" ek9-list-mode)
  (when (and ek9-list--namespace-override (not ek9-list--view))
    (user-error "ek9: This view always shows %s.  Press ^ to go back"
                (if (eq ek9-list--namespace-override 'all)
                    "all namespaces"
                  (concat "namespace " ek9-list--namespace-override))))
  (ek9-context-switch-namespace
   refresh (when ek9-list--view (ek9-list--open-normal-list))))

(defun ek9-list-switch-context (&optional refresh)
  "Select the kubectl context for ek9.
In a saved view with a context, the view keeps its context: change
the current context and show the normal list of the same type.  In
other buffers, the buffer shows the new context.  With prefix
argument REFRESH, load the contexts from kubeconfig again."
  (interactive "P" ek9-list-mode)
  (ek9-context-switch
   refresh (when ek9-list--view-context (ek9-list--open-normal-list))))

(defvar-keymap ek9-list-mode-map
  :doc "Keymap for `ek9-list-mode'."
  :parent tabulated-list-mode-map
  "g" #'ek9-list-refresh
  "n" #'ek9-list-switch-namespace
  "c" #'ek9-list-switch-context
  "/" #'ek9-list-filter
  ":" #'ek9-list-select-resource
  "d" #'ek9-describe
  "y" #'ek9-yaml
  "l" #'ek9-logs
  "x" #'ek9-secret-decode
  "e" #'ek9-edit
  "s" #'ek9-shell
  "f" #'ek9-port-forward
  "F" #'ek9-port-forwards
  "v" #'ek9-saved-view-open
  "Q" #'ek9-kill-all-buffers
  "E" #'ek9-list-events
  "D" #'ek9-delete
  "S" #'ek9-scale
  "o" #'tabulated-list-sort
  "w" #'ek9-list-toggle-wide
  "R" #'ek9-restart
  "?" #'ek9-dispatch
  "RET" #'ek9-list-drill-down
  "^" #'ek9-list-up)

(define-derived-mode ek9-list-mode tabulated-list-mode "ek9"
  "Major mode for ek9 resource lists.

\\{ek9-list-mode-map}"
  (setq tabulated-list-padding 1)
  ;; Some fonts have no "…" character.  Then Emacs uses a wider glyph
  ;; from another font, and the columns after a cut value move.
  (setq-local truncate-string-ellipsis "...")
  (setq-local tab-line-format '(:eval (ek9-list--tab-line)))
  (setq-local revert-buffer-function
              (lambda (&rest _) (ek9-list-refresh nil t)))
  (setq ek9-list--index (make-hash-table :test #'equal))
  (add-hook 'window-size-change-functions #'ek9-list--window-resized nil t)
  ;; These modes change the number of characters in a window, but not
  ;; the size of the window.
  (add-hook 'text-scale-mode-hook #'ek9-list--refit nil t)
  (add-hook 'display-line-numbers-mode-hook #'ek9-list--refit nil t)
  (add-hook 'kill-buffer-hook #'ek9-list--kill-process nil t))

(defun ek9-list--buffer-name (name spec)
  "Return the buffer name for the resource type NAME and SPEC.
SPEC is a drill-down plist, a saved-view plist, or nil for the top
level."
  (if-let* ((view (plist-get spec :view)))
      (format "*ek9 view %s*" view)
    (let ((namespace (plist-get spec :namespace)))
      (format "*%s*"
              (string-join (delq nil (list "ek9" (symbol-name name)
                                           (and (stringp namespace) namespace)
                                           (plist-get spec :owner)
                                           (plist-get spec :label)))
                           " ")))))

(defun ek9-list-open (name &optional spec parent)
  "Show the list buffer for the resource type NAME and return it.
SPEC is a drill-down plist with the keys :args, :label, :namespace,
:owner-uid, and :owner.  See `ek9-resource' for their meaning.  For a
saved view, SPEC has the keys :view (the name), :namespace, :filter,
:context, and :wide.  If :wide is missing, use `ek9-list-wide'.  If
SPEC is nil, show the top level.  PARENT is the list buffer of the
level above, or nil."
  (let ((resource (ek9-resource-get name))
        (buffer (get-buffer-create (ek9-list--buffer-name name spec))))
    (with-current-buffer buffer
      (if (not (derived-mode-p 'ek9-list-mode))
          (progn
            (ek9-list-mode)
            (setq ek9-list--resource resource
                  ek9-list--wide (if (buffer-live-p parent)
                                     (buffer-local-value 'ek9-list--wide parent)
                                   ek9-list-wide)
                  tabulated-list-sort-key (ek9-resource-sort resource))
            (ek9-list--render))
        ;; A buffer for the same view or drill-down can exist with an
        ;; old definition, for example after a save of a view with the
        ;; same name.  Then remove its rows, and ignore a refresh that
        ;; runs with the old arguments.
        (unless (and (eq resource ek9-list--resource)
                     (equal (plist-get spec :namespace)
                            ek9-list--namespace-override)
                     (equal (plist-get spec :args) ek9-list--args)
                     (equal (plist-get spec :context) ek9-list--view-context))
          (setq ek9-list--resource resource
                ek9-list--namespace-override (plist-get spec :namespace)
                ek9-list--args (plist-get spec :args)
                tabulated-list-sort-key (ek9-resource-sort resource))
          (ek9-list--reset)))
      (setq ek9-list--args (plist-get spec :args)
            ek9-list--label (plist-get spec :label)
            ek9-list--namespace-override (plist-get spec :namespace)
            ek9-list--owner-uid (plist-get spec :owner-uid)
            ek9-list--owner (plist-get spec :owner)
            ek9-list--view (plist-get spec :view)
            ek9-list--view-context (plist-get spec :context)
            ;; A context switch kills drill-down buffers, not views.
            ek9-list--context (and (not ek9-list--view)
                                   (or ek9-list--args
                                       ek9-list--namespace-override)
                                   (ek9-context-current))
            ek9-list--parent parent)
      (when ek9-list--view
        (setq ek9-list--filter (plist-get spec :filter)
              ek9-list--wide (if (plist-member spec :wide)
                                 (plist-get spec :wide)
                               ek9-list-wide))
        (ek9-list--render)))
    (pop-to-buffer-same-window buffer)
    (ek9-context-ensure (lambda ()
                          (ek9-types-prefetch)
                          (when (buffer-live-p buffer)
                            (ek9-list-refresh buffer))))
    (ek9-list--start-timer)
    buffer))

(provide 'ek9-list)

;;; ek9-list.el ends here
