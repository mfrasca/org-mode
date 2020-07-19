;;; org-plot.el --- Support for Plotting from Org -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.
;;
;; Author: Eric Schulte <schulte dot eric at gmail dot com>
;; Keywords: tables, plotting
;; Homepage: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Borrows ideas and a couple of lines of code from org-exp.el.

;; Thanks to the Org mailing list for testing and implementation and
;; feature suggestions

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-table)

(defmacro let-plist (plist &rest body)
  (declare (indent 1))
  (let ((syms (make-symbol "syms"))
        (vals (make-symbol "vals"))
        (list (make-symbol "list")))
    `(let ((,list ,plist)
           ,syms ,vals)
       (while ,list
         (push (intern (substring (symbol-name (pop ,list)) 1)) ,syms)
         (push (pop ,list) ,vals))
       (cl-progv ,syms ,vals
         ,@body))))

(declare-function gnuplot-delchar-or-maybe-eof "ext:gnuplot" (arg))
(declare-function gnuplot-mode "ext:gnuplot" ())
(declare-function gnuplot-send-buffer-to-gnuplot "ext:gnuplot" ())

(defvar org-plot/gnuplot-default-options
  '((:plot-type . 2d)
    (:with . lines)
    (:ind . 0))
  "Default options to gnuplot used by `org-plot/gnuplot'.")

(defvar org-plot-timestamp-fmt nil)

(defun org-plot/goto-nearest-table ()
  "Move the point to beginning of nearest table.
Go back to beginning of current table, or move forward to next
table, or stay in place.  Return value is the new point."
  (interactive)
  (let ((position (point)))
    (move-beginning-of-line 1)
    (when (looking-at "[[:space:]]*#\\+TBLFM:")
      (forward-line -1))
    (while (not (or (org-at-table-p)
		    (< 0 (forward-line 1)))))
    (goto-char (if (org-at-table-p) (org-table-begin) position))))

(defun org-plot/add-options-to-plist (props options)
  "Parse an OPTIONS line and set values in the PROPS property list.
Return the augmented property list."
  (let ((regexp "\\([[:word:]]+\\):\\([\"][^\"]+[\"]\\|[(][^)]+[)]\\|[^ \t\n\r;,.]*\\)")
	(keys '(:type :use :script :line :set :title :ind :deps
		:with :file :labels :map :xticdep :timeind :timefmt))
	(multiples '(:set :line)) ;; cons values into list
	(start 0))
    (while (string-match regexp options start)
      (let ((key (intern (concat ":" (match-string 1 options))))
	    (value (car (read-from-string (match-string 2 options)))))
	(setq start (match-end 0))
	(when (member key keys)
	  (when (eq key :type)
	    (setq key :plot-type))
	  (when (member key multiples)
	    (setq value (cons value (plist-get props key))))
	  (setq props (plist-put props key value)))))
    props))

(defun org-plot/collect-line-options (line &optional params)
  "Collect org-plot options from LINE.
If LINE matches the org-plot definitions pattern, collect the
options contained.  The options will be added to the optional
PARAMS property list.  Return the augmented property list."
  (or (when (string-match "#\\+PLOT\\(?:\\[\\(.*\\)\\]\\)?: +\\(.*\\)$" line)
	(let ((expect-use (match-string 1 line))
	      (options (match-string 2 line)))
	  (when (or (not expect-use)
		    (eq (string-to-number expect-use)
			(plist-get params :use)))
            (org-plot/add-options-to-plist params options))))
      params))

(defun org-plot/collect-table-options (&optional params)
  "Scans all `#+' lines preceding point, collecting options.
Point is assumed to be at table begin, immediately after last
`#+' line.  Accepts an optional property list PARAMS, to which
the options will be added.  Returns the accumulated property
list."
  (save-excursion
    (while (and (equal 0 (forward-line -1))
		(looking-at "[[:space:]]*#\\+"))
      (setq params (org-plot/collect-line-options
		    (string-trim (thing-at-point 'line))
		    params))))
  params)

(defun org-plot-quote-timestamp-field (s)
  "Convert field S from timestamp to Unix time and export to gnuplot."
  (format-time-string org-plot-timestamp-fmt (org-time-string-to-time s)))

(defun org-plot-quote-tsv-field (s)
  "Quote field S for export to gnuplot."
  (if (string-match org-table-number-regexp s) s
    (if (string-match org-ts-regexp3 s)
	(org-plot-quote-timestamp-field s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\""))))

(defun org-plot/gnuplot-to-data (table data-file params)
  "Export TABLE to DATA-FILE in a format readable by gnuplot.
Pass PARAMS through to `orgtbl-to-generic' when exporting TABLE."
  (with-temp-file
      data-file
    (setq-local org-plot-timestamp-fmt (or
					(plist-get params :timefmt)
					"%Y-%m-%d-%H:%M:%S"))
    (insert (orgtbl-to-generic
	     table
	     (org-combine-plists
	      '(:sep "\t" :fmt org-plot-quote-tsv-field)
	      params))))
  nil)

(defun org-plot/gnuplot-to-grid-data (table data-file params)
  "Export the data in TABLE to DATA-FILE for gnuplot.
This means in a format appropriate for grid plotting by gnuplot.
PARAMS specifies which columns of TABLE should be plotted as
independent and dependent variables.  Return the ind-column, as
list of (cons 0-based-row-num . cell-value)."
  (let* ((last-col (1- (length (car table))))
	 (ind (1- (plist-get params :ind)))
	 (deps (delq ind (if (plist-member params :deps)
			     (mapcar #'1- (plist-get params :deps))
			   (number-sequence 0 last-col))))
	 (skip-deps (set-difference
		     (number-sequence 0 last-col)
		     deps))
	 (row-vals ; the return value - indexed values from the ind column
	  (unless (< ind 0)
	    (cl-loop for i from 0
		     for row in table
		     collect (cons i (nth idx row))))))
    ;; remove non-plotting columns
    (setq table (mapcar (lambda (row) 
			  (org-remove-by-index row skip-deps))
			table))
    ;; write table to gnuplot grid datafile format
    (with-temp-file data-file
      (cl-loop with plot = (lambda (c r v)
			     (format "%f  %f  %s\n%f  %f  %s\n"
				     c (+ r 0.5) v c (+ r 1.5) v))
	       for col from 0 below (length (nth 0 table)) do
	       (cl-loop with front-edge and back-edge
			for row from 0
			for cell in (cl-loop for r in table
					     collect (nth col r))
			concat (funcall plot col row cell)
			into back-edge
			concat (funcall plot (1+ col) row cell)
			into front-edge
			finally (insert (concat back-edge "\n" front-edge "\n")))))
    row-vals))

(defun org-plot/zip-deps-with (num-cols ind deps with)
  "Describe each column to be plotted as (col . with).
Loops over DEPS and WITH in order to cons their elements.
If the DEPS list of columns is not given, use all columns from 1
to NUM-COLS, excluding IND.
If WITH is given as a string, use the given value for all columns.
If WITH is given as a list, and it's shorter than DEPS, expand it
with the global default value."
  (unless deps
    (setq deps (remove ind (number-sequence 1 num-cols))))
  (setq with
	(if (listp with)
	    (append with
		    (make-list (max 0 (- (length deps) (length with)))
			       "lines"))
	  (make-list (length deps) with)))
  (cl-mapcar #'cons deps with))

(defun org-plot/format-plot-str (ind col with col-labels
				     text-ind xticdep)
  (with-output-to-string
    (princ "'$datafile' using ")
    (when (and ind (> ind 0) (not text-ind))
      (princ ind)
      (princ ":"))
    (princ col)
    (when (or xticdep text-ind)
      (princ (format ":xticlabel(%d)"
		     (or xticdep ind))))
    (princ (format " with %s title '%s'"
		   with (or (nth (1- col) col-labels)
			    (format "%d" col))))))

(defun org-plot/gnuplot-script (num-cols params &optional preface)
  "Write a gnuplot script respecting the options set in PARAMS.
NUM-COLS controls the number of columns plotted in a 2-d plot.
Optional argument PREFACE returns only option parameters in a
manner suitable for prepending to a user-specified script."
  (cl-destructuring-bind
      (&key plot-type with set line map title file ind
	    timeind timefmt textind xticdep deps labels
	    xlabels ylabels &allow-other-keys)
      params
    (when (eq plot-type 'grid)
      (setq with 'pm3d))
    (let* ((plot-cmd (pcase plot-type
		       (`2d "plot")
		       (`3d "splot")
		       (`grid "splot")))
	   (script "\3reset")
	   ;; ats = add-to-script
	   (ats (lambda (line) (setq script (concat script "\n" line))))
	   plot-lines)
      (when file
	(funcall ats (format "set term %s" (file-name-extension file)))
	(funcall ats (format "set output '%s'" file)))
      (pcase plot-type
	(`2d ())
	(`3d (when map (funcall ats "set map")))
	(`grid (funcall ats (if map "set pm3d map" "set pm3d"))))
      (when title
	(funcall ats (format "set title '%s'" title)))
      (mapc ats line)
      (dolist (el set)
	(funcall ats (format "set %s" el)))
      ;; Unless specified otherwise, values are TAB separated.
      (unless (string-match-p "^set datafile separator" script)
	(funcall ats "set datafile separator \"\\t\""))
      (when xlabels
	(funcall ats
		 (format "set xtics (%s)"
			 (mapconcat (lambda (pair)
				      (format "\"%s\" %d" (cdr pair) (car pair)))
				    xlabels ", "))))
      (when ylabels
	(funcall ats
		 (format "set ytics (%s)"
			 (mapconcat (lambda (pair)
				      (format "\"%s\" %d" (cdr pair) (car pair)))
				    ylabels ", "))))
      (when timeind
	(funcall ats "set xdata time")
	(funcall ats (concat "set timefmt \""
			     (or timefmt ; timefmt passed to gnuplot
				 "%Y-%m-%d-%H:%M:%S") "\"")))
      (unless preface
	(setq plot-lines
	      (pcase plot-type
		(`2d (cl-loop
		      for (col . with)
		      in (org-plot/zip-deps-with num-cols ind deps with)
		      collect (org-plot/format-plot-str
			       ind col with labels
			       textind xticdep)))
		(`3d (list (format "'$datafile' matrix with %s title ''"
				   with)))
		(`grid (list (format "'$datafile' with %s title ''"
				     with)))))
	(funcall ats
		 (concat plot-cmd " " (mapconcat #'identity
						 plot-lines
						 ",\\\n    "))))
      script)))

;;-----------------------------------------------------------------------------
;; facade functions
;;;###autoload
(defun org-plot/gnuplot (&optional params)
  "Plot table using gnuplot.  Gnuplot options can be specified with PARAMS.
If not given options will be taken from the +PLOT
line directly before or after the table."
  (interactive)
  (require 'gnuplot)
  (save-window-excursion
    (delete-other-windows)
    (when (get-buffer "*gnuplot*") ; reset *gnuplot* if it already running
      (with-current-buffer "*gnuplot*"
	(goto-char (point-max))))
    (org-plot/goto-nearest-table)
    ;; Set default options.
    (dolist (pair org-plot/gnuplot-default-options)
      (unless (plist-member params (car pair))
	(setq params (plist-put params (car pair) (cdr pair)))))
    ;; collect table and table information
    (let ((data-file (make-temp-file "org-plot"))
	  (table (org-table-collapse-header (org-table-to-lisp))))
      (run-with-idle-timer 0.1 nil #'delete-file data-file)
      (when (eq (cadr table) 'hline)
	(setq params
	      (plist-put params :labels (pop table)))) ; headers to labels
      (setq table (delq 'hline (cdr table))) ; clean non-data from table
      ;; Collect options.
      (setq params (org-plot/collect-table-options params))
      ;; Dump table to datafile (very different for grid).
      (pcase (plist-get params :plot-type)
	(`2d   (org-plot/gnuplot-to-data table data-file params))
	(`3d   (org-plot/gnuplot-to-data table data-file params))
	(`grid (let ((y-labels (org-plot/gnuplot-to-grid-data
				table data-file params)))
		 (when y-labels (plist-put params :ylabels y-labels)))))
      ;; Check type of ind column (timestamp? text?)
      (when (eq `2d (plist-get params :plot-type))
	(let* ((ind (1- (plist-get params :ind)))
	       (ind-column (mapcar (lambda (row) (nth ind row)) table)))
	  (cond ((< ind 0) ; ind is implicit
		 nil)
		((cl-every (lambda (el)
			     (string-match org-ts-regexp3 el))
			   ind-column) ; ind holds timestamps
		 (plist-put params :timeind t))
		((or (equal (plist-get params :with) "hist")
		     (cl-notevery (lambda (el)
				    (string-match org-table-number-regexp el))
				  ind-column)) ; ind holds text
		 (plist-put params :textind t)))))
      ;; Write script.
      (with-temp-buffer
	(insert (org-plot/gnuplot-script (length (car table)) params
					 (plist-get params :script)))
	(when (plist-get params :script) ; user script
	  (insert "\n")
	  (insert-file-contents (plist-get params :script)))
	(goto-char (point-min))
	(while (re-search-forward "\\$datafile" nil t)
	  (replace-match data-file nil nil))
	;; Graph table.
	(gnuplot-mode)
	(gnuplot-send-buffer-to-gnuplot))
      ;; Cleanup.
      (bury-buffer (get-buffer "*gnuplot*")))))

(provide 'org-plot)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-plot.el ends here
