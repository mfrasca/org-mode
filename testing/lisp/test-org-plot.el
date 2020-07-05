;;; test-org-plot.el --- Tests for Org Plot library    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Mario Frasca

;; Author: Mario Frasca <mario at anche dot no>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org-test)
(require 'org-plot)


;; General auxiliaries

(ert-deftest test-org-plot/zip-deps-with ()
  "Test `org-plot/zip-deps-with' specifications."
  ;; no deps, no with. defaults to all except ind, and "lines"
  (should
   (equal (org-plot/zip-deps-with 3 1 nil nil)
	  '((2 . "lines") (3 . "lines"))))
  ;; no deps, single with. defaults to all except ind, and repeated with
  (should
   (equal (org-plot/zip-deps-with 3 1 nil "hist")
	  '((2 . "hist") (3 . "hist"))))
  ;; no deps, explicit with
  (should
   (equal (org-plot/zip-deps-with 3 1 nil '("points" "hist"))
	  '((2 . "points") (3 . "hist"))))
  ;; explicit with, same length as deps
  (should
   (equal (org-plot/zip-deps-with 5 1 '(2 4) '("points" "hist"))
	  '((2 . "points") (4 . "hist"))))
  ;; same as above, but different order
  (should
   (equal (org-plot/zip-deps-with 5 1 '(4 2) '("points" "hist"))
	  '((4 . "points") (2 . "hist"))))
  ;; if with exceeds deps, trailing elements are discarded
  (should
   (equal (org-plot/zip-deps-with 5 1 '(4 2) '("points" "hist" "lines"))
	  '((4 . "points") (2 . "hist"))))
  ;; fills in with "lines"
  (should
   (equal (org-plot/zip-deps-with 5 1 '(4 2 3) '("points"))
	  '((4 . "points") (2 . "lines") (3 . "lines")))))

(ert-deftest test-org-plot/collect-line-options ()
  "Test `org-plot/collect-line-options' specifications."
  ;; no options specified, no defaults
  (should
   (equal nil (org-plot/collect-line-options "")))
  ;; no options specified, keeps all defaults
  (let ((params '(:ind 1 :deps 2 3)))
    (should
     (equal params (org-plot/collect-line-options "" params))))
  ;; the independent column
  (should
   (equal '(:ind 1)
            (org-plot/collect-line-options "#+PLOT: ind:1")))
  ;; overruling default
  (should
   (equal '(:ind 2)
            (org-plot/collect-line-options "#+PLOT: ind:2" '(:ind 1))))
  ;; appends to already collected
  (should
   (equal '(:deps (1 3) :ind 2 )
            (org-plot/collect-line-options "#+PLOT: ind:2" '(:deps (1 3)))))
  ;; appends to already collected
  (should
   (equal '(:ind 2 :deps (1 3) )
            (org-plot/collect-line-options "#+PLOT: ind:2" '(:ind 1 :deps (1 3)))))
  ;; multiple options from single line
  (should  ; `op' in add-options-to-plist defines order of options
   (equal '(:plot-type 2d :title "example table" :ind 1 :deps (2 3) :with (lines points))
            (org-plot/collect-line-options "#+PLOT: title:\"example table\" ind:1 deps:(2 3) type:2d with:(lines points)")))
  ;; explicit use given, not required by def-line, def-line is accepted
  (should
   (equal '(:use 2 :with lines)
            (org-plot/collect-line-options "#+PLOT: with:lines" '(:use 2))))
  ;; explicit use given, required by def-line, def-line is accepted
  (should
   (equal '(:use 2 :with lines)
            (org-plot/collect-line-options "#+PLOT[2]: with:lines" '(:use 2))))
  ;; explicit use given, different than required by def-line, def-line is not accepted
  (should
   (equal '(:use 2)
            (org-plot/collect-line-options "#+PLOT[1]: with:lines" '(:use 2))))
  ;; with, as a single string
  (should
   (equal '(:with lines)
            (org-plot/collect-line-options "#+PLOT: with:lines")))
  ;; with, as a list
  (should
   (equal '(:with (lines hist))
            (org-plot/collect-line-options "#+PLOT: with:(lines hist)"))))


(ert-deftest test-org-plot/collect-table-options ()
  "Test `org-plot/collect-table-options' specifications."
  ;; all options on a single lines
  (should  ; `op' in add-options-to-plist defines order of options
   (equal '(:plot-type 2d :title "example table" :ind 1 :deps (2 3) :with (lines points))
          (org-test-with-temp-text
              "#+PLOT: title:\"example table\" ind:1 deps:(2 3) type:2d with:(lines points)\n"
	    (goto-char (point-max))  ; point must be on line after last options line
            (org-plot/collect-table-options))))
  ;; multiple options from multiple lines
  (should  ; `op' in add-options-to-plist defines order of options
   (equal '(:plot-type 2d :with (lines points) :title "example table" :ind 1 :deps (2 3))
          (org-test-with-temp-text
              "#+PLOT: title:\"example table\" ind:1 deps:(2 3)\n#+PLOT: type:2d with:(lines points)\n"
	    (goto-char (point-max))  ; point must be on line after last options line
            (org-plot/collect-table-options))))
  ;; one option per line, several lines (collected going up)
  (should
   (equal '(:with (lines points) :plot-type 2d :deps (2 3) :ind 1 :title "example table")
          (org-test-with-temp-text
              "#+PLOT: title:\"example table\"\n#+PLOT: ind:1\n#+PLOT: deps:(2 3)\n#+PLOT: type:2d\n#+PLOT: with:(lines points)\n"
	    (goto-char (point-max))  ; point must be on line after last options line
            (org-plot/collect-table-options)))))


(ert-deftest test-org-plot/goto-nearest-table ()
  "Test `org-plot/goto-nearest-table' specifications."
  ;; at table beginning stays put
  (should
   (= 1
      (org-test-with-temp-text
       "| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n| 1 | 2 | 3 |"
       (org-plot/goto-nearest-table))))
  ;; anywhere within the table moves back to table beginning
  (should
   (= 1
      (org-test-with-temp-text
       "| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n| 1 | 2 | 3 |"
       (goto-char 4)
       (org-plot/goto-nearest-table))))
  ;; on newline at end of table, goes back to table beginning
  (should
   (= 1
      (org-test-with-temp-text
       "| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n"
       (goto-char (point-max))
       (org-plot/goto-nearest-table))))
  ;; farther away than newline at end of last table, moves to end of buffer
  (should
   (= 44
      (org-test-with-temp-text
       "| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n\n"
       (goto-char (point-max))
       (org-plot/goto-nearest-table))))
  ;; moves to beginning of data, not metadata
  (should
   (= 26
      (org-test-with-temp-text
       "#+PLOT: ind:1 deps:(2 3)\n| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n| 1 | 2 | 3 |"
       (org-plot/goto-nearest-table))))
  ;; in case there's two tables
  (should
   (= 30
      (org-test-with-temp-text
       "| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n\n| 1 | 2 | 3 |"
       (goto-char (point-max))
       (org-plot/goto-nearest-table)))))


(ert-deftest test-org-plot/gnuplot-script ()
  "Regression Test for `org-plot/gnuplot-script'."
  (should
   (equal "reset
set datafile separator \"\\t\"
plot FILENAME using 1:2 with points title '2',\\
    FILENAME using 1:3 with points title '3'"
          (org-test-with-temp-text
           "| 1 | 2 | 3 |\n| 1 | 2 | 3 |\n| 1 | 2 | 3 |"
           (let ((data-file (make-temp-file "org-plot"))
                 (table (org-table-to-lisp))
                 (params '(:ind 1 :deps (2 3) :plot-type 2d :with points)))
             (org-plot/gnuplot-to-data table data-file params)
             (org-plot/gnuplot-script data-file 3 params t)
             (replace-regexp-in-string (concat "'" (regexp-quote data-file) "'")
                                       "FILENAME"
                                       (org-plot/gnuplot-script data-file 3 params)
                                       nil 'LITERAL)))))
  (should
   (equal "reset
set title 'example table'
set datafile separator \"\\t\"
plot FILENAME using 1:2 with lines title 'ylab',\\
    FILENAME using 1:3 with lines title 'zlab'"
          (org-test-with-temp-text
	   "\
#+PLOT: title:\"example table\" ind:1 deps:(2 3) type:2d with:lines
#+PLOT: labels:(\"xlab\" \"ylab\" \"zlab\")
#+TBLNAME:org-plot-example-1
|   x |    y |    z |
|-----+------+------|
| 0.1 | 0.42 | 0.37 |
| 0.2 | 0.31 | 0.33 |
| 0.3 | 0.24 | 0.28 |
| 0.4 | 0.27 | 0.23 |
"
           (org-plot/goto-nearest-table)
           (let ((data-file (make-temp-file "org-plot"))
                 (table (org-table-to-lisp))
                 params)
	     (setq params (org-plot/collect-table-options params))
             (org-plot/gnuplot-to-data table data-file params)
             (org-plot/gnuplot-script data-file 3 params t)
             (replace-regexp-in-string (concat "'" (regexp-quote data-file) "'")
                                       "FILENAME"
                                       (org-plot/gnuplot-script data-file 3 params)
                                       nil 'LITERAL))))))


(provide 'test-org-plot)
;;; test-org-plot.el end here
