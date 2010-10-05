;;; test-ob-exp.el

;; Copyright (c) 2010 Eric Schulte
;; Authors: Eric Schulte

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Template test file for Org-mode tests


;;; Code:
(require 'org-test)
(require 'org-test-ob-consts)


;;; Tests
(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers ()
  "Testing export without any headlines in the org-mode file."
  (let ((html-file (concat (file-name-sans-extension org-test-no-heading-file)
			   ".html")))
    (when (file-exists-p html-file) (delete-file html-file))
    (org-test-in-example-file org-test-no-heading-file
      ;; export the file to html
      (org-export-as-html nil))
    ;; should create a .html file
    (should (file-exists-p html-file))
    ;; should not create a file with "::" appended to it's name
    (should-not (file-exists-p (concat org-test-no-heading-file "::")))
    (when (file-exists-p html-file) (delete-file html-file))))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-file ()
  "Testing export from buffers which are not visiting any file."
  (when (get-buffer "*Org HTML Export*") (kill-buffer "*Org HTML Export*"))
  (should-not (get-buffer "*Org HTML Export*"))
  ;; export the file to HTML in a temporary buffer
  (org-test-in-example-file nil (org-export-as-html-to-buffer nil))
  ;; should create a .html buffer
  (should (buffer-live-p (get-buffer "*Org HTML Export*")))
  ;; should contain the content of the buffer
  (save-excursion
    (set-buffer (get-buffer "*Org HTML Export*"))
    (should (string-match (regexp-quote org-test-file-ob-anchor)
			  (buffer-string))))
  (when (get-buffer "*Org HTML Export*") (kill-buffer "*Org HTML Export*")))

(ert-deftest test-ob-exp/org-babel-exp-src-blocks/w-no-headers ()
  "Testing export without any headlines in the org-mode file."
  (let ((html-file (concat (file-name-sans-extension
			    org-test-link-in-heading-file)
			   ".html")))
    (when (file-exists-p html-file) (delete-file html-file))
    (org-test-in-example-file org-test-link-in-heading-file
      ;; export the file to html
      (org-export-as-html nil))
    ;; should create a .html file
    (should (file-exists-p html-file))
    ;; should not create a file with "::" appended to it's name
    (should-not (file-exists-p (concat org-test-link-in-heading-file "::")))
    (when (file-exists-p html-file) (delete-file html-file))))

(provide 'test-ob-exp)

;;; test-ob-exp.el ends here
