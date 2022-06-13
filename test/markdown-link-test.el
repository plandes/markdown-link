;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of markdown-link-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'markdown-link)

(ert-deftest test-link-entries ()
  "Test successful evaluation of markdown-link"
  (save-current-buffer
    (set-buffer (find-file-noselect "test/nominative-test.md"))
    (->> '((:ref "full link" :link "#here-partial\nurl"
		 :link-begin 153 :begin 141 :end 171)
	   (:ref "undefined reference" :link nil :link-begin nil
		 :begin 233 :end 254)
	   (:ref "a defined reference" :link nil :link-begin nil
		 :begin 581 :end 602))
	 (equal (markdown-link-entries))
	 (should))))

(ert-deftest test-link-definitions ()
  (save-current-buffer
    (set-buffer (find-file-noselect "test/nominative-test.md"))
    (->> '((:ref "a defined reference"
		 :link "https://example.org/defined-ref"
		 :link-begin 644
		 :begin 621
		 :end 675)
	   (:ref "unused definition"
		 :link "https://example.org/unused-ref"
		 :link-begin 697
		 :begin 676
		 :end 727))
	 (equal (markdown-link-definitions))
	 (should))))

(ert-deftest test-link-unused ()
  (save-current-buffer
    (set-buffer (find-file-noselect "test/nominative-test.md"))
    (should (equal '((:ref "unused definition"
			   :link "https://example.org/unused-ref"
			   :link-begin 697
			   :begin 676 :end 727))
		   (plist-get (markdown-link-diffs) :unused)))))

(ert-deftest test-link-undefined ()
  (save-current-buffer
    (set-buffer (find-file-noselect "test/nominative-test.md"))
    (should (equal '((:ref "undefined reference"
			   :link nil :link-begin nil
			   :begin 233 :end 254))
		   (plist-get (markdown-link-diffs) :undefined)))))

(ert-deftest test-link-undefined-in-code ()
  (save-current-buffer
    (set-buffer (find-file-noselect "test/code-ref-test.md"))
    (markdown-mode)
    (font-lock-ensure)
    (let ((markdown-link-skip-code t))
      (should (equal '((:ref "legit un-defined" :link nil :link-begin nil
			     :begin 74 :end 92))
		     (plist-get (markdown-link-diffs) :undefined))))))

(ert-deftest test-link-undefined-in-code-off ()
  (save-current-buffer
    (set-buffer (find-file-noselect "test/code-ref-test.md"))
    (markdown-mode)
    (font-lock-ensure)
    (let ((markdown-link-skip-code nil))
      (should (equal '((:ref "another un-derfined" :link nil :link-begin nil
			    :begin 27 :end 48)
		       (:ref "legit un-defined" :link nil :link-begin nil
			     :begin 74 :end 92))
		     (plist-get (markdown-link-diffs) :undefined))))))

(provide 'markdown-link-test)

;;; markdown-link-test ends here
