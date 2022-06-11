;;; markdown-link.el --- Report on missing, used and undefined links  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Paul Landes

;; Version: 0.1
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: markdown files
;; URL: https://github.com/plandes/markdown-link
;; Package-Requires: ((emacs "26") (dash "2.17.0") (markdown-mode "2.5"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Report on missing on used and undefined links..

;;; Code:

(require 'compile)
(require 'dash)
(require 'markdown-mode)

(defconst markdown-link-regex-link
  "\\[\\([a-zA-Z0-9@#$%^&*()[:space:]-]+?\\)\\]\\(?:(\\([^)]+\\))\\)?")

(defvar markdown-link-last-buffer nil
  "The most recent markdown  buffer to report on links.")

(define-compilation-mode markdown-link-mode "Markdown Link"
  (setq markdown-link-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-disable-input) t))

(defun markdown-link-entries ()
  "Report on missing, used and undefined links."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-link-regex-link nil t)
	(-some->> (-map (lambda (ix)
			  (let ((match (match-string ix)))
			    (when match
			      (substring-no-properties match))))
			'(1 2))
	  (funcall (lambda (matches)
		     (unless (save-excursion
			       (goto-char (1+ (match-end 1)))
			       (looking-at ":"))
		       matches)))
	  (funcall (lambda (matches)
		     (list :ref (replace-regexp-in-string
				 "[ \r\v\t\n]+" " " (nth 0 matches))
			   :link (nth 1 matches)
			   :begin (match-beginning 0)
			   :end (match-end 0))))
	  (list)
	  (append entries)
	  (setq entries))))
    entries))

(defun markdown-link-definitions ()
  "Get all link definitions."
  (let (matches)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-reference-definition nil t)
	(->> (list :ref (substring-no-properties (match-string 2))
		   :definition (substring-no-properties (match-string 5))
		   :begin (match-beginning 0)
		   :end (save-excursion
			  (goto-char (match-beginning 0))
			  (end-of-line)
			  (point)))
	     (list)
	     (append matches)
	     (setq matches))))
    matches))

(defun markdown-link-diffs ()
  "Report on missing, used and undefined links."
  (cl-flet ((cmp-refs
	     (a b)
	     (equal (plist-get a :ref) (plist-get b :ref))))
    (let* ((entries (markdown-link-entries))
	   (no-links (->> entries
			  (-filter (lambda (plist)
				     (null (plist-get plist :link))))))
	   (defs (markdown-link-definitions)))
      (list :entries entries
	    :definitions defs
	    :undefined (cl-set-difference no-links defs :test #'cmp-refs)
	    :unused (cl-set-difference defs entries :test #'cmp-refs)))))

;;;###autoload
(defun markdown-link-report (&optional buffer)
  "Report on missing, used and undefined links in BUFFER."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
	 (file-name (buffer-file-name buffer))
	 (diffs (with-current-buffer buffer
		  (markdown-link-diffs)))
	 (issues (->> '(:undefined :unused)
		      (-map (lambda (key)
			      (-map (lambda (plist)
				      (->> (pcase key
					     (:unused "unused definition")
					     (:undefined "undefined reference")
					     (_ (error "Unknown key: %s" key)))
					   (funcall
					    (lambda (type)
					      (format "%s: '%s'"
						      type
						      (plist-get plist :ref))))
					   (plist-put plist :message)))
				    (plist-get diffs key))))
		      (apply #'append)
		      (funcall (lambda (seq)
				 (cl-sort seq #'<
					  :key (lambda (plist)
						 (plist-get plist :begin)))))))
	 (num-issues (length issues)))
    (save-excursion
      (save-current-buffer
	(set-buffer (set-buffer (get-buffer-create "*Markdown Link Report*")))
	(read-only-mode 0)
	(erase-buffer)
	(insert "Markdown Link Report:\n\n")
	(->> issues
	     (-map (lambda (plist)
		     (with-current-buffer buffer
		       (goto-char (plist-get plist :begin))
		       (let ((pos (point)))
			 (beginning-of-line)
			 (format "%s:%d:%d: %s"
				 file-name
				 (line-number-at-pos pos nil)
				 (1+ (- pos (point)))
				 (plist-get plist :message))))))
	     (funcall (lambda (lines)
			(mapconcat #'identity lines "\n")))
	     (insert))
	(when (> num-issues 0)
	  (newline)
	  (newline))
	(insert (format "Found %d issues\n" num-issues))
	(markdown-link-mode)
	(display-buffer (current-buffer))
	(when (> num-issues 0)
	  (goto-char (point-min))
	  (compilation-next-error 1))
	(set-window-point (get-buffer-window (current-buffer) 'visible)
			  (point))
	num-issues))))

(provide 'markdown-link)

;;; markdown-link.el ends here
