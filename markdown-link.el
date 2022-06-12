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

;; Report on missing, invalid, used and undefined links and references.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)
(require 'compile)
(require 'dash)
(require 'markdown-mode)

(defgroup markdown-link nil
  "Report on missing, invalid, used and undefined links and references."
  :group 'markdown-link
  :prefix "markdown-link-")

(defconst markdown-link-regex-link
  "\\[\\([a-zA-Z0-9@#$%^&*()[:space:]-]+?\\)\\]\\(?:(\\([^)]+\\))\\)?")

(defcustom markdown-link-regex-url "^https?"
  "The regular expression used to identify URLs to check in references."
  :group 'markdown-link
  :type 'regexp)

(defcustom markdown-link-url-check t
  "Whether to check URLs when reporting links."
  :group 'markdown-link
  :type 'boolean)

(defcustom markdown-link-url-timeout-seconds 5
  "The timeout for URL validity checks."
  :group 'markdown-link
  :type 'integer)

;;;###autoload
(define-compilation-mode markdown-link-mode "Markdown Link"
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
			   :link-begin (match-beginning 1)
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
		   :link (substring-no-properties (match-string 5))
		   :link-begin (match-beginning 5)
		   :begin (match-beginning 0)
		   :end (save-excursion
			  (goto-char (match-beginning 0))
			  (end-of-line)
			  (point)))
	     (list)
	     (append matches)
	     (setq matches))))
    matches))

(defun markdown-link-url-status (url)
  "Return non-nil if URL is valid.
It determins if the URL is valid by returning if the status is 200 and status
message is OK."
  (condition-case err
      (let ((resp-buf (url-retrieve-synchronously
		       url nil nil markdown-link-url-timeout-seconds))
	    (status-regex "^HTTP\\/\\([0-9.]+\\)\s+\\([0-9]+\\)\s+\\(.+\\)$"))
	(with-current-buffer resp-buf
	  (unwind-protect
	      (progn
		(goto-char (point-min))
		(if (looking-at status-regex)
		    (list :ver (match-string 1)
			  :code (string-to-number (match-string 2))
			  :text (match-string 3))
		  (->> (thing-at-point 'line t)
		       (string-trim)
		       (format "Bad status line: '%s'")
		       (list :text))))
	    (if (buffer-live-p resp-buf)
		(kill-buffer resp-buf)))))
    (error (->> (error-message-string err)
		(format "URL access: %s")
		(list :text)))))

(defun markdown-link-url-valid (url)
  "Return a whether URL is valid.
It determins if the URL is valid by returning if the status is 200 and status
message is OK.  It returns the form (valid . text-message) where `valid' is
non-nil when the URL is valid and `text-message' the issue or `OK' if valid."
  (let* ((status (markdown-link-url-status url))
	 (text (plist-get status :text)))
    (cons (and (equal (plist-get status :code) 200)
	       (equal text "OK"))
	  text)))

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

(defun markdown-link-validate-urls (diffs)
  "Validate URLs found in DIFFS.

This is done by testing all links the `:definitions' and `:entries' properties
in DIFFS, which is obtained by the `markdown-link-diffs' function."
  (let ((definitions (plist-get diffs :definitions))
	(entries (plist-get diffs :entries)))
    (->> (append entries definitions)
	 (-filter (lambda (entry)
		    (let ((link (plist-get entry :link)))
		      (and link
			   (string-match markdown-link-regex-url link)))))
	 (-map (lambda (entry)
		 (let* ((link (plist-get entry :link))
			(valid (markdown-link-url-valid link)))
		   (unless (car valid)
		     (let ((link-begin (plist-get entry :link-begin))
			   (entry-copy (copy-tree entry)))
		       (message "PL: %S, LB: %S" entry link-begin)
		       (when link-begin
			 (plist-put entry-copy :begin link-begin))
		       (plist-put entry-copy :url-text (cdr valid)))))))
	 (-filter #'identity))))

(defun markdown-link-issues (buffer)
  "Return a list of plists containing any link issues/errors BUFFER."
  (let ((issues (with-current-buffer buffer
		 (markdown-link-diffs))))
    (setq issues (append issues
			 (list :invalid-url
			       (when markdown-link-url-check
				 (markdown-link-validate-urls issues)))))
    (->> '(:undefined :unused :invalid-url)
	 (-map (lambda (key)
		 (-map (lambda (plist)
			 (->> (pcase key
				(:unused "unused definition")
				(:undefined "undefined reference")
				(:invalid-url
				 (->> (plist-get plist :url-text)
				      (format "invalid url ('%s')")))
				(_ (error "Unknown key: %s" key)))
			      (funcall (lambda (type)
					 (format "%s: '%s'"
						 type
						 (plist-get plist :ref))))
			      (plist-put plist :message)))
		       (plist-get issues key))))
	 (apply #'append)
	 (funcall (lambda (seq)
		    (cl-sort seq #'<
			     :key (lambda (plist)
				    (plist-get plist :begin))))))))

;;;###autoload
(defun markdown-link-report (&optional buffer)
  "Report on missing, used and undefined links in BUFFER."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
	 (file-name (buffer-file-name buffer))
	 (issues (markdown-link-issues buffer))
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
