;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing a mule buffer as postscript.  Requires m2ps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-m2ps-buffer (&optional buffer)
  "Print a buffer by passing it through m2ps and lpr."
  (or buffer (setq buffer (current-buffer)))
  (let ((x (save-excursion (set-buffer buffer) tab-width)))
    (save-excursion
      (set-buffer (get-buffer-create " *mule-print*"))
      (erase-buffer)
      (insert-buffer buffer)
      (if (/= x tab-width)
	  (progn
	    (setq tab-width x)
	    (message "Converting tabs")
	    (untabify (point-min) (point-max))))
      (setq file-coding-system *internal*)
      (shell-command-on-region (point-min) (point-max)
			       "m2ps | lpr" t))))
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multi-Lingual Emacs (MULE) Specific Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-create-faces ()
  "Create faces, the no-quite-so-dumb MULE way"
  (setq w3-delimit-links nil))

(defvar attributed-region nil
  "Bogus definition to get rid of compile-time warnings.")

(defvar w3-type-attribute-alist 
  '(("SUBMIT"   . 2)
    ("RESET"    . 2)
    ("PASSWORD" . 3)
    ("OPTION"   . 3)
    (""         . 3))
  "Pairs of anchors' TYPEs and their display ATTRIBUTEs.")

(defun w3-mule-attribute-zones (zones attr)
  (save-excursion
    (let ((c 0) l z type at beg end)
      (while (setq z (nth c zones))
	(setq type (nth 2 (nth 2 z)))
	(if (null type)
	    t
	  (setq at (or (cdr (assoc type w3-type-attribute-alist)) attr))
	  (setq beg (nth 0 z))
	  (setq end (nth 1 z))
	  (setq l (cons (cons beg at) l))
	  (goto-char (marker-position beg))
	  (end-of-line)
	  (while (< (point) (marker-position end))
	    (setq l (cons (cons (copy-marker (point)) 0) l))
	    (forward-line 1)
	    (skip-chars-forward " \t")
	    (if (= (point) (marker-position end)) t
	      (setq l (cons (cons (copy-marker (point)) at) l)))
	    (end-of-line))
	  (setq l (cons (cons end 0) l)))
	(setq c (1+ c)))
      (setq attributed-region (cons '(0 . 0) (reverse l))))))

(defun w3-inhibit-code-conversion (proc buf)
  "Inhibit Mule's subprocess PROC from code converting in BUF."
  (save-excursion
    (set-buffer buf)
    (setq mc-flag nil))
  (set-process-coding-system proc *noconv* *noconv*))

(defconst w3-mime-alist-for-code-conversion
  '(
    ("http"    . "^text")
    ("gopher"  . "^\\(text\\|www\\)")
    ("news"    . ""))
  "Assoc list of protocols and regular expression for mime types.
Mule enocdes data whose mime type matches this REX.")

(defun w3-gently-parse-mime-headers ()
  "Parse mime headers, without modifying buffer."
  (save-excursion
    (let* ((header
	    (downcase
	     (buffer-substring
	      (point-min)
	      (progn 
		(goto-char (point-min))
		(re-search-forward "^\r*$" nil t)
		(point))))))
      (if (and (string-match "^http/" header)
	       (string-match "^mime-version:" header)
	       (string-match "^content-type: *\\([^\r\n]*\\)\r*$" header))
	  (substring header (match-beginning 1) (match-end 1))))))

(defun w3-convert-code-for-mule (type url)
  "Convert current data into the appropriate coding system"
  (let* ((rex (cdr (assoc type w3-mime-alist-for-code-conversion)))
	 (extn (w3-file-extension (w3-basepath url t)))
	 (mime
	  (cond ((equal type "http")
		 (w3-extension-to-mime
		  (if (string-match "^.+[#?]" extn) 
		      (substring extn 0 (1- (match-end 0)))
		    extn)))
		((equal type "gopher")
		 (nth 3 (w3-grok-gopher-href url)))
		((equal type "news") ""))))
    (if (and rex mime (string-match rex mime))
	(progn
	  (setq mc-flag t)
	  (code-convert-region (point-min) (point-max)
			       *autoconv* *internal*)))))

(provide 'w3-mule)
