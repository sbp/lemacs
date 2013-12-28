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

(require 'w3-vars)
(require 'w3-misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for printing out roman numerals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-decimal-to-roman (n)
  "Convert from decimal to roman numerals"
  (let ((curmod 1000)
	(str "")
	(j 7)
	i2 k curcnt)
    (while (>= curmod 1)
      (if (>= n curmod)
	  (progn
	    (setq curcnt (/ n curmod)
		  n (- n (* curcnt curmod)))
	    (if (= 4 (% curcnt 5))
		(setq i2 (+ j (if (> curcnt 5) 1 0))
		      str (format "%s%c%c" str
				  (aref w3-roman-characters (1- j))
				  (aref w3-roman-characters i2)))
	      (progn
		(if (>= curcnt 5)
		    (setq str (format "%s%c" str (aref w3-roman-characters j))
			  curcnt (- curcnt 5)))
		(setq k 0)
		(while (< k curcnt)
		  (setq str (format "%s%c" str
				    (aref w3-roman-characters (1- j)))
			k (1+ k)))))))
      (setq curmod (/ curmod 10)
	    j (- j 2)))
    str))
		     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for formatting nested lists in html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-build-table (indent-level)
  "Build a Definition List"
  (set-buffer w3-working-buffer)
  (let ((x (make-string indent-level 9))
	(y (or (nth (1- indent-level) (cdr (assoc "DL" w3-list-chars-assoc)))
	       "*")))
    (goto-char (point-min))
    (w3-replace-regexp "[ \\\t\\\n]*<DD[^>]*>[ \\\t\\\n]*"
		       (concat "\n" x "  "))
    (w3-replace-regexp "[ \\\t\\\n]*<DT[^>]*>[ \\\t\\\n]*"
		       (concat "\n" x y " "))
    (w3-replace-regexp "</*DL[^>]*>" "\n")))

(defun w3-build-ordered-list (indent-level &optional roman)
  "Builds ordered lists"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let ((x 1) y
	(z (or (nth (1- indent-level) (cdr (assoc "OL" w3-list-chars-assoc)))
	       "."))
	parm url alt)
    (goto-char (point-min))
    (while (re-search-forward "<LI\\([^>]*\\)>[ \\\t]*" nil t)
      (setq parm (buffer-substring (match-beginning 1) (match-end 1)))
      (replace-match "")
      (if (string-match "SRC[ \\\t]*=+[ \\\t]*\"\\([^\"]*\\)\"" parm)
	  (setq url (substring parm (match-beginning 1) (match-end 1)))
	(setq url nil))
      (if (string-match "ALT[ \\\t]*=+[ \\\t]*\"\\([^\"]*\\)\"" parm)
	  (setq alt (substring parm (match-beginning 1) (match-end 1)))
	(setq alt nil))
      (setq y (format "\n%s%3s%s "
		      (make-string indent-level 9)
		      (if roman (w3-decimal-to-roman x)
			(format "%d" x)) z))
      (cond
       ((and (null alt) (null url)	; No alt/img and not in emacs 18
	     (or w3-running-FSF19 w3-running-epoch
		 w3-running-lemacs w3-running-new-lucid))
	(insert y))
       ((and (null alt) (null url))	; No alt/img and in emacs 18
	(insert-before-markers y))
       ((and url w3-running-epoch (fboundp 'add-graphic-zone))
	(insert "^")
	(w3-insert-graphic
	 url (1- (point)) 'center
	 (or alt (nth (1- indent-level) (cdr (assoc "OL" w3-list-chars-assoc)))
	     ".")))
       (alt (insert alt)))
      (setq x (1+ x))))
  (goto-char (point-min))
  (w3-replace-regexp "</*OL[^>]*>" "\n"))

(defun w3-build-unordered-list (indent-level)
  "Build unordered lists"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let ((x (format "\n%s%s " (make-string indent-level 9)
		   (or (nth (1- indent-level)
			    (cdr (assoc "UL" w3-list-chars-assoc))) "*")))
	parm url alt)
    (while (re-search-forward "<LI\\([^>]*\\)>" nil t)
      (setq parm (buffer-substring (match-beginning 1) (match-end 1)))
      (replace-match "")
      (if (string-match "SRC[ \\\t]*=+[ \\\t]*\"\\([^\"]*\\)\"" parm)
	  (setq url (substring parm (match-beginning 1) (match-end 1)))
	(setq url nil))
      (if (string-match "ALT[ \\\t]*=+[ \\\t]*\"\\([^\"]*\\)\"" parm)
	  (setq alt (substring parm (match-beginning 1) (match-end 1)))
	(setq alt nil))
      (cond
       ((and (null alt) (null url)	; No alt/img and not in emacs 18
	     (or w3-running-FSF19 w3-running-epoch
		 w3-running-lemacs w3-running-new-lucid))
	(insert x))
       ((and (null alt) (null url))	; No alt/img and in emacs 18
	(insert-before-markers x))
       ((and url w3-running-epoch (fboundp 'add-graphic-zone))
	(insert "^")
	(w3-insert-graphic
	 url (1- (point)) 'center
	 (or alt (nth (1- indent-level) (cdr (assoc "UL" w3-list-chars-assoc)))
	     "*")))
       (alt (insert alt)))))
  (goto-char (point-min))
  (w3-replace-regexp "</*\\(UL\\|DIR\\|MENU\\)[^>]*>" "\n"))

(defun w3-handle-lists (indent-level)
  "Handle building of lists - INDENT-LEVEL is how many tabs to use
to indent from the left margin."
  (let ((type (upcase (buffer-substring (match-beginning 1) (match-end 1))))
	(parm (upcase (buffer-substring (match-beginning 2) (match-end 2))))
	(pos nil))
    (while (setq pos (w3-sublists-exist type))
      (goto-char pos)
      (setq indent-level (1+ indent-level)
	    type (upcase (buffer-substring
			  (match-beginning 1) (match-end 1)))))
    (narrow-to-region (- (point) (+ 2 (length type) (length parm)))
		      (progn
			(re-search-forward (format "</%s>" type) nil t)
			(point)))
    (cond
     ((equal "OL" type) (w3-build-ordered-list indent-level
					       (string-match "ROMAN" parm)))
     ((equal "DL" type) (w3-build-table indent-level))
     (t (w3-build-unordered-list indent-level)))
    (w3-fill-paragraphs-in-list indent-level type)
    (widen)))

(defun w3-fill-paragraphs-in-list (indent-level type)
  "This will fill all the paragraphs within the current list.  INDENT-LEVEL
is the number of tabs to use as the leading fill."
  (w3-replace-regexp "\\\n\\\n+" "\n")
  (goto-char (point-min))
  (let ((fill-prefix
	 (format "%s%s" (make-string indent-level 9)
		 (if (equal type "OL") "     " "  "))) st nd ptag)
    (while (re-search-forward "^[^\\\n]" nil t)
      (setq st (progn (beginning-of-line) (point))
	    nd (progn (end-of-line) (point)))
      (save-restriction
	(narrow-to-region st nd)
	(goto-char (point-min))
	(while (re-search-forward " *<P\\([^>]*\\)> *" nil t)
	  (setq ptag (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq st (match-beginning 0))
	  (replace-match "\n\n")
	  (if (string-match "ID=\"\\([^\"]+\\)\"" ptag)
	      (w3-add-zone st (progn (end-of-line) (point))
			   w3-default-style
			   (cons 'w3par
				 (list (substring ptag (match-beginning 1)
						  (match-end 1))
				       nil nil nil nil nil nil nil)))))
	(w3-replace-regexp "^\\([^\\\t]\\)" (concat fill-prefix "\\1"))
	(fill-region (point-min) (point-max))))))

(defun w3-sublists-exist (type)
  "Figure out if there are sublists in the current list.  Expects point to
be _AFTER_ the current list tag, and type to be bound to what sort of
list it is (OL, UL, DL, MENU, etc)"
  (save-excursion
    (let* ((thestart  (point))
	   (newend (progn
		     (re-search-forward (format "</%s>" type) nil t)
		     (point))))
      (goto-char thestart)
      (if (re-search-forward "<\\(DL\\|OL\\|UL\\|DIR\\|MENU\\)\\([^>]*\\)>"
			     newend t)
	  (point)
	nil))))

(defun w3-do-lists ()
  (let ((tmp 0)
	(last (point-min)))
    (while (progn
	     (goto-char last)
	     (re-search-forward "<\\(DL\\|OL\\|UL\\|DIR\\|MENU\\)\\([^>]*\\)>"
				nil t))
      (setq last (match-beginning 0))
      (w3-handle-lists 1)
      (setq tmp (1+ tmp))
      (message "Building lists...%s" (make-string tmp ?.)))))

(provide 'w3-lists)
