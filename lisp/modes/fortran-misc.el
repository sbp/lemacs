;;; fortran-misc.el --- Routines than can be used with fortran mode.

;;; Copyright (c) 1992 Free Software Foundation, Inc.

;; Author: Various authors.
;; Maintainer:
;; Version
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This file contains various routines that may be useful with GNU emacs
;;; fortran mode, but just don't seem to fit in.

(defun fortran-fill-statement ()
  "Fill a fortran statement up to `fill-column'."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(or (looking-at "[ \t]*$")
	    (looking-at comment-line-start-skip)
	    (and comment-start-skip
		 (looking-at (concat "[ \t]*" comment-start-skip)))))
      (fortran-indent-line)
    (let ((opos (point)) (beg) (cfi))
      (save-excursion
	(fortran-next-statement)
	(fortran-previous-statement)
	(setq cfi (calculate-fortran-indent))
	(setq beg (point)))
      (save-excursion
	(goto-char beg)
	(save-excursion
	  ;;(beginning-of-line)
	  (if (or (not (= cfi (fortran-current-line-indentation)))
		  (and (re-search-forward "^[ \t]*[0-9]+" (+ (point) 4) t)
		       (not (fortran-line-number-indented-correctly-p))))
	      (fortran-indent-to-column cfi)))
	(while (progn 
		 (forward-line 1)
		 (or (looking-at "     [^ 0\n]")
		     (looking-at "\t[1-9]")))
	  (delete-indentation)
	  (delete-char 2)
	  (delete-horizontal-space))
	(fortran-previous-statement)
	(if (> (save-excursion (end-of-line) (current-column)) fill-column)
	    (fortran-do-auto-fill)))
      (if (< (point) opos) (goto-char opos))
      (let ((cfi (calculate-fortran-indent)))
	(if (< (current-column) cfi)
	  (move-to-column cfi))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The upcase/downcase and beautifier code is originally from Ralph Finch
;;; (rfinch@water.ca.gov).
;;;
(defun fortran-downcase-subprogram ()
  "Properly downcases the Fortran subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-fortran-subprogram)
    (message "Downcasing subprogram...")
    (fortran-downcase-region (point) (mark)))
  (message "Downcasing subprogram...done."))

(defun fortran-downcase-region (start end)
  "Downcase region, excluding comment lines and anything
between quote marks."
  (interactive "r")
  (fortran-case-region start end nil))

(defun fortran-upcase-region (start end)
  "Upcase region, excluding comment lines and anything
between quote marks."
  (interactive "r")
  (fortran-case-region start end t))

(defun fortran-upcase-subprogram ()
  "Properly upcases the Fortran subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-fortran-subprogram)
    (message "Upcasing subprogram...")
    (fortran-upcase-region (point) (mark)))
  (message "Upcasing subprogram...done."))

(defun fortran-case-region (start end up)
  "Upcase region if UP is t, downcase, if UP downcase region,
 excluding comment lines and anything between quote marks."
  (let* ((start-re-comment "^[cC*#]")
	 (end-re-comment "$")
	 (start-re-quote "'")
	 (end-re-quote "\\('\\|$\\)")
	 (start-re-dquote (char-to-string ?\"))
	 (end-re-dquote (concat "\\(" start-re-dquote "\\|$\\)"))
	 (strt) (fin))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(if (inside-re start-re-comment end-re-comment)
	    (re-search-forward end-re-comment end 0))
	(if (inside-re start-re-quote end-re-quote)
	    (re-search-forward end-re-quote end 0))
	(if (inside-re start-re-dquote end-re-dquote)
	    (re-search-forward end-re-dquote end 0))
	(setq strt (point))
	(while (< (point) (point-max))
	  (re-search-forward
	   (concat "\\(" start-re-comment "\\|"
		   start-re-quote "\\|" start-re-dquote "\\)") end 0)
	  (setq fin (point))
	  (if up
	      (upcase-region strt fin)
	    (downcase-region strt fin))
	  (if (inside-re start-re-comment end-re-comment)
	      (re-search-forward end-re-comment end 0))
	  (if (inside-re start-re-quote end-re-quote)
	      (re-search-forward end-re-quote end 0))
	  (if (inside-re start-re-dquote end-re-dquote)
	      (re-search-forward end-re-dquote end 0))
	  (setq strt (point)))))))

(defun inside-re (start-re end-re)
  "Returns t if inside a starting regexp and an ending regexp
on the same line."
  (interactive "s")
  (let ((start-line) (end-line))
    (save-excursion
      (setq start-line (progn (beginning-of-line) (point)))
      (setq end-line (progn (end-of-line) (point))))
    (if (and (save-excursion
	       (re-search-backward start-re start-line t))
	     (save-excursion
	       (re-search-forward end-re end-line t)))
	t
      nil)))

;;; Note: Just as with some other routines, fortran-beautify-line
;;;       assumes trailing blanks are not significant. Code may need
;;;       to be adjusted to comply with this.
				       

(defun fortran-beautify-subprogram (&optional downit)
  "Beautify Fortran subprogram:
1) Remove trailing blanks.
2) Replace all continuation characters with fortran-continuation-char.
3) Replace all empty comment lines with blank lines.
4) Replace all multiple blank lines with one blank line.
5) Indent.
6) With prefix arg, downcase the subprogram, avoiding comments and
quoted strings."
  (interactive "P")
  (save-excursion
    (mark-fortran-subprogram)
    (message "Beautifying subprogram...")
    (fortran-beautify-region (point) (mark) downit))
  (message "Beautify subprogram...done."))

(defun fortran-beautify-region (start end &optional downit)
  "Beautify region in a Fortran program:
1) Remove trailing blanks.
2) Replace all continuation characters with fortran-continuation-char.
3) Replace all empty comment lines with blank lines.
4) Replace all multiple blank lines with one blank line.
5) Indent.
6) With prefix arg, downcase the region, avoiding comments and
 quoted strings."
  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (let ((m1 (make-marker))
	    (m2 (make-marker)))
	(set-marker m1 start)
	(set-marker m2 end)
	(indent-region start end nil)
	(narrow-to-region m1 m2)
	(goto-char (point-min))		; trailing blanks
	(perform-replace "[ \t]+$" "" nil t nil)
	(goto-char (point-min))		; continuation characters
	(perform-replace (concat "^     [^ " fortran-continuation-string
				 "]" )
			 (concat "     " fortran-continuation-string)
			 nil t nil)
	(goto-char (point-min))		; empty comments
	(perform-replace "^[cC][ \t]*$" "" nil t nil)
	(goto-char (point-min))		; multiple blank lines
	(perform-replace "\n\n\n+" "\n\n" nil t nil)
	(if downit
	    (fortran-downcase-region (point-min) (point-max)))
	)))

)
