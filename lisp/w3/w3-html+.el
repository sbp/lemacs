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
;;; Experimental HTML+ parsing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

(defun w3-determine-table-width ()
  "Figure out how wide (number of columns) a data table is
Expects the region to be narrowed between the <TBL> and </TBL> tags."
  (goto-char (point-min))
  (re-search-forward "^.*</TABLE>" nil t)
  (let ((nd (match-beginning 0))  ;; where the table ends
	(ln 0)                    ;; highest width found so far
	(cur-ln 0))               ;; current lines width
    (goto-char (point-min))
    (forward-line 1)
    (while (/= (point) nd)
      (save-restriction
	(let ((st (progn (beginning-of-line) (point)))
	      (nd (if (re-search-forward "<TR>" nil t) (point)
		    (progn (end-of-line) (point)))))
	  (if (not (equal (buffer-substring (point) (1+ (point))) "\n"))
	      (insert "\n"))
	  (narrow-to-region st nd)
	  (goto-char (point-min))
	  (setq cur-ln (w3-count-occurences "<T[DH][^>]*>"))
	  (if (> cur-ln ln) (setq ln cur-ln))
	  (goto-char (point-min))))
      (forward-line 1)
      (beginning-of-line))
    ln))

(defun w3-parse-a-table ()
  "Parse a table.  Expects region to be narrowed between <TABLE> and
</TABLE>."
  (let ((ptree nil)
	tag parm
	(last (point-min)))
    (w3-replace-regexp "</*TABLE[^>]*>\\\n*" "")
    (goto-char (point-min))
    (while (re-search-forward "\\\n*<\\(T[HDR]\\)\\([^>]*\\)>" nil t)
      (setq tag (buffer-substring (match-beginning 1) (match-end 1))
	    parm (buffer-substring (match-beginning 2) (match-end 2))
	    ptree (nconc ptree
			 (list (buffer-substring last (match-beginning 0))
			       (cons tag parm)))
	    last (match-end 0)))
    (nconc ptree (list (buffer-substring last (point-max))))))      

(defun w3-handle-tables ()
  "Build tables from HTML+ source"
;  (w3-replace-regexp "\\\n*<TR>\\\n*" "<TR>\n")
;  (goto-char (point-min))
;  (while (re-search-forward "<TBL[ \\\t]+\\([^>]*\\)>" nil t)
;    (let ((st (match-beginning 0))
;	  (nd (progn
;		(re-search-forward "</TBL>" nil t)
;		(point))))
;      (narrow-to-region st nd)
;      (w3-table-helper)
;      (widen))))
)
(defun w3-table-build-it (width)
  "Build the headers for the current table"
  (goto-char (point-min))
  (while (re-search-forward "<T[DH]\\([^>]*\\)>" nil t)
    (let ((mod (buffer-substring (match-beginning 1) (match-end 1)))
	  (ln 1)
	  (tmp 0)
	  (str ""))
      (replace-match "|")
      (if (string-match "ROWSPAN[ \\\t]*=[ \\\t]*\"\\([0-9]+\\)\"" mod)
	  (setq ln (string-to-int (substring mod (match-beginning 1)
					     (match-end 1)))))
      (setq tmp (point))
      (if (re-search-forward "<T[HDR][^>]*>" nil t)
	  (progn
	    (setq str (buffer-substring tmp (match-beginning 0)))
	    (goto-char tmp)
	    (if (< (length str) (+ (* ln width) (if (> ln 1) (1- ln) 0)))
		(insert (make-string
			 (- (+ (* ln width) (if (> ln 1) (1- ln) 0))
			    (length str)) 32))))))))

(provide 'w3-html+)
