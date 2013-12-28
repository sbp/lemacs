;; Functions for extending the character set and dealing with case tables.
;; Copyright (C) 1988, 1993 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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


;; Written by:
;; TN/ETX/TX/UMG Howard Gayle        UUCP : seismo!enea!erix!howard
;; Telefonaktiebolaget L M Ericsson  Phone: +46 8 719 55 65
;; Ericsson Telecom     	     Telex: 14910 ERIC S
;; S-126 25 Stockholm                FAX  : +46 8 719 64 82
;; Sweden

(defun describe-buffer-case-table ()
  "Describe the case table of the current buffer."
  (interactive)
  (let ((vector (make-vector 256 nil))
	(case-table (current-case-table))
	(ch 0))
    (with-output-to-temp-buffer "*Help*"
      (set-buffer standard-output)
      (while (< ch 256)
	(cond ((/= ch (downcase ch))
	       (insert (text-char-description ch))
	       (indent-to 16)
	       (insert "uppercase, matches "
		       (text-char-description (downcase ch))
		       "\n"))
	      ((/= ch (upcase ch))
	       (insert (text-char-description ch))
	       (indent-to 16)
	       (insert "lowercase, matches "
		       (text-char-description (upcase ch))
		       "\n"))
;;	      (t
;;	       (insert (text-char-description ch))
;;	       (indent-to 16)
;;	       (insert "case-invariant\n"))
	      )
	(setq ch (1+ ch))))))

(defun invert-case (count)
  "Change the case of the character just after point and move over it.
With arg, applies to that many chars.
Negative arg inverts characters before point but does not move."
  (interactive "p")
  (if (< count 0)
      (progn (setq count (min (1- (point)) (- count)))
	     (forward-char (- count))))
  (while (> count 0)
    (let ((ch (following-char)))
      (cond ((/= (upcase ch) ch)
	     (insert (upcase ch))
	     (delete-char 1))
	    ((/= (downcase ch) ch)
	     (insert (downcase ch))
	     (delete-char 1))
	    (t
	     (forward-char 1))))
    (setq count (1- count))))

(defun set-case-syntax-delims (l r table)
  "Make characters L and R a matching pair of non-case-converting delimiters.
Sets the entries for L and R in standard-case-table,
standard-syntax-table, and text-mode-syntax-table to indicate
left and right delimiters."
  (aset (car table) l l)
  (aset (car table) r r)
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       text-mode-syntax-table)
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       text-mode-syntax-table))

(defun set-case-syntax-pair (uc lc table)
  "Make characters UC and LC a pair of inter-case-converting letters.
Sets the entries for characters UC and LC in
standard-case-table, standard-syntax-table, and
text-mode-syntax-table to indicate an (uppercase, lowercase)
pair of letters."
  (aset (car table) uc lc)
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry lc "w   " text-mode-syntax-table)
  (modify-syntax-entry uc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " text-mode-syntax-table))

(defun set-case-syntax (c syntax table)
  "Make characters C case-invariant with syntax SYNTAX.
Sets the entries for character C in standard-case-table,
standard-syntax-table, and text-mode-syntax-table to indicate this.
SYNTAX should be \" \", \"w\", \".\" or \"_\"."
  (aset (car table) c c)
  (modify-syntax-entry c syntax (standard-syntax-table))
  (modify-syntax-entry c syntax text-mode-syntax-table))

(provide 'case-table)
