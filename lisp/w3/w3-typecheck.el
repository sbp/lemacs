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
;;; Type checking for FORMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Date checking, taken from edb.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst weekday-alist
 '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3)
   ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6)
   ("Tues" . 2) ("Thurs" . 4)
   ("Sun" . 0) ("Mon" . 1) ("Tue" . 2) ("Wed" . 3)
   ("Thu" . 4) ("Fri" . 5) ("Sat" . 6)))

(defconst full-monthname-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)))


(defconst monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defconst monthname-alist
  (append monthabbrev-alist
	  full-monthname-alist
	  '(("Sept" . 9))))

(defconst monthname-regexp
  (concat "\\("
	  (mapconcat (function car)
		     monthname-alist
		     "\\|")
	  "\\)\\.?"))

(defconst weekday-regexp
  (concat "\\("
	  (mapconcat (function car)
		     weekday-alist
		     "\\|")
	  "\\)\\.?"))

(defconst monthnumber-regexp "\\(0?[1-9]\\|1[0-2]\\)")
(defconst monthnumber-regexp-two-char "\\(0[1-9]\\|1[0-2]\\)")

(defconst monthday-regexp "\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)")
(defconst monthday-regexp-two-char "\\([0-2][0-9]\\|3[01]\\)")

(defconst full-year-regexp "[0-2][0-9][0-9][0-9]")
(defconst short-year-regexp "[0-9][0-9]")

(defconst year-regexp (concat "\\(" full-year-regexp
			      "\\|" short-year-regexp "\\)"))

(defconst elt-separator-regexp "[ -.,/']+")

(defconst date-regexps
  (list
   ;; MMDDYY
   (cons (concat monthname-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 nil 1 2))
   (cons (concat monthnumber-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 1 nil 2))
   ;; DDMMYY
   (cons (concat monthday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 nil 2 1))
   (cons (concat "\\("
		 monthday-regexp
		 elt-separator-regexp
		 "\\)?"
		 monthname-regexp
		 elt-separator-regexp
		 year-regexp)
	 '(4 nil 3 2))
   (cons (concat monthday-regexp
		 elt-separator-regexp
		 monthnumber-regexp
		 elt-separator-regexp
		 "\\(" full-year-regexp "\\)")
	 '(3 2 nil 1))
   ;; YYMMDD
   ;; Using year-regexp instead of full-year-regexp is ambiguous (consider
   ;; 11-11-11), but we already tried MMDDYY and it failed.
   (cons (concat year-regexp
		 elt-separator-regexp
		 monthname-regexp
		 elt-separator-regexp
		 monthday-regexp)
	 '(1 nil 2 3))
   (cons (concat year-regexp
		 elt-separator-regexp
		 monthnumber-regexp
		 elt-separator-regexp
		 monthday-regexp)
	 '(1 2 nil 3))
   ;; YYMMDD, no separators
   ;; This is ambiguous.
   (cons (concat year-regexp
		 monthnumber-regexp-two-char "?"
		 monthday-regexp-two-char "?")
	 '(1 2 nil 3))
   ;; WWMMDDYY
   (cons (concat weekday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(5 nil 2 3))
   ;; WWDDMMYY
   (cons (concat weekday-regexp
		 elt-separator-regexp
		 monthday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(5 nil 3 2))
   ;; ctime
   (cons (concat
	  weekday-regexp
	  " "
	  monthname-regexp
	  "  ?"
	  monthday-regexp
	  ;; time of day
	  " [0-9:]+ "
	  "\\(" full-year-regexp "\\)")
	 '(4 nil 2 3))
   )
  "Assoc list of regexps and match locators.
A match locator is a list of four numbers indicating which submatch of the
regexp contains the year, month number, month name, and day of the month.
The list elements may be nil if that information is not available.")

(defun w3-datep (date-string)
  "Parse DATE-STRING, and return a date object; err if the parse is invalid.
If DATE-STRING contains only whitespace, return a null date object.
If DATE-STRING is nil, use the result of `parse-date-default-function' instead."
  (let ((regexp-alist date-regexps)
	result)
    (if (zerop (length date-string))	;if empty string,
	(setq result t)			;empty date is kosher
      ;; regexp-alist is nulled if a match is found
      (progn
	(while regexp-alist
	  (if (string-match (concat "^" (car (car regexp-alist)) "$")
			    date-string)
	      (setq regexp-alist nil
		    result t)
	    ;; string-match failed
	    (setq regexp-alist (cdr regexp-alist))))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Integer checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-intp (str)
  "Integer checker"
  (string-match "^[0-9]+$" str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Floating point checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-floatp (str)
  "Floating point checker"
  (let (x y)
    (if (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)$" str)
	(progn
	  (setq x (substring str (match-beginning 1) (match-end 1))
		y (substring str (match-beginning 2) (match-end 2)))
	  (and (w3-intp x) (w3-intp y)))
      (w3-intp str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; URL Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-urlp (str)
  "URL checker..."
  (string-match w3-nonrelative-link str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Option list checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-optionp (val)
  "Option list checker"
  (if (null val)
      (progn
	(message "Please make a selection from the menu")
	nil)
    t))

(defun w3-textp (str) t)		; don't care whats in a text field
(fset 'w3-p 'w3-textp)			; for default of "" to be text
(fset 'w3-passwordp 'w3-textp)		; don't care whats in a paswd either
(fset 'w3-textareap 'w3-textp)		; try this - might work

(defun w3-read-correct-format (type name options num value)
  "Read in a FORMS entry with type TYPE, and do typechecking"
  (let ((func (read (format "w3-%sp" (downcase type))))
	(valu value) exitp)
    (while (not exitp)
      (cond
       ((or (equal "TEXT" type)
	    (equal "" type))
	(setq valu (read-string "Enter text: " valu)))
       ((or (equal "FLOAT" type)
	    (equal "INT" type))
	(setq valu (read-string "Enter numeric value: " valu)))
       ((equal "PASSWORD" type)
	(setq valu (funcall w3-passwd-entry-func "Enter password:" valu)))
       ((equal "OPTION" type)
	(if (or (not window-system)
		(not (fboundp 'w3-x-popup-menu)))
	    (setq valu (completing-read "Please choose: " options nil t valu))
	  (setq valu (w3-x-popup-menu
		      (if (and (boundp 'last-input-event)
			       (listp last-input-event))
			  last-input-event
			(list (list (current-column) 1)
			      (selected-window)))
		      (list "WWW"
			    (cons "Select An Item" options)))))
	(if (consp valu) (setq valu (car valu))))
       ((equal "DATE" type)
	(setq valu (read-string "Enter date: " valu)))
       ((equal "URL" type)
	(setq valu (read-string "Enter valid URL: " valu))))
      (if (not (fboundp func)) (setq func 'w3-textp))
      (if (funcall func valu)
	  (setq exitp t)
	(progn
	  (message "Wrong format for type %s, try again." (downcase type))
	  (sit-for 2))))
    valu))

(provide 'w3-typecheck)
