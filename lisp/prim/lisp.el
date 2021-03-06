;;; lisp.el --- Lisp editing commands for Emacs

;; Copyright (C) 1985, 1986, 1993, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: lisp, languages

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

;;; Commentary:

;; Lisp editing commands to go with Lisp major mode.

;;; Code:

(defvar defun-prompt-regexp nil
  "*Non-nil => regexp to ignore, before the character that starts a defun.
This is only necessary if the opening paren or brace is not in column 0.
See `beginning-of-defun'.")

(defvar parens-require-spaces t
  "Non-nil => `insert-parentheses' should insert whitespace as needed.")

(defun forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
With argument, do it that many times.
Negative arg -N means move backward across N balanced expressions."
  (interactive "_p")
  (or arg (setq arg 1))
  (goto-char (or (scan-sexps (point) arg) (buffer-end arg)))
  (if (< arg 0) (backward-prefix-chars)))

(defun backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With argument, do it that many times.
Negative arg -N means move forward across N balanced expressions."
  (interactive "_p")
  (or arg (setq arg 1))
  (forward-sexp (- arg)))

(defun mark-sexp (arg)
  "Set mark ARG sexps from point.
The place mark goes is the same place \\[forward-sexp] would move to
with the same argument.
Repeat this command to mark more sexps in the same direction."
  (interactive "p")
  (mark-something 'mark-sexp 'forward-sexp arg))

(defun forward-list (&optional arg)
  "Move forward across one balanced group of parentheses.
With argument, do it that many times.
Negative arg -N means move backward across N groups of parentheses."
  (interactive "_p")
  (or arg (setq arg 1))
  (goto-char (or (scan-lists (point) arg 0) (buffer-end arg))))

(defun backward-list (&optional arg)
  "Move backward across one balanced group of parentheses.
With argument, do it that many times.
Negative arg -N means move forward across N groups of parentheses."
  (interactive "_p")
  (or arg (setq arg 1))
  (forward-list (- arg)))

(defun down-list (arg)
  "Move forward down one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still go down a level.
In Lisp programs, an argument is required."
  (interactive "_p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) inc -1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun backward-up-list (arg)
  "Move backward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot.
In Lisp programs, an argument is required."
  (interactive "p")
  (up-list (- arg)))

(defun up-list (arg) 
  "Move forward out of one level of parentheses.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot.
In Lisp programs, an argument is required."
  (interactive "_p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (scan-lists (point) inc 1) (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun kill-sexp (arg)
  "Kill the sexp (balanced expression) following the cursor.
With argument, kill that many sexps after the cursor.
Negative arg -N means kill N sexps before the cursor."
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp arg)
    (kill-region opoint (point))))

(defun backward-kill-sexp (arg)
  "Kill the sexp (balanced expression) preceding the cursor.
With argument, kill that many sexps before the cursor.
Negative arg -N means kill N sexps after the cursor."
  (interactive "p")
  (kill-sexp (- arg)))

(defun beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of defun.
Returns t unless search stops due to beginning or end of buffer.

Normally a defun starts when there is an char with open-parenthesis
syntax at the beginning of a line.  If `defun-prompt-regexp' is
non-nil, then a string which matches that regexp may precede the
open-parenthesis."
  (interactive "_p")
  (and arg (< arg 0) (forward-char 1))
  (and (re-search-backward (if defun-prompt-regexp
			       (concat "^\\s(\\|"
				       "\\(" defun-prompt-regexp "\\)\\s(")
			     "^\\s(")
			   nil 'move (or arg 1))
       (progn (beginning-of-line) t)))

(defun buffer-end (arg)
  (if (> arg 0) (point-max) (point-min)))

(defun end-of-defun (&optional arg)
  "Move forward to next end of defun.  With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

An end of a defun occurs right after the close-parenthesis that matches
the open-parenthesis that starts a defun; see `beginning-of-defun'."
  (interactive "_p")
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (let ((first t))
    (while (and (> arg 0) (< (point) (point-max)))
      (let ((pos (point)) npos)
	(while (progn
		(if (and first
			 (progn
			  (forward-char 1)
			  (beginning-of-defun 1)))
		    nil
		  (or (bobp) (forward-char -1))
		  (beginning-of-defun -1))
		(setq first nil)
		(forward-list 1)
		(skip-chars-forward " \t")
		(if (looking-at "\\s<\\|\n")
		    (forward-line 1))
		(<= (point) pos))))
      (setq arg (1- arg)))
    (while (< arg 0)
      (let ((pos (point)))
	(beginning-of-defun 1)
	(forward-sexp 1)
	(forward-line 1)
	(if (>= (point) pos)
	    (if (beginning-of-defun 2)
		(progn
		  (forward-list 1)
		  (skip-chars-forward " \t")
		  (if (looking-at "[;\n]")
		      (forward-line 1)))
	      (goto-char (point-min)))))
      (setq arg (1+ arg)))))

(defun mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point) nil t)
  (beginning-of-defun)
  (re-search-backward "^\n" (- (point) 1) t))

(defun insert-parentheses (arg)
  "Put parentheses around next ARG sexps.  Leave point after open-paren.
No argument is equivalent to zero: just insert `()' and leave point between.
This command also sometimes inserts a space before and after,
depending on the surrounding characters."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (or (eq arg 0) (skip-chars-forward " \t"))
  (and parens-require-spaces
       (memq (char-syntax (preceding-char)) '(?w ?_ ?\) ))
       (insert " "))
  (insert ?\()
  (save-excursion
    (or (eq arg 0) (forward-sexp arg))
    (insert ?\))
  (and parens-require-spaces
       (memq (char-syntax (following-char)) '(?w ?_ ?\( ))
       (insert " "))))

(defun move-past-close-and-reindent ()
  "Move past next `)', delete indentation before it, then indent after it."
  (interactive)
  (up-list 1)
  (forward-char -1)
  (while (save-excursion		; this is my contribution
	   (let ((before-paren (point)))
	     (back-to-indentation)
	     (= (point) before-paren)))
    (delete-indentation))
  (forward-char 1)
  (newline-and-indent))

(defun lisp-complete-symbol ()
  "Perform completion on Lisp symbol preceding point.
That symbol is compared against the symbols that exist
and any additional characters determined by what is there
are inserted.
If the symbol starts just after an open-parenthesis,
only symbols with function definitions are considered.
Otherwise, all symbols with function definitions, values
or properties are considered."
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (if emacs-lisp-mode-syntax-table
			(set-syntax-table emacs-lisp-mode-syntax-table))
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point))
		(set-syntax-table buffer-syntax)))
	 (pattern (buffer-substring beg end))
	 (predicate
	  (if (eq (char-after (1- beg)) ?\()
	      'fboundp
	    #'(lambda (sym)
		(or (boundp sym) (fboundp sym)
		    (symbol-plist sym)))))
	 (completion (try-completion pattern obarray predicate)))
    (cond ((eq completion t))
	  ((null completion)
	   (message (gettext "Can't find completion for \"%s\"") pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message (gettext "Making completion list..."))
	   (let ((list (all-completions pattern obarray predicate)))
	     (or (eq predicate 'fboundp)
		 (let (new)
		   (while list
		     (setq new (cons (if (fboundp (intern (car list)))
					 (list (car list) " <f>")
				       (car list))
				     new))
		     (setq list (cdr list)))
		   (setq list (nreverse new))))
	     (with-output-to-temp-buffer (gettext "*Help*")
	       (display-completion-list list)))
	   (message (gettext "Making completion list...%s") "done")))))

;;; lisp.el ends here
