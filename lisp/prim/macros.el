;;; macros.el --- non-primitive commands for keyboard macros.

;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: abbrev

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

;; Extension commands for keyboard macros.  These permit you to assign
;; a name to the last-defined keyboard macro, expand and insert the
;; lisp corresponding to a macro, query the user from within a macro,
;; or apply a macro to each line in the reason.

;;; Code:

;;;###autoload
(defun name-last-kbd-macro (symbol)
  "Assign a name to the last keyboard macro defined.
Argument SYMBOL is the name to define.
The symbol's function definition becomes the keyboard macro string.
Such a \"function\" cannot be called from Lisp, but it is a valid
editor command."
  (interactive "SName for last kbd macro: ")
  (or last-kbd-macro
      (error "No keyboard macro defined"))
  (and (fboundp symbol)
       (not (stringp (symbol-function symbol)))
       (not (vectorp (symbol-function symbol)))
       (error "Function %s is already defined and not a keyboard macro."
	      symbol))
  (fset symbol last-kbd-macro))

(defun insert-kbd-macro-pretty-string (string)
  ;; Convert control characters to the traditional readable representation:
  ;; put the four characters \M-x in the buffer instead of the one char \370,
  ;; which would deceptively print as `oslash' with the default settings.
  (save-restriction
    (narrow-to-region (point) (point))
    (prin1 string (current-buffer))
    (goto-char (1+ (point-min)))
    (while (not (eobp))
      (cond ((= (following-char)   0) (insert "\\C-@") (delete-char 1))
	    ((= (following-char) ?\n) (insert "\\n") (delete-char 1))
	    ((= (following-char) ?\r) (insert "\\r") (delete-char 1))
	    ((= (following-char) ?\t) (insert "\\t") (delete-char 1))
	    ((= (following-char) ?\e) (insert "\\e") (delete-char 1))
	    ((= (following-char) 127) (insert "\\C-?") (delete-char 1))
	    ((> (following-char) 127)
	     (insert "\\M-")
	     (insert (- (following-char) 128))
	     (delete-char 1)
	     (forward-char -1))
	    ((< (following-char) 32)
	     ;;(insert "\\^") (insert (+ (following-char) 64))
	     (insert "\\C-") (insert (+ (following-char) 96))
	     (delete-char 1)
	     (forward-char -1))
	    (t
	     (forward-char 1))))))

;;;###autoload
(defun insert-kbd-macro (macroname &optional keys)
  "Insert in buffer the definition of kbd macro NAME, as Lisp code.
Optional second argument KEYS means also record the keys it is on
\(this is the prefix argument, when calling interactively.)

This Lisp code will, when executed, define the kbd macro with the
same definition it has now.  If you say to record the keys,
the Lisp code will also rebind those keys to the macro.
Only global key bindings are recorded since executing this Lisp code
always makes global bindings.

To save a kbd macro, visit a file of Lisp code such as your `~/.emacs',
use this command, and then save the file."
  (interactive "CInsert kbd macro (name): \nP")
  (let (definition)
    (if (string= (symbol-name macroname) "")
	(progn
	  (setq macroname 'last-kbd-macro 
                definition last-kbd-macro)
	  (insert "(setq "))
        (progn
          (setq definition (symbol-function macroname))
          (insert "(fset '")))
  (prin1 macroname (current-buffer))
  (insert "\n   ")
  (let ((string (events-to-keys definition t)))
    (if (stringp string)
	(insert-kbd-macro-pretty-string string)
      (prin1 string (current-buffer))))
  (insert ")\n")
  (if keys
      (let ((keys (where-is-internal macroname nil)))
	(while keys
	  (insert "(global-set-key ")
	  (prin1 (car keys) (current-buffer))
	  (insert " '")
	  (prin1 macroname (current-buffer))
	  (insert ")\n")
            (setq keys (cdr keys)))))))

;; #### This loser doesn't work -- says "junk in executing macro at `ESC'."
;;      Someone should debug this someday...
;;;###autoload
(defun kbd-macro-query (flag)
  "Query user during kbd macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a kbd macro.
 You can give different commands each time the macro executes.
Without prefix argument, asks whether to continue running the macro.
Your options are: \\<query-replace-map>
\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the screen, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that."
  (interactive "P")
  (or executing-macro
      defining-kbd-macro
      (error "Not defining or executing kbd macro"))
  (if flag
      (let (executing-macro defining-kbd-macro)
	(recursive-edit))
    (if (not executing-macro)
	nil
      (let ((loop t)
	    (msg (substitute-command-keys
                  "Proceed with macro?\\<query-replace-map>\
 (\\[act], \\[skip], \\[exit], \\[recenter], \\[edit]) ")))
	(while loop
	  (let ((key (let ((executing-macro nil)
			    (defining-kbd-macro nil))
                       (message msg)
                       (read-char)))
                def)
	    (setq key (vector key))
	    (setq def (lookup-key query-replace-map key))
	    (cond ((eq def 'act)
		   (setq loop nil))
		  ((eq def 'skip)
		   (setq loop nil)
		   (setq executing-macro ""))
		  ((eq def 'exit)
		   (setq loop nil)
		   (setq executing-macro t))
		  ((eq def 'recenter)
		   (recenter nil))
		  ((eq def 'edit)
		   (let (executing-macro defining-kbd-macro)
		     (recursive-edit)))
		  ((eq def 'quit)
		   (setq quit-flag t))
		  (t
		   (or (eq def 'help)
		       (ding))
		   (with-output-to-temp-buffer "*Help*"
		     (princ
		      (substitute-command-keys
		       "Specify how to proceed with keyboard macro execution.
Possibilities: \\<query-replace-map>
\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the screen, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that."))))
		  )))))))

;;;###autoload
(defun apply-macro-to-region-lines (top bottom &optional macro)
  "For each complete line between point and mark, move to the beginning
of the line, and run the last keyboard macro.

When called from lisp, this function takes two arguments TOP and
BOTTOM, describing the current region.  TOP must be before BOTTOM.
The optional third argument MACRO specifies a keyboard macro to
execute.

This is useful for quoting or unquoting included text, adding and
removing comments, or producing tables where the entries are regular.

For example, in Usenet articles, sections of text quoted from another
author are indented, or have each line start with `>'.  To quote a
section of text, define a keyboard macro which inserts `>', put point
and mark at opposite ends of the quoted section, and use
`\\[apply-macro-to-region-lines]' to mark the entire section.

Suppose you wanted to build a keyword table in C where each entry
looked like this:

    { \"foo\", foo_data, foo_function }, 
    { \"bar\", bar_data, bar_function },
    { \"baz\", baz_data, baz_function },

You could enter the names in this format:

    foo
    bar
    baz

and write a macro to massage a word into a table entry:

    \\C-x (
       \\M-d { \"\\C-y\", \\C-y_data, \\C-y_function },
    \\C-x )

and then select the region of un-tablified names and use
`\\[apply-macro-to-region-lines]' to build the table from the names.
"
  (interactive "r")
  (or macro
      (progn
	(if (null last-kbd-macro)
	    (error "No keyboard macro has been defined."))
	(setq macro last-kbd-macro)))
  (save-excursion
    (let ((end-marker (progn
			(goto-char bottom)
			(beginning-of-line)
			(point-marker)))
	  next-line-marker)
      (goto-char top)
      (if (not (bolp))
	  (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
	(goto-char next-line-marker)
	(save-excursion
	  (forward-line 1)
	  (set-marker next-line-marker (point)))
	(save-excursion
	  (execute-kbd-macro (or macro last-kbd-macro))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))))

;;; macros.el ends here
