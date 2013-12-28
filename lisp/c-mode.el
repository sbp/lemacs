;; C code editing commands for Emacs
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(defvar c-mode-abbrev-table nil
  "Abbrev table in use in C-mode buffers.")
(define-abbrev-table 'c-mode-abbrev-table ())

(defvar c-mode-map ()
  "Keymap used in C mode.")
(if (null c-mode-map)
    (setq c-mode-map (make-sparse-keymap)))

(define-key c-mode-map "{" 'electric-c-brace)
(define-key c-mode-map "}" 'electric-c-brace)
(define-key c-mode-map ";" 'electric-c-semi)
(define-key c-mode-map "#" 'electric-c-sharp-sign)
(define-key c-mode-map ":" 'electric-c-terminator)
(define-key c-mode-map "\e{" 'c-insert-braces)
;;; Commented out eletric square brackets because nobody likes them.
;;;(define-key c-mode-map "[" 'c-insert-brackets)
(define-key c-mode-map "\e\C-h" 'mark-c-function)
(define-key c-mode-map "\e\C-q" 'indent-c-exp)
(define-key c-mode-map "\eq" 'c-fill-paragraph)
(define-key c-mode-map "\177" 'backward-delete-char-untabify)
(define-key c-mode-map "\t" 'c-indent-command)

(autoload 'c-macro-expand "cmacexp"
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor."
  t)

(defvar c-mode-syntax-table nil
  "Syntax table in use in C-mode buffers.")

(if c-mode-syntax-table
    ()
  (setq c-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" c-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" c-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" c-mode-syntax-table)
  (modify-syntax-entry ?+ "." c-mode-syntax-table)
  (modify-syntax-entry ?- "." c-mode-syntax-table)
  (modify-syntax-entry ?= "." c-mode-syntax-table)
  (modify-syntax-entry ?% "." c-mode-syntax-table)
  (modify-syntax-entry ?< "." c-mode-syntax-table)
  (modify-syntax-entry ?> "." c-mode-syntax-table)
  (modify-syntax-entry ?& "." c-mode-syntax-table)
  (modify-syntax-entry ?| "." c-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" c-mode-syntax-table))

(defconst c-indent-level 2
  "*Indentation of C statements with respect to containing block.")
(defconst c-brace-imaginary-offset 0
  "*Imagined indentation of a C open brace that actually follows a statement.")
(defconst c-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")
(defconst c-argdecl-indent 5
  "*Indentation level of declarations of C function arguments.")
(defconst c-label-offset -2
  "*Offset of C label lines and case statements relative to usual indentation.")
(defconst c-continued-statement-offset 2
  "*Extra indent for lines not starting new statements.")
(defconst c-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to c-continued-statement-offset.")

(defconst c-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in C code.
If you do not want a leading newline before braces then use:
  (define-key c-mode-map "{" 'electric-c-semi)")

(defconst c-tab-always-indent t
  "*Non-nil means TAB in C mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defun c-mode ()
  "Major mode for editing C code.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments are delimited with /* ... */.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{c-mode-map}
Variables controlling indentation style:
 c-tab-always-indent
    Non-nil means TAB in C mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 c-auto-newline
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in C code.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 c-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to c-continued-statement-offset.
 c-brace-offset
    Extra indentation for line if it starts with an open brace.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or default.

Settings for K&R and BSD indentation styles are
  c-indent-level                5    8
  c-continued-statement-offset  5    8
  c-brace-offset               -5   -8
  c-argdecl-indent              0    8
  c-label-offset               -5   -8

Turning on C mode calls the value of the variable c-mode-hook with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map c-mode-map)
  (setq major-mode 'c-mode)
  (setq mode-name "C")
  (setq local-abbrev-table c-mode-abbrev-table)
  (set-syntax-table c-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'c-indent-region)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'c-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in C code
;; based on its context.
(defun c-comment-indent ()
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (let ((opoint (point)))
      (save-excursion
	(beginning-of-line)
	(cond ((looking-at "[ \t]*}[ \t]*\\($\\|/\\*\\)")
	       ;; A comment following a solitary close-brace
	       ;; should have only one space.
	       (search-forward "}")
	       (1+ (current-column)))
	      ((or (looking-at "^#[ \t]*endif[ \t]*")
		   (looking-at "^#[ \t]*else[ \t]*"))
	       7)			;2 spaces after #endif
	      ((progn
		 (goto-char opoint)
		 (skip-chars-backward " \t")
		 (and (= comment-column 0) (bolp)))
	       ;; If comment-column is 0, and nothing but space
	       ;; before the comment, align it at 0 rather than 1.
	       0)
	      (t
	       (max (1+ (current-column))	;Else indent at comment column
		    comment-column)))))))	; except leave at least one space.

(defun c-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handle C comments.
If point is inside a comment, the current paragraph of the comment
is filled, preserving the comment indentation or line-starting decorations."
  (interactive "P")
  (let ((first-line
	 (save-excursion
	   (beginning-of-line)
	   (skip-chars-forward " \t")
	   (looking-at comment-start-skip))))
    (if (or first-line
	    (eq (calculate-c-indent) t))
	;; Inside a comment: fill one comment paragraph.
	(let ((fill-prefix
	       ;; The prefix for each line of this paragraph
	       ;; is the appropriate part of the start of this line,
	       ;; up to the column at which text should be indented.
	       (save-excursion
		 (beginning-of-line)
		 (if (looking-at "[ \t]*/\\*.*\\*/")
		     (progn (re-search-forward comment-start-skip)
			    (make-string (current-column) ?\ ))
		   (if first-line (forward-line 1))
		   (buffer-substring (point)
				     (progn (move-to-column
					     (calculate-c-indent-within-comment t)
					     t)
					    (point))))))
	      (paragraph-start
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next to.
	       (concat paragraph-start
		       "\\|^[ \t]*/\\*[ \t]*$\\|^[ \t]*\\*/[ \t]*$\\|^[^ \t/*]"))
	      (paragraph-separate
	       (concat paragraph-separate
		       "\\|^[ \t]*/\\*[ \t]*$\\|^[ \t]*\\*/[ \t]*$\\|^[^ \t/*]")))
	  (save-restriction
	    ;; Don't fill the comment together with the code following it.
	    (narrow-to-region (point-min)
			      (save-excursion (search-forward "*/" nil 'move)
					      (forward-line 1)
					      (point)))
	    (fill-paragraph arg)
	    (save-excursion
	      (search-forward "*/")
	      (beginning-of-line)
	      (if (looking-at "[ \t]*\\*/")
		  (delete-indentation)))))
      ;; Outside of comments: do ordinary filling.
      (fill-paragraph arg))))

(defun electric-c-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if c-auto-newline (progn (c-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (c-indent-line)
	  (if c-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(c-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun c-insert-brackets ()
  (interactive)
  (insert ?[)
  (save-excursion
    (insert ?])))

(defun c-insert-braces ()
  (interactive)
  (electric-c-brace 1)
  (newline)
  (c-indent-line)
  (save-excursion
    (newline)
    (insert ?})
    (c-indent-line)))

(defun electric-c-sharp-sign (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (save-excursion
	(skip-chars-backward " \t")
	(bolp))
      (let ((c-auto-newline nil))
	(electric-c-terminator arg))
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-c-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if c-auto-newline
      (electric-c-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-c-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point)))
    (if (and (not arg) (eolp)
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or (= (following-char) ?#)
			;; Colon is special only after a label, or case ....
			;; So quickly rule out most other uses of colon
			;; and do no indentation for them.
			(and (eq last-command-char ?:)
			     (not (looking-at "case[ \t'/(]"))
			     (save-excursion
			       (skip-chars-forward "a-zA-Z0-9_$")
			       (skip-chars-forward " \t")
			       (< (point) end)))
			(progn
			  (beginning-of-defun)
			  (let ((pps (parse-partial-sexp (point) end)))
			    (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (insert last-command-char)
	  (c-indent-line)
	  (and c-auto-newline
	       (not (c-inside-parens-p))
	       (progn
		 (newline)
		 ;; (newline) may have done auto-fill
		 (setq insertpos (- (point) 2))
		 (c-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun c-inside-parens-p ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point)
			    (progn (beginning-of-defun) (point)))
	  (goto-char (point-max))
	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))

(defun c-indent-command (&optional whole-exp)
  "Indent current line as C code, or in some cases insert a tab character.
If c-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as C
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (c-indent-line))
	    beg end)
	(save-excursion
	  (if c-tab-always-indent
	      (beginning-of-line))
	  ;; Find beginning of following line.
	  (save-excursion
	    (forward-line 1) (setq beg (point)))
	  ;; Find first beginning-of-sexp for sexp extending past this line.
	  (while (< (point) beg)
	    (forward-sexp 1)
	    (setq end (point))
	    (skip-chars-forward " \t\n")))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not c-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (c-indent-line))))

(defun c-indent-line ()
  "Indent current line as C code.
Return the amount the indentation changed by."
  (let ((indent (calculate-c-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-c-indent-within-comment)))
	  ((looking-at "[ \t]*#")
	   (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((or (looking-at "case[ \t'/(]")
		      (and (looking-at "[A-Za-z]")
			   (save-excursion
			     (forward-sexp 1)
			     (looking-at ":"))))
		  (setq indent (max 1 (+ indent c-label-offset))))
		 ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (c-backward-to-start-of-if)
				 (current-indentation))))
		 ((and (looking-at "while\\b")
		       (save-excursion
			 (c-backward-to-start-of-do)))
		  ;; This is a `while' that ends a do-while.
		  (setq indent (save-excursion
				 (c-backward-to-start-of-do)
				 (current-indentation))))
		 ((= (following-char) ?})
		  (setq indent (- indent c-indent-level)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent c-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-c-indent (&optional parse-start)
  "Return appropriate indentation for current line as C code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     ;; or may be function argument declaration.
	     ;; Indent like the previous top level line
	     ;; unless that ends in a closeparen without semicolon,
	     ;; in which case this line is the first argument decl.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (if (= (following-char) ?{)
		 0   ; Unless it starts a function body
	       (c-backward-to-noncomment (or parse-start (point-min)))
	       ;; Look at previous line that's at column 0
	       ;; to determine whether we are in top-level decls
	       ;; or function's arg decls.  Set basic-indent accordingly.
	       (let ((basic-indent
		      (save-excursion
			(re-search-backward "^[^ \^L\t\n#]" nil 'move)
			(if (and (looking-at "\\sw\\|\\s_")
				 (looking-at "[^\"\n=]*(")
				 (progn
				   (goto-char (1- (match-end 0)))
				   (forward-sexp 1)
				   (and (< (point) indent-point)
					(not (memq (following-char)
						   '(?\, ?\;))))))
			    c-argdecl-indent 0))))
		 basic-indent)))

;; 		 ;; Now add a little if this is a continuation line.
;; 		 (+ basic-indent (if (or (bobp)
;; 					 (memq (preceding-char) '(?\) ?\; ?\}))
;; 					 ;; Line with zero indentation
;; 					 ;; is probably the return-type
;; 					 ;; of a function definition,
;; 					 ;; so following line is function name.
;; 					 (= (current-indentation) 0))
;;				     0 c-continued-statement-offset))

	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (c-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (or (eq (preceding-char) ?\,)
			(and (eq (preceding-char) ?:)
			     (or (eq (char-after (- (point) 2)) ?\')
				 (memq (char-syntax (char-after (- (point) 2)))
				       '(?w ?_)))))
	       (if (eq (preceding-char) ?\,)
		   (progn (forward-char -1)
			  (c-backward-to-start-of-continued-exp containing-sexp)))
	       (beginning-of-line)
	       (c-backward-to-noncomment containing-sexp))
	     ;; Now we get the answer.
	     (if (and (not (memq (preceding-char) '(nil ?\, ?\; ?\} ?\{)))
		      ;; But don't treat a line with a close-brace
		      ;; as a continuation.  It is probably the
		      ;; end of an enum type declaration.
		      (save-excursion
			(goto-char indent-point)
			(skip-chars-forward " \t")
			(not (= (following-char) ?}))))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  c-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (c-backward-to-start-of-continued-exp containing-sexp)
		   (+ c-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?{))
			  c-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (let ((colon-line-end 0))
		     (while (progn (skip-chars-forward " \t\n")
				   (looking-at "#\\|/\\*\\|case[ \t\n'/(].*:\\|[a-zA-Z0-9_$]*:"))
		       ;; Skip over comments and labels following openbrace.
		       (cond ((= (following-char) ?\#)
			      (forward-line 1))
			     ((= (following-char) ?\/)
			      (forward-char 2)
			      (search-forward "*/" nil 'move))
			     ;; case or label:
			     (t
			      (save-excursion (end-of-line)
					      (setq colon-line-end (point)))
			      (search-forward ":"))))
		     ;; The first following code counts
		     ;; if it is before the line we want to indent.
		     (and (< (point) indent-point)
			  (if (> colon-line-end (point))
			      (- (current-indentation) c-label-offset)
			    (current-column)))))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open brace in column zero, don't let statement
		 ;; start there too.  If c-indent-level is zero,
		 ;; use c-brace-offset + c-continued-statement-offset instead.
		 ;; For open-braces not the first thing in a line,
		 ;; add in c-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop c-indent-level))
			(+ c-brace-offset c-continued-statement-offset)
		      c-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the c-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 c-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun calculate-c-indent-within-comment (&optional after-star)
  "Return the indentation amount for line inside a block comment.
Non-nil arg AFTER-STAR means, if lines in the comment have a leading star,
return the indentation of the text that would follow this star."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if after-star
	  (and (looking-at "\\*")
	       (re-search-forward "\\*[ \t]*")))
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (not after-star)
	   (goto-char (1+ (match-beginning 0))))
      (if (and (looking-at "[ \t]*$") (= (preceding-char) ?\*))
	  (1+ (current-column))
	(current-column)))))


(defun c-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*/")))
	  (search-backward "/*" lim 'move)
	(setq stop (or (<= (point) lim)
		       (save-excursion
			 (while (progn
				  (beginning-of-line)
				  (eq ?\\ (char-after (- (point) 2))))
			   (forward-char -1)
			   (beginning-of-line))
			 (skip-chars-forward " \t")
			 (not (looking-at "#")))))
	(or stop (beginning-of-line))))))

(defun c-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) '(?\) ?\"))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun c-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (and (not (bobp)) (not (zerop if-level)))
      (backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun c-backward-to-start-of-do (&optional limit)
  "If point follows a `do' statement, move to beginning of it and return `t'.
Otherwise return `nil' and don't move point."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((first t)
	(startpos (point))
	(done nil))
    (while (not done)
      (let ((next-start (point)))
	(condition-case nil
	    ;; Move back one token or one brace or paren group.
	    (backward-sexp 1)
	  ;; If we find an open-brace, we lose.
	  (error (setq done 'fail)))
	(if done
	    nil
	  ;; If we reached a `do', we win.
	  (if (looking-at "do\\b")
	      (setq done 'succeed)
	    ;; Otherwise, if we skipped a semicolon, we lose.
	    ;; (Exception: we can skip one semicolon before getting
	    ;; to a the last token of the statement, unless that token
	    ;; is a close brace.)
	    (if (save-excursion
		  (forward-sexp 1)
		  (or (and (not first) (= (preceding-char) ?}))
		      (search-forward ";" next-start t
				      (if (and first
					       (/= (preceding-char) ?}))
					  2 1))))
		(setq done 'fail)
	      (setq first nil)
	      ;; If we go too far back in the buffer, we lose.
	      (if (< (point) limit)
		  (setq done 'fail)))))))
    (if (eq done 'succeed)
	t
      (goto-char startpos)
      nil)))

(defun mark-c-function ()
  "Put mark at end of C function, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point))
  (beginning-of-defun)
  (backward-paragraph)
  (zmacs-activate-region))

(defun indent-c-exp (&optional endpos)
  "Indent each line of the C grouping following point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (let* ((indent-stack (list nil))
	 (opoint (point))  ;; May be altered below.
	 (contain-stack
	  (list (if endpos
		    (let (funbeg)
		      ;; Find previous fcn-start.
		      (save-excursion (forward-char 1)
				      (beginning-of-defun)
				      (setq funbeg (point)))
		      ;; Try to find containing open,
		      ;; but don't scan past that fcn-start.
		      (save-restriction
			(narrow-to-region funbeg (point))
			(condition-case nil
			    (save-excursion
			      (backward-up-list 1) (point))
			  ;; We gave up: must be between fcns.
			  ;; Set opoint to beg of prev fcn
			  ;; since otherwise calculate-c-indent
			  ;; will get wrong answers.
			  (error (setq opoint funbeg)
				 (point)))))
		  (point))))
	 (case-fold-search nil)
	 restart outer-loop-done inner-loop-done state ostate
	 this-indent last-sexp
	 at-else at-brace at-while
	 last-depth
	 (next-depth 0))
    ;; If the braces don't match, get an error right away.
    (save-excursion
      (forward-sexp 1))
    ;; Realign the comment on the first line, even though we don't reindent it.
    (save-excursion
      (let ((beg (point)))
	(and (re-search-forward
	      comment-start-skip
	      (save-excursion (end-of-line) (point)) t)
	     ;; Make sure the comment starter we found
	     ;; is not actually in a string or quoted.
	     (let ((new-state
		    (parse-partial-sexp beg (point)
					nil nil state)))
	       (and (not (nth 3 new-state)) (not (nth 5 new-state))))
	    (progn (indent-for-comment) (beginning-of-line)))))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp))
		  (if endpos (< (point) endpos)
		    (not outer-loop-done)))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq inner-loop-done nil)
	(while (and (not inner-loop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (c-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq inner-loop-done t)))
	(and endpos
	     (while (< next-depth 0)
	       (setq indent-stack (append indent-stack (list nil)))
	       (setq contain-stack (append contain-stack (list nil)))
	       (setq next-depth (1+ next-depth))
	       (setq last-depth (1+ last-depth))
	       (setcar (nthcdr 6 state) (1+ (nth 6 state)))))
	(setq outer-loop-done (and (not endpos) (<= next-depth 0)))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (/= (char-after (car contain-stack)) ?{)
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (setq at-else (looking-at "else\\W"))
		    (setq at-brace (= (following-char) ?{))
		    (setq at-while (looking-at "while\\b"))
		    (c-backward-to-noncomment opoint)
		    (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?: ?{)))
			;; Preceding line did not end in comma or semi;
			;; indent this line  c-continued-statement-offset
			;; more than previous.
			(progn
			  (c-backward-to-start-of-continued-exp (car contain-stack))
			  (setq this-indent
				(+ c-continued-statement-offset (current-column)
				   (if at-brace c-continued-brace-offset 0))))
		      ;; Preceding line ended in comma or semi;
		      ;; use the standard indent for this level.
		      (cond (at-else (progn (c-backward-to-start-of-if opoint)
					    (setq this-indent
						  (current-indentation))))
			    ((and at-while (c-backward-to-start-of-do opoint))
			     (setq this-indent (current-indentation)))
			    (t (setq this-indent (car indent-stack)))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-c-indent
			   (if (car indent-stack)
			       (- (car indent-stack))
			     opoint))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (or (looking-at "case[ \t'/(]")
		    (and (looking-at "[A-Za-z]")
			 (save-excursion
			   (forward-sexp 1)
			   (looking-at ":"))))
		(setq this-indent (max 1 (+ this-indent c-label-offset))))
	    (if (= (following-char) ?})
		(setq this-indent (- this-indent c-indent-level)))
	    (if (= (following-char) ?{)
		(setq this-indent (+ this-indent c-brace-offset)))
	    ;; Don't leave indentation in empty lines.
	    (if (eolp) (setq this-indent 0))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(= (following-char) ?\#)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(let ((beg (point)))
		  (and (re-search-forward
			comment-start-skip
			(save-excursion (end-of-line) (point)) t)
		       ;; Make sure the comment starter we found
		       ;; is not actually in a string or quoted.
		       (let ((new-state
			      (parse-partial-sexp beg (point)
						  nil nil state)))
			 (and (not (nth 3 new-state)) (not (nth 5 new-state))))
		      (progn (indent-for-comment) (beginning-of-line)))))))))))

;; Indent every line whose first char is between START and END inclusive.
(defun c-indent-region (start end)
  (save-excursion
    (goto-char start)
    (let ((endmark (copy-marker end)))
      (and (bolp) (not (eolp))
	   (c-indent-line))
      (indent-c-exp endmark)
      (set-marker endmark nil))))
