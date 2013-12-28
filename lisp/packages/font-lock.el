;; Electric Font Lock Mode, by jwz for the LISPM Preservation Society.
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

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

;; Font-lock-mode is a minor mode that causes your comments to be 
;; displayed in one face, strings in another, reserved words in another,
;; documentation strings in another, and so on.
;;
;; Comments will be displayed in `font-lock-comment-face'.
;; Strings will be displayed in `font-lock-string-face'.
;; Doc strings will be displayed in `font-lock-doc-string-face'.
;; Function and variable names (in their defining forms) will be
;;  displayed in `font-lock-function-name-face'.
;; Reserved words will be displayed in `font-lock-keyword-face'.
;;
;; Don't let the name fool you: you can highlight things using different
;; colors or background stipples instead of fonts, though that is not the
;; default.  See the documentation on faces and how to change their
;; attributes.
;;
;; To make the text you type be fontified, use M-x font-lock-mode.
;; When this minor mode is on, the fonts of the current line will be
;; updated with every insertion or deletion.
;;
;; The `font-lock-keywords' variable defines other patterns to highlight.
;; The default font-lock-mode-hook sets it to the value of the variables
;; lisp-font-lock-keywords, c-font-lock-keywords, etc, as appropriate.
;; The easiest way to change the highlighting patterns is to change the
;; values of c-font-lock-keywords and related variables.  See the doc
;; string of the variable `font-lock-keywords' for the appropriate syntax.
;;
;; To turn this on automatically, add this to your .emacs file:
;;
;;	(setq emacs-lisp-mode-hook '(lambda () (font-lock-mode 1)))
;;	(setq c-mode-hook 	   '(lambda () (font-lock-mode 1)))
;;	(setq c++-mode-hook	   '(lambda () (font-lock-mode 1)))
;;	(setq dired-mode-hook	   '(lambda () (font-lock-mode 1)))
;;
;; and so on.
;;
;; On a Sparc2, the initial fontification takes about 12 seconds for a 120k
;; file of C code, using the default configuration.  You can speed this up
;; substantially by removing some of the patterns that are highlighted by
;; default.  Fontifying lisp code is significantly faster, because lisp has a
;; more regular syntax than C, so the expressions don't have to be as hairy.
;;
;; The default value for `lisp-font-lock-keywords' is the value of the variable
;; `lisp-font-lock-keywords-1'.  You may like `lisp-font-lock-keywords-2' 
;; better; it highlights many more words, but is slower and makes your buffers
;; be very visually noisy.
;;
;; The same is true of `c-font-lock-keywords-1' and `c-font-lock-keywords-2';
;; the former is subdued, the latter is loud.


(make-face 'font-lock-comment-face)
(make-face 'font-lock-doc-string-face)
(make-face 'font-lock-string-face)
(make-face 'font-lock-function-name-face)
(make-face 'font-lock-keyword-face)
(make-face 'font-lock-type-face)

(or (face-differs-from-default-p 'font-lock-comment-face)
    (copy-face 'italic 'font-lock-comment-face))

(or (face-differs-from-default-p 'font-lock-doc-string-face)
    (copy-face 'font-lock-comment-face 'font-lock-doc-string-face))

(or (face-differs-from-default-p 'font-lock-string-face)
    (progn
      (copy-face 'font-lock-doc-string-face 'font-lock-string-face)
      (set-face-underline-p 'font-lock-string-face t)))

(or (face-differs-from-default-p 'font-lock-function-name-face)
    (copy-face 'bold-italic 'font-lock-function-name-face))

(or (face-differs-from-default-p 'font-lock-keyword-face)
    (copy-face 'bold 'font-lock-keyword-face))

(or (face-differs-from-default-p 'font-lock-type-face)
    (copy-face 'italic 'font-lock-type-face))


(defvar font-lock-mode nil) ; for modeline
(or (assq 'font-lock-mode minor-mode-alist)
    (setq minor-mode-alist
	  (purecopy
	   (append minor-mode-alist
		   '((font-lock-mode " Font-Lock"))))))


(make-variable-buffer-local 'font-lock-keywords)
(defvar font-lock-keywords nil
  "*The keywords to highlight.
If this is a list, then elements may be of the forms:

  \"string\"			  ; a regexp to highlight in the 
				  ;  `font-lock-keyword-face'.
  (\"string\" . integer)  	  ; match N of the regexp will be highlighted
  (\"string\" . face-name)	  ; use the named face
  (\"string\" integer face-name)    ; both of the above
  (\"string\" integer face-name t)  ; this allows highlighting to overlap
				  ;  with already-highlighted regions.

These regular expressions should not match text which spans lines.  Multi-line
patterns will be correctly fontified when \\[font-lock-fontify-buffer] is used,
but will not be matched by the auto-fontification that font-lock-mode does,
since it looks at only one line at a time.

Be careful composing regexps for this list; the wrong pattern can dramatically
slow things down!")

(defvar font-lock-keywords-case-fold-search nil
  "*Whether the strings in `font-lock-keywords' should be case-folded.")

(defvar font-lock-verbose t
  "*Whether font-lock-fontify-buffer should print status messages.")

(defvar font-lock-mode-hook nil
  "*Function or functions to run on entry to font-lock-mode.")

(defvar font-lock-use-syntax-tables t
  "Whether font-lock should bother doing syntactic fontification.
This should be true for all ``language'' modes, but other modes, like
dired, do not have anything useful in the syntax tables (no comment
or string delimiters, etc) and so there is no need to use them.
You should not set this variable; its value is automatically computed
by examining the syntax table.")

;;; To fontify the whole buffer, we just go through it a character at a time,
;;; and create new extents when necessary (the extents we create span lines.)
;;;
;;; Each time a modification happens to a line, we remove all of the extents
;;; on that line (splitting line-spanning extents as necessary) and recompute
;;; the contexts for every character on the line.  This means that, as the
;;; user types, we repeatedly go back to the beginning of the line, doing more
;;; work the longer the line gets.  This happens in the buffer-syntactic-
;;; context subr, so it's plenty fast.
;;;
;;; We redo the whole line because that's a lot easier than dealing with the
;;; hair of modifying possibly-overlapping extents, and extents whose 
;;; endpoints were moved by the insertion we are reacting to.
;;;
;;; Extents as they now exist are not a good fit for this project, because
;;; extents talk about properties of *regions*, when what we want to talk
;;; about here are properties of *characters*.  

(defun font-lock-fontify-region (start end)
  (if (not font-lock-use-syntax-tables)
      nil
    (goto-char start)
    (if (> end (point-max)) (setq end (point-max)))
    (syntactically-sectionize start end
      #'(lambda (extent context depth)
          (set-extent-face extent
                           (cond ((eq context 'comment)
                                  'font-lock-comment-face)
                                 ((eq context 'block-comment)
                                  'font-lock-comment-face)
                                 ((eq context 'string)
                                  (if (= depth 1)
                                      ;; really we should only use this if
                                      ;;  in position 3 depth 1, but that's
                                      ;;  too expensive to compute.
                                      'font-lock-doc-string-face
                                      'font-lock-string-face))
                                 (t nil))))
      'font-lock)))

(defun font-lock-unfontify-region (beg end)
  ;; First delete all extents on this line (really, in this region).
  ;; If extents span the line (region), divide them first so that
  ;; previous and following lines are unaffected.
  (let (s e extent2)
    (map-extents
     (function
      (lambda (extent ignore)
	(if (not (eq 'font-lock (extent-data extent)))
	    nil				; if it's not ours, leave it alone...
	  (setq s (extent-start-position extent)
		e (extent-end-position extent))
	  (cond ((< s beg)		; starts before line
		 (set-extent-endpoints extent s (1- beg))
		 (if (> e (1+ end))	; ...and ends after line
		     (progn
		       (setq extent2 (make-extent (1+ end) e))
		       (set-extent-face extent2 (extent-face extent))
		       (set-extent-data extent2 (extent-data extent)))))
		((> e (1+ end))		; starts on line and ends after
		 (set-extent-endpoints extent (1+ end) e))
		(t			; contained on line
		 (delete-extent extent))))))
     (current-buffer) beg end nil)))


(defun font-lock-after-change-function (beg end old-len)
  ;; called when any modification is made to buffer text.
  (if (null font-lock-mode)
      nil
  (save-excursion
    (let ((data (match-data))
	  (zmacs-region-stays zmacs-region-stays)) ; protect from change!
      (goto-char beg)
      (if (or (> old-len 0)		; Deletions mean the cache is invalid.
	      (= (preceding-char) ?\n)	; Insertions at bol/bob mean that the
	      (bobp))			; bol cache might be invalid.
	  (buffer-syntactic-context-flush-cache))
      (goto-char end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (font-lock-unfontify-region beg end)
      (font-lock-fontify-region beg (1+ end))
      (font-lock-hack-keywords beg end)
      ;; it would be bad if `insert' were to stomp the match data...
      (store-match-data data)))))


;;; Fontifying arbitrary patterns

(defsubst font-lock-any-extents-p (start end)
  (catch 'done
    (map-extents #'(lambda (extent ignore)
                     (if (eq 'font-lock (extent-data extent))
                         (throw 'done extent)))
                 (current-buffer) start end)
    nil))

(defun font-lock-hack-keywords (start end &optional loudly)
  (goto-char start)
  (let ((case-fold-search font-lock-keywords-case-fold-search)
	(rest font-lock-keywords)
	(count 0)
	str match face s e extent allow-overlap-p)
    (while rest
      (goto-char start)
      (cond ((consp (car rest))
	     (setq str (car (car rest)))
	     (cond ((consp (cdr (car rest)))
		    (setq match (car (cdr (car rest)))
			  face (car (cdr (cdr (car rest))))
			  allow-overlap-p (car (cdr (cdr (cdr (car rest)))))))
		   ((symbolp (cdr (car rest)))
		    (setq match 0 allow-overlap-p nil
			  face (cdr (car rest))))
		   (t
		    (setq match (cdr (car rest))
			  allow-overlap-p nil
			  face 'font-lock-keyword-face))))
	    (t
	     (setq str (car rest) match 0 allow-overlap-p nil
		   face 'font-lock-keyword-face)))
      (while (re-search-forward str end t)
	(setq s (match-beginning match)
	      e (match-end match))
	(or s (error "expression did not match subexpression %d" match))
	;; don't fontify this keyword if we're already in some other context.
	(or (= s e)
	    (if allow-overlap-p nil (font-lock-any-extents-p s (1- e)))
	    (progn
	      (setq extent (make-extent s e))
	      (set-extent-face extent face)
	      (set-extent-data extent 'font-lock))))
      (if loudly (message (format "Fontifying %s... (regexps...%s)"
				  (buffer-name)
				  (make-string (setq count (1+ count)) ?.))))
      (setq rest (cdr rest)))))


;; The user level functions

(defvar font-lock-fontified nil) ; whether we have hacked this buffer
(put 'font-lock-fontified 'permanent-local t)

(defun font-lock-mode (&optional arg)
  "Toggle Font Lock Mode.
With arg, turn font-lock mode on if and only if arg is positive.
In the font-lock minor mode, text is fontified as you type it:

 - comments are displayed in font-lock-comment-face;
 - strings are displayed in font-lock-string-face;
 - documentation strings are displayed in font-lock-doc-string-face;
 - function and variable names in their defining forms are displayed
   in font-lock-function-name-face;
 - and certain other expressions are displayed in other faces
   according to the value of the variable `font-lock-keywords'.

When font-lock mode is turned on/off, the buffer is fontified/defontified.
To fontify a buffer without having newly typed text become fontified, you
can use \\[font-lock-fontify-buffer].

See the variable `font-lock-keywords' for customization."
  (interactive "P")
  (let ((on-p (if (null arg)
		  (not font-lock-mode)
		(> (prefix-numeric-value arg) 0))))
    (if (or noninteractive	; hack for visiting files in batch mode...
	    (equal (buffer-name) " *Compiler Input*")) ; hack for bytecomp...
	(setq on-p nil))
    (or (memq after-change-function
	      '(nil font-lock-after-change-function))
	(error "after-change-function is %S" after-change-function))
    (if on-p (font-lock-examine-syntax-table))
    (set (make-local-variable 'after-change-function)
	 (if on-p 'font-lock-after-change-function nil))
    (set (make-local-variable 'font-lock-mode) on-p)
    (cond (on-p
	   (run-hooks 'font-lock-mode-hook)
	   (or font-lock-fontified (font-lock-fontify-buffer)))
	  (font-lock-fontified
	   (setq font-lock-fontified nil)
	   (font-lock-unfontify-region (point-min) (point-max))))
    (redraw-mode-line)))


(defun font-lock-fontify-buffer ()
  "Fontify the current buffer the way `font-lock-mode' would:

 - comments are displayed in font-lock-comment-face;
 - strings are displayed in font-lock-string-face;
 - documentation strings are displayed in font-lock-doc-string-face;
 - function and variable names in their defining forms are displayed
   in font-lock-function-name-face;
 - and certain other expressions are displayed in other faces
   according to the value of the variable `font-lock-keywords'.

This can take a while for large buffers."
  (interactive)
  (let ((was-on font-lock-mode)
	(font-lock-verbose (or font-lock-verbose (interactive-p))))
    (if font-lock-verbose (message "Fontifying %s..." (buffer-name)))
    ;; Turn it on to run hooks and get the right font-lock-keywords.
    (or was-on (font-lock-mode 1))
    (font-lock-unfontify-region (point-min) (point-max))
    (if font-lock-verbose (message "Fontifying %s... (syntactically...)"
				   (buffer-name)))
    (buffer-syntactic-context-flush-cache)
    (save-excursion
      (font-lock-fontify-region (point-min) (point-max))
      (if font-lock-verbose (message "Fontifying %s... (regexps...)"
				     (buffer-name)))
      (font-lock-hack-keywords (point-min) (point-max) font-lock-verbose))
    (or was-on		; turn it off if it was off.
	(let ((font-lock-fontified nil)) ; kludge to prevent defontification
	  (font-lock-mode 0)))
    (set (make-local-variable 'font-lock-fontified) t)
    (if font-lock-verbose (message "Fontifying %s... done." (buffer-name)))
    ))

(defun font-lock-examine-syntax-table ()
  "Computes the value of font-lock-use-syntax-tables for this buffer."
  (let ((i (1- (length (syntax-table))))
	(got-one nil))
    (if (eq (syntax-table) (standard-syntax-table))
	;; Assume that modes which haven't bothered to install their own
	;; syntax table don't do anything syntactically interesting.
	;; Really, the standard-syntax-table shouldn't have comments and
	;; strings in it, but changing that now might break things.
	nil
      ;; else map over the syntax table looking for strings or comments.
      (while (>= i 0)
	(if (memq (char-syntax i) '(?\" ?\< ?\> ?\$))
	    (setq got-one t i 0))
	(setq i (1- i))))
    (set (make-local-variable 'font-lock-use-syntax-tables) got-one)))


;;; various mode interfaces

(defconst lisp-font-lock-keywords-1 (purecopy
 '(;;
   ;; highlight defining forms.  This doesn't work too nicely for
   ;; (defun (setf foo) ...) but it does work for (defvar foo) which
   ;; is more important.
   ("^(def[-a-z]+\\s +\\([^ \t\n\)]+\\)" 1 font-lock-function-name-face)
   ;;
   ;; highlight CL keywords
   ("\\s :\\(\\(\\sw\\|\\s_\\)+\\)\\>" . 1)
   ("(:\\(\\(\\sw\\|\\s_\\)+\\)\\>" . 1)
   ;;
   ;; this is highlights things like (def* (setf foo) (bar baz)), but may
   ;; be slower (I haven't really thought about it)
;   ("^(def[-a-z]+\\s +\\(\\s(\\S)*\\s)\\|\\S(\\S *\\)"
;    1 font-lock-function-name-face)
   ))
 "For consideration as a value of `lisp-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst lisp-font-lock-keywords-2 (purecopy
  (append
   lisp-font-lock-keywords-1
   '(;;
     ;; Highlight control structures
     ("(\\(cond\\|if\\|when\\|unless\\|[ec]?\\(type\\)?case\\)[ \t\n]" . 1)
     ("(\\(while\\|do\\|let*?\\|flet\\|labels\\|prog[nv12*]?\\)[ \t\n]" . 1)
     ("(\\(catch\\|\\throw\\|block\\|return\\|return-from\\)[ \t\n]" . 1)
     ("(\\(save-restriction\\|save-window-restriction\\)[ \t\n]" . 1)
     ("(\\(save-excursion\\|unwind-protect\\|condition-case\\)[ \t\n]" . 1)
     ;;
     ;; highlight function names in emacs-lisp docstrings (in the syntax
     ;; that substitute-command-keys understands.)
     ("\\\\\\\\\\[\\([^]\\\n]+\\)]" 1 font-lock-keyword-face t)
     ;;
     ;; highlight words inside `' which tend to be function names
     ("`\\([-a-zA-Z0-9_][-a-zA-Z0-9_][-a-zA-Z0-9_.]+\\)'"
      1 font-lock-keyword-face t)
     )))
 "For consideration as a value of `lisp-font-lock-keywords'.
This does a lot more highlighting.")

;; default to the gaudier variety?
;(defconst lisp-font-lock-keywords lisp-font-lock-keywords-2
;  "Additional expressions to highlight in lisp modes.")
(defconst lisp-font-lock-keywords lisp-font-lock-keywords-1
  "Additional expressions to highlight in lisp modes.")


(defvar c-font-lock-keywords-1 nil
 "For consideration as a value of `c-font-lock-keywords'.
This does fairly subdued highlighting.")

(defvar c-font-lock-keywords-2 nil
 "For consideration as a value of `c-font-lock-keywords'.
This does a lot more highlighting.")

(let ((storage "auto\\|extern\\|register\\|static\\|volatile")
      (prefixes "unsigned\\|short\\|long")
      (types (concat "int\\|char\\|float\\|double\\|void\\|struct\\|"
		     "union\\|enum\\|typedef"))
      (ctoken "\\(\\sw\\|\\s_\\|[:~*]\\)+")
      )
  (setq c-font-lock-keywords-1 (purecopy
   (list
    ;; fontify preprocessor directives as comments.
    '("^#[ \t]*[a-z]+" . font-lock-comment-face)
    ;;
    ;; fontify names being defined.
    '("^#[ \t]*\\(define\\|undef\\)[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)" 2
      font-lock-function-name-face)
    ;;
    ;; fontify other preprocessor lines.
    '("^#[ \t]*\\(if\\|ifn?def\\|elif\\)[ \t]+\\([^\n]+\\)"
      2 font-lock-function-name-face t)
    ;;
    ;; fontify the filename in #include <...>
    ;; don't need to do this for #include "..." because those were
    ;; already fontified as strings by the syntactic pass.
    '("^#[ \t]*include[ \t]+<\\([^>\"\n]+\\)>" 1 font-lock-string-face)
    ;;
    ;; fontify the names of functions being defined.
    ;; I think this should be fast because it's anchored at bol, but it's not.
    (list (concat
	   "^\\(" ctoken "[ \t]+\\)?"	; type specs; there can be no
	   "\\(" ctoken "[ \t]+\\)?"	; more than 3 tokens, right?
	   "\\(" ctoken "[ \t]+\\)?"
	   "\\(\\*+[ \t]*\\)?"		; pointer
	   "\\(" ctoken "\\)" "[ \t]*(")		; name
	  8 'font-lock-function-name-face)
    ;;
    ;; This is faster but not by much.  I don't see why not.
;    (list (concat "^\\(" ctoken "\\)[ \t]*(") 1 'font-lock-function-name-face)
    ;;
    ;; Fontify structure names (in structure definition form).
    (list (concat "^\\(typedef[ \t]+struct\\|struct\\|static[ \t]+struct\\)"
		  "[ \t]+\\(" ctoken "\\)[ \t]*\\(\{\\|$\\)")
	  2 'font-lock-function-name-face)
    ;;
    ;; Fontify case clauses.  This is fast because its anchored on the left.
    '("case[ \t]+\\(\\(\\sw\\|\\s_\\)+\\):". 1)
    '("\\<\\(default\\):". 1)
    )))

  (setq c-font-lock-keywords-2 (purecopy
   (append c-font-lock-keywords-1
    (list
     ;;
     ;; fontify all storage classes and type specifiers
     (cons (concat "\\<\\(" storage "\\)\\>") 'font-lock-type-face)
     (cons (concat "\\<\\(" types "\\)\\>") 'font-lock-type-face)
     (cons (concat "\\<\\(" prefixes "[ \t]+" types "\\)\\>")
	   'font-lock-type-face)
     ;;
     ;; fontify all builtin tokens
     (cons (concat
	    "[ \t]\\("
	    (mapconcat 'identity
	     '("for" "while" "do" "return" "goto" "case" "break" "switch"
	       "if" "then" "else if" "else" "return" "default" "continue"
	       "default"
	       )
	     "\\|")
	    "\\)[ \t\n(){};,]")
	   1)
     ;;
     ;; fontify case targets and goto-tags.  This is slow because the
     ;; expression is anchored on the right.
     "\\(\\sw\\|\\s_\\)+:"
     ;;
     ;; Fontify variables declared with structures, or typedef names.
     '("}[ \t*]*\\(\\(\\sw\\|\\s_\\)+\\)[ \t]*[,;]" 1
       font-lock-function-name-face)
     ;;
     ;; Fontify global variables without a type.
;     '("^\\([_a-zA-Z0-9:~*]+\\)[ \t]*[[;={]" 1 font-lock-function-name-face)

     ))))
  )


;; default to the gaudier variety?
;(defconst c-font-lock-keywords c-font-lock-keywords-2
;  "Additional expressions to highlight in C mode.")
(defconst c-font-lock-keywords c-font-lock-keywords-1
  "Additional expressions to highlight in C mode.")

(defconst c++-font-lock-keywords c-font-lock-keywords
  "Additional expressions to highlight in C++ mode.")


(defconst perl-font-lock-keywords (purecopy
  (list (cons (concat "[ \n\t{]*\\("
                      (mapconcat 'identity
                                 '("if" "until" "while" "elsif" "else"
                                   "unless" "for" "foreach" "continue" "exit"
                                   "die" "last" "goto" "next" "redo" "return"
                                   "local" "exec")
                                 "\\|")
                      "\\)[ \n\t;(]")
              1)
        (mapconcat 'identity
                   '("#endif" "#else" "#ifdef" "#ifndef" "#if" "#include"
                     "#define" "#undef")
                   "\\|")
        '("^[ \n\t]*sub[ \t]+\\([^ \t{]+\\)[ \n\t]*\\{"
          1 font-lock-function-name-face)
        '("[ \n\t{]*\\(eval\\)[ \n\t(;]" 
          1 font-lock-function-name-face)
        ;; '("\\(--- .* ---\\|=== .* ===\\)" 1 font-lock-doc-string-face)
        ))
  "Additional expressions to highlight in Perl-mode.")

(defconst tex-font-lock-keywords (purecopy
  (list
   '("\\(\\\\\\w+\\)" 1 font-lock-keyword-face t)
   '("{\\\\em\\([^}]+\\)}" 1 font-lock-comment-face t)
   '("{\\\\bf\\([^}]+\\)}" 1 font-lock-keyword-face t)
   '("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 font-lock-function-name-face t)
   '("\\\\\\(begin\\|end\\){\\([a-zA-Z0-9\\*]+\\)}"
     2 font-lock-function-name-face t)
   '("[^\\\\]\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
;   '("\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
   ))
  "Additional expressions to highlight in TeX-mode.")

(defconst texi-font-lock-keywords (purecopy
  (list
   "@\\(@\\|[^}\t \n{]+\\)"					;commands
   '("^\\(@c\\|@comment\\)[ \t].*$" . font-lock-comment-face)	;comments
   '("^\\(*.*\\)[\t ]*$" 1 font-lock-function-name-face t)	;menu items
   '("@\\(emph\\|strong\\|b\\|i\\){\\([^}]+\\)" 2 font-lock-comment-face t)
   '("@\\(file\\|kbd\\|key\\){\\([^}]+\\)" 2 font-lock-string-face t)
   '("@\\(samp\\|code\\|var\\){\\([^}]+\\)" 2 font-lock-function-name-face t)
   '("@\\(xref\\|pxref\\){\\([^}]+\\)" 2 font-lock-keyword-face t)
   '("@end *\\([a-zA-Z0-9]+\\)[ \t]*$" 1 font-lock-function-name-face t)
   '("@item \\(.*\\)$" 1 font-lock-function-name-face t)
   '("\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
   ))
  "Additional expressions to highlight in TeXinfo-mode.")

(defconst postscript-font-lock-keywords (purecopy
  (let ((break "][ \t\f\n\r()<>{}/%")
        (broken "[][<>{}]"))
    (list (cons (concat "//?[^" break "]+" broken "*")
                'font-lock-function-name-face)
          (cons "(\\([^)]\\|\\\\.\\|\\\\\n\\)*)"
                'font-lock-string-face)
          (cons (concat broken "+")
                'font-lock-keyword-face)
          (list (concat "[" break "]"
                        "\\("
                        (mapconcat 'identity
                                   (list "[a-zA-Z0-9-._]*def"
                                         "[Dd]efine[a-zA-Z0-9-._]*"
                                         "begin" "end"
                                         "save" "restore" "gsave" "grestore")
                                   "\\|")
                        "\\)"
                        "[" break "]")
                1 'font-lock-keyword-face))))
  "Expressions to highlight in Postscript buffers.")

(defconst dired-font-lock-keywords (purecopy
  (let ((bn (concat "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|"
		    "Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) +[0-9]+ +[0-9:]+")))
    (list
     '("^  [/~].*:$" . bold-italic)				   ; Header
     (list (concat "^\\(\\([^ ].*\\)" bn "\\) \\(.*\\)$") 1 'bold) ; Marked
     (list (concat "^. +d.*" bn " \\(.*\\)$") 2 'bold)		   ; Subdirs
     (list (concat "^. +l.*" bn " \\(.*\\)$") 2 'italic)	   ; Links
     (cons (concat "^. +-..[xsS]......\\|"	; Regular files with executable
		   "^. +-.....[xsS]...\\|"	; or setuid/setgid bits set
		   "^. +-........[xsS]")
	   'bold)
     )))
  "Expressions to highlight in Dired buffers.")


;; Kludge
(defun dummy-font-lock-mode-hook ()
  "Tries to set font-lock-keywords to something appropriate for this mode."
  (let ((major (symbol-name major-mode))
        (try #'(lambda (n)
                 (if (stringp n) (setq n (intern-soft n)))
                 (if (and n
                          (boundp n))
                     n
                     nil))))
    (setq font-lock-keywords 
          (symbol-value
           (or (funcall try (get major-mode 'font-lock-keywords))
               (funcall try (concat major "-font-lock-keywords"))
               (funcall try (and (string-match "-mode\\'" major)
                                 (concat (substring 
                                          major 0 (match-beginning 0))
                                         "-font-lock-keywords")))
               'font-lock-keywords)))))

            
;; These are the modes where the font-lock keywords are not trivially
;; deducible from the mode name (that is, modes where `FOO-mode' does
;; not use `FOO-font-lock-keywords'.)
;;
(put 'emacs-lisp-mode	'font-lock-keywords 'lisp-font-lock-keywords)
(put 'c++-c-mode	'font-lock-keywords 'c-font-lock-keywords)
;; the nine billion names of TeX mode...
(put 'plain-tex-mode	'font-lock-keywords 'tex-font-lock-keywords)
(put 'slitex-tex-mode	'font-lock-keywords 'tex-font-lock-keywords)
(put 'latex-tex-mode	'font-lock-keywords 'tex-font-lock-keywords)
(put 'LaTex-tex-mode	'font-lock-keywords 'tex-font-lock-keywords)
(put 'LaTeX-mode	'font-lock-keywords 'tex-font-lock-keywords)
(put 'LaTeX-mode        'font-lock-keywords 'tex-font-lock-keywords)

(add-hook 'font-lock-mode-hook 'dummy-font-lock-mode-hook)


(provide 'font-lock)
