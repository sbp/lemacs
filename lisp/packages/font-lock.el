;; Electric Font Lock Mode, by jwz for the LISPM Preservation Society.
;; Copyright (C) 1992 Free Software Foundation, Inc.

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
;; To make the text you type be fontified, use M-x font-lock-mode.
;; When this minor mode is on, the fonts of the current line will be
;; updated with every insertion or deletion.
;;
;; To define new reserved words or other patterns to highlight, use
;; the `font-lock-keywords' variable.  This should be mode-local.
;;
;; To turn this on automatically, add this to your .emacs file:
;;
;;	(setq emacs-lisp-mode-hook '(lambda () (font-lock-mode 1)))
;;
;; On a Sparc2, the initial fontification takes about 12 seconds for a 120k
;; file of C code, using the default configuration.  You can speed this up
;; substantially by removing some of the patterns that are highlighted by
;; default.  Fontifying lisp code is significantly faster, because lisp has a
;; more regular syntax than C, so the expressions don't have to be as hairy.


(or (find-face 'font-lock-comment-face)
    (make-face 'font-lock-comment-face))
(or (face-differs-from-default-p 'font-lock-comment-face)
    (copy-face 'italic 'font-lock-comment-face))

(or (find-face 'font-lock-doc-string-face)
    (make-face 'font-lock-doc-string-face))
(or (face-differs-from-default-p 'font-lock-doc-string-face)
    (copy-face 'font-lock-comment-face 'font-lock-doc-string-face))

(or (find-face 'font-lock-string-face)
    (make-face 'font-lock-string-face))
(or (face-differs-from-default-p 'font-lock-string-face)
    (progn
      (copy-face 'font-lock-doc-string-face 'font-lock-string-face)
      (set-face-underline-p 'font-lock-string-face t)))

(or (find-face 'font-lock-function-name-face)
    (make-face 'font-lock-function-name-face))
(or (face-differs-from-default-p 'font-lock-function-name-face)
    (copy-face 'bold-italic 'font-lock-function-name-face))

(or (find-face 'font-lock-keyword-face)
    (make-face 'font-lock-keyword-face))
(or (face-differs-from-default-p 'font-lock-keyword-face)
    (copy-face 'bold 'font-lock-keyword-face))


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

(defsubst font-lock-context-face (context depth)
  (cond ((eq context 'comment) 'font-lock-comment-face)
	((eq context 'block-comment) 'font-lock-comment-face)
	((eq context 'string)
	 (if (= depth 1)
	     ;; really we should only use this if in position 3 depth 1, but
	     ;; that's too expensive to compute.
	     'font-lock-doc-string-face
	   'font-lock-string-face))
	(t nil)))


(defun font-lock-fontify-region (start end)
  (goto-char start)
  (if (> end (point-max)) (setq end (point-max)))
  (syntactically-sectionize start end
    (function
     (lambda (extent context depth)
       (set-extent-face extent (font-lock-context-face context depth))))
   'font-lock))

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
      (store-match-data data))))


;;; Fontifying arbitrary patterns

(defsubst font-lock-any-extents-p (start end)
  (catch 'done
    (map-extents (function (lambda (extent ignore)
			     (if (eq 'font-lock (extent-data extent))
				 (throw 'done t))))
		 (current-buffer) start end nil)
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
	;; don't fontify this keyword if we're already in some other context.
	(or (if allow-overlap-p nil (font-lock-any-extents-p s e))
	    (progn
	      (setq extent (make-extent s e))
	      (set-extent-face extent face)
	      (set-extent-data extent 'font-lock))))
      (if loudly (message (format "Fontifying buffer... (regexps...%s)"
				  (make-string (setq count (1+ count)) ?.))))
      (setq rest (cdr rest)))))


;; The user level functions

(defvar font-lock-mode nil) ; for modeline
(or (assq 'font-lock-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(font-lock-mode " Font-Lock"))))

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
can use \\[font-lock-fontify-buffer]."
  (interactive "P")
  (let ((on-p (if (null arg)
		  (not font-lock-mode)
		(> (prefix-numeric-value arg) 0))))
    (if (equal (buffer-name) " *Compiler Input*") ; hack for bytecomp...
	(setq on-p nil))
    (or (memq after-change-function
	      '(nil font-lock-after-change-function))
	(error "after-change-function is %s" after-change-function))
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
    (if font-lock-verbose (message "Fontifying buffer..."))
    ;; Turn it on to run hooks and get the right font-lock-keywords.
    (or was-on (font-lock-mode 1))
    (map-extents (function (lambda (x y)
			     (if (eq 'font-lock (extent-data x))
				 (delete-extent x))))
		 (current-buffer) (point-min) (point-max) nil)
    (if font-lock-verbose (message "Fontifying buffer... (syntactically...)"))
    (buffer-syntactic-context-flush-cache)
    (save-excursion
      (font-lock-fontify-region (point-min) (point-max))
      (if font-lock-verbose (message "Fontifying buffer... (regexps...)"))
      (font-lock-hack-keywords (point-min) (point-max) font-lock-verbose))
    (or was-on (font-lock-mode 0)) ; turn it off if it was off.
    (set (make-local-variable 'font-lock-fontified) t)
    (if font-lock-verbose (message "Fontifying buffer... done."))
    ))


;;; Lisp and C mode interface.
;;; This stuff really belongs in lisp-mode.el and c-mode.el.

(defconst lisp-font-lock-keywords
 '(;; highlight defining forms.  This doesnt work too nicely for
   ;; (defun (setf foo) ...) but it does work for (defvar foo) which
   ;; is more important.
   ("^(def[-a-z]+\\s +\\([^ \t\n\)]+\\)" 1 font-lock-function-name-face)
   ;; Too gaudy for me, ma!
;   ("(\\(cond\\|if\\|when\\|unless\\|[ec]?\\(type\\)?case\\)[ \t\n]" . 1)
;   ("(\\(while\\|do\\|let*?\\|flet\\|labels\\|prog[nv12*]?\\)[ \t\n]" . 1)
;   ("(\\(catch\\|\\throw\\|block\\|return\\|return-from\\)[ \t\n]" . 1)
;   ("(\\(save-restriction\\|save-window-restriction\\)[ \t\n]" . 1)
;   ("(\\(save-excursion\\|unwind-protect\\|condition-case\\)[ \t\n]" . 1)
   ;; highlight CL keywords
   ("\\s :\\(\\sw\\|\\s_\\)+\\>" . 1)
   ;; highlight function names in emacs-lisp docstrings
;   ("\\\\\\\\\\[\\([^\]\n]+\\)]" 1 font-lock-keyword-face t)
   ;; highlight words inside `' which tend to be function names
;   ("`\\([^' \n\t][^' \n\t][^' \n\t]+\\)'" 1 font-lock-keyword-face t)
   ;; this is highlights things like (def* (setf foo) (bar baz)), but may
   ;; be slower (I haven't really thought about it)
;   ("^(def[-a-z]+\\s +\\(\\s(\\S)*\\s)\\|\\S(\\S *\\)"
;    1 font-lock-function-name-face)

   ))

(defvar c-font-lock-keywords
  (let ((storage "auto\\|extern\\|register\\|static\\|volatile")
	(prefixes "unsigned\\|short\\|long")
	(types (concat "int\\|char\\|float\\|double\\|void\\|struct\\|"
		       "union\\|enum\\|typedef")))
    (list
;     ;; Look ma, no taste!
;     storage
;     (list (concat "\\(" storage "\\)?\\s *"
;		   "\\(" prefixes "\\)?\\s *"
;		   "\\(" types "\\)\\s +"
;		   "\\(\\(\\sw\\|\\s_\\|[*&]\\)+\\)")
;	   4 'font-lock-function-name-face)
;     (cons (concat
;	    "[ \t]\\("
;	    (mapconcat 'identity
;	     '("for" "while" "do" "return" "goto" "case" "break" "switch")
;	     "\\|")
;	    "\\)[ \t\n(){};,]")
;	   1)

     ;; fontify preprocessor directives as comments.
     '("^#[ \t]*[a-z]+" . font-lock-comment-face)

     ;; fontify names being defined.
     '("^#[ \t]*define[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)" 1
       font-lock-function-name-face)

     ;; fontify the filename in #include <...> as a string.
     '("^#[ \t]*include[ \t]+<\\([^>\n]+\\)>" 1 font-lock-string-face)

     ;; fontify case targets and goto-tags.  This is too slow.
;     "\\(\\sw\\|\\s_\\)+:"

     ;; This is much faster because it's anchored on the left, but it
     ;; doesn't do `goto' targets.  
     '("case[ \t]+\\(\\sw\\|\\s_\\)+:". 1)
     '("\\<\\(default\\):". 1)

     ;; fontify the names of functions being defined.
     ;; I think this should be fast because it's anchored at bol, but it's not.
     (list (concat
	    "^\\(\\(\\sw\\|\\s_\\)+[ \t]+\\)?"	; type specs; there can be no
	     "\\(\\(\\sw\\|\\s_\\)+[ \t]+\\)?"	; more than 3 tokens, right?
	     "\\(\\(\\sw\\|\\s_\\)+[ \t]+\\)?"
	     "\\(\\*+[ \t]*\\)?"		; pointer
	     "\\(\\sw\\|\\s_\\)+[ \t]*(")	; name
	   8 'font-lock-function-name-face)

     ;; This is faster but not by much.  I don't see why not.
;     '("^\\(\\sw\\|\\s_\\)+[ \t]*(" 1 font-lock-function-name-face)

     ;; Fontify structure names
     (list (concat "^\\(typedef[ \t]+struct\\|struct\\|static[ \t]+struct\\)"
		   "[ \t]+\\(\\sw\\|\\s_\\)+[ \t]*\\(\{\\|$\\)")
	   2 'font-lock-function-name-face)

     ;; Fontify variables declared with structures, or typedef names.
;     '("}[ \t*]*\\(\\sw\\|\\s_\\)+[ \t]*[,;]" 1 font-lock-function-name-face)
     )))

(defvar perl-font-lock-keywords
  (list
   "[ \n\t{]*\\(if\\|until\\|while\\|elsif\\|else\\|unless\\|for\\|foreach\\|continue\\|exit\\|die\\|last\\|goto\\|next\\|redo\\|return\\|local\\|exec\\)[ \n\t;(]"
   "\\(#endif\\|#else\\|#ifdef\\|#ifndef\\|#if\\|#include\\|#define\\|#undef\\)"
   '("^[ \n\t]*sub[ \t]+\\([^ \t{]+\\)\\{" . font-lock-function-name-face)
   '("[ \n\t{]*\\(eval\\)[ \n\t(;]" . font-lock-function-name-face)
   '("\\(--- .* ---\\|=== .* ===\\)" . font-lock-doc-string-face)
   ))

(defvar tex-font-lock-keywords
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

(defvar texi-font-lock-keywords
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


;; Kludge
(defun dummy-font-lock-mode-hook ()
  (cond ((memq major-mode '(lisp-mode emacs-lisp-mode))
	 (set (make-local-variable 'font-lock-keywords)
	      lisp-font-lock-keywords))
	((memq major-mode '(c-mode c++-mode))
	 (set (make-local-variable 'font-lock-keywords)
	      c-font-lock-keywords))
	((eq major-mode 'perl-mode)
	 (set (make-local-variable 'font-lock-keywords)
	      perl-font-lock-keywords))
	((eq major-mode 'tex-mode)
	 (set (make-local-variable 'font-lock-keywords)
	      tex-font-lock-keywords))
	((eq major-mode 'texinfo-mode)
	 (set (make-local-variable 'font-lock-keywords)
	      texi-font-lock-keywords))
	))

(add-hook 'font-lock-mode-hook 'dummy-font-lock-mode-hook)
