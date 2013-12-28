;;; -*-Emacs-Lisp-*-
;;;%Header
;;; Lisp mode extensions from the ILISP package.
;;; Copyright (C) 1990, 1991, 1992 Chris McConnell, ccm@cs.cmu.edu.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; When loaded this file adds new functionality to emacs lisp mode
;;; and lisp mode. 
;;; 
;;; Default bindings:
;;;
;;; M-x find-unbalanced-lisp find unbalanced parens in the current
;;; buffer.  With a prefix in the current region. 
;;;
;;; ] Close all open parentheses back to the start of the containing
;;; sexp, or to a previous left bracket which will be converted to a
;;; left paren.
;;;
;;; M-q Reindent comments or strings in paragraph chunks or reindent
;;; the containing sexp.
;;;
;;; M-x comment-region-lisp inserts prefix copies of the comment-start
;;; character before lines in the region and the comment-end character
;;; at the end of each line.  If called with a negative prefix, that
;;; many copies are removed.
;;;
;;; C-M-r repositions the first line of the current defun to the top
;;; of the current window.
;;;
;;; C-M-l switches the current window to the previously seen buffer.
;;;
;;; EXAMPLE .emacs:
;;;
;;; (setq ilisp-ext-load-hook 
;;;   '(lambda () (define-key global-map "\C-\M-l" 'previous-buffer-lisp)))
;;; (require 'ilisp-ext)

;;;%Syntax
;;; This makes it so that .'s are treated as normal characters so that
;;; 3.141 gets treated as a single lisp token.  This does cause dotted
;;; pairs to be treated weird though.
(modify-syntax-entry ?. "_" lisp-mode-syntax-table)

;;; Brackets match
(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)

;;;%Globals
(defvar ilisp-ext-load-hook nil "Hook to run when extensions are loaded.")
(defvar left-delimiter "\(" "*Left delimiter for find-unbalanced.")
(defvar right-delimiter "\)" "*Right delimiter for find-unbalanced.")

;;; Copies of ilisp var definitions
(defvar ilisp-complete nil "T when ilisp is in completion mode.")
(defvar ilisp-modes '(ilisp-mode) "List of all inferior ilisp modes.")

;;;%Utils
;;; This should be in emacs, but it isn't.
(defun lisp-mem (item list &optional elt=)
  "Test to see if ITEM is equal to an item in LIST.
Option comparison function ELT= defaults to equal."
  (let ((elt= (or elt= (function equal)))
	(done nil))
    (while (and list (not done))
      (if (funcall elt= item (car list))
	  (setq done list)
	  (setq list (cdr list))))
    done))

;;;
(defun lisp-defun-begin ()
  "Go to the start of the containing defun and return point."
  (let (begin)
    (if (memq major-mode ilisp-modes)
	(lisp-input-start)
	(if (or (eobp) (not (and (bolp) (= (char-after (point)) ?\())))
	    (beginning-of-defun))
	(point))))

;;;
(defun lisp-defun-end (&optional no-errorp at-beginp)
  "Go to the end of the containing defun and return point or nil if
there is no end."
  (if (not at-beginp) (lisp-defun-begin))
  (condition-case ()
      (progn
	(lisp-skip (point-max))		;To skip comments on defun-end
	(forward-sexp)
	(point))
    (error (if no-errorp nil (error "Unbalanced parentheses")))))

;;;
(defun lisp-find-next-start ()
  "Find the start of the next line at the left margin that starts with
a character besides whitespace, a \) or ;;; and return the
point."
  (if (eobp)
      (point-max)
      (save-excursion
	(forward-char)
	(if (re-search-forward "^\\(\\(;;;\\)\\|\\([^ \t\n\);]\\)\\)" nil t)
	    (match-beginning 0)
	    (point-max)))))

;;;
(defun lisp-end-defun-text (&optional at-start)
  "Go the end of the text associated with the current defun and return
point.  The end is the last character before whitespace leading to
a left paren or ;;; at the left margin unless it is in a string."
  (if (not at-start) (lisp-defun-begin))
  (let ((point (point))
	(boundary (lisp-find-next-start))
	(final (save-excursion
		 (condition-case ()
		     (progn (forward-sexp) (point))
		   (error (point-max))))))
    ;; Find the next line starting at the left margin and then check
    ;; to see if it is in a string. 
    (while (progn
	     (skip-chars-forward "^\"" boundary) ;To the next string
	     (if (= (point) boundary)	
		 nil			;No quote found and at limit
		 (let ((string-boundary ;Start of next defun
			(save-excursion
			  (if (re-search-forward "^\(\\|^;;;" nil t)
			      (match-beginning 0)
			      (point-max)))))
		   (if (condition-case ()
			   (progn (forward-sexp) t)
			 (error (goto-char string-boundary) nil))
		       (if (>= (point) boundary)
			   ;; Boundary was in string
			   (if (> (point) string-boundary)
			       (progn	;String ended in next defun
				 (goto-char string-boundary)
				 nil)
			       (if (> (setq boundary
					    (lisp-find-next-start))
				      final)
				   ;; Normal defun
				   (progn (goto-char final) nil)
				   t))
			   t)
		       ;; Unclosed string
		       nil)))))
    (re-search-backward  "^[^; \t\n]\\|^[^;\n][ \t]*[^ \t\n]" point t)
    (end-of-line)
    (skip-chars-backward " \t")
    (if (< (point) point)
	(goto-char point)
	(if (save-excursion
	      (let ((point (point)))
		(beginning-of-line)
		(if comment-start (search-forward comment-start point t))))
	    (progn (next-line 1) (indent-line-ilisp)))
	(point))))

;;;
(defun lisp-in-comment (test)
  "Return T if you are in a comment."
  (beginning-of-line)
  (and (looking-at test)
       (not (= (match-end 0)
	       (progn (end-of-line) (point))))))

;;;
(defun lisp-in-string (&optional begin end)
  "Return the string region that immediately follows/precedes point or
that contains point in optional region BEGIN to END.  If point is in
region, T will be returned as well."
  (save-excursion
    (if (not begin)
	(save-excursion
	  (setq end (lisp-end-defun-text)
		begin (lisp-defun-begin))))
    (let* ((point (progn (skip-chars-forward " \t") (point)))
	   (done nil))
      (goto-char begin)
      (while (and (< (point) end) (not done))
	(skip-chars-forward "^\"" end)
	(setq begin (point))
	(if (< begin end)
	    (if (and (not (bobp)) (= (char-after (1- begin)) ??))
		(forward-char)
		(if (condition-case () (progn (forward-sexp) (<= (point) end))
		      (error nil))
		    (progn		;After string
		      (skip-chars-forward " \t")
		      (if (or (= begin point) (= point (point)))
			  (setq done (list begin (point) nil))
			  (if (and (< begin point) (< point (point)))
			      (setq done (list begin (point) t)))))
		    ;; In string at end of buffer
		    (setq done (list begin end t))))))
      done)))

;;;%Indentation
(defun indent-line-ilisp (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one.  This is restricted to the current buffer input."
  (interactive "P")
  (save-restriction
    (if (memq major-mode ilisp-modes)
	(narrow-to-region (save-excursion (lisp-input-start)) (point-max)))
    (lisp-indent-line whole-exp)))

;;;
(defun indent-sexp-ilisp ()
  "Indent each line of the list starting just after point."
  (interactive)
  (save-restriction
    (if (memq major-mode ilisp-modes)
	(narrow-to-region (save-excursion (lisp-input-start)) (point-max)))
    (indent-sexp)))

;;;%Unbalanced parentheses
(defun lisp-skip (end)
  "Skip past whitespace, comments, backslashed characters and strings
in the current buffer as long as you are before END.  This does move
the point."
  (if (< (point) end)
      (let ((comment (and comment-start (string-to-char comment-start)))
	    (done nil)
	    char)
	(while (and (< (point) end)
		    (not done))
	  (skip-chars-forward "\n\t " end)
	  (setq char (char-after (point)))
	  (cond ((eq char ?\")
		 (forward-sexp))
		((eq char comment)
		 (forward-char)
		 (skip-chars-forward "^\n" end))
		((eq char ?\\)
		 (forward-char 2))
		(t (setq done t)))))))

;;;
(defun lisp-count-pairs (begin end left-delimiter right-delimiter)
  "Return the number of top-level pairs of LEFT-DELIMITER and
RIGHT-DELIMITER between BEGIN and END.  If they don't match, the point
will be placed on the offending entry."
  (let ((old-point (point))
	(sexp 0)
	left)
    (goto-char begin)
    (lisp-skip end)
    (while (< (point) end)
      (let ((char (char-after (point))))
	(cond ((or (eq char left-delimiter)
		   ;; For things other than lists
		   (eq (char-after (1- (point))) ?\n))
	       (setq sexp (1+ sexp))
	       (if (condition-case ()
		       (progn (forward-sexp) nil)
		     (error t))
		   (error "Extra %s" (char-to-string left-delimiter))))
	      ((eq char right-delimiter)
	       (error "Extra %s" (char-to-string right-delimiter)))
	      ((< (point) end) (forward-char))))
      (lisp-skip end))
    (goto-char old-point)
    sexp))

;;;
(defun find-unbalanced-region-lisp (start end)
  "Go to the point in region where LEFT-DELIMITER and RIGHT-DELIMITER
become unbalanced.  Point will be on the offending delimiter."
  (interactive "r")
  (lisp-count-pairs start end
		    (string-to-char left-delimiter)
		    (string-to-char right-delimiter))
  (if (not ilisp-complete) (progn (beep) (message "Delimiters balance"))))

;;;
(defun find-unbalanced-lisp (arg)
  "Go to the point in buffer where LEFT-DELIMITER and RIGHT-DELIMITER
become unbalanced.  Point will be on the offending delimiter.  If
called with a prefix, use the current region."
  (interactive "P")
  (if arg
      (call-interactively 'find-unbalanced-region-lisp)
      (find-unbalanced-region-lisp (point-min) (point-max))))

;;;%Superbrackets
(defun close-all-lisp (arg)
  "Unless you are in a string, insert right parentheses as necessary
to balance unmatched left parentheses back to the start of the current
defun or to a previous left bracket which is then replaced with a left
parentheses.  If there are too many right parentheses, remove them
unless there is text after the extra right parentheses.  If called
with a prefix, the entire expression will be closed and all open left
brackets will be replaced with left parentheses."
  (interactive "P")
  (let* ((point (point))
	 (begin (lisp-defun-begin))
	 (end (lisp-end-defun-text))
	 inserted
	 (closed nil))
    (goto-char point)
    (if (or (car (cdr (cdr (lisp-in-string begin end))))
	    (save-excursion (beginning-of-line)
			    (looking-at "[ \t]*;")))
	(insert "]")
	(if (= begin end)
	    (error "No sexp to close.")
	    (save-restriction
	      (narrow-to-region begin end)
	      (if (< point begin) 
		  (setq point begin)
		  (if (> point end)
		      (setq point end)))
	      ;; Add parens at point until either the defun is closed, or we
	      ;; hit a square bracket.
	      (goto-char point)
	      (insert ?\))		;So we have an sexp
	      (while (progn
		       (setq inserted (point))
		       (condition-case () 
			   (progn (backward-sexp)
				  (or arg 
				      (not (eq (char-after (point)) ?\[))))
			 (error (setq closed t) nil)))
		;; With an arg replace all left brackets
		(if (and arg (= (char-after (point)) ?\[))
		    (progn
		      (delete-char 1)
		      (insert ?\()
		      (backward-char)))
		(forward-sexp)
		(insert ?\)))
	      (if (< (point) point)
		  ;; We are at a left bracket
		  (let ((left (point)))
		    (delete-char 1)
		    (insert ?\()
		    (backward-char)
		    (forward-sexp))
		  ;; There was not an open left bracket so close at end
		  (delete-region point inserted)
		  (goto-char begin)
		  (if (condition-case () (progn
					   (forward-sexp)
					   (<= (point) end))
			(error nil))
		      ;; Delete extra right parens
		      (let ((point (point)))
			(skip-chars-forward " \t)\n")
			(if (or (bolp) (eobp))
			    (progn
			      (skip-chars-backward " \t\n")
			      (delete-region point (point)))
			    (error
			     "There is text after the last right parentheses.")))
		      ;; Insert parens at end changing any left brackets
		      (goto-char end)
		      (while 
			  (progn
			    (insert ?\))
			    (save-excursion
			      (condition-case ()
				  (progn (backward-sexp)
					 (if (= (char-after (point)) ?\[)
					     (progn
					       (delete-char 1)
					       (insert ?\()
					       (backward-char)))
					 (> (point) begin))
				(error (delete-backward-char 1)
				       nil))))))))))))

;;;%Reindentation
(defvar lisp-fill-marker (make-marker)
  "Keeps track of point so that it does not move during a reindent-lisp.")

;;;
(defun reindent-lisp ()
  "If in a comment, indent the comment paragraph bounded by
non-comments, blank lines or empty comment lines.  If in a string,
indent the paragraph bounded by string delimiters or blank lines.
Otherwise go to the containing defun, close it and reindent the code
block."
  (interactive)
  (let ((region (lisp-in-string))
	(comment (concat "[ \t]*" comment-start "+[ \t]*")))
    (set-marker lisp-fill-marker (point))
    (back-to-indentation)
    (cond (region
	   (or (= (char-after (point)) ?\")
	       (and (< (point) (car region)) (goto-char (car region)))
	       (re-search-backward "^$" (car region) 'end))
	   (let ((begin (point))
		 (end (car (cdr region)))
		 (fill-prefix nil))
	     (forward-char)
	     (re-search-forward "^$" end 'end)
	     (if (= (point) end)
		 (progn (skip-chars-forward "^\n")
			(if (not (eobp)) (forward-char))))
	     (fill-region-as-paragraph begin (point))))
	  ((looking-at comment)
	   (let ((fill-prefix
		  (buffer-substring
		   (progn (beginning-of-line) (point))
		   (match-end 0))))
	     (while (and (not (bobp)) (lisp-in-comment comment))
	       (forward-line -1))
	     (if (not (bobp)) (forward-line 1))
	     (let ((begin (point)))
	       (while (and (lisp-in-comment comment) (not (eobp)))
		 (replace-match fill-prefix)
		 (forward-line 1))
	       (if (not (eobp))
		   (progn (forward-line -1)
			  (end-of-line)
			  (forward-char 1)))
	       (fill-region-as-paragraph begin (point)))))
	  (t
	   (goto-char lisp-fill-marker)
	   (close-all-lisp 1)
	   (lisp-defun-begin)
	   (indent-sexp-ilisp)))
  (goto-char lisp-fill-marker)
  (set-marker lisp-fill-marker nil)
  (message "Done")))

;;;%Comment region
(defvar ilisp-comment-marker (make-marker)
  "Marker for end of a comment region.")
(defun comment-region-lisp (start end prefix)
  "If prefix is positive, insert prefix copies of comment-start at the
start and comment-end at the end of each line in region.  If prefix is
negative, remove all comment-start and comment-end strings from the
region."
  (interactive "r\np")
  (save-excursion
    (goto-char end)
    (if (and (not (= start end)) (bolp)) (setq end (1- end)))
    (goto-char end)
    (beginning-of-line)
    (set-marker ilisp-comment-marker (point))
    (untabify start end)
    (goto-char start)
    (beginning-of-line)
    (let* ((count 1)
	   (comment comment-start)
	   (comment-end (if (not (equal comment-end "")) comment-end)))
      (if (> prefix 0)
	  (progn
	    (while (< count prefix)
	      (setq comment (concat comment-start comment)
		    count (1+ count)))
	    (while (<= (point) ilisp-comment-marker)
	      (beginning-of-line)
	      (insert comment)
	      (if comment-end (progn (end-of-line) (insert comment-end)))
	      (forward-line 1)))
	  (setq comment (concat comment "+"))
	  (while (<= (point) ilisp-comment-marker)
	    (back-to-indentation)
	    (if (looking-at comment) (replace-match ""))
	    (if comment-end
		(progn
		  (re-search-backward comment-end)
		  (replace-match "")))
	    (forward-line 1)))
      (set-marker ilisp-comment-marker nil))))

;;;%Movement
;;; beginning-of-defun-lisp and end-of-defun-lisp are overloaded by ilisp.el
(defun beginning-of-defun-lisp (&optional stay)
  "Go to the next left paren that starts at the left margin."
  (interactive)
  (beginning-of-defun))

;;;
(defun end-of-defun-lisp ()
  "Go to the next left paren that starts at the left margin."
  (interactive)
  (let ((point (point)))
    (beginning-of-line)
    (re-search-forward "^[ \t\n]*[^; \t\n]" nil t)
    (back-to-indentation)
    (if (not (bolp)) (beginning-of-defun-lisp t))
    (lisp-end-defun-text t)
    (if (= point (point))		;Already at end so move to next end
	(lisp-skip (point-max))
	(if (not (or (eobp)
		     (= (char-after (point)) ?\n)))
	    (lisp-end-defun-text t)))))

;;;%%Reposition-window
(defun count-screen-lines-lisp (start end)
  "Return the number of screen lines between start and end."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (vertical-motion (- (point-max) (point-min))))))

;;;
(defun count-screen-lines-signed-lisp (start end)
  "Return number of screen lines between START and END; returns a negative
number if END precedes START."
  (interactive "r")
  (let ((lines (count-screen-lines-lisp start end)))
    (if (< start end) lines (- lines))))

;;; This was written by Michael D. Ernst
(defun reposition-window-lisp (&optional arg)
  "Make the current definition and/or comment visible, move it to the
top of the window, or toggle the visibility of comments that precede
it.  Leaves point unchanged unless supplied with prefix ARG.  If the
definition is fully onscreen, it is moved to the top of the window.
If it is partly offscreen, the window is scrolled to get the
definition \(or as much as will fit) onscreen, unless point is in a
comment which is also partly offscreen, in which case the scrolling
attempts to get as much of the comment onscreen as possible.
Initially reposition-window attempts to make both the definition and
preceding comments visible.  Further invocations toggle the visibility
of the comment lines.  If ARG is non-nil, point may move in order to
make the whole defun visible \(if only part could otherwise be made
so), to make the defun line visible \(if point is in code and it could
not be made so, or if only comments, including the first comment line,
are visible), or to make the first comment line visible \(if point is
in a comment)."
  (interactive "P")
  (let* ((here (point))
	 ;; change this name once I've gotten rid of references to ht.
	 ;; this is actually the number of the last screen line
	 (ht (- (window-height (selected-window)) 2))
	 (line (count-screen-lines-lisp (window-start) (point)))
	 (comment-height
	  ;; The max deals with the case of cursor between defuns.
	  (max 0
	       (count-screen-lines-signed-lisp
		;; the beginning of the preceding comment
		(save-excursion
		  (if (not (and (bolp) (eq (char-after (point)) ?\()))
		      (beginning-of-defun-lisp))
		  (beginning-of-defun-lisp)
		  (end-of-defun-lisp)
		  ;; Skip whitespace, newlines, and form feeds.
		  (re-search-forward "[^\\s \n\014]")
		  (backward-char 1)
		  (point))
		here)))
	 (defun-height 
	     (count-screen-lines-signed-lisp
	      (save-excursion
	       (end-of-defun-lisp)	;associate comment with next defun 
	       (beginning-of-defun-lisp)
	       (point))
	      here))
	 ;; This must be positive, so don't use the signed version.
	 (defun-depth
	     (count-screen-lines-lisp
	      here
	      (save-excursion (end-of-defun-lisp) (point))))
	 (defun-line-onscreen-p
	     (and (<= defun-height line) (<= (- line defun-height) ht))))
    (cond ((or (= comment-height line)
	       (and (= line ht)
		    (> comment-height line)
		    ;; if defun line offscreen, we should be in case 4
		    defun-line-onscreen-p))
	   ;; Either first comment line is at top of screen or (point at
	   ;; bottom of screen, defun line onscreen, and first comment line
	   ;; off top of screen).  That is, it looks like we just did
	   ;; recenter-definition, trying to fit as much of the comment
	   ;; onscreen as possible.  Put defun line at top of screen; that
	   ;; is, show as much code, and as few comments, as possible.
	   (if (and arg (> defun-depth (1+ ht)))
	       ;; Can't fit whole defun onscreen without moving point.
	       (progn (end-of-defun-lisp) (beginning-of-defun-lisp)
		      (recenter 0))
	       (recenter (max defun-height 0))))
	  ((or (= defun-height line)
	       (= line 0)
	       (and (< line comment-height)
		    (< defun-height 0)))
	   ;; Defun line or cursor at top of screen, OR cursor in comment
	   ;; whose first line is offscreen.
	   ;; Avoid moving definition up even if defun runs offscreen;
	   ;; we care more about getting the comment onscreen.
	   (cond ((= line ht)
		  ;; cursor on last screen line (and so in a comment)
		  (if arg (progn (end-of-defun-lisp) 
				 (beginning-of-defun-lisp)))
		  (recenter 0))
		 ;; This condition, copied from case 4, may not be quite right
		 ((and arg (< ht comment-height))
		  ;; Can't get first comment line onscreen.
		  ;; Go there and try again.
		  (forward-line (- comment-height))
		  (beginning-of-line)
		  ;; was (reposition-window)
		  (recenter 0))
		 (t
		  (recenter (min ht comment-height))))
	   ;; (recenter (min ht comment-height))
	   )
	  ((and (> (+ line defun-depth -1) ht)
		defun-line-onscreen-p)
	   ;; Defun runs off the bottom of the screen and the defun
	   ;; line is onscreen.  Move the defun up.
	   (recenter (max 0 (1+ (- ht defun-depth)) defun-height)))
	  (t
	   ;; If on the bottom line and comment start is offscreen
	   ;; then just move all comments offscreen, or at least as
	   ;; far as they'll go.  Try to get as much of the comments
	   ;; onscreen as possible.
	   (if (and arg (< ht comment-height))
	       ;; Can't get defun line onscreen; go there and try again.
	       (progn (forward-line (- defun-height))
		      (beginning-of-line)
		      (reposition-window-lisp))
	       (recenter (min ht comment-height)))))))

;;;
(defun previous-buffer-lisp (n)
  "Switch to Nth previously selected buffer.  N defaults to the number
of windows plus 1.  That is, no argument switches to the most recently
selected buffer that is not visible.  If N is 1, repeated calls will
cycle through all buffers; -1 cycles the other way.  If N is greater
than 1, the first N buffers on the buffer list are rotated."
  (interactive "P")
  (if (not n)
      (switch-to-buffer nil)
      (let ((buffer-list (buffer-list)))
	(setq n (prefix-numeric-value n))
	(cond ((= n 1)
	       (bury-buffer (current-buffer))
	       (setq n 2))
	      ((< n 0)
	       (setq buffer-list (nreverse buffer-list)
		     n (- n)))
	      (t nil))
	(while (and (> n 1) buffer-list)
	  (setq n (1- n)
		buffer-list (cdr buffer-list))
	  (while (eq (elt (buffer-name (car buffer-list)) 0) ? )
	    (setq buffer-list (cdr buffer-list))))
	(if buffer-list
	    (switch-to-buffer (car buffer-list))
	    (error "There aren't that many buffers")))))

;;;%Bindings
(define-key emacs-lisp-mode-map "\M-q"    'reindent-lisp)
(define-key emacs-lisp-mode-map "\M-\C-a" 'beginning-of-defun-lisp)
(define-key emacs-lisp-mode-map "\M-\C-e" 'end-of-defun-lisp)
(define-key emacs-lisp-mode-map "\C-\M-r" 'reposition-window-lisp)
(define-key emacs-lisp-mode-map "]"       'close-all-lisp)
(define-key lisp-mode-map       "\M-q"    'reindent-lisp)
(define-key lisp-mode-map       "\C-\M-r" 'reposition-window-lisp)
(define-key lisp-mode-map       "]"       'close-all-lisp)
(define-key global-map          "\M-\C-l" 'previous-buffer-lisp)

;;;
(run-hooks 'ilisp-ext-load-hook)
(provide 'ilisp-ext)
