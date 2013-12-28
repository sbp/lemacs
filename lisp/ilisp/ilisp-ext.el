;;; -*-Emacs-Lisp-*-
;;;%Header
;;; Lisp mode extensions from the ILISP package.
;;; Copyright (C) 1990 Chris McConnell, ccm@cs.cmu.edu.

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
	(goto-char (max (point) (progn (end-of-line) (point)))))
    (error (if no-errorp nil (error "Unbalanced parentheses")))))

;;;
(defun lisp-find-next-start ()
  "Find the start of the next line at the left margin and return the
point."
  (if (eobp)
      (point-max)
      (save-excursion
	(forward-char)
	(if (re-search-forward "^[^ \t\n)]" nil t)
	    (match-beginning 0)
	    (point-max)))))

;;;
(defun lisp-end-defun-text (&optional at-start)
  "Go the end of the text associated with the current defun and return
point.  The end is the last character before whitespace leading to
text at the left margin unless it is in a string."
  (if (not at-start) (lisp-defun-begin))
  (let ((point (point))
	(boundary (lisp-find-next-start)))
    (while (progn
	     (skip-chars-forward "^\"" boundary)
	     (if (= (point) boundary)	
		 nil			;No quote found and at limit
		 (if (< (point) boundary)
		     (let ((string-boundary
			    (save-excursion
			      ;; Find paren or ; of next definition
			      (if (re-search-forward "^[(;]" nil t)
				  (match-beginning 0)
				  (point-max)))))
		       (if (condition-case ()
			       (progn (forward-sexp) t)
			     (error (goto-char string-boundary) nil))
			   (if (>= (point) boundary)
			       (if (> (point) string-boundary)
				   (progn
				     (goto-char string-boundary)
				     nil)
				   (progn
				     (setq boundary (lisp-find-next-start))
				     t))
			       t)
			   ;; Unclosed string
			   nil))
		     t))))
    (skip-chars-backward " \t\n")
    (if (< (point) point)
	(goto-char point)
	(if (save-excursion
	      (let ((point (point)))
		(beginning-of-line)
		(search-forward comment-start point t)))
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
that contains point in optional region BEGIN to END."
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
	(if (< (point) end)
	    (if (condition-case () (progn (forward-sexp) (< (point) end))
		  (error nil))
		(progn
		  (skip-chars-forward " \t")
		  (if (and (<= begin point) (<= point (point)))
		      (setq done (list begin (point)))))
		(setq done (list begin end)))))
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
	    (done nil))
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
    (if (lisp-in-string begin end)
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
				  (or arg (= (char-after (point)) ?\()))
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
  (set-marker lisp-fill-marker nil)))

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
    (untabify start end)
    (goto-char start)
    (beginning-of-line)
    (let* ((count 1)
	   (comment comment-start)
	   (comment-end (if (not (equal comment-end "")) comment-end)))
      (set-marker ilisp-comment-marker end)
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

;;;%Other
(defun reposition-window-lisp ()
  "Position the start of the current defun to the top of the window."
  (interactive)
  (save-excursion
    (lisp-defun-begin)
    (recenter 0)))

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
(define-key emacs-lisp-mode-map "\C-\M-r" 'reposition-window-lisp)
(define-key emacs-lisp-mode-map "]"       'close-all-lisp)
(define-key lisp-mode-map       "\M-q"    'reindent-lisp)
(define-key lisp-mode-map       "\C-\M-r" 'reposition-window-lisp)
(define-key lisp-mode-map       "]"       'close-all-lisp)

;;;
(run-hooks 'ilisp-ext-load-hook)
(provide 'ilisp-ext)
