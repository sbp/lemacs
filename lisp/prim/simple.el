;; Basic editing commands for Emacs
;; Copyright (C) 1985-1993 Free Software Foundation, Inc.

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


;;; Changes for zmacs-style active-regions:
;;;
;;; beginning-of-buffer, end-of-buffer, count-lines-region, 
;;; count-lines-buffer, what-line, what-cursor-position, set-goal-column,
;;; set-fill-column, prefix-arg-internal, and line-move (which is used by
;;; next-line and previous-line) set zmacs-region-stays to t, so that they
;;; don't affect the current region-hilighting state.
;;;
;;; mark-whole-buffer, mark-word, exchange-point-and-mark, and
;;; set-mark-command (without an argument) call zmacs-activate-region.
;;;
;;; mark takes an optional arg like the new Fmark_marker() does.  When 
;;; the region is not active, mark returns nil unless the optional arg is true.
;;;
;;; push-mark, pop-mark, exchange-point-and-mark, and set-marker, and
;;; set-mark-command use (mark t) so that they can access the mark whether
;;; the region is active or not.  
;;;
;;; shell-command, shell-command-on-region, yank, and yank-pop (which all
;;; push a mark) have been altered to call exchange-point-and-mark with an
;;; argument, meaning "don't activate the region".  These commands  only use
;;; exchange-point-and-mark to position the newly-pushed mark correctly, so
;;; this isn't a user-visible change.  These functions have also been altered
;;; to use (mark t) for the same reason.

(defun open-line (arg)
  "Insert a newline and leave point before it.  If there is a fill
prefix, inserts the fill prefix after the newline that it inserts.
With arg, inserts that many newlines."
  (interactive "*p")
  (let ((flag (and (bolp) (not (bobp)))))
    (if flag (forward-char -1))
    (while (> arg 0)
      (save-excursion
        (insert ?\n)
;; screw this
;;	(if fill-prefix (insert fill-prefix))
	)
      (setq arg (1- arg)))
    (if flag (forward-char 1))))

(defun split-line ()
  "Split current line, moving portion beyond point vertically down."
  (interactive "*")
  (skip-chars-forward " \t")
  (let ((col (current-column))
	(pos (point)))
    (insert ?\n)
    (indent-to col 0)
    (goto-char pos)))

(defun quoted-insert (arg)
  "Read next input character and insert it.
Useful for inserting control characters.
You may also type up to 3 octal digits, to insert a character with that code"
  (interactive "*p")
  (let ((char (read-quoted-char)))
    (while (> arg 0)
      (insert char)
      (setq arg (1- arg)))))

(defun delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
With argument, join this line to following line."
  (interactive "*P")
  (beginning-of-line)
  (if arg (forward-line 1))
  (if (eq (preceding-char) ?\n)
      (progn
	(delete-region (point) (1- (point)))
	(fixup-whitespace))))

(defun fixup-whitespace ()
  "Fixup white space between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (if (or (looking-at "^\\|\\s)")
	    (save-excursion (forward-char -1)
			    (looking-at "$\\|\\s(\\|\\s'")))
	nil
      (insert ?\ ))))

(defun delete-horizontal-space ()
  "Delete all spaces and tabs around point."
  (interactive "*")
  (skip-chars-backward " \t")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun just-one-space ()
  "Delete all spaces and tabs around point, leaving one space."
  (interactive "*")
  (skip-chars-backward " \t")
  (if (= (following-char) ? )
      (forward-char 1)
    (insert ? ))
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun delete-blank-lines ()
  "On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete all blank lines that follow it."
  (interactive "*")
  (let (thisblank singleblank)
    (save-excursion
      (beginning-of-line)
      (setq thisblank (looking-at "[ \t]*$"))
      (setq singleblank
	    (and thisblank
		 (not (looking-at "[ \t]*\n[ \t]*$"))
		 (or (bobp)
		     (progn (forward-line -1)
			    (not (looking-at "[ \t]*$")))))))
    (if thisblank
	(progn
	  (beginning-of-line)
	  (if singleblank (forward-line 1))
	  (delete-region (point)
			 (if (re-search-backward "[^ \t\n]" nil t)
			     (progn (forward-line 1) (point))
			   (point-min)))))
    (if (not (and thisblank singleblank))
	(save-excursion
	  (end-of-line)
	  (forward-line 1)
	  (delete-region (point)
			 (if (re-search-forward "[^ \t\n]" nil t)
			     (progn (beginning-of-line) (point))
			   (point-max)))))))

(defun back-to-indentation ()
  "Move point to the first non-whitespace character on this line."
  (interactive)
  (beginning-of-line 1)
  (skip-chars-forward " \t"))

(defun newline-and-indent ()
  "Insert a newline, then indent according to major mode.
Indentation is done using the current indent-line-function.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
specified left-margin column."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (insert ?\n)
  (indent-according-to-mode))

(defun reindent-then-newline-and-indent ()
  "Reindent current line, insert newline, then indent the new line.
Indentation of both lines is done according to the current major mode,
which means that the current value of indent-line-function is called.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
specified left-margin column."
  (interactive "*")
  (save-excursion
    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
    (indent-according-to-mode))
  (insert ?\n)
  (indent-according-to-mode))

;; Internal subroutine of delete-char
(defun kill-forward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (+ (point) arg)))

;; Internal subroutine of backward-delete-char
(defun kill-backward-chars (arg)
  (if (listp arg) (setq arg (car arg)))
  (if (eq arg '-) (setq arg -1))
  (kill-region (point) (- (point) arg)))

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete characters backward, changing tabs into spaces.
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1)
and KILLP is t if prefix arg is was specified."
  (interactive "*p\nP")
  (let ((count arg))
    (save-excursion
      (while (and (> count 0) (not (bobp)))
	(if (= (preceding-char) ?\t)
	    (let ((col (current-column)))
	      (forward-char -1)
	      (setq col (- col (current-column)))
	      (insert-char ?\ col)
	      (delete-char 1)))
	(forward-char -1)
	(setq count (1- count)))))
  (delete-backward-char arg killp)
  ;; In overwrite mode, back over columns while clearing them out,
  ;; unless at end of line.
  (and overwrite-mode (not (eolp))
       (save-excursion (insert-char ?\  arg))))

(defun zap-to-char (arg char)
  "Kill up to and including ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap to char: ")
  (kill-region (point) (progn
			 (search-forward (char-to-string char) nil nil arg)
;			 (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
			 (point))))

(defun beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the true beginning.
Don't use this in Lisp programs!
\(goto-char (point-min)) is faster and avoids clobbering the mark."
  (interactive "P")
  (push-mark)
  (goto-char (if arg
		 (if (> (buffer-size) 10000)
		     ;; Avoid overflow for large buffer sizes!
		     (* (prefix-numeric-value arg)
			(/ (buffer-size) 10))
		   (/ (+ 10 (* (buffer-size) (prefix-numeric-value arg))) 10))
	       (point-min)))
  (setq zmacs-region-stays t)
  (if arg (forward-line 1)))

;;; ought to be used elsewhere too, but this is where it annoys me most -- gumby
(defvar dont-redisplay-when-moving-within-window nil)

(defun end-of-buffer (&optional arg)
  "Move point to the end of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the true end.
Don't use this in Lisp programs!
\(goto-char (point-max)) is faster and avoids clobbering the mark."
  (interactive "P")
  (push-mark)
  (setq zmacs-region-stays t)
  (let ((scroll-to-end (not (pos-visible-in-window-p (point-max)))))
    (goto-char (if arg
		   (- (1+ (buffer-size))
		      (if (> (buffer-size) 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ (buffer-size) 10))
			(/ (* (buffer-size) (prefix-numeric-value arg)) 10)))
		 (point-max)))
    (cond (arg (forward-line 1))
	  (scroll-to-end (recenter -3)))))

(defun mark-beginning-of-buffer (&optional arg)
  "Push a mark at the beginning of the buffer; leave point where it is.
With arg N, push mark N/10 of the way from the true beginning."
  (interactive "P")
  (push-mark (if arg
		 (if (> (buffer-size) 10000)
		     ;; Avoid overflow for large buffer sizes!
		     (* (prefix-numeric-value arg)
			(/ (buffer-size) 10))
		   (/ (+ 10 (* (buffer-size) (prefix-numeric-value arg))) 10))
	       (point-min)))
  (zmacs-activate-region))
(fset 'mark-bob 'mark-beginning-of-buffer)

(defun mark-end-of-buffer (&optional arg)
  "Push a mark at the end of the buffer; leave point where it is.
With arg N, push mark N/10 of the way from the true end."
  (interactive "P")
  (push-mark (if arg
		 (- (1+ (buffer-size))
		    (if (> (buffer-size) 10000)
			;; Avoid overflow for large buffer sizes!
			(* (prefix-numeric-value arg)
			   (/ (buffer-size) 10))
		      (/ (* (buffer-size) (prefix-numeric-value arg)) 10)))
	       (point-max)))
  (zmacs-activate-region))
(fset 'mark-eob 'mark-end-of-buffer)

(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer."
  (interactive)
  (push-mark (point))
  (push-mark (point-max))
  (goto-char (point-min))
  (zmacs-activate-region))

(defun eval-current-buffer (&optional printflag)
  "Evaluate the current buffer as Lisp code.
Programs can pass argument PRINTFLAG which controls printing of output:
nil means discard it; anything else is stream for print."
  (interactive)
  (eval-buffer (current-buffer) printflag))

(defun count-words-buffer (b)
  (interactive "b")
  (save-excursion
    (let ((buf (or b (current-buffer))))
      (set-buffer buf)
      (message "Buffer has %d words"
	       (count-words-region (point-min) (point-max))))))

(defun count-words-region (debut fin)
  (interactive "r")
  (save-excursion
    (let ((cnt 0))
      (goto-char debut)
      (while (< (point) fin)
	(if (forward-word 1)
	    (setq cnt (1+ cnt))))
      (message "Region has %d words" cnt)
      cnt)))

(defun count-lines-region (start end)
  "Print number of lines in the region."
  (interactive "r")
  (prog1 (message "Region has %d lines" (count-lines start end))
    (setq zmacs-region-stays t)))

(defun count-lines-buffer (b)
  (interactive "b")
  (prog1
   (save-excursion
    (let ((buf (or b (current-buffer))))
      (set-buffer buf)
      (message "Buffer has %d lines" (count-lines (point-min) (point-max)))))
   (setq zmacs-region-stays t)))

(defun what-line ()
  "Print the current line number (in the buffer) of point."
  (interactive)
  (prog1
   (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (message "Line %d"
	       (1+ (count-lines 1 (point))))))
   (setq zmacs-region-stays t)))

(defun count-lines (start end)
  "Return number of lines between START and END.
This is usually the number of newlines between them,
but will be one more if START is not equal to END
and the greater of them is not at the start of a line."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (eq selective-display t)
	  (let ((done 0))
	    (while (re-search-forward "[\n\C-m]" nil t 40)
	      (setq done (+ 40 done)))
	    (while (re-search-forward "[\n\C-m]" nil t 1)
	      (setq done (+ 1 done)))
	    done)
	(- (buffer-size) (forward-line (buffer-size)))))))

(defun what-cursor-position ()
  "Print info on cursor position (on screen and within buffer)."
  (interactive)
  (prog1
   (let* ((char (following-char))
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format " Hscroll=%d" (window-hscroll))))
	 (col (current-column)))
    (if (= pos end)
	(if (or (/= beg 1) (/= end (1+ total)))
	    (message "point=%d of %d(%d%%) <%d - %d>  column %d %s"
		     pos total percent beg end col hscroll)
	  (message "point=%d of %d(%d%%)  column %d %s"
		   pos total percent col hscroll))
      (if (or (/= beg 1) (/= end (1+ total)))
	  (message "Char: %s (0%o)  point=%d of %d(%d%%) <%d - %d>  column %d %s"
		   (single-key-description char) char pos total percent beg end col hscroll)
	(message "Char: %s (0%o)  point=%d of %d(%d%%)  column %d %s"
		 (single-key-description char) char pos total percent col hscroll))))
   (setq zmacs-region-stays t)))

(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables))

(put 'eval-expression 'disabled t)

;; We define this, rather than making  eval  interactive,
;; for the sake of completion of names like eval-region, eval-current-buffer.
(defun eval-expression (expression)
  "Evaluate EXPRESSION and print value in minibuffer.
Value is also consed on to front of the variable `values'."
  (interactive (list (read-from-minibuffer "Eval: "
					   nil
                                           read-expression-map t
					   'minibuffer-sexp-history)))
  (setq values (cons (eval expression) values))
  (prin1 (car values) t))

(defun edit-and-eval-command (prompt command)
  "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
  (let ((command (read-from-minibuffer prompt
				       (prin1-to-string command)
				       read-expression-map t
				       'minibuffer-sexp-history)))
    ;; Add edited command to command history, unless redundant.
    (or (equal command (car command-history))
	(setq command-history (cons command command-history)))
    (eval command)))

(defun repeat-complex-command (arg)
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
You can use the minibuffer history commands \\<minibuffer-local-map>\\[next-history-element] and \\[previous-history-element]
to get different commands to edit and resubmit."
  (interactive "p")
  (let ((elt (nth (1- arg) command-history))
	(minibuffer-history-position arg)
	(minibuffer-history-sexp-flag t)
	newcmd)
    (if elt
	(progn
	  (setq newcmd (read-from-minibuffer "Redo: " 
			(condition-case ()
			    (let ((print-readably t)) (prin1-to-string elt))
			  (error (prin1-to-string elt)))
			read-expression-map t (cons 'command-history arg)))
	  ;; If command was added to command-history as a string,
	  ;; get rid of that.  We want only evallable expressions there.
	  (if (stringp (car command-history))
	      (setq command-history (cdr command-history)))
	  ;; If command to be redone does not match front of history,
	  ;; add it to the history.
	  (or (equal newcmd (car command-history))
	      (setq command-history (cons newcmd command-history)))
	  (eval newcmd))
      (ding))))


(defun goto-line (arg)
  "Goto line ARG, counting from line 1 at beginning of buffer."
  (interactive "NGoto line: ")
  (save-restriction
    (widen)
    (goto-char 1)
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- arg))
    (forward-line (1- arg)))))

;Put this on C-x u, so we can force that rather than C-_ into startup msg
(fset 'advertised-undo 'undo)

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p)))
    (message "Undo!")
    (or (eq last-command 'undo)
	(progn (undo-start)
	       (undo-more 1)))
    (setq this-command 'undo)
    (undo-more (or arg 1))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary))))

(defun undo-start ()
  "Move undo-pointer to front of undo records.
The next call to undo-more will undo the most recently made change."
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (setq pending-undo-list buffer-undo-list))

(defun undo-more (count)
  "Undo back N undo-boundaries beyond what was already undone recently.
Call undo-start to get ready to undo recent changes,
then call undo-more one or more times to undo them."
  (or pending-undo-list
      (error "No further undo information"))
  (setq pending-undo-list (primitive-undo count pending-undo-list)))

(defvar last-shell-command "")
(defvar last-shell-command-on-region "")

(defun shell-command (command &optional flag)
  "Execute string COMMAND in inferior shell; display output, if any.
If COMMAND ends in ampersand, execute it asynchronously.
 
Optional second arg non-nil (prefix arg, if interactive)
means insert output in current buffer after point (leave mark after it).
This cannot be done asynchronously."
  (interactive (list (read-shell-command "Shell command: " last-shell-command)
		     current-prefix-arg))
  (if flag
      (progn (barf-if-buffer-read-only)
	     (push-mark)
	     (call-process shell-file-name nil t nil
			   "-c" command)
	     (exchange-point-and-mark t))
    ;; Preserve the match data in case called from a program.
    (let ((data (match-data)))
      (unwind-protect
	  (if (string-match "[ \t]*&[ \t]*$" command)
	      ;; Command ending with ampersand means asynchronous.
	      (progn
 		(require 'background) ; whizzy comint background code
 		(background (substring command 0 (match-beginning 0))))
	    (shell-command-on-region (point) (point) command nil))
	(store-match-data data)))))

(defun shell-command-on-region (start end command &optional flag interactive)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.
Noninteractive args are START, END, COMMAND, FLAG.
Noninteractively FLAG means insert output in place of text from START to END,
and put point at the end, but don't alter the mark.

If the output is one line, it is displayed in the echo area,
but it is nonetheless available in buffer `*Shell Command Output*'
even though that buffer is not automatically displayed.  If there is no output
or output is inserted in the current buffer then `*Shell Command Output*' is
deleted." 
  (interactive (list (min (point) (mark)) (max (point) (mark))
		     (read-shell-command "Shell command on region: "
					 last-shell-command-on-region)
		     current-prefix-arg
		     (prefix-numeric-value current-prefix-arg)))
  (if flag
      ;; Replace specified region with output from command.
      (let ((swap (and interactive (< (point) (mark)))))
	;; Don't muck with mark
	;; unless called interactively.
	(and interactive (push-mark))
	(call-process-region start end shell-file-name t t nil
			     "-c" command)
	(if (get-buffer "*Shell Command Output*")
	    (kill-buffer "*Shell Command Output*"))
	(and interactive swap (exchange-point-and-mark t)))
    (let ((buffer (get-buffer-create "*Shell Command Output*")))
      (save-excursion
	(set-buffer buffer)
	(erase-buffer))
      (if (eq buffer (current-buffer))
	  (setq start 1 end 1))
      (call-process-region start end shell-file-name
			   nil buffer nil
			   "-c" command)
      (let ((lines (save-excursion
		     (set-buffer buffer)
		     (if (= (buffer-size) 0)
			 0
		       (count-lines (point-min) (point-max))))))
	(cond ((= lines 0)
	       (message "(Shell command completed with no output)")
	       (kill-buffer "*Shell Command Output*"))
	      ((= lines 1)
	       (message "%s"
			(save-excursion
			  (set-buffer buffer)
			  (goto-char (point-min))
			  (buffer-substring (point)
					    (progn (end-of-line) (point))))))
	      (t 
	       (set-window-start (display-buffer buffer) 1)))))))

(defun universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following this command make up the numeric argument.
If no digits or minus sign follow, this command by itself provides 4 as argument.
Used more than once, this command multiplies the argument by 4 each time."
  (interactive nil)
  (let* ((c-u 4)
	 (start-char last-command-event)
	 (event (allocate-event)))
    (while (equal start-char (next-command-event event))
      (setq c-u (* 4 c-u)))
    (prefix-arg-internal event c-u nil)))

(defun prefix-arg-internal (event c-u value)
  (setq zmacs-region-stays t)
  (let ((sign 1)
	char)
    (if (key-press-event-p event) (setq char (event-key event)))
    (if (and (numberp value) (< value 0))
	(setq sign -1 value (- value)))
    (if (eq value '-)
	(setq sign -1 value nil))
    (while (eq ?- char)
      (setq sign (- sign) c-u nil)
      (next-command-event event)
      (if (key-press-event-p event) (setq char (event-key event))))
    (while (and (numberp char) (>= char ?0) (<= char ?9))
      (setq value (+ (* (if (numberp value) value 0) 10) (- char ?0)) c-u nil)
      (next-command-event event)
      (if (key-press-event-p event) (setq char (event-key event))))
    (setq prefix-arg
	  (cond (c-u (list c-u))
		((numberp value) (* value sign))
		((= sign -1) '-)))
    (setq unread-command-event event)))

(defun digit-argument (arg)
  "Part of the numeric argument for the next command."
  (interactive "P")
  (prefix-arg-internal last-command-event nil arg))

(defun negative-argument (arg)
  "Begin a negative numeric argument for the next command."
  (interactive "P")
  (prefix-arg-internal (character-to-event ?- (allocate-event)) nil arg))

(defun forward-to-indentation (arg)
  "Move forward ARG lines and position at first nonblank character."
  (interactive "p")
  (forward-line arg)
  (skip-chars-forward " \t"))

(defun backward-to-indentation (arg)
  "Move backward ARG lines and position at first nonblank character."
  (interactive "p")
  (forward-line (- arg))
  (skip-chars-forward " \t"))

(defun kill-line (&optional arg)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg."
  (interactive "*P")
  (kill-region (point)
	       (progn
		 (if arg
		     (forward-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (looking-at "[ \t]*$")
		       (forward-line 1)
		     (end-of-line)))
		 (point))))

;;;; The kill ring

(defvar kill-ring nil
  "List of killed text sequences.")

(defconst kill-ring-max 30
  "*Maximum length of kill ring before oldest elements are thrown away.")

(defvar kill-ring-yank-pointer nil
  "The tail of the kill ring whose car is the last thing yanked.")

(defun kill-append (string before-p)
  (setcar kill-ring
	  (if before-p
	      (concat string (car kill-ring))
	      (concat (car kill-ring) string))))

(defun kill-region (beg end &optional verbose)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[kill-ring-save].)

This is the primitive for programs to kill text (as opposed to deleting it).
Supply two arguments, character numbers indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring."
  (interactive "*r\np")
;  (interactive
;   (let ((region-hack (and zmacs-regions (eq last-command 'yank))))
;     ;; This lets "^Y^W" work.  I think this is dumb, but zwei did it.
;     (if region-hack (zmacs-activate-region))
;     (prog1
;	 (list (point) (mark) current-prefix-arg)
;       (if region-hack (zmacs-deactivate-region)))))
  (or (and beg end) (error (if zmacs-regions
			       "The region is not active now"
			     "The mark is not set now")))
  (if verbose (message "Killing %d characters"
		       (- (max beg end) (min beg end))))
  (copy-region-as-kill beg end)
  (delete-region beg end))

(defvar kill-hooks nil
  "If non-nil, this should be a function or functions of one argument which
are called with the string most recently added to the kill ring.  You can use
this to, for example, make the most recent kill become the X Clipboard
selection.")

(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (setq kill-ring (cons (buffer-substring beg end) kill-ring))
    (if (> (length kill-ring) kill-ring-max)
	(setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq kill-ring-yank-pointer kill-ring) ; do now, as kill-hooks may error
  (let ((rest kill-hooks))
    (if (or (not (consp rest)) (eq (car-safe rest) 'lambda))
	(setq rest (list rest)))
    (while rest
      (funcall (car rest) (car kill-ring))
      (setq rest (cdr rest))))
  (setq this-command 'kill-region)
  nil)

(defun kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it."
  (interactive "r")
  (copy-region-as-kill beg end) ; copy before delay, for xclipboard's benefit
  (save-excursion
    (goto-char (if (= (point) end) beg end))
    (if (pos-visible-in-window-p (point))
	(sit-for 0))))

(defun append-next-kill ()
  "Cause following command, if kill, to append to previous kill."
  (interactive)
  (if (interactive-p)
      (progn
	(setq this-command 'kill-region)
	(message "If the next command is a kill, it will append"))
    (setq last-command 'kill-region))
  (setq zmacs-region-stays t))

(defun rotate-yank-pointer (arg)
  "Rotate the yanking point in the kill ring."
  (interactive "p")
  (let ((length (length kill-ring)))
    (if (zerop length)
	(error "Kill ring is empty")
      (setq kill-ring-yank-pointer
	    (nthcdr (% (+ arg (- length (length kill-ring-yank-pointer)))
		       length)
		    kill-ring)))))

(defun yank-pop (arg)
  "Replace just-yanked stretch of killed-text with a different stretch.
This command is allowed only immediately after a  yank  or a  yank-pop.
At such a time, the region contains a stretch of reinserted
previously-killed text.  yank-pop  deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument n, the n'th previous kill is inserted.
If n is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (let ((before (< (point) (mark t))))
    (delete-region (point) (mark t))
    (rotate-yank-pointer arg)
    (set-mark (point))
    (insert (car kill-ring-yank-pointer))
    (if before (exchange-point-and-mark t))))

(defun yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.
With just C-U as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  (interactive "*P")
  (rotate-yank-pointer (if (listp arg) 0
			 (if (eq arg '-) -1
			   (1- arg))))
  (push-mark (point))
  (insert (car kill-ring-yank-pointer))
  (if (consp arg)
      (exchange-point-and-mark t)))

(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  (interactive (list (read-buffer "Insert buffer: " (other-buffer) t)))
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
	(set-buffer buffer)
	(setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark)))

(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
a buffer or the name of one, and two character numbers
specifying the portion of the current buffer to be copied."
  (interactive "BAppend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))

(defun prepend-to-buffer (buffer start end)
  "Prepend to specified buffer the text of the region.
It is inserted into that buffer after its point.

When calling from a program, give three arguments:
a buffer or the name of one, and two character numbers
specifying the portion of the current buffer to be copied."
  (interactive "BPrepend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(defun copy-to-buffer (buffer start end)
  "Copy to specified buffer the text of the region.
It is inserted into that buffer, replacing existing text there.

When calling from a program, give three arguments:
a buffer or the name of one, and two character numbers
specifying the portion of the current buffer to be copied."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (erase-buffer)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

(defun mark (&optional inactive-p)
  "Return this buffer's mark value as integer, or nil if no mark.

If `zmacs-regions' is true, then this returns nil unless the region is
currently in the active (hilighted) state.  With an argument of t, this
returns the mark (if there is one) regardless of the active-region state.
You should *generally* not use the mark unless the region is active, if
the user has expressed a preference for the active-region model.

If you are using this in an editing command, you are most likely making
a mistake\; see the documentation of `set-mark'."
  (let ((m (mark-marker inactive-p)))
    (and m (marker-position m))))

(defun set-mark (pos)
  "Set this buffer's mark to POS.  Don't use this function!
That is to say, don't use this function unless you want
the user to see that the mark has moved, and you want the previous
mark position to be lost.

Normally, when a new mark is set, the old one should go on the stack.
This is why most applications should use push-mark, not set-mark.

Novice emacs-lisp programmers often try to use the mark for the wrong
purposes.  The mark saves a location for the user's convenience.
Most editing commands should not alter the mark.
To remember a location for internal use in the Lisp program,
store it in a Lisp variable.  Example:

   (let ((beg (point))) (forward-line 1) (delete-region beg (point)))."

  (set-marker (mark-marker t) pos (current-buffer)))

(defvar mark-ring nil
  "The list of saved former marks of the current buffer,
most recent first.")
(make-variable-buffer-local 'mark-ring)

(defconst mark-ring-max 16
  "*Maximum size of mark ring.  Start discarding off end if gets this big.")

(defun set-mark-command (arg)
  "Set mark at where point is, or jump to mark.
With no prefix argument, set mark, and push previous mark on mark ring.
With argument, jump to mark, and pop into mark off the mark ring.

Novice emacs-lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (interactive "P")
  (if (null arg)
      (progn
	(push-mark)
	(zmacs-activate-region))
    (if (null (mark t))
	(error "No mark set in this buffer")
      (goto-char (mark t))
      (pop-mark))))

(defun push-mark (&optional location nomsg)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
Displays \"Mark set\" unless the optional second arg NOMSG is non-nil.

Novice emacs-lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (if (null (mark t))
      nil
    (setq mark-ring (cons (copy-marker (mark-marker t)) mark-ring))
    (if (> (length mark-ring) mark-ring-max)
	(progn
	  (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
	  (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil))))
  (set-mark (or location (point)))
  (or nomsg executing-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  nil)

(defun pop-mark ()
  "Pop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (if mark-ring
      (progn
	(setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker t)))))
	(set-mark (car mark-ring))  ; do not activate region
	(move-marker (car mark-ring) nil)
	(if (null (mark t)) (ding))  ; how can this happen?
	(setq mark-ring (cdr mark-ring)))))

(fset 'exchange-dot-and-mark 'exchange-point-and-mark)
(defun exchange-point-and-mark (&optional dont-activate-region)
  "Put the mark where point is now, and point where the mark is now."
  (interactive nil)
  (let ((omark (mark t)))
    (if (null omark)
	(error "No mark set in this buffer"))
    (set-mark (point))
    (goto-char omark)
    (or dont-activate-region (zmacs-activate-region))
    nil))

(defvar next-line-extends-end-of-buffer t
  "*If non-nil, when \\[next-line] is invoked on the last line of a buffer,
a newline character is inserted to create a new line.
If nil, \\[next-line] signals an `end-of-buffer' in that situation.")

(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

If the variable `next-line-extends-end-of-buffer' is true and there is no line
in the buffer after this one, a newline character is inserted to create
a line and the cursor moves to that line.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.  This goal column is stored
in `goal-column', which is nil when there is none.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (= arg 1)
      (let ((opoint (point)))
	(forward-line 1)
	(if (or (= opoint (point))
		(not (eq (preceding-char) ?\n)))
	    (progn
	      (if (not next-line-extends-end-of-buffer)
		  (signal 'end-of-buffer nil))
	      (insert ?\n)
	      (setq zmacs-region-stays t))
	  (goto-char opoint)
	  (line-move arg)))
    (line-move arg))
  nil)

(defun previous-line (arg)
  "Move cursor vertically up ARG lines.
If there is no character in the target line exactly over the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider using
`forward-line' with negative argument instead..  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (line-move (- arg))
  nil)

(defconst track-eol nil
  "*Non-nil means vertical motion starting at end of line keeps to ends of lines.
This means moving to the end of each line moved onto.")

(make-variable-buffer-local
 (defvar goal-column nil
   "*Semipermanent goal column for vertical motion, as set by \\[set-goal-column], or nil."))

(defvar temporary-goal-column 0
  "Current goal column for vertical motion.
It is the column where point was at the start of current run of vertical motion commands.")

(defun line-move (arg)
  (if (not (or (eq last-command 'next-line)
	       (eq last-command 'previous-line)))
      (setq temporary-goal-column
	    (if (and track-eol (eolp))
		9999
	      (current-column))))
  (if (not (integerp selective-display))
      (forward-line arg)
    ;; Move by arg lines, but ignore invisible ones.
    (while (> arg 0)
      (vertical-motion 1)
      (forward-char -1)
      (forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (vertical-motion -1)
      (beginning-of-line)
      (setq arg (1+ arg))))
  (move-to-column (or goal-column temporary-goal-column))
  (setq zmacs-region-stays t)
  nil)


(defun set-goal-column (arg)
  "Set the current horizontal position as a goal for \\[next-line] and \\[previous-line].
Those commands will move to this position in the line moved to
rather than trying to keep the same horizontal position.
With a non-nil argument, clears out the goal column
so that \\[next-line] and \\[previous-line] resume vertical motion."
  (interactive "P")
  (if arg
      (progn
        (setq goal-column nil)
        (message "No goal column"))
    (setq goal-column (current-column))
    (message (substitute-command-keys
	      "Goal column %d (use \\[set-goal-column] with an arg to unset it)")
	     goal-column))
  (setq zmacs-region-stays t)
  nil)

(defun transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged."
  (interactive "*P")
  (and (null arg) (eolp) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))

(defun transpose-words (arg)
  "Interchange words around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other words (backward if ARG negative).
If ARG is zero, the words around or after point and around or after mark
are interchanged."
  (interactive "*p")
  (transpose-subr 'forward-word arg))

(defun transpose-sexps (arg)
  "Like \\[transpose-words] but applies to sexps.
Does not work on a sexp that point is in the middle of
if it is a list or string."
  (interactive "*p")
  (transpose-subr 'forward-sexp arg))

(defun transpose-lines (arg)
  "Exchange current line and previous line, leaving point after both.
With argument ARG, takes previous line and moves it past ARG lines.
With argument 0, interchanges line point is in with line mark is in."
  (interactive "*p")
  (transpose-subr (function
		   (lambda (arg)
		     (if (= arg 1)
			 (progn
			   ;; Move forward over a line,
			   ;; but create a newline if none exists yet.
			   (end-of-line)
			   (if (eobp)
			       (newline)
			     (forward-char 1)))
		       (forward-line arg))))
		  arg))

(defun transpose-subr (mover arg)
  (let (start1 end1 start2 end2)
    (if (= arg 0)
	(progn
	  (save-excursion
	    (funcall mover 1)
	    (setq end2 (point))
	    (funcall mover -1)
	    (setq start2 (point))
	    (goto-char (mark t))
	    (funcall mover 1)
	    (setq end1 (point))
	    (funcall mover -1)
	    (setq start1 (point))
	    (transpose-subr-1))
	  (exchange-point-and-mark t)))
    (while (> arg 0)
      (funcall mover -1)
      (setq start1 (point))
      (funcall mover 1)
      (setq end1 (point))
      (funcall mover 1)
      (setq end2 (point))
      (funcall mover -1)
      (setq start2 (point))
      (transpose-subr-1)
      (goto-char end2)
      (setq arg (1- arg)))
    (while (< arg 0)
      (funcall mover -1)
      (setq start2 (point))
      (funcall mover -1)
      (setq start1 (point))
      (funcall mover 1)
      (setq end1 (point))
      (funcall mover 1)
      (setq end2 (point))
      (transpose-subr-1)
      (setq arg (1+ arg)))))

(defun transpose-subr-1 ()
  (if (> (min end1 end2) (max start1 start2))
      (error "Don't have two things to transpose"))
  (let ((word1 (buffer-substring start1 end1))
	(word2 (buffer-substring start2 end2)))
    (delete-region start2 end2)
    (goto-char start2)
    (insert word1)
    (goto-char (if (< start1 start2) start1
		 (+ start1 (- (length word1) (length word2)))))
    (delete-char (length word1))
    (insert word2)))

(defconst comment-column 32
  "*Column to indent right-margin comments to.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'comment-column)

(defconst comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax defined.")

(defconst comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair.")

(defconst comment-end ""
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line.")

(defconst comment-indent-hook
  '(lambda () comment-column)
  "Function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defun indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  (interactive "*")
  (beginning-of-line 1)
  (if (null comment-start)
      (error "No comment syntax defined")
    (let* ((eolpos (save-excursion (end-of-line) (point)))
	   cpos indent begpos)
      (if (re-search-forward comment-start-skip eolpos 'move)
	  (progn (setq cpos (point-marker))
		 ;; Find the start of the comment delimiter.
		 ;; If there were paren-pairs in comment-start-skip,
		 ;; position at the end of the first pair.
		 (if (match-end 1)
		     (goto-char (match-end 1))
		   ;; If comment-start-skip matched a string with internal
		   ;; whitespace (not final whitespace) then the delimiter
		   ;; start at the end of that whitespace.
		   ;; Otherwise, it starts at the beginning of what was matched.
		   (skip-chars-backward " \t" (match-beginning 0))
		   (skip-chars-backward "^ \t" (match-beginning 0)))))
      (setq begpos (point))
      ;; Compute desired indent.
      (if (= (current-column)
	     (setq indent (funcall comment-indent-hook)))
	  (goto-char begpos)
	;; If that's different from current, change it.
	(skip-chars-backward " \t")
	(delete-region (point) begpos)
	(indent-to indent))
      ;; An existing comment?
      (if cpos 
	  (progn (goto-char cpos)
		 (set-marker cpos nil))
	;; No, insert one.
	(insert comment-start)
	(save-excursion
	  (insert comment-end))))))

(defun set-comment-column (arg)
  "Set the comment column based on point.
With no arg, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
With any other arg, set comment column to indentation of the previous comment
 and then align or create a comment on this line at that column."
  (interactive "P")
  (if (eq arg '-)
      (kill-comment nil)
    (if arg
	(progn
	  (save-excursion
	    (beginning-of-line)
	    (re-search-backward comment-start-skip)
	    (beginning-of-line)
	    (re-search-forward comment-start-skip)
	    (goto-char (match-beginning 0))
	    (setq comment-column (current-column))
	    (message "Comment column set to %d" comment-column))
	  (indent-for-comment))
      (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))))

(defun kill-comment (arg)
  "Kill the comment on this line, if any.
With argument, kill comments on that many lines starting with this one."
  ;; this function loses in a lot of situations.  it incorrectly recognises
  ;; comment delimiters sometimes (ergo, inside a string), doesn't work
  ;; with multi-line comments, can kill extra whitespace if comment wasn't
  ;; through end-of-line, et cetera.
  (interactive "*P")
  (or comment-start-skip (error "No comment syntax defined"))
  (let ((count (prefix-numeric-value arg)) endc)
    (while (> count 0)
      (save-excursion
	(end-of-line)
	(setq endc (point))
	(beginning-of-line)
	(and (string< "" comment-end)
	     (setq endc
		   (progn
		     (re-search-forward (regexp-quote comment-end) endc 'move)
		     (skip-chars-forward " \t")
		     (point))))
	(beginning-of-line)
	(if (re-search-forward comment-start-skip endc t)
	    (progn
	      (goto-char (match-beginning 0))
	      (skip-chars-backward " \t")
	      (kill-region (point) endc)
	      ;; to catch comments a line beginnings
	      (indent-according-to-mode))))
      (if arg (forward-line 1))
      (setq count (1- count)))))

(defun backward-word (arg)
  "Move backward until encountering the end of a word.
With argument, do this that many times.
In programs, it is faster to call forward-word with negative arg."
  (interactive "p")
  (forward-word (- arg)))

(defun mark-word (arg)
  "Set mark arg words away from point."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-word arg)
      (point)))
  (zmacs-activate-region))

(defun kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (kill-region (point) (progn (forward-word arg) (point))))

(defun backward-kill-word (arg)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (kill-word (- arg)))

(defconst fill-prefix nil
  "*String for filling to insert at front of new line, or nil for none.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'fill-prefix)

(defconst auto-fill-inhibit-regexp nil
  "*Regexp to match lines which should not be auto-filled.")

(defun do-auto-fill ()
  (let (give-up)
    (or (and auto-fill-inhibit-regexp
	     (save-excursion (beginning-of-line)
			     (looking-at auto-fill-inhibit-regexp)))
	(while (and (not give-up) (> (current-column) fill-column))
	  (let ((fill-point
		 (let ((opoint (point)))
		   (save-excursion
		     (move-to-column (1+ fill-column))
		     (skip-chars-backward "^ \t\n")
		     (if (bolp)
			 (re-search-forward "[ \t]" opoint t))
		     (skip-chars-backward " \t")
		     (point)))))
	    ;; If there is a space on the line before fill-point,
	    ;; and nonspaces precede it, break the line there.
	    (if (save-excursion
		  (goto-char fill-point)
		  (not (bolp)))
		;; If point is at the fill-point, do not `save-excursion'.
		;; Otherwise, if a comment prefix or fill-prefix is inserted,
		;; point will end up before it rather than after it.
		(if (save-excursion
		      (skip-chars-backward " \t")
		      (= (point) fill-point))
		    (indent-new-comment-line)
		  (save-excursion
		    (goto-char fill-point)
		    (indent-new-comment-line)))
	      ;; No place to break => stop trying.
	      (setq give-up t)))))))

(defconst comment-multi-line nil
  "*Non-nil means \\[indent-new-comment-line] should continue same comment
on new line, with no new terminator or starter.")

(defun indent-new-comment-line ()
  "Break line at point and indent, continuing comment if presently within one.
The body of the continued comment is indented under the previous comment line."
  (interactive "*")
  (let (comcol comstart)
    (skip-chars-backward " \t")
    (delete-region (point)
		   (progn (skip-chars-forward " \t")
			  (point)))
    (insert ?\n)
    (save-excursion
      (if (and comment-start-skip
	       (let ((opoint (point)))
		 (forward-line -1)
		 (re-search-forward comment-start-skip opoint t)))
	  ;; The old line is a comment.
	  ;; Set WIN to the pos of the comment-start.
	  ;; But if the comment is empty, look at preceding lines
	  ;; to find one that has a nonempty comment.
	  (let ((win (match-beginning 0)))
	    (while (and (eolp) (not (bobp))
			(let (opoint)
			  (beginning-of-line)
			  (setq opoint (point))
			  (forward-line -1)
			  (re-search-forward comment-start-skip opoint t)))
	      (setq win (match-beginning 0)))
	    ;; Indent this line like what we found.
	    (goto-char win)
	    (setq comcol (current-column))
	    (setq comstart (buffer-substring (point) (match-end 0))))))
    (if comcol
	(let ((comment-column comcol)
	      (comment-start comstart)
	      (comment-end comment-end))
	  (and comment-end (not (equal comment-end ""))
	       (if (not comment-multi-line)
		   (progn
		     (forward-char -1)
		     (insert comment-end)
		     (forward-char 1))
		 (setq comment-column (+ comment-column (length comment-start))
		       comment-start "")))
	  (if (not (eolp))
	      (setq comment-end ""))
	  (insert ?\n)
	  (forward-char -1)
	  (indent-for-comment)
	  (save-excursion
	    ;; Make sure we delete the newline inserted above.
	    (end-of-line)
	    (delete-char 1)))
      (if fill-prefix
	  (insert fill-prefix)
	(indent-according-to-mode)))))

(defun auto-fill-mode (&optional arg)
  "Toggle auto-fill mode.
With arg, turn auto-fill mode on if and only if arg is positive.
In auto-fill mode, inserting a space at a column beyond  fill-column
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (setq auto-fill-function
	       (if (if (null arg)
		       (not auto-fill-function)
		       (> (prefix-numeric-value arg) 0))
		   'do-auto-fill
		   nil))
    ;; update mode-line
    (set-buffer-modified-p (buffer-modified-p))))

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun set-fill-column (arg)
  "Set fill-column to current column, or to argument if given.
fill-column's value is separate for each buffer."
  (interactive "P")
  (setq fill-column (if (integerp arg) arg (current-column)))
  (setq zmacs-region-stays t)
  (message "fill-column set to %d" fill-column))

(defun set-selective-display (arg)
  "Set selective-display to ARG; clear it if no arg.
When selective-display is a number > 0,
lines whose indentation is >= selective-display are not displayed.
selective-display's value is separate for each buffer.

WARNING: selective-display does not currently work with Lucid Emacs.
This is a known bug that will be fixed eventually."
  (interactive "P")
  (if (eq selective-display t)
      (error "selective-display already in use for marked lines"))
  (setq selective-display
	(and arg (prefix-numeric-value arg)))
  (set-window-start (selected-window) (window-start (selected-window)))
  (princ "selective-display set to " t)
  (prin1 selective-display t)
  (princ "." t))

(defun overwrite-mode (arg)
  "Toggle overwrite mode.
With arg, turn overwrite mode on iff arg is positive.
In overwrite mode, printing characters typed in replace existing text
on a one-for-one basis, rather than pushing it to the right."
  (interactive "P")
  (setq overwrite-mode
	(if (null arg) (not overwrite-mode)
	  (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p (buffer-modified-p))) ;No-op, but updates mode line.

(defvar blink-matching-paren t
  "*Non-nil means show matching open-paren when close-paren is inserted.")

(defconst blink-matching-paren-distance 4000
  "*If non-nil, is maximum distance to search for matching open-paren
when close-paren is inserted.")

(defun blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (and (> (point) (1+ (point-min)))
       (/= (char-syntax (char-after (- (point) 2))) ?\\ )
       blink-matching-paren
       (let* ((oldpos (point))
	      (parse-sexp-ignore-comments t)
	      (blinkpos)
	      (mismatch))
	 (save-excursion
	   (save-restriction
	     (if blink-matching-paren-distance
		 (narrow-to-region (max (point-min)
					(- (point) blink-matching-paren-distance))
				   oldpos))
	     (condition-case ()
		 (setq blinkpos (scan-sexps oldpos -1))
	       (error nil)))
	   (and blinkpos (/= (char-syntax (char-after blinkpos))
			     ?\$)
		(setq mismatch
		      (/= (char-after (1- oldpos))
			  (logand (lsh (aref (syntax-table)
					     (char-after blinkpos))
				       -8)
				  255))))
	   (if mismatch (setq blinkpos nil))
	   (if blinkpos
	       (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (goto-char blinkpos)
		  (message
		   "Matches %s"
		   (if (save-excursion
			 (skip-chars-backward " \t")
			 (not (bolp)))
		       (buffer-substring (progn (beginning-of-line) (point))
					 (1+ blinkpos))
		     (buffer-substring blinkpos
				       (progn
					(forward-char 1)
					(skip-chars-forward "\n \t")
					(end-of-line)
					(point)))))))
	     (cond (mismatch
		    (message "Mismatched parentheses"))
		   ((not blink-matching-paren-distance)
		    (message "Unmatched parenthesis"))))))))

;Turned off because it makes dbx bomb out.
(setq blink-paren-function 'blink-matching-open)

; this is just something for the luser to see in a keymap -- this is not
;  how quitting works normally!
(defun keyboard-quit ()
  "Signal a `quit' condition."
  (interactive)
  (signal 'quit nil))

;;; right-thing-p?
;(defun keyboard-quit ()
;  "Signal a `quit' condition.
;If this character is typed while lisp code is executing, it will be treated
; as an interrupt.
;If this character is typed at top-level, this simply beeps.
;If `zmacs-regions' is true, and the zmacs region is active, then this
; key deactivates the region without beeping or signalling."
;  (interactive)
;  (if (and zmacs-regions (zmacs-deactivate-region))
;      ;; pseudo-zmacs compatibility: don't beep if this ^G is simply
;      ;; deactivating the region.  If it is inactive, beep.
;      nil
;    (signal 'quit nil)))


(defun set-variable (var val)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
When using this interactively, supply a Lisp expression for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes."
  (interactive
   (let* ((var (read-variable "Set variable: "))
	  (minibuffer-help-form
	   '(funcall myhelp))
	  (myhelp
	   (function
	    (lambda ()
	      (with-output-to-temp-buffer "*Help*"
		(prin1 var)
		(princ "\nDocumentation:\n")
		(princ (substring (documentation-property var 'variable-documentation)
				  1))
		(if (boundp var)
		    (let ((print-length 20))
		      (princ "\n\nCurrent value: ")
		      (prin1 (symbol-value var))))
		nil)))))
     (list var
	   (eval-minibuffer (format "Set %s to value: " var)))))
  (set var val))
