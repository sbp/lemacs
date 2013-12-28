;;; simple.el --- basic editing commands for Emacs

;; Copyright (C) 1985, 1986, 1987, 1993 Free Software Foundation, Inc.

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

;; A grab-bag of basic Emacs commands not specifically related to some
;; major mode or to file-handling.

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

;;; Code:

(defun open-line (arg)
  "Insert a newline and leave point before it.
With arg N, insert N newlines."
;;   "Insert a newline and leave point before it.
;; If there is a fill prefix, insert the fill prefix on the new line
;; if the line would have been empty.
;; With arg N, insert N newlines."
  (interactive "*p")
  (let* (;;(do-fill-prefix (and fill-prefix (bolp)))
	 (do-fill-prefix nil)  ;; screw this -- says JWZ
	 (flag (and (null do-fill-prefix) (bolp) (not (bobp)))))
    ;; If this is a simple case, and we are at the beginning of a line,
    ;; actually insert the newline *before* the preceding newline
    ;; instead of after.  That makes better display behavior.
    (if flag 
	(progn
	  ;; If undo is enabled, don't let this hack be visible:
	  ;; record the real value of point as the place to move back to
	  ;; if we undo this insert.
	  (if (and buffer-undo-list (not (eq buffer-undo-list t)))
	      (setq buffer-undo-list (cons (point) buffer-undo-list)))
	  (forward-char -1)))
    (while (> arg 0)
      (save-excursion
        (insert ?\n))
      (if do-fill-prefix (insert fill-prefix))
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
This is useful for inserting control characters.
You may also type up to 3 octal digits, to insert a character with that code.

In overwrite mode, this function inserts the character anyway, and
does not handle octal digits specially.  This means that if you use
overwrite as your normal editing mode, you can use this function to
insert characters when necessary.

In binary overwrite mode, this function does overwrite, and octal
digits are interpreted as a character code.  This is supposed to make
this function useful in editing binary files."
  (interactive "*p")
  (let ((char (if (or (not overwrite-mode)
		      (eq overwrite-mode 'overwrite-mode-binary))
		  (read-quoted-char)
		  (read-char))))
    (if (eq overwrite-mode 'overwrite-mode-binary)
	(delete-char arg))
    (insert-char char arg)))

(defun delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this line.
With argument, join this line to following line."
  (interactive "*P")
  (beginning-of-line)
  (if arg (forward-line 1))
  (if (eq (preceding-char) ?\n)
      (progn
	(delete-region (point) (1- (point)))
	;; If the second line started with the fill prefix,
	;; delete the prefix.
	(if (and fill-prefix
		 (<= (+ (point) (length fill-prefix)) (point-max))
		 (string= fill-prefix
			  (buffer-substring (point)
					    (+ (point) (length fill-prefix)))))
	    (delete-region (point) (+ (point) (length fill-prefix))))
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
      ;; Set singleblank if there is just one blank line here.
      (setq singleblank
	    (and thisblank
		 (not (looking-at "[ \t]*\n[ \t]*$"))
		 (or (bobp)
		     (progn (forward-line -1)
			    (not (looking-at "[ \t]*$")))))))
    ;; Delete preceding blank lines, and this one too if it's the only one.
    (if thisblank
	(progn
	  (beginning-of-line)
	  (if singleblank (forward-line 1))
	  (delete-region (point)
			 (if (re-search-backward "[^ \t\n]" nil t)
			     (progn (forward-line 1) (point))
			   (point-min)))))
    ;; Delete following blank lines, unless the current line is blank
    ;; and there are no following blank lines.
    (if (not (and thisblank singleblank))
	(save-excursion
	  (end-of-line)
	  (forward-line 1)
	  (delete-region (point)
			 (if (re-search-forward "[^ \t\n]" nil t)
			     (progn (beginning-of-line) (point))
			   (point-max)))))
    ;; Handle the special case where point is followed by newline and eob.
    ;; Delete the line, leaving point at eob.
    (if (looking-at "^[ \t]*\n\\'")
	(delete-region (point) (point-max)))))

(defun back-to-indentation ()
  "Move point to the first non-whitespace character on this line."
  (interactive)
  (beginning-of-line 1)
  (skip-chars-forward " \t"))

(defun newline-and-indent ()
  "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the variable `left-margin'."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (newline)
  (indent-according-to-mode))

(defun reindent-then-newline-and-indent ()
  "Reindent current line, insert newline, then indent the new line.
Indentation of both lines is done according to the current major mode,
which means calling the current value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this indents to the
column specified by the variable `left-margin'."
  (interactive "*")
  (save-excursion
    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
    (indent-according-to-mode))
  (newline)
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
Don't use this command in Lisp programs!
\(goto-char (point-min)) is faster and avoids clobbering the mark."
  (interactive "_P")
  (push-mark)
  (goto-char (if arg
		 (if (> (buffer-size) 10000)
		     ;; Avoid overflow for large buffer sizes!
		     (* (prefix-numeric-value arg)
			(/ (buffer-size) 10))
		   (/ (+ 10 (* (buffer-size) (prefix-numeric-value arg))) 10))
	       (point-min)))
  (if arg (forward-line 1)))

(defun end-of-buffer (&optional arg)
  "Move point to the end of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the true end.
Don't use this command in Lisp programs!
\(goto-char (point-max)) is faster and avoids clobbering the mark."
  (interactive "_P")
  (push-mark)
  (let ((scroll-to-end (not (pos-visible-in-window-p (point-max)))))
    (goto-char (if arg
		   (- (1+ (buffer-size))
		      (if (> (buffer-size) 10000)
			  ;; Avoid overflow for large buffer sizes!
			  (* (prefix-numeric-value arg)
			     (/ (buffer-size) 10))
			(/ (* (buffer-size) (prefix-numeric-value arg)) 10)))
		 (point-max)))
    (cond (arg
           ;; If we went to a place in the middle of the buffer,
           ;; adjust it to the beginning of a line.
           (forward-line 1))
	  (scroll-to-end
           ;; If the end of the buffer is not already on the screen,
           ;; then scroll specially to put it near, but not at, the bottom.
           (recenter -3)))))

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
	       (point-min))
             nil
             t))
(define-function 'mark-bob 'mark-beginning-of-buffer)

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
                 (point-max))
             nil
             t))
(define-function 'mark-eob 'mark-end-of-buffer)

(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer.
You probably should not use this function in Lisp programs;
it is usually a mistake for a Lisp function to use any subroutine
that uses or sets the mark."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))

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

(defun count-words-region (start end)
  (interactive "r")
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
	(if (forward-word 1)
	    (setq n (1+ n))))
      (message "Region has %d words" n)
      n)))

(defun count-lines-region (start end)
  "Print number of lines and characters in the region."
  (interactive "_r")
  (let ((n (count-lines start end)))
    (message "Region has %d lines, %d characters"
	     n (- end start))
    n))

(defun count-lines-buffer (b)
  "Print number of lines and charcters in the specified buffer."
  (interactive "_b")
  (save-excursion
    (let ((buf (or b (current-buffer)))
          cnt)
      (set-buffer buf)
      (setq cnt (count-lines (point-min) (point-max)))
      (message "Region has %d lines, %d characters"
               cnt (- (point-max) (point-min)))
      cnt)))

(defun what-line ()
  "Print the current line number (in the buffer) of point."
  (interactive "_")
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (message "Line %d" (1+ (count-lines 1 (point)))))))

(defun count-lines (start end)
  "Return number of lines between START and END.
This is usually the number of newlines between them,
but can be one more if START is not equal to END
and the greater of them is not at the start of a line."
  (save-match-data
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
          (- (buffer-size) (forward-line (buffer-size))))))))

(defun what-cursor-position ()
  "Print info on cursor position (on screen and within buffer)."
  (interactive "_")
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
		   (text-char-description char) char pos total percent beg
		   end col hscroll)
	(message "Char: %s (0%o)  point=%d of %d(%d%%)  column %d %s"
		 (text-char-description char) char pos total percent col
		 hscroll)))))

(defun fundamental-mode ()
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (interactive)
  (kill-all-local-variables))


;; in minibuf.el
;(defvar read-expression-map (copy-keymap minibuffer-local-map)
;  "Minibuffer keymap used for reading Lisp expressions.")
;(define-key read-expression-map "\M-\t" 'lisp-complete-symbol)

;; We define this, rather than making `eval' interactive,
;; for the sake of completion of names like eval-region, eval-current-buffer.
(defun eval-expression (expression)
  "Evaluate EXPRESSION and print value in minibuffer.
Value is also consed on to front of the variable `values'."
  (interactive (list (read-from-minibuffer "Eval: "
					   nil
					   read-expression-map t
					   'read-expression-history)))
  (setq values (cons (eval expression) values))
  (prin1 (car values) t))

(defun edit-and-eval-command (prompt command)
  "Prompting with PROMPT, let user edit COMMAND and eval result.
COMMAND is a Lisp expression.  Let user edit that expression in
the minibuffer, then read and evaluate the result."
  (let ((command (read-from-minibuffer prompt
				       (prin1-to-string command)
				       read-expression-map t
				       'read-expression-history)))
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
(define-function 'advertised-undo 'undo)

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p))
	(recent-save (recent-auto-save-p)))
    (or (eq (selected-window) (minibuffer-window))
	(message "Undo!"))
    (or (eq last-command 'undo)
	(progn (undo-start)
	       (undo-more 1)))
    (setq this-command 'undo)
    (undo-more (or arg 1))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary recent-save))))

(defvar pending-undo-list nil
  "Within a run of consecutive undo commands, list remaining to be undone.")

(defun undo-start ()
  "Set `pending-undo-list' to the front of the undo list.
The next call to `undo-more' will undo the most recently made change."
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (setq pending-undo-list buffer-undo-list))

(defun undo-more (count)
  "Undo back N undo-boundaries beyond what was already undone recently.
Call `undo-start' to get ready to undo recent changes,
then call `undo-more' one or more times to undo them."
  (or pending-undo-list
      (error "No further undo information"))
  (setq pending-undo-list (primitive-undo count pending-undo-list)))


(defun start-process-shell-command (name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER COMMAND &rest COMMAND-ARGS.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is command name, the name of a shell command.
Remaining arguments are the arguments for the command.
Wildcards and redirection are handle as usual in the shell."
  (if (eq system-type 'vax-vms)
      (apply 'start-process name buffer args)
    (start-process name buffer shell-file-name "-c"
		   (concat "exec " (mapconcat 'identity args " ")))))

(defun call-process-region (start end program
                            &optional deletep buffer displayp
                            &rest args)
  "Send text from START to END to a synchronous process running PROGRAM.
Delete the text if fourth arg DELETE is non-nil.
Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining args are passed to PROGRAM at startup as command args.
If BUFFER is nil, returns immediately with value nil.
Otherwise waits for PROGRAM to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is first killed with SIGINT, then with SIGKILL if
you quit again before the process exits."
  (let ((temp (make-temp-name (if (eq system-type 'vax-vms)
                                  "tmp:emacs"
                                  "/tmp/emacs"))))
    (unwind-protect
         (progn
           (write-region start end temp nil 'silent)
           (if deletep (delete-region start end))
           (apply #'call-process program temp buffer displayp args))
      (condition-case ()
          (delete-file temp)
        (file-error nil)))))


(defun shell-command (command &optional flag)
  "Execute string COMMAND in inferior shell; display output, if any.
If COMMAND ends in ampersand, execute it asynchronously.
 
Optional second arg non-nil (prefix arg, if interactive)
means insert output in current buffer after point (leave mark after it).
This cannot be done asynchronously."
  (interactive (list (read-shell-command "Shell command: ")
		     current-prefix-arg))
  (if flag
      (progn (barf-if-buffer-read-only)
	     (push-mark)
	     ;; We do not use -f for csh; we will not support broken use of
	     ;; .cshrcs.  Even the BSD csh manual says to use
	     ;; "if ($?prompt) exit" before things which are not useful
	     ;; non-interactively.  Besides, if someone wants their other
	     ;; aliases for shell commands then they can still have them.
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
		     (read-shell-command "Shell command on region: ")
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
    ;; No prefix argument: put the output in a temp buffer,
    ;; replacing its entire contents.
    (let ((buffer (get-buffer-create "*Shell Command Output*")))
      (if (eq buffer (current-buffer))
	  ;; If the input is the same buffer as the output,
	  ;; delete everything but the specified region,
	  ;; then replace that region with the output.
	  (progn
            (delete-region end (point-max))
            (delete-region (point-min) start)
            (call-process-region (point-min) (point-max)
                                 shell-file-name t t nil
                                 "-c" command))
          (progn
            ;; Clear the output buffer, 
            ;; then run the command with output there.
            (save-excursion
              (set-buffer buffer)
              (erase-buffer))
            (call-process-region start end shell-file-name
                                 nil buffer nil
                                 "-c" command)))
      ;; Report the amount of output.
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
Digits or minus sign following \\[universal-argument] make up the numeric argument.
\\[universal-argument] following the digits or minus sign ends the argument.
\\[universal-argument] without digits or minus sign provides 4 as argument.
Repeating \\[universal-argument] without digits or minus sign
 multiplies the argument by 4 each time."
  (interactive)
  (let ((factor 4)
	 (start-char last-command-event)
        (key nil))
;;;>>>>> Fix read-key-sequence so we can use that instead
;   (setq key (read-key-sequence nil t))
;   (while (equal (key-binding key) 'universal-argument)
;     (setq factor (* 4 factor))
;     (setq key (read-key-sequence nil t)))
    (while (progn
	     (setq key (next-command-event key))
	     (equal key start-char))
      (setq factor (* 4 factor)))
    (prefix-arg-internal key factor nil)))

(defun prefix-arg-internal (event factor value)
  (setq zmacs-region-stays t)
  (let ((sign 1)
	char)
    (if (key-press-event-p event) (setq char (event-key event)))
    (if (and (numberp value) (< value 0))
	(setq sign -1 value (- value)))
    (if (eq value '-)
	(setq sign -1 value nil))
    (while (eq ?- char)
      (setq sign (- sign) factor nil)
      ;;>>(setq key (read-key-sequence nil t)))
      (next-command-event event)
      (if (key-press-event-p event) (setq char (event-key event))))
    (while (and (numberp char) (>= char ?0) (<= char ?9))
      (setq value (+ (* (if (numberp value) value 0) 10) (- char ?0))
            factor nil)
      (next-command-event event)
      (if (key-press-event-p event) (setq char (event-key event))))
    (setq prefix-arg
	  (cond (factor (list factor))
		((numberp value) (* value sign))
		((= sign -1) '-)))
    (if (and (key-press-event-p event)
             (eq (key-binding (vector event)) 'universal-argument))
        ;; Calling universal-argument after digits
        ;; terminates the argument but is ignored.
	(describe-prefix-arg value sign)
      (setq unread-command-event event))))

(defun describe-prefix-arg (value sign)
  (cond ((numberp value)
	 (message "Arg: %d" (* value sign)))
	((consp value)
	 (message "Arg: [%d]" (car value)))
	((< sign 0)
	 (message "Arg: -"))))

(defun digit-argument (arg)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (prefix-arg-internal last-command-event nil arg))

(defun negative-argument (arg)
  "Begin a negative numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
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

(defvar kill-whole-line nil
  "*If non-nil, `kill-line' with no arg at beg of line kills the whole line.")

(defun kill-line (&optional arg)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

If `kill-whole-line' is non-nil, then kill the whole line
when given no argument at the beginning of a line."
  (interactive "*P")
  (kill-region (point)
	       ;; Don't shift point before doing the delete; that way,
	       ;; undo will record the right position of point.
	       (save-excursion
		 (if arg
		     (forward-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
		       (forward-line 1)
		     (end-of-line)))
		 (point))))

;;;; Window system cut and paste hooks.
;;;
;;; I think that kill-hooks is a better name and more general mechanism
;;; than interprogram-cut-function (from RMSmacs).  I don't like the behavior
;;; of interprogram-paste-function: ^Y should always come from the kill ring,
;;; not the X selection.  But if that were provided, it should be called (and
;;; behave as) yank-hooks instead.  -- jwz

;(defvar interprogram-cut-function nil
;  "Function to call to make a killed region available to other programs.
;
;Most window systems provide some sort of facility for cutting and
;pasting text between the windows of different programs.  On startup,
;this variable is set to a function which emacs will call whenever text
;is put in the kill ring to make the new kill available to other
;programs.
;
;The function takes one argument, TEXT, which is a string containing
;the text which should be made available.")
;
;(defvar interprogram-paste-function nil
;  "Function to call to get text cut from other programs.
;
;Most window systems provide some sort of facility for cutting and
;pasting text between the windows of different programs.  On startup,
;this variable is set to a function which emacs will call to obtain
;text that other programs have provided for pasting.
;
;The function should be called with no arguments.  If the function
;returns nil, then no other program has provided such text, and the top
;of the Emacs kill ring should be used.  If the function returns a
;string, that string should be put in the kill ring as the latest kill.
;
;Note that the function should return a string only if a program other
;than Emacs has provided a string for pasting; if Emacs provided the
;most recent string, the function should return nil.  If it is
;difficult to tell whether Emacs or some other program provided the
;current string, it is probably good enough to return nil if the string
;is equal (according to `string=') to the last text Emacs provided.")

(defvar kill-hooks nil
  "Functions run when something is added to the Emacs kill ring.
These functions are called with one argument, the string most recently
cut or copied.  You can use this to, for example, make the most recent 
kill become the X Clipboard selection.")


;;;; The kill ring data structure.

(defvar kill-ring '()
  "List of killed text sequences.
In order to maintain correct interaction with cut-and-paste facilities 
offered by window systems, the functions `kill-new', `kill-append', and
`current-kill' should be used to access the kill ring, instead of using
this variable directly.")

(defvar kill-ring-max 30
  "*Maximum length of kill ring before oldest elements are thrown away.")

(defvar kill-ring-yank-pointer nil
  "The tail of the kill ring whose car is the last thing yanked.")

(defun kill-new (string)
  "Make STRING the latest kill in the kill ring.
Set the kill-ring-yank pointer to point to it.
Runs `kill-hooks'."
  (setq kill-ring (cons string kill-ring))
  (if (> (length kill-ring) kill-ring-max)
      (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq kill-ring-yank-pointer kill-ring)
  ;; note that kill-hooks get called with an arg, so run-hooks won't do.
  (let ((rest kill-hooks))
    (if (or (not (listp rest)) (eq (car-safe rest) 'lambda))
	(and rest (funcall rest string))
      (while rest
	(funcall (car rest) string)
	(setq rest (cdr rest))))))

(defun kill-append (string before-p)
  "Append STRING to the end of the latest kill in the kill ring.
If BEFORE-P is non-nil, prepend STRING to the kill.
Runs `kill-hooks'."
  (setcar kill-ring
	  (if before-p
	      (concat string (car kill-ring))
	      (concat (car kill-ring) string)))
  (let ((rest kill-hooks))
    (if (or (not (listp rest)) (eq (car-safe rest) 'lambda))
	(and rest (funcall rest string))
      (while rest
	(funcall (car rest) string)
	(setq rest (cdr rest))))))

(defun current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If optional arg DO-NOT-MOVE is non-nil, then don't actually move the 
yanking point\; just return the Nth kill forward."
  (or kill-ring (error "Kill ring is empty"))
  (let* ((tem (nthcdr (mod (- n (length kill-ring-yank-pointer))
                           (length kill-ring))
		      kill-ring)))
    (or do-not-move
	(setq kill-ring-yank-pointer tem))
    (car tem)))


;;;; Commands for manipulating the kill ring.

(defun kill-region (beg end &optional verbose)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
\(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)

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
  (if verbose (message "%sing %d characters"
                       (if buffer-read-only "Copy" "Kill")
		       (- (max beg end) (min beg end))))
  (cond ;; I don't like this large change in behavior -- jwz
	;(buffer-read-only
	; (if verbose (message "Copying %d characters"
	;		      (- (max beg end) (min beg end))))
	; (copy-region-as-kill beg end))
        ((not (or (eq buffer-undo-list t)
                  (eq last-command 'kill-region)
                  (eq beg end)))
         ;; In certain cases, we can arrange for the undo list and the kill
         ;; ring to share the same string object.  This code does that.

         ;; Don't let the undo list be truncated before we can even access it.
         (let ((undo-high-threshold (+ (- (max beg end) (min beg end)) 100))
               (old-list buffer-undo-list)
               tail)
           (delete-region beg end)
           ;; Search back in buffer-undo-list for this string,
           ;; in case a change hook made property changes.
           (setq tail buffer-undo-list)
           (while (not (stringp (car (car tail))))
             (setq tail (cdr tail)))
           ;; Take the same string recorded for undo
           ;; and put it in the kill-ring.
           (kill-new (car (car tail)))
           (setq this-command 'kill-region)))
        (t
	 ;; if undo is not kept, grab the string then delete it (which won't
	 ;; add another string to the undo list.)
         (copy-region-as-kill beg end)
         (delete-region beg end))))

(defun copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
Runs `kill-hooks'."
  (interactive "r")
  (if (eq last-command 'kill-region)
      (kill-append (buffer-substring beg end) (< end beg))
    (kill-new (buffer-substring beg end)))
  (setq this-command 'kill-region)
  nil)

(defun kill-ring-save (beg end)
  "Save the region as if killed, but don't kill it.
This command is similar to `copy-region-as-kill', except that it gives
visual feedback indicating the extent of the region being copied."
  (interactive "r")
  (copy-region-as-kill beg end)
  ;; copy before delay, for xclipboard's benefit
  (if (interactive-p)
      (let ((other-end (if (= (point) beg) end beg))
	    (opoint (point))
	    ;; Inhibit quitting so we can make a quit here
	    ;; look like a C-g typed as a command.
	    (inhibit-quit t))
	  (if (pos-visible-in-window-p other-end (selected-window))
	      (progn
		(goto-char other-end)
              (sit-for 0)
              (goto-char opoint)
              ;; If user quit, deactivate the mark
	      ;; as C-g would as a command.
	      (and quit-flag (mark)
                   (zmacs-deactivate-region)))
;; too noisy. -- jwz
;	    (let* ((killed-text (current-kill 0))
;		   (message-len (min (length killed-text) 40)))
;	      (if (= (point) beg)
;		  ;; Don't say "killed"; that is misleading.
;		  (message "Saved text until \"%s\""
;			  (substring killed-text (- message-len)))
;                  (message "Saved text from \"%s\""
;                           (substring killed-text 0 message-len))))
	    ))))


(defun append-next-kill ()
  "Cause following command, if it kills, to append to previous kill."
  (interactive "_")
  (if (interactive-p)
      (progn
	(setq this-command 'kill-region)
	(message "If the next command is a kill, it will append"))
    (setq last-command 'kill-region)))

(defun yank-pop (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is allowed only immediately after a `yank' or a `yank-pop'.
At such a time, the region contains a stretch of reinserted
previously-killed text.  `yank-pop' deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument N, insert the Nth previous kill.
If N is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (let ((before (< (point) (mark t))))
    (delete-region (point) (mark t))
    (set-mark (point))
    (insert (current-kill arg))
    (if before (exchange-point-and-mark t))))

(defun yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.  Put point at end, and set mark at beginning.
With just C-u as argument, same but put point at beginning (and mark at end).
With argument N, reinsert the Nth most recently killed stretch of killed text.
See also the command \\[yank-pop]."
  (interactive "*P")
  (push-mark (point))
  (insert (current-kill (cond ((listp arg) 0)
                              ((eq arg '-) -1)
                              (t (1- arg)))))
  (if (consp arg)
      (exchange-point-and-mark t)))

(defun rotate-yank-pointer (arg)
  "Rotate the yanking point in the kill ring.
With argument, rotate that many kills forward (or backward, if negative)."
  (interactive "p")
  (current-kill arg))

(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  (interactive (list (progn (barf-if-buffer-read-only)
                            (read-buffer "Insert buffer: " (other-buffer) t))))
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
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive (list (read-buffer "Append to buffer: "
                                  (other-buffer nil t) t)
                     (region-beginning) (region-end)))
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))

(defun prepend-to-buffer (buffer start end)
  "Prepend to specified buffer the text of the region.
It is inserted into that buffer after its point.

When calling from a program, give three arguments:
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
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
BUFFER (or buffer name), START and END.
START and END specify the portion of the current buffer to be copied."
  (interactive "BCopy to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (erase-buffer)
      (save-excursion
	(insert-buffer-substring oldbuf start end)))))

;;;>>> RMSmacs
;(defvar mark-even-if-inactive nil
;  "*Non-nil means you can use the mark even when inactive.
;This option makes a difference in Transient Mark mode.
;When the option is non-nil, deactivation of the mark
;turns off region highlighting, but commands that use the mark
;behave as if the mark were still active.")
;
;(put 'mark-inactive 'error-conditions '(mark-inactive error))
;(put 'mark-inactive 'error-message "The mark is not active now")

(defun mark (&optional inactive-p)
  "Return this buffer's mark value as integer, or nil if no mark.

If `zmacs-regions' is true, then this returns nil unless the region is
currently in the active (highlighted) state.  With an argument of t, this
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

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  The mark saves a location for the user's convenience.
Most editing commands should not alter the mark.
To remember a location for internal use in the Lisp program,
store it in a Lisp variable.  Example:

   (let ((beg (point))) (forward-line 1) (delete-region beg (point)))."

  (set-marker (mark-marker t) pos (current-buffer)))

(defvar mark-ring '()
  "The list of saved former marks of the current buffer,
most recent first.")
(make-variable-buffer-local 'mark-ring)

(defvar mark-ring-max 16
  "*Maximum size of mark ring.  Start discarding off end if gets this big.")

(defun set-mark-command (arg)
  "Set mark at where point is, or jump to mark.
With no prefix argument, set mark, and push old mark position on mark ring.
With argument, jump to mark, and pop a new position for mark off the ring.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
  (interactive "P")
  (if (null arg)
      (push-mark nil nil t)
    (if (null (mark t))
	(error "No mark set in this buffer")
      (goto-char (mark t))
      (pop-mark))))

(defun push-mark (&optional location nomsg activate-region)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
Display \"Mark set\" unless the optional second arg NOMSG is non-nil.
Activate mark if optional third arg ACTIVATE-REGION non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
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
  (if activate-region (zmacs-activate-region))
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

(define-function 'exchange-dot-and-mark 'exchange-point-and-mark)
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

;(defun transient-mark-mode (arg)
;  "Toggle Transient Mark mode.
;With arg, turn Transient Mark mode on if arg is positive, off otherwise.
;
;In Transient Mark mode, changing the buffer \"deactivates\" the mark.
;While the mark is active, the region is highlighted."
;  (interactive "P")
;  (setq transient-mark-mode
;	(if (null arg)
;	    (not transient-mark-mode)
;	  (> (prefix-numeric-value arg) 0))))

(defvar next-line-add-newlines t
  "*If non-nil, when \\[next-line] is invoked on the last line of a buffer,
a newline character is inserted to create a new line.
If nil, \\[next-line] signals an `end-of-buffer' in that situation.")

(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.

If there is no line in the buffer after this one, behavior depends on the
value of next-line-add-newlines.  If non-nil, a newline character is inserted
to create a line and the cursor moves to that line, otherwise the cursor is
moved to the end of the buffer (if already at the end of the buffer, an error
is signaled).

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.  This goal column is stored
in `goal-column', which is nil when there is none.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "_p")
      (let ((opoint (point)))
    (if next-line-add-newlines
	(if (/= arg 1)
	    (line-move arg)
	(forward-line 1)
	(if (or (= opoint (point))
		(not (eq (preceding-char) ?\n)))
              (insert ?\n)
	  (goto-char opoint)
	  (line-move arg)))
      (if (eobp)
	  (signal 'end-of-buffer nil))
      (line-move arg)
      (if (= opoint (point))
	  (end-of-line))))
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
`forward-line' with a negative argument instead.  It is usually easier
to use and more reliable (no dependence on goal column, etc.)."
  (interactive "_p")
  (line-move (- arg))
  nil)

(defvar track-eol nil
  "*Non-nil means vertical motion starting at end of line keeps to ends of lines.
This means moving to the end of each line moved onto.
The beginning of a blank line does not count as the end of a line.")

(defvar goal-column nil
  "*Semipermanent goal column for vertical motion, as set by \\[set-goal-column], or nil.")
(make-variable-buffer-local 'goal-column)

(defvar temporary-goal-column 0
  "Current goal column for vertical motion.
It is the column where point was at the start of current run of vertical motion commands.
When the `track-eol' feature is doing its job, the value is 9999.")

(defun line-move (arg)
  (if (not (or (eq last-command 'next-line)
	       (eq last-command 'previous-line)))
      (setq temporary-goal-column
	    (if (and track-eol (eolp)
                     ;; Don't count beg of empty line as end of line
		     ;; unless we just did explicit end-of-line.
		     (or (not (bolp)) (eq last-command 'end-of-line)))
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
  nil)


(defun set-goal-column (arg)
  "Set the current horizontal position as a goal for \\[next-line] and \\[previous-line].
Those commands will move to this position in the line moved to
rather than trying to keep the same horizontal position.
With a non-nil argument, clears out the goal column
so that \\[next-line] and \\[previous-line] resume vertical motion.
The goal column is stored in the variable `goal-column'."
  (interactive "_P")
  (if arg
      (progn
        (setq goal-column nil)
        (message "No goal column"))
    (setq goal-column (current-column))
    (message (substitute-command-keys
	      "Goal column %d (use \\[set-goal-column] with an arg to unset it)")
	     goal-column))
  nil)

;;;>>>> RMSmacs terminal randomness
;;; Partial support for horizontal autoscrolling.  Someday, this feature
;;; will be built into the C level and all the (hscroll-point-visible) calls
;;; will go away.

;(defvar hscroll-step 0
;   "*The number of columns to try scrolling a window by when point moves out.
;If that fails to bring point back on frame, point is centered instead.
;If this is zero, point is always centered after it moves off frame.")
;
;(defun hscroll-point-visible ()
;  "Scrolls the window horizontally to make point visible."
;  (let* ((here (current-column))
;	 (left (window-hscroll))
;	 (right (- (+ left (window-width)) 3)))
;    (cond
;     ;; Should we recenter?
;     ((or (< here (- left  hscroll-step))
;	  (> here (+ right hscroll-step)))
;      (set-window-hscroll
;       (selected-window)
;       ;; Recenter, but don't show too much white space off the end of
;       ;; the line.
;       (max 0
;	    (min (- (save-excursion (end-of-line) (current-column))
;		    (window-width)
;		    -5)
;		 (- here (/ (window-width) 2))))))
;     ;; Should we scroll left?
;     ((> here right)
;      (scroll-left hscroll-step))
;     ;; Or right?
;     ((< here left)
;      (scroll-right hscroll-step)))))
;
;; rms: (1) The definitions of arrow keys should not simply restate
;; what keys they are.  The arrow keys should run the ordinary commands.
;; (2) The arrow keys are just one of many common ways of moving point
;; within a line.  Real horizontal autoscrolling would be a good feature,
;; but supporting it only for arrow keys is too incomplete to be desirable.

;;;;; Make arrow keys do the right thing for improved terminal support
;;;;; When we implement true horizontal autoscrolling, right-arrow and
;;;;; left-arrow can lose the (if truncate-lines ...) clause and become
;;;;; aliases.  These functions are bound to the corresponding keyboard
;;;;; events in loaddefs.el.

;;(defun right-arrow (arg)
;;  "Move right one character on the screen (with prefix ARG, that many chars).
;;Scroll right if needed to keep point horizontally onscreen."
;;  (interactive "P")
;;  (forward-char arg)
;;  (hscroll-point-visible))

;;(defun left-arrow (arg)
;;  "Move left one character on the screen (with prefix ARG, that many chars).
;;Scroll left if needed to keep point horizontally onscreen."
;;  (interactive "P")
;;  (backward-char arg)
;;  (hscroll-point-visible))

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
  (transpose-subr #'(lambda (arg)
		     (if (= arg 1)
			 (progn
			   ;; Move forward over a line,
			   ;; but create a newline if none exists yet.
			   (end-of-line)
			   (if (eobp)
			       (newline)
			     (forward-char 1)))
		       (forward-line arg)))
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

(defvar comment-column 32
  "*Column to indent right-margin comments to.
Setting this variable automatically makes it local to the current buffer.
Each mode establishes a different default value for this variable; you
can the value for a particular mode using that mode's hook.")
(make-variable-buffer-local 'comment-column)

(defvar comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax defined.")

(defvar comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair.")

(defvar comment-end ""
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line.")

(defconst comment-indent-hook nil
  "Obsolete variable for function to compute desired indentation for a comment.
Use `comment-indent-function' instead.
This function is called with no args with point at the beginning of
the comment's starting delimiter.")

(defvar comment-indent-function
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
		   ;; If comment-start-skip matched a string with
		   ;; internal whitespace (not final whitespace) then
		   ;; the delimiter start at the end of that
		   ;; whitespace.  Otherwise, it starts at the
		   ;; beginning of what was matched.
		   (skip-syntax-backward " " (match-beginning 0))
		   (skip-syntax-backward "^ " (match-beginning 0)))))
      (setq begpos (point))
      ;; Compute desired indent.
      (if (= (current-column)
	     (setq indent (if comment-indent-hook
                              ;; old name
                              (funcall comment-indent-hook)
                              (funcall comment-indent-function))))
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

(defun comment-region (beg end &optional arg)
  "Comment the region; third arg numeric means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.
Comments are terminated on each line, even for syntax in which newline does
not end the comment.  Blank lines do not get comments."
  ;; if someone wants it to only put a comment-start at the beginning and
  ;; comment-end at the end then typing it, C-x C-x, closing it, C-x C-x
  ;; is easy enough.  No option is made here for other than commenting
  ;; every line.
  (interactive "*r\np")
  (or comment-start (error "No comment syntax is defined"))
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (save-restriction
      (let ((cs comment-start) (ce comment-end))
        (cond ((not arg) (setq arg 1))
              ((> arg 1)
               (while (> (setq arg (1- arg)) 0)
                 (setq cs (concat cs comment-start)
                       ce (concat ce comment-end)))))
        (narrow-to-region beg end)
        (goto-char beg)
        (while (not (eobp))
          (if (< arg 0)
              (let ((count arg))
                (while (and (> 1 (setq count (1+ count)))
                            (looking-at (regexp-quote cs)))
                  (delete-char (length cs)))
                (if (string= "" ce) ()
                  (setq count arg)
                  (while (> 1 (setq count (1+ count)))
                    (end-of-line)
                    ;; this is questionable if comment-end ends in whitespace
                    ;; that is pretty brain-damaged though
                    (skip-chars-backward " \t")
                    (backward-char (length ce))
                    (if (looking-at (regexp-quote ce))
                        (delete-char (length ce)))))
                (forward-line 1))
            (if (looking-at "[ \t]*$") ()
              (insert cs)
              (if (string= "" ce) ()
                (end-of-line)
                (insert ce)))
            (search-forward "\n" nil 'move)))))))

(defun backward-word (arg)
  "Move backward until encountering the end of a word.
With argument, do this that many times.
In programs, it is faster to call `forward-word' with negative arg."
  (interactive "_p")
  (forward-word (- arg)))

(defun mark-word (arg)
  "Set mark arg words away from point."
  (interactive "p")
  (push-mark
    (save-excursion
      (forward-word arg)
      (point))
    nil t))

(defun kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (kill-region (point) (save-excursion (forward-word arg) (point))))

(defun backward-kill-word (arg)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (kill-word (- arg)))

(defun current-word ()
  "Return the word point is on as a string, if it's between two
word-constituent characters. If not, but it immediately follows one,
move back first.  Otherwise, if point precedes a word constituent,
move forward first.  Otherwise, move backwards until a word constituent
is found and get that word; if you reach a newline first, move forward
instead."
  (interactive)
  (save-excursion
    (let ((oldpoint (point)) (start (point)) (end (point)))
      (skip-syntax-backward "w_") (setq start (point))
      (goto-char oldpoint)
      (skip-syntax-forward "w_") (setq end (point))
      (if (and (eq start oldpoint) (eq end oldpoint))
	  (progn
	    (skip-syntax-backward "^w_"
				  (save-excursion (beginning-of-line) (point)))
	    (if (eq (preceding-char) ?\n)
		(progn
		  (skip-syntax-forward "^w_")
		  (setq start (point))
		  (skip-syntax-forward "w_")
		  (setq end (point)))
	      (setq end (point))
	      (skip-syntax-backward "w_")
	      (setq start (point)))))
      (buffer-substring start end))))

(defvar fill-prefix nil
  "*String for filling to insert at front of new line, or nil for none.
Setting this variable automatically makes it local to the current buffer.")
(make-variable-buffer-local 'fill-prefix)

(defvar auto-fill-inhibit-regexp nil
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
		(let ((prev-column (current-column)))
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
		  ;; If making the new line didn't reduce the hpos of
		  ;; the end of the line, then give up now;
		  ;; trying again will not help.
		  (if (>= (current-column) prev-column)
		      (setq give-up t)))
	      ;; No place to break => stop trying.
	      (setq give-up t)))))))

(defvar comment-multi-line nil
  "*Non-nil means \\[indent-new-comment-line] should continue same comment
on new line, with no new terminator or starter.
This is obsolete because you might as well use \\[newline-and-indent].")

(defun indent-new-comment-line ()
  "Break line at point and indent, continuing comment if presently within one.
The body of the continued comment is indented under the previous comment line.

This command is intended for styles where you write a comment per line,
starting a new comment (and terminating it if necessary) on each line.
If you want to continue one comment across several lines, use \\[newline-and-indent]."
  (interactive "*")
  (let (comcol comstart)
    (skip-chars-backward " \t")
    (delete-region (point)
		   (progn (skip-chars-forward " \t")
			  (point)))
    (insert ?\n)
    (if (not comment-multi-line)
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
		(setq comstart (buffer-substring (point) (match-end 0)))))))
    (if comcol
	(let ((comment-column comcol)
	      (comment-start comstart)
	      (comment-end comment-end))
	  (and comment-end (not (equal comment-end ""))
;	       (if (not comment-multi-line)
		   (progn
		     (forward-char -1)
		     (insert comment-end)
		     (forward-char 1))
;		 (setq comment-column (+ comment-column (length comment-start))
;		       comment-start "")
;                  )
          )
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
  "Set `fill-column' to current column, or to argument if given.
The variable `fill-column' has a separate value for each buffer."
  (interactive "_P")
  (setq fill-column (if (integerp arg) arg (current-column)))
  (message "fill-column set to %d" fill-column))

(defun set-selective-display (arg)
  "Set `selective-display' to ARG; clear it if no arg.
When the value of `selective-display' is a number > 0,
lines whose indentation is >= that value are not displayed.
The variable `selective-display' has a separate value for each buffer."
  (interactive "P")
  (if (eq selective-display t)
      (error "selective-display already in use for marked lines"))
  (let ((current-vpos
	 (save-restriction
	   (narrow-to-region (point-min) (point))
	   (goto-char (window-start))
	   (vertical-motion (window-height)))))
    (setq selective-display
	  (and arg (prefix-numeric-value arg)))
    (recenter current-vpos))
  (set-window-start (selected-window) (window-start (selected-window)))
  (princ "selective-display set to " t)
  (prin1 selective-display t)
  (princ "." t))

(defvar overwrite-mode-textual (purecopy " Ovwrt")
  "The string displayed in the mode line when in overwrite mode.")
(defvar overwrite-mode-binary (purecopy " Bin Ovwrt")
  "The string displayed in the mode line when in binary overwrite mode.")

(defun overwrite-mode (arg)
  "Toggle overwrite mode.
With arg, turn overwrite mode on iff arg is positive.
In overwrite mode, printing characters typed in replace existing text
on a one-for-one basis, rather than pushing it to the right.  At the
end of a line, such characters extend the line.  Before a tab,
such characters insert until the tab is filled in.
\\[quoted-insert] still inserts characters in overwrite mode; this
is supposed to make it easier to insert characters when necessary."
  (interactive "P")
  (setq overwrite-mode
	(if (if (null arg) (not overwrite-mode)
	      (> (prefix-numeric-value arg) 0))
	    'overwrite-mode-textual))
  (force-mode-line-update))

(defun binary-overwrite-mode (arg)
  "Toggle binary overwrite mode.
With arg, turn binary overwrite mode on iff arg is positive.
In binary overwrite mode, printing characters typed in replace
existing text.  Newlines are not treated specially, so typing at the
end of a line joins the line to the next, with the typed character
between them.  Typing before a tab character simply replaces the tab
with the character typed.
\\[quoted-insert] replaces the text at the cursor, just as ordinary
typing characters do.

Note that binary overwrite mode is not its own minor mode; it is a
specialization of overwrite-mode, entered by setting the
`overwrite-mode' variable to `overwrite-mode-binary'."
  (interactive "P")
  (setq overwrite-mode
	(if (if (null arg)
		(not (eq overwrite-mode 'overwrite-mode-binary))
	      (> (prefix-numeric-value arg) 0))
	    'overwrite-mode-binary))
  (force-mode-line-update))

;;;>>> NYI
;(defvar line-number-mode nil
;  "*Non-nil means display line number in mode line.")
;
;(defun line-number-mode (arg)
;  "Toggle Line Number mode.
;With arg, turn Line Number mode on iff arg is positive.
;When Line Number mode is enabled, the line number appears
;in the mode line."
;  (interactive "P")
;  (setq line-number-mode
;	(if (null arg) (not line-number-mode)
;	  (> (prefix-numeric-value arg) 0)))
;  (force-mode-line-update))


(defvar blink-matching-paren t
  "*Non-nil means show matching open-paren when close-paren is inserted.")

(defvar blink-matching-paren-distance 12000
  "*If non-nil, is maximum distance to search for matching open-paren
when close-paren is inserted.")

(defun blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  (interactive "_")
  (and (> (point) (1+ (point-min)))
       (/= (char-syntax (char-after (- (point) 2))) ?\\ )
       blink-matching-paren
       (let* ((oldpos (point))
	      (parse-sexp-ignore-comments t) ; to avoid C++ lossage
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

(defun set-variable (var val)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
When using this interactively, supply a Lisp expression for VALUE.
If you want VALUE to be a string, you must surround it with doublequotes.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value."
  (interactive
   (let* ((var (read-variable "Set variable: "))
	  (minibuffer-help-form
	   '(funcall myhelp))
	  (myhelp
	   #'(lambda ()
	      (with-output-to-temp-buffer "*Help*"
		(prin1 var)
		(princ "\nDocumentation:\n")
		(princ (substring (documentation-property var 'variable-documentation)
				  1))
		(if (boundp var)
		    (let ((print-length 20))
		      (princ "\n\nCurrent value: ")
		      (prin1 (symbol-value var))))
		nil))))
     (list var
	   (let ((prop (get var 'variable-interactive)))
	     (if prop
		 ;; Use VAR's `variable-interactive' property
		 ;; as an interactive spec for prompting.
		 (call-interactively (list 'lambda '(arg)
					   (list 'interactive prop)
					   'arg))
	       (eval-minibuffer (format "Set %s to value: " var)))))))
  (set var val))
