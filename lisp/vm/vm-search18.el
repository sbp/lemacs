;; Incremental search through a mail folder
;; For version 18 of FSF Emacs only.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; Adapted for the VM mail reader, Kyle Jones, May 1989


;; This function does all the work of incremental search.
;; The functions attached to ^R and ^S are trivial,
;; merely calling this one, but they are always loaded by default
;; whereas this file can optionally be autoloadable.
;; This is the only entry point in this file.

(defun vm-isearch (forward &optional regexp)
  (let ((search-string "")
	(search-message "")
	(cmds nil)
	(success t)
	(wrapped nil)
	(barrier (point))
	adjusted
	(invalid-regexp nil)
	(slow-terminal-mode (and (<= (baud-rate) search-slow-speed)
				 (> (window-height)
				    (* 4 search-slow-window-lines))))
	(other-end nil)    ;Start of last match if fwd, end if backwd.
	(small-window nil)		;if t, using a small window
	(found-point nil)		;to restore point from a small window
	;; This is the window-start value found by the search.
	(found-start nil)
	(opoint (point))
	(vm-ml-message-new vm-ml-message-new)
	(vm-ml-message-unread vm-ml-message-unread)
	(vm-ml-message-read vm-ml-message-read)
	(vm-ml-message-edited vm-ml-message-edited)
	(vm-ml-message-replied vm-ml-message-replied)
	(vm-ml-message-forwarded vm-ml-message-forwarded)
	(vm-ml-message-filed vm-ml-message-filed)
	(vm-ml-message-written vm-ml-message-written)
	(vm-ml-message-marked vm-ml-message-marked)
	(vm-ml-message-deleted vm-ml-message-deleted)
	(vm-ml-message-number vm-ml-message-number)
	(vm-message-pointer vm-message-pointer)
	(inhibit-quit t))  ;Prevent ^G from quitting immediately.
    (vm-isearch-push-state)
    (save-window-excursion
     (catch 'search-done
       (while t
	 (or (>= unread-command-char 0)
	     (progn
	       (or (input-pending-p)
		   (vm-isearch-message))
	       (if (and slow-terminal-mode
			(not (or small-window (pos-visible-in-window-p))))
		   (progn
		     (setq small-window t)
		     (setq found-point (point))
		     (move-to-window-line 0)
		     (let ((window-min-height 1))
		       (split-window nil (if (< search-slow-window-lines 0)
					     (1+ (- search-slow-window-lines))
					   (- (window-height)
					      (1+ search-slow-window-lines)))))
		     (if (< search-slow-window-lines 0)
			 (progn (vertical-motion (- 1 search-slow-window-lines))
				(set-window-start (next-window) (point))
				(set-window-hscroll (next-window)
						    (window-hscroll))
				(set-window-hscroll (selected-window) 0))
		       (other-window 1))
		     (goto-char found-point)))))
	 (let ((char (if quit-flag
			 ?\C-g
		       (read-char))))
	   (setq quit-flag nil adjusted nil)
	   ;; Meta character means exit search.
	   (cond ((and (>= char 128)
		       search-exit-option)
		  (setq unread-command-char char)
		  (throw 'search-done t))
		 ((eq char search-exit-char)
		  ;; Esc means exit search normally.
		  ;; Except, if first thing typed, it means do nonincremental
		  (if (= 0 (length search-string))
		      (vm-nonincremental-search forward regexp))
		  (throw 'search-done t))
		 ((= char ?\C-g)
		  ;; ^G means the user tried to quit.
		  (ding)
		  (discard-input)
		  (if success
		      ;; If search is successful, move back to starting point
		      ;; and really do quit.
		      (progn (goto-char opoint)
			     (vm-update-search-position)
			     (signal 'quit nil))
		    ;; If search is failing, rub out until it is once more
		    ;;  successful.
		    (while (not success) (vm-isearch-pop))))
		 ((or (eq char search-repeat-char)
		      (eq char search-reverse-char))
		  (if (eq forward (eq char search-repeat-char))
		      ;; C-s in forward or C-r in reverse.
		      (if (equal search-string "")
			  ;; If search string is empty, use last one.
			  (setq search-string
				(if regexp
				    search-last-regexp search-last-string)
				search-message
				(mapconcat 'text-char-description
					   search-string ""))
			;; If already have what to search for, repeat it.
			(or success
			    (progn (goto-char (if forward (point-min) (point-max)))
				   (setq wrapped t))))
		    ;; C-s in reverse or C-r in forward, change direction.
		    (setq forward (not forward)))
		  (setq barrier (point)) ; For subsequent \| if regexp.
		  (setq success t)
		  (or (equal search-string "")
		      (vm-isearch-search))
		  (vm-isearch-push-state))
		 ((= char search-delete-char)
		  ;; Rubout means discard last input item and move point
		  ;; back.  If buffer is empty, just beep.
		  (if (null (cdr cmds))
		      (ding)
		    (vm-isearch-pop)))
		 (t
		  (cond ((or (eq char search-yank-word-char)
			     (eq char search-yank-line-char))
			 ;; ^W means gobble next word from buffer.
			 ;; ^Y means gobble rest of line from buffer.
			 (let ((word (save-excursion
				       (and (not forward) other-end
					    (goto-char other-end))
				       (buffer-substring
					(point)
					(save-excursion
					  (if (eq char search-yank-line-char)
					      (end-of-line)
					    (forward-word 1))
					  (point))))))
			   (setq search-string (concat search-string word)
				 search-message
				   (concat search-message
					   (mapconcat 'text-char-description
						      word "")))))
			 ;; Any other control char =>
			 ;;  unread it and exit the search normally.
			 ((and search-exit-option
			       (/= char search-quote-char)
			       (or (= char ?\177)
				   (and (< char ? ) (/= char ?\t) (/= char ?\r))))
			  (setq unread-command-char char)
			  (throw 'search-done t))
			 (t
			  ;; Any other character => add it to the
			  ;;  search string and search.
			  (cond ((= char search-quote-char)
				 (setq char (read-quoted-char
					     (vm-isearch-message t))))
				((= char ?\r)
				 ;; unix braindeath
				 (setq char ?\n)))
			  (setq search-string (concat search-string
						      (char-to-string char))
				search-message (concat search-message
						       (text-char-description char)))))
		  (if (and (not success)
			   ;; unsuccessful regexp search may become
			   ;;  successful by addition of characters which
			   ;;  make search-string valid
			   (not regexp))
		      nil
		    ;; If a regexp search may have been made more
		    ;; liberal, retreat the search start.
		    ;; Go back to place last successful search started
		    ;; or to the last ^S/^R (barrier), whichever is nearer.
		    (and regexp success cmds
			 (cond ((memq char '(?* ??))
				(setq adjusted t)
				(let ((cs (nth (if forward
						   5 ; other-end
						 2) ; saved (point)
					       (car (cdr cmds)))))
				  ;; (car cmds) is after last search;
				  ;; (car (cdr cmds)) is from before it.
				  (setq cs (or cs barrier))
				  (goto-char
				   (if forward
				       (max cs barrier)
				     (min cs barrier)))))
			       ((eq char ?\|)
				(setq adjusted t)
				(goto-char barrier))))
		    ;; In reverse regexp search, adding a character at
		    ;; the end may cause zero or many more chars to be
		    ;; matched, in the string following point.
		    ;; Allow all those possibilities without moving point as
		    ;; long as the match does not extend past search origin.
		    (if (and regexp (not forward) (not adjusted)
			     (condition-case ()
				 (looking-at search-string)
			       (error nil))
			     (<= (match-end 0) (min opoint barrier)))
			(setq success t invalid-regexp nil
			      other-end (match-end 0))
		      ;; Not regexp, not reverse, or no match at point.
		      (if (and other-end (not adjusted))
			  (goto-char (if forward other-end
				       (min opoint barrier (1+ other-end)))))
		      (vm-isearch-search)))
		  (vm-isearch-push-state))))))
     (setq found-start (window-start (selected-window)))
     (setq found-point (point)))
    (if (> (length search-string) 0)
	(if regexp
	    (setq search-last-regexp search-string)
	    (setq search-last-string search-string)))
    (message "")
    (if small-window
	(goto-char found-point)
      ;; Exiting the save-window-excursion clobbers this; restore it.
      (set-window-start (selected-window) found-start t))))

(defun vm-isearch-message (&optional c-q-hack ellipsis)
  ;; If about to search, and previous search regexp was invalid,
  ;; check that it still is.  If it is valid now,
  ;; let the message we display while searching say that it is valid.
  (and invalid-regexp ellipsis
       (condition-case ()
	   (progn (re-search-forward search-string (point) t)
		  (setq invalid-regexp nil))
	 (error nil)))
  ;; If currently failing, display no ellipsis.
  (or success (setq ellipsis nil))
  (let ((m (concat (if success "" "failing ")
		   (if wrapped "wrapped ")
		   (if regexp "regexp " "")
		   "VM I-search"
		   (if forward ": " " backward: ")
		   search-message
		   (if c-q-hack "^Q" "")
		   (if invalid-regexp
		       (concat " [" invalid-regexp "]")
		     ""))))
    (aset m 0 (upcase (aref m 0)))
    (let ((cursor-in-echo-area ellipsis))
      (if c-q-hack m (message "%s" m)))))

(defun vm-isearch-pop ()
  (setq cmds (cdr cmds))
  (let ((cmd (car cmds)))
    (setq search-string (car cmd)
	  search-message (car (cdr cmd))
	  success (nth 3 cmd)
	  forward (nth 4 cmd)
	  other-end (nth 5 cmd)
	  invalid-regexp (nth 6 cmd)
	  wrapped (nth 7 cmd)
	  barrier (nth 8 cmd)
; unused now
;	  vm-ml-attributes-string (nth 9 cmd)
	  vm-ml-message-number (nth 10 cmd)
	  vm-message-pointer (nth 11 cmd))
    (if vm-summary-buffer
	(save-excursion
	  (set-buffer vm-summary-buffer)
	  (setq
	   ; unused now
	   ;vm-ml-attributes-string (nth 9 cmd)
	   vm-ml-message-number (nth 10 cmd))))
    (goto-char (car (cdr (cdr cmd))))
    (vm-set-summary-pointer (car vm-message-pointer))))

(defun vm-isearch-push-state ()
  (setq cmds (cons (list search-string search-message (point)
			 success forward other-end invalid-regexp
			 wrapped barrier
; unused now
;			 vm-ml-attributes-string
			 nil
			 vm-ml-message-number
			 vm-message-pointer)
		   cmds)))

(defun vm-isearch-search ()
  (vm-isearch-message nil t)
  (condition-case lossage
      (let ((inhibit-quit nil))
	(if regexp (setq invalid-regexp nil))
	(setq success
	      (funcall
	       (if regexp
		   (if forward 're-search-forward 're-search-backward)
		 (if forward 'search-forward 'search-backward))
	       search-string nil t))
	(if success
	    (setq other-end
		  (if forward (match-beginning 0) (match-end 0)))))
    (quit (setq unread-command-char ?\C-g)
	  (setq success nil))
    (invalid-regexp (setq invalid-regexp (car (cdr lossage)))
		    (if (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
				      invalid-regexp)
			(setq invalid-regexp "incomplete input"))))
  (if success
      (vm-update-search-position)
    ;; Ding if failed this time after succeeding last time.
    (and (nth 3 (car cmds))
	 (ding))
    (goto-char (nth 2 (car cmds)))))

;; This is called from incremental-search
;; if the first input character is the exit character.
;; The interactive-arg-reader uses free variables `forward' and `regexp'
;; which are bound by `incremental-search'.

;; We store the search string in `search-string'
;; which has been bound already by `incremental-search'
;; so that, when we exit, it is copied into `search-last-string'.

(defun vm-nonincremental-search (forward regexp)
  (let (message char function string inhibit-quit
		(cursor-in-echo-area t))
    ;; Prompt assuming not word search,
    (setq message (if regexp 
		      (if forward "VM Regexp search: "
			"VM Regexp search backward: ")
		    (if forward "VM Search: " "VM Search backward: ")))
    (message "%s" message)
    ;; Read 1 char and switch to word search if it is ^W.
    (setq char (read-char))
    (if (eq char search-yank-word-char)
	(setq message (if forward "VM Word search: " "VM Word search backward: "))
      ;; Otherwise let that 1 char be part of the search string.
      (setq unread-command-char char))
    (setq function
	  (if (eq char search-yank-word-char)
	      (if forward 'word-search-forward 'word-search-backward)
	    (if regexp
		(if forward 're-search-forward 're-search-backward)
	      (if forward 'search-forward 'search-backward))))
    ;; Read the search string with corrected prompt.
    (setq string (read-string message))
    ;; Empty means use default.
    (if (= 0 (length string))
	(setq string search-last-string)
      ;; Set last search string now so it is set even if we fail.
      (setq search-last-string string))
    ;; Since we used the minibuffer, we should be available for redo.
    (setq command-history (cons (list function string) command-history))
    ;; Go ahead and search.
    (funcall function string)))

(defun vm-update-search-position (&optional record-change)
  (if (and (>= (point) (vm-start-of (car vm-message-pointer)))
	   (<= (point) (vm-end-of (car vm-message-pointer))))
      nil
    (let ((mp vm-message-list)
	  (point (point)))
      (while mp
	(if (and (>= point (vm-start-of (car mp)))
		 (<= point (vm-end-of (car mp))))
	    (if record-change
		(progn
		  (vm-record-and-change-message-pointer vm-message-pointer mp)
		  (setq mp nil))
	      (setq vm-message-pointer mp mp nil))
	  (setq mp (cdr mp))))
      (setq vm-need-summary-pointer-update t)
      (vm-update-summary-and-mode-line))))

(defun vm-isearch-forward ()
  "Incrementally search forward through the current folder's messages.
Usage is identical to the standard Emacs incremental search.
When the search terminates the message containing point will be selected.

If the variable vm-search-using-regexps is non-nil, regular expressions
are understood; nil means the search will be for the input string taken
literally."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-error-if-virtual-folder)
  (vm-display (current-buffer) t '(vm-isearch-forward) '(vm-isearch-forward))
  (let ((clip-head (point-min))
	(clip-tail (point-max))
	(old-w (selected-window)))
    (unwind-protect
	(progn (select-window (get-buffer-window (current-buffer)))
	       (widen)
	       (vm-isearch t vm-search-using-regexps)
	       (vm-update-search-position t)
	       ;; vm-show-current-message only adjusts (point-max),
	       ;; it doesn't change (point-min).
	       (narrow-to-region
		(if (< (point) (vm-vheaders-of (car vm-message-pointer)))
		    (vm-start-of (car vm-message-pointer))
		  (vm-vheaders-of (car vm-message-pointer)))
		(point-max))
	       (vm-show-current-message)
	       (setq vm-system-state 'reading)
	       ;; turn the clipping unwind into a noop
	       (setq clip-head (point-min))
	       (setq clip-tail (point-max)))
      (narrow-to-region clip-head clip-tail)
      (select-window old-w))))

(defun vm-isearch-backward ()
  "Incrementally search backward through the current folder's messages.
Usage is identical to the standard Emacs incremental search.
When the search terminates the message containing point will be selected.

If the variable vm-search-using-regexps is non-nil, regular expressions
are understood; nil means the search will be for the input string taken
literally."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-error-if-virtual-folder)
  (vm-display (current-buffer) t '(vm-isearch-backward)
	      (list 'vm-isearch-backward 'searching-message))
  (let ((clip-head (point-min))
	(clip-tail (point-max))
	(old-w (selected-window)))
    (unwind-protect
	(progn (select-window (get-buffer-window (current-buffer)))
	       (widen)
	       (vm-isearch nil vm-search-using-regexps)
	       (vm-update-search-position t)
	       ;; vm-show-current-message only adjusts (point-max),
	       ;; it doesn't change (point-min).
	       (narrow-to-region
		(if (< (point) (vm-vheaders-of (car vm-message-pointer)))
		    (vm-start-of (car vm-message-pointer))
		  (vm-vheaders-of (car vm-message-pointer)))
		(point-max))
	       (vm-show-current-message)
	       (setq vm-system-state 'reading)
	       ;; turn the clipping unwind into a noop
	       (setq clip-head (point-min))
	       (setq clip-tail (point-max)))
      (narrow-to-region clip-head clip-tail)
      (select-window old-w))))
