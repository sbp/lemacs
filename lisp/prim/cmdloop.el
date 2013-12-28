;;; cmdloop.el
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
 
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

;; Written by Richard Mlynarik 8-Jul-92

(defun recursion-depth ()
  "Return the current depth in recursive edits."
  (+ command-loop-level (minibuffer-depth)))

(defun top-level ()
  "Exit all recursive editing levels."
  (interactive)
  (throw 'top-level nil))

(defun exit-recursive-edit ()
  "Exit from the innermost recursive edit or minibuffer."
  (interactive)
  (if (> (recursion-depth) 0)
      (throw 'exit nil))
  (error (gettext "No recursive edit is in progress")))

(defun abort-recursive-edit ()
  "Abort the command that requested this recursive edit or minibuffer input."
  (interactive)
  (if (> (recursion-depth) 0)
      (throw 'exit t))
  (error (gettext "No recursive edit is in progress")))

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

;;>>> This should really be a ring of last errors.
;;>>> Better, there should -really- be a ring of last-echo-area-messages,
;;>>>  so that users can look back at ones they missed!!!!
(defvar last-error nil
  ">>>Doc.")

(defun command-error (error-object)
  (let ((inhibit-quit t)
	(debug-on-error nil))
    (setq quit-flag nil)
    (setq standard-output t)
    (setq standard-input t)
    (setq executing-macro nil)
    (zmacs-deactivate-region)
    (discard-input)

    (setq last-error error-object)

    (message nil)
    (ding nil 'command-error)
    (display-error error-object t)

    (if (noninteractive)
        (progn
          (message (gettext "Emacs exiting."))
          (kill-emacs -1)))
    t))

(defun describe-last-error ()
  "Redisplay the last error-message.  See the variable `last-error'."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Last error was:\n" standard-output)
    (display-error last-error standard-output)))

;;>>> Must be done later in the loadup sequence
;(define-key (symbol-function 'help-command) "e" 'describe-last-error)


(defun truncate-command-history-for-gc ()
  (let ((tail (nthcdr 30 command-history)))
    (if tail (setcdr tail nil)))
  (let ((tail (nthcdr 30 values)))
    (if tail (setcdr tail nil)))
  )

(add-hook 'pre-gc-hook 'truncate-command-history-for-gc)


;;;; Object-oriented programming at its finest

(defun display-error (error-object stream) ;(defgeneric report-condition ...)
  "Display `error-object' on `stream' in a user-friendly way."
  (funcall (or (let ((type (car-safe error-object)))
                 (catch 'error
                   (and (consp error-object)
                        (symbolp type)
                        ;;(stringp (get type 'error-message))
			(consp (get type 'error-conditions))
                        (let ((tail (cdr error-object)))
                          (while (not (null tail))
                            (if (consp tail)
                                (setq tail (cdr tail))
                                (throw 'error nil)))
                          t)
                        ;; (check-type condition condition)
                        (get type 'error-conditions)
                        ;; Search class hierarchy
                        (let ((tail (get type 'error-conditions)))
                          (while (not (null tail))
                            (cond ((not (and (consp tail)
                                             (symbolp (car tail))))
                                   (throw 'error nil))
                                  ((get (car tail) 'display-error)
                                   (throw 'error (get (car tail)
                                                      'display-error)))
                                  (t
                                   (setq tail (cdr tail)))))
                          ;; Default method
                          #'(lambda (error-object stream)
                              (let ((type (car error-object))
                                    (tail (cdr error-object))
                                    (first t))
                                (if (eq type 'error)
                                    (progn (princ (car tail) stream)
                                           (setq tail (cdr tail)))
				  (princ (or (gettext (get type 'error-message)) type)
					 stream))
                                (while tail
                                  (princ (if first ": " ", ") stream)
                                  (prin1 (car tail) stream)
                                  (setq tail (cdr tail)
                                        first nil))))))))
	       #'(lambda (error-object stream)
                   (princ (gettext "Peculiar error ") stream)
                   (prin1 error-object stream)))
           error-object stream))

(put 'file-error 'display-error
     #'(lambda (error-object stream)
         (let ((tail (cdr error-object))
               (first t))
           (princ (car tail) stream)
           (while (setq tail (cdr tail))
             (princ (if first ": " ", ") stream)
             (princ (car tail) stream)
             (setq first nil)))))

(put 'undefined-keystroke-sequence 'display-error
     #'(lambda (error-object stream)
         (princ (key-description (car (cdr error-object))) stream)
         (princ (gettext " not defined.") stream) ; doo dah, doo dah.
         ))


(defvar teach-extended-commands-p nil
  "*If true, then `\\[execute-extended-command]' will teach you keybindings.
Any time you execute a command with \\[execute-extended-command] which has a\
 shorter keybinding,
you will be shown the alternate binding before the command executes.")

(defun execute-extended-command (prefix-arg)
  (interactive "P")
  ;; Note:  This doesn't hack "this-command-keys"
  (let ((prefix-arg prefix-arg))
    (setq this-command (read-command
                        ;; Note: this has the hard-wired
                        ;;  "C-u" and "M-x" string bug in common
                        ;;  with all GNU Emacs's.
                        (cond ((eq prefix-arg '-)
                               "- M-x ")
                              ((equal prefix-arg '(4))
                               "C-u M-x ")
                              ((integerp prefix-arg)
                               (format "%d M-x " prefix-arg))
                              ((and (consp prefix-arg)
                                    (integerp (car prefix-arg)))
                               (format "%d M-x " (car prefix-arg)))
                              (t
                               "M-x ")))))

  (if (and teach-extended-commands-p (interactive-p))
      (let ((keys (where-is-internal this-command (current-local-map))))
	(if keys
	    (progn
	      (message "M-x %s (bound to key%s: %s)"
		       this-command
		       (if (cdr keys) "s" "")
		       (mapconcat 'key-description
				  (sort keys #'(lambda (x y)
						 (< (length x) (length y))))
				  ", "))
	      (sit-for 2)))))

  (command-execute this-command t))


;;; C code calls this; the underscores in the variable names are to avoid
;;; cluttering the specbind namespace (lexical scope!  lexical scope!)
;;; Putting this in Lisp instead of C slows kbd macros by 50%.
;(defun command-execute (_command &optional _record-flag)
;  "Execute CMD as an editor command.
;CMD must be a symbol that satisfies the `commandp' predicate.
;Optional second arg RECORD-FLAG non-nil
;means unconditionally put this command in `command-history'.
;Otherwise, that is done only if an arg is read using the minibuffer."
;  (let ((_prefix prefix-arg)
;        (_cmd (indirect-function _command)))
;    (setq prefix-arg nil
;          this-command _command
;          current-prefix-arg _prefix
;          zmacs-region-stays nil)
;    ;; >>> debug_on_next_call = 0;
;    (cond ((and (symbolp _command)
;                (get _command 'disabled))
;           (run-hooks disabled-command-hook))
;          ((or (stringp _cmd) (vectorp _cmd))
;           ;; If requested, place the macro in the command history.  
;           ;;  For other sorts of commands, call-interactively takes
;           ;;  care of this. 
;           (if _record-flag
;               (setq command-history
;                     (cons (list 'execute-kbd-macro _cmd _prefix)
;                           command-history)))
;             (execute-kbd-macro _cmd _prefix))
;            (t
;             (call-interactively _command _record-flag)))))

(defun y-or-n-p-minibuf (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no."
  (let* ((prompt (format "%s(y or n) " prompt))
         (p prompt)
	 event)
    (while (stringp p)
      (if (let ((cursor-in-echo-area t)
                (inhibit-quit t))
            (message "%s" p)
            (setq event (next-command-event event))
            (prog1 quit-flag (setq quit-flag nil)))
          (progn
            (message "%s%s" p (single-key-description event))
            (setq quit-flag nil)
            (signal 'quit '())))
      (let* ((key (and (key-press-event-p event) (event-key event)))
             (char (and key (event-to-character event))))
        (if char (setq char (downcase char)))
        (cond ((or (eq char ?y) (eq char ? ))
               (message "%sYes" p)
               (setq p t))
              ((or (eq char ?n) (eq key 'delete))
               (message "%sNo" p)
               (setq p nil))
	      ((button-release-event-p event) ; ignore them
	       nil)
              (t
               (message "%s%s" p (single-key-description event))
               (ding nil 'y-or-n-p)
               (discard-input)
               (if (eq p prompt)
                   (setq p (concat (gettext "Please answer y or n.  ")
				   prompt)))))))
    p))

(defun yes-or-no-p-minibuf (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.
Takes one argument, which is the string to display to ask the question.
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
The user must confirm the answer with RET,
and can edit it until it has been confirmed."
  (let ((p (concat prompt (gettext "(yes or no) ")))
	(ans ""))
    (while (stringp ans)
      (setq ans (downcase (read-string p nil t))) ;no history
      (cond ((string-equal ans (gettext "yes"))
             (setq ans 't))
            ((string-equal ans (gettext "no"))
             (setq ans 'nil))
            (t
             (ding nil 'yes-or-no-p)
             (discard-input)
             (message (gettext "Please answer yes or no."))
             (sleep-for 2))))
    ans))

;; these may be redefined later, but make the original def easily encapsulable
(define-function 'yes-or-no-p 'yes-or-no-p-minibuf)
(define-function 'y-or-n-p 'y-or-n-p-minibuf)


(defun read-char ()
  "Read a character from the command input (keyboard or macro).
If a mouse click is detected, an error is signalled.  The character typed
is returned as an ASCII value.  This is most likely the wrong thing for you
to be using: consider using the `next-command-event' function instead."
  (let ((inhibit-quit t)
        (event (next-command-event)))
    (prog1 (or (event-to-character event)
	       ;; Kludge.  If the event we read was a mouse-release, discard it
	       ;; and read the next one.
	       (if (button-release-event-p event)
		   (event-to-character (next-command-event event)))
	       (error (gettext "Key read has no ASCII equivalent %S") event))
      ;; this is not necessary, but is marginally more efficient than GC.
      (deallocate-event event))))

(defun read-quoted-char (&optional prompt)
  "Like `read-char', except that if the first character read is an octal
digit, we read up to two more octal digits and return the character
represented by the octal number consisting of those digits.
Optional argument PROMPT specifies a string to use to prompt the user."
  (let ((count 0) (code 0) char event)
    (while (< count 3)
      (let ((inhibit-quit (zerop count))
	    (help-form nil))
	(and prompt (message (gettext "%s-") prompt))
	(setq event (next-command-event)
	      char (or (event-to-character event nil nil t)
		       (error
			(gettext "key read cannot be inserted in a buffer: %S")
			event)))
	(if inhibit-quit (setq quit-flag nil)))
      (cond ((null char))
	    ((and (<= ?0 char) (<= char ?7))
	     (setq code (+ (* code 8) (- char ?0))
		   count (1+ count))
	     (and prompt (message (setq prompt
					(format "%s %c" prompt char)))))
	    ((> count 0)
	     (setq unread-command-event event
		   count 259))
	    (t (setq code char count 259))))
    (logand 255 code)))

(defun redraw-mode-line (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL then force then force redisplay of all mode-lines."
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))
(define-function 'force-mode-line-update 'redraw-mode-line)

(defun momentary-string-display (string pos &optional exit-char message) 
  "Momentarily display STRING in the buffer at POS.
Display remains until next character is typed.
If the char is EXIT-CHAR (optional third arg, default is SPC) it is swallowed;
otherwise it is then available as input (as a command if nothing else).
Display MESSAGE (optional fourth arg) in the echo area.
If MESSAGE is nil, instructions to type EXIT-CHAR are displayed there."
  (or exit-char (setq exit-char ?\ ))
  (let ((buffer-read-only nil)
	(modified (buffer-modified-p))
	(name buffer-file-name)
	insert-end)
    (unwind-protect
	(progn
	  (save-excursion
	    (goto-char pos)
	    ;; defeat file locking... don't try this at home, kids!
	    (setq buffer-file-name nil)
	    (insert-before-markers string)
	    (setq insert-end (point))
	    ;; If the message end is off screen, recenter now.
	    (if (> (window-end) insert-end)
		(recenter (/ (window-height) 2)))
	    ;; If that pushed message start off the screen,
	    ;; scroll to start it at the top of the screen.
	    (move-to-window-line 0)
	    (if (> (point) pos)
		(progn
		  (goto-char pos)
		  (recenter 0))))
	  (message (or message (gettext "Type %s to continue editing."))
		   (single-key-description exit-char))
	  (let ((event (next-command-event)))
	    (or (eq (event-to-character event) exit-char)
		(setq unread-command-event event))))
      (if insert-end
	  (save-excursion
	    (delete-region pos insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))
