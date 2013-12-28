;;; Interval timers for GNU Emacs
;;; Copyright (C) 1988, 1991 Kyle E. Jones
;;;
;;; Verbatim copies of this file may be freely redistributed.
;;;
;;; Modified versions of this file may be redistributed provided that this
;;; notice remains unchanged, the file contains prominent notice of
;;; author and time of modifications, and redistribution of the file
;;; is not further restricted in any way.
;;;
;;; This file is distributed `as is', without warranties of any kind.

(provide 'timer)

;; `timer' feature means Emacs-Lisp programers get:
;;    timerp, timer-value, timer-restart, timer-function,
;;    set-timer-value, set-timer-restart, set-timer-function
;;    get-timer, start-timer, read-timer, delete-timer
;;
;; Interactive users get these commands:
;;    edit-timers, list-timers, start-timer
;;
;; See the doc strings of these functions for more information.

(defvar timer-list nil
  "List of all active timers.")

(defvar timer-process nil
  "Process that drives all timers.")

;; This value is maintained internally; it does not determine timer
;; granularity.  Timer granularity is 1 second, plus delays due to
;; system and Emacs internal activity that delay dealing with process
;; output.
(defvar timer-process-next-wakeup 1
  "Timer process will wakeup to service running timers within this
many seconds.")

(defvar timer-edit-map nil
  "Keymap used when in Timer Edit mode.")

(if timer-edit-map
    ()
  (setq timer-edit-map (make-sparse-keymap))
  (define-key timer-edit-map "s" 'timer-edit-set-field)
  (define-key timer-edit-map "d" 'timer-edit-delete-timer)
  (define-key timer-edit-map "q" 'timer-edit-quit)
  (define-key timer-edit-map "\t" 'timer-edit-next-field)
  (define-key timer-edit-map " " 'next-line)
  (define-key timer-edit-map "n" 'next-line)
  (define-key timer-edit-map "p" 'previous-line)
  (define-key timer-edit-map "\C-?" 'timer-edit-previous-field)
  (define-key timer-edit-map "x" 'start-timer)
  (define-key timer-edit-map "?" 'timer-edit-help))
  
(defvar timer-edit-start-marker nil)

;; macros must come first... or byte-compile'd code will throw back its
;; head and scream.

(defmacro decrement (variable)
  (list 'setq variable (list '1- variable)))

(defmacro increment (variable)
  (list 'setq variable (list '1+ variable)))

(defmacro signum (n)
  (list 'if (list '> n 0) 1
    (list 'if (list 'zerop n) 0 -1)))

;; Timer access functions should behave as if they were subrs.  These
;; macros are used to check the arguments to the timer functions and
;; signal errors appropriately if the arguments are not valid.

(defmacro check-timer (var)
  "If VAR is not bound to a timer, signal wrong-type-argument.
This is a macro."
  (list 'setq var
	(list 'if (list 'timerp var) var
	      (list 'signal ''wrong-type-argument
		    (list 'list ''timerp var)))))

(defmacro check-timer-coerce-string (var)
  "If VAR is not bound to a string, look up the timer that it names and
bind VAR to it.  Otherwise if VAR is not bound to a timer, signal
wrong-type-argument.  This is a macro."
  (list 'setq var
	(list 'cond
	      (list (list 'timerp var) var)
	      (list (list 'stringp var) (list 'get-timer var))
	      (list t (list 'signal ''wrong-type-argument
			    (list 'list ''string-or-timer-p var))))))

(defmacro check-natnumber (var)
  "If VAR is not bound to a non-negative number, signal wrong-type-argument.
This is a macro."
  (list 'setq var
	(list 'if (list 'natnump var) var
	      (list 'signal ''wrong-type-argument
		    (list 'list ''natnump var)))))

(defmacro check-string (var)
  "If VAR is not bound to a string, signal wrong-type-argument.
This is a macro."
  (list 'setq var
	(list 'if (list 'stringp var) var
	      (list 'signal ''wrong-type-argument
		    (list 'list ''stringp var)))))

;; Functions to access and modify timer attributes.

(defun timerp (obj)
  "Returns non-nil iff OBJ is a timer."
  (and (consp obj) (stringp (car obj)) (eq (length obj) 4)))

(defun timer-name (timer)
  "Returns the name of TIMER."
  (check-timer timer)
  (car timer))

(defun timer-value (timer)
  "Returns the number of seconds until TIMER expires."
  (check-timer timer)
  (nth 1 timer))

(defun timer-restart (timer)
  "Returns the value to which TIMER will be set at restart.
nil is returned if this timer doesn't restart."
  (check-timer timer)
  (nth 2 timer))

(defun timer-function (timer)
  "Returns the function of TIMER.
This function is called each time TIMER expires."
  (check-timer timer)
  (nth 3 timer))

(defun set-timer-value (timer value &optional nowakeup)
  "Set the timeout value of TIMER to be VALUE.
Timer will expire is this many seconds.
Returns VALUE."
;; Optional third arg NOWAKEUP non-nil means do not wakeup the timer
;; process to recompute a correct wakeup time, even if it means this
;; timer will expire late.  timer-process-filter uses this option.
;; This is not meant for ordinary usage, which is why it is not
;; mentioned in the doc string.
  (check-timer timer)
  (check-natnumber value)
  (let ((inhibit-quit t))
    ;; If we're allowed to wakeup the timer process,
    ;; and the timer process's next wakeup needs to be recomputed,
    ;; and the timer is running, then we wakeup the timer process.
    (or (and (not nowakeup) (< value timer-process-next-wakeup)
	     (get-timer (timer-name timer))
	     (progn (timer-process-wakeup)
		    (setcar (cdr timer) value)
		    (timer-process-wakeup)))
	(setcar (cdr timer) value))
    value))

(defun set-timer-restart (timer restart)
  "Set the restart value of TIMER to be RESTART.
If RESTART is nil, TIMER is will not restart when it expires.
Returns RESTART."
  (check-timer timer)
  (if restart (check-natnumber restart))
  (and restart (< restart 1) (signal 'args-out-of-range (list restart)))
  (setcar (cdr (cdr timer)) restart))

(defun set-timer-function (timer function)
  "Set the function of TIMER to be FUNCTION.
FUNCTION will be called when timer expires.
Returns FUNCTION."
  (check-timer timer)
  (setcar (cdr (cdr (cdr timer))) function))

(defun get-timer (name)
  "Return timer named NAME, or nil if there is none."
  (check-string name)
  (assoc name timer-list))

(defun read-timer (prompt &optional initial-input)
  "Read the name of a timer from the minibuffer and return the timer
associated with that name.  The user is prompted with PROMPT.
Optional second arg INITIAL-INPUT non-nil is inserted into the
  minibuffer as initial user input."
  (get-timer (completing-read prompt timer-list nil 'confirm initial-input)))

(defun delete-timer (timer)
  "Deletes TIMER.  TIMER may be a timer or the name of one."
  (check-timer-coerce-string timer)
  (setq timer-list (delq timer timer-list)))

(defun start-timer (name function value &optional restart)
  "Start a timer.
Args are NAME, FUNCTION, VALUE &optional RESTART.
NAME is an identifier for the timer.  It must be a string.  If a timer
  already exists with this name, NAME will be modified slightly to until
  it is unique.
FUNCTION should be a function (or symbol naming one) of no arguments.  It
  will be called each time the timer expires.  The function can access
  timer that invoked it through the variable `current-timer'.
VALUE is the number of seconds until this timer expires.
Optional fourth arg RESTART non-nil means that this timer should be
  restarted automatically after its function is called.  Normally a timer
  is deleted at expiration after its function has returned. 
  If non-nil RESTART should be a number indicating the value at which the
  timer should be set at restart time.
Returns the newly created timer."
  (interactive
   (list (completing-read "Start timer: " timer-list)
	 (read (completing-read "Timer function: " obarray 'fboundp))
	 (let (value)
	   (while (not (natnump value))
	     (setq value (read-from-minibuffer "Timer value: " nil nil t)))
	   value)
	 (let ((restart t))
	   (while (and restart (not (natnump restart)))
	     (setq restart (read-from-minibuffer "Timer restart: " nil nil t)))
	   restart)))
  (check-string name)
  (check-natnumber value)
  (if restart (check-natnumber restart))
  ;; Make proposed timer name unique if it's not already.
  (let ((oname name)
	(num 2))
    (while (get-timer name)
      (setq name (concat oname "<" num ">"))
      (increment num)))
  ;; If there's no timer process, start one now.
  ;; Otherwise wake up the timer process so that seconds slept before
  ;; the new timer is created won't be counted against it.
  (if timer-process
      (timer-process-wakeup)
    (timer-process-start))
  (let ((inhibit-quit t))
    ;; add the timer to the global list
    (setq timer-list
	  (cons (list name value restart function)
		timer-list))
    ;; If the timer process is scheduled to wake up too late for the timer
    ;; we wake it up to calculate a correct wakeup value giving consideration
    ;; to the newly added timer.
    (if (< value timer-process-next-wakeup)
	(timer-process-wakeup)))
  (car timer-list))

;; User level functions to list and modify existing timers.
;; Timer Edit major mode, and the editing commands thereof.

(defun list-timers ()
  "Pop up a buffer containing a list of all timers.
The major mode of the buffer is Timer Edit mode.  This major mode provides
commands to manipulate timers; see the documentation for
`timer-edit-mode' for more information."
  (interactive)
  (let* ((buf (get-buffer-create "*Timer List*"))
	 (opoint (point))
	 (standard-output buf)
	 (timers (reverse timer-list)))
    (set-buffer buf)
    (timer-edit-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Name                  Value     Restart   Function\n"
	    "----                  -----     -------   --------")
    (if (null timer-edit-start-marker)
	(setq timer-edit-start-marker (point)))
    (while timers
      (newline 1)
      (prin1 (timer-name (car timers)))
      (tab-to-tab-stop)
      (prin1 (timer-value (car timers)))
      (tab-to-tab-stop)
      (prin1 (timer-restart (car timers)))
      (tab-to-tab-stop)
      (prin1 (timer-function (car timers)))
      (setq timers (cdr timers)))
    ;; restore point
    (goto-char opoint)
    (if (< (point) timer-edit-start-marker)
	(goto-char timer-edit-start-marker))
    (setq buffer-read-only t)
    (display-buffer buf)))

(defun edit-timers ()
  "Display a list of all timers and select it for editing.
The major mode of the buffer containing the listing is Timer Edit mode.
This major mode provides commands to manipulate timers; see the documentation
for `timer-edit-mode' for more information."
  (interactive)
  ;; since user is editing, make sure displayed data is reasonably up-to-date
  (if timer-process
      (timer-process-wakeup))
  (list-timers)
  (select-window (get-buffer-window "*Timer List*"))
  (goto-char timer-edit-start-marker)
  (if timer-list
      (progn
	(forward-sexp 2)
	(backward-sexp)))
  (message "type q to quit, ? for help"))

;; no point in making this interactive.
(defun timer-edit-mode ()
  "Major mode for manipulating timers.
Atrributes of running timers are changed by moving the cursor to the
desired field and typing `s' to set that field.  The field will then be
set to the value read from the minibuffer.

Commands:
TAB    move forward a field
DEL    move backward a field
s      set a field
d      delete the selected timer
x      start a new timer
?      help"
  (kill-all-local-variables)
  (make-local-variable 'tab-stop-list)
  (setq major-mode 'timer-edit-mode
	mode-name "Timer Edit"
	truncate-lines t
	tab-stop-list '(22 32 42))
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (buffer-flush-undo (current-buffer))
  (use-local-map timer-edit-map)
  (set-syntax-table lisp-mode-syntax-table))

(put 'timer-edit-mode 'mode-class 'special)

(defun timer-edit-help ()
  "Help function for Timer Edit."
  (interactive)
  (if (eq last-command 'timer-edit-help)
      (describe-mode)
    (message "TAB, DEL select fields, (s)et field, (d)elete timer   (type ? for more help)")))

(defun timer-edit-quit ()
  "End Timer Edit."
  (interactive)
  (bury-buffer (current-buffer))
  (if (one-window-p t)
      (switch-to-buffer (other-buffer (current-buffer)))
    (delete-window)))

(defun timer-edit-set-field ()
  (interactive)
  ;; First two lines in list buffer are headers.
  ;; Cry out against the luser who attempts to change a field there.
  (if (<= (point) timer-edit-start-marker)
      (error ""))
  ;; field-value must be initialized to be something other than a
  ;; number, symbol, or list.
  (let (timer field (field-value ""))
    (setq timer (save-excursion
		  ;; read the name of the timer from the beginning of
		  ;; the current line.
		  (beginning-of-line)
		  (get-timer (read (current-buffer))))
	  field (save-excursion
		  (timer-edit-beginning-of-field)
		  (let ((opoint (point))
			(n 0))
		    ;; count the number of sexprs until we reach the cursor
		    ;; and use this info to determine which field the user
		    ;; wants to modify.
		    (beginning-of-line)
		    (while (and (>= opoint (point)) (< n 4))
		      (forward-sexp 2)
		      (backward-sexp)
		      (increment n))
		    (cond ((eq n 1) (error "Cannot change timer name."))
			  ((eq n 2) 'value)
			  ((eq n 3) 'restart)
			  ((eq n 4) 'function)))))
    (cond ((eq field 'value)
	   (let ((prompt "Set timer value: "))
	     (while (not (natnump field-value))
	       (setq field-value (read-from-minibuffer prompt nil nil t)))))
	  ((eq field 'restart)
	   (let ((prompt "Set timer restart: "))
	     (while (and field-value (not (natnump field-value)))
	       (setq field-value (read-from-minibuffer prompt nil nil t)))))
	  ((eq field 'function)
	   (let ((prompt "Set timer function: "))
	     (while (not (or (and (symbolp field-value) (fboundp field-value))
			     (and (consp field-value)
				  (memq (car field-value) '(lambda macro)))))
	       (setq field-value
		     (read (completing-read prompt obarray 'fboundp nil)))))))
    ;; set the timer field
    (funcall (intern (concat "set-timer-" (symbol-name field)))
	     timer field-value)
    ;; move to beginning of field to be changed
    (timer-edit-beginning-of-field)
    ;; modify the list buffer to reflect the change.
    (let (buffer-read-only kill-ring)
      (kill-sexp 1)
      (kill-region (point) (progn (skip-chars-forward " \t") (point)))
      (prin1 field-value (current-buffer))
      (if (not (eolp))
	  (tab-to-tab-stop))
      (backward-sexp))))

(defun timer-edit-delete-timer ()
  (interactive)
  ;; First two lines in list buffer are headers.
  ;; Cry out against the luser who attempts to change a field there.
  (if (<= (point) timer-edit-start-marker)
      (error ""))
  (delete-timer
   (read-timer "Delete timer: "
	       (save-excursion (beginning-of-line) (read (current-buffer)))))
  ;; update list information
  (list-timers))

(defun timer-edit-next-field (count)
  (interactive "p")
  (timer-edit-beginning-of-field)
  (cond ((> (signum count) 0)
	 (while (not (zerop count))
	   (forward-sexp)
	   ;; wrap from eob to timer-edit-start-marker
	   (if (eobp)
	       (progn
		 (goto-char timer-edit-start-marker)
		 (forward-sexp)))
	   (forward-sexp)
	   (backward-sexp)
	   ;; treat fields at beginning of line as if they weren't there.
	   (if (bolp)
	       (progn
		 (forward-sexp 2)
		 (backward-sexp)))
	   (decrement count)))
	((< (signum count) 0)
	 (while (not (zerop count))
	   (backward-sexp)
	   ;; treat fields at beginning of line as if they weren't there.
	   (if (bolp)
	       (backward-sexp))
	   ;; wrap from timer-edit-start-marker to field at eob.
	   (if (<= (point) timer-edit-start-marker)
	       (progn
		 (goto-char (point-max))
		 (backward-sexp)))
	   (increment count)))))

(defun timer-edit-previous-field (count)
  (interactive "p")
  (timer-edit-next-field (- count)))

(defun timer-edit-beginning-of-field ()
  (let ((forw-back (save-excursion (forward-sexp) (backward-sexp) (point)))
	(back (save-excursion (backward-sexp) (point))))
    (cond ((eq forw-back back) (backward-sexp))
	  ((eq forw-back (point)) t)
	  (t (backward-sexp)))))


;; internals of the timer implementation.

(defun timer-process-filter (process string)
  ;; If the timer process dies and generates output while doing
  ;; so, we may be called before the process-sentinel.  Sanity
  ;; check the output just in case...
  (if (not (string-match "^[0-9]" string))
      (message "timer process gave odd output: %s" string)
    ;; if there are no active timers, return quickly.
    (if timer-list
	(let ((time-elapsed (string-to-int string))
	      (timers timer-list)
	      (timer)
	      ;; process filters can be hit by stray C-g's from the user,
	      ;; so we must protect this stuff appropriately.
	      ;; Quit's are allowed from within timer functions, but we
	      ;; catch them.
	      (inhibit-quit t))
	  (setq timer-process-next-wakeup 600)
	  (while timers
	    (setq timer (car timers))
	    (set-timer-value timer (max 0 (- (timer-value timer) time-elapsed)) t)
	    (if (> (timer-value timer) 0)
		(setq timer-process-next-wakeup
		      (min timer-process-next-wakeup (timer-value timer)))
	      ;; timer has expired, we must call its function.
	      ;; protect our local vars from the timer function.
	      ;; allow keyboard quit to occur, but catch and report it.
	      ;; provide the variable `current-timer' in case the function
	      ;; is interested.
	      (condition-case condition-data
		  (let* ((current-timer timer)
			 timer timers time-elapsed
			 quit-flag inhibit-quit)
		    (funcall (timer-function current-timer)))
		(error (message "timer \"%s\" signaled: %s" (timer-name timer)
				(prin1-to-string condition-data)))
		(quit (message "timer \"%s\" quit" (timer-name timer))))
	      ;; restart the timer if we should, otherwise delete it.
	      (if (null (timer-restart timer))
		  (delete-timer timer)
		(set-timer-value timer (timer-restart timer) t)
		(setq timer-process-next-wakeup
		      (min timer-process-next-wakeup (timer-value timer)))))
	    (setq timers (cdr timers)))
	  ;; if user is editing timers, update displayed info
	  (if (eq major-mode 'timer-edit-mode)
	      (list-timers)))
      (setq timer-process-next-wakeup 600))
    ;; tell timer-process when to wakeup again
    (process-send-string timer-process
			 (concat (int-to-string timer-process-next-wakeup)
				 "\n"))))

(defun timer-process-sentinel (process message)
  (let ((inhibit-quit t))
    (if (eq (process-status process) 'stop)
	(continue-process process)
      ;; not stopped, so it must have died.
      ;; cleanup first...
      (delete-process process)
      (setq timer-process nil)
      ;; now, if there are any active timers then we need to immediately
      ;; start another timer process, otherwise we can wait until the next
      ;; start-timer call,  which will start one automatically.
      (if (null timer-list)
	  ()
	;; there may have been an error message in the echo area;
	;; give the user at least a little time to read it.
	(sit-for 2)
	(message "timer process %s... respawning." (substring message 0 -1))
	(timer-process-start)))))

(defun timer-process-start ()
  (let ((inhibit-quit t)
	(process-connection-type nil))
    (setq timer-process (start-process "timer" nil "timer"))
    (process-kill-without-query timer-process)
    (set-process-filter timer-process 'timer-process-filter)
    (set-process-sentinel timer-process 'timer-process-sentinel)
    ;; Tell timer process to wake up quickly, so that a correct wakeup
    ;; time can be computed.  Zero instead of one here loses because of
    ;; underlying timer implementations that use 0 to mean `disable the
    ;; timer'.
    (setq timer-process-next-wakeup 1)
    (process-send-string timer-process "1\n")))

(defun timer-process-wakeup ()
  (interrupt-process timer-process)
  (accept-process-output))
