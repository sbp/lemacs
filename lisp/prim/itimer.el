;;; Interval timers for GNU Emacs
;;; Copyright (C) 1988, 1991, 1993 Kyle E. Jones
;;; Modified 5 Feb 91 by Jamie Zawinski <jwz@lucid.com> for Lucid Emacs
;;; And again, 15 Dec 93.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@uunet.uu.net.

;; The original v18 version of this file worked by having an external program
;; wake up once a second to generate an interrupt for emacs; then an emacs
;; process filter was used to schedule timers. 
;;
;; This version works by associating with each timer a "timeout" object,
;; since the Lucid Emacs event loop has the concept of timers built in to
;; it.  There is no single scheduler function; instead, each timer re-sets
;; itself as it is invoked.

;; `itimer' feature means Emacs-Lisp programers get:
;;    itimerp, itimer-value, itimer-restart, itimer-function,
;;    set-itimer-value, set-itimer-restart, set-itimer-function
;;    get-itimer, start-itimer, read-itimer, delete-itimer
;;
;; Interactive users get these commands:
;;    edit-itimers, list-itimers, start-itimer
;;
;; See the doc strings of these functions for more information.

(defvar itimer-version "1.00"
  "Version number of the itimer package.")

(defvar itimer-list nil
  "List of all active itimers.")

;; not needed in lemacs
;(defvar itimer-process nil
;  "Process that drives all itimers.")

;; This value is maintained internally; it does not determine itimer
;; granularity.  Itimer granularity is 1 second, plus delays due to
;; system and Emacs internal activity that delay dealing with process
;; output.
;; not needed in lemacs
;(defvar itimer-process-next-wakeup 1
;  "Itimer process will wakeup to service running itimers within this
;many seconds.")

(defvar itimer-edit-map nil
  "Keymap used when in Itimer Edit mode.")

(if itimer-edit-map
    ()
  (setq itimer-edit-map (make-sparse-keymap))
  (define-key itimer-edit-map "s" 'itimer-edit-set-field)
  (define-key itimer-edit-map "d" 'itimer-edit-delete-itimer)
  (define-key itimer-edit-map "q" 'itimer-edit-quit)
  (define-key itimer-edit-map "\t" 'itimer-edit-next-field)
  (define-key itimer-edit-map " " 'next-line)
  (define-key itimer-edit-map "n" 'next-line)
  (define-key itimer-edit-map "p" 'previous-line)
  (define-key itimer-edit-map "\C-?" 'itimer-edit-previous-field)
  (define-key itimer-edit-map "x" 'start-itimer)
  (define-key itimer-edit-map "?" 'itimer-edit-help))
  
(defvar itimer-edit-start-marker nil)

;; macros must come first... or byte-compile'd code will throw back its
;; head and scream.

(defmacro itimer-decf (variable)
  (list 'setq variable (list '1- variable)))

(defmacro itimer-incf (variable)
  (list 'setq variable (list '1+ variable)))

(defmacro itimer-signum (n)
  (list 'if (list '> n 0) 1
    (list 'if (list 'zerop n) 0 -1)))

;; Itimer access functions should behave as if they were subrs.  These
;; macros are used to check the arguments to the itimer functions and
;; signal errors appropriately if the arguments are not valid.

(defmacro check-itimer (var)
  "If VAR is not bound to an itimer, signal wrong-type-argument.
This is a macro."
  (list 'setq var
	(list 'if (list 'itimerp var) var
	      (list 'signal ''wrong-type-argument
		    (list 'list ''itimerp var)))))

(defmacro check-itimer-coerce-string (var)
  "If VAR is not bound to a string, look up the itimer that it names and
bind VAR to it.  Otherwise if VAR is not bound to an itimer, signal
wrong-type-argument.  This is a macro."
  (list 'setq var
	(list 'cond
	      (list (list 'itimerp var) var)
	      (list (list 'stringp var) (list 'get-itimer var))
	      (list t (list 'signal ''wrong-type-argument
			    (list 'list ''string-or-itimer-p var))))))

(defmacro itimer-check-natnum (var)
  "If VAR is not bound to a non-negative number, signal wrong-type-argument.
This is a macro."
  (list 'setq var
	(list 'if (list 'natnump var) var
	      (list 'signal ''wrong-type-argument
		    (list 'list ''natnump var)))))

(defmacro itimer-check-string (var)
  "If VAR is not bound to a string, signal wrong-type-argument.
This is a macro."
  (list 'setq var
	(list 'if (list 'stringp var) var
	      (list 'signal ''wrong-type-argument
		    (list 'list ''stringp var)))))

;; Functions to access and modify itimer attributes.

(defun itimerp (obj)
  "Returns non-nil iff OBJ is an itimer."
  (and (consp obj) (stringp (car obj)) (eq (length obj)
					   5 ; for lemacs
					   ;4 ; original version
					   )))

(defun itimer-name (itimer)
  "Returns the name of ITIMER."
  (check-itimer itimer)
  (car itimer))

(defun itimer-value (itimer)
  "Returns the number of seconds until ITIMER expires."
  (check-itimer itimer)
  (nth 1 itimer))

(defun itimer-restart (itimer)
  "Returns the value to which ITIMER will be set at restart.
nil is returned if this itimer doesn't restart."
  (check-itimer itimer)
  (nth 2 itimer))

(defun itimer-function (itimer)
  "Returns the function of ITIMER.
This function is called each time ITIMER expires."
  (check-itimer itimer)
  (nth 3 itimer))

;; lemacs-specific
(defun itimer-id (itimer)
  "Returns the timeout-id of ITIMER."
  (check-itimer itimer)
  (nth 4 itimer))

(defun set-itimer-value (itimer value
				;; lemacs doesn't need this
				;; &optional nowakeup
				)
  "Set the timeout value of ITIMER to be VALUE.
Itimer will expire is this many seconds.
Returns VALUE."
;; Optional third arg NOWAKEUP non-nil means do not wakeup the itimer
;; process to recompute a correct wakeup time, even if it means this
;; itimer will expire late.  itimer-process-filter uses this option.
;; This is not meant for ordinary usage, which is why it is not
;; mentioned in the doc string.
  (check-itimer itimer)
  (itimer-check-natnum value)
  (let ((inhibit-quit t))

;    ;; If we're allowed to wakeup the itimer process,
;    ;; and the itimer process's next wakeup needs to be recomputed,
;    ;; and the itimer is running, then we wakeup the itimer process.
;    (or (and (not nowakeup) (< value itimer-process-next-wakeup)
;	     (get-itimer (itimer-name itimer))
;	     (progn (itimer-process-wakeup)
;		    (setcar (cdr itimer) value)
;		    (itimer-process-wakeup)))
;	(setcar (cdr itimer) value))

    ;; the lemacs way:
    (if (itimer-id itimer)
	(deactivate-itimer itimer))
    (setcar (cdr itimer) value)
    (activate-itimer itimer)

    value))

(defun set-itimer-restart (itimer restart)
  "Set the restart value of ITIMER to be RESTART.
If RESTART is nil, ITIMER will not restart when it expires.
Returns RESTART."
  (check-itimer itimer)
  (if restart (itimer-check-natnum restart))
  (and restart (< restart 1) (signal 'args-out-of-range (list restart)))
;;  (setcar (cdr (cdr itimer)) restart)
  ;; the lemacs way
  (let ((was-active (itimer-id itimer))
	(inhibit-quit t))
    (if was-active
	(deactivate-itimer itimer))
    (setcar (cdr (cdr itimer)) restart)
    (if was-active
	(progn
	  (setcar (cdr itimer) restart)
	  (if restart
	      (activate-itimer itimer)))))
  restart)

(defun set-itimer-function (itimer function)
  "Set the function of ITIMER to be FUNCTION.
FUNCTION will be called when itimer expires.
Returns FUNCTION."
  (check-itimer itimer)
  (setcar (cdr (cdr (cdr itimer))) function))

;; lemacs-specific
(defun set-itimer-id (itimer id)
  (check-itimer itimer)
  (setcar (cdr (cdr (cdr (cdr itimer)))) id))

(defun get-itimer (name)
  "Return itimer named NAME, or nil if there is none."
  (itimer-check-string name)
  (assoc name itimer-list))

(defun read-itimer (prompt &optional initial-input)
  "Read the name of an itimer from the minibuffer and return the itimer
associated with that name.  The user is prompted with PROMPT.
Optional second arg INITIAL-INPUT non-nil is inserted into the
  minibuffer as initial user input."
  (get-itimer (completing-read prompt itimer-list nil 'confirm initial-input)))

(defun delete-itimer (itimer)
  "Deletes ITIMER.  ITIMER may be an itimer or the name of one."
  (check-itimer-coerce-string itimer)
  (deactivate-itimer itimer)  ;; for lemacs
  (setq itimer-list (delq itimer itimer-list)))

;jwz: this is preloaded so don't ;;;###autoload
(defun start-itimer (name function value &optional restart)
  "Start an itimer.
Args are NAME, FUNCTION, VALUE &optional RESTART.
NAME is an identifier for the itimer.  It must be a string.  If an itimer
  already exists with this name, NAME will be modified slightly to until
  it is unique.
FUNCTION should be a function (or symbol naming one) of no arguments.  It
  will be called each time the itimer expires.  The function can access
  itimer that invoked it through the variable `current-itimer'.
VALUE is the number of seconds until this itimer expires.
Optional fourth arg RESTART non-nil means that this itimer should be
  restarted automatically after its function is called.  Normally an itimer
  is deleted at expiration after its function has returned. 
  If non-nil RESTART should be a number indicating the value at which the
  itimer should be set at restart time.
Returns the newly created itimer."
  (interactive
   (list (completing-read "Start itimer: " itimer-list)
	 (read (completing-read "Itimer function: " obarray 'fboundp))
	 (let (value)
	   (while (not (natnump value))
	     (setq value (read-from-minibuffer "Itimer value: " nil nil t)))
	   value)
	 (let ((restart t))
	   (while (and restart (not (natnump restart)))
	     (setq restart (read-from-minibuffer "Itimer restart: " nil nil t)))
	   restart)))
  (itimer-check-string name)
  (itimer-check-natnum value)
  (if restart (itimer-check-natnum restart))
  ;; Make proposed itimer name unique if it's not already.
  (let ((oname name)
	(num 2))
    (while (get-itimer name)
      (setq name (concat oname "<" num ">"))
      (itimer-incf num)))
;  ;; If there's no itimer process, start one now.
;  ;; Otherwise wake up the itimer process so that seconds slept before
;  ;; the new itimer is created won't be counted against it.
;  (if itimer-process
;      (itimer-process-wakeup)
;    (itimer-process-start))
  (let ((inhibit-quit t))
    ;; add the itimer to the global list
    (setq itimer-list
	  (cons (list name value restart function nil) ; extra slot for lemacs
		itimer-list))
;    ;; If the itimer process is scheduled to wake up too late for the itimer
;    ;; we wake it up to calculate a correct wakeup value giving consideration
;    ;; to the newly added itimer.
;    (if (< value itimer-process-next-wakeup)
;	(itimer-process-wakeup)))
    ;; for lemacs
    (activate-itimer (car itimer-list))
    )
  (car itimer-list))

;; User level functions to list and modify existing itimers.
;; Itimer Edit major mode, and the editing commands thereof.

(defun list-itimers ()
  "Pop up a buffer containing a list of all itimers.
The major mode of the buffer is Itimer Edit mode.  This major mode provides
commands to manipulate itimers; see the documentation for
`itimer-edit-mode' for more information."
  (interactive)
  (let* ((buf (get-buffer-create "*Itimer List*"))
	 (opoint (point))
	 (standard-output buf)
	 (itimers (reverse itimer-list)))
    (set-buffer buf)
    (itimer-edit-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Name                  Value     Restart   Function\n"
	    "----                  -----     -------   --------")
    (if (null itimer-edit-start-marker)
	(setq itimer-edit-start-marker (point)))
    (while itimers
      (newline 1)
      (prin1 (itimer-name (car itimers)))
      (tab-to-tab-stop)
      (prin1 (itimer-value (car itimers)))
      (tab-to-tab-stop)
      (prin1 (itimer-restart (car itimers)))
      (tab-to-tab-stop)
      (prin1 (itimer-function (car itimers)))
      (setq itimers (cdr itimers)))
    ;; restore point
    (goto-char opoint)
    (if (< (point) itimer-edit-start-marker)
	(goto-char itimer-edit-start-marker))
    (setq buffer-read-only t)
    (display-buffer buf)))

(defun edit-itimers ()
  "Display a list of all itimers and select it for editing.
The major mode of the buffer containing the listing is Itimer Edit mode.
This major mode provides commands to manipulate itimers; see the documentation
for `itimer-edit-mode' for more information."
  (interactive)
  ;; since user is editing, make sure displayed data is reasonably up-to-date
;  (if itimer-process
;      (itimer-process-wakeup))
  (list-itimers)
  (select-window (get-buffer-window "*Itimer List*"))
  (goto-char itimer-edit-start-marker)
  (if itimer-list
      (progn
	(forward-sexp 2)
	(backward-sexp)))
  (message "type q to quit, ? for help"))

;; no point in making this interactive.
(defun itimer-edit-mode ()
  "Major mode for manipulating itimers.
Atrributes of running itimers are changed by moving the cursor to the
desired field and typing `s' to set that field.  The field will then be
set to the value read from the minibuffer.

Commands:
TAB    move forward a field
DEL    move backward a field
s      set a field
d      delete the selected itimer
x      start a new itimer
?      help"
  (kill-all-local-variables)
  (make-local-variable 'tab-stop-list)
  (setq major-mode 'itimer-edit-mode
	mode-name "Itimer Edit"
	truncate-lines t
	tab-stop-list '(22 32 42))
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (buffer-disable-undo (current-buffer))
  (use-local-map itimer-edit-map)
  (and lisp-mode-syntax-table (set-syntax-table lisp-mode-syntax-table)))

(put 'itimer-edit-mode 'mode-class 'special)

(defun itimer-edit-help ()
  "Help function for Itimer Edit."
  (interactive)
  (if (eq last-command 'itimer-edit-help)
      (describe-mode)
    (message "TAB, DEL select fields, (s)et field, (d)elete itimer   (type ? for more help)")))

(defun itimer-edit-quit ()
  "End Itimer Edit."
  (interactive)
  (bury-buffer (current-buffer))
  (if (one-window-p t)
      (switch-to-buffer (other-buffer (current-buffer)))
    (delete-window)))

(defun itimer-edit-set-field ()
  (interactive)
  ;; First two lines in list buffer are headers.
  ;; Cry out against the luser who attempts to change a field there.
  (if (<= (point) itimer-edit-start-marker)
      (error ""))
  ;; field-value must be initialized to be something other than a
  ;; number, symbol, or list.
  (let (itimer field (field-value ""))
    (setq itimer (save-excursion
		  ;; read the name of the itimer from the beginning of
		  ;; the current line.
		  (beginning-of-line)
		  (get-itimer (read (current-buffer))))
	  field (save-excursion
		  (itimer-edit-beginning-of-field)
		  (let ((opoint (point))
			(n 0))
		    ;; count the number of sexprs until we reach the cursor
		    ;; and use this info to determine which field the user
		    ;; wants to modify.
		    (beginning-of-line)
		    (while (and (>= opoint (point)) (< n 4))
		      (forward-sexp 2)
		      (backward-sexp)
		      (itimer-incf n))
		    (cond ((eq n 1) (error "Cannot change itimer name."))
			  ((eq n 2) 'value)
			  ((eq n 3) 'restart)
			  ((eq n 4) 'function)))))
    (cond ((eq field 'value)
	   (let ((prompt "Set itimer value: "))
	     (while (not (natnump field-value))
	       (setq field-value (read-from-minibuffer prompt nil nil t)))))
	  ((eq field 'restart)
	   (let ((prompt "Set itimer restart: "))
	     (while (and field-value (not (natnump field-value)))
	       (setq field-value (read-from-minibuffer prompt nil nil t)))))
	  ((eq field 'function)
	   (let ((prompt "Set itimer function: "))
	     (while (not (or (and (symbolp field-value) (fboundp field-value))
			     (and (consp field-value)
				  (memq (car field-value) '(lambda macro)))))
	       (setq field-value
		     (read (completing-read prompt obarray 'fboundp nil)))))))
    ;; set the itimer field
    (funcall (intern (concat "set-itimer-" (symbol-name field)))
	     itimer field-value)
    ;; move to beginning of field to be changed
    (itimer-edit-beginning-of-field)
    ;; modify the list buffer to reflect the change.
    (let (buffer-read-only kill-ring)
      (kill-sexp 1)
      (kill-region (point) (progn (skip-chars-forward " \t") (point)))
      (prin1 field-value (current-buffer))
      (if (not (eolp))
	  (tab-to-tab-stop))
      (backward-sexp))))

(defun itimer-edit-delete-itimer ()
  (interactive)
  ;; First two lines in list buffer are headers.
  ;; Cry out against the luser who attempts to change a field there.
  (if (<= (point) itimer-edit-start-marker)
      (error ""))
  (delete-itimer
   (read-itimer "Delete itimer: "
	       (save-excursion (beginning-of-line) (read (current-buffer)))))
  ;; update list information
  (list-itimers))

(defun itimer-edit-next-field (count)
  (interactive "p")
  (itimer-edit-beginning-of-field)
  (cond ((> (itimer-signum count) 0)
	 (while (not (zerop count))
	   (forward-sexp)
	   ;; wrap from eob to itimer-edit-start-marker
	   (if (eobp)
	       (progn
		 (goto-char itimer-edit-start-marker)
		 (forward-sexp)))
	   (forward-sexp)
	   (backward-sexp)
	   ;; treat fields at beginning of line as if they weren't there.
	   (if (bolp)
	       (progn
		 (forward-sexp 2)
		 (backward-sexp)))
	   (itimer-decf count)))
	((< (itimer-signum count) 0)
	 (while (not (zerop count))
	   (backward-sexp)
	   ;; treat fields at beginning of line as if they weren't there.
	   (if (bolp)
	       (backward-sexp))
	   ;; wrap from itimer-edit-start-marker to field at eob.
	   (if (<= (point) itimer-edit-start-marker)
	       (progn
		 (goto-char (point-max))
		 (backward-sexp)))
	   (itimer-incf count)))))

(defun itimer-edit-previous-field (count)
  (interactive "p")
  (itimer-edit-next-field (- count)))

(defun itimer-edit-beginning-of-field ()
  (let ((forw-back (save-excursion (forward-sexp) (backward-sexp) (point)))
	(back (save-excursion (backward-sexp) (point))))
    (cond ((eq forw-back back) (backward-sexp))
	  ((eq forw-back (point)) t)
	  (t (backward-sexp)))))


;; internals of the itimer implementation.

(defun itimer-process-filter (process string)
  (error "itimer-process-filter is not used in Lucid Emacs")
;  ;; If the itimer process dies and generates output while doing
;  ;; so, we may be called before the process-sentinel.  Sanity
;  ;; check the output just in case...
;  (if (not (string-match "^[0-9]" string))
;      (progn (message "itimer process gave odd output: %s" string)
;	     ;; it may be still alive and waiting for input
;	     (process-send-string itimer-process "3\n"))
;    ;; if there are no active itimers, return quickly.
;    (if itimer-list
;	(let ((time-elapsed (string-to-int string))
;	      (itimers itimer-list)
;	      (itimer)
;	      ;; process filters can be hit by stray C-g's from the user,
;	      ;; so we must protect this stuff appropriately.
;	      ;; Quit's are allowed from within itimer functions, but we
;	      ;; catch them.
;	      (inhibit-quit t))
;	  (setq itimer-process-next-wakeup 600)
;	  (while itimers
;	    (setq itimer (car itimers))
;	    (set-itimer-value itimer (max 0 (- (itimer-value itimer) time-elapsed)) t)
;	    (if (> (itimer-value itimer) 0)
;		(setq itimer-process-next-wakeup
;		      (min itimer-process-next-wakeup (itimer-value itimer)))
;	      ;; itimer has expired, we must call its function.
;	      ;; protect our local vars from the itimer function.
;	      ;; allow keyboard quit to occur, but catch and report it.
;	      ;; provide the variable `current-itimer' in case the function
;	      ;; is interested.
;	      (condition-case condition-data
;		  (let* ((current-itimer itimer)
;			 itimer itimers time-elapsed
;			 quit-flag inhibit-quit)
;		    (funcall (itimer-function current-itimer)))
;		(error (message "itimer \"%s\" signaled: %s" (itimer-name itimer)
;				(prin1-to-string condition-data)))
;		(quit (message "itimer \"%s\" quit" (itimer-name itimer))))
;	      ;; restart the itimer if we should, otherwise delete it.
;	      (if (null (itimer-restart itimer))
;		  (delete-itimer itimer)
;		(set-itimer-value itimer (itimer-restart itimer) t)
;		(setq itimer-process-next-wakeup
;		      (min itimer-process-next-wakeup (itimer-value itimer)))))
;	    (setq itimers (cdr itimers)))
;	  ;; if user is editing itimers, update displayed info
;	  (if (eq major-mode 'itimer-edit-mode)
;	      (list-itimers)))
;      (setq itimer-process-next-wakeup 600))
;    ;; tell itimer-process when to wakeup again
;    (process-send-string itimer-process
;			 (concat (int-to-string itimer-process-next-wakeup)
;				 "\n")))
  )

(defun itimer-process-sentinel (process message)
  (error "itimer-process-sentinel is not used in Lucid Emacs")
;  (let ((inhibit-quit t))
;    (if (eq (process-status process) 'stop)
;	(continue-process process)
;      ;; not stopped, so it must have died.
;      ;; cleanup first...
;      (delete-process process)
;      (setq itimer-process nil)
;      ;; now, if there are any active itimers then we need to immediately
;      ;; start another itimer process, otherwise we can wait until the next
;      ;; start-itimer call,  which will start one automatically.
;      (if (null itimer-list)
;	  ()
;	;; there may have been an error message in the echo area;
;	;; give the user at least a little time to read it.
;	(sit-for 2)
;	(message "itimer process %s... respawning." (substring message 0 -1))
;	(itimer-process-start))))
  )

(defun itimer-process-start ()
  (error "itimer-process-start is not used in Lucid Emacs")
;  (let ((inhibit-quit t)
;	(process-connection-type nil))
;    (setq itimer-process (start-process "itimer" nil "itimer"))
;    (process-kill-without-query itimer-process)
;    (set-process-filter itimer-process 'itimer-process-filter)
;    (set-process-sentinel itimer-process 'itimer-process-sentinel)
;    ;; Tell itimer process to wake up quickly, so that a correct wakeup
;    ;; time can be computed.  Zero instead of one here loses because of
;    ;; underlying itimer implementations that use 0 to mean `disable the
;    ;; itimer'.
;    (setq itimer-process-next-wakeup 1)
;    (process-send-string itimer-process "1\n"))
  )

(defun itimer-process-wakeup ()
  (error "itimer-process-wakeup is not used in Lucid Emacs")
;  (interrupt-process itimer-process)
;  (accept-process-output)
  )


;; Lemacs-specific code

(defun activate-itimer (itimer)
  (let ((inhibit-quit t))
    (set-itimer-id itimer
		  (add-timeout (itimer-value itimer)
			       'itimer-callback
			       itimer
			       (itimer-restart itimer))))
  itimer)

(defun deactivate-itimer (itimer)
  (let ((inhibit-quit t)
	(id (itimer-id itimer)))
    (and id (disable-timeout id))
    (set-itimer-id itimer nil))
  itimer)

(defun itimer-callback (current-itimer)
  (funcall (itimer-function current-itimer)))


;;; itimer-driven auto-saves

;jwz: this is preloaded so don't ;;;###autoload
(defvar auto-save-timeout 30
  "*Number of seconds idle time before auto-save.
Zero or nil means disable auto-saving due to idleness.

The actual amount of idle time between auto-saves is logarithmically related
to the size of the current buffer.  This variable is the number of seconds
after which an auto-save will happen when the current buffer is 50k or less;
the timeout will be 2 1/4 times this in a 200k buffer, 3 3/4 times this in a
1000k buffer, and 4 1/2 times this in a 2000k buffer.

See also the variable `auto-save-interval', which controls auto-saving based
on the number of characters typed.")

;jwz: this is preloaded so don't ;;;###autoload
(defvar auto-gc-threshold (/ gc-cons-threshold 3)
  "*GC when this many bytes have been consed since the last GC, 
and the user has been idle for `auto-save-timeout' seconds.")

(defun auto-save-itimer ()
  "For use as a itimer callback function.
Auto-saves and garbage-collects based on the size of the current buffer
and the value of `auto-save-timeout', `auto-gc-threshold', and the current
keyboard idle-time."
  (if (or (null auto-save-timeout)
	  (<= auto-save-timeout 0)
	  (eq (minibuffer-window) (selected-window)))
      nil
    (let ((buf-size (1+ (ash (buffer-size) -8)))
	  (delay-level 0)
	  (now (current-time))
	  delay)
      (while (> buf-size 64)
	(setq delay-level (1+ delay-level)
	      buf-size (- buf-size (ash buf-size -2))))
      (if (< delay-level 4)
	  (setq delay-level 4))
      ;; delay_level is 4 for files under around 50k, 7 at 100k, 9 at 200k,
      ;; 11 at 300k, and 12 at 500k, 15 at 1 meg, and 17 at 2 meg.
      (setq delay (/ (* delay-level auto-save-timeout) 4))
      (let ((idle-time (if (or (not (consp last-input-time))
			       (/= (car now) (car last-input-time)))
			   (1+ delay)
			 (- (car (cdr now)) (cdr last-input-time)))))
	(and (> idle-time delay)
	     (do-auto-save))
	(and (> idle-time auto-save-timeout)
	     (> (consing-since-gc) auto-gc-threshold)
	     (garbage-collect)))))
  ;; Look at the itimer that's currently running; if the user has changed
  ;; the value of auto-save-timeout, modify this itimer to have the correct
  ;; restart time.  There will be some latency between when the user changes
  ;; this variable and when it takes effect, but it will happen eventually.
  (let ((self (get-itimer "auto-save")))
    (or self (error "auto-save-itimer can't find itself"))
    (if (and auto-save-timeout (> auto-save-timeout 4))
	(or (= (itimer-restart self) (/ auto-save-timeout 4))
	    (set-itimer-restart self (/ auto-save-timeout 4)))))
  nil)

(defun itimer-init-auto-gc ()
  (or noninteractive ; may be being run from after-init-hook in -batch mode.
      (get-itimer "auto-save")
      ;; the time here is just the first interval; if the user changes it
      ;; later, it will adjust.
      (let ((time (max 2 (/ (or auto-save-timeout 30) 4))))
	(start-itimer "auto-save" 'auto-save-itimer time time))))

(cond (purify-flag
       ;; This file is being preloaded into an emacs about to be dumped.
       ;; So arrange for the auto-save itimer to be started once emacs
       ;; is launched.
       (add-hook 'after-init-hook 'itimer-init-auto-gc))
      (noninteractive
       ;; This file is being loaded into a -batch emacs.  Do nothing.
       ;; (Lemacs currently can't cope with timers or subprocesses when
       ;; not connected to a window system; this will change eventually.)
       nil)
      (t
       ;; Otherwise, this file is being loaded into a normal, interactive
       ;; emacs.  Start the auto-save timer now.
       (itimer-init-auto-gc)))


(provide 'itimer)
