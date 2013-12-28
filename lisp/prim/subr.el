;; Basic lisp subroutines for Emacs
;; Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc.

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


;; called by Fkill_buffer()
(defvar kill-buffer-hook nil
  "Function or functions to be called when a buffer is killed.
The value of this variable may be buffer-local.
The buffer about to be killed is current when this hook is run.")

(defun generate-new-buffer (name)
  "Create and return a buffer with a name based on NAME.
Choose the buffer's name using `generate-new-buffer-name'."
  (get-buffer-create (generate-new-buffer-name name)))

(defun one-window-p (&optional nomini)
  "Returns non-nil if there is only one window.
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active."
  (eq (selected-window)
      (next-window (selected-window) (if nomini 'nomini))))

(defun walk-windows (proc &optional minibuf all-screens)
  "Cycle through all visible windows, calling PROC for each one.
PROC is called with a window as argument.
Optional second arg MINIBUF t means count the minibuffer window
even if not active.  If MINIBUF is neither t nor nil it means
not to count the minibuffer even if it is active.
Optional third arg ALL-SCREENS t means include all windows in all screens;
otherwise cycle within the selected screen."
  (let* ((walk-windows-start (selected-window))
	 (walk-windows-current walk-windows-start))
    (while (progn
	     (setq walk-windows-current
		   (next-window walk-windows-current minibuf all-screens))
	     (funcall proc walk-windows-current)
	     (not (eq walk-windows-current walk-windows-start))))))

(defun read-quoted-char (&optional prompt)
  "Like `read-char', except that if the first character read is an octal
digit, we read up to two more octal digits and return the character
represented by the octal number consisting of those digits.
Optional argument PROMPT specifies a string to use to prompt the user."
  (let ((count 0) (code 0) char)
    (while (< count 3)
      (let ((inhibit-quit (zerop count))
	    (help-form nil))
	(and prompt (message "%s-" prompt))
	(setq char (read-char))
	(if inhibit-quit (setq quit-flag nil)))
      (cond ((null char))
	    ((and (<= ?0 char) (<= char ?7))
	     (setq code (+ (* code 8) (- char ?0))
		   count (1+ count))
	     (and prompt (message (setq prompt
					(format "%s %c" prompt char)))))
	    ((> count 0)
	     (setq unread-command-event
		    (character-to-event char (allocate-event))
		   count 259))
	    (t (setq code char count 259))))
    (logand 255 code)))

(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'."
  (while t
    (signal 'error (list (apply 'format args)))))

(defun undefined ()
  (interactive)
  (ding))

;; Some programs still use this as a function.
(defun baud-rate ()
  "Obsolete function returning the value of the `baud-rate' variable."
  baud-rate)

;Prevent the \{...} documentation construct
;from mentioning keys that run this command.
(put 'undefined 'suppress-keymap t)

(defun suppress-keymap (map &optional nodigits)
  "Make MAP override all normally self-inserting keys to be undefined.
Normally, as an exception, digits and minus-sign are set to make prefix args,
but optional second arg NODIGITS non-nil treats them like other chars."
  (map-keymap (function (lambda (key binding)
			  (if (eq binding 'self-insert-command)
			      (define-key map (vector key) 'undefined))))
	      global-map)
  (or nodigits
      (let ((string (make-string 1 ?0)))
	(define-key map "-" 'negative-argument)
	;; Make plain numbers do numeric args.
	(while (<= (aref string 0) ?9)
	  (define-key map string 'digit-argument)
	  (aset string 0 (1+ (aref string 0)))))))

;; now in fns.c
;(defun nth (n list)
;  "Returns the Nth element of LIST.
;N counts from zero.  If LIST is not that long, nil is returned."
;  (car (nthcdr n list)))
;
;(defun copy-alist (alist)
;  "Return a copy of ALIST.
;This is a new alist which represents the same mapping
;from objects to objects, but does not share the alist structure with ALIST.
;The objects mapped (cars and cdrs of elements of the alist)
;are shared, however."
;  (setq alist (copy-sequence alist))
;  (let ((tail alist))
;    (while tail
;      (if (consp (car tail))
;	  (setcar tail (cons (car (car tail)) (cdr (car tail)))))
;      (setq tail (cdr tail))))
;  alist)

;Moved to keymap.c
;(defun copy-keymap (keymap)
;  "Return a copy of KEYMAP"  
;  (while (not (keymapp keymap))
;    (setq keymap (signal 'wrong-type-argument (list 'keymapp keymap))))
;  (if (vectorp keymap)
;      (copy-sequence keymap)
;      (copy-alist keymap)))

;;;>>> FSF19 takes arguments (olddef newdef keymap &optional oldmap prefix),
;;;>>> where "If optional fourth argument OLDMAP is specified, we redefine
;;;>>> in KEYMAP as NEWDEF those chars which are defined as OLDDEF in OLDMAP."
(defun substitute-key-definition (olddef newdef keymap)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.
Prefix keymaps reached from KEYMAP are not checked recursively;
perhaps they ought to be."
  (map-keymap (function (lambda (key binding)
			  (if (eq binding olddef)
			      (define-key keymap key newdef))))
	      keymap))

(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (list 'let '((_match_data_ (match-data)))
	(list 'unwind-protect
	      (cons 'progn body)
	      '(store-match-data _match_data_))))

(defun ignore (&rest ignore) nil)

; old names
(fset 'make-syntax-table 'copy-syntax-table)
(fset 'dot 'point)
(fset 'dot-marker 'point-marker)
(fset 'dot-min 'point-min)
(fset 'dot-max 'point-max)
(fset 'window-dot 'window-point)
(fset 'set-window-dot 'set-window-point)
(fset 'read-input 'read-string)
(fset 'send-string 'process-send-string)
(fset 'send-region 'process-send-region)
(fset 'show-buffer 'set-window-buffer)
(fset 'buffer-flush-undo 'buffer-disable-undo)

; alternate names
(fset 'string= 'string-equal)
(fset 'string< 'string-lessp)
(fset 'mod '%)
(fset 'move-marker 'set-marker)
(fset 'eql 'eq)
(fset 'not 'null)
(fset 'rplaca 'setcar)
(fset 'rplacd 'setcdr)
(fset 'beep 'ding) ;preserve lingual purtity
(fset 'indent-to-column 'indent-to)
(fset 'backward-delete-char 'delete-backward-char)
(fset 'search-forward-regexp (symbol-function 're-search-forward))
(fset 'search-backward-regexp (symbol-function 're-search-backward))

(defun run-hooks (&rest hooklist)
  "Takes hook names and runs each one in turn.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments."
  (while hooklist
    (let ((sym (car hooklist)))
      (and (boundp sym)
	   (symbol-value sym)
	   (let ((value (symbol-value sym)))
	     (if (and (listp value) (not (eq (car value) 'lambda)))
		 (mapcar 'funcall value)
	       (funcall value)))))
    (setq hooklist (cdr hooklist))))

;; Tell C code how to call this function.
(defconst run-hooks 'run-hooks
  "Variable by which C primitives find the function `run-hooks'.
Don't change it.")

(defun add-hook (hook-var function &optional at-end)
  "Add a function to a hook.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to add.
Third (optional) argument AT-END means to add the function at the end
 of the hook list instead of the beginning.  If the function is already
 present, this has no effect.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
 value of HOOK-VAR."
  ;(interactive "SAdd to hook-var (symbol): \naAdd which function to %s? ")
  (if (not (boundp hook-var)) (set hook-var nil))
  (let ((old (symbol-value hook-var)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(setq old (list old)))
    (if (member function old)
	nil
      (set hook-var
	   (if at-end
	       (nconc old (list function))
	     (cons function old))))))

(defun remove-hook (hook-var function)
  "Remove a function from a hook, if it is present.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to remove (compared with `eq')."
  (let (val)
    (cond ((not (boundp hook-var))
	   nil)
	  ((eq function (setq val (symbol-value hook-var)))
	   (setq hook-var nil))
	  ((consp val)
	   (set hook-var (delq function val))))))


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
	    (setq insert-end (point)))
	  (message (or message "Type %s to continue editing.")
		   (single-key-description exit-char))
	  (let ((event (next-command-event (allocate-event))))
	    (or (eq (event-to-character event) exit-char)
		(setq unread-command-event event))))
      (if insert-end
	  (save-excursion
	    (delete-region pos insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))

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

;;>> What a piece of junk!  This is what hooks are for!!
;(defun eval-after-load (file form)
;  "Arrange that, if FILE is ever loaded, FORM will be run at that time.
;This makes or adds to an entry on `after-load-alist'.
;FILE should be the name of a library, with no directory name."
;  (or (assoc file after-load-alist)
;      (setq after-load-alist (cons (list file) after-load-alist)))
;  (nconc (assoc file after-load-alist) (list form))
;  form)
;
;(defun eval-next-after-load (file)
;  "Read the following input sexp, and run it whenever FILE is loaded.
;This makes or adds to an entry on `after-load-alist'.
;FILE should be the name of a library, with no directory name."
;  (eval-after-load file (read)))

(defun user-original-login-name ()
  "Return user's login name from original login.
This tries to remain unaffected by `su', by looking in environment variables."
  (or (getenv "LOGNAME") (getenv "USER") (user-login-name)))

(defun redraw-mode-line (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL then force then force redisplay of all mode-lines."
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))

(fset 'force-mode-line-update 'redraw-mode-line)

;;;; Keymap stuff

(defun local-key-binding (keys)
  "Return the binding for command KEYS in current local keymap only.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
The binding is probably a symbol with a function definition; see
the documentation for `lookup-key' for more information."
  (let ((map (current-local-map)))
    (if map
        (lookup-key map keys)
        nil)))

(defun global-key-binding (keys)
  "Return the binding for command KEYS in current global keymap only.
KEYS is a string or vector of events, a sequence of keystrokes.
The binding is probably a symbol with a function definition; see
the documentation for `lookup-key' for more information."
  (lookup-key (current-global-map) keys))


(defun global-set-key (keys function)
  "Give KEY a global binding as COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
Note that if KEY has a local binding in the current buffer
that local binding will continue to shadow any global binding."
  (interactive "kSet key globally: \nCSet key %s to command: ")
  (define-key (current-global-map) keys function))

(defun local-set-key (keys function)
  "Give KEY a local binding as COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
The binding goes in the current buffer's local map,
which is shared with other buffers in the same major mode."
  (interactive "kSet key locally: \nCSet key %s locally to command: ")
  (if (null (current-local-map))
      (use-local-map (make-sparse-keymap)))
  (define-key (current-local-map) keys function))

(defun global-unset-key (keys)
  "Remove global binding of KEY.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function."
  (interactive "kUnset key globally: ")
  (global-set-key keys nil))

(defun local-unset-key (keys)
  "Remove local binding of KEY.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function."
  (interactive "kUnset key locally: ")
  (if (current-local-map)
      (define-key (current-local-map) keys nil)))

;;>>> What a crock
(defun define-prefix-command (name &optional mapvar)
  "Define COMMAND as a prefix command.
A new sparse keymap is stored as COMMAND's function definition.
If second optional argument MAPVAR is not specified,
 COMMAND's value (as well as its function definition) is set to the keymap.
If a second optional argument MAPVAR is given and is not `t',
  the map is stored as its value.
Regardless of MAPVAR, COMMAND's function-value is always set to the keymap."
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map name)
    (fset name map)
    (cond ((not mapvar)
           (set name map))
          ((eq mapvar 't)
           )
          (t
           (set mapvar map)))
    name))
