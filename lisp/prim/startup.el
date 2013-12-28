;; Process Emacs shell arguments
;; Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


; These are processed only at the beginning of the argument list.
; -batch		execute noninteractively (messages go to stdout,
;			 variable noninteractive set to t)
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -t file		Specify to use file rather than stdin/stdout
;			 as the terminal.
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -nw			Inhibit the use of any window-system-specific display
;			 code; use the current virtual terminal.
;			 This option must be the first in the arglist.
;			 Processed by `main' in emacs.c -- never seen by lisp
; -q			load no init file
; -no-init-file		same
; -u user		load user's init file
; -user user		same

; These are processed in the order encountered.
; -f function		execute function
; -funcall function	same
; -l file		load file
; -load file		same
; -i file		insert file into buffer
; -insert file		same
; file			visit file
; -kill			kill (exit) emacs

(setq top-level '(normal-top-level))

(defvar command-line-processed nil "t once command line has been processed")

(defconst inhibit-startup-message nil
  "*Non-nil inhibits the initial startup messages.
This is for use in your personal init file, once you are familiar
with the contents of the startup message.")

(defconst inhibit-default-init nil
  "*Non-nil inhibits loading the `default' library.")

(defconst command-switch-alist nil
  "Alist of command-line switches.
Elements look like (SWITCH-STRING . HANDLER-FUNCTION).
HANDLER-FUNCTION receives switch name as sole arg;
remaining command-line args are in the variable `command-line-args-left'.")

(defvar term-setup-hook nil
  "Function to be called after loading terminal-specific lisp code.
It is called with no arguments.  This variable exists for users to set,
so as to override the definitions made by the terminal-specific file.
Emacs never sets this variable itself.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.  This variable is used to define
the proper function and keypad keys for use under X.  It is used in a
fashion analogous to the environment value TERM.")

(defvar window-setup-hook nil
  "Function used to initialize window system display, after command line args.
Users should not set this variable; use term-setup-hook instead.")

(defconst initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer.")

(defvar init-file-user nil
  "When the `.emacs' file is read, this says which user's init file it is.
The value may be the null string or a string containing a user's name.
If the value is a null string, it means that the init file was taken from
the user that originally logged in.

In all cases, `(concat \"~\" init-file-user \"/\")' evaluates to the
directory name of the directory where the `.emacs' file was looked for.")

(defvar command-line-args-left) ; bound by `command-line'


;;; default switches

(defun command-line-process-funcall (arg)
  (let ((fn (intern (car command-line-args-left))))
    (setq command-line-args-left (cdr command-line-args-left))
    (funcall fn)))

(defun command-line-process-load (arg)
  (let ((file (car command-line-args-left)))
    ;; Take file from default dir if it exists there;
    ;; otherwise let `load' search for it.
    (if (file-exists-p (expand-file-name file))
	(setq file (expand-file-name file)))
    (load file nil t))
  (setq command-line-args-left (cdr command-line-args-left)))

(defun command-line-process-insert (arg)
  (insert-file-contents (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left)))

(defun command-line-process-kill (arg)
  (kill-emacs t))

(setq command-switch-alist
      '(("-f"		. command-line-process-funcall)
	("-e"		. command-line-process-funcall)
	("-funcall"	. command-line-process-funcall)
	("-l"		. command-line-process-load)
	("-load"	. command-line-process-load)
	("-i"		. command-line-process-insert)
	("-insert"	. command-line-process-insert)
	("-kill"	. command-line-process-kill)
	;; Options like +35 are handled specially.
	;; Window-system, site, or package-specific code might add to this.
	;; X11 handles its options by letting Xt remove args from this list.
	))


(defun premature-death-function (string &optional error)
  (let ((stream (function external-debugging-output)))
    (princ (if error 
	       (format "\n%s: %s%s%s\n" string
		       (get (car error) 'error-message)
		       (if (cdr error) ": ")
		       (mapconcat 'prin1-to-string (cdr error) ", "))
	     string)
	   stream)
    (if (getenv "EMACSLOADPATH")
	(princ (format "\n$EMACSLOADPATH is %s" (getenv "EMACSLOADPATH"))
	       stream))
    (princ (format "\nload-path is %S" load-path) stream)
    (princ (format "\nexec-directory is %S\n" exec-directory) stream))
  (kill-emacs 33))

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    ;; In presence of symlinks, switch to cleaner form of default directory.
    (if (not (eq system-type 'vax-vms))
	(mapcar (function
		 (lambda (var)
		   (let ((value (getenv var)))
		     (if (and value
			      (< (length value) (length default-directory))
			      (equal (file-attributes default-directory)
				     (file-attributes value)))
			 (setq default-directory
			       (file-name-as-directory value))))))
		'("PWD" "HOME")))
    (let ((tail directory-abbrev-alist))
      (while tail
	(if (string-match (car (car tail)) default-directory)
	    (setq default-directory
		  (concat (cdr (car tail))
			  (substring default-directory (match-end 0)))))
	(setq tail (cdr tail))))
    (command-line)))


(defun command-line-init ()
  (let ((done nil))
    ;; If user has not done su, use current $HOME to find .emacs.
    (and init-file-user (string= init-file-user (user-real-login-name))
	 (setq init-file-user ""))
    (while (and (not done) command-line-args-left)
      (let ((argi (car command-line-args-left)))
	(if (or (string-equal argi "-q")
		(string-equal argi "-no-init-file"))
	    (setq init-file-user nil
		  command-line-args-left (cdr command-line-args-left))
	  (if (or (string-equal argi "-u")
		  (string-equal argi "-user"))
	      (setq command-line-args-left (cdr command-line-args-left)
		    init-file-user (car command-line-args-left)
		    command-line-args-left (cdr command-line-args-left))
	    (setq done t)))))
    (let ((vc (getenv "VERSION_CONTROL")))
      (and vc (cond
	       ((or (string= vc "t")
		    (string= vc "numbered"))
		(setq version-control t))
	       ((or (string= vc "nil")
		    (string= vc "existing"))
		(setq version-control nil))
	       ((or (string= vc "never")
		    (string= vc "simple"))
		(setq version-control 'never)))))
    ))


(defun command-line ()
  (let ((command-line-args-left (cdr command-line-args)))
    (condition-case error
      (progn
	(set-default-load-path)
	(setq init-file-user (if noninteractive nil (user-login-name)))
	;;
	;; When running emacs under a window system, the window-system-specific
	;; files are loaded and hooks are run before the user's init file is
	;; loaded; this is so that the user can see messages that the init file
	;; prints out, so that the init file can display buffers in windows,
	;; etc.
	;;
	;; When running emacs on a terminal, the terminal-specific files are
	;; loaded and hooks are run after the user's init file is loaded so
	;; that the user can override what file is loaded, and has a little
	;; more flexibility.
	;;
	;; The term-setup-hook is always run after the window and terminal
	;; initializations have happened and the user's init file has been
	;; loaded so that the user can customize things.
	;;
	;; Maybe this hairiness is pointless, and terminal initialization
	;; should work the same as window-system initialization; if this were
	;; the case, then there would be no need for the term-init-hook (the
	;; init file could do it directly.)
	;;
	(if window-system (initialize-terminal-1))

	(if (and (eq window-system 'x)
		 (null (x-window-id (selected-screen))))
	    (premature-death-function
	"Initialization error: Loading term/x-win.el didn't create an X screen!
This probably means that this emacs is picking up an old (v18) lisp directory.
"))
	;; process magic command-line switches like -q and -u.
	(command-line-init)

	;; initialize redisplay to make Fmessage() work.
	(initialize-first-screen)
	)
      ;;
      ;; If we get an error above, it's almost always because emacs couldn't
      ;; find lisp/term/x-win.el, or it's loading the v18 lisp/term/x-win.el.
      ;; If emacs supported ttys, then we could concievably continue here,
      ;; and simply run in tty mode, but right now, that just causes the
      ;; bogus "only runs under X" error to be printed.  Even when ttys work,
      ;; there's not much point in trying to run if we know we're going to be
      ;; so completely crippled.  It probably just won't work.
      ;;
      (error
       (premature-death-function "Initialization error" error)))
    ;;
    ;; We have normality, I repeat, we have normality.  Anything you still
    ;; can't cope with is therefore your own problem.  (And we don't need
    ;; to kill emacs for it.)
    ;;
    (load-init-file)
    
    ;; If *scratch* exists and init file didn't change its mode, initialize it.
    (if (get-buffer "*scratch*")
	(save-excursion
	  (set-buffer "*scratch*")
	  (if (eq major-mode 'fundamental-mode)
	      (funcall initial-major-mode))))

    ;; Initialize terminal (not window system.)  See comment above.
    (or window-system (initialize-terminal-1))
    
    ;; run user's terminal init hooks.
    (initialize-terminal-2)
    
    ;; now process the rest of the command line, including user options.
    (command-line-1)
    
    (if noninteractive (kill-emacs t))))


;;; Load user's init file, or load default one.
(defun load-init-file ()
  (condition-case error
      (if init-file-user
	  (progn (load (if (eq system-type 'vax-vms)
			   "sys$login:.emacs"
			 (concat "~" init-file-user "/.emacs"))
		       t t t)
		 (or inhibit-default-init
		     (let ((inhibit-startup-message nil))
		       ;; Users are supposed to be told their rights.
		       ;; (Plus how to get help and how to undo.)
		       ;; Don't you dare turn this off for anyone
		       ;; except yourself.
		       (load "default" t t)))))
    (error (message "Error in init file: %s%s%s"
		    (get (car error) 'error-message)
		    (if (cdr error) ": ")
		    (mapconcat 'prin1-to-string (cdr error) ", ")))))

(defun initialize-terminal-1 ()
  ;; Load library for our terminal type or window system.
  ;; User init file can set term-file-prefix to nil to prevent this.
  (and term-file-prefix (not noninteractive)
       (if window-system
	   (load (concat term-file-prefix
			 (symbol-name window-system)
			 "-win")
		 ;; Every window system should have a startup file;
		 ;; barf if we can't find it.
		 nil t)
	 ;; else
	 (let ((term (getenv "TERM"))
	       hyphend)
	   (while (and term
		       (not (load (concat term-file-prefix term) t t)))
	     ;; Strip off last hyphen and what follows, then try again
	     (if (setq hyphend (string-match "[-_][^-_]+$" term))
		 (setq term (substring term 0 hyphend))
	       (setq term nil))))))

  ;; initialize the window system, create the first screen, etc.
  (condition-case error
      (and window-setup-hook
	   (run-hooks 'window-setup-hook))
    (error
     (premature-death-function "Error in window-setup-hook" error))))

(defun initialize-terminal-2 ()
  ;; run the user's terminal init hooks.
  (condition-case error
      (and term-setup-hook
	   (run-hooks 'term-setup-hook))
    (error (message "Error in term-setup-hook: %s%s%s"
		    (get (car error) 'error-message)
		    (if (cdr error) ": ")
		    (mapconcat 'prin1-to-string (cdr error) ", ")))))


(defun command-line-1 ()
  (if (null command-line-args-left)
      (cond ((and (not inhibit-startup-message) (not noninteractive)
		  ;; Don't clobber a non-scratch buffer if init file
		  ;; has selected it.
		  (string= (buffer-name) "*scratch*")
		  (not (input-pending-p)))
	     (unwind-protect
		 (progn
		   (insert (emacs-version)
			   "
Copyright (C) 1990 Free Software Foundation, Inc.\n\
Copyright (C) 1992 Lucid, Inc.\n")
		   ;; with the new Fwhere_is_internal(), this takes 0.02 secs.
		   (insert (substitute-command-keys
       "Type \\[help-command] for help; \\[advertised-undo] to undo changes.  (`C-' means use CTRL key.)
To kill the Emacs job, type \\[save-buffers-kill-emacs].
Type \\[help-with-tutorial] for a tutorial on using Emacs.

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for full details.
You may give out copies of Emacs; type \\[describe-copying] to see the conditions.
Type \\[describe-distribution] for information on getting the latest version."))
		   (fontify-copyleft)
		   (set-buffer-modified-p nil)
		   (sit-for 120))
	       (save-excursion
		 ;; In case the Emacs server has already selected
		 ;; another buffer, erase the one our message is in.
		 (set-buffer (get-buffer "*scratch*"))
		 (erase-buffer)
		 (set-buffer-modified-p nil)))))
    (let ((dir default-directory)
	  (file-count 0)
	  first-file-buffer
	  (line 0))
      (while command-line-args-left
	(let ((argi (car command-line-args-left))
	      tem)
	  (setq command-line-args-left (cdr command-line-args-left))
	  (cond ((setq tem (assoc argi command-switch-alist))
		 (funcall (cdr tem) argi))
		((string-match "^\\+[0-9]+\\'" argi)
		 (setq line (string-to-int argi)))
		(t
		 (setq file-count (1+ file-count))
		 (cond ((= file-count 1)
			(setq first-file-buffer
			      (progn
				(find-file (expand-file-name argi dir))
				(current-buffer))))
		       (t
			(find-file-other-window (expand-file-name argi dir))))
		 (or (zerop line)
		     (goto-line line))
		 (setq line 0)))))
      ;; If 3 or more files visited, and not all visible, show user what
      ;; they all are.
      (if (> file-count 2)
	  (or (get-buffer-window first-file-buffer)
	      (progn (other-window 1)
		     (buffer-menu nil)))))))


(defun set-default-load-path ()
  ;;
  ;; Make sure there is a reasonable elisp library directory on the load path,
  ;; and a reasonable emacs utility directory on the exec-path, even if this
  ;; emacs was dumped with now-inappropriate defaults.  Look at the full
  ;; pathname of the running emacs image:
  ;;
  ;;   o  If the emacs image is named D/src/P, and D/lisp/ exists, then use
  ;;	  that for the lisp library, use D/etc/ for the exec-directory, and
  ;;	  use D/lock/ for the lock directory.
  ;;   o  If it is named D/P, and D/lisp/ (or D/etc/ or D/lock/) exists,
  ;;	  then use that.
  ;;   o  If it is named D/bin/P, and D/P/lisp/ exists, then use that.
  ;;	  That is, the executable "/foo/local/bin/emacs19" would look for
  ;;	  the lisp library "/foo/local/emacs19/lisp/".
  ;;   o  If the emacs program that was run is itself a symlink, then chase
  ;;      the link and try again on the resultant directory.
  ;;
  ;; These heuristics fail if the emacs binary was copied from the main emacs
  ;; tree to some other directory, and links for the lisp directory were not
  ;; put in, but I can't think of a way around that.
  ;;
  (let ((lisp (find-default-load-path-internal "lisp"))
	(etc  (find-default-load-path-internal "etc"))
	(lock (and (boundp 'lock-directory)
		   (or lock-directory
		       (find-default-load-path-internal "lock")))))
    (if lisp
	(or (member lisp load-path)
	    (progn
	      ;; If the lisp dir isn't on the load-path, add it.
	      (setq load-path (append load-path (list lisp)))
	      ;; If the lisp dir wasn't on the load-path, then also add
	      ;; any direct subdirectories of the lisp directory to the
	      ;; load-path.  But don't add dirs whose names begin with
	      ;; dot or hyphen.
	      (let ((files (directory-files lisp nil "^[^-.]" nil 'dirs-only))
		    file)
		(while files
		  (setq file (car files))
		  (if (and (not (member file '("RCS" "CVS")))
			   (setq file (expand-file-name file lisp))
			   (not (member file load-path)))
		      (setq load-path
			    (nconc load-path
				   (list (file-name-as-directory file)))))
		  (setq files (cdr files))))
	      )))
    (if etc
	(or (member etc exec-path)
	    (setq exec-path (append exec-path (list etc)))))
    (if (and (null exec-directory) etc)
	(setq exec-directory (file-name-as-directory etc)))
    (if (and (boundp 'Info-directory-list) (null Info-directory-list))
	(setq Info-directory-list
	      (list (expand-file-name "../info/" exec-directory))))
    (if (boundp 'lock-directory)
	(progn
	  (setq lock-directory lock)
	  (if (and lock-directory (null superlock-path))
	      (setq superlock-path
		    (concat lock-directory "!!!SuperLock!!!")))))
    (set-default-load-path-warning)))


(defun set-default-load-path-warning ()
  (if (and exec-directory load-path
	   (or (not (boundp 'lock-directory)) lock-directory))
      nil
    (save-excursion
      (set-buffer (get-buffer-create " *warning-tmp*"))
      (erase-buffer)
      (buffer-disable-undo (current-buffer))
      (let ((fill-column 70)
	    warnings message guess)
	(if (and (boundp 'lock-directory) (null lock-directory))
	    (setq warnings (cons "lock-directory" warnings)))
	(if (null exec-directory)
	    (setq warnings (cons "exec-directory" warnings)))
	(if (null load-path)
	    (setq warnings (cons "load-path" warnings)))
	(cond ((cdr (cdr warnings))
	       (setq message (apply 'format "%s, %s, and %s" warnings)))
	      ((cdr warnings)
	       (setq message (apply 'format "%s and %s" warnings)))
	      (t (setq message (format "variable %s" (car warnings)))))
	(insert "couldn't find an obvious default for " message
		", and there were no defaults specified in paths.h when emacs "
		"was built.  Perhaps some directories don't exist, or the "
		"emacs executable, " execution-path " is in a strange place?")
	(setq guess (or exec-directory
			(car (reverse load-path))
			(and (string-match "/[^/]+$" execution-path)
			     (substring execution-path 0
					(match-beginning 0)))))
	(if (and guess (string-match "/\\(src\\|etc\\|lisp\\)/$" guess))
	    (setq guess (substring guess 0 (match-beginning 0))))
	(if (and guess (string-match "/$" guess))
	    (setq guess (substring guess 0 (match-beginning 0))))
	(if (and (boundp 'lock-directory) (null lock-directory))
	    (progn
	      (insert
	       "\n\nWithout lock-directory set, file locking won't work.  ")
	      (if guess
		  (insert
		   "Consider creating " guess "/lock/ as a directory or "
		   "symbolic link for use as the lock directory.  "))))

	(if (or (null exec-directory) (null load-path))
	    (insert
	     "\n\nWithout both exec-directory and load-path, emacs will "
	     "be very broken.  "))
	(if (and (null exec-directory) guess)
	    (insert
	     "Consider making a symbolic link from " guess
	     "/etc/ to wherever the appropriate emacs etc directory is"))
	(if (and (null load-path) guess)
	    (insert
	     (if exec-directory "Consider making a symbolic link " ", and ")
	     "from " guess
	     "/lisp/ to wherever the appropriate emacs lisp library is.  ")
	  (if (and (null exec-directory) guess) (insert ".")))
	(fill-region (point-min) (point-max))
	(goto-char (point-min))
	(princ "\nWARNING:\n" (function external-debugging-output))
	(princ (buffer-string) (function external-debugging-output))
	(erase-buffer)
	t))))


(defun find-default-load-path-internal (dir)
  (let ((invocation-directory (file-name-directory execution-path))
        tmp)
    (cond ;;
          ;; If in .../src/ or ../bin/, look for subdirs of this directory's
	  ;; parent.
          ;;
          ((and (string-match "/\\(src\\|etc\\|lisp\\|lock\\|bin\\)/$"
			      invocation-directory)
		(file-directory-p
		 (setq tmp (concat (substring invocation-directory 0
					      (match-beginning 0))
				   "/" dir "/"))))
	   tmp)
	  ;;
          ;; If in .../bin/lemacs, look for .../lib/lemacs/lisp/
          ;;
          ((and (string-match "\\(^\\|/\\)bin/$" invocation-directory)
		(file-directory-p
		 (setq tmp (concat (substring invocation-directory 0
					      (match-end 1))
				   "lib/"
				   (file-name-nondirectory execution-path)
				   "/" dir "/"))))
	   tmp)
	  ;;
	  ;; Look for subdirectories of this directory.
	  ;;
	  ((file-directory-p
	    (setq tmp (concat invocation-directory dir "/")))
	   tmp)
	  ;;
	  ;; Chase symlinks and try again.
	  ;;
	  ((setq tmp (file-symlink-p execution-path))
	   (let ((execution-path tmp))
	     (find-default-load-path-internal dir)))
	  ;;
	  ;; Otherwise, look in a couple of obvious places.
	  ;; It's probably not worth it to try any harder than this.
	  ;;
	  ((not (or (string-match "^/usr/local/lib/emacs/" execution-path)
		    (string-match "^/usr/local/emacs/" execution-path)))
	   (let ((execution-path
		  (concat "/usr/local/lib/emacs/"
			  (file-name-nondirectory execution-path))))
	     (or (find-default-load-path-internal dir)
		 (progn
		   (setq execution-path 
			 (concat "/usr/local/emacs/"
				 (file-name-nondirectory execution-path)))
		   (find-default-load-path-internal dir)))))
	  
	  (t nil))))


(defvar cdlist nil)

(defun initialize-cdlist ()
  (let ((cdpath (getenv "CDPATH"))
	buf here end l)
    (if cdpath
	(save-excursion 
	  (setq buf (get-buffer-create "** cdpath-decode **"))
	  (set-buffer buf)
	  (erase-buffer)
	  (insert cdpath)
	  (insert ?:)
	  (goto-char (point-min))
	  (setq here (point)
		end (point-max))
	  (while (< (point) (point-max))
	    (re-search-forward ":" end 33)
	    (setq l (cons (directory-file-name
			   (buffer-substring here (1- (point)))) l)
		  here (point)))
	  (nreverse l))
      nil)))


(defun fontify-copyleft ()
  (and window-system (fboundp 'set-extent-face)
       (save-excursion
	 (let ((case-fold-search nil))
	   (goto-char (point-min))
	   (while (re-search-forward
		   "\\b\\(C-[xh]\\( \\([CM]-\\)?.\\)?\\|M-x [-a-z]+\\)\\b"
		   nil t)
	     (set-extent-face (make-extent (match-beginning 0) (match-end 0))
			      'bold))
	   (goto-char (point-min))
	   (while (re-search-forward "^Copyright[^\n]+$" nil t)
	     (set-extent-face (make-extent (match-beginning 0) (match-end 0))
			      'bold-italic))
	   (goto-char (point-min))
	   (and (search-forward "ABSOLUTELY NO WARRANTY" nil t)
		(set-extent-face
		 (make-extent (match-beginning 0) (match-end 0))
		 'italic))))))
