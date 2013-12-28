;; Process Emacs shell arguments
;; Copyright (C) 1985, 1986, 1990, 1992, 1993 Free Software Foundation, Inc.
;; Copyright (c) 23 Sep 1993 Sun Microsystems, Inc.  All Rights Reserved.

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

;; Modifications:
;;
;; aaron.endelman@sun.com 9/23/93
;; Added -sparcworks switch; only exists if sparcworks is a feature.
;; Complete the ToolTalk initialization, if needed.
;; Separated off the tail end of command-line-1, now called
;; parse-command-line, in order to handle sparcworks 'yielding' protocol.
;; Added code to handle the 'yielding' case.
;; Added startup-message-timeout variable.  Normally 120 [seconds].

(defun command-line-do-help (arg)
  "Print this message and exit."
  (let ((standard-output 'external-debugging-output))
    (princ (emacs-version))
    (princ "\n\n")
    (if (fboundp 'x-create-screen)
	(princ "Lucid Emacs accepts all standard X Toolkit command line options.\
  In addition,\nthe ")
      (princ "The "))
    (princ "following options are processed in the order encountered:\n\n")
    (let ((l command-switch-alist)
	  (insert (function (lambda (&rest x)
			      (princ "  ")
			      (let ((len 2))
				(while x
				  (princ (car x))
				  (setq len (+ len (length (car x))))
				  (setq x (cdr x)))
				(if (>= len 24)
				    (progn (terpri) (setq len 0)))
				(while (< len 24)
				  (princ " ")
				  (setq len (1+ len))))))))
      (while l
        (let ((name (car (car l)))
              (fn (cdr (car l)))
	      doc arg cons)
	  (cond
	   ((and (symbolp fn) (get fn 'undocumented)) nil)
	   (t
	    (setq doc (documentation fn))
	    (if (member doc '(nil "")) (setq doc "(undocumented)"))
	    (cond ((string-match "\n\\(<.*>\\)\n?\\'" doc)
		   ;; Doc of the form "The frobber switch\n<arg1> <arg2>"
		   (setq arg (substring doc (match-beginning 1) (match-end 1))
			 doc (substring doc 0 (match-beginning 0))))
		  ((string-match "\n+\\'" doc)
		   (setq doc (substring doc 0 (match-beginning 0)))))
	    (if (and (setq cons (rassq fn command-switch-alist))
		     (not (eq cons (car l))))
		(setq doc (format "Same as %s." (car cons))))
	    (if arg
		(funcall insert name " " arg)
	      (funcall insert name))
	    (princ doc)
	    (terpri))))
        (setq l (cdr l))))
    (princ "\
  +N <file>             Start displaying <file> at line N.

These options are processed only if they appear before all other options:

  -batch                Execute noninteractively (messages go to stderr.)
                        This option must be first in the list.
  -nw                   Inhibit the use of any window-system-specific
                        display code: use the current tty.
  -no-site-file         Do not load the site-specific init file (site-run.el).
  -no-init-file         Do not load the user-specific init file (~/.emacs).
  -q                    Same as -no-init-file.
  -user <user>          Load user's init file instead of your own.
  -u <user>             Same as -user.")

    (if (featurep 'sparcworks)
	(progn
	  (princ "
  -sparcworks on|off	Connect to the SPARCworks manager 
			and set up GUI for SPARCworks.")
	  (if sparcworks-enabled
	      (princ "
			(default on)")
	      (princ "
			(default off)"))

	  (princ "
  -proworks on|off	Same as -sparcworks.
  -sunpro on|off	Same as -sparcworks.")))

    (princ "

Anything else is considered a file name, and is placed into a buffer for
editing.

Emacs has an online tutorial and manuals.  Type ^Ht (Control-h t) after
starting emacs to run the tutorial.  Type ^Hi to enter the manual browser.\n")
    (kill-emacs 0)
    ))

;;; -batch, -t, and -nw are processed by main() in emacs.c and are 
;;; never seen by lisp code.

;;; -version and -help are special-cased as well: they imply -batch,
;;; but are left on the list for lisp code to process.


;; This should really be in files.el, but is used very early.
(defvar directory-abbrev-alist nil
  "*Alist of abbreviations for file directories.
A list of elements of the form (FROM . TO), each meaning to replace
FROM with TO when it appears in a directory name.
This replacement is done when setting up the default directory of a
newly visited file.  *Every* FROM string should start with \\\\` or ^.

Use this feature when you have directories which you normally refer to
via absolute symbolic links.  Make TO the name of the link, and FROM
the name it is linked to.")

(defvar abbreviated-home-dir nil
  "The user's homedir abbreviated according to `directory-abbrev-alist'.")

(defun abbreviate-file-name (filename &optional hack-homedir)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
See \\[describe-variable] directory-abbrev-alist RET for more information.
If optional argument HACK-HOMEDIR is non-nil, then This also substitutes
\"~\" for the user's home directory."
  (let ((tail directory-abbrev-alist))
    (while tail
      (if (string-match (car (car tail)) filename)
	  (setq filename
		(concat (cdr (car tail)) (substring filename (match-end 0)))))
      (setq tail (cdr tail))))
  (if hack-homedir
      (progn
	;; Compute and save the abbreviated homedir name.
	;; We defer computing this until the first time it's needed, to
	;; give time for directory-abbrev-alist to be set properly.
	;; We include a slash at the end, to avoid spurious matches
	;; such as `/usr/foobar' when the home dir is `/usr/foo'.
	(or abbreviated-home-dir
	    (setq abbreviated-home-dir
		  (let ((abbreviated-home-dir "$foo"))
		    (concat "\\`" (regexp-quote (abbreviate-file-name
						 (expand-file-name "~")))
			    "\\(/\\|\\'\\)"))))
	(if (string-match abbreviated-home-dir filename)
	    (setq filename
		  (concat "~"
			  ;; If abbreviated-home-dir ends with a slash,
			  ;; don't remove the corresponding slash from
			  ;; filename.  On MS-DOS and OS/2, you can have
			  ;; home directories like "g:/", in which it is
			  ;; important not to remove the slash.  And what
			  ;; about poor root on Unix systems?
			  (if (eq ?/ (aref abbreviated-home-dir
					   (1- (length abbreviated-home-dir))))
			      "/"
			    "")
			  (substring filename
				     (match-beginning 1) (match-end 1))
			  (substring filename (match-end 0)))))))
  filename)


(setq top-level '(normal-top-level))

(defvar command-line-processed nil "t once command line has been processed")

(defvar startup-message-timeout 120
  "How long to keep the startup message visible.")

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

(defvar before-init-hook nil
  "Functions to call after handling urgent options but before loading init file.
The window system interface uses this to open screens to display messages while
Emacs loads the user's initialization file.")

(defvar after-init-hook nil
  "Functions to call after loading the init file (`~/.emacs').
The call is not protected by a condition-case, so you can set `debug-on-error'
in `.emacs', and put all the actual code on `after-init-hook'.")

(defvar term-setup-hook nil
  "Functions to be called after loading terminal-specific lisp code.
See `run-hooks'.  This variable exists for users to set,
so as to override the definitions made by the terminal-specific file.
Emacs never sets this variable itself.")

(defvar keyboard-type nil
  "The brand of keyboard you are using.  This variable is used to define
the proper function and keypad keys for use under X.  It is used in a
fashion analogous to the environment value TERM.")

(defvar window-setup-hook nil
  "Function called to initialize window system display.
Emacs calls this after processing the command line arguments and loading
the user's init file.

Users should not set this variable; use `term-setup-hook' instead.")

(defconst initial-major-mode 'lisp-interaction-mode
  "Major mode command symbol to use for the initial *scratch* buffer.")

(defvar init-file-user nil
  "Identity of user whose `.emacs' file is or was read.
The value may be the null string or a string containing a user's name.
If the value is a null string, it means that the init file was taken from
the user that originally logged in.

In all cases, `(concat \"~\" init-file-user \"/\")' evaluates to the
directory name of the directory where the `.emacs' file was looked for.")

(defvar init-file-debug nil)

(defvar command-line-args-left) ; bound by `command-line'

(defvar site-start-file (purecopy "site-start")
  "File containing site-wide run-time initializations.
This file is loaded at run-time before ~/.emacs.
It should contain initializations which need to be in place
for the entire site, but which, due to their higher incidence
of change, don't make sense to load into emacs' dumped image.

Thus, the run-time load order is:
  1. file named by this variable, if non-nil;
  2. User's `.emacs';
  3. default.el.")


;;; default switches
;;; Note: these doc strings are semi-magical.

(defun command-line-do-funcall (arg)
  "Invoke the named lisp function with no arguments.
<function>"
  (let ((fn (intern (car command-line-args-left))))
    (setq command-line-args-left (cdr command-line-args-left))
    (funcall fn)))
(fset 'command-line-do-funcall-1 'command-line-do-funcall)
(put 'command-line-do-funcall-1 'undocumented t)

(defun command-line-do-eval (arg)
  "Evaluate the lisp form.  Quote it carefully.
<form>"
  (let ((form (car command-line-args-left)))
    (setq command-line-args-left (cdr command-line-args-left))
    (eval (read form))))

(defun command-line-do-load (arg)
  "Load the named file of Lisp code into Emacs.
<file>"
  (let ((file (car command-line-args-left)))
    ;; Take file from default dir if it exists there;
    ;; otherwise let `load' search for it.
    (if (file-exists-p (expand-file-name file))
	(setq file (expand-file-name file)))
    (load file nil t))
  (setq command-line-args-left (cdr command-line-args-left)))

(defun command-line-do-insert (arg)
  "Insert file into the current buffer.
<file>"
  (insert-file-contents (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left)))

(defun command-line-do-kill (arg)
  "Exit Emacs."
  (kill-emacs t))

(defun command-line-do-version (arg)
  "Print version info and exit."
  (princ (concat (emacs-version) "\n") 'external-debugging-output)
  (kill-emacs 0))

(setq command-switch-alist
      (purecopy
       '(("-help"	. command-line-do-help)
	 ("-version"	. command-line-do-version)
	 ("-funcall"	. command-line-do-funcall)
         ("-f"		. command-line-do-funcall)
	 ("-e"		. command-line-do-funcall-1)
	 ("-eval"	. command-line-do-eval)
	 ("-load"	. command-line-do-load)
	 ("-l"		. command-line-do-load)
	 ("-insert"	. command-line-do-insert)
	 ("-i"		. command-line-do-insert)
	 ("-kill"	. command-line-do-kill)
	 ;; Options like +35 are handled specially.
	 ;; Window-system, site, or package-specific code might add to this.
	 ;; X11 handles its options by letting Xt remove args from this list.
	 )))

(defun premature-death-function (string &optional error)
  (let ((stream 'external-debugging-output))
    (if (null error)
	(princ string stream)
      (princ (concat "\n" string ": ") stream)
      (display-error error stream)
      (princ "\n" stream))
    (if (getenv "EMACSLOADPATH")
	(princ (format "\n$EMACSLOADPATH is %s" (getenv "EMACSLOADPATH"))
	       stream))
    (princ (format "\nload-path is %S" load-path) stream)
    (princ (format "\nexec-directory is %S" exec-directory) stream)
    (princ (format "\ndata-directory is %S\n\n" data-directory) stream)
    (backtrace stream t))
  (kill-emacs 33))

(defun normal-top-level ()
  (if command-line-processed
      (message "Back to top level.")
    (setq command-line-processed t)
    ;; Canonicalise HOME (PWD is canonicalised by init_buffer in buffer.c)
    (if (not (eq system-type 'vax-vms))
        (let ((value (getenv "HOME")))
          (if (and value
                   (< (length value) (length default-directory))
                   (equal (file-attributes default-directory)
                          (file-attributes value)))
              (setq default-directory (file-name-as-directory value)))))
    (setq default-directory (abbreviate-file-name default-directory))
    (unwind-protect
         (command-line)
      (run-hooks 'emacs-startup-hook)
      (run-hooks 'term-setup-hook)
      (setq term-setup-hook nil)
      (run-hooks 'window-setup-hook)
      (setq window-setup-hook nil))))

(defun command-line-init ()
  ;; See if we should import version-control from the environment variable.
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
  (let ((done nil))
    ;; If user has not done su, use current $HOME to find .emacs.
    (and init-file-user (string= init-file-user (user-real-login-name))
	 (setq init-file-user ""))

    (while (and (not done) command-line-args-left)
      (let ((argi (car command-line-args-left)))
	(cond ((or (string-equal argi "-q")
		   (string-equal argi "-no-init-file"))
	       (setq init-file-user nil
		     command-line-args-left (cdr command-line-args-left)))
	      ((string-equal argi "-no-site-file")
	       (setq site-start-file nil
		     command-line-args-left (cdr command-line-args-left)))
	      ((or (string-equal argi "-u")
		   (string-equal argi "-user"))
	       (setq command-line-args-left (cdr command-line-args-left)
		     init-file-user (car command-line-args-left)
		     command-line-args-left (cdr command-line-args-left)))
	      ((or (string-equal argi "-sparcworks")
		   (string-equal argi "-proworks")
		   (string-equal argi "-sunpro"))
	       ;; processed earlier
	       (setq command-line-args-left (cddr command-line-args-left)))
              ((string-equal argi "-debug-init")
               (setq init-file-debug t
                     command-line-args-left (cdr command-line-args-left)))
 	      (t (setq done t)))))))


(defun command-line ()
  (let ((command-line-args-left (cdr command-line-args)))
    (condition-case error
      (progn
	(set-default-load-path)

	(setq init-file-user (if (noninteractive) nil (user-login-name)))

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
	(if window-system
            (condition-case error
                (progn
                  (load (concat term-file-prefix
                                (symbol-name window-system)
                                "-win")
                        ;; Every window system should have a startup file;
                        ;; barf if we can't find it.
                        nil t)
                  ;; initialize the window system, 
                  ;;  create the first screen, etc.
                  (run-hooks 'window-setup-hook)
                  (setq window-setup-hook nil))
              (error
               (premature-death-function "Error in window-setup" error))))

        (and (eq window-system 'x)
             (null (x-window-id (selected-screen)))
             (premature-death-function 
  "Initialization error: Loading term/x-win.el didn't create an X screen!
This probably means that this emacs is picking up an old (v18) lisp directory.
"))

	;; process magic command-line switches like -q and -u.
	(command-line-init)

	;; initialize redisplay to make Fmessage() work.
        ;; >>> shouldn't window-setup-hook do this?
	(or (noninteractive) (initialize-first-screen))
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

    ;;; Load init files.
    (load-init-file)
    
    ;; If *scratch* exists and init file didn't change its mode, initialize it.
    (if (get-buffer "*scratch*")
	(save-excursion
	  (set-buffer "*scratch*")
	  (if (eq major-mode 'fundamental-mode)
	      (funcall initial-major-mode))))

    ;; Initialize terminal (not window system.)  See comment about the
    ;; crockishness of this above.
    (and (not window-system)
         ;; Load library for our terminal type or window system.
         ;; User init file can set term-file-prefix to nil to prevent this.
         term-file-prefix
         (not (noninteractive))
         (let ((term (getenv "TERM"))
               hyphend)
           (while (and term
                       (not (load (concat term-file-prefix term) t t)))
             ;; Strip off last hyphen and what follows, then try again
             (if (setq hyphend (string-match "[-_][^-_]+\\'" term))
                 (setq term (substring term 0 hyphend))
                 (setq term nil)))))

    ;; run the user's terminal init hooks.
    (condition-case error
        (run-hooks 'term-setup-hook)
      (error
       (message "Error in term-setup-hook: ")
       (display-error error nil)
       (sit-for 1)))
    ;; Don't let the hook be run twice.
    (setq term-setup-hook nil)

     ;; complete tt startup if needed
    (if (not noninteractive)
	(if (fboundp 'complete-sparcworks-tooltalk-init)
	    (complete-sparcworks-tooltalk-init)))

    ;; now process the rest of the command line, including user options.
    (command-line-1)
    
    (if (noninteractive) (kill-emacs t))))


;;; Load user's init file and default ones.
(defun load-init-file ()
  (run-hooks 'before-init-hook)

  ;; Sites should not disable this.  Only individuals should disable
  ;; the startup message.
  (setq inhibit-startup-message nil)

  ;; Run the site-start library if it exists.  The point of this file is
  ;; that it is run before .emacs.  There is no point in doing this after
  ;; .emacs; that is useless.
  (let ((inhibit-startup-message nil))
    (and (stringp site-start-file)
         (load site-start-file t t)))

  (let ((load-init-file
          (function (lambda ()
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
                             (load "default" t t)))))))))
    ;; Load that user's init file, or the default one, or none.
    (if init-file-debug
        (let ((debug-on-error t))
          (funcall load-init-file))
        (condition-case error
            (funcall load-init-file)
          (error
           (message "Error in init file: ")
           (display-error error nil)))))

    (run-hooks 'after-init-hook))

(defun command-line-1 ()
  (if (null command-line-args-left)
      (cond ((and (not inhibit-startup-message) (not (noninteractive))
		  ;; Don't clobber a non-scratch buffer if init file
		  ;; has selected it.
		  (string= (buffer-name) "*scratch*")
		  (not (input-pending-p)))
	     (unwind-protect
		 (progn
		   (insert (emacs-version)
			   "
Copyright (C) 1990 Free Software Foundation, Inc.
Copyright (C) 1990-1993 Lucid, Inc.
Copyright (C) 1993 Sun Microsystems, Inc.

This version of Emacs is a part of Lucid's Energize Programming System,
a C/C++ development environment.  Send mail to lucid-info@lucid.com for
more information about Energize, or about Lucid Emacs support.")
		   ;; with the new Fwhere_is_internal(), this takes 0.02 secs.
		   (insert (substitute-command-keys
       "\n\nType \\[help-command] for help; \\[advertised-undo] to undo changes.  (`C-' means use CTRL key.)
To kill the Emacs job, type \\[save-buffers-kill-emacs].
Type \\[help-with-tutorial] for a tutorial on using Emacs.
Type \\[info] to enter Info, which you can use to read GNU documentation.

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for full details.
You may give out copies of Emacs; type \\[describe-copying] to see the conditions.
Type \\[describe-distribution] for information on getting the latest version."))
		   (fontify-copyleft)
		   (set-buffer-modified-p nil)
		   (sit-for startup-message-timeout))
	       (save-excursion
		 ;; In case the Emacs server has already selected
		 ;; another buffer, erase the one our message is in.
		 (set-buffer (get-buffer "*scratch*"))
		 (erase-buffer)
		 (set-buffer-modified-p nil)))))
    (parse-command-line)))

;;; Separated from above so we can call it later if another editor
;;; tries to start up but yields via the SPARCworks yielding protocol.
;;; If that happens, the remaining editor gets a callback invoked
;;; that imbeds the command line from the yielding editor.  So we
;;; process the yielding editor's command line.
(defun parse-command-line ()
  (let ((dir default-directory)
	(file-count 0)
	first-file-buffer
	(line nil))
    (while command-line-args-left
      (let ((argi (car command-line-args-left))
	    tem)
	(setq command-line-args-left (cdr command-line-args-left))
	(or (cond (line 
		   nil)
		  ((setq tem (or (assoc argi command-switch-alist)
				 (and (string-match "\\`--" argi)
				      (assoc (substring argi 1)
					     command-switch-alist))))
		   (funcall (cdr tem) argi)
		   t)
		  ((string-match "\\`\\+[0-9]+\\'" argi)
		   (setq line (string-to-int argi))
		   t)
		  ((or (equal argi "-") (equal argi "--"))
		   ;; "- file" means don't treat "file" as a switch
		   ;;  ("+0 file" has the same effect; "-" added
		   ;;   for unixoidiality.)
		   ;; This is worthless; the `unixoid' way is "./file". -jwz
		   (setq line 0))
		  (t
		   nil))
	    (progn
	      (setq file-count (1+ file-count))
	      (setq argi (expand-file-name argi dir))
	      (if (= file-count 1)
		  (setq first-file-buffer (progn (find-file argi)
						 (current-buffer)))
		(if noninteractive
		    (find-file argi)
		  (find-file-other-window argi)))
	      (goto-line (or line 0))
	      (setq line nil)))))
    ;; If 3 or more files visited, and not all visible,
    ;; show user what they all are.
    (if (and (not noninteractive)
	     (> file-count 2))
	(or (get-buffer-window first-file-buffer)
	    (progn (other-window 1)
		   (buffer-menu nil))))))

(defvar yielding-args-to-ignore-alist '(("-q" . 0)
					("-no-init-file" . 0)
					("-user" . 1)
					("-u" . 1)
					("-nw" . 0))
  "List of args to NOT pass onto running emacs before exiting.  SPARCworks only.")

;;; Lop off the switches that make no sense to pass onto a running emacs.
(defun yielding-parse-command-line ()
  (let ((args command-line-args-left)
	(new-args nil))
    (while args
      (let ((argi (car args))
	    tem)
	(setq args (cdr args))
	(cond ((setq tem (assoc argi yielding-args-to-ignore-alist))
	       (setq args (nthcdr (cdr tem) args)))
	      (t
	       (setq new-args (cons argi new-args))))))
    (setq command-line-args-left (nreverse new-args)))
  (parse-command-line))


(defun find-emacs-root-internal (path)
  (let ((dir (file-name-directory path))
	(name (file-name-nondirectory path)))
    (or
     ;;
     ;; If this directory is a plausible root of the emacs tree, return it.
     ;;
     (and (file-directory-p (expand-file-name "lisp/prim" dir))
	  (file-directory-p (expand-file-name "etc" dir))
	  dir)
     ;;
     ;; If the parent of this directory is a plausible root, use it.
     ;; (But don't do so recursively!)
     ;;
     (and (file-directory-p (expand-file-name "../lisp/prim" dir))
	  (file-directory-p (expand-file-name "../etc" dir))
	  (expand-file-name "../" dir))
     ;;
     ;; If that doesn't work, and the emacs executable is a symlink, then
     ;; chase the link and try again there.
     ;;
     (and (setq path (file-symlink-p path))
	  (find-emacs-root-internal (expand-file-name path dir)))
     ;;
     ;; Otherwise, this directory just doesn't cut it.
     ;; Some bozos think they can use the 18.59 lisp directory with 19.*.
     ;; This is because they're not using their brains.  But it might be
     ;; nice to notice that that is happening and point them in the
     ;; general direction of a clue.
     ;;
     nil)))


(defun set-default-load-path ()
  (setq execution-path
	;; don't let /tmp_mnt/... get into the load-path or exec-path.
	(abbreviate-file-name execution-path))

  (let* ((root (find-emacs-root-internal execution-path))
	 (lisp (and root (expand-file-name "lisp" root)))
	 (etc  (and root (expand-file-name "etc" root)))
	 (lock (and root (boundp 'lock-directory)
		    (file-name-as-directory
		     (or lock-directory (expand-file-name "lock" root))))))
    (if lisp
	(or (member lisp load-path)
	    (progn
	      ;; If the lisp dir isn't on the load-path, add it to the end.
	      (setq load-path (append load-path (list lisp)))
	      ;; If the lisp dir wasn't on the load-path, then also add any
	      ;; direct subdirectories of the lisp directory to the load-path.
	      ;; But don't add dirs whose names begin with dot or hyphen.
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
    (if (and (null data-directory) etc)
	(setq data-directory (file-name-as-directory etc)))
    ;; Default the info dir to being a sibling of the data-directory.
    (if (or (not (boundp 'Info-directory-list)) (null Info-directory-list))
	(setq Info-directory-list
	      (list (expand-file-name "../info/" data-directory))))
    ;; Default the lock dir to being a sibling of the data-directory.
    ;; If superlock isn't set, derive it from the lock dir.
    (if (boundp 'lock-directory)
	(progn
	  (setq lock-directory lock)
	  (if (and lock-directory (null superlock-path))
	      (setq superlock-path
		    (concat lock-directory "!!!SuperLock!!!")))))
    (set-default-load-path-warning)))


(defun set-default-load-path-warning ()
  (let ((lock (if (boundp 'lock-directory) lock-directory 't))
	(fill-column 70)
	warnings message guess)
    (if (and (stringp lock) (not (file-directory-p lock)))
	(setq lock nil))
    (cond
     ((not (and exec-directory load-path lock))
      (save-excursion
	(set-buffer (get-buffer-create " *warning-tmp*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(if (null lock)
	    (setq warnings (cons "lock-directory" warnings)))
	(if (null exec-directory)
	    (setq warnings (cons "exec-directory" warnings)))
        ;>>>> data-directory
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
			(and (string-match "/[^/]+\\'" execution-path)
			     (substring execution-path 0
					(match-beginning 0)))))
	(if (and guess (string-match "/\\(src\\|etc\\|lisp\\)/?\\'" guess))
	    (setq guess (substring guess 0 (match-beginning 0))))
	(if (and guess (string-match "/\\'" guess))
	    (setq guess (substring guess 0 (match-beginning 0))))

        ;>>>> data-directory
	(if (or (null exec-directory) (null load-path))
	    (insert
	     "\n\nWithout both exec-directory and load-path, emacs will "
	     "be very broken.  "))
	(if (and (null exec-directory) guess)
	    (insert
	     "Consider making a symbolic link from " guess
	     "/etc to wherever the appropriate emacs etc directory is"))
	(if (and (null load-path) guess)
	    (insert
	     (if exec-directory "Consider making a symbolic link " ", and ")
	     "from " guess
	     "/lisp to wherever the appropriate emacs lisp library is.  ")
	  (if (and (null exec-directory) guess) (insert ".")))

	(if (null lock)
	    (progn
	      (insert
	       "\n\nWithout lock-directory set, file locking won't work.  ")
	      (if guess
		  (insert
		   "Consider creating " guess "/lock as a directory or "
		   "symbolic link for use as the lock directory.  "
		   "(This directory must be globally writable.)"))))

        (if (fboundp 'fill-region)
            ;; Might not be bound in the cold load environment...
	    (fill-region (point-min) (point-max)))
	(goto-char (point-min))
	(princ "\nWARNING:\n" 'external-debugging-output)
	(princ (buffer-string) 'external-debugging-output)
	(erase-buffer)
	t)))))

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
		 'italic))
	   (goto-char (point-min))
	   (and (search-forward "Energize Programming System" nil t)
		(set-extent-face
		 (make-extent (match-beginning 0) (match-end 0))
		 'italic))
	   (and (re-search-forward "[-A-Za-z_]+@lucid\\.com" nil t)
		(set-extent-face
		 (make-extent (match-beginning 0) (match-end 0))
		 'italic))
	   ))))
