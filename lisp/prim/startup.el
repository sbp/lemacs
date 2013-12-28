;;; startup.el --- process Emacs shell arguments

;; Copyright (C) 1985, 1986, 1990, 1992, 1993, 1994
;; Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;;; Code:

(defun command-line-do-help (arg)
  "Print this message and exit."
  (let ((standard-output 'external-debugging-output))
    (cond ((featurep 'sunpro)
	   (princ (era-version))
	   (princ "\n")))
    (princ (emacs-version))
    (princ "\n\n")
    (cond ((fboundp 'x-create-screen)
	   (princ (if (featurep 'sunpro) "XEmacs" "Lucid Emacs"))
	   (princ " accepts all standard X Toolkit command line options.\
  In addition,\nthe "))
	  (t (princ "The ")))
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
  -unmapped             Do not map the initial screen.
  -no-site-file         Do not load the site-specific init file (site-run.el).
  -no-init-file         Do not load the user-specific init file (~/.emacs).
  -q                    Same as -no-init-file.
  -user <user>          Load user's init file instead of your own.
  -u <user>             Same as -user.")

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
  ;; Get rid of the prefixes added by the automounter.
  ;(if (and (string-match automount-dir-prefix filename)
  ;         (file-exists-p (file-name-directory
  ;                         (substring filename (1- (match-end 0))))))
  ;    (setq filename (substring filename (1- (match-end 0)))))
  (let ((tail directory-abbrev-alist))
    ;; If any elt of directory-abbrev-alist matches this name,
    ;; abbreviate accordingly.
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
        ;; If FILENAME starts with the abbreviated homedir,
        ;; make it start with `~' instead.
	(if (and (string-match abbreviated-home-dir filename)
                 ;; If the home dir is just /, don't change it.
                 (not (and (= (match-end 0) 1) ;>>> unix-specific
                           (= (aref filename 0) ?/))))
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

(defconst startup-message-timeout 120)

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
  "Functions to call after handling urgent options but before loading init file.")

(defvar after-init-hook nil
  "Functions to call after loading the init file (`~/.emacs').")

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

(defvar site-start-file (purecopy "site-start")
  "File containing site-wide run-time initializations.
This file is loaded at run-time before `~/.emacs'.  It contains inits
that need to be in place for the entire site, but which, due to their
higher incidence of change, don't make sense to load into emacs'
dumped image.  Thus, the run-time load order is: 1. file described in
this variable, if non-nil; 2. `~/.emacs'; 3. `default.el'.")

(defvar init-file-debug nil)

(defvar init-file-had-error nil)

(defvar initial-screen-unmapped-p nil)

(defvar command-line-args-left) ; bound by `command-line'


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
  (if (featurep 'sunpro)
      (princ (concat (era-version) "\n") 'external-debugging-output))
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

;;; Processing the command line and loading various init files

(defun early-error-handler (&rest debugger-args)
  ;; Used as the debugger during emacs initialization; if an error occurs,
  ;; print some diagnostics, and kill emacs.
  (let ((string "Initialization error")
	(error (nth 1 debugger-args))
	(debug-on-error nil)
	(stream 'external-debugging-output))
    (if (null error)
	(princ string stream)
      (princ (concat "\n" string ": ") stream)
      (condition-case ()
	  (display-error error stream)
	(error (princ "<<< error printing error message >>>" stream)))
      (princ "\n" stream)
      (if (memq (car-safe error) '(void-function void-variable))
	  (princ "
	This probably means that lemacs is picking up an old version of
	the lisp library, or that some .elc files are not up-to-date.\n"
		 stream)))
    (let ((print-length 1000)
	  (print-level 1000)
	  (print-escape-newlines t)
	  (print-readably nil))
      (if (getenv "EMACSLOADPATH")
	  (princ (format "\n$EMACSLOADPATH is %s" (getenv "EMACSLOADPATH"))
		 stream))
      (princ (format "\nexec-directory is %S" exec-directory) stream)
      (princ (format "\ndata-directory is %S" data-directory) stream)
      (princ (format "\nload-path is %S" load-path) stream)
      (princ "\n\n" stream))
    (backtrace stream t))
  (kill-emacs -1))

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
      ;; Do this again, in case .emacs defined more abbreviations.
      (setq default-directory (abbreviate-file-name default-directory))
      (run-hooks 'emacs-startup-hook)
      (run-hooks 'term-setup-hook)
      (setq term-setup-hook nil)
      (run-hooks 'window-setup-hook)
      (setq window-setup-hook nil))))

(defun command-line-early ()
  ;; This processes those switches which need to be processed before
  ;; starting up the window system.

  ;; See if we should import version-control from the environment variable.
  (let ((vc (getenv "VERSION_CONTROL")))
    (cond ((eq vc nil))			;don't do anything if not set
	  ((or (string= vc "t")
	       (string= vc "numbered"))
	   (setq version-control t))
	  ((or (string= vc "nil")
	       (string= vc "existing"))
	   (setq version-control nil))
	  ((or (string= vc "never")
	       (string= vc "simple"))
	   (setq version-control 'never))))
  (let ((done nil))
    ;; Figure out which user's init file to load,
    ;; either from the environment or from the options.
    (setq init-file-user (if (noninteractive) nil (user-login-name)))
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
              ((string-equal argi "-debug-init")
               (setq init-file-debug t
                     command-line-args-left (cdr command-line-args-left)))
              ((string-equal argi "-unmapped")
               (setq initial-screen-unmapped-p t
                     command-line-args-left (cdr command-line-args-left)))
 	      (t (setq done t)))))))


(defun command-line ()
  (let ((command-line-args-left (cdr command-line-args)))

    (let ((debugger 'early-error-handler)
	  (debug-on-error t))
      (set-default-load-path)

      ;; Process magic command-line switches like -q and -u.  Do this
      ;; before creating the first screen because some of these switches
      ;; may affect that.  I think it's ok to do this before establishing
      ;; the X connection, and maybe someday things like -nw can be
      ;; handled here instead of down in C.
      (command-line-early)

      ;; Read window system's init file if using a window system.
      (if (and window-system (not noninteractive))
	  (load (concat term-file-prefix
			(symbol-name window-system)
			"-win")
		;; Every window system should have a startup file;
		;; barf if we can't find it.
		nil t))

      ;; Under a window system, this creates the first visible screen,
      ;; and deletes the stdio screen.
      (screen-initialize)
      )

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

    ;; Load library for our terminal type.
    ;; User init file can set term-file-prefix to nil to prevent this.
    (and term-file-prefix
	 (not (noninteractive))
	 (not window-system)
         (let ((term (getenv "TERM"))
               hyphend)
           (while (and term
                       (not (load (concat term-file-prefix term) t t)))
             ;; Strip off last hyphen and what follows, then try again
             (if (setq hyphend (string-match "[-_][^-_]+\\'" term))
                 (setq term (substring term 0 hyphend))
                 (setq term nil)))))

    ;; Process the remaining args.
    (command-line-1)
    
    ;; If -batch, terminate after processing the command options.
    (if (noninteractive) (kill-emacs t))))


;;; Load user's init file and default ones.
(defun load-init-file ()
  (run-hooks 'before-init-hook)

  ;; Run the site-start library if it exists.  The point of this file is
  ;; that it is run before .emacs.  There is no point in doing this after
  ;; .emacs; that is useless.
  (if site-start-file
      (load site-start-file t t))

  ;; Sites should not disable this.  Only individuals should disable
  ;; the startup message.
  (setq inhibit-startup-message nil)

  ;; Load that user's init file, or the default one, or none.
  (let ((load-init-file
         #'(lambda ()
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
			      (load "default" t t))))))))
    (if init-file-debug
        (let ((debug-on-error t))
          (funcall load-init-file))
        (condition-case error
            (funcall load-init-file)
          (error
           (message "Error in init file: ")
           (display-error error nil)))))

  (run-hooks 'after-init-hook)
  nil)

(defun command-line-1 ()
  (if (null command-line-args-left)
      (cond ((and (not inhibit-startup-message) (not (noninteractive))
		  ;; Don't clobber a non-scratch buffer if init file
		  ;; has selected it.
		  (string= (buffer-name) "*scratch*")
		  (not (input-pending-p)))

	     ;; If there are no switches to process, run the term-setup-hook
	     ;; before displaying the copyright notice; there may be some need
	     ;; to do it before doing any output.  If we're not going to
	     ;; display a copyright notice (because other options are present)
	     ;; then this is run after those options are processed.
	     (run-hooks 'term-setup-hook)
	     ;; Don't let the hook be run twice.
	     (setq term-setup-hook nil)

	     (unwind-protect
		 (progn
		   (insert (emacs-version) "\n")
		   (if (featurep 'sunpro)
		       (insert (era-version) "\n"))
		   (insert
"Copyright (C) 1985-1990 Free Software Foundation, Inc.
Copyright (C) 1990-1994 Lucid, Inc.
Copyright (C) 1993-1994 Sun Microsystems, Inc.")
		   (if (not (featurep 'sunpro))
		       (insert "\n
This version of Emacs is a part of Lucid's Energize Programming System,
a C/C++ development environment.  Send mail to lucid-info@lucid.com for
more information about Energize, or about Lucid Emacs support."))
		   ;; with the new Fwhere_is_internal(), this takes 0.02 secs.
		   (insert (substitute-command-keys
       "\n\nType \\[help-command] for help; \\[advertised-undo] to undo changes.  (`C-' means use the CTRL key.)
To get out of Emacs, type \\[save-buffers-kill-emacs].
Type \\[help-with-tutorial] for a tutorial on using Emacs.
Type \\[info] to enter Info, which you can use to read documentation.

GNU Emacs comes with ABSOLUTELY NO WARRANTY; type \\[describe-no-warranty] for full details.
You may give out copies of Emacs; type \\[describe-copying] to see the conditions.
Type \\[describe-distribution] for information on getting the latest version."))
		   (insert "\n\n")
		   (let ((p (point))
			 (fill-column 76))
		     (insert
		      "For customization examples, see the files "
		      (expand-file-name "sample.emacs" data-directory)
		      " and "
		      (expand-file-name "sample.Xdefaults" data-directory)
		      ".\n")
		     (fill-region p (point)))
		   (fontify-copyleft)
		   (set-buffer-modified-p nil)
;		   (or (pos-visible-in-window-p (point-min))
		       (goto-char (point-min))
;		       )
		   (sit-for startup-message-timeout))
	       (save-excursion
		 ;; In case the Emacs server has already selected
		 ;; another buffer, erase the one our message is in.
		 (set-buffer (get-buffer "*scratch*"))
		 (erase-buffer)
		 (set-buffer-modified-p nil)))))
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
		     (buffer-menu nil)))))))

;; Some really hairy stuff because I'm in a hurry.  -Ben
;; Good thing the condition-cas is there, because this shit doesn't 
;; work --at all-- on a vanilla MIT R6 system.  Gee.  Might it have
;; been a mistake to try to do this the day before the release? -Jamie
(defvar insert-xemacs-logo-indent 30)
(defun insert-xemacs-logo-real ()
  (cond (t ;(not (find-face 'xemacs-logo))
	 ;; maybe you want to try and make this be in a font other than
	 ;; the default one, but some of those fonts you were trying don't
	 ;; exist on my system, so you'd better arrange for the sizing to
	 ;; be done by something that knows what sizes exist.  Or just 
	 ;; make the damn thing a bitmap...  (This method also means that
	 ;; the (big) font will be kept loaded forever, even though we only
	 ;; need it once/occasionally.) --jwz
	 (make-face 'xemacs-logo)
	 (let (font)
	   ;; first try some fonts that look good ...
	   (setq font
		 (or (try-font "-*-palatino-bold-i-*-*-*-600-*-*-*-*-*-*")
		     (try-font "-*-helvetica-bold-o-*-*-*-600-*-*-*-*-*-*")))
	   (cond
	    (font (setq insert-xemacs-logo-indent 22))
	    (t
	     (setq font
		   (or 
		    (try-font "-*-palatino-bold-i-*-*-*-400-*-*-*-*-*-*")
		    (try-font "-*-helvetica-bold-o-*-*-*-400-*-*-*-*-*-*")))
	     (cond
	      (font (setq insert-xemacs-logo-indent 24)))))
	   (cond
	    (font (set-face-font 'xemacs-logo font))
	    (t
	     ;; or revert to the default-font if those couldn't be found
	     (copy-face 'default 'xemacs-logo)
	     (make-face-bold 'xemacs-logo)
	     (condition-case nil
		 (progn ; this should not error! -jwz
		   (make-face-larger 'xemacs-logo)
		   (make-face-larger 'xemacs-logo)
		   (make-face-larger 'xemacs-logo)
		   (make-face-larger 'xemacs-logo))
	       (error nil))
	     (setq insert-xemacs-logo-indent 32))))))
  (indent-to insert-xemacs-logo-indent)
  (let ((p (point)))
    (insert "XEmacs")
    (set-extent-face (make-extent p (point)) 'xemacs-logo)))

;; more blood, guts, and gore.
(defun insert-xemacs-logo ()
  (condition-case nil
      (insert-xemacs-logo-real)
    (error 
     (indent-to 32)
     (insert "XEmacs"))))

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

	   ;; Stick the politically correct logo on the front.
	   (goto-char (point-min))
	   (insert "\n")
	   (cond ((featurep 'sunpro)
		  (insert-xemacs-logo)
		  (insert "\n")
		  (indent-to 24)
		  (insert "(also known as Lucid Emacs)"))
		 (t
		  (insert "\t")
		  (set-extent-begin-glyph (make-extent (point) (point))
					  lucid-logo)
		  (insert "\n\n")
		  (indent-to 26)
		  (insert "(also known as XEmacs)")))
	   (insert "\n\n")))))


;;;; Computing the default load-path, etc.
;;;
;;; This stuff is a complete mess and isn't nearly as general as it 
;;; thinks it is.  It should be rethunk.  In particular, too much logic
;;; is duplicated between the code that looks around for the various
;;; directories, and the code which suggests where to create the various
;;; directories once it decides they are missing.

;;; The source directory has this layout:
;;;
;;;    BUILD_ROOT/src/lemacs*			  argv[0]
;;;    BUILD_ROOT/lemacs*			  argv[0], possibly
;;;    BUILD_ROOT/lisp/
;;;    BUILD_ROOT/etc/				  data-directory
;;;    BUILD_ROOT/info/
;;;    BUILD_ROOT/lib-src/			  exec-directory
;;;    BUILD_ROOT/lock/
;;;
;;; The default tree created by "make install" has this layout:
;;;
;;;    PREFIX/bin/lemacs*	  		argv[0]
;;;    PREFIX/lib/lemacs-VERSION/lisp/
;;;    PREFIX/lib/lemacs-VERSION/etc/		  data-directory
;;;    PREFIX/lib/lemacs-VERSION/info/
;;;    PREFIX/lib/lemacs-VERSION/CONFIGURATION/	  exec-directory
;;;    PREFIX/lib/lemacs/lock/
;;;    PREFIX/lib/lemacs/site-lisp/
;;;
;;; The binary packages we ship have that layout, except that argv[0] has
;;; been moved one level deeper under the bin directory:
;;;
;;;    PREFIX/bin/CONFIGURATION/lemacs*
;;;
;;; The following code has to deal with at least the above three situations,
;;; and it should be possible for it to deal with more.  Though perhaps that
;;; does cover it all?  The trick is, when something is missing, realizing
;;; which of those three layouts is mostly in place, so that we can suggest
;;; the right directories in the error message.


;; extremely low-tech debugging, since this happens so early in startup.
;(or (fboundp 'orig-file-directory-p)
;    (fset 'orig-file-directory-p (symbol-function 'file-directory-p)))
;(defun file-directory-p (path)
;  (send-string-to-terminal (format "PROBING %S" path))
;  (let ((v (orig-file-directory-p path)))
;    (send-string-to-terminal (format " -> %S\n" v))
;    v))

(defun startup-make-version-dir ()
  (let ((version (and (string-match "\\`[^0-9]*\\([0-9]+\\.[0-9]+\\)"
				    emacs-version)
		      (substring emacs-version
				 (match-beginning 1) (match-end 1)))))
    (if (string-match "(beta *\\([0-9]+\\))" emacs-version)
	(setq version (concat version "-b"
			      (substring emacs-version (match-beginning 1)
					 (match-end 1)))))
    (concat "lib/lemacs-" version)))


(defun find-emacs-root-internal (path)
;;  (send-string-to-terminal (format "FINDING ROOT FOR %S\n" path))
  (let ((dir (file-name-directory path)))
    (or
     ;;
     ;; If this directory is a plausible root of the emacs tree, return it.
     ;;
     (and (file-directory-p (expand-file-name "lisp/prim" dir))
	  (or (file-directory-p (expand-file-name "lib-src" dir))
	      (file-directory-p (expand-file-name system-configuration dir)))
	  dir)
     ;;
     ;; If the parent of this directory is a plausible root, use it.
     ;; (But don't do so recursively!)
     ;;
     (and (file-directory-p (expand-file-name "../lisp/prim" dir))
	  (or (file-directory-p (expand-file-name
				 (format "../%s" system-configuration)
				 dir))
	      (file-directory-p (expand-file-name "../lib-src" dir)))
	  (expand-file-name "../" dir))

     ;;
     ;; If ../lib/lemacs-<version> exists check it.
     ;; This is of the form "lemacs-19.10/" or "lemacs-19.10-b7/".
     ;;
     (let ((ver-dir (concat "../" (startup-make-version-dir))))
       (and (file-directory-p (expand-file-name
			       (format "%s/lisp/prim" ver-dir)
			       dir))
	    (or (file-directory-p (expand-file-name
				   (format "%s/%s" ver-dir system-configuration)
				   dir))
		(file-directory-p (expand-file-name
				   (format "%s/lib-src" ver-dir)
				   dir)))
	    (expand-file-name (file-name-as-directory ver-dir) dir)))
     ;;
     ;; Same thing, but one higher: ../../lib/lemacs-<version>.
     ;;
     (let ((ver-dir (concat "../../" (startup-make-version-dir))))
       (and (file-directory-p (expand-file-name
			       (format "%s/lisp/prim" ver-dir)
			       dir))
	    (or (file-directory-p (expand-file-name
				   (format "%s/%s" ver-dir system-configuration)
				   dir))
		(file-directory-p (expand-file-name
				   (format "%s/lib-src" ver-dir)
				   dir)))
	    (expand-file-name (file-name-as-directory ver-dir) dir)))
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
	 (site-lisp (and root
			 (or
			  (let ((f (expand-file-name "lemacs/site-lisp" root)))
			    (and (file-directory-p f) f))
			  (let ((f (expand-file-name "../lemacs/site-lisp"
						     root)))
			    (and (file-directory-p f) f)))))
	 (lib-src (and root
		       (or
			(let ((f (expand-file-name "lib-src" root)))
			  (and (file-directory-p f) f))
			(let ((f (expand-file-name system-configuration root)))
			  (and (file-directory-p f) f)))))
	 (etc  (and root
		    (let ((f (expand-file-name "etc" root)))
		      (and (file-directory-p f) f))))
	 (info (and root
		    (let ((f (expand-file-name "info" root)))
		      (and (file-directory-p f) (file-name-as-directory f)))))
	 (lock (and root
		    (boundp 'lock-directory)
		    (if (and lock-directory (file-directory-p lock-directory))
			(file-name-as-directory lock-directory)
		      (or
		       (let ((f (expand-file-name "lemacs/lock" root)))
			 (and (file-directory-p f)
			      (file-name-as-directory f)))
		       (let ((f (expand-file-name "../lemacs/lock" root)))
			 (and (file-directory-p f)
			      (file-name-as-directory f)))
		       (let ((f (expand-file-name "lock" root)))
			 (and (file-directory-p f)
			      (file-name-as-directory f)))
		       ;; if none of them exist, make the "guess" be the one that
		       ;; set-default-load-path-warning will suggest.
		       (file-name-as-directory
			(expand-file-name "../lemacs/lock" root))
		       )))))
    (if lisp
	(progn
	  ;; If the lisp dir isn't on the load-path, add it to the end.
	  (or (member lisp load-path)
	      (setq load-path (append load-path (list lisp))))
	  ;; Also add any direct subdirectories of the lisp directory
	  ;; to the load-path.  But don't add dirs whose names begin
	  ;; with dot or hyphen.
	  (let ((files (directory-files lisp nil "^[^-.]" nil 'dirs-only))
		file)
	    (while files
	      (setq file (car files))
	      (if (and (not (member file '("RCS" "CVS" "SCCS")))
		       (setq file (expand-file-name file lisp))
		       (not (member file load-path)))
		  (setq load-path
			(nconc load-path
			       (list (file-name-as-directory file)))))
	      (setq files (cdr files))))
	  ))
    ;; add site-lisp dir to load-path
    (if site-lisp
	(progn
	  ;; If the site-lisp dir isn't on the load-path, add it to the end.
	  (or (member site-lisp load-path)
	      (setq load-path (append load-path (list site-lisp))))
	  ;; Also add any direct subdirectories of the site-lisp directory
	  ;; to the load-path.  But don't add dirs whose names begin
	  ;; with dot or hyphen.
	  (let ((files (directory-files site-lisp nil "^[^-.]" nil 'dirs-only))
		file)
	    (while files
	      (setq file (car files))
	      (if (and (not (member file '("RCS" "CVS" "SCCS")))
		       (setq file (expand-file-name file site-lisp))
		       (not (member file load-path)))
		  (setq load-path
			(nconc load-path
			       (list (file-name-as-directory file)))))
	      (setq files (cdr files))))
	  ))

    ;; If running from the build directory, always prefer the exec-directory
    ;; that is here over the one that came from paths.h.
    (if (or (and (null exec-directory) lib-src)
	    (and (equal lib-src (expand-file-name "lib-src" root))
		 (not (equal exec-directory lib-src))))
	(setq exec-directory (file-name-as-directory lib-src)))

    (if exec-directory
	(or (member exec-directory exec-path)
	    (setq exec-path (append exec-path (list exec-directory)))))
    (if (or (and (null data-directory) etc)
	    (and (equal etc (expand-file-name "etc" root))
		 (not (equal data-directory etc))))
	(setq data-directory (file-name-as-directory etc)))

    ;; If `configure' specified an info dir, use it.
    (or (boundp 'Info-directory-list) (setq Info-directory-list nil))
    (cond (configure-info-directory
	   (setq configure-info-directory (file-name-as-directory
					   configure-info-directory))
	   (or (member configure-info-directory Info-directory-list)
	       (setq Info-directory-list
		     (append Info-directory-list
			     (list configure-info-directory))))))
    ;; If we've guessed the info dir, use that (too).
    (if (and info (not (member info Info-directory-list)))
	(setq Info-directory-list (append Info-directory-list (list info))))

    ;; Default the lock dir to being a sibling of the data-directory.
    ;; If superlock isn't set, or is set to a file in a nonexistent
    ;; directory, derive it from the lock dir.
    (if (boundp 'lock-directory)
	(progn
	  (setq lock-directory lock)
	  (cond ((null lock-directory)
		 (setq superlock-path nil))
		((or (null superlock-path)
		     (not (file-directory-p
			   (file-name-directory superlock-path))))
		 (setq superlock-path
		       (expand-file-name "!!!SuperLock!!!"
					 lock-directory))))))

    (set-default-load-path-warning)))


(defun set-default-load-path-warning ()
  (let ((lock (if (boundp 'lock-directory) lock-directory 't))
	warnings message guess)
    (if (and (stringp lock) (not (file-directory-p lock)))
	(setq lock nil))
    (cond
     ((not (and exec-directory data-directory load-path lock))
      (save-excursion
	(set-buffer (get-buffer-create " *warning-tmp*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(if (null lock)
	    (setq warnings (cons "lock-directory" warnings)))
	(if (null exec-directory)
	    (setq warnings (cons "exec-directory" warnings)))
	(if (null data-directory)
	    (setq warnings (cons "data-directory" warnings)))
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
			data-directory
			(car load-path)
			(and (string-match "/[^/]+\\'" execution-path)
			     (substring execution-path 0
					(match-beginning 0)))))
	(if (and guess
		 (or
		  ;; parent of a terminal bin/<configuration> pair (hack hack.)
		  (string-match (concat "/bin/"
					(regexp-quote system-configuration)
					"/?\\'")
				guess)
		  ;; parent of terminal src, lib-src, etc, or lisp dir.
		  (string-match "/\\(bin\\|src\\|lib-src\\|etc\\|lisp\\)[^/]*/?\\'"
				guess)))
	    (setq guess (substring guess 0 (match-beginning 0))))

	;; If neither the exec nor lisp dirs are around, then "guess" that
	;; the new configure-style lib dir should be used.  Otherwise, if
	;; only one of them appears to be missing, or it's just lock,
	;; then guess it to be a sibling of whatever already exists.
	(if (and (null exec-directory) (null load-path))
	    (setq guess (expand-file-name (startup-make-version-dir) guess)))

	(if (or (null exec-directory) (null load-path))
	    (insert
	     "\n\nWithout both exec-directory and load-path, emacs will "
	     "be very broken.  "))
	(if (and (null exec-directory) guess)
	    (insert
	     "Consider making a symbolic link from "
	     (expand-file-name system-configuration guess)
	     " to wherever the appropriate emacs exec-directory directory is"))
	(if (and (null data-directory) guess)
	    (insert
	     (if exec-directory "\n\nConsider making a symbolic link " ", and ")
	     "from "
	     ;; Oops, free ref to `lisp'...
	     (expand-file-name "etc" (if lisp
					 (file-name-directory
					  (directory-file-name lisp))
				       guess))
	     ;; But I'm not entirely sure this is the same and don't want to
	     ;; deal with it right now (car load-path) vs (last load-path).
;	     (expand-file-name "etc"
;			       (if load-path
;				   (file-name-directory
;				    (directory-file-name (car load-path)))
;				 guess))
	     " to wherever the appropriate emacs data-directory is"))
	(if (and (null load-path) guess)
	    (insert
	     (if (and exec-directory data-directory)
		 "Consider making a symbolic link "
	       ", and ")
	     "from "
	     (expand-file-name "lisp" guess)
	     " to wherever the appropriate emacs lisp library is"))
	(insert ".")

	(if (null lock)
	    (progn
	      (insert
	       "\n\nWithout lock-directory set, file locking won't work.  ")
	      (if guess
		  (insert
		   "Consider creating "
		   (expand-file-name "../lemacs/lock"
				     (or (find-emacs-root-internal execution-path)
					 guess))
		   " as a directory or symbolic link for use as the lock "
		   "directory.  (This directory must be globally writable.)"
		   ))))

        (if (fboundp 'fill-region)
            ;; Might not be bound in the cold load environment...
	    (let ((fill-column 76))
	      (fill-region (point-min) (point-max))))
	(goto-char (point-min))
	(princ "\nWARNING:\n" 'external-debugging-output)
	(princ (buffer-string) 'external-debugging-output)
	(erase-buffer)
	t)))))


;;; startup.el ends here
