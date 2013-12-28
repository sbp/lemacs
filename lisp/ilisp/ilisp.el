;;; -*-Emacs-Lisp-*-
;;;%Header
;;; Inferior LISP interaction package for GNU Emacs.  Version 4.00
;;; Copyright (C) 1990 Chris McConnell, ccm@cs.cmu.edu.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; This replaces the standard inferior-lisp mode.  It is based on
;;; comint mode and derived from a number of different interfaces
;;; including Symbolics, cmulisp, and Thinking Machines.  Thanks also
;;; to Todd Kaufmann, Neil Smithline, David Braunegg, Fred White, Jim
;;; Healy, Hans Chalupsky, David Duff, Dan Pierson, Michael Kashket,
;;; Jamie Zawinski and Brian Dennis for bug reports, suggestions and
;;; code.  Please send bug reports, fixes and extensions to
;;; ccm@cs.cmu.edu so I can merge them into the master source.

;;; This file defines a generic LISP interface that can be customized
;;; to match a specific LISP dialect.  Support is already provided for
;;; a number of common LISP dialects.  Lucid, Allegro and CMU are
;;; fully supported.  Other LISP dialects are missing features like
;;; arglist and find-source.

;;; Since this is built on top of the general command-interpreter-in-
;;; a-buffer mode (comint mode), it shares a common base
;;; functionality, and a common set of bindings, with all modes
;;; derived from comint mode.  This makes it easier to use.

;;; For documentation on the functionality provided by comint mode,
;;; and the hooks available for customising it, see the file
;;; comint.el.

;;; Throughout this file you will find comment lines with %'s on them.
;;; These lines define sections for outline mode which I use while
;;; programming to temporarily hide code.

;;;%%SITE INFORMATION
;;; The files you need to use ilisp are:
;;;  symlink.el      Expand pathnames resolving links.
;;;  completer.el    Partial completion code.
;;;  popper.el       Shrink-wrapped temporary windows.
;;;  epoch-pop.el    Popper for epoch.
;;;  comint.el       The basic comint abstraction.
;;;  comint-ipc.el   Extensions for sending commands and getting results.
;;;  ilisp-ext.el    Standalone lisp-mode extensions.
;;;  ilisp-src.el    Ilisp source code module.
;;;  ilisp-bat.el    Ilisp batch code module.
;;;  ilisp.el        Actual ilisp definitions.
;;;  *.lisp          Each dialect will have one of these files.
;;;
;;; All of the .el files should be byte-compiled by typing C-u M-x
;;; byte-recompile-directory.  Once a lisp dialect is started up, you
;;; should execute the command ilisp-compile-inits which will compile
;;; the *.lisp files and write them to the same directory as the ilisp
;;; files.  An alternative is to not compile the interface files.
;;; This will work although initialization will be slower.  The ILISP
;;; interface files can also be built into the LISP image if desired.
;;;
;;; There is also an ilisp-site-hook for initializing site specific
;;; stuff like program locations when ilisp is first loaded.  You may
;;; also want to define appropriate autoloads in your system Emacs
;;; start up file.
;;;
;;; ;;; CMU site
;;; (setq ilisp-site-hook
;;;       '(lambda ()
;;;         (setq ilisp-motd "CMU ILISP V%s")
;;;         (setq expand-symlinks-rfs-exists t)
;;;         (add-hook 'allegro-hook
;;;          '(lambda ()
;;;            (setq ilisp-program "/usr/misc/.allegro/bin/cl")))
;;;         (add-hook 'lucid-hook
;;;          '(lambda ()
;;;            (setq ilisp-program "/usr/misc/.lucid/bin/lisp")))))

;;;%% YOUR .EMACS FILE
;;;============================================================================
;;; Some suggestions for your .emacs file.
;;;
;;; If ilisp lives in some non-standard directory, you must tell emacs
;;; where to get it. This may or may not be necessary.
;;; (setq load-path (cons (expand-file-name "~jones/emacs/ilisp/") load-path))
;;;
;;; ;;; If you always want partial minibuffer completion
;;; (require 'completer)
;;;
;;; ;;; If you always want popper windows
;;; (require 'popper)
;;;
;;; ;;; If you want to redefine popper keys
;;; (setq popper-load-hook
;;;       '(lambda ()
;;;         (define-key global-map "\C-c1" 'popper-bury-output)
;;;         (define-key global-map "\C-cv" 'popper-scroll-output)
;;;         (define-key global-map "\C-cg" 'popper-grow-output)))
;;;
;;; ;;; Autoload based on your LISP.  You only really need the one you use.
;;; ;;; If called with a prefix, you will be prompted for a buffer name.
;;; (autoload 'run-ilisp "ilisp" "Select a new inferior LISP." t)
;;; (autoload 'clisp     "ilisp" "Inferior generic Common LISP." t)
;;; (autoload 'allegro   "ilisp" "Inferior Allegro Common LISP." t)
;;; (autoload 'lucid     "ilisp" "Inferior Lucid Common LISP." t)
;;; (autoload 'cmulisp   "ilisp" "Inferior CMU Common LISP." t)
;;; (autoload 'kcl       "ilisp" "Inferior Kyoto Common LISP." t)
;;; (autoload 'scheme    "ilisp" "Inferior generic Scheme." t)
;;; (autoload 'oaklisp   "ilisp" "Inferior Oaklisp Scheme." t)
;;;
;;; ;;; This makes reading a lisp file load in ilisp.
;;; (set-default 'auto-mode-alist
;;;              (append '(("\\.lisp$" . lisp-mode)) auto-mode-alist))
;;; (setq lisp-mode-hook '(lambda () (require 'ilisp)))
;;;
;;; The default key prefix is C-z since C-c is reserved for the user.
;;; To change it back to C-c, put this in your .emacs:
;;; (setq ilisp-load-hook
;;;   '(lambda () 
;;;      (setq ilisp-prefix "\C-c")))
;;;
;;; ;;; Set the inferior LISP directory to the directory of the
;;; ;;; buffer that spawned it.
;;; (setq ilisp-load-hook 
;;;       '(lambda ()
;;;         (add-hook 'clisp-hook 
;;;          (function (lambda ()
;;;            (add-hook 'ilisp-init-hook
;;;                      (function (lambda ()
;;;                        (default-directory-lisp ilisp-last-buffer)))))))))

;;;%%CUSTOMIZING DIALECTS
;;;
;;; ILISP is already set up with support for a number of dialects.
;;; Each dialect has a command NAME that will start an inferior LISP
;;; of that dialect and a hook NAME-hook that can be used for
;;; customizing that dialect.  A prefix when starting a dialect will
;;; cause you to be prompted for the buffer name and the program.
;;; When setting something in a hook, you should use the most general
;;; dialect that makes sense. The hooks will be executed before the
;;; inferior LISP is started.
;;;
;;; These are the currently supported dialects.  The dialects
;;; are listed so that the indentation correponds to the hierarchical
;;; relationship between dialects.
;;; clisp
;;;   allegro
;;;   lucid
;;;   cmulisp
;;;   kcl
;;; scheme
;;;   oaklisp
;;;
;;; If anyone figures out support for other dialects I would be happy
;;; to include it in future releases.
;;;
;;; ;;; Example of local changes and extensions to ilisp mode
;;; (setq ilisp-load-hook
;;;       '(lambda ()
;;;         ;; Define C-M-l to switch buffers
;;;         (define-key global-map "\C-\M-l" 'lisp-previous-buffer)
;;;         ;; Change the allegro lisp program
;;;         (add-hook 'allegro-hook
;;;          '(lambda ()
;;;            (setq ilisp-program "/usr/misc/bin/lisp")))
;;;         ;; Define a new subdialect to run on another machine.
;;;         (defdialect cmlisp "Connection Machine LISP."
;;;           lucid
;;;           (setq ilisp-program
;;;            "rsh power /usr/local/cm/bin/starlisp"))))
;;;
;;; ;;; Automatically load a new subdialect
;;; (autoload 'cmlisp "ilisp" "Run an inferior CM lisp." t)
;;;
;;; To define a new dialect use the macro defdialect.  For examples,
;;; look at the dialect definitions in this file.  There are hooks and
;;; variables for almost anything that you are likely to need to
;;; change.  The relationship between dialects is hierarchical with
;;; the root values being defined in setup-ilisp.  For a new dialect,
;;; you only need to change the variables that are different than in
;;; the parent dialect.
;;;
;;; ILISP Mode Hooks:
;;; ilisp-site-hook         Executed when file is loaded
;;; ilisp-load-hook  	    Executed when file is loaded
;;; ilisp-mode-hook         Executed when an ilisp buffer is created
;;; ilisp-init-hook         Executed on first prompt
;;; DIALECT-hook            Executed when dialect is set
;;;
;;; Variables you might want to set in a hook or dialect:
;;; ilisp-prefix          Keys to prefix ilisp key bindings
;;; ilisp-program         Program to start for inferior LISP
;;; ilisp-motd            String printed on startup with version
;;; lisp-wait-p           Set to T for synchronous sends
;;; lisp-show-status      Set to nil to stop showing process status
;;; ilisp-prefix-match    Set to T if you do not want partial completion
;;; ilisp-filter-regexp	  Input history filter 
;;; ilisp-filter-length   Input history minium length
;;; ilisp-other-prompt    Prompt for non top level read-eval print loops

;;; %%WRITING NEW COMMANDS
;;;
;;; Basic tools for creating new commands:
;;; deflocal -- Define a new buffer local variable.
;;; ilisp-dialect -- List of dialect types.  For specific dialect clauses.
;;; lisp-symbol -- Create a symbol.
;;; lisp-symbol-name -- Return a symbol's name
;;; lisp-symbol-delimiter -- Return a symbol's qualification
;;; lisp-symbol-package -- Return a symbol's package
;;; lisp-string-to-symbol -- Convert string to symbol
;;; lisp-symbol-to-string -- Convert symbol to string
;;; lisp-buffer-symbol -- Convert symbol to string qualified for buffer
;;; lisp-previous-symbol -- Return previous symbol 
;;; lisp-previous-sexp -- Return previous sexp
;;; lisp-def-name -- Return name of current definition
;;; lisp-function-name -- Return previous function symbol
;;; ilisp-read -- Read an sexp with completion, arglist, etc
;;; ilisp-read-symbol -- Read a symbol or list with completion
;;; ilisp-completing-read -- Read from choices or list with completion
;;;
;;; Special commands like arglist should use ilisp-send to send a
;;; message to the inferior LISP.
;;;
;;; Eval/compile commands should use eval-region-lisp or compile-region-lisp.
;;;

;;; See the documentation for ILISP mode, or read the rest of this
;;; file for more information.  Commands that work only in lisp
;;; buffers or work in both lisp buffers and inferior lisp buffers
;;; have a package specifiction of lisp.  Otherwise they have a
;;; package specifiction of ilisp.  If a function is intended to be
;;; used interactively, then the package specification comes at the
;;; end, otherwise at the start.

;;;%%KNOWN BUGS
;;; 
;;; When running a lisp on Ultrix, you need to set ilisp-program to
;;; "/bin/sh -c your/path/your-lisp-image".
;;; 
;;; If you get lisp output breaking up in weird places it almost
;;; certainly means that comint-prompt-regexp is not precise enough.

;;;%%SUGGESTIONS
;;;
;;; These are suggestions for future releases. Anybody ambitious
;;; enough to implement these suggestions, please send them back to me!
;;;
;;; Should hack reverting a file and some other operations to
;;; recompute the package.
;;;
;;; Write an edit compiler warnings facility.
;;;

;;;%Requirements
(require 'symlink)
(require 'comint)
(require 'comint-ipc)
(require 'ilisp-ext)

;;;%Variables
;;;%%Deflocal
(defvar ilisp-locals '(comint-prompt-regexp 
		       input-ring-size
		       comint-get-old-input
		       comint-input-sentinel
		       comint-input-filter
		       comint-input-sender
		       comint-eol-on-send
		       comint-send-newline
		       comint-fix-error
		       comint-continue
		       comint-interrupt-regexp
		       comint-output-filter
		       comint-interrupt-start
		       comint-handler
		       comint-update-status
		       comint-prompt-status)
  "List of ilisp local variables.")
(defun lisp-deflocal (local)
  (if (not (memq local ilisp-locals))
      (setq ilisp-locals (cons local ilisp-locals))))

;;;
(defmacro deflocal (variable default &optional documentation)
  "Define an ilisp local variable."
  (` (progn (lisp-deflocal '(, variable))
	    (defvar (, variable) (, default) (, documentation)))))

;;;%%Simple customization
(defvar ilisp-prefix "\C-z" "Prefix sequence for ilisp commands.")

(deflocal ilisp-program nil
  "*Program and arguments for invoking an inferior LISP.  The program
can be an rsh to run on a remote machine.  If there is not a common
file system, the interface files will be sent down the pipe instead.")

(defvar ilisp-motd 
  "ILISP V%s  Use M-x ilisp-bug for problems and suggestions."
  "Message of the day format string for ILISP given VERSION.")

(defvar lisp-wait-p nil
  "*T if LISP eval/compile commands should wait for the result.  A
minus prefix to the command will change the sense of this switch for
just the next command.")

(defvar lisp-show-status t 
  "*Set to nil to stop showing process status in lisp-mode buffers.")

(defvar ilisp-prefix-match nil
  "*Set to T to match only as a prefix when completing through the
inferior LISP.  This will speed up completion, but you no longer get
partial completion.") 

(deflocal ilisp-filter-regexp nil
  "*What not to save on an inferior LISP's input history.
Input matching this regexp is not saved on the input history in ilisp
mode.")

(deflocal ilisp-filter-length 3
  "*Do not save strings less than this in the command history.")

(deflocal ilisp-other-prompt nil
  "*Regexp to recognise prompts in the inferior LISP that are prompts
of non-(read/eval/print) top-levels so that bol-ilisp skips them.")

;;;%%%Hooks
(defvar ilisp-site-hook nil
  "Hook for site customization of ilisp mode when it is loaded.")

(defvar ilisp-load-hook '()
  "Hook for customizing ilisp mode when it is loaded.")

(defvar ilisp-mode-hook '()
  "Hook for customising ilisp mode.")

(deflocal ilisp-init-hook nil
  "Hook of functions to call on first prompt in inferior LISP.")

;;;%%Advanced customization
;;;%%%Commands
(deflocal ilisp-load-or-send-command nil
  "Format string for loading BINARY if possible otherwise loading
FILE.  If you can't load either, return NIL.")

(deflocal ilisp-package-regexp nil
  "Regular expression for finding a package specification in a buffer.
The next sexp after the end of the regular expression will be passed
to ilisp-package-command to find the package.")

(deflocal ilisp-package-command nil
  "format string to find the package given PACKAGE.")

(deflocal ilisp-last-command nil
  "Format string for getting the last returned value.")

(deflocal ilisp-save-command nil
  "Format string for saving result history given FORM.")

(deflocal ilisp-restore-command nil
  "Format string for restoring result history.")

(deflocal ilisp-block-command nil
  "Format string for grouping FORMS into one.")

(deflocal ilisp-eval-command nil
  "Format string for evaluating FORM in PACKAGE from FILE.")

(deflocal ilisp-defvar-regexp nil
  "Regular expression for identifying a defvar form.")

(deflocal ilisp-defvar-command nil
  "Format string for re-evaluating DEFVAR in PACKAGE from FILE.")

(deflocal ilisp-describe-command nil
  "Format string for describing FORM in PACKAGE.")

(deflocal ilisp-arglist-command nil
  "Format string for arglist of SYMBOL in PACKAGE.")

(deflocal ilisp-documentation-types nil
  "((\"type\") ...) possible LISP documentation types.")

(deflocal ilisp-documentation-command nil
  "Format string for documentation given SYMBOL in PACKAGE and TYPE.")

(deflocal ilisp-macroexpand-1-command nil
  "Format string for top-level macroexpand given FORM and PACKAGE.")

(deflocal ilisp-macroexpand-command  nil
  "Format string for macroexpand given FORM and PACKAGE.")

(deflocal ilisp-complete-command nil
  "Format string for finding possibly matching symbols given SYMBOL,
PACKAGE, FUNCTIONP, EXTERNALP and PARTIAL-MATCHP.  It should print
((string) (string) ...).")

(deflocal ilisp-callers-command nil
  "Format for finding the callers of SYMBOL in PACKAGE.  The function
should print out callers with one per line.")

(deflocal ilisp-trace-command nil
  "Format for tracing SYMBOL in PACKAGE.")
(deflocal ilisp-untrace-command nil
  "Format for untracing SYMBOL in PACKAGE.")

(deflocal ilisp-directory-command nil
  "Format for setting default DIRECTORY.")

(deflocal ilisp-binary-command nil
  "Command to return the extension for binary files.")

(deflocal ilisp-binary-extension nil
  "*The extension to use for LISP binaries.  If there is an
ilisp-binary-command, this string will be determined at initilization time.")

(deflocal ilisp-init-binary-command nil
  "Command to return the extension for initialization binary files.")

(deflocal ilisp-init-binary-extension nil
  "The extension for initialization binary files.  If there is an
ilisp-init-binary-command, this string will be determined at
initilization time.")

(deflocal ilisp-load-command nil
  "Format string for loading a file in LISP given FILE.")

(deflocal ilisp-compile-file-command nil
  "Format string for compiling a file in LISP given FILE and EXTENSION.")

;;;%%%%Source
(deflocal ilisp-source-types nil
  "Alist of strings for source types.  The strings can be either
symbols or list expressions since the input accepts symbols or open
ended lists as type specifiers.")

(deflocal ilisp-find-source-command nil
  "Format string for finding the source file that defined SYMBOL in
PACKAGE.  It should return NIL if no source is found.")

(deflocal ilisp-locator nil
  "Function \(SYMBOL TYPE FIRST-P BACK-P) that finds the next SYMBOL TYPE
definition in the current buffer.  FIRST-P is T the first time it is
called in a buffer.  BACK-P is T to move backwards.")

(deflocal ilisp-calls-locator nil
  "Function \(SYMBOL TYPE FIRST-P BACK-P ) that finds calls to SYMBOL
in the current buffer.  FIRST-P is T the first time it is called in a
buffer.  BACK-P is T to move backwards.")

;;;%%%Misc
(defvar ilisp-bugs-to "ccm@cs.cmu.edu" "Who to send bug reports to.")

(defvar ilisp-modes '(ilisp-mode) "List of all inferior ilisp modes.")
(defvar lisp-source-modes '(lisp-mode scheme-mode)
  "*Used to determine if a buffer contains Lisp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a lisp source file by find-file-lisp, load-file-lisp and
compile-file-lisp. Used by these commands to determine defaults.")

(deflocal ilisp-no-newline nil
  "Set to T to stop ilisp from inserting a newline after a command.")

(deflocal ilisp-error-filter nil "Function to filter error output.")
(deflocal ilisp-error-regexp nil "Regular expression to match error.")

;;;%%Program
(defvar ilisp-version "4.00" "Interface version.")
(defvar ilisp-directory nil "The directory that ilisp is found in.")
(defvar ilisp-mode-map nil "Key map for ILISP.")
(defvar ilisp-buffer nil "Name of selected ilisp buffer.")
(defvar ilisp-status nil "Status string for selected ilisp buffer.")
(defvar ilisp-buffers nil "List of ILISP buffers.")
(defvar ilisp-dialects nil "List of ILISP dialects.")

(deflocal ilisp-load-inits nil
  "Alist of dialect files to load when initializing an inferior LISP.
By default the file will be loaded from the ilisp-directory.")

;;; This is useful to have a clause in ilisp code like:
;;; (if (memq 'allegro (ilisp-value 'ilisp-dialect)) 
;;;     allegro-code
;;;     normal-code)
(deflocal ilisp-dialect nil
  "List of the dialects that defined the current inferior LISP.")

(defvar ilisp-initialized nil
  "List of buffer names that have been initialized.")
(deflocal ilisp-initializing nil
  "Set to T while waiting for inferior LISP to get initialized.")

(deflocal ilisp-load-files nil "List of files being loaded.")

(defvar lisp-changes nil
  "List of markers for changed forms.")
(deflocal ilisp-pending-changes nil
  "List of changes that are pending, but have not been confirmed yet.")

;;;%%%Completion
;;; Dynamically bound variables for controlling reading
(defvar ilisp-complete nil "T if in minibuffer completion mode.")
(defvar ilisp-no-complete nil "T if incomplete symbols are allowed.")
(defvar ilisp-table nil "Completion table for ilisp readers.")
(defvar ilisp-paren nil "T if paren is allowed in ilisp readers.")
(defvar ilisp-completion-package nil 
  "Package of buffer requesting completion.")
(defvar ilisp-completion-function-p nil
  "T if only symbols with function values are allowed.")

;;; State variables for ilisp reading
(defvar ilisp-mini-prefix nil "Package and qualification from minibuffer.")
(defvar ilisp-original nil "Original string for ilisp completion.")
(defvar ilisp-original-function-p nil "Function-p for ilisp completion.")
(defvar ilisp-original-table nil "Completion table for ilisp-original.")
(defvar ilisp-current-symbol nil "Current symbol for ilisp completion.")

;;;%Utilities
;;;%%Misc
(defun lisp-memk (item list key)
  "Test to see if ITEM is in LIST using KEY on each item in LIST
before comparing it to ITEM."
  (lisp-mem item list (function (lambda (x y)
			(equal x (funcall key y))))))

;;; This should be in emacs, but it isn't.
(defun lisp-del (item list &optional test)
  "Delete ITEM from LIST using TEST comparison and return the result.
Default test is equal."
  (let ((test (or test (function equal)))
	(element list)
	(prev nil)
	(done nil))
    (while (and element (not done))
      (if (funcall test item (car element))
	  (progn
	    (setq done t)
	    (if prev
		(rplacd prev (cdr element))
		(setq list (cdr list))))
	  (setq prev element
		element (cdr element))))
    list))

;;;
(defun lisp-last (list)
  "Return the last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

;;;%%Symbol
(defun lisp-symbol (package delimiter name)
  "Create a LISP symbol."
  (list package (if package (or delimiter "::")) name))
(defun lisp-symbol-name (symbol)
  "Return the name of SYMBOL."
  (car (cdr (cdr symbol))))
(defun lisp-symbol-package (symbol)
  "Return the package of SYMBOL."
  (car symbol))
(defun lisp-symbol-delimiter (symbol)
  "Return the qualifier of SYMBOL."
  (car (cdr symbol)))

;;;
(defun lisp-symbol= (symbol1 symbol2)
  "Return T is SYMBOL1 is equal to SYMBOL2."
  (and (string= (lisp-symbol-name symbol1) (lisp-symbol-name symbol2))
       (string= (lisp-symbol-package symbol1) (lisp-symbol-package symbol2))
       (string= (lisp-symbol-delimiter symbol1)
		(lisp-symbol-delimiter symbol2))))

;;;%%String
(defun lisp-prefix-p (s1 s2)
  "Returns t if S1 is a prefix of S2 considering all non alphanumerics
as word delimiters."
  (let ((len1 (length s1)))
    (and (<= len1 (length s2))
	 (let ((start 0)
	       end next-start 
	       (start2 0)
	       end2)
	   (while 
	       (if (setq end (string-match "[^a-zA-Z0-9]" s1 start))
		   (if (string= (substring s1 start end)
				(substring s2 start2 (+ start2 (- end start))))
		       (progn (setq start (match-end 0))
			      (if (string-match 
				   (regexp-quote (substring s1 end start))
				   s2 start2)
				  (setq start2 (match-end 0)))))))
	   (string= (substring s1 start len1)
		    (substring s2 start2 (+ start2 (- len1 start))))))))

;;;
(defun lisp-last-line (string)
  "Return the last line of STRING."
  (let* ((position 0))
    (while (string-match "\\(\n+\\)[^\n]" string position)
      (setq position (match-end 1)))
    (substring string position)))

;;;
(defun lisp-show-send (string)
  "Show STRING in the *ilisp-send* buffer."
  (save-excursion
    (if (ilisp-buffer)
	(set-buffer "*ilisp-send*")
	(error "You must start an inferior LISP with run-ilisp."))
    (erase-buffer)
    (insert string)
    string))

;;;
(defun lisp-slashify (string)
  "Put string in the *ilisp-send* buffer, put backslashes before
quotes and backslashes and return the resulting string."
  (save-excursion
    (lisp-show-send string)
    (set-buffer "*ilisp-send*")
    (goto-char (point-min))
    (while (search-forward "\\" nil t)
      (delete-char -1)
      (insert "\\\\"))
    (goto-char (point-min))
    (while (search-forward "\"" nil t)
      (backward-char)
      (insert ?\\)
      (forward-char))
    (buffer-substring (point-min) (point-max))))

;;;%%File
(defun ilisp-directory (file &optional dirs)
  "Return the directory of DIRS that FILE is found in.  By default
load-path is used for the directories."
  (let* ((dirs (or dirs (cons "" load-path)))
	 (dir (car dirs)))
    (while (and dir (not (file-exists-p (expand-file-name file dir))))
      (setq dirs (cdr dirs)
	    dir (car dirs)))
    dir))

;;;
(defun lisp-file-extension (file extension)
  "Return FILE with new EXTENSION."
  (concat (substring file 0 (string-match ".[^.]*$" file))
	  "." extension))

;;;%Buffer and process selection
(defun ilisp-buffer ()
  "Return the current ILISP buffer."
  (if (memq major-mode ilisp-modes)
      (current-buffer)
      (if ilisp-buffer
	  (get-buffer ilisp-buffer)
	  (error "You must start an inferior LISP with run-ilisp."))))

;;;
(defun ilisp-process ()
  "Return the current ILISP process."
  (get-buffer-process (ilisp-buffer)))

;;;
(defun ilisp-value (variable &optional no-error-p)
  "Return the value of VARIABLE in the ILISP buffer.
If NO-ERROR-P is NIL, then an error will be signalled if VARIABLE is nil."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (let ((value (eval variable)))
      (if value
	  value
	  (if no-error-p
	      nil
	      (error "%s is not defined." variable))))))

;;;
(defun set-ilisp-value (variable value)
  "Set the value of VARIABLE in the ILISP buffer."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (set variable value)))

;;;
(defun select-ilisp ()
  "Select the current ILISP buffer."
  (interactive)
  (let ((new (completing-read
	      (if ilisp-buffer
		  (format "Buffer [%s]: "
			  (substring ilisp-buffer 1
				     (1- (length ilisp-buffer))))
		  "Buffer: ")
	      ilisp-buffers nil t)))
    (if (not (zerop (length new)))
	(setq ilisp-buffer (format "*%s*" new)))))

;;;
(defvar ilisp-last-buffer nil
  "The last used LISP buffer.")
(defun switch-to-lisp (eob-p &optional ilisp-only)
  "If in an ILISP buffer, switch to the buffer that last switched to
an ILISP otherwise, switch to the current ILISP buffer.  With
argument, positions cursor at end of buffer.  If you don't want to
split windows, set pop-up-windows to NIL."
  (interactive "P")
  (if (and (not ilisp-only) ilisp-last-buffer (memq major-mode ilisp-modes))
      (pop-to-buffer ilisp-last-buffer)
      (if (not (memq major-mode ilisp-modes))
	  (setq ilisp-last-buffer (current-buffer)))
      (pop-to-buffer (ilisp-buffer))
      (cond (eob-p (goto-char (point-max))))))

;;;
(defun abort-commands-lisp (&optional message)
  "Abort the sends of the current ilisp."
  (interactive)
  (beep)
  (message "Aborting sends")
  (comint-abort-sends (ilisp-process))
  (message (or message "Aborted sends")))

;;;
(defun interrupt-subjob-ilisp ()
  "Interrupt the current top level command in the inferior LISP."
  (interactive)
  (message "Interrupt top level")
  (while (not (eq comint-send-queue comint-end-queue))
    (accept-process-output))
  (comint-interrupt-subjob))

;;;
(defun status-lisp (showp)
  "Show the message of the current command being executed in the
inferior LISP.  With a prefix show pending sends as well."  
  (interactive "P")
  (save-excursion
    (set-buffer (ilisp-buffer))
    (comint-current-send showp)))

;;;%Buffer
;;;%%Packages
(defvar buffer-package 'not-yet-computed)
(defvar lisp-buffer-package nil "T if in lisp-buffer-package.")

;;;
(defun lisp-buffer-package ()
  "Return the package for this buffer.  The package name is a string.
If there is none, return NIL.  This caches the package, so calling
this more than once is cheap."
  (cond ((not (eq buffer-package 'not-yet-computed)) buffer-package)
	(ilisp-completion-package ilisp-completion-package)
	((or lisp-buffer-package 
	     (memq major-mode ilisp-modes)
	     (not (memq major-mode lisp-source-modes)))
	 nil)
	(t
	 (make-local-variable 'buffer-package)
	 (setq mode-line-process 'ilisp-status)
	 ;; go search for the first in-package in the current buffer
	 (let* ((lisp-buffer-package t)
		(case-fold-search t)
		(regexp (ilisp-value 'ilisp-package-regexp t))
		(spec
		 (if regexp
		     (save-excursion
		       (goto-char (point-min))
		       (if (re-search-forward regexp nil t)
			   (buffer-substring (match-end 0)
					     (progn (forward-sexp)
						    (point)))))))
		(package
		 (if spec
		     (ilisp-send 
		      (format (ilisp-value 'ilisp-package-command) spec)
		      "Finding buffer package"
		      'pkg))))
	   (if (ilisp-value 'comint-errorp t)
	       (progn
		 (comint-display-output package)
		 (error "No package"))
	       (if (and package 
			(string-match "[ \n\t:\"]*\\([^ \n\t\"]\\)*" package))
		   (setq package
			 (substring package
				    (match-beginning 1) (match-end 1)))))
	   (message "")
	   (setq buffer-package package)
	   ;; Display package in mode line
	   (if package (setq mode-name (concat "Lisp:"  buffer-package)))
	   buffer-package))))

;;;
(defun reparse-attribute-list ()
  "Reset the current package of the current buffer."
  (interactive)
  (setq buffer-package 'not-yet-computed
	mode-name (if (memq major-mode ilisp-modes)
		      "ILISP"
		      "Lisp"))
  (lisp-buffer-package))

;;;%Process interface
;;;%%Comint 
(defun ilisp-get-old-input ()
  "Snarf the sexp starting at the nearest previous prompt, or NIL if none."
  (save-excursion
    (let ((begin (lisp-defun-begin))
	  (end (lisp-defun-end t)))
      (if end (buffer-substring begin end)))))

;;;
(defun ilisp-input-filter (str)
  "Don't save anything matching ilisp-filter-regexp or less than
ilisp-filter-length long."
  (and (not (string-match ilisp-filter-regexp str))
       (> (length str) ilisp-filter-length)))

;;;
(defun ilisp-error-filter (output)
  "Keep from OUTPUT only what matches ilisp-error-regexp or everything
if there is no match."
  (if (string-match (ilisp-value 'ilisp-error-regexp) output)
      (substring output (match-beginning 0) (match-end 0))
      output))

;;;
(defun ilisp-handler (error-p wait-p message output prompt)
  "Given ERROR-P, WAIT-P, MESSAGE, OUTPUT and PROMPT, show the message
and output if there is an error or the output is multiple lines and
let the user decide what to do."
  (if (and (not wait-p)
	   (setq output (comint-remove-whitespace output))
	   (or error-p (string-match "\n" output)))
      (let* ((buffer (popper-output-buffer))
	     (out (if error-p 
		      (funcall ilisp-error-filter output)
		      output))
	     (key
	      (if (and error-p (not (comint-interrupted)))
		  (comint-handle-error
		   out
		   "SPC-scroll, I-ignore, K-keep, A-abort sends and keep or B-break: "
		   '(?i ?k ?a ?b))
		  (comint-handle-error 
		   out 
		   "SPC-scroll, I-ignore, K-keep or A-abort sends and keep: "
		   '(?i ?k ?a))))
	     (clear comint-queue-emptied))
	(if (= key ?i)
	    (progn
	      (message "Ignore message")
	      (if buffer 
		  (funcall temp-buffer-show-hook buffer)
		  (popper-bury-output))
	      t)
	    (save-excursion
	      (set-buffer (get-buffer-create "*Errors*"))
	      (if clear (delete-region (point-min) (point-max)))
	      (goto-char (point-max))
	      (insert message)
	      (insert ?\n)
	      (insert out) 
	      (insert "\n\n"))
	    (if clear (setq comint-queue-emptied nil))
	    (if (= key ?a)
		(progn 
		  (message "Abort pending sends and keep in *Errors*")
		  (comint-abort-sends)
		  t)
		(if (= key ?b)
		    (progn 
		      (comint-insert
		       (concat comment-start comment-start comment-start
			       message "\n"
			       output "\n" prompt))
		      (message "Preserve break") nil)
		    (message "Keep error in *Errors* and continue")
		    t))))
      t))

;;;
(defun ilisp-update-status (status)
  "Update process STATUS of the current buffer and let all lisp mode
buffers know as well."
  (setq ilisp-status (if lisp-show-status (format " :%s" status)))
  (comint-update-status status))

;;;
(defun ilisp-abort-handler ()
  "Handle when the user aborts commands."
  (setq ilisp-initializing nil
	ilisp-load-files nil)
  (let ((add nil))
    (while ilisp-pending-changes
      (if (not (memq (car ilisp-pending-changes) lisp-changes))
	  (setq add (cons (car ilisp-pending-changes) add)))
      (setq ilisp-pending-changes (cdr ilisp-pending-changes)))
    (setq lisp-changes (nconc lisp-changes add))))

;;;%%Ilisp stuff
(defun ilisp-initialized ()
  "Return T if the current inferior LISP has been initialized."
  (memq (buffer-name (ilisp-buffer)) ilisp-initialized))

;;;
(defun ilisp-compile-inits ()
  "Compile the initialization files for the current inferior LISP
dialect."
  (interactive)
  (ilisp-init t)
  (let ((files (ilisp-value 'ilisp-load-inits t)))
    (while files
      (compile-file-lisp (expand-file-name (cdr (car files)) ilisp-directory)
			 (ilisp-value 'ilisp-init-binary-extension t))
      (setq files (cdr files)))))

;;;
(defun ilisp-load-or-send (file)
  "Try to load FILE into the inferior LISP.  If the file is not
accessible in the inferior LISP as determined by
ilisp-load-or-send-command, then visit the file and send the file over
the process interface."
  (let* ((command
	  (format (ilisp-value 'ilisp-load-or-send-command) 
		  (lisp-file-extension
		   file 
		   (ilisp-value 'ilisp-init-binary-extension t))
		  file)))
    (set-ilisp-value 'ilisp-load-files 
		     (nconc (ilisp-value 'ilisp-load-files t) (list file)))
    (comint-send
     (ilisp-process) command t nil 'load
     (format "Loading %s" file)
     (function (lambda (error wait message output last)
       (let* ((file (lisp-last ilisp-load-files))
	      (process (get-buffer-process (current-buffer)))
	      (case-fold-search t))
	 (if error
	     (progn (comint-display-error output)
		    (abort-commands-lisp (format "Error loading %s" file)))
	     (if (and output (string-match "nil" (lisp-last-line output)))
		 (let* ((old-buffer (get-file-buffer file))
			(buffer (find-file-noselect file))
			(string (save-excursion
				  (set-buffer buffer)
				  (buffer-string))))
		   (if (not old-buffer) (kill-buffer buffer))
		   (if (string= "" string)
		       (abort-commands-lisp (format "Can't find file %s" file))
		       (comint-send
			process
			(format ilisp-block-command string)
			t nil 'send (format "Sending %s" file)
			(function (lambda (error wait message output last)
			  (if error
			      (progn 
				(comint-display-error output)
				(abort-commands-lisp
				 (format "Error sending %s"
					 (lisp-last
					  ilisp-load-files))))
			      (setq ilisp-load-files
				    (delq (lisp-last ilisp-load-files)
					  ilisp-load-files))))))))
		 (setq ilisp-load-files 
		       (delq file ilisp-load-files))))))))))

;;;
(defun ilisp-load-init (dialect file)
  "Add FILE to the files to be loaded into the inferior LISP when
dialect is initialized.  If FILE is NIL, the entry will be removed."
  (let ((old (assoc dialect ilisp-load-inits)))
    (if file
	(if old
	    (rplacd old file)
	    (setq ilisp-load-inits (nconc ilisp-load-inits 
					  (list (cons dialect file)))))
	(if old (setq ilisp-load-inits (delq old ilisp-load-inits))))))

;;;
(defun ilisp-binary (init var)
  "Initialize VAR to the result of INIT if VAR is NIL."
  (if (not (ilisp-value var t))
      (let ((binary (ilisp-value init t)))
	(if binary
	    (let ((result (comint-send (ilisp-process) binary
				       t t 'binary)))
	      (if (and (stringp (cdr result))
		       (string-match "\"[^\"]*\"" (car result)))
		  (set-ilisp-value var
				   (substring (car result)
					      (1+ (match-beginning 0))
					      (1- (match-end 0))))))))))

;;;
(defun ilisp-done-init ()
  "Make sure that initialization is done and if not dispatch another check."
  (if ilisp-load-files
      (comint-send-code (get-buffer-process (current-buffer))
			'ilisp-done-init)
      (if ilisp-initializing
	  (setq ilisp-initializing nil
		ilisp-initialized
		(cons (buffer-name (current-buffer)) ilisp-initialized)))))

;;;
(defun ilisp-init (&optional waitp)
  "Initialize the current inferior LISP if necessary by loading the
files in ilisp-load-inits.  Optional WAITP waits for initialization to
finish."  
  (interactive)
  (if (not (ilisp-initialized))
      (progn
	(message "Started initializing ILISP")
	(if (not (ilisp-value 'ilisp-initializing t))
	    (unwind-protect
		 (let ((files (ilisp-value 'ilisp-load-inits t)))
		   (comint-sync
		    (ilisp-process)
		    "\"Start sync\""  "\"Start sync\""
		    "\"End sync\""    "\"End sync\"")
		   (ilisp-binary 'ilisp-binary-command 'ilisp-binary-extension)
		   (ilisp-binary 'ilisp-init-binary-command
				 'ilisp-init-binary-extension)
		   (if (not (ilisp-value 'ilisp-init-binary-extension t))
		       (set-ilisp-value 
			'ilisp-init-binary-extension 
			(ilisp-value 'ilisp-binary-extension t)))
		   (while files
		     (ilisp-load-or-send
		      (expand-file-name (cdr (car files)) ilisp-directory))
		     (setq files (cdr files)))
		   (comint-send-code (ilisp-process) 'ilisp-done-init)
		   (set-ilisp-value 'ilisp-initializing t))
	      (if (not (ilisp-value 'ilisp-initializing t))
		  (abort-commands-lisp))))
	(if waitp
	    (while (ilisp-value 'ilisp-initializing t)
	      (accept-process-output))))))

;;;
(defun ilisp-send (string &optional message status and-go)
  "Send STRING to the ILISP buffer, print MESSAGE set STATUS and
return the result if AND-GO is NIL, otherwise switch to ilisp if
and-go is T and show message and results.  If AND-GO is 'dispatch,
then the command will be executed without waiting for results. If this
is the first time an ilisp command has been executed, the lisp will
also be initialized from the files in ilisp-load-inits.  If there is
an error, comint-errorp will be T."
  (ilisp-init t)
  (let ((process (ilisp-process))
	(dispatch (eq and-go 'dispatch)))
    (if message
	(message "%s" (if dispatch
			  (concat "Started " message)
			  message)))
    ;; No completion table
    (setq ilisp-original nil)
    (if (eq and-go 't)
	(progn (comint-send process string nil nil status message)
	       (switch-to-lisp t t))
	(let* ((save (ilisp-value 'ilisp-save-command t))
	       (result
		(comint-send 
		 process
		 (if save (format save string) string)
		 t (if (not dispatch) 'dispatch) status message)))
	  (if save 
	      (comint-send
	       process
	       (ilisp-value 'ilisp-restore-command t)
	       t 'dispatch 'restore nil nil t))
	  (if (not dispatch)
	      (progn
		(while (not (cdr result)) (accept-process-output))
		(comint-remove-whitespace (car result))))))))

;;;
(defun return-ilisp ()
  "Grab the current expression with comint-get-old-input.  If we have
a complete sexp, send it.  Otherwise, indent appropriately."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
	(error "Current buffer has no process")
	(let* ((pmark (process-mark proc))
	       (input (ilisp-get-old-input)))
	  (if input
	      (progn
		(if (>= (point) pmark)
		    (goto-char (point-max))
		    (goto-char pmark)
		    (insert input))
		(if (not ilisp-no-newline) (insert ?\n))
		(if (and (funcall comint-input-filter input)
			 (or (ring-empty-p input-ring)
			     (not (string= (ring-ref input-ring 0) input))))
		    (ring-insert input-ring input))
		(funcall comint-input-sentinel input)
		;; Nuke symbol table
		(setq ilisp-original nil)
		(funcall comint-input-sender proc input)
		(set-marker (process-mark proc) (point))
		(set-marker comint-last-input-end (point))
		(goto-char (point-max)))
	      (insert ?\n)
	      (save-restriction
	       (narrow-to-region pmark (point-max))
	       (funcall indent-line-function)))))))

;;;
(defun close-and-send-lisp ()
  "Close and indent the current sexp then send it to the inferior
lisp." 
  (interactive)
  (reindent-lisp)
  (if (memq major-mode ilisp-modes)
      (return-ilisp)
      (eval-defun-lisp)))

;;;%Completion
;;; The basic idea behind the completion stuff is to use as much of
;;; the standard Emacs stuff as possible.  The extensions here go out
;;; to the inferior LISP to complete symbols if necessary.  
;;; 
(defun ilisp-display-choices (symbol choices)
  "Display the possible choices for SYMBOL in alist CHOICES."
  (with-output-to-temp-buffer " *Completions*"
    (display-completion-list
     (sort 
      (all-completions (lisp-symbol-name symbol) choices)
      'string-lessp))))

;;;%%ilisp-can-complete
(defun ilisp-can-complete (symbol function-p)
  "Return T if ilisp completion can complete SYMBOL from the current table."
  (and ilisp-original 
       (string= (lisp-symbol-package ilisp-original) 
		(lisp-symbol-package symbol))
       (string= (lisp-symbol-delimiter ilisp-original)
		(lisp-symbol-delimiter symbol))
       (lisp-prefix-p (lisp-symbol-name ilisp-original)
		      (lisp-symbol-name symbol))
       (eq function-p ilisp-original-function-p)))

;;;%%ilisp-complete
(defun ilisp-complete (symbol &optional function-p)
  "Return a list of the possible completions for symbol from the
inferior LISP.  If FUNCTION-P is T, only symbols with function
bindings will be considered.  If no package is specified the buffer
package will be used."
  (let* ((choices 
	  (ilisp-send 
	   (format  (ilisp-value 'ilisp-complete-command) 
		    (lisp-symbol-name symbol) (lisp-symbol-package symbol)
		    function-p
		    (string= (lisp-symbol-delimiter symbol) ":")
		    ilisp-prefix-match)
	   (if (not ilisp-complete)
	       (concat "Complete " (lisp-buffer-symbol symbol)))
	   'complete)))
    (if (ilisp-value 'comint-errorp t)
	(progn (comint-display-output choices)
	       (error "Completion error"))
	(setq choices (read choices)
	      choices (if (eq choices 'NIL) nil choices)))
    (setq ilisp-current-symbol symbol
	  ilisp-original symbol
	  ilisp-original-function-p function-p
	  ilisp-original-table choices)))

;;;%%ilisp-completion-table
(defun ilisp-completion-table (symbol function-p)
  "Return the completion table for SYMBOL trying to use the current
one.  If FUNCTION-P is T, only symbols with function cells will be
returned."
  (if (ilisp-can-complete symbol function-p) 
      ilisp-original-table
      (ilisp-complete symbol function-p)))

;;;%%Minibuffer completion
(defun ilisp-restore-prefix ()
  "Restore the prefix from ilisp-mini-prefix at the start of the
minibuffer."
  (if ilisp-mini-prefix
      (save-excursion
	(goto-char (point-min))
	(insert ilisp-mini-prefix)
	(setq ilisp-mini-prefix nil))))

;;;
(defun ilisp-current-choice ()
  "Set up the minibuffer completion table for the current symbol.
If there is a paren at the start of the minibuffer, or there is not an
ilisp-table, this will be from the inferior LISP.  Otherwise, it will
be the ilisp-table."
  (if (or (null ilisp-table) (eq (char-after 1) ?\())
      (progn
	(let* ((symbol-info (lisp-previous-symbol))
	       (symbol (car symbol-info)))
	  (setq minibuffer-completion-table 
		(ilisp-completion-table symbol ilisp-completion-function-p)
		ilisp-current-symbol symbol))
	(save-excursion 
	  (skip-chars-backward "^: \(")
	  (setq ilisp-mini-prefix (buffer-substring (point-min) (point)))
	  (delete-region (point-min) (point)))
	;; Nothing can match this table
	(if (not minibuffer-completion-table)
	    (setq minibuffer-completion-table '((" ")))))
      (setq minibuffer-completion-table ilisp-table
	    minibuffer-completion-predicate nil)))

;;;%%Commands
(defvar ilisp-completion-help
  (lookup-key minibuffer-local-must-match-map "?"))
(defun ilisp-completion-help ()
  "Inferior LISP minibuffer completion help."
  (interactive)
  (ilisp-current-choice) 
  (funcall ilisp-completion-help)
  (ilisp-restore-prefix))

;;;
(defvar ilisp-completion
  (lookup-key minibuffer-local-must-match-map "\t"))
(defun ilisp-completion ()
  "Inferior LISP minibuffer complete."
  (interactive)
  (ilisp-current-choice)
  (funcall ilisp-completion)
  (ilisp-restore-prefix))

;;;
(defvar ilisp-completion-word
  (lookup-key minibuffer-local-must-match-map " "))
(defun ilisp-completion-word ()
  "Inferior LISP minibuffer complete word."
  (interactive)
  (if (eq (char-after 1) ?\()
      (insert " ")
      (ilisp-current-choice)
      (funcall ilisp-completion-word)
      (ilisp-restore-prefix)))

;;;
(defun ilisp-completion-paren ()
  "Only allow a paren if ilisp-paren is T."
  (interactive)
  (if ilisp-paren 
      (if (or (eq last-input-char ?\() (eq (char-after 1) ?\())
	  (insert last-input-char)
	  (beep))
      (beep)))
      
;;; 
(defvar ilisp-completion-exit 
  (lookup-key minibuffer-local-must-match-map "\n"))
(defun ilisp-completion-exit ()
  "Inferior LISP completion complete and exit."
  (interactive)
  (if (eq (char-after 1) ?\()
      (progn (find-unbalanced-lisp nil)
	     (exit-minibuffer))
      (if ilisp-no-complete
	  (exit-minibuffer)
	  (if (= (point-min) (point-max))
	      (exit-minibuffer)
	      (ilisp-current-choice)
	      (unwind-protect (funcall ilisp-completion-exit)
		(ilisp-restore-prefix))))))

;;;%%ilisp-completer
(defun ilisp-completer (symbol function-p)
  "Complete SYMBOL from the inferior LISP using only function symbols
if FUNCTION-P is T.  Return the longest common substring.  If symbol
is already the longest, possible completions will be displayed."
  (let* ((name (lisp-symbol-name symbol))
	 (table (ilisp-completion-table symbol function-p))
	 (choice (if table 
		     (or (try-completion name table)
			 (if (not ilisp-prefix-match)
			     (car 
			      (completer name table nil 
					 completer-delimiters)))))))
    (if choice
	(setq ilisp-current-symbol 
	      (if (or (eq choice t) (string= name choice))
		  (progn
		    (if (not (eq choice t))
			(ilisp-display-choices symbol 
					       ilisp-original-table))
		    symbol)
		  (lisp-symbol (lisp-symbol-package symbol) 
			       (lisp-symbol-delimiter symbol)
			       choice))))))

;;;%Interface functions
;;;%%Symbols
(defun lisp-string-to-symbol (string)
  "Convert STRING to a symbol, (package delimiter symbol) where the
package is either package:symbol or from the current buffer."
  (let* ((start (string-match ":+" string))
	 (end (if start (match-end 0))))
    (if start
	(lisp-symbol
	 (if (= start 0)
	     "KEYWORD"
	     (substring string 0 start))
	 (substring string start end)
	 (substring string end))
	(let ((package (lisp-buffer-package)))
	  (lisp-symbol package (if package "::") string)))))

;;;
(defun lisp-symbol-to-string (symbol)
  "Convert SYMBOL to a string."
  (apply 'concat symbol))

;;;
(defun lisp-buffer-symbol (symbol)
  "Return SYMBOL as a string qualified for the current buffer."
  (let ((symbol-name (lisp-symbol-name symbol))
	(pkg (lisp-symbol-package symbol))
	(delimiter (lisp-symbol-delimiter symbol)))
    (cond ((string= pkg (lisp-buffer-package)) symbol-name)
	  ((string= pkg "KEYWORD") (concat ":" symbol-name))
	  (pkg (concat pkg delimiter symbol-name))
	  (t symbol-name))))

;;;
(defun lisp-previous-symbol (&optional stay)
  "Return the immediately preceding symbol as ((package delimiter symbol)
function-p start end).  If STAY is T, the end of the symbol will be point."
  (save-excursion
    (if (or (and (memq major-mode ilisp-modes)
		 (= (point) (process-mark (get-buffer-process
					   (current-buffer)))))
	    (progn
	      (skip-chars-backward " \t\n")
	      (or (bobp) (memq (char-after (1- (point))) '(?\) ?\")))))
	nil
	(let* ((end (progn
		      (if (not stay) (skip-chars-forward "^ \t\n)\""))
		      (point)))
	       (start (progn
			(skip-chars-backward "^ \t\n('\"")
			(point)))
	       (prefix (if (not (bobp)) (1- start)))
	       (function-p
		(and prefix
		     (or (eq (char-after prefix) ?\()
			 (and (eq (char-after prefix) ?')
			      (not (bobp))
			      (eq (char-after (1- prefix)) ?#)))
		     (not (looking-at "[^: \t\n]*:*\\*[^ \t\n]")))))
	  (cons (lisp-string-to-symbol (buffer-substring start end))
		(list function-p start end))))))

;;;%%Sexps
(defun lisp-previous-sexp (&optional prefix)
  "Return the previous sexp.  If PREFIX is T, then prefix like ' or #'
are allowed."
  (save-excursion
    (condition-case ()
	(progn
	  (if (and (memq major-mode ilisp-modes)
		   (= (point)
		      (process-mark (get-buffer-process (current-buffer)))))
	      nil
	      (if (not
		   (or (eobp) (memq (char-after (point)) '(? ?\) ?\n ?\t))))
		  (forward-sexp))
	      (skip-chars-backward " \t\n")
	      (let ((point (point)))
		(backward-sexp)
		(skip-chars-backward "^ \t\n(\",")
		(if (not prefix) (skip-chars-forward "#'"))
		(buffer-substring (point) point))))
      (error nil))))

;;;
(defun lisp-def-name ()
  "Return the name of a definition assuming that you are at the start
of the sexp.  If the form starts with DEF, the form start and the next
symbol will be returned."
  (let ((case-fold-search t))
    (if (looking-at
	 "\\(\\((\\(def[^ \t\n]*\\)[ \t\n]+\\(\\((\\(setf\\)[ \t\n]+\\)\\|(?\\)\\)\\|(?\\)\\([^ \t\n)]*\\)")
	(let ((symbol (buffer-substring (match-beginning 7) (match-end 7))))
	  (if (match-end 6)
	      (concat (buffer-substring (match-beginning 3) (match-end 3))
		      " ("
		      (buffer-substring (match-beginning 6) (match-end 6))
		      " " symbol ")")
	      (if (match-end 3)
		  (concat (buffer-substring (match-beginning 3) (match-end 3))
			  " " symbol)
		  symbol))))))

;;;
(defun lisp-function-name ()
  "Return the previous function symbol.  This is either after a #' or
at the start of the current sexp.  If there is no current sexp, return
nil."
  (save-excursion
    (let ((symbol (lisp-previous-symbol)))
      (if (car (cdr symbol))
	  (car symbol)
	  (condition-case ()
	      (if (and (memq major-mode ilisp-modes)
		       (= (point)
			  (process-mark 
			   (get-buffer-process (current-buffer)))))
		  nil
		  (backward-up-list 1)
		  (down-list 1)
		  (lisp-string-to-symbol
		   (buffer-substring (point) 
				     (progn (forward-sexp 1) (point)))))
	    (error nil))))))

;;;
(defun lisp-minus-prefix ()
  "Set current-prefix-arg to its absolute value if numeric and return
T if it is a negative."
  (if current-prefix-arg
      (if (symbolp current-prefix-arg)
	  (progn (setq current-prefix-arg nil) t)
	  (if (< (setq current-prefix-arg
		       (prefix-numeric-value current-prefix-arg))
		 0)
	      (progn (setq current-prefix-arg (- current-prefix-arg)) t)))))

;;;%%ilisp-read
(defvar ilisp-completion-map nil "Keymap for reading ilisp readers.")
(defun ilisp-completion-map ()
  "Set up the ilisp-completion-map from lisp-mode-map for the ilisp
readers and return it."
  (if (not ilisp-completion-map)
      (progn
	(setq ilisp-completion-map (copy-keymap lisp-mode-map))
	(define-key ilisp-completion-map " "  'ilisp-completion-word)
	(define-key ilisp-completion-map "\t" 'ilisp-completion)
	(define-key ilisp-completion-map "?" 'ilisp-completion-help)
	(define-key ilisp-completion-map "\M-\t" 'ilisp-completion)
	(define-key ilisp-completion-map "\n" 'ilisp-completion-exit)
	(define-key ilisp-completion-map "\r" 'ilisp-completion-exit)
	(define-key ilisp-completion-map "\C-g" 'abort-recursive-edit)
	(define-key ilisp-completion-map "(" 'ilisp-completion-paren)
	(define-key ilisp-completion-map ")" 'ilisp-completion-paren)
	(define-key ilisp-completion-map "'" nil)
	(define-key ilisp-completion-map "#" nil)
	(define-key ilisp-completion-map "\"" nil)))
  ilisp-completion-map)

;;;
(defun ilisp-read (prompt &optional initial-contents)
  "PROMPT in the minibuffer with optional INITIAL-CONTENTS and return
the result.  Completion of symbols though the inferior LISP is
allowed."
  (let ((ilisp-complete t)
	(ilisp-paren t)
	(ilisp-no-complete t)
	(ilisp-completion-package (lisp-buffer-package)))
    (read-from-minibuffer prompt initial-contents
			  (ilisp-completion-map))))

;;;%%lisp-read-program
(defvar lisp-program-map nil
  "Minibuffer map for reading a program and arguments.")

;;; Replacement for comint version that uses pop-up window
(defun comint-dynamic-list-completions ()
  "List in help buffer all possible completions of the filename at point."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completions
	  (file-name-all-completions pathnondir
				     (or pathdir default-directory))))
    (with-output-to-temp-buffer " *Completions*"
      (display-completion-list completions))))

;;;
(defun lisp-complete-file ()
  "Complete the previous filename or display possibilities if done
twice in a row."
  (interactive)
  (setq this-command 'lisp-complete-file)
  (if (eq last-command 'lisp-complete-file)
      (comint-dynamic-list-completions)
      (let* ((start (save-excursion (skip-chars-backward "^ \t") (point)))
	     (string (buffer-substring start (point))))
	(comint-dynamic-complete)
	(if (string= (buffer-substring start (point)) string)
	    (comint-dynamic-list-completions)))))

;;;
(defun lisp-read-program (prompt &optional initial)
  "Read a program with PROMPT and INITIAL.  TAB or Esc-TAB will complete
filenames."
  (if (null lisp-program-map)
      (progn 
	(setq lisp-program-map (copy-keymap minibuffer-local-map))
	(define-key lisp-program-map "\M-\t" 'lisp-complete-file)
	(define-key lisp-program-map "\t" 'lisp-complete-file)
	(define-key lisp-program-map "?" 'comint-dynamic-list-completions)))
  (read-from-minibuffer prompt initial lisp-program-map))

;;;%%ilisp-read-symbol
(defun ilisp-read-symbol (prompt &optional default function-p no-complete)
  "PROMPT in the minibuffer with optional DEFAULT and return a symbol
from the inferior LISP.  If FUNCTION-P is T, only symbols with
function values will be returned.  If NO-COMPLETE is T, then
uncompleted symbols will be allowed."
  (let* ((ilisp-complete t)
	 (ilisp-no-complete no-complete)
	 (ilisp-completion-package (lisp-buffer-package))
	 (ilisp-completion-function-p function-p)
	 (string (read-from-minibuffer prompt nil (ilisp-completion-map))))
    (if (equal string "")
	default
	(lisp-string-to-symbol string))))

;;;%%ilisp-completing-read
(defun ilisp-completing-read (prompt table &optional default)
  "Read with PROMPT from an alist of TABLE.  No input returns DEFAULT.
Symbols are from table, other specs are in parentheses."
  (let* ((ilisp-complete t)
	 (ilisp-table table)
	 (ilisp-completion-package (lisp-buffer-package))
	 (ilisp-paren
	  (let ((entry table) (done nil))
	    (while (and entry (not done))
	      (setq done (= (elt (car (car entry)) 0) ?\()
		    entry (cdr entry)))
	    done))
	 (string (read-from-minibuffer prompt nil (ilisp-completion-map))))
    (if (string= string "") default string)))

;;;%%Input 
(defun lisp-at-start ()
  "Return the point if you are at the start of an input expression in
an inferior Lisp."
  (save-excursion
    (let ((point (point)))
      (beginning-of-line)
      (comint-skip-prompt)
      (if (= point (point))
	  point))))

;;;
(defun lisp-input-start ()
  "Go to the start of the input region."
  (let* ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (if (>= (point) pmark)
	(goto-char pmark)
	(progn 
	  (end-of-line)
	  (if (re-search-backward comint-prompt-regexp (point-min) 'stay)
	      (comint-skip-prompt)
	      (point))))))

;;;%%Defuns
(defun lisp-defun-region-and-name ()
  "Return the region of the current defun and the name starting it."
  (save-excursion
    (let ((end (lisp-defun-end))
	  (begin (lisp-defun-begin)))
      (list begin end (lisp-def-name)))))
  
;;;
(defun lisp-defun-name ()
  "Return the name of the current defun."
  (save-excursion
    (lisp-defun-begin)
    (lisp-string-to-symbol (lisp-def-name))))

;;;%Lisp mode extensions
;;;%%Movement
(defun bol-ilisp (arg)
  "Goes to the beginning of line, then skips past the prompt, if any.
If a prefix argument is given (\\[universal-argument]), then no prompt skip 
-- go straight to column 0.

The prompt skip is done by skipping text matching the regular expression
comint-prompt-regexp or ilisp-other-prompt, both buffer local variables."
  (interactive "P")
  (beginning-of-line)
  (if (null arg) 
      (or (comint-skip-prompt)
	  (if ilisp-other-prompt
	      (let ((comint-prompt-regexp ilisp-other-prompt))
		(comint-skip-prompt))))))

;;;
(defun beginning-of-defun-lisp (&optional stay)
  "Go to the next left paren that starts at the left margin or after a
prompt in an ILISP buffer.  If optional STAY, then do not move to
prior defun if at the start of one in an ilisp mode."
  (interactive)
  (if (memq major-mode ilisp-modes)
      (let ((point (point)))
	(if (and (not stay) (= point (lisp-input-start)))
	    (progn (forward-line -1) (lisp-input-start))))
      (beginning-of-defun)))

;;;
(defun end-of-defun-lisp ()
  "Go to the next left paren that starts at the left margin or after a
prompt in an ILISP buffer and go to the end of the expression."
  (interactive)
  (let ((point (point)))
    (if (memq major-mode lisp-source-modes)
	(progn (beginning-of-line)
	       (re-search-forward "^[ \t\n]*[^; \t\n]" nil t)
	       (back-to-indentation)
	       (if (not (bolp)) (beginning-of-defun-lisp t)))
	(beginning-of-defun-lisp t))
    (lisp-end-defun-text t)
    (if (= point (point))		;Already at end so move to next end
	(progn
	  (if (memq major-mode ilisp-modes)
	      (re-search-forward comint-prompt-regexp (point-max) t)
	      (lisp-skip (point-max)))
	  (if (not (or (eobp)
		       (= (char-after (point)) ?\n)))
	      (lisp-end-defun-text t))))))

;;;%%Indentation
(defun newline-and-indent-ilisp ()
  "If at the end of the buffer, send the string back to the process
mark with no newline.  Otherwise, insert a newline, then indent.  In
an ilisp buffer the region is narrowed first.  See newline-and-indent
for more information."
  (interactive "*")
  (if ilisp-complete
      (exit-minibuffer)
      (let (input)
	(if (and (= (point) (point-max)) 
		 (memq major-mode ilisp-modes)
		 (setq input (ilisp-get-old-input)))
	    (let ((process (ilisp-process))
		  (comint-send-newline (not comint-send-newline)))
	      (funcall comint-input-sender process input)
	      (set-marker (process-mark process) (point)))
	    (save-restriction
	      (if (memq major-mode ilisp-modes)
		  (narrow-to-region (save-excursion (lisp-input-start))
				    (point-max)))
	      (newline-and-indent))))))

;;;%Special commands
(defun describe-lisp (sexp)
  "Describe the current sexp using ilisp-describe-command.  With a
negative prefix, prompt for the expression.  If in an ILISP buffer,
and there is no current sexp, describe ilisp-last-command."
  (interactive
   (list
    (if (lisp-minus-prefix)
	(ilisp-read "Describe: " (lisp-previous-sexp t))
	(if (memq major-mode ilisp-modes)
	    (if (= (point)
		   (process-mark (get-buffer-process (current-buffer))))
		(or (ilisp-value 'ilisp-last-command t)
		    (error "No sexp to describe."))
		(lisp-previous-sexp t))
	    (lisp-previous-sexp t)))))
  (let ((result
	 (ilisp-send
	  (format (ilisp-value 'ilisp-describe-command) 
		  (lisp-slashify sexp) (lisp-buffer-package))
	  (concat "Describe " sexp)
	  'describe)))
    (comint-display-output result)))

;;;
(defun arglist-lisp (symbol)
  "Return the arglist of the currently looked at function.  With a
numeric prefix, the arglist will be inserted.  With a negative one,
the symbol will be prompted for."
  (interactive
   (let* ((function (lisp-function-name)))
     (list (if (lisp-minus-prefix)
	       (ilisp-read-symbol
		(format "Arglist [%s]: " (lisp-buffer-symbol function))
		function t)
	       function))))
  (if (null symbol)
      (error "No symbol")
      (let* ((arglist
	      (ilisp-send
	       (format (ilisp-value 'ilisp-arglist-command)
		       (lisp-symbol-name symbol) 
		       (lisp-symbol-package symbol))
	       (concat "Arglist " (lisp-buffer-symbol symbol))
	       'args))
	     (position (string-match "(" arglist)))
	(cond ((and (not (ilisp-value 'comint-errorp t))
		    current-prefix-arg position)
	       (let ((temp (point)))
		 (insert (substring arglist (1+ position)))
		 (goto-char temp)))
	      (t (comint-display-output 
		  (if position
		      (substring arglist position)
		      arglist)))))))

;;;
(defun documentation-lisp (symbol type)
  "Return the documentation of the previous symbol using
ilisp-documentation-command.  If the symbol is at the start of
a list, it is assumed to be a function, otherwise variable
documentation is searched for.  With a minus prefix, prompt for
the symbol and type."
  (interactive
   (if (lisp-minus-prefix)
       (let* ((symbol-info (lisp-previous-symbol))
	      (symbol (car symbol-info))
	      (doc (ilisp-read-symbol 
		    (format "Documentation [%s]: " 
			    (lisp-buffer-symbol symbol))
		    symbol))
	      (default (if (car (cdr symbol-info))
			   'function
			   'variable))
	      (types (ilisp-value 'ilisp-documentation-types t))
	      (type
	       (if types
		   (ilisp-completing-read
		    (if default
			(format "Type [%s]: " default)
			"Type: ")
		    types
		    default))))
	 (list doc (if (stringp type) (read type) type)))
       (let* ((symbol-info (lisp-previous-symbol)))
	 (list (car symbol-info)
	       (if (car (cdr symbol-info))
		   'function
		   'variable)))))
  (comint-display-output
       (ilisp-send
	(format (ilisp-value 'ilisp-documentation-command)
		(lisp-symbol-name symbol) (lisp-symbol-package symbol) type)
	(format "Documentation %s %s" type (lisp-buffer-symbol symbol))
	'doc)))

;;;%%Macroexpand
(defun lisp-macroexpand-form ()
  "Return the next form for macroexpanding."
  (save-excursion
    (skip-chars-forward " \t\n")
    (let* ((begin (point))
	   (end (progn (forward-sexp) (point)))
	   (form (buffer-substring begin end)))
      (list
       (if (lisp-minus-prefix)
	   (ilisp-read "Macroexpand: " form)
	   form)))))

;;;
(defun macroexpand-lisp (form &optional top)
  "Macroexpand the next sexp until it is no longer a macro.  With a
prefix, insert into buffer."
  (interactive (lisp-macroexpand-form))
  (if (string-match "(\\([^ \t\n)]*\\)" form)
      (let ((message (concat "Macroexpand"
			     (if top "-1 " " ")
			     (substring form
					(match-beginning 1)
					(match-end 1))))
	    result)
	(setq result
	      (ilisp-send
	       (format
		(ilisp-value
		 (if top
		     'ilisp-macroexpand-1-command
		     'ilisp-macroexpand-command))
		(lisp-slashify form)
		(lisp-buffer-package)
		(buffer-file-name))
	       message 'expand))
	(if current-prefix-arg
	    (save-excursion (forward-sexp) (insert ?\n) (insert result))
	    (comint-display-output result)))
      (error "Not a form: %s" form)))

(defun macroexpand-1-lisp (form)
  "Macroexpand the next sexp once.  With a prefix, insert into buffer."
  (interactive (lisp-macroexpand-form))
  (macroexpand-lisp form t))

;;;%%complete-lisp
(defun complete-lisp (all-p)
  "Complete the current symbol using information from the current
ILISP buffer.  If in a string, complete as a filename.  If called with
a prefix force all symbols to be considered.  Partial completion is
allowed unless ilisp-prefix-match is T.  If a symbol starts after a
left paren or #', then only function symbols will be considered.
Package specifications are also allowed and the distinction between
internal and exported symbols is considered."
  (interactive "P")
  (let* ((filep
	  (save-excursion
	    (skip-chars-backward "^ \t\n")
	    (= (char-after (point)) ?\"))))
    (if filep
	(lisp-complete-file)
	(let* ((symbol-info (lisp-previous-symbol t))
	       (symbol (car symbol-info))
	       (name (lisp-symbol-name symbol))
	       (choice (ilisp-completer 
			symbol 
			(if (not all-p) (car (cdr symbol-info))))))
	  (if (not choice) 
	      (if (not ilisp-complete) (error "No completions"))
	      (let ((point (point)))
		(apply 'delete-region (cdr (cdr symbol-info)))
		(insert (lisp-buffer-symbol choice))
		(if (not ilisp-complete) (message "Completed"))))))))

;;;%%Trace
(defun trace-lisp (function)
  "Trace FUNCTION without arg, untrace with.  Prompt for function with
negative prefix."
  (interactive
   (let ((function (lisp-defun-name)))
     (if (lisp-minus-prefix)
	 (list (ilisp-read-symbol
		(format (if current-prefix-arg 
			    "Untrace [%s]: "
			    "Trace [%s]: ")
			(lisp-buffer-symbol function))
		function
		t))
	 (list function))))
  (if function
      (comint-display-output
       (ilisp-send
	(format (if current-prefix-arg 
		    (ilisp-value 'ilisp-untrace-command)
		    (ilisp-value 'ilisp-trace-command))
		(lisp-symbol-name function)
		(lisp-symbol-package function))
	(format "%srace %s" (if current-prefix-arg "Unt" "T") 
		(lisp-buffer-symbol function))
	(if current-prefix-arg 'untrace 'trace)))
      (error "No function to %strace" (if current-prefix-arg "un" ""))))

;;;%%Default-directory
(defun default-directory-lisp (&optional buffer)
  "Set the inferior LISP default directory to the default directory of
optional BUFFER."
  (interactive)
  (let ((directory (save-excursion
		     (set-buffer (or buffer (current-buffer)))
		     default-directory)))
    (ilisp-send 
     (format (ilisp-value 'ilisp-directory-command)
	     directory)
     (format "Set LISP directory to %s" directory)
     'dir
     (if lisp-wait-p nil 'dispatch))))
  
;;;%Source
(autoload 'lisp-directory "ilisp-src" 
	  "Select directories to search." t)
(autoload 'next-definition-lisp "ilisp-src"
	  "Edit the next definition." t)
(autoload 'edit-definitions-lisp "ilisp-src" 
	  "Edit definitions." t)
(autoload 'search-lisp "ilisp-src" 
	  "Search for pattern in source files." t)
(autoload 'replace-lisp "ilisp-src" 
	  "Relace pattern in source files." t)
(autoload 'who-calls-lisp "ilisp-src"
	  "Show callers of a function." t)
(autoload 'next-caller-lisp "ilisp-src" 
	  "Edit the next caller of a function." t)
(autoload 'next-caller-lisp "ilisp-src" 
	  "Edit the callers of a function." t)

;;;%Eval/compile
(defun lisp-send-region (start end switch message status format)
  "Given START, END, SWITCH, MESSAGE, STATUS and FORMAT send the
region between START and END to the lisp buffer and execute the
command defined by FORMAT on the region, its package and filename.  If
called with a positive prefix, the results will be inserted at the end
of the region.  If SWITCH is T, the command will be sent and the
buffer switched to the inferior LISP buffer.  If SWITCH is 'result the
result will be returned without being displayed.  Otherwise the
results will be displayed in a popup window if lisp-wait-p is T and
the current-prefix-arg is not '- or if lisp-wait-p is nil and the
current-prefix-arg is '-.  If not displayed in a pop-up window then
comint-handler will display the results in a pop-up window if they are
more than one line long, or they are from an error.  STATUS will be
the process status when the command is actually executing.  MESSAGE is
a message to let the user know what is going on."
  (let ((sexp (lisp-count-pairs start end ?\( ?\)))
	(string (buffer-substring start end)))
    (setq string
	  (format (ilisp-value format)
		  (lisp-slashify
		   (if (= sexp 1)
		       string
		       (format (ilisp-value 'ilisp-block-command) string)))
		  (lisp-buffer-package) (buffer-file-name)))
    (let ((result 
	   (ilisp-send
	    string message status
	    (cond ((eq switch t) t)
		  ((or (not (eq lisp-wait-p (lisp-minus-prefix))) 
		       current-prefix-arg
		       (eq switch 'result)) nil)
		  (t 'dispatch)))))
      (if result
	  (if current-prefix-arg
	      (save-excursion
		(goto-char end)
		(insert ?\n)
		(insert result))
	      (if (or (if (ilisp-value 'comint-errorp t)
			  (setq result 
				(funcall (ilisp-value 'ilisp-error-filter)
					 result)))
		      (string-match "\n" result))
		  (comint-display-output result)
		  (popper-bury-output t)
		  (message result)))
	  result))))

;;;%%Eval
(defun eval-region-lisp (start end &optional switch message status)
  "Evaluate the current region."
  (interactive "r")
  (setq message (or message "Evaluate region"))
  (let ((defvar (ilisp-value 'ilisp-defvar-regexp t)))
    (if (and defvar
	     (save-excursion
	       (goto-char start)
	       (skip-chars-forward " \t\n")
	       (and (let ((case-fold-search t)) (looking-at defvar) )
		    (progn (forward-sexp) (skip-chars-forward " \t\n" end)
			   (= (point) end)))))
	(lisp-send-region start end switch message (or status 'defvar)
			  'ilisp-defvar-command)
	(lisp-send-region start end switch message (or status 'eval)
			  'ilisp-eval-command))))

;;;
(defun eval-next-sexp-lisp (&optional switch)
  "Evaluate the next sexp."
  (interactive)
  (let (start end)
    (save-excursion
      (setq start (point))
      (forward-sexp)
      (setq end (point)))
    (eval-region-lisp start end switch
		      (format "Evaluate %s" (buffer-substring start end)))))

;;;
(defun eval-prev-sexp-lisp (&optional switch)
  "Evaluate the previous sexp."
  (interactive)
  (let (start end)
    (save-excursion
      (setq end (point))
      (backward-sexp)
      (setq start (point)))
    (eval-region-lisp start end switch
		      (format "Evaluate %s" (buffer-substring start end)))))

;;;
(defun eval-defun-lisp (&optional switch)
  "Evaluate the current form."
  (interactive)
  (let ((form (lisp-defun-region-and-name)))
    (eval-region-lisp (car form) (car (cdr form)) switch
		      (format "Evaluate %s" (car (cdr (cdr form)))))))
		      
;;;%%%And go
(defun eval-region-and-go-lisp (start end)
  "Evaluate the current region and switch to the current ILISP buffer."
  (interactive "r")
  (eval-region-lisp start end t))

(defun eval-next-sexp-lisp-and-go (&optional switch)
  "Evaluate the next sexp and switch to the current ILISP buffer."
  (interactive)
  (eval-next-sexp-lisp t))

(defun eval-prev-sexp-and-go-lisp (&optional switch)
  "Evaluate the previous sexp and switch to the current ILISP buffer."
  (interactive)
  (eval-prev-sexp-lisp t))

(defun eval-defun-and-go-lisp ()
  "Evaluate the current defun and switch to the current ILISP buffer."
  (interactive)
  (eval-defun-lisp t))

;;;%%Compile
(defun compile-region-lisp (start end &optional switch message status)
  "Compile the current region."
  (interactive "r")
  (lisp-send-region
   start end switch (or message "Compile region") (or status 'compile)
   'ilisp-compile-command))
		
;;;
(defun compile-defun-lisp (&optional switch)
  "Compile the current defun or the result of the last command in an
ILISP buffer."
  (interactive)
  (let ((form (lisp-defun-region-and-name)))
    (compile-region-lisp (car form) (car (cdr form)) switch
			 (format "Compile %s" (car (cdr (cdr form)))))))

;;;%%%And-go
(defun compile-region-and-go-lisp (start end)
  "Compile the current region and switch to the current ILISP buffer."
  (interactive "r")
  (compile-region-lisp start end t))

(defun compile-defun-and-go-lisp ()
  "Compile the current defun and switch to the current ILISP buffer."
  (interactive)
  (compile-defun-lisp t))

;;;%%Changed definitions
(autoload 'mark-change-lisp "ilisp-bat" 
	  "Mark the current defun as changed." t)
(autoload 'list-changes-lisp "ilisp-bat"
	  "List the current LISP changes." t)
(autoload 'clear-changes-lisp "ilisp-bat"
	  "Clear the list of LISP changes." t)
(autoload 'eval-changes-lisp "ilisp-bat"
	  "Evaluate the list of LISP changes." t)
(autoload 'compile-changes-lisp "ilisp-bat"
	  "Compile the list of LISP changes." t)

;;;%File operations
(defvar lisp-prev-l/c-dir/file nil
  "Saves the (directory . file) pair used in the last find-file-lisp,
load-file-lisp or compile-file-lisp command. Used for determining the
default in the next one.")

;;;
(defvar lisp-buffer-file nil 
  "Cons of buffer-file-name and the expanded name.")
(make-variable-buffer-local 'lisp-buffer-file)
(defun lisp-find-file (file &optional pop no-name)
  "Find FILE, optionally POPping.  If optional NO-NAME is nil, and
there is a buffer with a name that is the same as the final pathname
component, select that instead of reading the file associated with the
full path name.  If the expanded name of FILE and buffer match, select
that buffer."  
  (let* ((buffers (buffer-list))
	 (position 0)
	 (expand-symlinks t)
	 (expanded (expand-file-name file))
	 filename)
    (if (not no-name)
	(progn (while (string-match "/" file position)
		 (setq position (match-end 0)))
	       (setq filename (substring file position))))
    (while buffers
      (save-excursion 
	(set-buffer (car buffers))
	(let* ((name (and (not no-name) (buffer-name)))
	       (buffer-file (buffer-file-name))
	       (buffer-expanded
		(cdr 
		 (if (string-equal buffer-file (car lisp-buffer-file)) 
		     lisp-buffer-file
		     (setq lisp-buffer-file
			   (cons buffer-file 
				 (expand-file-name buffer-file)))))))
	  (if (or (and name (string-equal filename name))
		  (string-equal expanded buffer-expanded))
	      (setq file buffer-file
		    buffers nil)
	      (setq buffers (cdr buffers)))))))
  (if pop
      (pop-to-buffer (find-file-noselect file))
      (find-file file)))

;;;
(defun find-file-lisp (file-name)
  "Find a file.  If point is on a string, that will be the default.
If the buffer is one of lisp-source-modes, the buffer file will be the
default.  Otherwise, the last file used in a lisp-source-mode will be
used."
  (interactive
   (comint-get-source "Find Lisp file: " lisp-prev-l/c-dir/file
		      lisp-source-modes nil))
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (lisp-find-file file-name nil t))

;;;
(defun load-file-lisp (file-name)
  "Load a lisp file into the current inferior LISP and go there."
  (interactive (comint-get-source "Load Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (ilisp-init t)
  (let* ((extension (ilisp-value 'ilisp-binary-extension t))
	 (binary (lisp-file-extension file-name extension)))
    (if (file-newer-than-file-p file-name binary)
	(if (and extension (y-or-n-p "Compile first? "))
	    ;; Load binary if just compiled
	    (progn
	      (message "")
	      (compile-file-lisp file-name)
	      (setq file-name binary)))
	;; Load binary if it is current
	(if (file-readable-p binary)
	    (setq file-name binary)))
    (ilisp-send
     (format (ilisp-value 'ilisp-load-command) file-name)
     (concat "Loading " file-name) 'load
     t)))

;;;
(defun compile-file-lisp (file-name &optional extension)
  "Compile a Lisp file in the current inferior LISP and go there."
  (interactive (comint-get-source
		"Compile Lisp file: " lisp-prev-l/c-dir/file
		lisp-source-modes nil))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
	       		     (file-name-nondirectory file-name)))
  (ilisp-init t)
  (ilisp-send
   (format (ilisp-value 'ilisp-compile-file-command) file-name
	   (or extension (ilisp-value 'ilisp-binary-extension)))
   (concat "Compile " file-name) 'compile
   t))

;;;%Dialects
(defun lisp-add-dialect (dialect)
  "Add DIALECT as a supported ILISP dialect."
  (if (not (lisp-memk dialect ilisp-dialects 'car))
      (setq ilisp-dialects
	    (cons (list dialect) ilisp-dialects))))

;;;
(defun lisp-buffer-and-program (buffer)
  "Return the buffer and the program to run in buffer.  BUFFER is the
default buffer." 
  (if current-prefix-arg
      (list (read-from-minibuffer "Buffer: " buffer)
	    (lisp-read-program "Program: " nil))
      (list buffer nil)))

;;;
(defmacro defdialect (dialect full-name parent &rest body)
  "Define a new ILISP dialect.  DIALECT is the name of the function to
invoke the inferior LISP. The hook for that LISP will be called
DIALECT-HOOK.  FULL-NAME is a string that describes the inferior LISP.
PARENT is the name of the parent dialect."
  (let ((setup (read (format "setup-%s" dialect)))
	(hook (read (format "%s-hook" dialect)))
	(dialects (format "%s" dialect)))
    (`
     (progn
       (defvar (, hook) nil (, (format "*Inferior %s hook." full-name)))
       (defun (, setup) (buffer)
	 (, (format "Set up for interacting with %s." full-name))
	 (, (read (format "(setup-%s buffer)" parent)))
	 (,@ body)
	 (setq ilisp-dialect (cons '(, dialect) ilisp-dialect))
	 (run-hooks '(, (read (format "%s-hook" dialect)))))
       (defun (, dialect) (buffer program)
	 (, (format "Create an inferior %s.  With prefix, prompt for buffer and program."
		   full-name))
	 (interactive (lisp-buffer-and-program (, dialects)))
	 (setq ilisp-last-buffer (current-buffer))
	 ((, setup) buffer)
	 (if program (setq ilisp-program program))
	 (ilisp buffer))
       (lisp-add-dialect (, dialects))))))

;;;%%ilisp
(defun setup-ilisp (buffer)
  "Set up for interacting with an inferior LISP."
  (set-buffer (get-buffer-create "*ilisp-send*"))
  (lisp-mode)
  (setq ilisp-buffer (format "*%s*" buffer))
  (set-buffer (get-buffer-create ilisp-buffer))
  (setq major-mode 'ilisp-mode
	mode-name "ILISP")
  (lisp-mode-variables t)
  (use-local-map ilisp-mode-map)
  ;; Set variables to nil
  (let ((binary ilisp-binary-extension)
	(init ilisp-init-binary-extension)
	(vars ilisp-locals))
    (while (not (null vars))
      (make-local-variable (car vars))
      (set (car vars) nil)
      (setq vars (cdr vars)))
    ;; Preserve from initialization
    (if binary (setq ilisp-binary-extension binary))
    (if init (setq ilisp-init-binary-extension init)))
  ;; Comint defaults
  (setq comint-prompt-regexp "^[^<> ]*>+:? *"
	input-ring-size 200
	comint-get-old-input 'ilisp-get-old-input
	comint-input-sentinel (function ignore)
	comint-input-filter 'ilisp-input-filter
	comint-input-sender 'comint-default-send
	comint-eol-on-send t)
  ;; Comint-ipc defaults
  (setq comint-send-newline t
	comint-output-buffer " *Output*"
	comint-error-buffer " *Error Output*"
	comint-output-filter (function identity)
	comint-interrupt-start 'comint-interrupt-start
	comint-handler 'ilisp-handler
	comint-update-status 'ilisp-update-status
	comint-prompt-status 'comint-prompt-status
	comint-abort-hook 'ilisp-abort-handler)
  (setq ilisp-init-hook '(ilisp-init)
	ilisp-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
	ilisp-filter-length 3
	ilisp-error-filter 'ilisp-error-filter
	ilisp-error-regexp ".*" 
	ilisp-program "lisp"
	ilisp-locator 'lisp-locate-ilisp
	ilisp-calls-locator 'lisp-locate-calls)
  (run-hooks 'ilisp-mode-hook))

(defun run-ilisp ()
  "Create an inferior LISP prompting for dialect.  With prefix, prompt
for buffer name as well."
  (interactive)
  (let ((dialect (completing-read "Dialect: " ilisp-dialects nil t)))
    (if (not (zerop (length dialect)))
	(call-interactively (read dialect)))))

;;;%%Common LISP
(defdialect clisp "Common LISP"
  ilisp
  (if (not (fboundp 'common-lisp-indent-hook))
      (load "cl-indent"))
  (setq lisp-indent-hook 'common-lisp-indent-hook)
  (setq ilisp-load-or-send-command 
	"(or (and (fboundp 'user::ilisp-matching-symbols) t)
             (and (load \"%s\" :if-does-not-exist nil) t)
             (and (load \"%s\" :if-does-not-exist nil) t))")
  (ilisp-load-init 'clisp "clisp.lisp")
  (setq ilisp-package-regexp "^[^;]*(in-package[ \t\n]*"
	ilisp-package-command "%s"
	ilisp-last-command "*"
	ilisp-save-command "(progn (user:ilisp-save) %s)"
	ilisp-restore-command "(user:ilisp-restore)"
	ilisp-block-command "(progn %s)"
	ilisp-eval-command "(user:ilisp-eval \"%s\" \"%s\" \"%s\")"
	ilisp-defvar-regexp "(defvar[ \t\n]")
  (setq ilisp-defvar-command 
	"(user:ilisp-eval \"(let ((form '%s)) (progn (makunbound (second form)) (eval form)))\" \"%s\" \"%s\")")
  (setq ilisp-compile-command "(user:ilisp-compile \"%s\" \"%s\" \"%s\")"
	ilisp-describe-command "(user:ilisp-describe \"%s\" \"%s\")"
	ilisp-arglist-command "(user:ilisp-arglist \"%s\" \"%s\")")
  (setq ilisp-documentation-types
	'(("function") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))
  (setq ilisp-documentation-command
	"(user:ilisp-documentation \"%s\" \"%s\" \"%s\")")
  (setq ilisp-macroexpand-1-command 
	"(user:ilisp-macroexpand-1 \"%s\" \"%s\")")
  (setq ilisp-macroexpand-command "(user:ilisp-macroexpand \"%s\" \"%s\")")
  (setq ilisp-complete-command 
	"(user:ilisp-matching-symbols \"%s\" \"%s\" %s %s %s)")
  (setq ilisp-locator 'lisp-locate-clisp)
  (setq ilisp-source-types 
	'(("function") ("macro") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))
  (setq ilisp-callers-command "(user:ilisp-callers \"%s\" \"%s\")"
	ilisp-trace-command "(user:ilisp-trace \"%s\" \"%s\")"
	ilisp-untrace-command "(user:ilisp-untrace \"%s\" \"%s\")")
  (setq ilisp-directory-command
	"(setq *default-pathname-defaults* (parse-namestring \"%s\"))")
  (setq ilisp-load-command "(load \"%s\")")
  (setq ilisp-compile-file-command 
	"(user:ilisp-compile-file \"%s\" \"%s\")"))

;;;%%%Allegro
(defun allegro-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (eq 1 (string-match "[0-9]+" old))))
 	 (old-level (if was-in-break
 			(string-to-int (substring old 1))
 			0))
 	 (is-in-break (eq 1 (string-match "[0-9]+" new)))
 	 (new-level (if is-in-break
 			(string-to-int (substring new 1))
 			0)))
    (<= new-level old-level)))
 
;;;
(defdialect allegro "Allegro Common LISP"
  clisp
  (ilisp-load-init 'allegro "allegro.lisp")
  (setq comint-fix-error ":pop"
	comint-continue ":cont"
	comint-interrupt-regexp  "Error: [^\n]* interrupt\)")
  (setq comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'allegro-check-prompt))))
  (setq ilisp-program "cl")
  ;; <cl> or package> at top-level
  ;; [0-9c] <cl> or package> in error
  (setq comint-prompt-regexp "^\\(\\[[0-9]*c*\\] \\|\\)\\(<\\|\\)[A-Z]*> ")
  (setq ilisp-error-regexp "\\(Error:[^\n]*\\)\\|\\(Break:[^\n]*\\)")
  (setq ilisp-binary-command "excl:*fasl-default-type*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(user:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-init-binary-command 
	"(let ((ext #+m68k \"68fasl\"
		    #+sparc \"sfasl\"
		    #+iris4d \"ifasl\"
                    #+dec3100 \"pfasl\"))
	   (push (make-pathname :type ext) system:*load-search-list*)
           ext)"))

;;;%%%Lucid
(defun lucid-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (eq 1 (string-match "-+" old))))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (eq 1 (string-match "-+" new)))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect lucid "Lucid Common LISP"
  clisp
  (ilisp-load-init 'lucid "lucid.lisp")
  (setq comint-prompt-regexp "^\\(->\\)+ \\|[^> ]*> "
	comint-fix-error ":a"
	comint-continue ":c"
	comint-interrupt-regexp ">>Break: Keyboard interrupt"
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'lucid-check-prompt))))
  (setq ilisp-error-regexp ">>[^\n]*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(user:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-binary-command 
	"(first (last lucid::*load-binary-pathname-types*))"))

;;;%%%KCL
(defdialect kcl "Kyoto Common LISP" clisp
  (setq comint-prompt-regexp "^>+ *"))

;;;%%%CMULisp
(defun cmulisp-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match "]+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match "]+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect cmulisp "CMU Common LISP"
  clisp
  (ilisp-load-init 'cmu "cmulisp.lisp")
  (setq comint-prompt-regexp "\\([0-9]+\\]+\\|\\*\\) "
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'cmulisp-check-prompt)))
	ilisp-error-regexp "Error [^\n]*"
	ilisp-arglist-command "(ext:arglist \"%s\" \"%s\")"
	ilisp-find-source-command "(ext:source-file \"%s\" \"%s\" \"%s\")"
	comint-fix-error ":pop"
	comint-continue ":go"
	comint-interrupt-regexp "Software Interrupt"
	ilisp-binary-extension "fasl"))

;;;%%Scheme
(defdialect scheme "Scheme" ilisp
  (setq ilisp-program "scheme")
  (setq ilisp-block-command "(begin %s)")
  (setq ilisp-load-command "(load \"%s\")")
  )

;;;Cscheme
;;; This has a problem since interrupts cause things to crash
;(defdialect cscheme "C Scheme"
;  scheme
;  (setq comint-prompt-regexp
;   "^[0-9]+ \\([\\]=]=>\\|Error->\\|Bkpt->\\|Debug->\\|Where->\\) ")
;  (setq ilisp-program "cscheme")
;  (setq ilisp-binary-extension "bin")
;  )

;;;Oaklisp
(defdialect oaklisp "Oaklisp Scheme"
  scheme
  (setq comint-prompt-regexp ">+ ")
  (setq comint-fix-error "(ret 0)")
  (setq ilisp-last-command "*")
  (setq ilisp-describe-command "(describe %s)")
  )

;;;%ilisp-mode
(defvar ilisp-documentation
  "Major mode for interacting with an inferior LISP process.  Runs a
LISP interpreter as a subprocess of Emacs, with Lisp I/O through an
Emacs buffer.

To start a LISP use M-x run-ilisp, or a specific dialect like M-x
allegro.  If called with a prefix you will be prompted for a buffer
name and a program to run.  If there are multiple LISP's, use the
dialect name or select-ilisp \(\\[select-ilisp]) to select the current
ILISP buffer.

Currently supported LISP dialects include:
 clisp
   allegro
   lucid
   kcl
   cmulisp
 scheme
   oaklisp

Customization: Entry to this mode runs the hooks on comint-mode-hook and
ilisp-mode-hook and then DIALECT-hooks specific to LISP
dialects in the nesting order above.  For more information on creating
a new dialect see ilisp.el.

Most of these key bindings work in both Lisp Mode and ILISP mode.
There are a few additional and-go bindings found in Lisp Mode.
\\{ilisp-mode-map}

ILISP uses a dynamically sized pop-up window that can be buried and
scrolled from any window for displaying output.  By default the
smallest window will have just one line.  If you like bigger windows,
set window-min-height to the number of lines desired plus one.  The
variable popper-pop-buffers has a list of temporary buffer names that
will be displayed in the pop-up window.  By default only
\*Typeout-window* and \*Completions* will be displayed in the pop-up
window.  If you want all temporary windows to use the pop-up window,
set popper-pop-buffers to T.  The variable popper-buffers-to-skip has
a list of the buffer names popper-other-window
\(\\[popper-other-window]) skips.  popper-bury-output
\(\\[popper-bury-output]) burys the output window.
popper-scroll-output \(\\[popper-scroll-output]) scrolls the output
window if it is already showing, otherwise it pops it up.  If it is
called with a negative prefix, it will scroll backwards.
popper-grow-output \(\\[popper-grow-output]) will grow the output
window if showing by the prefix number of lines.  Otherwise, it will
pop the window up.

If you are running epoch, the popper window will be in a separate
X window that is not automatically grown or shrunk.  The variable
popper-screen-properties can be used to set window properties for that
window. 

Each ILISP buffer has a command history associated with it.  Commands
that do not match ilisp-filter-regexp and that are longer than
ilisp-filter-length and that do not match the immediately prior
command will be added to this history.  comint-previous-input
\(\\[comint-previous-input]) and comint-next-input
\(\\[comint-next-input]) cycle through the input history.
comint-previous-similar-input \(\\[comint-previous-similar-input])
cycles through input that has the string typed so far as a prefix.

See comint-mode documentation for more information on comint commands.

A number of commands refer to \"defun\".  A \"defun\" is a list that
starts at the left margin in a LISP buffer, or after a prompt in the
ILISP buffer.  So the commands refer to the \"defun\" that contains
point.

When you send something to LISP, the status light will reflect the
progress of the command. In a lisp mode buffer the light will reflect
the status of the currently selected inferior LISP unless
lisp-show-status is nil.  If you want to find out what command is
currently running, use the command status-lisp \\(\[status-lisp]).  If
you call it with a prefix, the pending commands will be displayed as well.

If you are want to abort the last command you can use
\\(\[keyboard-quit]).  If you want to abort all commands, you should
use the command abort-commands-lisp \\(\[abort-commands-lisp]).
Commands that are aborted will be put in the buffer *Aborted Commands*
so that you can see what was aborted.  If you want to abort the
currently running top-level command, use interrupt-subjob-ilisp
\(\\[interrupt-subjob-ilisp]).

bol-ilisp \(\\[bol-ilisp]) will go after the prompt as defined by
comint-prompt-regexp or ilisp-other-prompt.  

\\[return-ilisp] knows about prompts and sexps.  If an sexp is not
complete, it will indent properly.  When an entire sexp is complete,
it is sent to the inferior LISP together with a new line.  If you edit
old input, the input will be copied to the end of the buffer first.

\\[close-and-send-lisp] will close the current sexp, indent it, then
send it to the current inferior LISP.

\\[indent-line-ilisp] indents for LISP.  With prefix, shifts rest of
expression rigidly with the current line.

\\[newline-and-indent-ilisp] will insert a new line and then indent to
the appropriate level.  If you are at the end of the inferior LISP
buffer and an sexp, the sexp will be sent to the inferior LISP without
a trailing newline.  

\\[indent-sexp-ilisp] will indent each line in the next sexp.

\\[backward-delete-char-untabify] converts tabs to spaces as it moves
back.

switch-to-lisp \(\\[switch-to-lisp]) will pop to the current ILISP
buffer or if already in an ILISP buffer, it will return to the buffer
that last switched to an ILISP buffer.  With a prefix, it will also go
to the end of the buffer.  If you do not want it to pop, set
pop-up-windows to nil.

reposition-window-lisp \(\\[reposition-window-lisp]) will put the
start of the current defun at the top of the current window.

lisp-previous-buffer \(\\[lisp-previous-buffer]) will switch to the
last visited buffer in the current window.

find-unbalanced-lisp \(\\[find-unbalanced-lisp]) will find unbalanced
parentheses in the current buffer.  When called with a prefix it will
look in the current region.

close-all-lisp \(\\[close-all-lisp]) will close all outstanding
parentheses back to the containing form, or a previous left bracket
which will be converted to a left parentheses.  If there are too many
parentheses, they will be deleted unless there is text between the
last parenthese and the end of the defun.  If called with a prefix,
all open left brackets will be closed.

reindent-lisp \(\\[reindent-lisp]) will reindent the current paragraph
if in a comment or string.  Otherwise it will close the containing
defun and reindent it.

comment-region-lisp \(\\[comment-region-lisp]) will put prefix copies of
comment-start before and comment-end's after the lines in region.  To
uncomment a region, use a minus prefix.

The very first inferior LISP command executed may send some forms to
initialize the inferior LISP.

Each time an inferior LISP command is executed, the last form sent can be
seen in the \*ilisp-send* buffer.

The first time an inferior LISP mode command is executed in a Lisp
Mode buffer, the package will be determined by using the regular
expression ilisp-package-regexp to find a package sexp and then
passing that sexp to the inferior LISP through ilisp-package-command.
For the clisp dialect, this will find the first \(in-package PACKAGE)
form in the file.  A buffer's package will be displayed in the mode
line.  reparse-attribute-list \(\\[reparse-attribute-list]) will
update the current package from the buffer.  If a buffer has no
specification, forms will be evaluated in the current inferior LISP
package.

describe-lisp, arglist-lisp, documentation-lisp, macroexpand-1-lisp,
macroexpand-lisp, edit-definitions-lisp, who-calls-lisp,
edit-callers-lisp and trace-lisp will switch whether they prompt for a
response or use a default when called with a negative prefix.  If they
are prompting, there is completion through the inferior LISP by using
TAB or ESC-TAB.  When you are entering an expression in the
minibuffer, all of the normal ilisp commands like arglist-lisp also
work.

Commands that work on a function will use the nearest previous
function symbol.  This is either a symbol after a #' or the symbol at
the start of the current list.

describe-lisp \(\\[describe-lisp]) will describe the previous sexp.
If there is no previous-sexp and you are in an ILISP buffer, the
previous result will be described.

arglist-lisp \(\\[arglist-lisp]) will return the arglist of the
current function.  With a numeric prefix, the leading paren will be
removed and the arglist will be inserted into the buffer.

documentation-lisp \(\\[documentation-lisp]) infers whether function or
variable documentation is desired.  With a negative prefix, you can
specify the type of documentation as well.

macroexpand-lisp \(\\[macroexpand-lisp]) and macroexpand-1-lisp
\(\\[macroexpand-1-lisp]) will be applied to the next sexp.  They will
insert their result into the buffer if called with a numeric prefix.

complete-lisp \(\\[complete-lisp]) will try to complete the previous
symbol in the current inferior LISP.  Partial completion is supported
unless ilisp-prefix-match is set to T.  (If you set it to T, inferior
LISP completions will be faster.)  With partial completion, \"p--n\"
would complete to \"position-if-not\" in Common LISP.  If the symbol
follows a left paren or a #', only symbols with function cells will be
considered.  If the symbol starts with a \* all possible completions
will be considered.  To force all symbols to be considered, call it
with a prefix.  Only external symbols are considered if there is a
package qualification with only one colon.  The first time you try to
complete a string the longest common substring will be inserted.  If
you try to complete again, you can see the possible completions.  If
you are in a string, then filename completion will be done instead.
And if you try to complete a filename twice, you will see a list of
possible completions.

trace-lisp \(\\[trace-lisp]) traces the previous function symol.  When
called with a numeric prefix the function will be untraced.

default-directory-lisp \(\\[default-directory-lisp]\) sets the default
directory in the inferior LISP to the directory of the current buffer.

The eval/compile commands verify that their expressions are balanced
and then send the form to the inferior LISP.  If called with a
positive prefix, the result of the operation will be inserted into the
buffer after the form that was just sent.  If lisp-wait-p is t, then
EMACS will display the result of the command in the minibuffer or a
pop-up window.  If lisp-wait-p is nil, (the default) the send is done
asynchronously and the results will be brought up only if there is
more than one line or there is an error.  In this case, you will be
given the option of ignoring the error, keeping it in another buffer
or keeping it and aborting all pending sends.  If there is not a
command already running in the inferior LISP, you can preserve the
break loop.  If called with a negative prefix, the sense of
lisp-wait-p will be inverted for the next command.  The and-go
versions will perform the operation and then immediately switch to the
ILISP buffer where you will see the results of executing your form.

When an eval is done of a single form matching ilisp-defvar-regexp,
the corresponding symbol will be unbound and the value assigned again.

The following commands all deal with finding things in source code.
The first time that one of these commands is used, there may be some
delay while the source module is loaded.  When searching files, the
first applicable rule is used: 1) try the inferior LISP, 2) try a tags
file if defined, 3) try all buffers in one of lisp-modes or all files
defined using lisp-directory.

lisp-directory \(\\[lisp-directory]) defines a set of files to be
searched by the source code commands.  It prompts for a directory and
sets the source files to be those in the directory that match entries
in auto-mode-alist for modes in lisp-source-modes.  With a positive
prefix, the files are appended.  With a negative prefix, all current
buffers that are in one of lisp-source-modes will be searched.  This
is also what happens by default.  Using this command stops using a
tags file.

edit-definitions-lisp \(\\[edit-definitions-lisp]) will find a
particular type of definition for a symbol.  It trys to use the rules
described above.  The files to be searched are listed in the buffer
\*Edit-Definitions*.  If lisp-edit-files is nil, no search will be
done if not found through the inferior LISP.  The variable
ilisp-locator contains a function that when given the name and type
should be able to find the appropriate definition in the file.  There
is often a flag to cause your LISP to record source files that you
will need to set in the initialization file for your LISP.  The
variable is \*record-source-files* in both allegro and lucid.  Once a
definition has been found, next-definition-lisp
\(\\[next-definition-lisp]) will find the next definition.  \(Or the
previous definition with a prefix.)

edit-callers-lisp \(\\[edit-callers-lisp]) will generate a list of all
of the callers of a function in the current inferior LISP and edit the
first caller using edit-definition.  Each successive call to
next-caller-lisp \(\\[next-caller-lisp]) will edit the next caller.
\(Or the previous caller with a prefix.)  The list is stored in the
buffer \*All-Callers*.  You can also look at the callers by doing
who-calls-lisp \(\\[who-calls-lisp]).

search-lisp \(\\[search-lisp]) will search the current tags files,
lisp directory files or buffers in one of lisp-source-modes for a
string or a regular expression when called with a prefix.
\(\\[next-definition-lisp]) will find the next definition.  \(Or the
previous definition with a prefix.)

replace-lisp \(\\[replace-lisp]) will replace a string (or a regexp
with a prefix) in the current tags files, lisp directory files or
buffers in one of lisp-source-modes.

The following commands all deal with making a number of changes all at
once.  The first time one of these commands is used, there may be some
delay as the module is loaded.  The eval/compile versions of these
commands are always executed asynchronously.

mark-change-lisp \(\\[mark-change-lisp]) marks the current defun as
being changed.  A prefix causes it to be unmarked.  clear-changes-lisp
\(\\[clear-changes-lisp]) will clear all of the changes.
list-changes-lisp \(\\[list-changes-lisp]) will show the forms
currently marked. 

eval-changes-lisp \(\\[eval-changes-lisp]), or compile-changes-lisp
\(\\[compile-changes-lisp]) will evaluate or compile these changes as
appropriate.  If called with a positive prefix, the changes will be
kept.  If called with a negative prefix, the commands will be sent to
the inferior LISP without waiting.  If there is an error, the process
will stop and show the error and all remaining changes will remain in
the list.  All of the results will be kept in the buffer
*Last-Changes*.

File commands in lisp-source-mode buffers keep track of the last used
directory and file.  If the point is on a string, that will be the
default.  If the buffer is one of lisp-source-modes, the buffer file
will be the default.  Otherwise, the last file used in a
lisp-source-mode will be used.

find-file-lisp \(\\[find-file-lisp]) will find a file.  If it is in a
string, that will be used as the default.  Symbolic links are expanded
so that different references to the same file will end up with the
same buffer. 

load-file-lisp \(\\[load-file-lisp]) will load a file into the inferior
LISP.  You will be given the opportunity to save the buffer if it has
changed and to compile the file if the compiled version is older than
the current version.

compile-file-lisp \(\\[compile-file-lisp]) will compile a file in the
current inferior LISP."
  "Documentation string for ILISP mode.")

;;;
(defun ilisp-set-doc (function string)
  "Set the documentation of the symbol FUNCTION to STRING."
  (let* ((old-function (symbol-function function))
	 (old-doc (cdr (cdr old-function))))
    ;; I did not use rplacd so that I can replace read-only objects
    (fset function
	  (nconc (list (car old-function)
		       (car (cdr old-function))
		       string)
		 (if (or (stringp (car old-doc)) (numberp (car old-doc)))
		     (cdr old-doc)
		     old-doc)))))

;;;
(defun ilisp-mode ()
  (interactive)
  (run-ilisp))
(ilisp-set-doc 'ilisp-mode ilisp-documentation)
(ilisp-set-doc 'lisp-mode ilisp-documentation)

;;;%%ILISP
(defun lisp-command-args (string)
  "Break up STRING into (command args ...)."
  (let ((len (length string))
	(position 0)
	(arg 0)
	(args nil))
    (while (< position len)
      (if (eq (aref string position) ?\ )
	  (setq args (cons (substring string arg position)  args)
		arg (1+ position)))
      (setq position (1+ position)))
    (setq args (reverse (cons (substring string arg position)  args)))
    args))

;;;
(defun ilisp (name)
  "Run an inferior LISP process NAME, input and output via buffer *name*.
If there is a process already running in *name*, just switch to that buffer.
Takes the program name from the variable ilisp-program.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (set-buffer ilisp-buffer)
  (if (not (comint-check-proc ilisp-buffer))
      (let* ((dialect (car ilisp-dialect))
	     (program ilisp-program)
	     (args (lisp-command-args program))
	     ;; Use pipes so that strings can be long
	     (process-connection-type nil)
	     (names (format "%s" name))
	     start)
	(apply 'make-comint name (car args) nil (cdr args))
	(comint-setup-ipc)
	;; Comint-mode nukes locals, so we have to set them up again
	(eval (read (format "(setup-%s \"%s\")" dialect name)))
	(rplaca (car comint-send-queue) (function (lambda ()
					  (run-hooks 'ilisp-init-hook))))
	(setq ilisp-initialized (lisp-del ilisp-buffer ilisp-initialized))
	(if (not (lisp-memk names ilisp-buffers 'car))
	    (setq ilisp-buffers (cons (list names) ilisp-buffers)))
	(pop-to-buffer ilisp-buffer)
	(setq start (window-start (selected-window))
	      ilisp-program program)
	(insert (format "Starting %s ...\n" ilisp-program))
	(set-marker (process-mark (ilisp-process)) (point))
	(if ilisp-motd
	    (progn (comint-display-output (format ilisp-motd ilisp-version))
		   (set-window-start (selected-window) start)))
	(if (not ilisp-prefix-match) (require 'completer)))
      (pop-to-buffer ilisp-buffer)))

;;;%Bugs
(defun ilisp-bug ()
  "Generate an ilisp bug report."
  (interactive)
  (let ((buffer (current-buffer)))
    (mail)
    (insert ilisp-bugs-to)
    (search-forward (concat "\n" mail-header-separator "\n"))
    (forward-line 1)
    (insert (emacs-version))
    (insert 
     (format "\nWindow System: %s %s" window-system window-system-version))
    (insert (format "\n\nILISP V%s" ilisp-version))
    (let ((mode (save-excursion (set-buffer buffer) major-mode))
	  (local (if ilisp-buffer (buffer-local-variables (ilisp-buffer))))
	  string)
      (if (or (memq mode lisp-source-modes) (memq mode ilisp-modes))
	  (progn
	    (if (and (ilisp-buffer) 
		     (memq 'clisp (ilisp-value 'ilisp-dialect)))
		(insert (format "\nLISP: %s"
				(comint-remove-whitespace
				 (car (comint-send
				       (save-excursion
					 (set-buffer buffer)
					 (ilisp-process))
				       "(lisp-implementation-version)"
				       t t 'version))))))
	    (save-excursion
	      (set-buffer buffer)
	      (let ((point (point))
		    (start (lisp-defun-begin))
		    (end (lisp-end-defun-text t)))
		(setq string
		      (format "
Mode: %s
Start: %s
End: %s
Point: %s
Point-max: %s
Code: %s"
			      major-mode start end point (point-max)
			      (buffer-substring start end)))))
	    (insert string)
	    (while local
	      (let* ((entry (car local))
		     (name (car entry)))
		(if (string-match "ilisp-\\|comint-" 
				  (format "%s" name))
		    (insert (format "\n%s: %s" name (cdr entry))))
		(setq local (cdr local)))))))
    (goto-char (point-max))
    (insert (format "\nLossage: %s" (key-description (recent-keys))))
    (goto-char (point-min))
    (re-search-forward "^Subject")
    (end-of-line)))

;;;%Modes
(set-default 'auto-mode-alist
	     (append '(("\\.cl$" . lisp-mode) ("\\.lisp$" . lisp-mode))
		     auto-mode-alist))

;;;%Bindings
(defun ilisp-defkey (keymap key command)
  "Define KEYMAP ilisp-prefix+KEY as command."
  (let ((prefix-map (lookup-key keymap ilisp-prefix)))
    (if (not (keymapp prefix-map))
	(setq prefix-map
	      (define-key keymap ilisp-prefix (make-sparse-keymap))))
    (define-key prefix-map key command)))

;;;
(defun lisp-bindings (keymap &optional inferior-p)
  "Set up the bindings for interacting with an inferior LISP in
KEYMAP."
  (if inferior-p
      (progn (define-key keymap "\C-m" 'return-ilisp)
	     (define-key keymap "\C-a" 'bol-ilisp)
	     (define-key keymap "\C-c\C-c" 'interrupt-subjob-ilisp))
      (ilisp-defkey keymap "\C-c" 'compile-defun-and-go-lisp)
      (define-key keymap "\C-m" 'newline-and-indent-ilisp))
  (define-key   keymap "]"        'close-all-lisp)
  (define-key   keymap "\M-q"     'reindent-lisp)
  (define-key   keymap "\M-\C-m"  'close-and-send-lisp)
  (define-key   keymap "\t"       'indent-line-ilisp)
  (define-key   keymap "\n"       'newline-and-indent-ilisp)
  (define-key   keymap "\M-\C-q"  'indent-sexp-ilisp)
  (ilisp-defkey keymap ";"        'comment-region-lisp)
  (ilisp-defkey keymap ")"        'find-unbalanced-lisp)
  (define-key   keymap "\M-\C-a"  'beginning-of-defun-lisp)
  (define-key   keymap "\M-\C-e"  'end-of-defun-lisp)
  (define-key   keymap "\C-\M-r"  'reposition-window-lisp)
  (ilisp-defkey keymap "i"        'describe-lisp)
  (ilisp-defkey keymap "a"        'arglist-lisp)
  (ilisp-defkey keymap "d"        'documentation-lisp)
  (ilisp-defkey keymap "m"        'macroexpand-1-lisp)
  (ilisp-defkey keymap "M"        'macroexpand-lisp)
  (define-key   keymap "\M-,"     'next-definition-lisp)
  (define-key   keymap "\M-."     'edit-definitions-lisp)
  (define-key   keymap "\M-?"     'search-lisp)
  (define-key   keymap "\M-\""    'replace-lisp)
  (ilisp-defkey keymap "^"        'edit-callers-lisp)
  (define-key   keymap "\M-`"     'next-caller-lisp)
  (define-key   keymap "\M-\t"    'complete-lisp)
  (ilisp-defkey keymap "r"        'eval-region-lisp)
  (define-key   keymap "\M-\C-x"  'eval-defun-lisp) ; Gnu convention
  (ilisp-defkey keymap "e"        'eval-defun-lisp)
  (ilisp-defkey keymap "n"        'eval-next-sexp-lisp)
  (ilisp-defkey keymap "p"        'eval-prev-sexp-lisp)
  (ilisp-defkey keymap "w"        'compile-region-lisp)
  (ilisp-defkey keymap "c"        'compile-defun-lisp)
  (ilisp-defkey keymap "\C-r"     'eval-region-and-go-lisp)
  (ilisp-defkey keymap "\C-e"     'eval-defun-and-go-lisp)
  (ilisp-defkey keymap "\C-n"     'eval-next-sexp-and-go-lisp)
  (ilisp-defkey keymap "\C-p"     'eval-prev-sexp-and-go-lisp)
  (ilisp-defkey keymap "\C-w"     'compile-region-and-go-lisp)
  (ilisp-defkey keymap "t"        'trace-lisp)
  (ilisp-defkey keymap "!"        'default-directory-lisp)
  (ilisp-defkey keymap " "        'mark-change-lisp)
  (let ((ilisp-prefix (concat ilisp-prefix "*")))
    (ilisp-defkey keymap "l"      'list-changes-lisp)
    (ilisp-defkey keymap "e"      'eval-changes-lisp)
    (ilisp-defkey keymap "c"      'compile-changes-lisp)
    (ilisp-defkey keymap "0"      'clear-changes-lisp))
  (ilisp-defkey keymap "b"        'switch-to-lisp)
  (ilisp-defkey keymap "z"        'switch-to-lisp)
  (ilisp-defkey keymap "g"        'abort-commands-lisp)
  (ilisp-defkey keymap "s"        'status-lisp)
  (ilisp-defkey keymap "S"        'select-ilisp)
  (define-key   keymap "\C-x\C-f" 'find-file-lisp)
  (ilisp-defkey keymap "l"        'load-file-lisp)
  (ilisp-defkey keymap "k"        'compile-file-lisp))

;;;
(defun ilisp-bindings ()
  "Set up the key bindings for LISP and ILISP buffers."
  (setq ilisp-mode-map (full-copy-sparse-keymap comint-mode-map))
  (lisp-mode-commands ilisp-mode-map)
  (lisp-bindings ilisp-mode-map t)
  (if (boundp 'lisp-mode-map) (lisp-bindings lisp-mode-map))
  (if (boundp 'scheme-mode-map) (lisp-bindings scheme-mode-map))
  (ilisp-defkey emacs-lisp-mode-map ";" 'comment-region-lisp)
  (ilisp-defkey global-map "b" 'switch-to-lisp)
  (ilisp-defkey global-map "1" 'popper-bury-output)
  (ilisp-defkey global-map "v" 'popper-scroll-output)
  (ilisp-defkey global-map "g" 'popper-grow-output))

;;;
(setq ilisp-directory (ilisp-directory "ilisp.el" load-path))

;;; All done
(provide 'ilisp)
(run-hooks 'ilisp-site-hook)
(run-hooks 'ilisp-load-hook)
(if (not ilisp-mode-map) (ilisp-bindings))

