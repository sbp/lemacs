;;; -*-Emacs-Lisp-*-
;;;%Header
;;; Inferior LISP interaction package for GNU Emacs.  Version 4.12
;;; Copyright (C) 1990, 1991, 1992 Chris McConnell, ccm@cs.cmu.edu.
;;; Hacked for Lucid GNU Emacs

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

;;; ILISP replaces the standard inferior LISP mode. It is based on
;;; comint mode and derived from a number of different interfaces
;;; including Symbolics, cmulisp, and Thinking Machines.  There are
;;; many people that have taken the time to report bugs, make
;;; suggestions and even better send code to fix bugs or implement new
;;; features.  Special thanks to Todd Kaufmann for the texinfo file,
;;; work on bridge, epoch-pop and for really exercising everything.
;;; Thanks to Neil Smithline, David Braunegg, Fred White, Jim Healy,
;;; Larry Stead, Hans Chalupsky, Michael Ernst, Frank Ritter, Tom
;;; Emerson, David Duff, Dan Pierson, Michael Kashket, Jamie Zawinski,
;;; Bjorn Victor and Brian Dennis for bug reports, suggestions and
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
;;; and the hooks available for customizing it, see the file
;;; comint.el.

;;; Throughout this file you will find comment lines with %'s on them.
;;; These lines define sections for outline mode which I use while
;;; programming to temporarily hide code.

;;;%%SITE INFORMATION
;;; The files you need to use ilisp are:
;;;  ilisp.emacs     File with sample .emacs code for ILISP.
;;;  symlink.el      Expand pathnames resolving links.
;;;  completer.el    Partial completion code.
;;;  completion.el   Completion package from TMC.
;;;  popper.el       Shrink-wrapped temporary windows.
;;;  epoch-pop.el    Popper for epoch.
;;;  bridge.el       Process to process communication.
;;;  comint.el       The basic comint abstraction.
;;;  comint-ipc.el   Extensions for sending commands and getting results.
;;;  ilisp-ext.el    Standalone lisp-mode extensions.
;;;  ilisp-src.el    Ilisp source code module.
;;;  ilisp-bat.el    Ilisp batch code module.
;;;  ilisp.el        Actual ilisp definitions.
;;;  ilisp.texi      Texinfo file.
;;;  ilisp.ps        Postscript version of the manual.
;;;  ilisp.info      Info file.
;;;  *.lcd           Descriptors for the Lisp Code Directory.
;;;  *.lisp          Each dialect will have one of these files.
;;;
;;; All of the .el files in the ilisp directory should be
;;; byte-compiled by typing C-u M-x byte-recompile-directory.  Before
;;; compiling, make sure that load-path has the location of the files
;;; on it.  If you plan to use epoch, you must make sure that the
;;; epoch EMACS code is loaded before compiling epoch-pop.  If you do
;;; not plan to use epoch, you should rename the epoch-pop.el file to
;;; epoch-pop so that it will not get compiled.  The first time a
;;; dialect is started, the interface files will complain about not
;;; being compiled, just hit 'i' to ignore the message.  Once a lisp
;;; dialect is started up, you should execute the EMACS command M-x
;;; ilisp-compile-inits which will compile the *.lisp files and write
;;; them to the same directory as the ilisp files.  The binary files
;;; should have a unique extension for each different combination of
;;; architecture and LISP dialect.  You will need to change
;;; ilisp-init-binary-extension/command to get additional extensions.
;;; The binary for each different architecture should be different.
;;; If you want to build the interface files into a LISP world, you
;;; will also need to set ilisp-load-inits to nil in the same place
;;; that you change ilisp-program to load the LISP world.
;;;
;;; There is an ilisp-site-hook for initializing site specific stuff
;;; like program locations when ilisp is first loaded.  You may want
;;; to define appropriate autoloads in your system Emacs start up
;;; file.
;;;
;;; ;;; CMU site
;;; (setq ilisp-site-hook
;;;       '(lambda ()
;;;         (setq ilisp-motd "CMU ILISP V%s")
;;;         (setq expand-symlinks-rfs-exists t)
;;;         (setq allegro-program "/usr/misc/.allegro/bin/cl")
;;;         (setq lucid-program "/usr/misc/.lucid/bin/lisp")))

;;;%%CUSTOMIZING DIALECTS
;;;
;;; ILISP is already set up with support for a number of dialects.
;;; Each dialect has a command NAME that will start an inferior LISP
;;; of that dialect.  NAME-hook is a hook that will run after the
;;; default settings for NAME are set up.  NAME-program is the default
;;; program for NAME. A prefix when starting a dialect will cause you
;;; to be prompted for the buffer name and the program.  When setting
;;; something in a hook, you should use the most general dialect that
;;; makes sense. Dialect definitions and their hooks are executed from
;;; least specific to most specific.  They will be executed before the
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
;;;     akcl
;;;     ibcl
;;; scheme
;;;   oaklisp
;;;
;;; If anyone figures out support for other dialects I would be happy
;;; to include it in future releases.
;;;
;;; ;;; Example of local changes and extensions to ilisp mode
;;; (setq ilisp-load-hook
;;;       '(lambda ()
;;;         ;; Change the allegro lisp program
;;;         (setq allegro-program "/usr/misc/bin/lisp")
;;;         ;; Add a new key binding
;;;         (defkey-ilisp "\C-\M-a" 'arglist-lisp)
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
;;; ilisp-init-hook         Executed after inferior LISP is initialized
;;; DIALECT-hook            Executed when dialect is set
;;;
;;; Variables you might want to set in a hook or dialect:
;;; ilisp-prefix          Keys to prefix ilisp key bindings
;;; ilisp-program         Program to start for inferior LISP
;;; ilisp-motd            String printed on startup with version
;;; lisp-wait-p           Set to T for synchronous sends
;;; lisp-no-popper        Set to T to have all output in inferior LISP
;;; lisp-show-status      Set to nil to stop showing process status
;;; ilisp-prefix-match    Set to T if you do not want partial completion
;;; ilisp-filter-regexp	  Input history filter 
;;; ilisp-filter-length   Input history minimum length
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
;;; file for more information.  All of the EMACS function names begin
;;; or end with lisp or ilisp to separate ilisp functions from
;;; functions in other packages.  Functions that work only in lisp
;;; buffers or that work in both lisp buffers and inferior lisp
;;; buffers use lisp, all other functions use ilisp.  If a function is
;;; intended to be used interactively, then the lisp or ilisp comes at
;;; the end of the function name, otherwise at the start.

;;;%%KNOWN BUGS
;;; 
;;; If you type multiple things to the top level before you get a
;;; prompt, the LISP may be running with the status light indicating
;;; ready.  This is because I have no way to distinguish between input
;;; to a program and that to the top level.
;;;
;;; When running a lisp on Ultrix, you need to set ilisp-program to
;;; "/bin/sh -c your/path/your-lisp-image".
;;; 
;;; If you get lisp output breaking up in weird places it almost
;;; certainly means that comint-prompt-regexp is not precise enough.
;;;
;;; I would like to eat Lucid's return from break in the process
;;; filter, but I can't tell how many newlines to eat after.

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
		       comint-always-scroll
		       comint-fix-error
		       comint-continue
		       comint-interrupt-regexp
		       comint-error-regexp
		       comint-output-filter
		       comint-interrupt-start
		       comint-handler
		       comint-update-status
		       comint-prompt-status
		       comint-abort-hook)
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
file system, the interface files will be sent down the pipe instead.
The value of this variable is set from DIALECT-program, or inherited
from a less specific dialect if DIALECT-program is nil.")

(defvar ilisp-motd 
  "ILISP V%s  Use M-x ilisp-bug for problems and suggestions."
  "*Message of the day format string for ILISP given VERSION. To
prevent any message from being printed, set this to nil.")

(defvar lisp-wait-p nil
  "*T if LISP eval/compile commands should wait for the result.  A
minus prefix to the command will change the sense of this switch for
just the next command.")

(defvar lisp-no-popper nil
  "*T if you want all output in the inferior LISP rather than in a
pop-up window.  You should probably also set comint-always-scroll to T
as well so that output is always visible.")

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

(deflocal ilisp-raw-echo nil
  "*Set this to T to cause echoing in raw keyboard mode.")

(deflocal ilisp-load-no-compile-query nil
  "*Set this to T to stop load querying about compile.")

;;;%%%Hooks
(defvar ilisp-site-hook nil
  "Hook for site customization of ilisp mode when it is loaded.")

(defvar ilisp-load-hook '()
  "Hook for customizing ilisp mode when it is loaded.")

(defvar ilisp-mode-hook '()
  "Hook for customizing ilisp mode.")

(deflocal ilisp-init-hook nil
  "Hook of functions to call on first prompt in inferior LISP.")

;;;%%Advanced customization
;;;%%%Commands
(deflocal ilisp-reset nil
  "String for resetting the top-level of the inferior LISP.")

(deflocal ilisp-load-or-send-command nil
  "Format string for loading BINARY if possible otherwise loading
FILE.  If you can't load either, return NIL.")

(deflocal ilisp-package-regexp nil
  "Regular expression for finding a package specification in a buffer.
The entire sexp starting with this pattern will be passed to
ilisp-package-command to find the package.")

(deflocal ilisp-package-command nil
  "Format string to find the package given PACKAGE.")

(deflocal ilisp-package-name-command nil
  "Format string to return the name of the current package.")

(deflocal ilisp-in-package-command nil
  "Format string to set the package given PACKAGE.")

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

(deflocal ilisp-inspect-command nil
  "Format string for inspecting FORM in PACKAGE.")

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
  "Format for getting default DIRECTORY.")
(deflocal ilisp-set-directory-command nil
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
(deflocal ilisp-use-map nil "Keymap to use in ILISP mode.")

(defvar ilisp-bugs-to "ccm@cs.cmu.edu" "Who to send bug reports to.")

(defvar ilisp-modes '(ilisp-mode) "List of all inferior ilisp modes.")
(defvar lisp-source-modes '(lisp-mode scheme-mode)
  "Used to determine if a buffer contains Lisp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a lisp source file by find-file-lisp, load-file-lisp and
compile-file-lisp. Used by these commands to determine defaults.")

(deflocal ilisp-no-newline nil
  "Set to T to stop ilisp from inserting a newline after a command.")

(deflocal ilisp-error-filter nil "Function to filter error output.")
(deflocal ilisp-error-regexp nil "Regular expression to match error.")

(deflocal ilisp-symbol-delimiters nil
  "Delimiters found around symbols.")

;;;%%Program
(defvar ilisp-epoch-running (and (boundp 'epoch::version) epoch::version)
  "Non-nil if epoch is running.")
(defvar ilisp-version "4.12" "Interface version.")
(defvar ilisp-directory nil "The directory that ilisp is found in.")
(defvar ilisp-mode-map nil "Key map for ILISP.")
(defvar ilisp-raw-map  nil
  "Keyboard map for sending characters directly to the inferior LISP.")
(defvar ilisp-raw-message "Raw keyboard mode until C-g"
  "Message for how to stop raw mode.")
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

;;;
(defun lisp-pop-to-buffer (buffer)
  "Like pop-to-buffer, but select a screen that buffer was shown in."
  (let ((ilisp-window (if ilisp-epoch-running
			  (epoch::get-buffer-window buffer)
			  (get-buffer-window buffer))))
    (if ilisp-window
	(select-window ilisp-window)
	;; It is not currently displayed, so find some place to display
	;; it.
	(if ilisp-epoch-running
	    ;; Select a screen that the buffer has been displayed in before
	    ;; or the current screen otherwise.
	    (epoch::select-screen
	     ;; allowed-screens in epoch 3.2, was called screens before that
	     (or (car (symbol-buffer-value 'allowed-screens buffer))
		 (epoch::current-screen))))
	(pop-to-buffer buffer)))
  (set-buffer buffer))

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
	       (start2 0) 
	       end
	       (match t))
	   (while
	       (if (setq end (string-match "[^a-zA-Z0-9]" s1 start))
		   ;; Found delimiter
		   (if (string= (substring s1 start end)
				(substring s2 start2 (+ start2 (- end start))))
		       ;; Words are the same
		       (progn (setq start (match-end 0))
			      (if (string-match
				   (regexp-quote (substring s1 end start))
				   s2 start2)
				  (setq start2 (match-end 0)) ;OK
				  (setq match nil))) ;Can't find delimiter
		       (setq match nil)) ;Words don't match 
		   nil))		;Ran out of delimiters in s1
	   (and match
		(string= (substring s1 start len1)
			 (substring s2 start2 (+ start2 (- len1 start)))))))))

;;;
(defun lisp-last-line (string)
  "Return the last line of STRING with everything else."
  (let* ((position 0))
    (while (string-match "\\(\n+\\)[^\n]" string position)
      (setq position (match-end 1)))
    (cons (substring string position)
	  (substring string 0 position))))

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
      (let ((buffer 
	     (if ilisp-buffer 
		 (or (get-buffer ilisp-buffer)
		     (get-buffer
		      (setq ilisp-buffers
			    (lisp-del (substring ilisp-buffer 1 
						 (1- (length ilisp-buffer)))
				      ilisp-buffers 
				      (function (lambda (s1 s2)
					(string= s1 (car s2)))))
			    ilisp-buffer 
			    (format "*%s*" (car (car ilisp-buffers)))))))))
	(or buffer
	    (error "You must start an inferior LISP with run-ilisp.")))))

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
      (lisp-pop-to-buffer ilisp-last-buffer)
      (if (not (memq major-mode ilisp-modes))
	  (setq ilisp-last-buffer (current-buffer)))
      (lisp-pop-to-buffer (ilisp-buffer))
      (cond (eob-p (goto-char (point-max))))))

;;;
(defun abort-commands-lisp (&optional message)
  "Abort the commands sent to the current ilisp."
  (interactive)
  (if (ilisp-value comint-aborting t)
      (message "Already aborted commands")
      (beep)
      (message (or message "Aborted commands"))
      (comint-abort-sends (ilisp-process))))

;;;
(defun panic-lisp ()
  "Panic reset for the inferior LISP."
  (interactive)
  (save-excursion
    (if (y-or-n-p "Panic reset LISP? ")
	(save-excursion
	  (set-buffer (ilisp-buffer))
	  (comint-setup-ipc t)
	  (message "LISP is reset, state is unknown"))
	(message ""))))

;;;
(defun interrupt-subjob-ilisp ()
  "Interrupt the current top level command in the inferior LISP."
  (interactive)
  (if (not (eq comint-send-queue comint-end-queue))
      (if (y-or-n-p "Abort commands before interrupting top level? ")
	  (abort-commands-lisp)
	  (message "Waiting for commands to finish")
	  (while (not (eq comint-send-queue comint-end-queue))
	    (accept-process-output)
	    (sit-for 0))))
  (message "Interrupted top level")
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
(defvar buffer-package 'not-yet-computed "Cached package name.")
(defvar buffer-mode-name nil "Original mode name.")
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
	 (make-local-variable 'buffer-mode-name)
	 (setq mode-line-process 'ilisp-status)
	 ;; go search for the first package in the current buffer
	 (let* ((lisp-buffer-package t)
		(case-fold-search t)
		(regexp (ilisp-value 'ilisp-package-regexp t))
		(spec
		 (if regexp
		     (save-excursion
		       (goto-char (point-min))
		       (if (re-search-forward regexp nil t)
			   (buffer-substring (match-beginning 0)
					     (progn 
					       (goto-char (match-beginning 0))
					       (forward-sexp)
					       (point)))))))
		(package
		 (if spec
		     (ilisp-send 
		      (format (ilisp-value 'ilisp-package-command) spec)
		      "Finding buffer package"
		      'pkg))))
	   (if (ilisp-value 'comint-errorp t)
	       (progn
		 (lisp-display-output package)
		 (error "No package"))
	       (if (and package 
			(string-match "[ \n\t:\"]*\\([^ \n\t\"]\\)*" package))
		   (setq package
			 (substring package
				    (match-beginning 1) (match-end 1)))))
	   (message "")
	   (setq buffer-package package)
	   ;; Display package in mode line
	   (if package 
	       (setq mode-name
		     (concat (or buffer-mode-name
				 (setq buffer-mode-name mode-name))
			     ":" buffer-package)))
	   buffer-package))))

;;;
(defun package-lisp ()
  "Show current inferior LISP package."
  (interactive)
  (message "Inferior LISP package is %s"
	   (ilisp-send (ilisp-value 'ilisp-package-name-command)
		       "Finding inferior LISP package" 'pkg)))

;;;
(defun set-package-lisp (package)
  "Set inferior LISP to package of buffer or a named package with prefix."
  (interactive 
   (let ((default (lisp-buffer-package)))
     (if (or current-prefix-arg (null default))
	 (let ((name
		(read-no-blanks-input 
		 (format "Package [%s]: " (lisp-buffer-package)) "")))
	   (list (if (equal name "") default name)))
	 (list default))))
  (if package
      (ilisp-send (format (ilisp-value 'ilisp-in-package-command) package)
		  (format "Set %s's package to %s" 
			  (buffer-name (ilisp-buffer))
			  package)
		  'pkg 'dispatch)
      (error "No package")))

;;;
(defun set-buffer-package-lisp (package)
  "Reset the current package of the current buffer.  With prefix
specify manually."
  (interactive (if current-prefix-arg
		   (list (read-from-minibuffer "Package: " ))
		   (list nil)))
  (if package
      (setq buffer-package package
	    mode-name (concat (or buffer-mode-name mode-name) ":" package))
      (setq buffer-package 'not-yet-computed)
      (lisp-buffer-package)))

;;;%Process interface
;;;%%Comint 
(defun ilisp-get-old-input ()
  "Snarf the sexp starting at the nearest previous prompt, or NIL if none."
  (save-excursion
    (let* ((begin (lisp-defun-begin))
	   (pmark (process-mark (get-buffer-process (current-buffer))))
	   (once (if (< (point) pmark)
		     (save-excursion (end-of-line) (point))))
	   (end nil)
	   (done nil))
      (condition-case ()
	  (while (and (not done) (< (point) (point-max)))
	    (forward-sexp)
	    (setq end (point))
	    (skip-chars-forward " \t\n")
	    (if (and once (>= (point) once)) (setq done t)))
	(error (setq end nil)))
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
(defvar ilisp-last-message nil)
(defvar ilisp-last-prompt nil)
(defun lisp-display-output (output)
  "Display OUTPUT in a popper window unless lisp-no-popper is T."
  (if output
      (progn
	(if (ilisp-value 'comint-errorp t)
	    (setq output (funcall (ilisp-value 'ilisp-error-filter) output)))
	(if lisp-no-popper
	    (let ((buffer (current-buffer))
		  (window (selected-window)))
	      (unwind-protect
		   (progn
		     (lisp-pop-to-buffer (ilisp-buffer))
		     (if (not (eq (current-buffer) buffer))
			 (setq ilisp-last-buffer buffer))
		     (comint-insert 
		      (concat (if ilisp-last-message
				  (concat ";;; " ilisp-last-message "\n"))
			      (comint-remove-whitespace output)
			      "\n"
			      ilisp-last-prompt))
		     (setq ilisp-last-message nil))
		(if (window-point window)
		    (progn (select-window window)
			   (set-buffer buffer)))))
	    (comint-display-output output)))))

;;;
(defun ilisp-handler (error-p wait-p message output prompt)
  "Given ERROR-P, WAIT-P, MESSAGE, OUTPUT and PROMPT, show the message
and output if there is an error or the output is multiple lines and
let the user decide what to do."
  (if lisp-no-popper
      (progn
	(if message
	    (progn
	      (setq ilisp-last-message message
		    ilisp-last-prompt prompt)
	      (if (not wait-p) (lisp-display-output output))))
	nil)
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
		      (funcall (if (boundp 'temp-buffer-show-function)
				   temp-buffer-show-function
				 temp-buffer-show-hook)
			       buffer)
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
		      (message "Abort pending commands and keep in *Errors*")
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
	  t)))

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
	 (if (and output 
		  (string-match "nil" (car (lisp-last-line output))))
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
				     (lisp-last ilisp-load-files))))
			  (setq ilisp-load-files
				(delq (lisp-last ilisp-load-files)
				      ilisp-load-files))))))))
	       (if error (ilisp-handler error wait message output last))
	       (setq ilisp-load-files (delq file ilisp-load-files)))))))))

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
	    (comint-send 
	     (ilisp-process) binary
	     t nil 'binary nil 
	     (` (lambda (error wait message output last)
		  (if (or error
			  (not (string-match "\"[^\"]*\"" output)))
		      (progn
			(lisp-display-output output)
			(abort-commands-lisp "No binary"))
		      (setq (, var)
			    (substring output
				       (1+ (match-beginning 0))
				       (1- (match-end 0))))))))))))

;;;
(defun ilisp-done-init ()
  "Make sure that initialization is done and if not dispatch another check."
  (if ilisp-load-files
      (comint-send-code (get-buffer-process (current-buffer))
			'ilisp-done-init)
      (if ilisp-initializing
	  (progn
	    (message "Finished initializing %s" (car ilisp-dialect))
	    (setq ilisp-initializing nil
		  ilisp-initialized
		  (cons (buffer-name (current-buffer)) ilisp-initialized))))))

;;;
(defun ilisp-init-internal (&optional sync)
  "Send all of the stuff necessary to initialize."
  (unwind-protect
       (progn
	 (if sync
	     (comint-sync
	      (ilisp-process)
	      "\"Start sync\""  "[ \t\n]*\"Start sync\""
	      "\"End sync\""    "\"End sync\""))
	 (ilisp-binary 'ilisp-binary-command 'ilisp-binary-extension)
	 (ilisp-binary 'ilisp-init-binary-command 'ilisp-init-binary-extension)
	 ;; This gets executed in the process buffer
	 (comint-send-code
	  (ilisp-process)
	  (function (lambda ()
	    (let ((files ilisp-load-inits)
		  (done nil))
	      (unwind-protect
		   (progn
		     (if (not ilisp-init-binary-extension)
			 (setq ilisp-init-binary-extension 
			       ilisp-binary-extension))
		     (while files
		       (ilisp-load-or-send
			(expand-file-name 
			 (cdr (car files)) ilisp-directory))
		       (setq files (cdr files)))
		     (comint-send-code (ilisp-process)
				       'ilisp-done-init)
		     (setq done t))
		(if (not done)
		    (progn
		      (setq ilisp-initializing nil)
		      (abort-commands-lisp))))))))
	 (set-ilisp-value 'ilisp-initializing t))
    (if (not (ilisp-value 'ilisp-initializing t))
	(abort-commands-lisp))))

;;;
(defun ilisp-init (&optional waitp forcep sync)
  "Initialize the current inferior LISP if necessary by loading the
files in ilisp-load-inits.  Optional WAITP waits for initialization to
finish.  When called interactively, force reinitialization.  With a
prefix, get the binary extensions again."  
  (interactive 
   (list (if current-prefix-arg
	     (progn
	       (set-ilisp-value 'ilisp-init-binary-extension nil)
	       (set-ilisp-value 'ilisp-binary-extension nil)
	       nil))
	 t))
  (if (or forcep (not (ilisp-initialized)))
      (progn
	(message "Started initializing ILISP")
	(if (not ilisp-directory)
	    (setq ilisp-directory (or (ilisp-directory "ilisp.elc" load-path)
				      (ilisp-directory "ilisp.el" load-path))))
	(if (not (ilisp-value 'ilisp-initializing t))
	    (ilisp-init-internal sync))
	(if waitp
	    (while (ilisp-value 'ilisp-initializing t)
	      (accept-process-output)
	      (sit-for 0))))))

;;;
(defun ilisp-init-and-sync ()
  "Synchronize with the inferior LISP and then initialize."
  (ilisp-init nil nil t))

;;;
(defun ilisp-send (string &optional message status and-go handler)
  "Send STRING to the ILISP buffer, print MESSAGE set STATUS and
return the result if AND-GO is NIL, otherwise switch to ilisp if
and-go is T and show message and results.  If AND-GO is 'dispatch,
then the command will be executed without waiting for results.  If
AND-GO is 'call, then a call will be generated. If this is the first
time an ilisp command has been executed, the lisp will also be
initialized from the files in ilisp-load-inits.  If there is an error,
comint-errorp will be T and it will be handled by HANDLER."
  (ilisp-init t)
  (let ((process (ilisp-process))
	(dispatch (eq and-go 'dispatch)))
    (if message
	(message "%s" (if dispatch
			  (concat "Started " message)
			  message)))
    ;; No completion table
    (setq ilisp-original nil)
    (if (memq and-go '(t call))
	(progn (comint-send process string nil nil status message handler)
	       (if (eq and-go 'call)
		   (call-defun-lisp nil)
		   (switch-to-lisp t t))
	       nil)
	(let* ((save (ilisp-value 'ilisp-save-command t))
	       (result
		(comint-send 
		 process
		 (if save (format save string) string)
		 ;; Interrupt without waiting
		 t (if (not dispatch) 'wait) status message handler)))
	  (if save 
	      (comint-send
	       process
	       (ilisp-value 'ilisp-restore-command t)
	       t nil 'restore "Restore" t t))
	  (if (not dispatch)
	      (progn
		(while (not (cdr result))
		  (sit-for 0)
		  (accept-process-output))
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
	      (let ((input-ring (get-input-ring)))
		(if (>= (point) pmark)
		    (goto-char (point-max))
		    (goto-char pmark)
		    (insert input))
		(if (not ilisp-no-newline) (insert ?\n))
		(if (and (funcall comint-input-filter input)
			 (or (ring-empty-p input-ring)
			     (not (string= (ring-ref input-ring 0) input))))
		    (ring-insert-new input-ring input))
		(funcall comint-input-sentinel input)
		;; Nuke symbol table
		(setq ilisp-original nil)
		(funcall comint-input-sender proc input)
		(set-marker (process-mark proc) (point))
		(set-marker comint-last-input-end (point))
		(goto-char (point-max)))
	      (if (= pmark (point-max)) 
		  (let ((comint-send-newline t))
		    (if (not ilisp-no-newline) (insert ?\n))
		    (set-marker (process-mark proc) (point))
		    (funcall comint-input-sender proc ""))
		  (insert ?\n)
		  (save-restriction
		    (narrow-to-region pmark (point-max))
		    (funcall indent-line-function))))))))

;;;
(defun close-and-send-lisp ()
  "Close and indent the current sexp then send it to the inferior
LISP." 
  (interactive)
  (reindent-lisp)
  (if (memq major-mode ilisp-modes)
      (return-ilisp)
      (eval-defun-lisp)))

;;;%%Keyboard mode
(defun raw-keys-ilisp ()
  "Start using raw keyboard mode to send each character typed to the
inferior LISP until a key bound to interactive-keys-ilisp is
encountered.  See also io-bridge-ilisp." 
  (interactive)
  (if (not ilisp-raw-map)
      (let ((map (make-keymap)))
	(if (vectorp map)
	    (fillarray map 'ilisp-send-char)
	  ;; Lucid GNU Emacs keymaps
	  (let ((i 0))
	    (while (< i 128)
	      (define-key map (make-string 1 i) 'ilisp-send-char)
	      (setq i (1+ i)))))
	(define-key map "\C-g" 'interactive-keys-ilisp)
	(setq ilisp-raw-map map)))
  (use-local-map ilisp-raw-map)
  (message ilisp-raw-message))

;;;
(defun interactive-keys-ilisp ()
  "Go back to interactive keyboard interactions in the inferior LISP."
  (interactive)
  (use-local-map ilisp-use-map)
  (message "Interactive keyboard mode"))

;;;
(defun ilisp-send-char ()
  "Send the last typed character to the current inferior LISP echoing
if ilisp-raw-echo is T."
  (interactive)
  (if (ilisp-value 'ilisp-raw-echo t)
      (progn
	(goto-char (point-max))
	(insert last-input-char)
	(set-marker (process-mark (ilisp-process)) (point))
	(set-marker comint-last-input-end (point))))
  (process-send-string (ilisp-process) 
		       (make-string 1 last-input-char))
  (message ilisp-raw-message))

;;;
(defun ilisp-raw-handler (process output)
  "Turn on raw keyboard mode."
  (raw-keys-ilisp))
(defun ilisp-interactive-handler (process output)
  "Turn on interactive keyboard mode."
  (interactive-keys-ilisp))

;;;
(defun io-bridge-ilisp ()
  "Set up so that the inferior LISP can turn on EMACS raw mode by
sending ^[1^] and turn it off by sending ^[0^]."
  (interactive)
  (require 'bridge)
  (install-bridge)
  (setq bridge-handlers (cons '("1" . ilisp-raw-handler)
			      (cons '("0" . ilisp-interactive-handler)
				    bridge-handlers))))

;;;%%Debugger interface
(defun delete-char-or-pop-ilisp (arg &optional killflag)
  "Delete ARG characters, or pop break level if at end of buffer.  
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
Interactively, ARG is the prefix arg, and KILLFLAG is set if
ARG was explicitly specified."
  (interactive "p")
  (if (eobp)
      (progn
	(message "Pop LISP one level")
	(comint-simple-send (ilisp-process) (ilisp-value 'comint-fix-error)))
      (call-interactively 'delete-char (list arg killflag))))

;;;
(defun reset-ilisp ()
  "Reset the inferior LISP top level."
  (interactive)
  (message "Reset LISP to top level")
  (comint-simple-send (ilisp-process) (ilisp-value 'ilisp-reset)))

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
	       (concat "Complete " 
		       (if function-p "function ")
		       (lisp-buffer-symbol symbol)))
	   'complete)))
    (if (ilisp-value 'comint-errorp t)
	(progn (lisp-display-output choices)
	       (error "Error completing %s" (lisp-buffer-symbol symbol)))
	(setq choices (read choices)
	      choices (if (eq choices 'NIL) nil choices)))
    (setq ilisp-original symbol
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
		(ilisp-completion-table symbol ilisp-completion-function-p)))
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
if FUNCTION-P is T.  Return (SYMBOL LCS-SYMBOL CHOICES UNIQUEP)."
  (let* ((name (lisp-symbol-name symbol))
	 (table (ilisp-completion-table symbol function-p))
	 (choice (and table (try-completion name table))))
    (cond ((eq choice t)		;Name is it
	   (list symbol symbol nil t))
	  ((string= name choice)	;Name is LCS
	   (list symbol symbol (all-completions name table) nil))
	  (choice			;New LCS
	   (let ((symbol
		  (lisp-symbol (lisp-symbol-package symbol) 
			       (lisp-symbol-delimiter symbol)
			       choice)))
	     (list symbol symbol (all-completions choice table) nil)))
	  ((and (not ilisp-prefix-match) table)	;Try partial matches
	   (let ((matches
		  (completer name table nil (regexp-quote completer-words))))
	     (cons (lisp-symbol (lisp-symbol-package symbol)
				(lisp-symbol-delimiter symbol)
				(car matches))
		   (cons  (lisp-symbol (lisp-symbol-package symbol)
				(lisp-symbol-delimiter symbol)
				(car (cdr matches)))
			  (cdr (cdr matches)))))))))

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
	     ""
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
	  ((string= pkg "") (concat ":" symbol-name))
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
	(let* ((delimiters (ilisp-value 'ilisp-symbol-delimiters))
	       (end (progn
		      (if (not stay) (skip-chars-forward delimiters))
		      (point)))
	       (start (progn
			(skip-chars-backward delimiters)
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
(defun lisp-def-name (&optional namep)
  "Return the name of a definition assuming that you are at the start
of the sexp.  If the form starts with DEF, the form start and the next
symbol will be returned.  Optional NAMEP will return only the name without the defining symbol."
  (let ((case-fold-search t))
    (if (looking-at
	 ;; (( \( (def*) (( \( (setf)) | \(?)) | \(?) (symbol)
	 ;; 12    3    3 45    6    65      42      1 7      7
	 ;;0011\(22 def*        22         32 43\(54 setf54         43   \(?32 11      00 60           60
	 "\\(\\((\\(def[^ \t\n]*\\)[ \t\n]+\\(\\((\\(setf\\)[ \t\n]+\\)\\|(?\\)\\)\\|(?\\)\\([^ \t\n)]*\\)")
	(let ((symbol (buffer-substring (match-beginning 7) (match-end 7))))
	  (if (match-end 6)
	      (concat (if (not namep) 
			  (concat 
			   (buffer-substring (match-beginning 3) (match-end 3))
			   " "))
		      "("
		      (buffer-substring (match-beginning 6) (match-end 6))
		      " " symbol ")")
	      (if (match-end 3)
		  (concat (if (not namep)
			      (concat 
			       (buffer-substring (match-beginning 3) 
						 (match-end 3))
			       " "))
			  symbol)
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
	(if (string-match "Lucid" emacs-version)
	    ;; not necessary, but friendlier.
	    (set-keymap-parent ilisp-completion-map lisp-mode-map)
	  (setq ilisp-completion-map (copy-keymap lisp-mode-map)))
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

;;;
(defun lisp-read-program (prompt &optional initial)
  "Read a program with PROMPT and INITIAL.  TAB or Esc-TAB will complete
filenames."
  (if (null lisp-program-map)
      (progn 
	(setq lisp-program-map (copy-keymap minibuffer-local-map))
	(define-key lisp-program-map "\M-\t" 'comint-dynamic-complete)
	(define-key lisp-program-map "\t" 'comint-dynamic-complete)
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
    (lisp-string-to-symbol (lisp-def-name t))))

;;;
(defun lisp-region-name (start end)
  "Return a name for the region from START to END."
  (save-excursion
    (goto-char start)
    (if (re-search-forward "^[ \t]*[^;\n]" end t)
	(forward-char -1))
    (setq start (point))
    (goto-char end)
    (re-search-backward "^[ \t]*[^;\n]" start 'move)
    (end-of-line)
    (skip-chars-backward " \t")
    (setq end (min (point) end))
    (goto-char start)
    (let ((from
	   (if (= (char-after (point)) ?\()
	       (lisp-def-name)
	       (buffer-substring (point) 
				 (progn (forward-sexp) (point))))))
      (goto-char end)
      (if (= (char-after (1- (point))) ?\))
	  (progn
	    (backward-sexp)
	    (if (= (point) start)
		from
		(concat "from " from " to " (lisp-def-name))))
	  (concat "from " from " to " 
		  (buffer-substring (save-excursion
				      (backward-sexp)
				      (point)) 
				    (1- (point))))))))

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
    (if (memq major-mode ilisp-modes)
	(beginning-of-defun-lisp t)
	(if (or (lisp-in-string)
		(progn (beginning-of-line)
		       (re-search-forward "^[ \t\n]*[^; \t\n]" nil t)
		       (back-to-indentation)
		       (not (bolp))))
	    (beginning-of-defun-lisp t)))
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
(defun newline-and-indent-lisp ()
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

;;;%%Call
(defun match-ring (ring regexp start)
  "Return the index in RING of REGEXP starting at START."
  (let ((n 0)
	(len (ring-length ring)))
    (while (and (< n len) 
		(not (string-match regexp (ring-ref ring n))))
      (setq n (1+ n)))
    (if (= n len)
	nil
	n)))

;;;
(defun lisp-match-ring (regexp string &optional no-insert)
  "Match REGEXP in the input-ring of the current buffer and set the
ring variables to look like comint-previous-similar-input if found.
If not found insert STRING, unless NO-INSERT."
  (let ((n (if regexp (match-ring (get-input-ring) regexp 0))))
    (if n
	(let ((point (progn (comint-kill-input) (point))))
	  (insert (ring-ref (get-input-ring) n))
	  (save-excursion
	    (goto-char (+ point (length string)))
	    (skip-chars-forward "^ \t\n\)")
	    (setq point (point)))
	  (push-mark point)
	  (setq this-command 'comint-previous-similar-input
		input-ring-index n
		comint-last-similar-string string)
	  t)
	(if (and string (not no-insert))
	    (progn (comint-kill-input) (insert string) t)
	    nil))))

;;;
(defun call-defun-lisp (arg)
  "Put a call of the current defun in the inferior LISP and go there.
If it is a \(def* name form, look up reasonable forms of name in the
input history unless called with prefix ARG. If not found, use \(name
or *name* as the call.  If is not a def* form, put the whole form in
the buffer."
  (interactive "P")
  (if (save-excursion (lisp-defun-begin) (looking-at "(def"))
      (let* ((symbol (lisp-defun-name))
	     (name (lisp-symbol-name symbol))
	     (package (if (lisp-symbol-package symbol)
			  (concat "\\("
				  (lisp-symbol-package symbol) ":+\\)?")))
	     (variablep (string-match "^\\*" name))
	     (setfp (string-match "(setf \\([^\)]+\\)" name)))
	(switch-to-lisp t t)
	(cond (setfp 
	       (setq name (substring name (match-beginning 1) (match-end 1)))
	       (lisp-match-ring (if (not arg)
				    (concat "(setf[ \t\n]*(" 
					    package name "[ \t\n]"))
				(concat "(setf (" name)))
	      (variablep (lisp-match-ring (if (not arg) 
					      (concat package name))
					  name))
	      (t
	       (let ((fun (concat "(" name)))
		 (setq name (regexp-quote name))
		 (or (lisp-match-ring 
		      (if (not arg) (concat "(" package name "[ \t\n\)]"))
		      fun 
		      (not arg))
		     (lisp-match-ring (concat "(" package
					      "[^ \t\n]*-*" name)
				      fun))))))
      (let ((form 
	     (save-excursion
	       (buffer-substring (lisp-defun-begin) (lisp-end-defun-text t)))))
	(switch-to-lisp t t)
	(comint-kill-input)
	(insert form))))

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
    (lisp-display-output result)))

;;;
(defun inspect-lisp (sexp)
  "Inspect the current sexp using ilisp-inspect-command.  With a
prefix, prompt for the expression.  If in an ILISP buffer, and there
is no current sexp, inspect ilisp-last-command."
  (interactive
   (list
    (if current-prefix-arg
	(ilisp-read "Inspect: " (lisp-previous-sexp t))
	(if (memq major-mode ilisp-modes)
	    (if (= (point)
		   (process-mark (get-buffer-process (current-buffer))))
		(or (ilisp-value 'ilisp-last-command t)
		    (error "No sexp to inspect."))
		(lisp-previous-sexp t))
	    (lisp-previous-sexp t)))))
  (ilisp-send
   (format (ilisp-value 'ilisp-inspect-command) 
	   (lisp-slashify sexp) (lisp-buffer-package))
   (concat "Inspect " sexp)
   'inspect t))

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
	      (t (lisp-display-output 
		  (if position
		      (substring arglist position)
		      arglist)))))))

;;;
(defun documentation-lisp (symbol type)
  "Return the documentation of the previous symbol using
ilisp-documentation-command.  If the symbol is at the start of a list,
it is assumed to be a function, otherwise variable documentation is
searched for.  With a minus prefix, prompt for the symbol and type.
With a numeric prefix always return the current function call
documentation."
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
       (if current-prefix-arg
	   (list (lisp-function-name) 'function)
	   (let* ((symbol-info (lisp-previous-symbol)))
	     (list (car symbol-info)
		   (if (car (cdr symbol-info))
		       'function
		       'variable))))))
  (lisp-display-output
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
	    (lisp-display-output result)))
      (error "Not a form: %s" form)))

(defun macroexpand-1-lisp (form)
  "Macroexpand the next sexp once.  With a prefix, insert into buffer."
  (interactive (lisp-macroexpand-form))
  (macroexpand-lisp form t))

;;;%%complete-lisp
(autoload 'complete "completion" "Complete previous symbol." t)
(defun complete-lisp (mode)
  "Complete the current symbol using information from the current
ILISP buffer.  If in a string, complete as a filename.  If called with
a positive prefix force all symbols to be considered.  If called with
a negative prefix, undo the last completion.  Partial completion is
allowed unless ilisp-prefix-match is T.  If a symbol starts after a
left paren or #', then only function symbols will be considered.
Package specifications are also allowed and the distinction between
internal and exported symbols is considered."
  (interactive "P")
  (if (< (prefix-numeric-value mode) 0)
      (completer-undo)
      (let* ((filep
	      (save-excursion
		(skip-chars-backward "^ \t\n")
		(= (char-after (point)) ?\"))))
	(if filep
	    (comint-dynamic-complete)
	    (let* ((symbol-info (lisp-previous-symbol))
		   (symbol (car symbol-info))
		   (name (lisp-symbol-name symbol))
		   (choice (ilisp-completer 
			    symbol 
			    (if (not mode) (car (cdr symbol-info)))))
		   (match (lisp-buffer-symbol (car choice)))
		   (lcs (lisp-buffer-symbol (car (cdr choice))))
		   (choices (car (cdr (cdr choice))))
		   (unique (car (cdr (cdr (cdr choice))))))
	      (skip-chars-backward " \t\n")
	      (completer-goto match lcs choices unique 
			      (ilisp-value 'ilisp-symbol-delimiters)
			      completer-words)))
	(message "Completed"))))

;;;%%Trace
(defun trace-defun-lisp (function)
  "Trace FUNCTION without arg, untrace with.  Prompt for function with
negative prefix.  Default function is the current defun."
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
      (ilisp-send
       (format (if current-prefix-arg 
		   (ilisp-value 'ilisp-untrace-command)
		   (ilisp-value 'ilisp-trace-command))
	       (lisp-symbol-name function)
	       (lisp-symbol-package function))
       (format "%srace %s" (if current-prefix-arg "Unt" "T") 
	       (lisp-buffer-symbol function))
       (if current-prefix-arg 'untrace 'trace)
       (if lisp-wait-p nil 'dispatch))
      (error "No function to %strace" (if current-prefix-arg "un" ""))))

;;;%%Default-directory
(defun default-directory-lisp (&optional buffer)
  "Set the inferior LISP default directory to the default directory of
optional BUFFER.  If you are in an inferior LISP buffer, set the
default directory to the current directory of the LISP."
  (interactive)
  (if (and (not buffer) (memq major-mode ilisp-modes))
      (let ((dir
	     (ilisp-send
	      (ilisp-value 'ilisp-directory-command)
	      (format "Getting LISP directory")
	      'dir)))
	(if (ilisp-value 'comint-errorp t)
	    (progn
	      (lisp-display-output dir)
	      (error "Error getting directory"))
	    (setq default-directory (read dir)
		  lisp-prev-l/c-dir/file (cons default-directory nil))
	    (message "Default directory is %s" default-directory)))
      (let ((directory (save-excursion
			 (set-buffer (or buffer (current-buffer)))
			 default-directory)))
	(ilisp-send 
	 (format (ilisp-value 'ilisp-set-directory-command) directory)
	 (format "Set %s's directory to %s" 
		 (buffer-name (ilisp-buffer)) directory)
	 'dir
	 (if lisp-wait-p nil 'dispatch)))))
  
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
(autoload 'edit-callers-lisp "ilisp-src" 
	  "Edit the callers of a function." t)

;;;%Eval/compile
(defun lisp-send-region (start end switch message status format
			       &optional handler)
  "Given START, END, SWITCH, MESSAGE, STATUS, FORMAT and optional
HANDLER send the region between START and END to the lisp buffer and
execute the command defined by FORMAT on the region, its package and
filename.  If called with a positive prefix, the results will be
inserted at the end of the region.  If SWITCH is T, the command will
be sent and the buffer switched to the inferior LISP buffer.  if
SWITCH is 'call, a call will be inserted.  If SWITCH is 'result the
result will be returned without being displayed.  Otherwise the
results will be displayed in a popup window if lisp-wait-p is T and
the current-prefix-arg is not '- or if lisp-wait-p is nil and the
current-prefix-arg is '-.  If not displayed in a pop-up window then
comint-handler will display the results in a pop-up window if they are
more than one line long, or they are from an error.  STATUS will be
the process status when the command is actually executing.  MESSAGE is
a message to let the user know what is going on."
  (if (= start end) (error "Region is empty"))
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
	    (cond ((memq switch '(t call)) switch)
		  ((or (not (eq lisp-wait-p (lisp-minus-prefix))) 
		       current-prefix-arg
		       (eq switch 'result)) nil)
		  (t 'dispatch))
	    handler)))
      (if result
	  (if current-prefix-arg
	      (save-excursion
		(goto-char end)
		(insert ?\n)
		(insert result))
	      (if (or (ilisp-value 'comint-errorp t)
		      (string-match "\n" result))
		  (lisp-display-output result)
		  (popper-bury-output t)
		  (message "%s" result)))
	  result))))

;;;%%Eval
(defun eval-region-lisp (start end &optional switch message status handler)
  "Evaluate the current region."
  (interactive "r")
  (setq message (or message 
		    (concat "Evaluate " (lisp-region-name start end))))
  (let ((defvar (ilisp-value 'ilisp-defvar-regexp t)))
    (if (and defvar
	     (save-excursion
	       (goto-char start)
	       (skip-chars-forward " \t\n")
	       (and (let ((case-fold-search t)) (looking-at defvar))
		    (progn (forward-sexp) (skip-chars-forward " \t\n" end)
			   (= (point) end)))))
	(lisp-send-region start end switch message (or status 'defvar)
			  'ilisp-defvar-command handler)
	(lisp-send-region start end switch message (or status 'eval)
			  'ilisp-eval-command handler))))

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

(defun eval-next-sexp-and-go-lisp (&optional switch)
  "Evaluate the next sexp and switch to the current ILISP buffer."
  (interactive)
  (eval-next-sexp-lisp t))

(defun eval-defun-and-go-lisp ()
  "Evaluate the current defun and switch to the current ILISP buffer.
With prefix, insert a call as well."
  (interactive)
  (eval-defun-lisp (if current-prefix-arg 
		       (progn
			 (setq current-prefix-arg nil)
			 'call)
		       t)))

;;;%%Compile
(defun compile-region-lisp (start end &optional switch message status handler)
  "Compile the current region."
  (interactive "r")
  (lisp-send-region
   start end switch 
   (or message (concat "Compile " (lisp-region-name start end)))
   (or status 'compile)
   'ilisp-compile-command 
   handler))
		
;;;
(defun compile-defun-lisp (&optional switch)
  "Compile the current defun or the last command in the input-ring of
an ILISP buffer if no current defun."
  (interactive)
  (let* ((form (lisp-defun-region-and-name))
	 (start (car form))
	 (end (car (cdr form))))
    (if (and (= start end) (memq major-mode ilisp-modes))
	(save-excursion
	  (let ((form (ring-ref (get-input-ring) input-ring-index)))
	    (set-buffer "*ilisp-send*")
	    (delete-region (point-min) (point-max))
	    (insert form)
	    (compile-defun-lisp)))
	(compile-region-lisp start end switch
			     (format "Compile %s" (car (cdr (cdr form))))))))

;;;%%%And-go
(defun compile-region-and-go-lisp (start end)
  "Compile the current region and switch to the current ILISP buffer."
  (interactive "r")
  (compile-region-lisp start end t))

(defun compile-defun-and-go-lisp ()
  "Compile the current defun and switch to the current ILISP buffer."
  (interactive)
  (compile-defun-lisp 
   (if current-prefix-arg
       (progn
	 (setq current-prefix-arg nil)
	 'call)
       t)))

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
      (lisp-pop-to-buffer (find-file-noselect file))
      (find-file file)))

;;;
(defun find-file-lisp (file-name)
  "Find a file.  If point is on a string that points to an existing
file, that will be the default.  If the buffer is one of
lisp-source-modes, the buffer file will be the default.  Otherwise,
the last file used in a lisp-source-mode will be used."
  (interactive
   (comint-get-source "Find file: " lisp-prev-l/c-dir/file
		      lisp-source-modes nil))
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (lisp-find-file file-name nil t))

;;;
(defun load-file-lisp (file-name)
  "Load a lisp file into the current inferior LISP and go there."
  (interactive (comint-get-source "Load Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil))
  (comint-check-source file-name)	; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (ilisp-init t)
  (let* ((extension (ilisp-value 'ilisp-binary-extension t))
	 (binary (lisp-file-extension file-name extension)))
    (save-excursion
      (set-buffer (ilisp-buffer))
      (if (not (eq comint-send-queue comint-end-queue))
	  (if (y-or-n-p "Abort commands before loading? ")
	      (abort-commands-lisp)
	      (message "Waiting for commands to finish")
	      (while (not (eq comint-send-queue comint-end-queue))
		(accept-process-output)
		(sit-for 0))))
      (if (and (car (comint-send-variables (car comint-send-queue)))
	       (y-or-n-p "Interrupt top level? "))
	  (let ((result (comint-send-results (car comint-send-queue))))
	    (interrupt-subjob-ilisp)
	    (while (not (cdr result))
	      (accept-process-output)
	      (sit-for 0)))))
    (if (file-newer-than-file-p file-name binary)
	(if (and (not ilisp-load-no-compile-query)
		 extension (y-or-n-p "Compile first? "))
	    ;; Load binary if just compiled
	    (progn
	      (message "")
	      (compile-file-lisp file-name)
	      (setq file-name binary)))
	;; Load binary if it is current
	(if (file-readable-p binary) (setq file-name binary)))
    (switch-to-lisp t t)
    (comint-sender
     (ilisp-process)
     (format (ilisp-value 'ilisp-load-command) file-name))
    (message "Loading %s" file-name)))

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
(defun ilisp-start-dialect (buffer program setup)
  ;; Allow dialects to be started from command line
  (if (eq current-prefix-arg 0) (setq current-prefix-arg nil))
  (setq ilisp-last-buffer (current-buffer)
	buffer (if current-prefix-arg
		   (read-from-minibuffer "Buffer: " buffer)
		   buffer))
  (funcall setup buffer)
  (setq ilisp-program
	(or program 
	    (if current-prefix-arg
		(lisp-read-program "Program: " ilisp-program)
		ilisp-program)))
  (ilisp buffer))

;;;
(defmacro defdialect (dialect full-name parent &rest body)
  "Define a new ILISP dialect.  DIALECT is the name of the function to
invoke the inferior LISP. The hook for that LISP will be called
DIALECT-hook.  The default program will be DIALECT-program.  FULL-NAME
is a string that describes the inferior LISP.  PARENT is the name of
the parent dialect."
  (let ((setup (read (format "setup-%s" dialect)))
	(hook (read (format "%s-hook" dialect)))
	(program (read (format "%s-program" dialect)))
	(dialects (format "%s" dialect)))
    (`
     (progn
       (defvar (, hook) nil (, (format "*Inferior %s hook." full-name)))
       (defvar (, program) nil
	 (, (format "*Inferior %s default program." full-name)))
       (defun (, setup) (buffer)
	 (, (format "Set up for interacting with %s." full-name))
	 (, (read (format "(setup-%s buffer)" parent)))
	 (,@ body)
	 (setq ilisp-program (or (, program) ilisp-program)
	       ilisp-dialect (cons '(, dialect) ilisp-dialect))
	 (run-hooks '(, (read (format "%s-hook" dialect)))))
       (defun (, dialect) (&optional buffer program)
	 (, (format "Create an inferior %s.  With prefix, prompt for buffer and program."
		   full-name))
	 (interactive (list nil nil))
	 (ilisp-start-dialect (or buffer (, dialects)) 
			      program 
			      '(, setup))
	 (setq (, program) ilisp-program))
       (lisp-add-dialect (, dialects))))))

;;;%%ilisp
(defun setup-ilisp (buffer)
  "Set up for interacting with an inferior LISP."
  (set-buffer (get-buffer-create "*ilisp-send*"))
  (kill-all-local-variables)
  (lisp-mode)
  (setq ilisp-buffer (format "*%s*" buffer))
  (set-buffer (get-buffer-create ilisp-buffer))
  (setq major-mode 'ilisp-mode
	mode-name "ILISP")
  (lisp-mode-variables t)
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
;;  (or input-ring (set-input-ring (make-ring input-ring-size)))
  ;; Comint-ipc defaults
  (setq comint-send-newline t
	comint-always-scroll nil
	comint-output-buffer " *Output*"
	comint-error-buffer " *Error Output*"
	comint-error-regexp "^\"ILISP:"
	comint-output-filter (function identity)
	comint-interrupt-start 'comint-interrupt-start
	comint-handler 'ilisp-handler
	comint-update-status 'ilisp-update-status
	comint-prompt-status 'comint-prompt-status
	comint-abort-hook 'ilisp-abort-handler)
  (setq ilisp-use-map ilisp-mode-map
	ilisp-init-hook '((lambda () (ilisp-init nil nil t)))
	ilisp-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)*\\)?\\s *\\'"
	ilisp-filter-length 3
	ilisp-error-filter 'ilisp-error-filter
	ilisp-error-regexp ".*" 
	ilisp-symbol-delimiters "^ \t\n\('\"#.\)<>"
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
	"(or (and (load \"%s\" :if-does-not-exist nil) t)
             (and (load \"%s\" :if-does-not-exist nil) t))")
  (ilisp-load-init 'clisp "clisp.lisp")
  (setq ilisp-package-regexp "^[ \t]*(in-package[ \t\n]*"
	ilisp-package-command "(let ((*package* *package*)) %s (package-name *package*))"
	ilisp-package-name-command "(package-name *package*)"
	ilisp-in-package-command "(in-package \"%s\")"
	ilisp-last-command "*"
	ilisp-save-command "(progn (ILISP:ilisp-save) %s\n)"
	ilisp-restore-command "(ILISP:ilisp-restore)"
	ilisp-block-command "(progn %s\n)"
	ilisp-eval-command "(ILISP:ilisp-eval \"%s\" \"%s\" \"%s\")"
	ilisp-defvar-regexp "(defvar[ \t\n]")
  (setq ilisp-defvar-command 
	"(ILISP:ilisp-eval \"(let ((form '%s)) (progn (makunbound (second form)) (eval form)))\" \"%s\" \"%s\")")
  (setq ilisp-compile-command "(ILISP:ilisp-compile \"%s\" \"%s\" \"%s\")"
	ilisp-describe-command "(ILISP:ilisp-describe \"%s\" \"%s\")"
	ilisp-inspect-command "(ILISP:ilisp-inspect \"%s\" \"%s\")"
	ilisp-arglist-command "(ILISP:ilisp-arglist \"%s\" \"%s\")")
  (setq ilisp-documentation-types
	'(("function") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))
  (setq ilisp-documentation-command
	"(ILISP:ilisp-documentation \"%s\" \"%s\" \"%s\")")
  (setq ilisp-macroexpand-1-command 
	"(ILISP:ilisp-macroexpand-1 \"%s\" \"%s\")")
  (setq ilisp-macroexpand-command "(ILISP:ilisp-macroexpand \"%s\" \"%s\")")
  (setq ilisp-complete-command 
	"(ILISP:ilisp-matching-symbols \"%s\" \"%s\" %s %s %s)")
  (setq ilisp-locator 'lisp-locate-clisp)
  (setq ilisp-source-types 
	'(("function") ("macro") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))
  (setq ilisp-callers-command "(ILISP:ilisp-callers \"%s\" \"%s\")"
	ilisp-trace-command "(ILISP:ilisp-trace \"%s\" \"%s\")"
	ilisp-untrace-command "(ILISP:ilisp-untrace \"%s\" \"%s\")")
  (setq ilisp-directory-command "(namestring *default-pathname-defaults*)"
	ilisp-set-directory-command
	"(setq *default-pathname-defaults* (parse-namestring \"%s\"))")
  (setq ilisp-load-command "(load \"%s\")")
  (setq ilisp-compile-file-command 
	"(ILISP:ilisp-compile-file \"%s\" \"%s\")"))

;;;%%%Allegro
(defun allegro-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((old-level (if (and old (eq 1 (string-match "[0-9]+" old)))
 			(string-to-int (substring old 1))
 			0))
 	 (new-level (if (eq 1 (string-match "[0-9]+" new))
 			(string-to-int (substring new 1))
 			0)))
    (<= new-level old-level)))
 
;;;
(defdialect allegro "Allegro Common LISP"
  clisp
  (ilisp-load-init 'allegro "allegro.lisp")
  (setq comint-fix-error ":pop"
	ilisp-reset ":reset"
	comint-continue ":cont"
	comint-interrupt-regexp  "Error: [^\n]* interrupt\)")
  (setq comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'allegro-check-prompt))))
  ;; <cl> or package> at top-level
  ;; [0-9c] <cl> or package> in error
  (setq comint-prompt-regexp "^\\(\\[[0-9]*c*\\] \\|\\)\\(<\\|\\)[^>]*> ")
  (setq ilisp-error-regexp
	"\\(ILISP:[^\"]*\\)\\|\\(Error:[^\n]*\\)\\|\\(Break:[^\n]*\\)")
  (setq ilisp-binary-command "excl:*fasl-default-type*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-init-binary-command 
	"(let ((ext (or #+m68k \"68fasl\"
		        #+sparc \"sfasl\"
		        #+iris4d \"ifasl\"
                        #+dec3100 \"pfasl\"
                        excl:*fasl-default-type*)))
           #+allegro-v4.0 (setq ext (concatenate 'string ext \"4\"))
           ext)"))
(if (not allegro-program) (setq allegro-program "cl"))

;;;%%%Lucid
(defun lucid-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((old-level (if (and old (eq 0 (string-match "\\(->\\)+" old)))
 			(- (match-end 0) (match-beginning 0))
 			0))
	 (new-level (if (eq 0 (string-match "\\(->\\)+" new))
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect lucid "Lucid Common LISP"
  clisp
  (ilisp-load-init 'lucid "lucid.lisp")
  (setq comint-prompt-regexp "^\\(->\\)+ \\|^[^> ]*> "
	comint-fix-error ":a"
	ilisp-reset ":a :t"
	comint-continue ":c"
	comint-interrupt-regexp ">>Break: Keyboard interrupt"
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'lucid-check-prompt))))
  (setq ilisp-error-regexp "ILISP:[^\"]*\\|>>[^\n]*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-binary-command 
	"(first (last lucid::*load-binary-pathname-types*))"))
(if (not lucid-program) (setq lucid-program "lisp"))

;;;%%%KCL--these dialects by Tom Emerson
;;; kcl-check-prompt doesn't after the first break because the
;;; number of ">" characters doesn't increase.

(defun kcl-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match ">+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match ">+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect kcl "Kyoto Common LISP" clisp
  (setq comint-prompt-regexp "^>+"
        ilisp-error-regexp "Error: "
        ilisp-binary-extension "o"
        comint-fix-error ":q"
        comint-continue ":r"
	comint-prompt-status
	(function
	 (lambda (old line)
	   (comint-prompt-status old line 'kcl-check-prompt)))))
(if (not kcl-program) (setq kcl-program "kcl"))

;;;%%%AKCL
(defdialect akcl "Austin Kyoto Common LISP" kcl)
(if (not akcl-program) (setq akcl-program "akcl"))

;;;%%%IBCL
(defdialect ibcl "Ibuki Common LISP" kcl
  (setq comint-prompt-regexp "^[-A-Z]*>+\\|^[-A-Z]* ->"
        comint-interrupt-regexp ">>Condition: Terminal Interrupt"
        comint-continue ":q"
        ilisp-reset ":q!"
        ilisp-error-regexp ">>Error:"))
(if (not ibcl-program) (setq ibcl-program "ibcl"))

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
  (setq comint-prompt-regexp "^\\([0-9]+\\]+\\|\\*\\) "
	comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'cmulisp-check-prompt)))
	ilisp-error-regexp "ILISP:[^\"]*\\|Error [^\n]*"
	ilisp-arglist-command "(ILISP:arglist \"%s\" \"%s\")"
	ilisp-find-source-command "(ILISP:source-file \"%s\" \"%s\" \"%s\")"
	comint-fix-error ":pop"
	comint-continue ":go"
	ilisp-reset ":q"
	comint-interrupt-regexp "Software Interrupt"
	ilisp-binary-extension "fasl"))

;;;%%Scheme
(defdialect scheme "Scheme" ilisp
  (setq ilisp-block-command "(begin \n%s)")
  (setq ilisp-load-command "(load \"%s\")")
  )
(if (not scheme-program) (setq scheme-program "scheme"))

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
LISP interpreter as a subprocess of Emacs, with LISP I/O through an
Emacs buffer.  If you have problems, use M-x ilisp-bug in the buffer
where you are having a problem to send a bug report.

To start a LISP use M-x run-ilisp, or a specific dialect like M-x
allegro.  If called with a prefix you will be prompted for a buffer
name and a program to run.  The default buffer name is the name of the
dialect.  The default program for a dialect will be the value of
DIALECT-program or the value of ilisp-program inherited from a less
specific dialect.  If there are multiple LISP's, use the dialect name
or select-ilisp \(\\[select-ilisp]) to select the current ILISP
buffer.

Currently supported LISP dialects include:
 clisp
   allegro
   lucid
   kcl
     akcl
     ibcl
   cmulisp
 scheme
   oaklisp

Customization: Starting a dialect runs the hooks on comint-mode-hook
and ilisp-mode-hook and then DIALECT-hooks specific to dialects in the
nesting order above.  On the very first prompt in the inferior LISP,
the hooks on ilisp-init-hook are run.  For more information on
creating a new dialect or variables to set in hooks, see ilisp.el.

Most of these key bindings work in both Lisp Mode and ILISP mode.
There are a few additional and-go bindings found in Lisp Mode.
\\{ilisp-use-map}
There are also a few bindings found in global-map including:
  \\[popper-bury-output] popper-bury-output
  \\[popper-scroll-output] popper-scroll-output
  \\[popper-other-window] popper-other-window
  \\[popper-grow-output] popper-grow-output
  \\[previous-buffer-lisp] previous-buffer-lisp
  \\[switch-to-lisp] switch-to-lisp

ILISP uses a dynamically sized pop-up window that can be buried and
scrolled from any window for displaying output.  See the file
popper.el or the ILISP info node for information on customizing popper
windows.  \(\\[popper-other-window]) skips the popper window.  If
called with a C-u prefix, the popper window will be selected.
popper-bury-output \(\\[popper-bury-output]) buries the output window.
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

An alternative to popper windows is to always have the inferior LISP
buffer visible and have all output go there.  Setting lisp-no-popper
to T will cause all output to go to the inferior LISP buffer.
Setting comint-always-scroll to T will cause process output to always
be visible.  If a command gets an error, you will be left in the break
loop.

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

There are two keyboard modes for interacting with the inferior LISP,
\"interactive\" and \"raw\".  Normally you are in interactive mode
where keys are interpreted as commands to EMACS and nothing is sent to
the inferior LISP unless a specific command does so.  In raw mode, all
characters are passed directly to the inferior LISP without any
interpretation as EMACS commands.  Keys will not be echoed unless
ilisp-raw-echo is T.  Raw mode can be turned on interactively by
raw-keys-ilisp \(\\[raw-keys-ilisp]) and will continue until you type
C-g.  Raw mode can also be turned on/off by inferior LISP functions if
io-bridge-ilisp \(\\[io-bridge-ilisp]) has been executed in the
inferior LISP interactively or on a hook.  To turn on raw mode, a
function should print ^[1^] and to turn it off should print ^[0^].

When you send something to LISP, the status light will reflect the
progress of the command.  If you type top-level forms ahead of the
processing, the status may indicate ready when the LISP is actually
running.  In a lisp mode buffer the light will reflect the status of
the currently selected inferior LISP unless lisp-show-status is nil.
If you want to find out what command is currently running, use the
command status-lisp \(\\[status-lisp]).  If you call it with a prefix,
the pending commands will be displayed as well.

If you are want to abort the last command you can use
\(\\[keyboard-quit]).  If you want to abort all commands, you should
use the command abort-commands-lisp \(\\[abort-commands-lisp]).
Commands that are aborted will be put in the buffer *Aborted Commands*
so that you can see what was aborted.  If you want to abort the
currently running top-level command, use interrupt-subjob-ilisp
\(\\[interrupt-subjob-ilisp]).  As a last resort, \\[panic-lisp] will
reset the ILISP state without affecting the inferior LISP so that you
can see what is happening.

bol-ilisp \(\\[bol-ilisp]) will go after the prompt as defined by
comint-prompt-regexp or ilisp-other-prompt or to the left margin with
a prefix.

return-ilisp \(\\[return-ilisp]) knows about prompts and sexps.  If an
sexp is not complete, it will indent properly.  When an entire sexp is
complete, it is sent to the inferior LISP together with a new line.
If you edit old input, the input will be copied to the end of the
buffer first.

close-and-send-lisp \(\\[close-and-send-lisp]) will close the current
sexp, indent it, then send it to the current inferior LISP.

indent-line-ilisp \(\\[indent-line-ilisp]) indents for LISP.  With
prefix, shifts rest of expression rigidly with the current line.

newline-and-indent-lisp \(\\[newline-and-indent-lisp]) will insert a
new line and then indent to the appropriate level.  If you are at the
end of the inferior LISP buffer and an sexp, the sexp will be sent to
the inferior LISP without a trailing newline.

indent-sexp-ilisp \(\\[indent-sexp-ilisp]) will indent each line in
the next sexp.

backward-delete-char-untabify \(\\[backward-delete-char-untabify])
converts tabs to spaces as it moves back.

delete-char-or-pop-ilisp \(\\[delete-char-or-pop-ilisp]) will delete
prefix characters unless you are at the end of an ILISP buffer in
which case it will pop one level in the break loop.

reset-ilisp, \(\\[reset-ilisp]) will reset the current inferior LISP's
top-level so that it will no longer be in a break loop.

switch-to-lisp \(\\[switch-to-lisp]) will pop to the current ILISP
buffer or if already in an ILISP buffer, it will return to the buffer
that last switched to an ILISP buffer.  With a prefix, it will also go
to the end of the buffer.  If you do not want it to pop, set
pop-up-windows to nil.  

call-defun-lisp \(\\[call-defun-lisp]) will put a call to the current
defun in the inferior LISP and go there.  If it is a \(def* name form,
it looks up reasonable forms of name in the input history unless
called with a prefix. If not found, \(name or *name* will be inserted.
If it is not a def* form, the whole defun will be put in the buffer.

reposition-window-lisp \(\\[reposition-window-lisp]) will scroll the
current window to show as much of the current defun and its
introductory comments as possible without moving the point.  If called
with a prefix, the point will be moved if necessary to show the start
of the defun.  If called more than once with the first line of the
defun showing, the introductory comments will be shown or suppressed.

previous-buffer-lisp \(\\[previous-buffer-lisp]) will switch to the
last visited buffer in the current window or the Nth previous buffer
with a prefix.

find-unbalanced-lisp \(\\[find-unbalanced-lisp]) will find unbalanced
parens in the current buffer.  When called with a prefix it will look
in the current region.

close-all-lisp \(\\[close-all-lisp]) will close all outstanding
parens back to the containing form, or a previous left bracket
which will be converted to a left parens.  If there are too many
parens, they will be deleted unless there is text between the
last paren and the end of the defun.  If called with a prefix,
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
line.  set-buffer-package-lisp \(\\[set-buffer-package-lisp]) will
update the current package from the buffer.  If it is called with a
prefix, the package can be set manually.  If a buffer has no
specification, forms will be evaluated in the current inferior LISP
package.  package-lisp \(\\[package-lisp]) will show the current
package of the inferior LISP.  set-package-lisp
\(\\[set-package-lisp]) will set the inferior LISP package to the
current buffer's package or to a manually entered package with a
prefix.

describe-lisp, inspect-lisp, arglist-lisp, documentation-lisp,
macroexpand-1-lisp, macroexpand-lisp, edit-definitions-lisp,
who-calls-lisp, edit-callers-lisp and trace-defun-lisp will switch
whether they prompt for a response or use a default when called with a
negative prefix.  If they are prompting, there is completion through
the inferior LISP by using TAB or M-TAB.  When you are entering an
expression in the minibuffer, all of the normal ilisp commands like
arglist-lisp also work.

Commands that work on a function will use the nearest previous
function symbol.  This is either a symbol after a #' or the symbol at
the start of the current list.

describe-lisp \(\\[describe-lisp]) will describe the previous sexp.
inspect-lisp \(\\[inpsect-lisp]) will inspect the previous sexp.If
there is no previous-sexp and you are in an ILISP buffer, the previous
result will be described or inspected.

arglist-lisp \(\\[arglist-lisp]) will return the arglist of the
current function.  With a numeric prefix, the leading paren will be
removed and the arglist will be inserted into the buffer.

documentation-lisp \(\\[documentation-lisp]) infers whether function
or variable documentation is desired.  With a negative prefix, you can
specify the type of documentation as well.  With a positive prefix the
documentation of the current function call is returned.

If the Franz online Common LISP manual is available, fi:clman
\(\\[fi:clman]) will get information on a specific symbol.
fi:clman-apropos \(\\[fi:clman-apropos]) will get information apropos
a specific string.  Some of the documentation is specific to the
allegro dialect, but most of it is for standard Common LISP.

macroexpand-lisp \(\\[macroexpand-lisp]) and macroexpand-1-lisp
\(\\[macroexpand-1-lisp]) will be applied to the next sexp.  They will
insert their result into the buffer if called with a numeric prefix.

complete-lisp \(\\[complete-lisp]) will try to complete the previous
symbol in the current inferior LISP.  Partial completion is supported
unless ilisp-prefix-match is set to T.  \(If you set it to T, inferior
LISP completions will be faster.)  With partial completion, \"p--n\"
would complete to \"position-if-not\" in Common LISP.  If the symbol
follows a left paren or a #', only symbols with function cells will be
considered.  If the symbol starts with a \* or you call with a
positive prefix all possible completions will be considered.  Only
external symbols are considered if there is a package qualification
with only one colon.  The first time you try to complete a string the
longest common substring will be inserted and the cursor will be left
on the point of ambiguity.  If you try to complete again, you can see
the possible completions.  If you are in a string, then filename
completion will be done instead.  And if you try to complete a
filename twice, you will see a list of possible completions.  Filename
components are completed individually, so /u/mi/ could expand to
/usr/misc/.  If you complete with a negative prefix, the most recent
completion \(symbol or filename) will be undone.

complete \(\\[complete]) will complete the current symbol to the most
recently seen symbol in Emacs that matches what you have typed so far.
Executing it repeatedly will cycle through potential matches.  This is
from the TMC completion package and there may be some delay as it is
initially loaded.

trace-defun-lisp \(\\[trace-defun-lisp]) traces the current defun.
When called with a numeric prefix the function will be untraced.

default-directory-lisp \(\\[default-directory-lisp]\) sets the default
inferior LISP directory to the directory of the current buffer.  If
called in an inferior LISP buffer, it sets the Emacs default-directory
the LISP default directory.

The eval/compile commands evaluate or compile the forms specified.  If
any of the forms contain an interactive command, then the command will
never return.  To get out of this state, you need to use
abort-commands-lisp \(\\[abort-commands-lisp]).  The eval/compile
commands verify that their expressions are balanced and then send the
form to the inferior LISP.  If called with a positive prefix, the
result of the operation will be inserted into the buffer after the
form that was just sent.  If lisp-wait-p is t, then EMACS will display
the result of the command in the minibuffer or a pop-up window.  If
lisp-wait-p is nil, (the default) the send is done asynchronously and
the results will be brought up only if there is more than one line or
there is an error.  In this case, you will be given the option of
ignoring the error, keeping it in another buffer or keeping it and
aborting all pending sends.  If there is not a command already running
in the inferior LISP, you can preserve the break loop.  If called with
a negative prefix, the sense of lisp-wait-p will be inverted for the
next command.  The and-go versions will perform the operation and then
immediately switch to the ILISP buffer where you will see the results
of executing your form.  If eval-defun-and-go-lisp
\(\\[eval-defun-and-go-lisp]) or compile-defun-and-go-lisp
\(\\[compile-defun-and-go-lisp]) is called with a prefix, a call for
the form will be inserted as well.

When an eval is done of a single form matching ilisp-defvar-regexp,
the corresponding symbol will be unbound and the value assigned again.

When compile-defun-lisp \(\\[compile-defun-lisp]) is called in an
inferior LISP buffer with no current form, the last form typed to the
top-level will be compiled.

The following commands all deal with finding things in source code.
The first time that one of these commands is used, there may be some
delay while the source module is loaded.  When searching files, the
first applicable rule is used: 1) try the inferior LISP, 2) try a tags
file if defined, 3) try all buffers in one of lisp-source-modes or all
files defined using lisp-directory.

lisp-directory \(\\[lisp-directory]) defines a set of files to be
searched by the source code commands.  It prompts for a directory and
sets the source files to be those in the directory that match entries
in auto-mode-alist for modes in lisp-source-modes.  With a positive
prefix, the files are appended.  With a negative prefix, all current
buffers that are in one of lisp-source-modes will be searched.  This
is also what happens by default.  Using this command stops using a
tags file.

edit-definitions-lisp \(\\[edit-definitions-lisp]) will find a
particular type of definition for a symbol.  It tries to use the rules
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
first caller using edit-definitions-lisp.  Each successive call to
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
kept.  If there is an error, the process will stop and show the error
and all remaining changes will remain in the list.  All of the results
will be kept in the buffer *Last-Changes*.

File commands in lisp-source-mode buffers keep track of the last used
directory and file.  If the point is on a string, that will be the
default if the file exists.  If the buffer is one of
lisp-source-modes, the buffer file will be the default.  Otherwise,
the last file used in a lisp-source-mode will be used.

find-file-lisp \(\\[find-file-lisp]) will find a file.  If it is in a
string, that will be used as the default if it matches an existing
file.  Symbolic links are expanded so that different references to the
same file will end up with the same buffer.

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
  (let* ((old-function (symbol-function function)))
    (if (consp old-function)  ; old-style v18 compiled-function objects
	;; I did not use rplacd so that I can replace read-only objects
	(let ((old-doc (cdr (cdr old-function))))
	  (fset function
		(nconc (list (car old-function)
			     (car (cdr old-function))
			     string)
		       (if (or (stringp (car old-doc)) (numberp (car old-doc)))
			   (cdr old-doc)
			 old-doc))))
      ;; else, new-style compiled-code objects
      (let ((code-as-list (append old-function nil)))
	(if (nthcdr 4 code-as-list)
	    (setcar (nthcdr 4 code-as-list) string)
	  (setcdr (nthcdr 3 code-as-list) (cons string nil)))
	(fset function (apply 'make-byte-code code-as-list))))))

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
	(setq major-mode 'ilisp-mode
	      mode-name "ILISP")
	(rplaca (car comint-send-queue) (function (lambda ()
					  (run-hooks 'ilisp-init-hook))))
	(setq ilisp-initialized (lisp-del ilisp-buffer ilisp-initialized))
	(if (not (lisp-memk names ilisp-buffers 'car))
	    (setq ilisp-buffers (cons (list names) ilisp-buffers)))
	(lisp-pop-to-buffer ilisp-buffer)
	(setq start (window-start (selected-window))
	      ilisp-program program)
	(goto-char (point-max))
	(insert (format "Starting %s ...\n" ilisp-program))
	(set-marker (process-mark (ilisp-process)) (point))
	(funcall comint-update-status 'start)
	(if ilisp-motd
	    (progn (lisp-display-output (format ilisp-motd ilisp-version))
		   (set-window-start (selected-window) start)))
	(if (not ilisp-prefix-match) (require 'completer)))
      (lisp-pop-to-buffer ilisp-buffer))
  (use-local-map ilisp-use-map)
  ;; This is necessary to get mode documentation to come out right
  (set-default 'ilisp-use-map ilisp-use-map))

;;;%Manual
(autoload 'fi:clman         "fi/clman" 
	  "Look up SYMBOL in the online manual with completion." t)
(autoload 'fi:clman-apropos "fi/clman" 
	  "Do an apropos search in online manual for STRING." t)

;;;%Bridges
(autoload 'install-bridge "bridge" "Install process bridge." t)

;;;%Bugs
(defun ilisp-bug ()
  "Generate an ilisp bug report."
  (interactive)
  (let ((buffer 
	 (if (y-or-n-p 
	      (format "Is %s the buffer where the error occurred? " 
		      (buffer-name (current-buffer))))
	     (current-buffer))))
    (if (or (not buffer) (not (mail)))
	(progn
	  (message 
	   (if buffer 
	       "Can't send bug report until mail buffer is empty."
	       "Switch to the buffer where the error occurred."))
	  (beep))
      (insert ilisp-bugs-to)
      (search-forward (concat "\n" mail-header-separator "\n"))
      (insert "\nYour problem: \n\n")
      (insert "Type C-c C-c to send\n")
      (insert "======= Emacs state below: for office use only =======\n")
      (forward-line 1)
      (insert (emacs-version))
      (insert 
       (format "\nWindow System: %s %s" window-system window-system-version))
      (let ((mode (save-excursion (set-buffer buffer) major-mode))
	    (match "popper-\\|completer-")
	    (val-buffer buffer)
	    string)
	(if (or (memq mode lisp-source-modes) (memq mode ilisp-modes))
	    (progn
	      (setq match (concat "ilisp-\\|comint-\\|lisp-" match)
		    val-buffer (save-excursion (set-buffer buffer)
					       (or (ilisp-buffer) buffer)))
	      (mapcar (function (lambda (dialect)
				  (setq match (concat (format "%s-\\|" (car dialect))
						      match))))
		      ilisp-dialects)
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
	      (insert string)))
	(mapatoms
	 (function (lambda (symbol)
		     (if (and (boundp symbol)
			      (string-match match (format "%s" symbol))
			      (not (eq symbol 'ilisp-documentation)))
			 (let ((val (save-excursion
				      (set-buffer val-buffer) 
				      (symbol-value symbol))))
			   (if val
			       (insert (format "\n%s: %s" symbol val))))))))
	(insert (format "\nLossage: %s" (key-description (recent-keys))))
	(if (and (or (memq mode lisp-source-modes)
		     (memq mode ilisp-modes))
		 (ilisp-buffer) 
		 (memq 'clisp (ilisp-value 'ilisp-dialect t))
		 (not (cdr (ilisp-value 'comint-send-queue))))
	    (progn
	      (insert (format "\nLISP: %s"
			      (comint-remove-whitespace
			       (car (comint-send
				     (save-excursion
				       (set-buffer buffer)
				       (ilisp-process))
				     "(lisp-implementation-version)"
				     t t 'version)))))
	      (insert (format "\n*FEATURES*: %s"
			      (comint-remove-whitespace
			       (car (comint-send
				     (save-excursion
				       (set-buffer buffer)
				       (ilisp-process))
				     "(let ((*print-length* nil)
				       (*print-level* nil))
				   (print *features*)
				   nil)"
				     t t 'version)))))))
	(insert ?\n)
	(goto-char (point-min))
	(re-search-forward "^Subject")
	(end-of-line)
	(message "Send with sendmail or your favorite mail program.")))))

;;;%Modes
(set-default 'auto-mode-alist
	     (append '(("\\.cl$" . lisp-mode) ("\\.lisp$" . lisp-mode))
		     auto-mode-alist))
(setq completion-ignored-extensions 
      (append '(".68fasl" ".sfasl" ".ifasl" ".pfasl" 
		".68fasl4" ".sfasl4" ".ifasl4" ".pfasl4" 
		".sbin")
	      completion-ignored-extensions))

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
	     (define-key keymap "\C-c\C-c" 'interrupt-subjob-ilisp)
	     (define-key keymap "\C-d" 'delete-char-or-pop-ilisp)
	     (ilisp-defkey keymap "#" 'raw-keys-ilisp))
      (ilisp-defkey keymap "\C-c" 'compile-defun-and-go-lisp)
      (define-key keymap "\C-m" 'newline-and-indent-lisp))
  (define-key   keymap "]"        'close-all-lisp)
  (define-key   keymap "\M-q"     'reindent-lisp)
  (define-key   keymap "\C-]"     'close-and-send-lisp)
  (define-key   keymap "\t"       'indent-line-ilisp)
  (define-key   keymap "\n"       'newline-and-indent-lisp)
  (define-key   keymap "\M-\C-q"  'indent-sexp-ilisp)
  (ilisp-defkey keymap ";"        'comment-region-lisp)
  (ilisp-defkey keymap ")"        'find-unbalanced-lisp)
  (define-key   keymap "\M-\C-a"  'beginning-of-defun-lisp)
  (define-key   keymap "\M-\C-e"  'end-of-defun-lisp)
  (define-key   keymap "\C-\M-r"  'reposition-window-lisp)
  (ilisp-defkey keymap "i"        'describe-lisp)
  (ilisp-defkey keymap "I"        'inspect-lisp)
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
  (define-key   keymap "\M-\C-m"  'complete)
  (ilisp-defkey keymap "r"        'eval-region-lisp)
  (define-key   keymap "\M-\C-x"  'eval-defun-lisp) ; Gnu convention
  (ilisp-defkey keymap "e"        'eval-defun-lisp)
  (ilisp-defkey keymap "n"        'eval-next-sexp-lisp)
  (ilisp-defkey keymap "p"        'package-lisp)
  (ilisp-defkey keymap "P"        'set-package-lisp)
  (ilisp-defkey keymap "w"        'compile-region-lisp)
  (ilisp-defkey keymap "c"        'compile-defun-lisp)
  (ilisp-defkey keymap "\C-r"     'eval-region-and-go-lisp)
  (ilisp-defkey keymap "\C-e"     'eval-defun-and-go-lisp)
  (ilisp-defkey keymap "\C-n"     'eval-next-sexp-and-go-lisp)
  (ilisp-defkey keymap "\C-w"     'compile-region-and-go-lisp)
  (ilisp-defkey keymap "t"        'trace-defun-lisp)
  (ilisp-defkey keymap "!"        'default-directory-lisp)
  (ilisp-defkey keymap " "        'mark-change-lisp)
  (let ((ilisp-prefix (concat ilisp-prefix "*")))
    (ilisp-defkey keymap "l"      'list-changes-lisp)
    (ilisp-defkey keymap "e"      'eval-changes-lisp)
    (ilisp-defkey keymap "c"      'compile-changes-lisp)
    (ilisp-defkey keymap "0"      'clear-changes-lisp))
  (ilisp-defkey keymap "b"        'switch-to-lisp)
  (ilisp-defkey keymap "y"        'call-defun-lisp)
  (ilisp-defkey keymap "z"        'reset-ilisp)
  (ilisp-defkey keymap "g"        'abort-commands-lisp)
  (ilisp-defkey keymap "s"        'status-lisp)
  (ilisp-defkey keymap "S"        'select-ilisp)
  (define-key   keymap "\C-x\C-f" 'find-file-lisp)
  (ilisp-defkey keymap "l"        'load-file-lisp)
  (ilisp-defkey keymap "k"        'compile-file-lisp)
  (ilisp-defkey keymap "A"        'fi:clman-apropos)
  (ilisp-defkey keymap "D"        'fi:clman))

;;;
(defun ilisp-bindings ()
  "Set up the key bindings for LISP and ILISP buffers."
  (setq ilisp-mode-map (full-copy-sparse-keymap comint-mode-map))
  ;; Remove stop and quit subjob from comint
  (define-key ilisp-mode-map "\C-c\C-z" nil)
  (define-key ilisp-mode-map "\C-c\C-\\" nil)
  (if (fboundp 'lisp-mode-commands)
      (lisp-mode-commands ilisp-mode-map)
    (if (fboundp 'set-keymap-parent)
	(set-keymap-parent ilisp-mode-map shared-lisp-mode-map)))
  (lisp-bindings ilisp-mode-map t)
  (if (boundp 'lisp-mode-map) (lisp-bindings lisp-mode-map))
  (if (boundp 'scheme-mode-map) (lisp-bindings scheme-mode-map))
  (ilisp-defkey emacs-lisp-mode-map ";" 'comment-region-lisp)
  (ilisp-defkey global-map "b" 'switch-to-lisp)
  (ilisp-defkey global-map "1" 'popper-bury-output)
  (ilisp-defkey global-map "v" 'popper-scroll-output)
  (ilisp-defkey global-map "G" 'popper-grow-output)
  (if (not (boundp 'fi:clman-mode-map))
      (setq fi:clman-mode-map (make-sparse-keymap)))
  (ilisp-defkey fi:clman-mode-map "D" 'fi:clman)
  (ilisp-defkey fi:clman-mode-map "A" 'fi:clman-apropos))

(defun defkey-ilisp (key command &optional inferior-only)
  "Define KEY as COMMAND in ilisp-mode-map and lisp-mode-map unless
optional INFERIOR-ONLY is T.  If the maps do not exist they will be
created.  This should only be called after ilisp-prefix is set to the
desired prefix."
  (if (not ilisp-mode-map) (ilisp-bindings))
  (define-key ilisp-mode-map key command)
  (define-key lisp-mode-map key command))

;;;
;;; All done
(provide 'ilisp)
(run-hooks 'ilisp-site-hook)
(run-hooks 'ilisp-load-hook)
(if (not lisp-no-popper) 
    (if (and (boundp 'epoch::version) epoch::version)
	(require 'epoch-pop)
	(require 'popper)))
(if (not ilisp-mode-map) (ilisp-bindings))

