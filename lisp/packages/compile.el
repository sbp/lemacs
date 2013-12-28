;;; compile.el --- run compiler as inferior of Emacs, parse error messages.

;; Copyright (C) 1985, 1986, 1987, 1993, 1994 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@prep.ai.mit.edu>
;; Maintainer: FSF
;; Keywords: tools, processes

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

;;; Commentary:

;; This package provides the compile and grep facilities documented in
;; the Emacs user's manual.

;;; Code:

;;;###autoload
(defvar compilation-mode-hook nil
  "*List of hook functions run by `compilation-mode' (see `run-hooks').")

;;;###autoload
(defconst compilation-window-height nil
  "*Number of lines in a compilation window.  If nil, use Emacs default.")

(defvar compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a cons (or nil).  Its car is a marker pointing to
an error message.  If its cdr is a marker, it points to the text of the
line the message is about.  If its cdr is a cons, that cons's car is a cons
\(DIRECTORY . FILE\), specifying the file the message is about, and its cdr
is the number of the line the message is about.  Or its cdr may be nil if
that error is not interesting.

The value may be t instead of a list; this means that the buffer of
error messages should be reparsed the next time the list of errors is wanted.

Some other commands (like `diff') use this list to control the error
message tracking facilites; if you change its structure, you should make
sure you also change those packages.  Perhaps it is better not to change
it at all.")

(defvar compilation-old-error-list nil
  "Value of `compilation-error-list' after errors were parsed.")

(defvar compilation-parse-errors-function 'compilation-parse-errors 
  "Function to call to parse error messages from a compilation.
It takes args LIMIT-SEARCH and FIND-AT-LEAST.
If LIMIT-SEARCH is non-nil, don't bother parsing past that location.
If FIND-AT-LEAST is non-nil, don't bother parsing after finding that 
 many new erros.
It should read in the source files which have errors and set
`compilation-error-list' to a list with an element for each error message
found.  See that variable for more info.")

;;;###autoload
(defvar compilation-buffer-name-function nil
  "Function to compute the name of a compilation buffer.
The function receives one argument, the name of the major mode of the
compilation buffer.  It should return a string.
nil means compute the name with `(concat \"*\" (downcase major-mode) \"*\")'.")

;;;###autoload
(defvar compilation-finish-function nil
  "*Function to call when a compilation process finishes.
It is called with two arguments: the compilation buffer, and a string
describing how the process finished.")

(defvar compilation-last-buffer nil
  "The most recent compilation buffer.
A buffer becomes most recent when its compilation is started
or when it is used with \\[next-error] or \\[compile-goto-error].")

(defvar compilation-in-progress nil
  "List of compilation processes now running.")
(or (assq 'compilation-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-in-progress " Compiling")
				 minor-mode-alist)))

(defvar compilation-parsing-end nil
  "Position of end of buffer when last error messages were parsed.")

(defvar compilation-error-message "No more errors"
  "Message to print when no more matches are found.")

(defvar compilation-num-errors-found)

;; lemacs change: 4th elt can be column number.
(defvar compilation-error-regexp-alist
  '(
    ;; NOTE!  This first one is repeated in grep-regexp-alist, below.

    ;; 4.3BSD grep, cc, lint pass 1:
    ;; 	/usr/src/foo/foo.c(8): warning: w may be used before set
    ;; or GNU utilities:
    ;; 	foo.c:8: error message
    ;; or HP-UX 7.0 fc:
    ;; 	foo.f          :16    some horrible error message
    ;;
    ;; We'll insist that the number be followed by a colon or closing
    ;; paren, because otherwise this matches just about anything
    ;; containing a number with spaces around it.
    ("\n\\([^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)[:) \t]" 1 2)

    ;; 4.3BSD lint pass 2
    ;; 	strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)
    ("[ \t:]\\([^:( \t\n]+\\)[:(](+[ \t]*\\([0-9]+\\))[:) \t]*$" 1 2)

    ;; 4.3BSD lint pass 3
    ;; 	bloofle defined( /users/wolfgang/foo.c(4) ), but never used
    ;; This used to be
    ;; ("[ \t(]+\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+" 1 2)
    ;; which is regexp Impressionism - it matches almost anything!
    ("([ \t]*\\([^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))" 1 2)

    ;; Ultrix 3.0 f77:
    ;;  Error on line 3 of t.f: Execution error unclassifiable statement    
    ;; Unknown who does this:
    ;;  Line 45 of "foo.c": bloofel undefined
    ("\n\\(Error on \\)?[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
of[ \t]+\"?\\([^\"\n]+\\)\"?:" 3 2)

    ;; Lucid Compiler, lcc 3.x
    ;; E, file.cc(35,52) Illegal operation on pointers
    ("\n[A-Z], \\([^(]*\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)" 1 2 3)

    ;; Apollo cc, 4.3BSD fc:
    ;;	"foo.f", line 3: Error: syntax error near end of statement
    ;; IBM RS6000:
    ;;  "vvouch.c", line 19.5: 1506-046 (S) Syntax error.
    ;; Unknown compiler:
    ;;  File "foobar.ml", lines 5-8, characters 20-155: blah blah
    ("\"\\([^,\" \n\t]+\\)\", lines? \\([0-9]+\\)[:.,-]" 1 2)

    ;; MIPS RISC CC - the one distributed with Ultrix:
    ;;	ccom: Error: foo.c, line 2: syntax error
    ("rror: \\([^,\" \n\t]+\\), line \\([0-9]+\\):" 1 2)

    ;; IBM AIX PS/2 C version 1.1:
    ;;	****** Error number 140 in line 8 of file errors.c ******
    ("in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)
    ;; IBM AIX lint is too painful to do right this way.  File name
    ;; prefixes entire sections rather than being on each line.

    )
  "Alist that specifies how to match errors in compiler output.
Each element has the form (REGEXP FILE-IDX LINE-IDX [COL-IDX]).
If REGEXP matches, the FILE-IDX'th subexpression gives the file
name, and the LINE-IDX'th subexpression gives the line number.
COL-IDX'th subexpression gives the column number of the error. COL-IDX
is optional, and if not present, beginning of line is used.")

(defvar grep-regexp-alist
  '(("^\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match grep hits.  See `compilation-error-regexp-alist'.")

;;;###autoload
(defvar compilation-search-path '(nil)
  "*List of directories to search for source files named in error messages.
Elements should be directory names, not file names of directories.
nil as an element means to try the default directory.")

(defvar compile-command "make -k "
  "Last shell command used to do a compilation; default for next compilation.

Sometimes it is useful for files to supply local values for this variable.
You might also use mode hooks to specify it in certain modes, like this:

    (setq c-mode-hook
      '(lambda () (or (file-exists-p \"makefile\") (file-exists-p \"Makefile\")
		      (progn (make-local-variable 'compile-command)
			     (setq compile-command
				    (concat \"make -k \"
					    buffer-file-name))))))")

(defconst compilation-enter-directory-regexp
  ": Entering directory `\\(.*\\)'$"
  "Regular expression matching lines that indicate a new current directory.
This must contain one \\(, \\) pair around the directory name.

The default value matches lines printed by the `-w' option of GNU Make.")

(defconst compilation-leave-directory-regexp
  ": Leaving directory `\\(.*\\)'$"
  "Regular expression matching lines that indicate restoring current directory.
This may contain one \\(, \\) pair around the name of the directory
being moved from.  If it does not, the last directory entered \(by a
line matching `compilation-enter-directory-regexp'\) is assumed.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar compilation-directory-stack nil
  "Stack of previous directories for `compilation-leave-directory-regexp'.
The head element is the directory the compilation was started in.")

;; History of compile commands.
(defvar compile-history nil)
;; History of grep commands.
(defvar grep-history nil)

;;;###autoload
(defun compile (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

To run more than one compilation at once, start one and rename the
\`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive (list (read-string "Compile command: "
				  compile-command 'compile-history)))
  (setq compile-command command)
  (save-some-buffers nil nil)
  (compile-internal compile-command "No more errors"))

;;;###autoload
(defun grep (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command."
  (interactive
   (list (read-shell-command "Run grep (like this): "
			     "grep -n " 'grep-history)))
  (compile-internal (concat command-args " /dev/null")
		    "No more grep hits" "grep"
		    ;; Give it a simpler regexp to match.
		    nil grep-regexp-alist))

(defun compile-internal (command error-message
				 &optional name-of-mode parser regexp-alist
				 name-function)
  "Run compilation command COMMAND (low level interface).
ERROR-MESSAGE is a string to print if the user asks to see another error
and there are no more errors.  Third argument NAME-OF-MODE is the name
to display as the major mode in the compilation buffer.

Fourth arg PARSER is the error parser function (nil means the default).  Fifth
arg REGEXP-ALIST is the error message regexp alist to use (nil means the
default).  Sixth arg NAME-FUNCTION is a function called to name the buffer (nil
means the default).  The defaults for these variables are the global values of
\`compilation-parse-errors-function', `compilation-error-regexp-alist', and
\`compilation-buffer-name-function', respectively.

Returns the compilation buffer created."
  (let (outbuf)
    (save-excursion
      (or name-of-mode
	  (setq name-of-mode "Compilation"))
      (setq outbuf
	    (get-buffer-create
	     (funcall (or name-function compilation-buffer-name-function
			  (function (lambda (mode)
				      (concat "*" (downcase mode) "*"))))
		      name-of-mode)))
      (set-buffer outbuf)
      (let ((comp-proc (get-buffer-process (current-buffer))))
	(if comp-proc
	    (if (or (not (eq (process-status comp-proc) 'run))
		    (yes-or-no-p
		     (format "A %s process is running; kill it? "
			     name-of-mode)))
		(condition-case ()
		    (progn
		      (interrupt-process comp-proc)
		      (sit-for 1)
		      (delete-process comp-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
		     (buffer-name))
	      )))
      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables))
    (let ((regexp-alist (or regexp-alist compilation-error-regexp-alist))
	  (parser (or parser compilation-parse-errors-function))
	  (thisdir default-directory)
	  outwin) 
      (save-excursion
	;; Clear out the compilation buffer and make it writable.
	;; Change its default-directory to the directory where the compilation
	;; will happen, and insert a `cd' command to indicate this.
	(set-buffer outbuf)
	(setq buffer-read-only nil)
	(erase-buffer)
	(setq default-directory thisdir)
	(insert "cd " thisdir "\n" command "\n")
	(set-buffer-modified-p nil))
      ;; If we're already in the compilation buffer, go to the end
      ;; of the buffer, so point will track the compilation output.
      (if (eq outbuf (current-buffer))
	  (goto-char (point-max)))
      ;; Pop up the compilation buffer.
      (setq outwin (display-buffer outbuf))
      (save-excursion
	(set-buffer outbuf)
	(compilation-mode)
	(buffer-disable-undo (current-buffer))
	;; (setq buffer-read-only t)  ;;; Non-ergonomic.
	(set (make-local-variable 'compilation-parse-errors-function) parser)
	(set (make-local-variable 'compilation-error-message) error-message)
	(set (make-local-variable 'compilation-error-regexp-alist) regexp-alist)
	(setq default-directory thisdir
	      compilation-directory-stack (list default-directory))
	(set-window-start outwin (point-min))
	(setq mode-name name-of-mode)
	(or (eq outwin (selected-window))
	    (set-window-point outwin (point-min)))
	(and compilation-window-height
	     (= (window-width outwin) (screen-width))
	     (let ((w (selected-window)))
	       (unwind-protect
		   (progn
		     (select-window outwin)
		     (enlarge-window (- compilation-window-height
					(window-height))))
		 (select-window w))))
	;; Start the compilation.
	(let ((proc (start-process-shell-command (downcase mode-name)
						 outbuf
						 command)))
	  (set-process-sentinel proc 'compilation-sentinel)
	  (set-process-filter proc 'compilation-filter)
	  (set-marker (process-mark proc) (point) outbuf)
	  (setq compilation-in-progress (cons proc compilation-in-progress)))))
    ;; Make it so the next C-x ` will use this buffer.
    (setq compilation-last-buffer outbuf)))

(defvar compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'compilation-minor-mode-map)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    map)
  "Keymap for `compilation-minor-mode'.")

(defvar compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (set-keymap-name map 'compilation-mode-map)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map 'button2 'compile-mouse-goto-error)
    (define-key map 'button3 'compile-popup-menu)
    map)
  "Keymap for compilation log buffers.
`compilation-minor-mode-map' is a parent of this.")

(defun compilation-mode ()
  "Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error],
or click on the line with \\[compile-mouse-goto-error].
There is a menu of commands on \\[compile-popup-menu].
To kill the compilation, type \\[kill-compilation].

Runs `compilation-mode-hook' with `run-hooks' (which see)."
  (interactive)
  (fundamental-mode)
  (use-local-map compilation-mode-map)
  (setq major-mode 'compilation-mode
	mode-name "Compilation")
  (compilation-setup)
  (run-hooks 'compilation-mode-hook))

;; Prepare the buffer for the compilation parsing commands to work.
(defun compilation-setup ()
  ;; Make the buffer's mode line show process state.
  (setq mode-line-process '(": %s"))
  (set (make-local-variable 'compilation-error-list) nil)
  (set (make-local-variable 'compilation-old-error-list) nil)
  (set (make-local-variable 'compilation-parsing-end) 1)
  (set (make-local-variable 'compilation-directory-stack) nil)
  (setq compilation-last-buffer (current-buffer))
  ;; Lemacs change: highlight lines, install menubar.
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line)
  )

(defvar compilation-minor-mode nil
  "Non-nil when in compilation-minor-mode.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available.")
(make-variable-buffer-local 'compilation-minor-mode)

(or (assq 'compilation-minor-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-minor-mode " Compilation")
				 minor-mode-alist)))
;(or (assq 'compilation-minor-mode minor-mode-map-alist)
;    (setq minor-mode-map-alist (cons (cons 'compilation-minor-mode
;					   compilation-minor-mode-map)
;				     minor-mode-map-alist)))

;;;###autoload
(defun compilation-minor-mode (&optional arg)
  "Toggle compilation minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'."
  (interactive "P")
  (if (setq compilation-minor-mode (if (null arg)
				       (null compilation-minor-mode)
				     (> (prefix-numeric-value arg) 0)))
      (compilation-setup)))

;; Called when compilation process changes state.
(defun compilation-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (let ((buffer (process-buffer proc)))
    (if (memq (process-status proc) '(signal exit))
	(progn
	  (if (null (buffer-name buffer))
	      ;; buffer killed
	      (set-process-buffer proc nil)
	    (let ((obuf (current-buffer))
		  omax opoint)
	      ;; save-excursion isn't the right thing if
	      ;; process-buffer is current-buffer
	      (unwind-protect
		  (progn
		    ;; Write something in the compilation buffer
		    ;; and hack its mode line.
		    (set-buffer buffer)
		    (let ((buffer-read-only nil))
		      (setq omax (point-max)
			    opoint (point))
		      (goto-char omax)
		      ;; Record where we put the message, so we can ignore it
		      ;; later on.
		      (insert ?\n mode-name " " msg)
		      (forward-char -1)
		      (insert " at " (substring (current-time-string) 0 19))
		      (forward-char 1)
		      (setq mode-line-process
			    (concat ": "
				    (symbol-name (process-status proc))))
		      ;; Since the buffer and mode line will show that the
		      ;; process is dead, we can delete it now.  Otherwise it
		      ;; will stay around until M-x list-processes.
		      (delete-process proc)
		      ;; Force mode line redisplay soon.
		      (set-buffer-modified-p (buffer-modified-p)))
		    (if (and opoint (< opoint omax))
			(goto-char opoint))
		    (if compilation-finish-function
			(funcall compilation-finish-function buffer msg)))
		(set-buffer obuf))))
	  (setq compilation-in-progress (delq proc compilation-in-progress))
	  ))))

(defun compilation-filter (proc string)
  "Process filter for compilation buffers.
Just inserts the text, but uses `insert-before-markers'."
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((buffer-read-only nil))
      (save-excursion
	(goto-char (process-mark proc))
	(insert-before-markers string)
	(set-marker (process-mark proc) (point))))))

;; Return the cdr of compilation-old-error-list for the error containing point.
(defun compile-error-at-point ()
  (compile-reinitialize-errors nil (point))
  (let ((errors compilation-old-error-list))
    (while (and errors
		(> (point) (car (car errors))))
      (setq errors (cdr errors)))
    errors))

(defun compilation-next-error (n)
  "Move point to the next error in the compilation buffer.
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer."))
  (setq compilation-last-buffer (current-buffer))

  (let ((errors (compile-error-at-point)))

    ;; Move to the error after the one containing point.
    (goto-char (car (if (< n 0)
			(let ((i 0)
			      (e compilation-old-error-list))
			  ;; See how many cdrs away ERRORS is from the start.
			  (while (not (eq e errors))
			    (setq i (1+ i)
				  e (cdr e)))
			  (if (> (- n) i)
			      (error "Moved back past first error")
			    (nth (+ i n) compilation-old-error-list)))
		      (let ((compilation-error-list (cdr errors)))
			(compile-reinitialize-errors nil nil n)
			(if compilation-error-list
			    (nth (1- n) compilation-error-list)
			  (error "Moved past last error"))))))))

(defun compilation-previous-error (n)
  "Move point to the previous error in the compilation buffer.
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (compilation-next-error (- n)))


;; Given an elt of `compilation-error-list', return an object representing
;; the referenced file which is equal to (but not necessarily eq to) what
;; this function would return for another error in the same file.
(defsubst compilation-error-filedata (data)
  (setq data (cdr data))
  (if (markerp data)
      (marker-buffer data)
    (car data)))

;; Return a string describing a value from compilation-error-filedata.
;; This value is not necessarily useful as a file name, but should be
;; indicative to the user of what file's errors are being referred to.
(defsubst compilation-error-filedata-file-name (filedata)
  (if (bufferp filedata)
      (buffer-file-name filedata)
    (car filedata)))

(defun compilation-next-file (n)
  "Move point to the next error for a different file than the current one."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer."))
  (setq compilation-last-buffer (current-buffer))

  (let ((reversed (< n 0))
	errors filedata)

    (if (not reversed)
	(setq errors (or (compile-error-at-point)
			 (error "Moved past last error")))

      ;; Get a reversed list of the errors up through the one containing point.
      (compile-reinitialize-errors nil (point))
      (setq errors (reverse compilation-old-error-list)
	    n (- n))

      ;; Ignore errors after point.  (car ERRORS) will be the error
      ;; containing point, (cadr ERRORS) the one before it.
      (while (and errors
		  (< (point) (car (car errors))))
	(setq errors (cdr errors))))

    (while (> n 0)
      (setq filedata (compilation-error-filedata (car errors)))

      ;; Skip past the following errors for this file.
      (while (equal filedata
		    (compilation-error-filedata
		     (car (or errors
			      (if reversed
				  (error "%s the first erring file"
					 (compilation-error-filedata-file-name
					  filedata))
				(let ((compilation-error-list nil))
				  ;; Parse some more.
				  (compile-reinitialize-errors nil nil 2)
				  (setq errors compilation-error-list)))
			      (error "%s is the last erring file" 
				     (compilation-error-filedata-file-name
				      filedata))))))
	(setq errors (cdr errors)))

      (setq n (1- n)))

    ;; Move to the following error.
    (goto-char (car (car (or errors
			     (if reversed
				 (error "This is the first erring file")
			       (let ((compilation-error-list nil))
				 ;; Parse the last one.
				 (compile-reinitialize-errors nil nil 1)
				 compilation-error-list))))))))

(defun compilation-previous-file (n)
  "Move point to the previous error for a different file than the current one."
  (interactive "p")
  (compilation-next-file (- n)))


(defun kill-compilation ()
  "Kill the process made by the \\[compile] command."
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
	(interrupt-process (get-buffer-process buffer))
      (error "The compilation process is not running."))))


;; Parse any new errors in the compilation buffer,
;; or reparse from the beginning if the user has asked for that.
(defun compile-reinitialize-errors (argp &optional limit-search find-at-least)
  (save-excursion
    (set-buffer compilation-last-buffer)
    ;; If we are out of errors, or if user says "reparse",
    ;; discard the info we have, to force reparsing.
    (if (or (eq compilation-error-list t)
	    (consp argp))
	(progn (compilation-forget-errors)
	       (setq compilation-parsing-end 1)))
    (if (and compilation-error-list
	     (or (not limit-search)
		 (> compilation-parsing-end limit-search))
	     (or (not find-at-least)
		 (>= (length compilation-error-list) find-at-least)))
	;; Since compilation-error-list is non-nil, it points to a specific
	;; error the user wanted.  So don't move it around.
	nil

      ;; lemacs change: if the compilation buffer is already visible
      ;; in a window, use that instead of thrashing the display.
      (let ((w (get-buffer-window compilation-last-buffer)))
	(if w
	    (select-window w)
	  (switch-to-buffer compilation-last-buffer)))

      (set-buffer-modified-p nil)
      (if (< compilation-parsing-end (point-max))
	  (let ((at-start (= compilation-parsing-end 1)))
	    (funcall compilation-parse-errors-function
		     limit-search find-at-least)
	    ;; Remember the entire list for compilation-forget-errors.
	    ;; If this is an incremental parse, append to previous list.
	    (if at-start
		(setq compilation-old-error-list compilation-error-list)
	      (setq compilation-old-error-list
		    (nconc compilation-old-error-list compilation-error-list)))
	    )))))

(defun compile-goto-error (&optional argp)
  "Visit the source for the error message point is on.
Use this command in a compilation log buffer.
\\[universal-argument] as a prefix arg means to reparse the buffer's error messages first;
other kinds of prefix arguments are ignored."
  (interactive "P")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer."))
  (setq compilation-last-buffer (current-buffer))
  (compile-reinitialize-errors argp (point))

  ;; Move to bol; the marker for the error on this line will point there.
  (beginning-of-line)

  ;; Move compilation-error-list to the elt of compilation-old-error-list
  ;; we want.
  (setq compilation-error-list compilation-old-error-list)
  (while (and compilation-error-list
	      (> (point) (car (car compilation-error-list))))
    (setq compilation-error-list (cdr compilation-error-list)))

  ;; Move to another window, so that next-error's window changes
  ;; result in the desired setup.
  (or (one-window-p)
      (progn
	(other-window -1)
	;; other-window changed the selected buffer,
	;; but we didn't want to do that.
	(set-buffer compilation-last-buffer)))

  (next-error 1))

(defun compile-mouse-goto-error (event)
  "Visit the source for the error under the mouse.
Use this command in a compilation log buffer."
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (compile-goto-error))

(defun compilation-buffer-p (buffer)
  (assq 'compilation-error-list (buffer-local-variables buffer)))

;; Return a compilation buffer.
;; If the current buffer is a compilation buffer, return it.
;; If compilation-last-buffer is set to a live buffer, use that.
;; Otherwise, look for a compilation buffer and signal an error
;; if there are none.
(defun compilation-find-buffer (&optional other-buffer)
  (if (and (not other-buffer)
	   (compilation-buffer-p (current-buffer)))
      ;; The current buffer is a compilation buffer.
      (current-buffer)
    (if (and compilation-last-buffer (buffer-name compilation-last-buffer)
	     (or (not other-buffer) (not (eq compilation-last-buffer
					     (current-buffer)))))
	compilation-last-buffer
      (let ((buffers (buffer-list)))
	(while (and buffers (or (not (compilation-buffer-p (car buffers)))
				(and other-buffer
				     (eq (car buffers) (current-buffer)))))
	  (setq buffers (cdr buffers)))
	(if buffers
	    (car buffers)
	  (or (and other-buffer
		   (compilation-buffer-p (current-buffer))
		   ;; The current buffer is a compilation buffer.
		   (progn
		     (if other-buffer
			 (message "This is the only compilation buffer."))
		     (current-buffer)))
	      (error "No compilation started!")))))))

;;;###autoload
(defun next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.

A prefix arg specifies how many error messages to move;
negative means move back to previous error messages.
Just C-u as a prefix means reparse the error message buffer
and start at the first error.

\\[next-error] normally applies to the most recent compilation started,
but as long as you are in the middle of parsing errors from one compilation
output buffer, you stay with that compilation output buffer.

Use \\[next-error] in a compilation output buffer to switch to
processing errors from that compilation.

See variables `compilation-parse-errors-function' and
\`compilation-error-regexp-alist' for customization ideas."
  (interactive "P")
  (setq compilation-last-buffer (compilation-find-buffer))
  (compile-reinitialize-errors argp nil
			       ;; We want to pass a number here only if
			       ;; we got a numeric prefix arg, not just C-u.
			       (and (not (consp argp))
				    (if (< (prefix-numeric-value argp) 1)
					0
				      (1- (prefix-numeric-value argp)))))
  ;; Make ARGP nil if the prefix arg was just C-u,
  ;; since that means to reparse the errors, which the
  ;; compile-reinitialize-errors call just did.
  ;; Now we are only interested in a numeric prefix arg.
  (if (consp argp)
      (setq argp nil))
  (let (next-errors next-error)
    (save-excursion
      (set-buffer compilation-last-buffer)
      ;; compilation-error-list points to the "current" error.
      (setq next-errors 
	    (if (> (prefix-numeric-value argp) 0)
		(nthcdr (1- (prefix-numeric-value argp))
			compilation-error-list)
	      ;; Zero or negative arg; we need to move back in the list.
	      (let ((n (1- (prefix-numeric-value argp)))
		    (i 0)
		    (e compilation-old-error-list))
		;; See how many cdrs away the current error is from the start.
		(while (not (eq e compilation-error-list))
		  (setq i (1+ i)
			e (cdr e)))
		(if (> (- n) i)
		    (error "Moved back past first error")
		  (nthcdr (+ i n) compilation-old-error-list))))
	    next-error (car next-errors))
      (while
	  (progn
	    (if (null next-error)
;; lemacs change by Barry Warsaw.
;; Without this, if you a "no more errors" error, then you can't do
;; previous-error or goto-error until you kill the buffer.
;		(progn
;		  (if argp (if (> (prefix-numeric-value argp) 0)
;			       (error "Moved past last error")
;			     (error "Moved back past first error")))
;		  (compilation-forget-errors)
;		  (error (concat compilation-error-message
;				 (and (get-buffer-process (current-buffer))
;				      (eq (process-status
;					   (get-buffer-process
;					    (current-buffer)))
;					  'run)
;				      " yet"))))

		(if (> (prefix-numeric-value argp) 0)
		    (error "Moved past last error")
		  (error "Moved back past first error"))

	      (setq compilation-error-list (cdr next-errors))
	      (if (null (cdr next-error))
		  ;; This error is boring.  Go to the next.
		  t
		(or (markerp (cdr next-error))
		    ;; This error has a filename/lineno pair.
		    ;; Find the file and turn it into a marker.
		    (let* ((fileinfo (car (cdr next-error)))
			   (buffer (compilation-find-file (cdr fileinfo)
							  (car fileinfo)
							  (car next-error))))
		      (if (null buffer)
			  ;; We can't find this error's file.
			  ;; Remove all errors in the same file.
			  (progn
			    (setq next-errors compilation-old-error-list)
			    (while next-errors
			      (and (consp (cdr (car next-errors)))
				   (equal (car (cdr (car next-errors)))
					  fileinfo)
				   (progn
				     (set-marker (car (car next-errors)) nil)
				     (setcdr (car next-errors) nil)))
			      (setq next-errors (cdr next-errors)))
			    ;; Look for the next error.
			    t)
			;; We found the file.  Get a marker for this error.
			;; compilation-old-error-list is a buffer-local
			;; variable, so we must be careful to extract its value
			;; before switching to the source file buffer.
			(let ((errors compilation-old-error-list)
			      ;; lemacs change (columns)
			      (last-line (nth 1 (cdr next-error)))
			      (column (nth 2 (cdr next-error)))
			      )
			  (set-buffer buffer)
			  (save-excursion
			    (save-restriction
			      (widen)
			      (goto-line last-line)
			      (beginning-of-line)
			      ;; lemacs change (columns)
			      (and column
				   (forward-char (1- column)))
			      (setcdr next-error (point-marker))
			      ;; Make all the other error messages referring
			      ;; to the same file have markers into the buffer.
			      (while errors
				(and (consp (cdr (car errors)))
				     (equal (car (cdr (car errors))) fileinfo)
				     (let ((this (cdr (cdr (car errors))))
					   ;; lemacs change (columns)
					   (lines (- (nth 1 (cdr (car errors)))
						     last-line))
					   (col (nth 2 (cdr (car errors)))))
				       (if (eq selective-display t)
					   (if (< lines 0)
					       (re-search-backward "[\n\C-m]"
								   nil 'end
								   (- lines))
					     (re-search-forward "[\n\C-m]"
								nil 'end
								lines))
					 (forward-line lines))
				       ;; lemacs change (columns)
				       (setq last-line (car this))
				       (if col
					   (let ((eol (save-excursion
							(end-of-line)
							(point))))
					     (forward-char  ; don't pass eol.
					      (min (1- col) (- eol (point))))))
				       (setcdr (car errors) (point-marker))))
				(setq errors (cdr errors)))))))))
		;; If we didn't get a marker for this error,
		;; go on to the next one.
		(not (markerp (cdr next-error))))))
	(setq next-errors compilation-error-list
	      next-error (car next-errors))))

    ;; Skip over multiple error messages for the same source location,
    ;; so the next C-x ` won't go to an error in the same place.
    (while (and compilation-error-list
		(equal (cdr (car compilation-error-list)) (cdr next-error)))
      (setq compilation-error-list (cdr compilation-error-list)))

    ;; Lemacs change: If a new window has to be displayed, select the other
    ;; window to avoid swapping the position of the compilation error buffer.
    (and (get-buffer-window (marker-buffer (car next-error)))
	(progn
	  (select-window (get-buffer-window (marker-buffer (car next-error))))
	  (other-window -1)))
	  
    ;; We now have a marker for the position of the error.
    (switch-to-buffer (marker-buffer (cdr next-error)))
    (goto-char (cdr next-error))
    ;; If narrowing got in the way of
    ;; going to the right place, widen.
    (or (= (point) (marker-position (cdr next-error)))
	(progn
	  (widen)
	  (goto-char (cdr next-error))))

    ;; Show compilation buffer in other window, scrolled to this error.
    (let* ((pop-up-windows t)
	   (w (display-buffer (marker-buffer (car next-error)))))
      (set-window-point w (car next-error))
      (set-window-start w (car next-error)))))

;; lemacs addition
;;;###autoload
(defun previous-error (&optional argp)
  "\\[next-error] backwards."
  (interactive "P")
  (next-error (cond ((null argp) -1)
		    ((numberp argp) (- argp))
		    (t argp))))

;;;###autoload
(define-key ctl-x-map "`" 'next-error)

;; Find a buffer for file FILENAME.
;; Search the directories in compilation-search-path.
;; A nil in compilation-search-path means to try the
;; current directory, which is passed in DIR.
;; If FILENAME is not found at all, ask the user where to find it.
;; Pop up the buffer containing MARKER and scroll to MARKER if we ask the user.
(defun compilation-find-file (filename dir marker)
  (let ((dirs compilation-search-path)
	result name)
    (while (and dirs (null result))
      (setq name (expand-file-name filename (or (car dirs) dir))
	    result (and (file-exists-p name)
			(find-file-noselect name))
	    dirs (cdr dirs)))
    (or result
	;; The file doesn't exist.
	;; Ask the user where to find it.
	;; If he hits C-g, then the next time he does
	;; next-error, he'll skip past it.
	(progn
	  (let* ((pop-up-windows t)
		 (w (display-buffer (marker-buffer marker))))
	    (set-window-point w marker)
	    (set-window-start w marker))
	  (setq name
		(expand-file-name
		 (read-file-name
		  (format "Find this error in: (default %s) "
			  filename) dir filename t)))
	  (if (file-directory-p name)
	      (setq name (concat (file-name-as-directory name) filename)))
	  (if (file-exists-p name)
	      (find-file-noselect name))))))

;; Set compilation-error-list to nil, and unchain the markers that point to the
;; error messages and their text, so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection, but it is better to
;; do it right away.
(defun compilation-forget-errors ()
  (while compilation-old-error-list
    (let ((next-error (car compilation-old-error-list)))
      (set-marker (car next-error) nil)
      (if (markerp (cdr next-error))
	  (set-marker (cdr next-error) nil)))
    (setq compilation-old-error-list (cdr compilation-old-error-list)))
  (setq compilation-error-list nil
	compilation-directory-stack nil))


(defun count-regexp-groupings (regexp)
  "Return the number of \\( ... \\) groupings in REGEXP (a string)."
  (let ((groupings 0)
	(len (length regexp))
	(i 0)
	c)
    (while (< i len)
      (setq c (aref regexp i)
	    i (1+ i))
      (cond ((= c ?\[)
	     ;; Find the end of this [...].
	     (while (and (< i len)
			 (not (= (aref regexp i) ?\])))
	       (setq i (1+ i))))
	    ((= c ?\\)
	     (if (< i len)
		 (progn
		   (setq c (aref regexp i)
			 i (1+ i))
		   (if (= c ?\))
		       ;; We found the end of a grouping,
		       ;; so bump our counter.
		       (setq groupings (1+ groupings))))))))
    groupings))

(defun compilation-parse-errors (limit-search find-at-least)
  "Parse the current buffer as grep, cc or lint error messages.
See variable `compilation-parse-errors-function' for the interface it uses."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (let (;;text-buffer
	orig orig-expanded parent-expanded
	regexp enter-group leave-group error-group
	alist subexpr error-regexp-groups
	(found-desired nil)
	(compilation-num-errors-found 0))

    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(progn
	  (forward-line 2)
	  ;; Move back so point is before the newline.
	  ;; This matters because some error regexps use \n instead of ^
	  ;; to be faster.
	  (forward-char -1)))

    ;; Compile all the regexps we want to search for into one.
    (setq regexp (concat "\\(" compilation-enter-directory-regexp "\\)\\|"
			 "\\(" compilation-leave-directory-regexp "\\)\\|"
			 "\\(" (mapconcat (function
					   (lambda (elt)
					     (concat "\\(" (car elt) "\\)")))
					  compilation-error-regexp-alist
					  "\\|") "\\)"))

    ;; Find out how many \(...\) groupings are in each of the regexps, and set
    ;; *-GROUP to the grouping containing each constituent regexp (whose
    ;; subgroups will come immediately thereafter) of the big regexp we have
    ;; just constructed.
    (setq enter-group 1
	  leave-group (+ enter-group
			 (count-regexp-groupings
			  compilation-enter-directory-regexp)
			 1)
	  error-group (+ leave-group
			 (count-regexp-groupings
			  compilation-leave-directory-regexp)
			 1))

    ;; Compile an alist (IDX FILE LINE), where IDX is the number of the
    ;; subexpression for an entire error-regexp, and FILE and LINE are the
    ;; numbers for the subexpressions giving the file name and line number.
    (setq alist (or compilation-error-regexp-alist
		    (error "compilation-error-regexp-alist is empty!"))
	  subexpr (1+ error-group))
    (while alist
      (setq error-regexp-groups (cons (list subexpr
					    (+ subexpr (nth 1 (car alist)))
					    ;; lemacs change (columns)
					    (+ subexpr (nth 2 (car alist)))
					    (let ((col (nth 3 (car alist))))
					      (and col
						   (+ subexpr col))))
				      error-regexp-groups))
      (setq subexpr (+ subexpr 1 (count-regexp-groupings (car (car alist)))))
      (setq alist (cdr alist)))

    (setq orig default-directory)
    (setq orig-expanded (file-truename orig))
    (setq parent-expanded (expand-file-name "../" orig-expanded))

    (while (and (not found-desired)
		;; We don't just pass LIMIT-SEARCH to re-search-forward
		;; because we want to find matches containing LIMIT-SEARCH
		;; but which extend past it.
		(re-search-forward regexp nil t))

      ;; Figure out which constituent regexp matched.
      (cond ((match-beginning enter-group)
	     ;; The match was the enter-directory regexp.
	     (let ((dir
		    (file-name-as-directory
		     (expand-file-name
		      (buffer-substring (match-beginning (+ enter-group 1))
					(match-end (+ enter-group 1)))))))
	       ;; The directory name in the "entering" message
	       ;; is a truename.  Try to convert it to a form
	       ;; like what the user typed in.
	       (setq dir
		     (compile-abbreviate-directory dir orig orig-expanded
						   parent-expanded))
	       (setq compilation-directory-stack
		     (cons dir compilation-directory-stack))
	       (and (file-directory-p dir)
		    (setq default-directory dir))))
	    
	    ((match-beginning leave-group)
	     ;; The match was the leave-directory regexp.
	     (let ((beg (match-beginning (+ leave-group 1)))
		   (stack compilation-directory-stack))
	       (if beg
		   (let ((dir
			  (file-name-as-directory
			   (expand-file-name
			    (buffer-substring beg
					      (match-end (+ leave-group
							    1)))))))
		     ;; The directory name in the "entering" message
		     ;; is a truename.  Try to convert it to a form
		     ;; like what the user typed in.
		     (setq dir
			   (compile-abbreviate-directory dir orig orig-expanded
							 parent-expanded))
		     (while (and stack
				 (not (string-equal (car stack) dir)))
		       (setq stack (cdr stack)))))
	       (setq compilation-directory-stack (cdr stack))
	       (setq stack (car compilation-directory-stack))
	       (if stack
		   (setq default-directory stack))
	       ))
	    
	    ((match-beginning error-group)
	     ;; The match was the composite error regexp.
	     ;; Find out which individual regexp matched.
	     (setq alist error-regexp-groups)
	     (while (and alist
			 (null (match-beginning (car (car alist)))))
	       (setq alist (cdr alist)))
	     (if alist
		 (setq alist (car alist))
	       (error "compilation-parse-errors: impossible regexp match!"))
	     
	     ;; Extract the file name and line number from the error message.
	     (let ((beginning-of-match (match-beginning 0)) ;looking-at nukes
		   (filename
		    (cons default-directory
			  (buffer-substring (match-beginning (nth 1 alist))
					    (match-end (nth 1 alist)))))
		   (linenum (save-restriction
			      (narrow-to-region
			       (match-beginning (nth 2 alist))
			       (match-end (nth 2 alist)))
			      (goto-char (point-min))
			      ;; lemacs change (columns)
			      (save-match-data
				(if (looking-at "[0-9]")
				    (read (current-buffer))))
			      ))
		   ;; lemacs change (columns)
		   (column (and (nth 3 alist)
				(save-restriction
				  (narrow-to-region
				   (match-beginning (nth 3 alist))
				   (match-end (nth 3 alist)))
				  (goto-char (point-min))
				  (if (looking-at "[0-9]")
				      (read (current-buffer))))))
		   )
	       ;; Locate the erring file and line.
	       ;; Cons a new elt onto compilation-error-list,
	       ;; giving a marker for the current compilation buffer
	       ;; location, and the file and line number of the error.
	       (save-excursion
		 (beginning-of-line 1)
		 (let ((this (cons (point-marker)
				   ;; lemacs change (columns)
				   (list filename linenum column)
				   )))
		   ;; Don't add the same source line more than once.
		   (if (equal (cdr this) (cdr (car compilation-error-list)))
		       nil
		     (setq compilation-error-list
			   (cons this
				 compilation-error-list))
		     (setq compilation-num-errors-found
			   (1+ compilation-num-errors-found)))))
	       (and find-at-least (>= compilation-num-errors-found
				      find-at-least)
		    ;; We have found as many new errors as the user wants.
		    ;; We continue to parse until we have seen all
		    ;; the consecutive errors in the same file,
		    ;; so the error positions will be recorded as markers
		    ;; in this buffer that might change.
		    (cdr compilation-error-list) ; Must check at least two.
		    (not (equal (car (cdr (nth 0 compilation-error-list)))
				(car (cdr (nth 1 compilation-error-list)))))
		    (progn
		      ;; Discard the error just parsed, so that the next
		      ;; parsing run can get it and the following errors in
		      ;; the same file all at once.  If we didn't do this, we
		      ;; would have the same problem we are trying to avoid
		      ;; with the test above, just delayed until the next run!
		      (setq compilation-error-list
			    (cdr compilation-error-list))
		      (goto-char beginning-of-match)
		      (setq found-desired t)))
	       )
	     )
	    (t
	     (error "compilation-parse-errors: known groups didn't match!")))

      (message "Parsing error messages...%d (%d%% of buffer)"
	       compilation-num-errors-found
	       (/ (* 100 (point)) (point-max)))

      (and limit-search (>= (point) limit-search)
	   ;; The user wanted a specific error, and we're past it.
	   (setq found-desired t)))
    (setq compilation-parsing-end (if found-desired
				      (point)
				    ;; We have searched the whole buffer.
				    (point-max))))
  (setq compilation-error-list (nreverse compilation-error-list))
  (message "Parsing error messages...done"))

;; If directory DIR is a subdir of ORIG or of ORIG's parent,
;; return a relative name for it starting from ORIG or its parent.
;; ORIG-EXPANDED is an expanded version of ORIG.
;; PARENT-EXPANDED is an expanded version of ORIG's parent.
;; Those two args could be computed here, but we run faster by
;; having the caller compute them just once.
(defun compile-abbreviate-directory (dir orig orig-expanded parent-expanded)
  (if (and (> (length dir) (length orig-expanded))
	   (string= orig-expanded
		    (substring dir 0 (length orig-expanded))))
      (setq dir
	    (concat orig
		    (substring dir (length orig-expanded)))))
  (if (and (> (length dir) (length parent-expanded))
	   (string= parent-expanded
		    (substring dir 0 (length parent-expanded))))
    (setq dir
	  (concat (file-name-directory
		   (directory-file-name orig))
		  (substring dir (length parent-expanded)))))
  dir)


;;; lemacs menus

(defun compilation-errors-exist-p (&optional buffer)
  "Whether we are in a state where the `next-error' command will work,
that is, whether there exist (or may exist) error targets in the *compile*
or *grep* buffers."
  (or buffer
      (setq buffer (condition-case nil
			  (compilation-find-buffer)
			(error nil))))
  (and buffer
       (compilation-buffer-p buffer)
       (save-excursion
	 (set-buffer buffer)
	 ;; Has errors on the list, or needs to be parsed.
	 ;; But don't parse it now!
	 (or (not (null compilation-error-list))
	     (< compilation-parsing-end (point-max))))))

(defvar compilation-mode-menu
  '("Compilation Mode Commands"
    ["Compile..."	compile t]
    ["Kill Compilation"	kill-compilation (get-buffer-process (current-buffer))]
    ["Goto Error"	compile-goto-error	(compilation-errors-exist-p)]
    ["Next Error" 	next-error		(compilation-errors-exist-p)]
    ["Previous Error"	previous-error		(compilation-errors-exist-p)]
    ))

(defvar grep-mode-menu
  '("Grep Mode Commands"
    ["Grep..."		grep t]
    ["Kill Grep"	kill-compilation (get-buffer-process (current-buffer))]
    ["Goto Match" compile-goto-error (default-value 'compilation-error-list)]
    ["Next Match"	next-error (default-value 'compilation-error-list)]
    ["Previous Match"	previous-error (default-value 'compilation-error-list)]
    ))

(defun compile-popup-menu (e)
  "Pops the compile menu up."
  (interactive "@e")
  (let* ((grep-p (equal mode-name "grep"))
	 (submenu (mapcar #'(lambda (string)
			      (vector string
				      (list (if grep-p 'grep 'compile) string)
				      t))
			  (if grep-p grep-history compile-history)))
	 (menu (if grep-p grep-mode-menu compilation-mode-menu))
	 (name (if grep-p "Grep History" "Compile History"))
	 (existing (assoc name menu)))
    (if existing
	(setcdr existing submenu)
      (nconc menu (list (cons name submenu))))
    (popup-menu menu)))


(provide 'compile)

;;; compile.el ends here
