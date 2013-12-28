;;;; gmhist-mh.el - emulate proposed Emacs 19 builtin Minibuffer History
;;;; Id: gmhist-mh.el,v 4.8 1991/09/20 13:15:40 sk RelBeta 

;;;; This package redefines the functions
;;;;
;;;;     read-string
;;;;     completing-read
;;;;     write-region
;;;;     delete-file
;;;;     read-buffer
;;;;     read-file-name
;;;;     switch-to-buffer
;;;;
;;;; to implement the variables
;;;;
;;;;     minibuffer-history-symbol
;;;;     file-history-symbol
;;;;     buffer-history-symbol
;;;;     buffer-history-lru-order
;;;;     max-minibuffer-history-length
;;;;
;;;; and the hooks
;;;;
;;;;    after-write-region-hook
;;;;    after-delete-file-hook

(require 'gmhist)
(provide 'gmhist-mh)

(defvar max-minibuffer-history-length 'not-implemented)

;;;; Redefining basic Emacs functions

(defun gmhist-overwrite (fun)
  ;; Overwrite FUN (a symbol, the name of a function) with gmhist-new-FUN.
  ;; Save the old def of FUN in gmhist-old-FUN.
  ;; Conventions: gmhist-FUN emulates FUN, but with history.
  ;; 		  gmhist-new-FUN may take additional care of the case
  ;; 		  that history is disabled before calling gmhist-FUN
  ;; 		  to do the real work.
  (let* ((fun-name (symbol-name fun))
	 (old-name (intern (concat "gmhist-old-" fun-name)))
	 (new-name (intern (concat "gmhist-new-" fun-name))))
    (or (fboundp old-name)
	(fset old-name (symbol-function fun)))
    (fset fun new-name)))

;;; Minibuffer history (not specialized like file or buffer history)

;;; Should perhaps modify minibuffer keymaps directly:
;;; minibuffer-local-completion-map
;;; minibuffer-local-map
;;; minibuffer-local-must-match-map
;;; minibuffer-local-ns-map

(defun gmhist-new-read-string (gnrs-prompt &optional initial-input)
  "Read a string from the minibuffer, prompting with string PROMPT.
If non-nil second arg INITIAL-INPUT is a string to insert before reading.
See also `minibuffer-history-symbol'."
  (if minibuffer-history-symbol
      (gmhist-read-from-minibuffer gnrs-prompt initial-input gmhist-map)
    (gmhist-old-read-string gnrs-prompt initial-input)))

(gmhist-overwrite 'read-string)

(defun gmhist-new-completing-read
  (gncr-prompt table &optional predicate mustmatch initial)
  "Read a string in the minibuffer, with completion and history.
Args are PROMPT, TABLE, PREDICATE, REQUIRE-MATCH and INITIAL-INPUT.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray (see
  try-completion).
PREDICATE limits completion to a subset of TABLE see try-completion
  for details.
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
 the input is (or completes to) an element of TABLE.
 If it is also not t, Return does not exit if it does non-null completion.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
Case is ignored if ambient value of  completion-ignore-case  is non-nil.

*** This is the gmhist version ***
See variable `minibuffer-history-symbol'."
  (if minibuffer-history-symbol
      (gmhist-completing-read gncr-prompt table predicate mustmatch initial)
    (gmhist-old-completing-read gncr-prompt table predicate mustmatch initial)))

(gmhist-overwrite 'completing-read)

;;; File history

(defvar file-history (get file-history-symbol 'initial-hist)
  "Default history of file names read with read-file-name.
This symbol is the default value of file-history-symbol (q.v.).")

(defvar insert-file-default nil
  "*If non-nil, defaults for filenames will be inserted into the
minibuffer prompt.  This has the advantage of putting the default onto
the file-history (which see).")

(defun gmhist-new-read-file-name (gnrfn-prompt
				  &optional dir default mustmatch initial)
    "Read file name, maintaining history in value of
file-history-symbol, prompting with PROMPT, completing in directory DIR.

Value is not expanded!  You must call expand-file-name yourself.

Default name to third arg DEFAULT if user enters a null string.
\(If DEFAULT is omitted, the visited file name is used.)

Fourth arg MUSTMATCH non-nil means require existing file's name.
Non-nil and non-t means also require confirmation after completion.

Fifth arg INITIAL specifies text to start with.
DIR defaults to current buffer's default-directory.

*** This is the gmhist version ***

It differs from the original read-file-name in providing a
history of filenames in the variable whose name is the value of
file-history-symbol (usually 'file-history) (both of which see).

INITIAL defaults to default-directory's value if
insert-default-directory is non-nil.  Also, if insert-file-default is
non-nil, it inserts the DEFAULT string if no INITIAL is given, which
has the advantage of putting the default onto the file-history.
However, setting INITIAL to a string is a way for providing an
editable default, something not possible with (pre Emacs-19)
read-file-name.  Setting INITIAL and insert-default-directory to nil
will yield a basename for the file, relative to default-directory.

See function read-with-history-in for a list of properties you can put
on file-history-symbol."
    (if (null file-history-symbol)
	(gmhist-old-read-file-name gnrfn-prompt dir default mustmatch)
      (gmhist-read-file-name gnrfn-prompt dir default mustmatch
			     (if (and insert-file-default
				      (not initial))
				 default
			       initial))))

;; It is a shame that none of the standard hooks are defvar'd!
;; Also, the coexistence of `hooks' vs `hook' is annoying.
;; The singular seems to be the majority, so I'll use that.

(defvar after-write-region-hook nil
  "Run after the gmhist version of `write-region'.
The variables `start', `end', `filename', `append', `visit' are bound
around the call to the hook.")

;; Don't use &rest args, as the hook may want to take advantage of our
;; arglist.
(defun gmhist-new-write-region (start end filename
				      &optional append visit)
  "Write current region into specified file.
When called from a program, takes three arguments:
START, END and FILENAME.  START and END are buffer positions.
Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).
Optional fifth argument VISIT if t means
  set last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is neither t nor nil, it means do not print
  the \"Wrote file\" message.

*** This is the gmhist version ***
See variable `after-write-region-hook'."
  (interactive "r\nFWrite region to file: ")
  (prog1
      (gmhist-old-write-region start end filename append visit)
    (condition-case err
	;; basic-save-buffer would assume an error to mean
	;; write-region failed
	(run-hooks 'after-write-region-hook)
      (error (message "Error in after-write-region-hook %s" err)
	     (sit-for 1)))))

(defvar after-delete-file-hook nil
  "Run after the gmhist version of `delete-file'.
The hook is run with `filename' bound to the filename.")

(defun gmhist-new-delete-file (filename)
  "Delete specified file.  One argument, a file name string.
If file has multiple names, it continues to exist with the other names.

*** This is the gmhist version ***
See variable `after-delete-file-hook'."
  (interactive "fDelete file: ")
  (prog1
      (gmhist-old-delete-file filename)
    (condition-case err
	;; We don't want callers to assume an error in the hook to
	;; mean delete-file failed - or do we?
	(run-hooks 'after-delete-file-hook)
      (error (message "Error in after-delete-file-hook %s" err)
	     (sit-for 1)))))

(gmhist-overwrite 'read-file-name)
(gmhist-overwrite 'write-region)
(gmhist-overwrite 'delete-file)

;; Redefining read-file-name does not suffice as interactive "f"
;; calls the C version of read-file-name.
;; gmhist-interactive of gmhist.el,v 4.4 and later understands the
;; indirection from file-history-symbol to 'file-history (or whatever
;; the current value may be).
(gmhist-make-magic 'find-file              'file-history-symbol)
(gmhist-make-magic 'find-file-other-window 'file-history-symbol)
(gmhist-make-magic 'find-file-read-only    'file-history-symbol)
(gmhist-make-magic 'insert-file            'file-history-symbol)
(gmhist-make-magic 'load-file              'file-history-symbol)
(gmhist-make-magic 'set-visited-file-name  'file-history-symbol)
(gmhist-make-magic 'append-to-file         'file-history-symbol)
;; write-region is wrapped by gmhist, no longer a subr, thus this works:
(gmhist-make-magic 'write-region           'file-history-symbol)
;; ditto for delete-file:
(gmhist-make-magic 'delete-file            'file-history-symbol)
(if gmhist-emacs-19-p
    ;; In Emacs 19, these call the redefined read-file-name inside
    ;; interactive, so we don't need to do anything
    nil
  (gmhist-make-magic 'write-file           'file-history-symbol)
  (gmhist-make-magic 'find-alternate-file  'file-history-symbol))


;;; Buffer history

(defvar buffer-history-lru-order nil
  "*If non-nil, the buffer history will be the complete buffer
list in most recently used order (as returned by buffer-list).

Usually, the buffer history is in the order entered using read-buffer.")

(defvar buffer-history (get 'buffer-history 'initial-hist)
  "History of all buffer names read with read-buffer.")

(defun gmhist-new-read-buffer (gnrb-prompt &optional default existing)
  "One arg PROMPT, a string.  Read the name of a buffer and return as a string.
Prompts with PROMPT.
Optional second arg is value to return if user enters an empty line.
If optional third arg REQUIRE-MATCH is non-nil, only existing buffer names are allowed.

*** This is the gmhist version ***

See variables `buffer-history-symbol' and `buffer-history-lru-order'."
  (if (and buffer-history-symbol
	   buffer-history-lru-order)
      (set buffer-history-symbol
	   (mapcar (function buffer-name) (buffer-list))))
  (gmhist-read-buffer gnrb-prompt default existing))

(defun gmhist-new-switch-to-buffer (buffer &optional norecord)
  "Select buffer BUFFER in the current window.
BUFFER may be a buffer or a buffer name.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones.

WARNING: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead.  That avoids messing with
the window-buffer correspondences.

*** This is the gmhist version ***

It adds buffer-history to switch-to-buffer."
  (interactive
   ;; should perhaps bypass gmhist if NORECORD is given?
   (list (gmhist-new-read-buffer "Switch to buffer: " (other-buffer) nil)))
  (gmhist-old-switch-to-buffer buffer norecord))

(gmhist-overwrite 'read-buffer)
;; switch-to-buffer is a subr:
(gmhist-overwrite 'switch-to-buffer)
;; Redefining read-buffer does not suffice as interactive "b"
;; calls the C version of read-buffer.
;; gmhist-interactive of gmhist.el,v 4.4 and later understands the
;; indirection from buffer-history-symbol to 'buffer-history (or
;; whatever the current value may be).
(mapcar (function (lambda (fun)
		    (gmhist-make-magic fun 'buffer-history-symbol)))
	'(switch-to-buffer-other-window ; files.el
	  append-to-buffer		; the rest from simple.el
	  prepend-to-buffer
	  copy-to-buffer))


;;; read-from-minibuffer
;;; saved and defined in gmhist.el, just need to overwrite:

(fset 'read-from-minibuffer 'gmhist-new-read-from-minibuffer)

;; Now that we've redefined read-from-minibuffer we need to make sure
;; that repeat-complex-command (C-x ESC), which calls
;; read-from-minibuffer, adds the command to command-history and not
;; to the ambient value of minibuffer-history-symbol.  The latter
;; could be confusing if e.g. inside a C-x C-f a C-x ESC is done (with
;; enable-recursive-minibuffers t): it would add a command to the
;; file-history.

;(defun repeat-complex-command (repeat-complex-command-arg)
;  "Edit and re-evaluate last complex command, or ARGth from last.
;A complex command is one which used the minibuffer.
;The command is placed in the minibuffer as a Lisp form for editing.
;The result is executed, repeating the command as changed.
;If the command has been changed or is not the most recent previous command
;it is added to the front of the command history.
;Whilst editing the command, the following commands are available:
;\\{repeat-complex-command-map}"
;  (interactive "p")
;  (let ((elt (nth (1- repeat-complex-command-arg) command-history))
;	newcmd)
;    (if elt
;	(progn
;	  (setq newcmd
;		(let ((minibuffer-history-symbol nil))
;		  ;; Don't let gmhist interfere with command-history.
;		  ;; command-history is special because it's builtin to M-x.
;		  ;; Also, gmhist would store commands as strings, not
;		  ;; as s-exprs.
;		  ;; When gmhist is implemented in C, M-x must be
;		  ;; fixed to store strings, too.
;		  (read-from-minibuffer "Redo: "
;					(prin1-to-string elt)
;					repeat-complex-command-map
;					t)))
;	  ;; If command to be redone does not match front of history,
;	  ;; add it to the history.
;	  (or (equal newcmd (car command-history))
;	      (setq command-history (cons newcmd command-history)))
;	  (eval newcmd))
;      (ding))))

;; Actually, it's easier to just use the gmhist re-implementation instead
(define-key ctl-x-map "\e" 'gmhist-repeat-complex-command)

(defun gmhist-repeat-complex-command (arg) ; C-x ESC
  ;; This function from Mike Williams <Mike.Williams@comp.vuw.ac.nz>
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history."
  (interactive "p")
  (let ((print-escape-newlines t))
    (put 'command-history 'backup arg)
    (put 'command-history 'cursor-end t)
    (eval (read-with-history-in 'command-history "Redo: " nil 'lisp))
    (put 'command-history 'backup nil)))

;; TODO:
;; read-minibuffer
;; eval-minibuffer
;; read-no-blanks-input
;; read-command
;; read-variable
