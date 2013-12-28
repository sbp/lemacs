;;;; gmhist-app.el - applications of gmhist for some standard commands
;;;; Id: gmhist-app.el,v 4.16 1992/02/26 14:32:27 sk RelBeta 

;;;; The following commands are redefined to get history:
;;;;     keep-lines
;;;;     flush-lines
;;;;     how-many
;;;;     occur
;;;;				=> regexp-history
;;;;     grep			=> grep-history
;;;;     shell-command
;;;;     shell-command-on-region
;;;;                            => shell-history
;;;;     eval-expression	=> eval-expression-history
;;;;     compile		=> compile-history

;;;; You probably want to establish this key binding in your ~/.emacs,
;;;; it will make `M-x M-p' equivalent to `C-x ESC':

;;;; (define-key esc-map "x" 'gmhist-execute-extended-command)
;;;; (define-key esc-map "X" 'execute-extended-command) ; save old M-x command

;;;; The second line is to save the old M-x command under M-X, just in
;;;; case anything goes wrong.

(require 'gmhist)

;;; gmhist modifications for replace.el (preloaded).

(mapcar '(lambda (x)
	   (gmhist-make-magic x 'regexp-history))
	(if gmhist-emacs-19-p
	    '(keep-lines flush-lines how-many)
	  '(keep-lines flush-lines how-many occur)))


(if gmhist-emacs-19-p
    (progn
      (gmhist-replace-spec
       'occur
       '(gmhist-interactive "sList lines matching regexp: \nP"
			    'regexp-history))
      (gmhist-replace-spec
       'grep
       '(list (read-with-history-in
	       'grep-history		; or 'regexp-history?
	       (concat "Run "
		       (substring grep-command 0
				  (string-match "[\t ]+" grep-command))
		       " (with args): ")
	       ))))
  ;; else
  (gmhist-make-magic 'grep 'grep-history))

;;; gmhist modification for simple.el (is preloaded)

(if gmhist-emacs-19-p
    (progn
      (gmhist-replace-spec
       'shell-command
       '(gmhist-interactive "sShell command: \nP" 'shell-history))
      (gmhist-replace-spec
       'shell-command-on-region
       '(gmhist-interactive "r\nsShell command on region: \nP\np"
			    'shell-history))
      )
  (gmhist-make-magic 'shell-command 'shell-history)
  (gmhist-make-magic 'shell-command-on-region 'shell-history)
  )
(gmhist-make-magic 'eval-expression)

;;; gmhist modification for compile.el (autoloaded)

;; Often people make the variable compile-command buffer-local.
;;
;; Instead of compile-command, you now have compile-history, which is
;; initialized to
;; 
;;     (list compile-command)
;; 
;; but afterwards gmhist ignores compile-command.  So your old file
;; local variable sections or mode hooks will cease to work.
;;
;; Here is a solution: Make compile-history instead of compile-command
;; buffer-local (in a local var section of a file or in a hook, using
;; function make-local-variable).  If you only sometimes have gmhist
;; loaded, make both variables buffer-local.

;; (gmhist-make-magic 'compile 'compile-history) won't work because
;; the interactive spec is not a string.  Instead, hand-craft it:

(gmhist-replace-spec
 'compile
 '(list
   (read-with-history-in 'compile-history "Compile command: ")))
;; instead of...
;;(put 'compile-history 'default compile-command)
;; ... do the following
(put 'compile-history 'backup t)	; requires at least gmhist 3.22
(put 'compile-history 'no-default t)
(put 'compile-history 'initial-hist (list compile-command))
(put 'compile-history 'cursor-end t)

;;; gmhist modifications for tags.el (is autoloaded)
;;; The distributed version of tags.el does not support a load hook.
;;; Add the statement
;;;     (run-hooks 'tags-load-hook)
;;; at the very end of tags.el.

(defvar tags-history nil
  "History of tags.")

(setq tags-load-hook
      ;; redefine find-tag-tag upon loading of tags.el
      '(lambda ()
	 (fset 'find-tag-tag 'gmhist-find-tag-tag)))

(defun gmhist-find-tag-tag (string)
  ;; compare these two lines to the original definition...
  (let ((defalt (find-tag-default)))
    (if (and defalt
	     (string-match "[:']$" defalt))
	(setq defalt (substring defalt 0 -1)))
    (put 'tags-history 'default defalt)
    ;; so that M-p lets you edit the default
    (setq tags-history (cons defalt tags-history))
    (list (read-with-history-in 'tags-history string))))

;; Gmhist version of M-x

;; Make M-x have history (it actually has one already, but only through
;; C-x ESC (repeat-complex-command), not via M-p within the M-x
;; prompt.)

;; execute-extended-command must be rewritten if minibuffer history is
;; implemented in C.  Probably call-interactively too.

(defvar gmhist-execute-extended-command-map (copy-keymap gmhist-completion-map)
  "Keymap used inside `gmhist-execute-extended-command'.")

;; We have to define custom version of RET and SPC (actually TAB as
;; well) since they behave completely different immediately after M-x
;; (reading a command) or after the history postion has been changed
;; to a non-zero value (editing an s-expr, an old command with its
;; arguments).

(define-key gmhist-execute-extended-command-map
  "\r" 'gmhist-execute-extended-command-exit)

(define-key gmhist-execute-extended-command-map
  " " 'gmhist-execute-extended-command-space)

(defun gmhist-execute-extended-command-exit ()
  "Maybe complete the minibuffer contents, and exit.
Completes commands before exiting, but leaves command history items alone."
  ;; Completion (over the set of commands) only occurs if
  ;; minibufer-history-position is 0, meaning we are editing a command
  ;; name.  Non-zero history positions mean we are editing an sexp
  ;; resulting from an earlier command and its argument, and
  ;; completion is not meaningful.
  (interactive)
  (if (equal 0 minibuffer-history-position)
      ;; Rather than calling minibuffer-complete-and-exit directly,
      ;; account for the possibility that e.g. a partial completion
      ;; has been loaded and changed the bindings
      (funcall (lookup-key minibuffer-local-must-match-map "\C-m"))	
    (exit-minibuffer)))

(defun gmhist-execute-extended-command-space ()
  (interactive)
  (if (equal 0 minibuffer-history-position)
        (funcall (lookup-key minibuffer-local-must-match-map " "))	
    (insert " ")))

(defun gmhist-execute-extended-command () ; M-x
  "Read function name, then read its arguments and call it.
You can use all gmhist commands (see variable gmhist-completion-map),
especially \\<gmhist-completion-map>\\[gmhist-previous] to backup in command-history."
  (interactive)
  ;; We don't want '(gmhist-execute-extended-command (quote COMMAND))
  ;; on the command history, since this is ugly, and COMMAND itself is
  ;; always right next to it.  This is so because
  ;; gmhist-execute-extended-command is not a builtin like
  ;; execute-extended-command and thus is itself entered on the
  ;; command-history.
  (if (assq 'gmhist-execute-extended-command command-history)
      (let ((list command-history)
	    elt)
	(while list
	  (setq elt (car list))
	  (if (eq (car-safe elt) 'gmhist-execute-extended-command)
	      ;; destructively remove this elt from command-history
	      (progn
		(setcar list nil)
		;; and exit the loop since if we're doing this each time
		;; there shouldn't be more than one such elt - the one
		;; from the last time
		(setq list nil))
	    (setq list (cdr list))))
	(setq command-history (delq nil command-history))))
  (let (cmd)
    (let ((minibuffer-completion-confirm nil)
	  ;; We only need read-with-history-in here to make M-p available,
	  ;; the new command will be recorded below
	  (minibuffer-history-read-only t))
      (put 'command-history 'cursor-end t)
      ;; command-history is maintained automatically:
      (put 'command-history 'hist-ignore ".*")
      (put 'command-history 'no-default t)
      (put 'command-history 'completion-table obarray)
      (put 'command-history 'hist-map gmhist-execute-extended-command-map)
      (put 'command-history 'completion-predicate 'commandp)
      (put 'command-history 'backup nil)
      (setq cmd
	    (read-with-history-in
	     'command-history
	     (if current-prefix-arg
		 (format "%s M-x "
			 current-prefix-arg
			 ;; this is not exactly like the original M-x
			 ;; but the following doesn't seem to work right
;			 (cond ((eq '(4) current-prefix-arg)
;				"C-u")
;			       (t
;				(prefix-numeric-value current-prefix-arg)))
			 )
	       "M-x ")
				  nil t)))
    (if (commandp cmd)
	(let ((prefix-arg current-prefix-arg))
	  (setq this-command cmd)
	  (command-execute cmd t))
      ;; else it is a lisp form from the history of old commands
      (prog1
	  (eval cmd)
	(setq command-history (cons cmd command-history))))))

