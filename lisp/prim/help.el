;; Help commands for Emacs
;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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


(defvar help-map (let ((map (make-sparse-keymap)))
                   (set-keymap-name map 'help-map)
                   (set-keymap-prompt map (gettext "(Type ? for further options)"))
                   map)
  "Keymap for characters following the Help key.")

(fset 'help-command help-map)

(define-key help-map '(control h) 'help-for-help)
(define-key help-map "?" 'help-for-help)

(define-key help-map "\C-c" 'describe-copying)
(define-key help-map "\C-d" 'describe-distribution)
(define-key help-map "\C-w" 'describe-no-warranty)
(define-key help-map "a" 'command-apropos)

(define-key help-map "b" 'describe-bindings)
(define-key help-map "p" 'describe-pointer)

(define-key help-map "c" 'describe-key-briefly)
(define-key help-map "k" 'describe-key)

(define-key help-map "d" 'describe-function)
(define-key help-map "e" 'describe-last-error)
(define-key help-map "f" 'describe-function)

(define-key help-map "i" 'info)

(define-key help-map "l" 'view-lossage)

(define-key help-map "m" 'describe-mode)

(define-key help-map "\C-n" 'view-emacs-news)
(define-key help-map "n" 'view-emacs-news)

(define-key help-map "s" 'describe-syntax)

(define-key help-map "t" 'help-with-tutorial)

(define-key help-map "w" 'where-is)

(define-key help-map "v" 'describe-variable)

(if (fboundp 'view-last-error)
    (define-key help-map "e" 'view-last-error))

(define-key help-map '(control i) 'Info-query)
(define-key help-map '(control k) 'Info-emacs-key)
(define-key help-map '(control f) 'Info-elisp-ref)

;; This is a grody hack of the same genotype as `advertised-undo'; if the
;; bindings of Backspace and C-h are the same, we want the menubar to claim
;; that `info' in invoked with `C-h i', not `BS i'.

(defun deprecated-help-command ()
  (interactive)
  (if (eq 'help-command (key-binding "\C-h"))
      (setq unread-command-event (character-to-event ?\C-h))
    (help-for-help)))

;;(define-key global-map 'backspace 'deprecated-help-command)

(defun help-with-tutorial ()
  "Select the Emacs learn-by-doing tutorial."
  (interactive)
  (let ((file (expand-file-name "~/TUTORIAL")))
    (delete-other-windows)
    (if (get-file-buffer file)
	(switch-to-buffer (get-file-buffer file))
      (switch-to-buffer (create-file-buffer file))
      (setq buffer-file-name file)
      (setq default-directory (expand-file-name "~/"))
      (setq buffer-auto-save-file-name nil)
      (insert-file-contents (expand-file-name "TUTORIAL" data-directory))
      (goto-char (point-min))
      (search-forward "\n<<")
      (beginning-of-line)
      (delete-region (point) (progn (end-of-line) (point)))
      (newline (- (window-height (selected-window))
		  (count-lines (point-min) (point))
		  6))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))

(defun describe-key-briefly (key)
  "Print the name of the function KEY invokes.  KEY is a string."
  (interactive "kDescribe key briefly: ")
  (let (defn
	 (menup nil))
    ;; If the key typed was really a menu selection, grab the form out
    ;; of the event object and intuit the function that would be called,
    ;; and describe that instead.
    (if (and (vectorp key) (= 1 (length key))
	     (or (menu-event-p (aref key 0))
		 (eq (car-safe (aref key 0)) 'menu-selection)))
	(let ((event (aref key 0)))
	  (setq defn (if (eventp event)
			 (list (event-function event) (event-object event))
		       (cdr event)))
	  (setq menup t)
	  (if (eq (car defn) 'eval)
	      (setq defn (car (cdr defn))))
	  (if (eq (car-safe defn) 'call-interactively)
	      (setq defn (car (cdr defn))))
	  (if (and (consp defn) (null (cdr defn)))
	      (setq defn (car defn))))
      ;; else
      (setq defn (key-binding key)))
    (if (or (null defn) (integerp defn))
        (message (gettext "%s is undefined") (key-description key))
      ;; If it's a keyboard macro which trivially invokes another command,
      ;; document that instead.
      (if (or (stringp defn) (vectorp defn))
	  (setq defn (or (key-binding defn)
			 defn)))
      (message (gettext "%s runs the command %s")
	       (if menup "This menu item" (key-description key))
	       (if (symbolp defn) defn (prin1-to-string defn))))))

(defun print-help-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
Computes a message and applies the optional argument FUNCTION to it.
If FUNCTION is nil, applies `message' to the message, thus printing it."
  (and (not (get-buffer-window standard-output))
       (funcall
	(or function 'message)
	(concat
	 (substitute-command-keys
	  (if (one-window-p t)
	      (if pop-up-windows
		  (gettext "Type \\[delete-other-windows] to remove help window.")
		(gettext "Type \\[switch-to-buffer] RET to remove help window."))
   (gettext "Type \\[switch-to-buffer-other-window] RET to restore the other window."))) 
	 (substitute-command-keys
	  (gettext "  \\[scroll-other-window] to scroll the help."))))))

(defun describe-key (key)
  "Display documentation of the function invoked by KEY.
KEY is a string, or vector of events.
When called interactvely, KEY may also be a menu selection."
  (interactive "kDescribe key: ")
  (let (defn)
    ;; If the key typed was really a menu selection, grab the form out
    ;; of the event object and intuit the function that would be called,
    ;; and describe that instead.
    (if (and (vectorp key) (= 1 (length key))
	     (or (menu-event-p (aref key 0))
		 (eq (car-safe (aref key 0)) 'menu-selection)))
	(let ((event (aref key 0)))
	  (setq defn (if (eventp event)
			 (list (event-function event) (event-object event))
		       (cdr event)))
	  (if (eq (car defn) 'eval)
	      (setq defn (car (cdr defn))))
	  (if (eq (car-safe defn) 'call-interactively)
	      (setq defn (car (cdr defn))))
	  (if (and (consp defn) (null (cdr defn)))
	      (setq defn (car defn))))
      ;; else
      (setq defn (key-binding key)))
    (if (or (null defn) (integerp defn))
        (message (gettext "%s is undefined") (key-description key))
      (with-output-to-temp-buffer (gettext "*Help*")
;	(princ (key-description key))
;	(princ " runs the command ")
	(prin1 defn)
	(princ ":\n")
	(cond
	 ((or (stringp defn) (vectorp defn))
	  (let ((cmd (key-binding defn)))
	    (if (not cmd)
		(princ (gettext "a keyboard macro"))
	      (progn
		(princ (format
			(gettext "a keyboard macro which runs the command %s:\n\n")
			cmd))
		(princ cmd)
		(if (documentation cmd) (princ (documentation cmd)))))))
	 ((and (consp defn) (not (eq 'lambda (car-safe defn))))
	  (princ "\n")
	  (let ((describe-function-show-arglist nil))
	    (describe-function-1 (car defn) standard-output)))
	 ((documentation defn)
	  (princ (documentation defn)))
	 (t
	  (princ (gettext "not documented"))))
	(print-help-return-message)))))


(defun where-is (definition)
  "Print message listing key sequences that invoke specified command.
Argument is a command definition, usually a symbol with a function definition."
  (interactive "CWhere is command: ")
  (let ((keys (where-is-internal definition (current-local-map) nil nil nil)))
    (if keys
	(message (gettext "%s is on %s") definition
	  (mapconcat 'key-description
		     (sort keys #'(lambda (x y) (< (length x) (length y))))
		     ", "))
      (message (gettext "%s is not on any keys") definition)))
  nil)


(defun describe-mode (&optional minor)
  "Display documentation of current major mode.
If optional MINOR is non-nil (or prefix argument is given if interactive),
display documentation of active minor modes as well.
For this to work correctly for a minor mode, the mode's indicator variable
\(listed in `minor-mode-alist') must also be a function whose documentation
describes the minor mode."
  (interactive)
  (with-output-to-temp-buffer (gettext "*Help*")
    (princ mode-name)
    (princ (gettext " Mode:\n"))
    (princ (documentation major-mode))
    (let ((minor-modes (if minor minor-mode-alist '()))
	  (locals (buffer-local-variables)))
      (while minor-modes
	(let* ((minor-mode (car (car minor-modes)))
	       (indicator (car (cdr (car minor-modes))))
	       (local-binding (assq minor-mode locals)))
	  ;; Document a minor mode if it is listed in minor-mode-alist,
	  ;; bound locally in this buffer, non-nil, and has a function
	  ;; definition.
	  (if (and local-binding
		   (cdr local-binding)
		   (fboundp minor-mode))
	      (progn
		(princ (format (gettext "\n\n\n%s minor mode (indicator%s):\n")
			       minor-mode indicator))
		(princ (documentation minor-mode)))))
	(setq minor-modes (cdr minor-modes))))
    (print-help-return-message)))

(defun describe-distribution ()
  "Display info on how to obtain the latest version of GNU Emacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "DISTRIB" data-directory)))

(defun describe-copying ()
  "Display info on how you may redistribute copies of GNU Emacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "COPYING" data-directory))
  (goto-char (point-min)))

(defun describe-pointer ()
  "Show a list of all defined mouse buttons, and their definitions.
This is the same as \\[universal-argument] \\[describe-bindings]."
  (interactive)
  (describe-bindings t))

(defun describe-no-warranty ()
  "Display info on all the kinds of warranty Emacs does NOT have."
  (interactive)
  (describe-copying)
  (let (case-fold-search)
    (search-forward "NO WARRANTY")
    (recenter 0)))

(defun view-emacs-news ()
  "Display info on recent changes to Emacs."
  (interactive)
  (find-file-read-only (expand-file-name "NEWS" data-directory)))

(defun view-lossage ()
  "Display last 100 input keystrokes."
  (interactive)
  (with-output-to-temp-buffer (gettext "*Help*")
    (princ (key-description (recent-keys)))
    (save-excursion
      (set-buffer standard-output)
      (goto-char (point-min))
      (while (progn (move-to-column 50) (not (eobp)))
	(search-forward " " nil t)
	(insert "\n")))
    (print-help-return-message)))

(defun help-for-help ()
  "You have typed \\[help-for-help], the help character.  Type a Help option:

A  command-apropos.   Give a substring, and see a list of commands
              (functions interactively callable) that contain
	      that substring.  See also the  apropos  command.
B  describe-bindings.  Display table of all key bindings.
C  describe-key-briefly.  Type a command key sequence;
	      it prints the function name that sequence runs.
F  describe-function.  Type a function name and get documentation of it.
I  info. The  info  documentation reader.
K  describe-key.  Type a command key sequence;
	      it displays the full documentation.
L  view-lossage.  Shows last 100 characters you typed.
M  describe-mode.  Print documentation of current major mode,
	      which describes the commands peculiar to it.
N  view-emacs-news.  Shows emacs news file.
P  describe-pointer.  Display table of all mouse-button bindings.
S  describe-syntax.  Display contents of syntax table, plus explanations
T  help-with-tutorial.  Select the Emacs learn-by-doing tutorial.
V  describe-variable.  Type name of a variable;
	      it displays the variable's documentation and value.
W  where-is.  Type command name; it prints which keystrokes
	      invoke that command.
C-c print Emacs copying permission (General Public License).
C-d print Emacs ordering information.
C-n print news of recent Emacs changes.
C-w print information on absence of warranty for GNU Emacs.

C-i Info-query.  Info reader, prompt for topic name.
C-k  Info-emacs-key.  Look up a key in Emacs manual.
C-f  Info-elisp-ref.  Look up a function in Emacs Lisp manual."
  (interactive)
  (let ((help-key (copy-event last-command-event))
	event char)
    (message
  (gettext "A B C F I K L M N P S T V W C-c C-d C-n C-w.  Type %s again for more help: ")
  ;; arrgh, no room for "C-i C-k C-f" !!
     (single-key-description help-key))
    (setq event (next-command-event)
	  char (or (event-to-character event) event))
    (if (or (equal char help-key) (equal char ?\C-h) (equal char ??))
	(save-window-excursion
	  (switch-to-buffer (gettext "*Help*"))
	  (delete-other-windows)
	  (erase-buffer)
	  (insert (documentation 'help-for-help))
	  (goto-char (point-min))
	  (while (or (equal char help-key)
		     (memq char '(?\C-h ?? ?\C-v ?\ ?\177 ?\M-v)))
	    (if (memq char '(?\C-v ?\ ))
		(scroll-up))
	    (if (memq char '(?\177 ?\M-v))
		(scroll-down))
	    (message (if (pos-visible-in-window-p (point-max))
			 (gettext "A B C F I K L M N P S T V W C-c C-d C-n C-w C-i C-k C-f: ")
		       (gettext "A B C F I K L M N P S T V W C-c C-d C-n C-w C-i C-k C-f or Space to scroll: ")))
	    (let ((cursor-in-echo-area t))
	      (setq event (next-command-event event)
		    char (or (event-to-character event) event))))))
    (let ((defn (or (lookup-key help-map (vector event))
		    (and (numberp char)
			 (lookup-key help-map
				     (make-string 1 (downcase char)))))))
      (message nil)
      (if defn
	  (call-interactively defn)
	(ding)))))

;; Return a function which is called by the list containing point.
;; If that gives no function, return a function whose name is around point.
;; If that doesn't give a function, return nil.
(defun function-called-at-point ()
  (or (condition-case ()
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	      (backward-up-list 1)
	      (forward-char 1)
	      (let (obj)
		(setq obj (read (current-buffer)))
		(and (symbolp obj) (fboundp obj) obj))))
	(error nil))
      (condition-case ()
	  (save-excursion
	    (forward-sexp -1)
	    (skip-chars-forward "'")
	    (let ((obj (read (current-buffer))))
	      (and (symbolp obj) (fboundp obj) obj)))
	(error nil))))

(defvar describe-function-show-arglist t  ; default to nil for the non-hackers?
  "*If true, then describe-function will show its arglist if the function is
not an autoload.")


(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
    (let* ((fn (function-called-at-point))
           (val (let ((enable-recursive-minibuffers t))
                  (completing-read
                    (if fn 
                        (format (gettext "Describe function (default %s): ") fn)
                        (gettext "Describe function: "))
                    obarray 'fboundp t))))
      (list (if (equal val "") fn (intern val)))))
  (with-output-to-temp-buffer (gettext "*Help*")
    (describe-function-1 function standard-output)
    (print-help-return-message)
    (save-excursion (set-buffer standard-output) (buffer-string))))

(defun describe-function-1 (function stream &optional nodoc)
  (prin1 function stream)
  (princ ": " stream)
  (let* ((def function)
         (doc (or (documentation function)
                  (gettext "not documented")))
	 aliases kbd-macro-p fndef macrop)
    (while (symbolp def)
      (or (eq def function)
	  (if aliases
	      (setq aliases (concat aliases 
				    (format (gettext "\n     which is an alias for %s, ")
					    (symbol-name def))))
	    (setq aliases (format (gettext "an alias for %s, ") (symbol-name def)))))
      (setq def (symbol-function def)))
    (if (eq 'macro (car-safe def))
	(setq fndef (cdr def)
	      macrop t)
      (setq fndef def))
    (if describe-function-show-arglist
        (if (cond ((eq 'autoload (car-safe fndef))
                   nil)
                  ((eq 'lambda (car-safe fndef))
                   (princ (or (nth 1 fndef) "()") stream)
                   t)
                  ((compiled-function-p fndef)
                   (princ (or (aref fndef 0) "()") stream)
                   t)
                  ((and (subrp fndef)
                        (string-match "[\n\t ]*\narguments: ?\\((.*)\\)\n?\\'"
                                      doc))
                   (princ (substring doc (match-beginning 1) (match-end 1))
                          stream)
                   (setq doc (substring doc 0 (match-beginning 0)))
                   t)
                  (t
                   nil))
            (princ "\n  -- " stream)))
    (if aliases (princ aliases stream))
    (let ((int #'(lambda (string)
		   (princ (format (if (commandp def)
				      (gettext "an interactive %s")
				    (gettext "a %s"))
				  string)
			  stream))))
      (cond ((or (stringp def) (vectorp def))
             (princ (gettext "a keyboard macro.") stream)
	     (setq kbd-macro-p t))
            ((subrp fndef)
             (funcall int (if macrop
			      (gettext "built-in macro.")
			    (gettext "built-in function."))))
            ((compiled-function-p fndef)
             (funcall int (if macrop (gettext "compiled Lisp macro.")
			    (gettext "compiled Lisp function."))))
            ((symbolp fndef)
             (princ (format (gettext "alias for `%s'.")
			    (prin1-to-string def)) stream))
            ((eq (car-safe fndef) 'lambda)
             (funcall int (if macrop (gettext "Lisp macro.")
			    (gettext "Lisp function."))))
            ((eq (car-safe fndef) 'mocklisp)
             (princ (if macrop (gettext "a mocklisp macro.")
		      (gettext "a mocklisp function."))
		    stream))
            ((eq (car-safe def) 'autoload)
	     (if (elt def 4)
		 (funcall int (gettext "autoloaded Lisp macro"))
	       (funcall int (gettext "autoloaded Lisp function")))
	     (princ (format (gettext "\n  -- loads from \"%s\"") (elt def 1)) stream))
            (t
             nil)))
    (terpri)
    (cond (kbd-macro-p
	   (princ (gettext "These characters are executed:\n\n\t") stream)
	   (princ (key-description def) stream)
	   (cond ((setq def (key-binding def))
		  (princ (format (gettext "\n\nwhich executes the command %s.\n\n") def) stream)
		  (describe-function-1 def stream))))
	  (nodoc nil)
	  (t
	   (princ doc stream)))))


(defun describe-function-arglist (function)
  (interactive (list (or (function-called-at-point)
			 (error "no function call at point"))))
  (let ((b nil))
    (unwind-protect
	(save-excursion
	  (set-buffer (setq b (get-buffer-create " *arglist*")))
	  (buffer-disable-undo b)
	  (erase-buffer)
	  (describe-function-1 function b t)
	  (goto-char (point-min))
	  (end-of-line)
	  (or (eobp) (delete-char 1))
	  (just-one-space)
	  (end-of-line)
	  (message (buffer-substring (point-min) (point))))
      (and b (kill-buffer b)))))


(defun variable-at-point ()
  (condition-case ()
      (save-excursion
	(forward-sexp -1)
	(skip-chars-forward "'")
	(let ((obj (read (current-buffer))))
	  (and (symbolp obj) (boundp obj) obj)))
    (error nil)))

(defun describe-variable (variable)
  "Display the full documentation of VARIABLE (a symbol)."
  (interactive 
   (let* ((v (variable-at-point))
          (val (let ((enable-recursive-minibuffers t))
                 (completing-read
                   (if v
                       (format (gettext "Describe variable (default %s): ") v)
                       (gettext "Describe variable: "))
                   obarray 'boundp t))))
     (list (if (equal val "") v (intern val)))))
  (with-output-to-temp-buffer (gettext "*Help*")
    (princ (format (gettext "%s's value is ") variable))
    (if (not (boundp variable))
        (princ (gettext "void."))
      (prin1 (symbol-value variable)))
    (terpri) (terpri)
    (princ (gettext "Documentation:"))
    (terpri)
    (let ((doc (documentation-property variable 'variable-documentation)))
      (if doc
	  ;; note: documentation-property calls substitute-command-keys.
	  (princ doc)
	(princ (gettext "not documented as a variable."))))
    (print-help-return-message)
    ;; Return the text we displayed.
    (save-excursion (set-buffer standard-output) (buffer-string))))

(defun command-apropos (string)
  "Like apropos but lists only symbols that are names of commands
\(interactively callable functions).  Argument REGEXP is a regular expression
that is matched against command symbol names.  Returns list of symbols and
documentation found."
  (interactive "sCommand apropos (regexp): ")
  (let ((message
	 (let ((standard-output (get-buffer-create (gettext "*Help*"))))
	   (print-help-return-message 'identity))))
    (apropos string t 'commandp)
    (and message (message message))))

(defun locate-library (library &optional nosuffix)
  "Show the full path name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY (a la calling `load' instead of `load-library')."
  (interactive "sLocate library: \nP")
  (let ((file (locate-file library load-path (if nosuffix nil ".elc:.el:"))))
    (if file
	(message (gettext "Library is file %s") file)
      (message (gettext "No library %s in search path") library))
    file))

(defun describe-syntax ()
  "Describe the syntax specifications in the syntax table.
The descriptions are inserted in a buffer, which is then displayed."
  (interactive)
  (with-output-to-temp-buffer (gettext "*Help*")
    ;; defined in syntax.el
    (describe-syntax-table (syntax-table) standard-output)))

(defun list-processes ()
  "Display a list of all processes.
\(Any processes listed as Exited or Signaled are actually eliminated
after the listing is made.)"
  (interactive)
  (with-output-to-temp-buffer (gettext "*Process List*")
    (set-buffer standard-output)
    (buffer-disable-undo standard-output)
    (make-local-variable 'truncate-lines)
    (setq truncate-lines t)
    (let ((stream standard-output))
      ;;      00000000001111111111222222222233333333334444444444
      ;;      01234567890123456789012345678901234567890123456789
      (princ (gettext "Proc         Status   Buffer         Command\n") stream)
      (princ (gettext "----         ------   ------         -------\n") stream)
      (let ((tail (process-list)))
        (while tail
          (let* ((p (car tail))
                 (pid (process-id p))
                 (s (process-status p)))
            (setq tail (cdr tail))
            (princ (format "%-13s" (process-name p)) stream)
            ;(if (and (eq system-type 'vax-vms)
            ;         (eq s 'signal)
            ;         (< (process-exit-status p) NSIG))
            ;    (princ (aref sys_errlist (process-exit-status p)) stream))
            (princ s stream)
            (if (and (eq s 'exit) (/= (process-exit-status p) 0))
                (princ (format " %d" (process-exit-status p)) stream))
            (if (memq s '(signal exit closed))
                ;; Do delete-exited-processes' work
                (delete-process p))
            (indent-to 22 1)            ;>>>
            (let ((b (process-buffer p)))
              (cond ((not b)
                     (princ (gettext "(none)") stream))
                    ((not (buffer-name b))
                     (princ (gettext "(killed)") stream))
                    (t
                     (princ (buffer-name b) stream))))
            (indent-to 37 1)            ;>>>
            (if (not (integerp pid))
                (progn
                  (princ (gettext "network stream connection ") stream)
                  (princ (car pid) stream)
                  (princ "@" stream)
                  (princ (cdr pid) stream))
                (let ((cmd (process-command p)))
                  (while cmd
                    (princ (car cmd) stream)
                    (setq cmd (cdr cmd))
                    (if cmd (princ " " stream)))))
            (terpri stream)))))))
