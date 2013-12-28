;; Help commands for Emacs
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


(defvar help-map (make-sparse-keymap)
  "Keymap for characters following the Help key.")

(define-key global-map "\C-h" 'help-command)
(fset 'help-command help-map)

(define-key help-map "\C-h" 'help-for-help)
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

;; This is a grody hack of the same genotype as `advertised-undo'; we want
;; the default bindings of Backspace and C-h to be the same, but we want
;; the menubar to claim that `info' in invoked with `C-h i', not `BS i'.

(defun deprecated-help-command ()
  (interactive)
  (if (eq 'help-command (key-binding "\C-h"))
      (setq unread-command-event (character-to-event ?\C-h (allocate-event)))
    (help-for-help)))

(define-key global-map 'backspace 'deprecated-help-command)

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
      (setq auto-save-file-name nil)
      (insert-file-contents (expand-file-name "TUTORIAL" exec-directory))
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
  (let ((defn (key-binding key)))
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" (key-description key))
      (message "%s runs the command %s"
	       (key-description key)
	       (if (symbolp defn) defn (prin1-to-string defn))))))

(defun print-help-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
Computes a message and applies the argument FUNCTION to it.
If FUNCTION is nil, applies `message' to it, thus printing it."
  (and (not (get-buffer-window standard-output))
       (funcall (or function 'message)
		(substitute-command-keys
		 (if (one-window-p t)
		     (if pop-up-windows
			 "Type \\[delete-other-windows] to remove help window."
		       "Type \\[switch-to-buffer] RET to remove help window.")
		   "Type \\[switch-to-buffer-other-window] RET to restore old contents of help window.")))))

(defun describe-key (key)
  "Display documentation of the function KEY invokes.  
KEY is a string, or vector of events.  When called interactvely, key may
also be a menu selection."
  (interactive "kDescribe key: ")
  (let (defn)
    ;; If the key typed was really a menu selection, grab the form out
    ;; of the event object and intuit the function that would be called,
    ;; and describe that instead.
    (if (and (vectorp key) (= 1 (length key)) (menu-event-p (aref key 0)))
	(let ((event (aref key 0)))
	  (setq defn (list (event-function event) (event-object event)))
	  (if (eq (car defn) 'eval)
	      (setq defn (car (cdr defn))))
	  (if (eq (car-safe defn) 'call-interactively)
	      (setq defn (car (cdr defn))))
	  (if (and (consp defn) (null (cdr defn)))
	      (setq defn (car defn))))
      (setq defn (key-binding key)))
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" (key-description key))
      (with-output-to-temp-buffer "*Help*"
	(prin1 defn)
	(princ ":\n")
	(if (documentation defn)
	    (princ (documentation defn))
	  (princ "not documented"))
	(print-help-return-message)))))


(defun where-is (definition)
  "Print message listing key sequences that invoke specified command.
Argument is a command definition, usually a symbol with a function definition."
  (interactive "CWhere is command: ")
  (let ((keys (where-is-internal definition (current-local-map) nil nil nil)))
    (if keys
	(message "%s is on %s" definition
	  (mapconcat 'key-description
		     (sort keys '(lambda (x y) (< (length x) (length y))))
		     ", "))
      (message "%s is not on any keys" definition)))
  nil)


(defun describe-mode ()
  "Display documentation of current major mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ mode-name)
    (princ " Mode:\n")
    (princ (documentation major-mode))
    (print-help-return-message)))

(defun describe-distribution ()
  "Display info on how to obtain the latest version of GNU Emacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "DISTRIB" exec-directory)))

(defun describe-copying ()
  "Display info on how you may redistribute copies of GNU Emacs."
  (interactive)
  (find-file-read-only
   (expand-file-name "COPYING" exec-directory))
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
  (find-file-read-only (expand-file-name "NEWS" exec-directory)))

(defun view-lossage ()
  "Display last 100 input keystrokes."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (key-description (recent-keys)))
    (save-excursion
      (set-buffer standard-output)
      (goto-char (point-min))
      (while (progn (move-to-column 50) (not (eobp)))
	(search-forward " " nil t)
	(insert "\n")))
    (print-help-return-message)))

(defun help-for-help ()
  "You have typed C-h, the help character.  Type a Help option:

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
C-w print information on absence of warranty for GNU Emacs."
  (interactive)
  (let ((help-key (copy-event last-command-event))
	event char)
    (message
  "A B C F I K L M N P S T V W C-c C-d C-n C-w.  Type %s again for more help: "
     (single-key-description help-key))
    (setq event (next-command-event (allocate-event))
	  char (or (event-to-character event) event))
    (if (or (equal char help-key) (equal char ?\C-h) (equal char ??))
	(save-window-excursion
	  (switch-to-buffer "*Help*")
	  (erase-buffer)
	  (insert (documentation 'help-for-help))
	  (goto-char (point-min))
	  (while (or (equal char help-key)
		     (memq char '(?\C-h ?? ?\C-v ?\ ?\177 ?\M-v)))
	    (if (memq char '(?\C-v ?\ ))
		(scroll-up))
	    (if (memq char '(?\177 ?\M-v))
		(scroll-down))
	    (message "A B C F I K L M N P S T V W C-c C-d C-n C-w%s: "
		     (if (pos-visible-in-window-p (point-max))
			 "" " or Space to scroll"))
	    (let ((cursor-in-echo-area t))
	      (setq event (next-command-event event)
		    char (or (event-to-character event) event))))))
    (let ((defn (lookup-key help-map (if (eventp char) (vector char)
				       (char-to-string (downcase char))))))
      (if defn (call-interactively defn) (ding)))))


(defun function-called-at-point ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	  (backward-up-list 1)
	  (forward-char 1)
	  (let (obj)
	    (setq obj (read (current-buffer)))
	    (and (symbolp obj) (fboundp obj) obj))))
    (error nil)))

(defvar describe-function-show-arglist t  ; default to nil for the non-hackers?
  "*If true, then describe-function will show its arglist if the function is
not an autoload.")

(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)	     
	 val)
     (setq val (completing-read (if fn
				    (format "Describe function (default %s): " fn)
				  "Describe function: ")
				obarray 'fboundp t))
     (list (if (equal val "")
	       fn (intern val)))))
  (with-output-to-temp-buffer "*Help*"
    (prin1 function)
    (princ ": ")
    (let (arglist)
      (if describe-function-show-arglist
	  (let ((def function))
	    (while (symbolp def) (setq def (symbol-function def)))
	    (setq arglist (cond ((subrp def) 'subr)
				((eq 'autoload (car-safe def)) 'autoload)
				((eq 'lambda (car-safe 'def)) (nth 1 def))
				((compiled-function-p def) (aref def 0))))
	    (if arglist (prin1 arglist) (princ "()"))))
      (princ "\n")
      (if (documentation function)
	  (princ (documentation function))
	(princ "not documented"))
      (if (eq arglist 'subr)
	  (save-excursion
	    (set-buffer "*Help*")
	    (goto-char (point-max))
	    (forward-line -1)
	    (if (looking-at "arguments:")
		(let ((p (point)))
		  (goto-char (match-end 0))
		  (setq arglist (buffer-substring (point) (point-max)))
		  (delete-region p (point-max))
		  (goto-char (point-min))
		  (end-of-line)
		  (delete-backward-char 5)
		  (insert " ")
		  (insert arglist))))))
    (print-help-return-message)))

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
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if v
				    (format "Describe variable (default %s): " v)
				  "Describe variable: ")
				obarray 'boundp t))
     (list (if (equal val "")
	       v (intern val)))))
  (with-output-to-temp-buffer "*Help*"
    (prin1 variable)
    (princ "'s value is ")
    (if (not (boundp variable))
        (princ "void.")
      (prin1 (symbol-value variable)))
    (terpri) (terpri)
    (princ "Documentation:")
    (terpri)
    (let ((doc (documentation-property variable 'variable-documentation)))
      (if doc
	  (princ (substitute-command-keys doc))
	(princ "not documented as a variable.")))
    (print-help-return-message)))

(defun command-apropos (string)
  "Like apropos but lists only symbols that are names of commands
\(interactively callable functions).  Argument REGEXP is a regular expression
that is matched against command symbol names.  Returns list of symbols and
documentation found."
  (interactive "sCommand apropos (regexp): ")
  (let ((message
	 (let ((standard-output (get-buffer-create "*Help*")))
	   (print-help-return-message 'identity))))
    (apropos string t 'commandp)
    (and message (message message))))

(defun locate-library (library &optional nosuffix)
  "Show the full path name of Emacs library LIBRARY.
This command searches the directories in `load-path' like  M-x load-library
to find the file that  M-x load-library RET LIBRARY RET  would load.
Optional prefix arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY - a la calling (load LIBRARY nil nil t)."
  (interactive "sLocate library: \nP")
  (let ((file (locate-file library load-path (if nosuffix nil ".elc:.el:"))))
    (if file
	(message "Library is file %s" file)
      (message "No library %s in search path" library))
    file))
