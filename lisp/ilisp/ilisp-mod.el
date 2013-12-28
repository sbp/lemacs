;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-mod.el,v 1.22 1993/09/03 02:05:07 ivan Rel $
;;;%Header
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;
;;;


;;;
;;; ILISP mode top level definitions.
;;; 

;;;%ilisp-mode

(defun ilisp-byte-code-to-list (function)
  "Returns a list suitable for passing to make-byte-code from FUNCTION."
  (let ((function-object 
	 (if (symbolp function)
	     (symbol-function function)
	   function)))
    (nconc
     (list (aref function-object 0)
	   (aref function-object 1)
	   (aref function-object 2)
	   (aref function-object 3))
     (condition-case nil
	 (cons (aref function-object 4)
	       (condition-case nil
		   (list (aref function-object 5))
		 (error nil)))
       (error nil)))))

;;;
(defun ilisp-set-doc (function string)
  "Set the documentation of the symbol FUNCTION to STRING."
  (let* ((old-function (symbol-function function)))
    (cond ((listp old-function)
	   ;; Probe to test whether function is in preloaded read-only
	   ;; memory, and if so make writable copy:
	   (condition-case nil
	       (setcar old-function (car old-function))
	     (error
	      (setq old-function (copy-sequence old-function)) ; shallow copy only
	      (fset function old-function)))
	   (let ((ndoc-cdr (nthcdr 2 old-function)))
	     (if (stringp (car ndoc-cdr))
		 ;; Replace the existing docstring.
		 (setcar ndoc-cdr string)
	       ;; There is no docstring.  Insert the overwrite msg.
	       (setcdr ndoc-cdr (cons (car ndoc-cdr) (cdr ndoc-cdr)))
	       (setcar ndoc-cdr string))))
	  (t
	   ;; it's an emacs19 compiled-code object
	   (let ((new-code (ilisp-byte-code-to-list old-function)))
	     (if (nthcdr 4 new-code)
		 (setcar (nthcdr 4 new-code) string)
	       (setcdr (nthcdr 3 new-code) (cons string nil)))
	     (fset function (apply 'make-byte-code new-code)))))))
    


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
(defun ilisp (name setup)
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
	;; Because comint-mode kills all buffer-local variables in
	;; fsf-19 we have to re-call the setup here.
	(funcall setup name)
	(setq major-mode 'ilisp-mode
	      mode-name "ILISP")
	(rplaca (car comint-send-queue) 
		(function (lambda ()
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

;;;%Modes
(set-default 'auto-mode-alist
	     (append '(("\\.cl$" . lisp-mode) ("\\.lisp$" . lisp-mode))
		     auto-mode-alist))
(setq completion-ignored-extensions 
      (append '(".68fasl" ".sfasl" ".ifasl" ".pfasl" 
		".68fasl4" ".sfasl4" ".ifasl4" ".pfasl4" 
		".sbin")
	      completion-ignored-extensions))

(provide 'ilisp-mod)
