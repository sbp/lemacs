;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1991-1993 by Lucid, Inc.  All Rights Reserved.

;;; This is greatly complicated by the fact that both the old functions
;;; and the new functions are dumped.  The only method I've found that
;;; works and doesn't have obscure bootstrapping/feedback problems is
;;; to RELOAD the original definition of the function we are advising
;;; at compile time so that we can extract its original docstring, and
;;; emit a modified version of that to the .elc version of this file.

;; To avoid the problem that if the docstrings in energize-mode.el change, 
;; this file wouldn't pick that up unless it is compiled after that one,
;; this file explicitly loads the .el versions of the energize libraries
;; at compile-time.  Too bad the byte compiler isn't reentrant or we could
;; just force those to be recompiled and reloaded when this is compiled.
;; As it is, we pollute the compile environment with uncompiled source...

(eval-when-compile	; this only works at compile-time anyway...

 ;; load the .el versions to make sure we get the *current* docstrings.
(load-library "energize-mode.el")
(load-library "energize-shell.el")

(defmacro energize-advise-function (fun library)
  (if library
      (let ((old (symbol-function fun)))
	(load-library library)  ; to get original defs/doc strings
	(if (eq old (symbol-function fun))
	    (error "%s failed to redefine %s" library fun))))
  (let* ((name (symbol-name fun))
	 (saved (intern (concat "energize-orig-" name)))
	 (new (intern (concat "energize-" name)))
	 (nfun (symbol-function new))
	 odoc ndoc doc
	 int arglist args)
    (setq odoc (documentation fun))
    (if (equal "" odoc) setq odoc nil)
    (or odoc (error "%s has no doc" fun))
    (setq ndoc (documentation new))
    (if (equal "" ndoc) (setq ndoc nil))
    (or ndoc (error "%s has no doc" new))
    (setq doc (concat odoc "\n\n" ndoc))

    ;; don't lose on autoloads
    (if (eq 'autoload (car-safe (symbol-function fun)))
	(error "%s is an autoload" fun))

    (cond ((compiled-function-p (symbol-function fun))
	   (setq arglist (aref (symbol-function fun) 0)
		 int (if (> (length (symbol-function fun)) 5)
			 (list 'interactive (aref (symbol-function fun) 5)))))
	  (t
	   (setq arglist (nth 1 (symbol-function fun))
		 int (nth 2 (symbol-function fun)))
	   (or (eq 'interactive (car-safe int)) (setq int nil))
	   ))

    (setq args (delq '&optional (copy-sequence arglist)))
    (if (memq '&rest args) (error "can't cope with &rest, dude"))

    (` (progn
	 (or (fboundp '(, saved))
	     (fset '(, saved) (symbol-function '(, fun))))

	 (defun (, fun) (, arglist)
	   (, doc)
	   (,@ (if int (list int) nil))
	   ((, new) (,@ args)))
	 ))))

) ;closes eval-when-compile


;;; Install the advice...
;;; Be really careful when you're changing this junk.  Talk to jwz first.

;; these are defined in energize-mode.el
(energize-advise-function set-visited-file-name "files")
(energize-advise-function find-file-noselect	nil)
(energize-advise-function write-file		nil)
(energize-advise-function normal-mode		nil)

(energize-advise-function ask-user-about-lock	"userlock")

(energize-advise-function next-error		"compile")
;(energize-advise-function previous-error	nil)

(energize-advise-function gdb-break		"gdb")
(energize-advise-function gdb-step		nil)
(energize-advise-function gdb-stepi		nil)

;; these are defined in energize-shell.el
(energize-advise-function comint-mark		"comint")
(energize-advise-function comint-send-input	nil)
