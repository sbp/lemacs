;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1991-1993 by Lucid, Inc.  All Rights Reserved.

;;; This file contains the definitions of existing functions which Energize
;;; must encapsulate.  (The number of such functions should be minimized.)


;;; This is greatly complicated by the fact that both the old functions
;;; and the new functions are dumped.  The only method I've found that
;;; works and doesn't have obscure bootstrapping/feedback problems is
;;; to RELOAD the original definition of the function we are advising
;;; at compile time so that we can extract its original docstring, and
;;; emit a modified version of that to the .elc version of this file.


(eval-when-compile	; this only works at compile-time anyway...

 (or noninteractive
     (error "bad idea to compile this file in a non-batch-mode emacs!"))

 (load-library "advice-freeze")

 (load-library "files.el")
 (load-library "userlock.el")
 (load-library "compile.el")
 (load-library "gdb.el")

) ;closes eval-when-compile

;; Kludge to make docstrings prettier.
;; This can't be in the same top-level form as the above.
(eval-when-compile

 (defun ad-make-advised-docstring (function)
   (let* ((origdef (ad-real-orig-definition function))
	  (origdoc
	   (documentation origdef t)))
     (concat (or origdoc "")
	     (mapconcat
	      #'(lambda (class)
		  (mapconcat
		   #'(lambda (advice)
		       (let ((doc (ad-docstring (ad-advice-definition advice))))
			 (if doc (concat "\n\n" doc) "")))
		   (ad-get-enabled-advices function class)
		   ""))
	      ad-advice-classes ""))))
 )


;;; The actual definitions

(defadvice ask-user-about-lock (around energize freeze)
  "Energize buffers do this by asking the server."
  (if (energize-buffer-p (current-buffer))
      (setq ad-return-value t) ; note: return value matters
    ad-do-it))

(defadvice normal-mode (after energize freeze)
  "If this is an Energize buffer, the Energize modes are turned on as well."
  (if (and (energize-buffer-p (current-buffer))
	   (not inside-energize-buffer-creation-hook-function))
      (funcall energize-create-buffer-hook (current-buffer))))

(defadvice find-file-noselect (around energize freeze)
  "This function has been encapsulated to work with Energize."
  (if (and (connected-to-energize-p)
	   (not (file-directory-p (ad-get-arg 0)))
	   (energize-query-buffer (ad-get-arg 0) t))
      ;; after-find-file and abbreviate-file-name are called from
      ;; energize-buffer-creation-hook-function, which is run from
      ;; editorside.c (way down under energize-query-buffer).
      ;; This is a mess...
      (setq ad-return-value ; note: return value matters
	    (energize-query-buffer (ad-get-arg 0)))
    ;; else
    ad-do-it))

(defadvice write-file (around energize freeze)
  "When executed on an Energize buffer, this will cause all 
annotations to be lost (that is, the buffer will become a 
normal buffer, not one that the Energize server knows about.)"
  (if (not (energize-write-file-buffer-p))
      ad-do-it
    ;; else...
    (let ((filename (ad-get-arg 0)))
      (if (and (file-exists-p filename)
	       (not
		(yes-or-no-p (format "File %s already exists.  Overwrite it? "
				     filename))))
	  (error "Aborted"))
      (write-region (point-min) (point-max) filename nil nil)
      (if buffer-file-name
	  (revert-buffer t t)) ; revert this buffer from the Energize server
      (kill-buffer nil) ; kill the current buffer, to lose Energize properties
      (set-window-buffer ; and now visit the "new" file, and all that entails
       (selected-window)
       (find-file-noselect filename)))))

(defadvice set-visited-file-name (around energize freeze)
  "The file name associated with an Energize buffer cannot be changed in this\
way.\nUse the `write-file' command instead."
  (if (and (energize-write-file-buffer-p)
	   (not (equal (ad-get-arg 0) buffer-file-name)))
      (error "Can't change the name associated with an Energize buffer.")
    (prog1
	ad-do-it
      (if (energize-buffer-p (current-buffer))
	  (energize-mode-internal)))))

(defadvice next-error (around energize freeze)
  "This function has been encapsulated to work with the Energize Error Log."
  (if (or (not (connected-to-energize-p))
	  (non-energize-errors-exist-p))
      ad-do-it
    (energize-execute-command (if (ad-get-arg 0)
				  "previouserror"
				"nexterror"))))

(defadvice gdb-break (around energize freeze)
  "This function has been encapsulated to work with the Energize debugger."
  (if (not (energize-buffer-p (current-buffer)))
      ad-do-it
    (energize-execute-command "setbreakpoint")))

(defadvice gdb-step (around energize freeze)
  "This function has been encapsulated to work with the Energize debugger."
  (if (not (energize-buffer-p (current-buffer)))
      ad-do-it
    (let ((arg (ad-get-arg 0)))
      (while (> arg 0)
	(energize-execute-command "steponce")
	(setq arg (1- arg))))))

(defadvice gdb-stepi (around energize freeze)
  "This function has been encapsulated to work with the Energize debugger."
  (if (not (energize-buffer-p (current-buffer)))
      ad-do-it
    (let ((arg (ad-get-arg 0)))
      ;; there's no energize command for this, so do it this way...
      (save-excursion
	(set-buffer "*Debugger*")
	(goto-char (point-max))
	(insert (format "stepi %d" arg))
	(comint-send-input)))))
