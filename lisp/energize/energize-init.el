;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1990-1993 by Lucid, Inc.  All Rights Reserved.

(defvar energize-auto-raise-screen t
  "If T screens are automatically raised when Energize wants to show them.")

(defvar energize-connect-hook nil
  "*Function or functions to run when the Energize connection is established.")

(defvar energize-disconnect-hook nil
 "*Function or functions to run when the Emacs/Energize connection is closed.")

;;; Multi and single screen modes

;;    what should happen in non-vi multi-screen mode when one selects a 
;;    non-visible source buffer?  Show it in the current screen?  
;;    In vi-mode, we show all sources in the debugger screen, since they're
;;    read-only.  But in emacs-mode, we use a "sources" screen, and create
;;    a new one each time.  So find-file will reuse a screen, but the buffers
;;    menu will reuse an old one or create a new one.  Another option is to 
;;    always us the same screen, possibly the "energize" screen.

(defun energize-multi-screen-mode ()
  "Call this function to put Energize into multi-screen mode.

A screen named \"debugger\" will be used for the *Debugger* buffer,
  and its associated source files.
A screen named \"energize\" will be used for the Top-Level buffer.
A screen named \"browser\" will be created for each L.E. Browser buffer.
 At most 5 of these will be created; then they will be reused.
A screen named \"project\" will be created for each Project buffer.
A screen named \"error-log\" will be created for the Error Log buffer
 and its associated source files (as when the Next Error command 
 displays a source file.)
A screen named \"manual\" will be created for each UNIX Manual page.
 At most 5 of these will be created; then they will be reused.

If an external editor is being used, then source files will be displayed
read-only in the \"debugger\" screen.

If an external editor is not being used, then screens named \"sources\" 
will be created to edit source files.  At most five of these will be 
created; then they will be reused.  Find-file will use the current screen,
whatever that happens to be, but find-file-other-window, and selecting 
source files from the Buffers menu will use an existing screen displaying
the file in question, or create a new one if there isn't one.

Call `energize-single-screen-mode' to turn this off.

See the documentation for the function get-screen-for-buffer for 
information on how to customize this."
  (interactive)
  (put 'project      'instance-limit 0)
  (put 'sources      'instance-limit 5)
  (put 'manual       'instance-limit 5)
  (put 'browser      'instance-limit 5)
  (put 'energize-debugger-mode        'screen-name 'debugger)
  (put 'energize-top-level-mode       'screen-name 'energize)
  (put 'energize-browser-mode         'screen-name 'browser)
  (put 'energize-breakpoint-mode      'screen-name 'browser)
  (put 'energize-project-mode         'screen-name 'project)
  (put 'energize-no-file-project-mode 'screen-name 'project)
  (put 'energize-log-mode             'screen-name 'error-log)
  (put 'energize-manual-entry-mode    'screen-name 'manual)
  (if energize-external-editor
      (setq get-screen-for-buffer-default-screen-name 'debugger)
    ;; hmmmm...
    (setq get-screen-for-buffer-default-screen-name 'sources))
  (setq buffers-menu-switch-to-buffer-function 'pop-to-buffer)
  t)

(defun energize-single-screen-mode ()
  "Call this function to put Energize into single-screen mode.
All buffers will be displayed in the currently selected screen."
  (interactive)
  (remprop 'browser      'instance-limit)
  (remprop 'project      'instance-limit)
  (remprop 'manual       'instance-limit)
  (remprop 'sources      'instance-limit)
  (remprop 'energize-debugger-mode        'screen-name)
  (remprop 'energize-top-level-mode       'screen-name)
  (remprop 'energize-browser-mode         'screen-name)
  (remprop 'energize-breakpoint-mode      'screen-name)
  (remprop 'energize-project-mode         'screen-name)
  (remprop 'energize-no-file-project-mode 'screen-name)
  (remprop 'energize-log-mode             'screen-name)
  (remprop 'energize-manual-entry-mode    'screen-name)
  (setq get-screen-for-buffer-default-screen-name nil)
  (setq buffers-menu-switch-to-buffer-function 'switch-to-buffer)
  nil)

(energize-single-screen-mode)


;;; Connecting and disconnecting

(setq energize-attributes-mapping 
  '((51 attributeSectionHeader) (54 attributeBrowserHeader)
    (68 attributeWriteProtected) (69 attributeModifiedText)
    (1 bold) (2 italic) (3 bold-italic)
    (4 attributeSmall) (50 attributeGlyph) (52 attributeToplevelFormGlyph)
    (53 attributeModifiedToplevelFormGlyph)))

(defun energize-initialize-faces ()
  (setq energize-attributes-mapping
	(mapcar (function (lambda (l)
			    (cons (car l)
				  (cons (car (cdr l))
					(face-id
					 (or (find-face (car (cdr l)))
					     (make-face (car (cdr l)))))))))
		energize-attributes-mapping)))

(defun any-energize-buffers-p ()
  (let ((rest (buffer-list))
	(result nil))
    (while rest
      (if (energize-buffer-p (car rest))
	  (setq result (car rest) rest nil)
	(setq rest (cdr rest))))
    result))

(defun connect-to-energize (server &optional enarg)
  "Connect this emacs to a Energize server.
The SERVER argument should be the name of the host that the kernel is
running on (empty-string for localhost).  It may also be of the form
``hostname:user'' or ``:user'', meaning to use the server running with
userid USER."
  (interactive (if (connected-to-energize-p)
		   (error "Already connected to the server.") ; you bogon.
		 (list (read-string "connect to energize server: "))))
  (if (connected-to-energize-p)
      (error "Already connected to the server.")) ; you bogon.
  (if (or (null server) (equal server ""))
      (setq server (system-name)))
  (setq default-screen-name "energize")
  (energize-rename-things)
  (energize-hack-external-editor-mode)
  (connect-to-energize-internal server enarg)
  (energize-initialize-faces)
  ;; Wait for the Top-Level buffer to be created.
  ;; This really should happen down in C, but...
  (let ((p (or (get-process "energize")
	       (error "Could not connect to Energize.")))
	b)
    (while (progn
	     (or (connected-to-energize-p)
		 (error "Energize connection refused."))
	     (not (setq b (any-energize-buffers-p))))
      (accept-process-output p))
    ;; Make the displayed Energize buffer initially displayed.
    (pop-to-buffer b)
    (delete-other-windows)
    (run-hooks 'energize-connect-hook)))

(defun disconnect-from-energize ()
  (interactive)
  "Close the connection to energize"
  (close-connection-to-energize))

;;; Energizing all buffers
;; After being connected to energize this function energizes all the
;; buffers that contain files that Energize knows about.

(defun energize-all-buffers ()
  "Energize any buffer showing a file that the Energize server knows about.
Has to be called after Emacs has been connected to Energize"
  (if (not (connected-to-energize-p))
      (error "You have to connect to Energize first"))
  (save-window-excursion
   (save-excursion
    (let ((buffers (buffer-list))
	  (buffers-to-avoid '())
	  (lock-directory nil)
	  buffer
	  filename)
      (while buffers
	(setq buffer (car buffers))
	(setq buffers (cdr buffers))
	(setq filename (buffer-file-name buffer))
	(set-buffer buffer)
	(cond
	 ((and filename
	       (not (energize-buffer-p buffer))
	       (energize-query-buffer filename t))
	  (cond ((buffer-modified-p)
		 (if (y-or-n-p
		      (format
		       "Buffer %s must be saved to be Energized; save it? "
		       (buffer-name buffer)))
		     (progn
		       (set-buffer buffer) ; oh, man...
		       (save-buffer))
		   ;; said "no"
		   (setq buffers-to-avoid (cons buffer buffers-to-avoid))))
		
		((and (null (verify-visited-file-modtime buffer))
		      (file-exists-p filename))
		 (set-buffer buffer)
		 (if (y-or-n-p
		      (format "Buffer %s has changed on disk, revert? "
			      (buffer-name buffer)))
		     (progn
		       (set-buffer buffer)
		       (revert-buffer nil t))
		   ;; said "no"
		   (setq buffers-to-avoid (cons buffer buffers-to-avoid))))

		;; It's wrong to check to also see if someone else is locking
		;; the file.  The file is already in the buffer, and the user
		;; isn't really modifying it -- we're just rewriting it because
		;; energize likes to do that.  That's why locking should be
		;; disabled here.
		)
	  (if (not (memq buffer buffers-to-avoid))
	      (find-file-noselect filename)
	    (message (format "Buffer %s not Energized." (buffer-name buffer)))
	    (sit-for 1)))))))))

(add-hook 'energize-connect-hook 'energize-all-buffers)


;; This is called when the connection to Energize is lose (for whatever reason).
;; We could just run the energize-disconnect-hook from C and put this function
;; on it, but then the user could hurt themselves.
;;
(defun de-energize-all-buffers ()
  (save-excursion
    (let ((buffers (buffer-list))
	  buffer)
      (while buffers
	(condition-case condition
	    (progn
	      (setq buffer (car buffers))
	      (set-buffer buffer)
	      (cond ((not (energize-buffer-p buffer))
		     nil)
		    ((eq (energize-buffer-type buffer) 'energize-source-buffer)
		     (map-extents
		      (function (lambda (extent ignore)
				  (if (eq 'energize-extent-data
					  (car-safe (extent-data extent)))
				      (delete-extent extent))
				  nil))
		      buffer)
		     (setq write-file-hooks
			   (delq 'energize-write-file-hook write-file-hooks))
		     (setq revert-buffer-insert-file-contents-function nil)
		     (energize-orig-normal-mode))
		    (t ; non-source-file Energize buffers
		     (set-buffer-modified-p nil)
		     (if (eq (other-buffer buffer) buffer)
			 (set-buffer (get-buffer-create "*scratch*"))
		       (set-buffer (other-buffer buffer)))
		     (kill-buffer buffer))))
	  (error ;; condition-case clause
	   (beep)
	   (message "Error while de-Energizing: %s" condition)))
	(setq buffers (cdr buffers)))))
  ;; now clean the menubar
  (deactivate-all-energize-menu-items)
  (energize-rename-things 'back)
  (run-hooks 'energize-disconnect-hook)
  nil)


(defun energize-rename-things (&optional back)
  ;; People who don't like emacs don't like seeing the word "Emacs" either
  (let ((case-fold-search t))
    (if (and (consp mode-line-buffer-identification)
	     (stringp (car mode-line-buffer-identification))
	     (string-match (if back "\\bEnergize\\b"
			     "\\bL?Emacs\\([- \t]*[-._0-9]+\\)?\\b")
			   (car mode-line-buffer-identification)))
	(setq-default mode-line-buffer-identification
		      (cons
		       (concat (substring (car mode-line-buffer-identification)
					  0 (match-beginning 0))
			       (if back "Emacs" "Energize")
			       (substring (car mode-line-buffer-identification)
					  (match-end 0)))
		       (cdr mode-line-buffer-identification))))
;    (if (stringp screen-title-format)
;	(if back
;	    (if (string-match "^Energize\\b ?" screen-title-format)
;		(setq-default screen-title-format "%S: %b"))
;	  (or (string-match "Energize" screen-title-format)
;	      (setq-default screen-title-format "Energize: %b"))))
    )
  nil)



;;; The kernel is very random about the buffer-types it returns.
;;; This is a temporary permanent fix...

(defun energize-buffer-type (buffer)
  "Returns a symbol denoting the type of an Energize buffer, or nil."
  (let ((type (energize-buffer-type-internal buffer)))
    (cond ((eq type 'energize-top-level-buffer)
	   (cond ((equal "Error Log" (buffer-name buffer))
		  'energize-error-log-buffer)
		 ((equal "*includers*" (buffer-name buffer))
		  'energize-includers-buffer)
		 ((string-match "^Browser" (buffer-name buffer))
		  'energize-browser-buffer)
		 (t type)))
	  ((eq type 'energize-unspecified-buffer)
	   (signal 'error (list "buffer type unspecified" buffer)))
	  ((and (null type) (energize-buffer-p buffer))
	   (signal 'error
		   (list "null buffer type for energize buffer" buffer)))
	  (t type))))


;;; Misc Energize hook functions

(defvar inside-energize-buffer-creation-hook-function nil)

(defun energize-buffer-creation-hook-function (buffer)
  ;; This loser is called every time Energize wants to create a buffer,
  ;; whether it is being spontaniously displayed (as by the debugger) or
  ;; as a result of calling find-file -> energize-find-file-noselect ->
  ;; energize-query-buffer.
  (let ((inside-energize-buffer-creation-hook-function t))
    ;; the above is so we can call this from normal-mode, except when
    ;; we're calling normal-mode.
    (save-excursion
      (set-buffer buffer)

      ;; Energize always hands us truenames, or something close to them
      ;; (it chomps the /tmp_mnt/ automounter cruft off.)  Let the user
      ;; set up a pretty translation just like they can for normal files.
      (if buffer-file-name
	  (setq buffer-file-name (abbreviate-file-name
				  (expand-file-name buffer-file-name))
		default-directory (file-name-directory buffer-file-name)))

      (if buffer-file-name (set-buffer-modtime buffer))

      (let ((type (energize-buffer-type buffer)))
	(cond ((eq type 'energize-top-level-buffer)
	       (energize-top-level-mode))
	      ((eq type 'energize-browser-buffer)
	       (energize-browser-mode))
	      ((eq type 'energize-includers-buffer)
	       (energize-browser-mode))
	      ((or (eq type 'energize-error-log-buffer)
		   (eq type 'energize-log-file-buffer))
	       (energize-log-mode)
	       (setq buffer-read-only t))
	      ((eq type 'energize-project-buffer)
	       (if (buffer-file-name)
		   (energize-project-mode)
		 (energize-no-file-project-mode)))
	      ((eq type 'energize-debugger-buffer)
	       (energize-debugger-mode))
	      ((eq type 'energize-breakpoint-buffer)
	       (energize-breakpoint-mode))
	      ((or (eq type 'energize-source-buffer)
		   ;;(eq type 'energize-unspecified-buffer)
		   ;;(null type)
		   )
	       (compute-buffer-file-truename)
	       ;; energize-source-minor-mode is run by find-file-hooks
	       (if (buffer-file-name buffer)
		   (after-find-file nil t)
		 (funcall default-major-mode))
	       )
	      (t
	       (signal 'error (list "unknown energize buffer type" type)))))
      (energize-external-editor-set-mode buffer)
      )))

(setq energize-create-buffer-hook 'energize-buffer-creation-hook-function)

;;; Buffer modified hook

(defun notify-send-buffer-modified-request (start end)
  (send-buffer-modified-request t start end))

(setq before-change-function 'notify-send-buffer-modified-request)

;;; Energize kernel busy hook

(defun energize-message-if-not-in-minibuffer (reason)
  (if (not (eq (selected-window) (minibuffer-window)))
      (message reason)))

(setq energize-kernel-busy-hook 'energize-message-if-not-in-minibuffer)

;;; set-buffer-modified-p hook

(setq energize-buffer-modified-hook 'send-buffer-modified-request)

;;; hook in editorside.c

(setq energize-kernel-modification-hook nil)


;; command line

(defvar energize-args '(("-context"	. command-line-process-energize)
			("-energize"	. command-line-process-energize)
			("-beam-me-up"	. command-line-process-energize)))

(setq command-switch-alist (append command-switch-alist energize-args))

(defun command-line-process-energize (arg)
  (let ((e-arg (car command-line-args-left))
	(e-host (getenv "ENERGIZE_PORT"))) ; maybe nil
    (if (and e-arg (string-match "\\`[0-9a-fA-F]+[,][0-9a-fA-F]+\\'" e-arg))
	(setq command-line-args-left (cdr command-line-args-left))
      (setq e-arg nil))
    (message "Connecting to Energize...") 
    (sit-for 0)
    (condition-case ()
	(connect-to-energize e-host e-arg)
      (error
       (beep)
       (if e-host
	   (message "Failed to connect to Energize at %s." e-host)
	 (message "Failed to connect to Energize."))
       (sit-for 1)))))


;;; Originally defined in screen.el
;;; If we're being invoked with -energize, then set the default
;;; screen name to "energize"

(or (fboundp 'energize-orig-multi-minibuffer-startup)
    (fset 'energize-orig-multi-minibuffer-startup
	  (symbol-function 'multi-minibuffer-startup)))

(defun multi-minibuffer-startup (window-system-switches)
  (if (let ((rest energize-args))
	(catch 'foo
	  (while rest
	    (if (member (car (car rest)) command-line-args)
		(throw 'foo t))
	    (setq rest (cdr rest)))
	  nil))
      (setq default-screen-name "energize"))
  (energize-orig-multi-minibuffer-startup window-system-switches))
