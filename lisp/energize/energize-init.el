;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1990-1994 by Lucid, Inc.  All Rights Reserved.

(defvar energize-auto-raise-screen t
  "If T screens are automatically raised when Energize wants to show them.")

(defvar energize-connect-hook nil
  "*Function or functions to run when the Energize connection is established.")

(defvar energize-disconnect-hook nil
 "*Function or functions to run when the Emacs/Energize connection is closed.")


(defvar energize-screen-mode nil)
(defvar energize-split-screens-p t)

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
  (put 'gdb-mode		      'screen-name 'debugger)
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
  (setq energize-screen-mode 'multi)
  t)

(defun energize-several-screens-mode ()
  "Call this function to put Energize into multi-screen mode, 
but with only a few screens.  See also `energize-multi-screen-mode'.

A screen named \"debugger\" will be used for the *Debugger* buffer,
  and its associated source files.
A screen named \"energize\" will be used for the Top-Level buffer.
A single screen named \"browser\" will be created for L.E. Browser buffers.
A single screen named \"project\" will be created for Project buffers.
A screen named \"error-log\" will be created for the Error Log buffer
 and its associated source files (as when the Next Error command 
 displays a source file.)
A single screen named \"manual\" will be created for UNIX Manual page buffers.

If an external editor is being used, then source files will be displayed
read-only in the \"debugger\" screen.

If an external editor is not being used, then a single screen named 
\"sources\" will be created to edit source files.  Find-file will use the
current screen, whatever that happens to be, but find-file-other-window, 
and selecting source files from the Buffers menu will use an existing screen
displaying the file in question, or create a new one if there isn't one.

Call `energize-single-screen-mode' to turn this off.

See the documentation for the function get-screen-for-buffer for 
information on how to customize this."
  (interactive)
  (energize-multi-screen-mode)
  (remprop 'browser 'instance-limit)
  (remprop 'project 'instance-limit)
  (remprop 'manual  'instance-limit)
  (remprop 'sources 'instance-limit)
  (setq energize-screen-mode 'several)
  t)

(defun energize-single-screen-mode ()
  "Call this function to put Energize into single-screen mode.
All buffers will be displayed in the currently selected screen."
  (interactive)
  (remprop 'browser 'instance-limit)
  (remprop 'project 'instance-limit)
  (remprop 'manual  'instance-limit)
  (remprop 'sources 'instance-limit)
  (remprop 'energize-debugger-mode        'screen-name)
  (remprop 'gdb-mode		          'screen-name)
  (remprop 'energize-top-level-mode       'screen-name)
  (remprop 'energize-browser-mode         'screen-name)
  (remprop 'energize-breakpoint-mode      'screen-name)
  (remprop 'energize-project-mode         'screen-name)
  (remprop 'energize-no-file-project-mode 'screen-name)
  (remprop 'energize-log-mode             'screen-name)
  (remprop 'energize-manual-entry-mode    'screen-name)
  (setq get-screen-for-buffer-default-screen-name nil)
  (setq buffers-menu-switch-to-buffer-function 'switch-to-buffer)
  (setq energize-screen-mode 'single)
  nil)

(energize-single-screen-mode)


;;; Connecting and disconnecting

(or energize-attributes-mapping
    (setq energize-attributes-mapping
	  (purecopy
	   '((0 . default)
	     (1 . bold)
	     (2 . italic)
	     (3 . bold-italic)
	     (4 . attributeSmall)
	     (50 . attributeGlyph)
	     (51 . attributeSectionHeader)
	     (52 . attributeToplevelFormGlyph)
	     (53 . attributeModifiedToplevelFormGlyph)
	     (54 . attributeBrowserHeader)
	     (68 . attributeWriteProtected)
	     (69 . attributeModifiedText)
	     ))))

;; Make the faces before emacs is dumped - this should be ok, they will be
;; initialized from the resource database when the first screen is created.
(let ((rest energize-attributes-mapping))
  (while rest
    (make-face (cdr (car rest)))
    (setq rest (cdr rest))))


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

  (let ((energize-disconnect-hook
	 ;; If we're being run interactively, don't exit emacs if connecting
	 ;; to Energize fails!  That's damn annoying.
	 (if (and (interactive-p)
		  (consp energize-disconnect-hook)
		  (memq 'save-buffers-kill-emacs energize-disconnect-hook))
	     (delq 'save-buffers-kill-emacs
		   (copy-sequence energize-disconnect-hook))
	   energize-disconnect-hook)))

    (connect-to-energize-internal server enarg)
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
      (run-hooks 'energize-connect-hook))))

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
	      (progn
		(message "Energizing buffer %s..." (buffer-name buffer))
		(find-file-noselect filename))
	    (message (format "Buffer %s not Energized." (buffer-name buffer)))
	    (sit-for 1)))))
      (message nil)))))

(add-hook 'energize-connect-hook 'energize-all-buffers)


;; This is called when the connection to Energize is lose (for whatever
;; reason).   We could just run the energize-disconnect-hook from C and
;; put this function on it, but then the user could hurt themselves.
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
				  (if (extent-property extent 'energize)
				      (delete-extent extent))
				  nil))
		      buffer)
		     (remove-hook 'write-file-data-hooks
				  'energize-write-data-hook)
		     (setq revert-buffer-insert-file-contents-function nil)
		     (ad-Orig-normal-mode-after-energize) ; #### necessary?
		     )
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

(defun energize-extent-at (pos &optional buffer)
  (extent-at pos buffer 'energize))

(defun non-energize-errors-exist-p ()
  ;; Whether `next-error' executed right now should do the emacs thing.
  ;; If we're in a *grep* or *compile* buffer, always do the emacs thing.
  ;; If we're in the Error Log, always do the Energize thing.
  ;; Otherwise, do the emacs thing if it would succeed; otherwise do the
  ;; Energize thing.
  (or (compilation-buffer-p (current-buffer))			; in *grep*
      (and (not (eq (energize-buffer-type (current-buffer))	; in ErrLog
		    'energize-error-log-buffer))
	   ;; defined in compile.el (a lemacs addition).
	   (compilation-errors-exist-p))))


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
		default-directory (file-name-directory buffer-file-name))
	(setq default-directory
	      (abbreviate-file-name (expand-file-name default-directory))))

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
	      ((eq type 'energize-unix-manual-buffer)
	       (energize-manual-mode))
	      ((or (eq type 'energize-source-buffer)
		   ;;(eq type 'energize-unspecified-buffer)
		   ;;(null type)
		   )
	       (compute-buffer-file-truename)
	       (if (buffer-file-name buffer)
		   (after-find-file nil t)
		 (funcall default-major-mode))
	       )
	      (t
	       (signal 'error (list "unknown energize buffer type" type)))))

      (if (eq (energize-buffer-type (current-buffer)) 'energize-source-buffer)
	  (energize-source-minor-mode))

      (energize-external-editor-set-mode buffer)
      )))

(setq energize-create-buffer-hook 'energize-buffer-creation-hook-function)

;;; Buffer modified hook

(defun energize-send-buffer-modified-1 (start end)
  (if (not (energize-buffer-p (current-buffer)))
      nil
    (map-extents #'(lambda (ext ignore)
		     (and (extent-property ext 'energize)
			  (set-extent-face ext 'attributeModifiedText))
		     nil)
		 (current-buffer) start end)
    (energize-send-buffer-modified t start end)))

(add-hook 'before-change-functions 'energize-send-buffer-modified-1)

;;; Reverting buffers
;;; This is called when Energize has informed us that a buffer has changed
;;; on disk, and we need to revert.

(defun energize-auto-revert-buffer (buf)
  (cond ((not (file-exists-p (buffer-file-name buf)))
	 ;; Signal an error here?  Naah, let someone else deal with it.
	 nil)
	;; If it's not modified, just revert.  If it is modified, ask.
	((or (not (buffer-modified-p buf))
	     (yes-or-no-p
	      (format "File %s changed on disk.  Discard your edits? "
		      (file-name-nondirectory (buffer-file-name buf)))))
	 (save-excursion
	   (set-buffer buf)
	   (revert-buffer t t)))))

;;; Energize kernel busy hook

(defun energize-message-if-not-in-minibuffer (reason)
  (if (not (eq (selected-window) (minibuffer-window)))
      (message reason)))

(setq energize-kernel-busy-hook 'energize-message-if-not-in-minibuffer)

;;; set-buffer-modified-p hook

(defun energize-send-buffer-modified-2 (state start end)
  (if (not (energize-buffer-p (current-buffer)))
      nil
    (if (not state)
	;; If we're unmodifying the buffer, un-highlight all Energize extents.
	(let ((e (next-extent (current-buffer))))
	  (while e
	    (if (and (extent-property e 'energize)
		     (eq (extent-face e) 'attributeModifiedText))
		(set-extent-face e nil))
	    (setq e (next-extent e)))))
    (energize-send-buffer-modified state start end)))

(setq energize-buffer-modified-hook 'energize-send-buffer-modified-2)

;;; hook in editorside.c

(setq energize-kernel-modification-hook nil)


;; command line

(defconst energize-args '(("-energize"	 . command-line-process-energize)
			  ("-context"	 . command-line-process-energize-1)
			  ("-beam-me-up" . command-line-process-energize-1)))

(setq command-switch-alist (append command-switch-alist energize-args))

(fset 'command-line-process-energize 'command-line-process-energize-1)
(put 'command-line-process-energize-1 'undocumented t)
(defun command-line-process-energize-1 (arg)
  "Connect to the Energize server at $ENERGIZE_PORT."
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
;;; This is a total kludge; there ought to be a hook that gets
;;; run before the first screen is created (either before or
;;; after term/x-win.el is loaded.)

(or (fboundp 'energize-orig-screen-initialize)
    (fset 'energize-orig-screen-initialize
	  (symbol-function 'screen-initialize)))

(defun screen-initialize ()
  (if (let ((rest energize-args))
	(catch 'done
	  (while rest
	    (if (member (car (car rest)) command-line-args)
		(throw 'done t))
	    (setq rest (cdr rest)))
	  nil))
      (setq default-screen-name "energize"))
  (energize-orig-screen-initialize))
