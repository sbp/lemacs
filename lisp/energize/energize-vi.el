;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1992-1993 by Lucid, Inc.  All Rights Reserved.
;;; Energize support for the Editor of the Beast, and others.

(evi-define-key '(vi motion ex) "\C-x" 'evi-emacs-command)
(evi-define-key '(vi motion)    "\177" 'evi-backward-char)
(evi-define-key '(vi)	        "\C-z" 'evi-quit-evi)

(evi-define-key '(vi motion top-level) 'button3 'energize-popup-menu)

(setq evi-meta-prefix-char ?\C-a)

(defvar energize-external-editor nil)		; nil, vi, or a string
(defvar energize-internal-editor nil)		; nil or vi
(defvar energize-internal-viewer nil)		; nil, vi, or less
(defvar energize-vi-terminal-emulator nil)	; xterm, shelltool, or cmdtool

(defun ex-quit (discard)
  ;; originally defined in evi.el
  ;; The old version would exit emacs; this version just kills the current
  ;; buffer and deletes the current window (and screen if appropriate.)
  (and (not discard) (buffer-file-name) (buffer-modified-p)
       (evi-error
	 "No write since last change (use :quit! to override)"))
  ;; #### Since we're unmodifying this buffer with the intent of killing it,
  ;; we need to avoid telling Energize that we have unmodified it, or for some
  ;; reason it recreates the buffer later.  I don't understand...
  (let ((energize-buffer-modified-hook nil))
    (set-buffer-modified-p nil))
  (delete-auto-save-file-if-necessary)
  (kill-buffer (current-buffer))
  (setq ex-user-buffer (current-buffer))
  (condition-case nil
      (delete-window (selected-window))
    ;; ignore error about trying to delete only window on only screen
    (error nil))
  )

(defun ex-write-all-and-quit (quietly)
  ;; originally defined in evi.el
  ;; The old version would exit emacs; this version just kills the current
  ;; buffer and deletes the current window (and screen if appropriate.)
  (save-some-buffers quietly)
  (ex-quit t))

(defun energize-external-editor-set-menubar ()
  "Set the menubar to be used for the external editor"
  (delete-menu-item '("File" "Open..."))
  (add-menu-item '("File") "View..." 'find-file-other-window t
		 "Save")
  (add-menu-item '("File") '("Edit" . "")
		 'energize-edit-buffer-externally t "Save")
  (delete-menu-item '("File" "Exit Emacs")))

(defun energize-internal-editor-set-menubar ()
  "Set the menubar to be used for the internal editor"
  (add-menu-item '("File") "Open..." 'find-file t "Save")
  (delete-menu-item '("File" "View..."))
  (delete-menu-item '("File" "Edit"))
  (add-menu-item '("File") "Exit Emacs" 'save-buffers-kill-emacs t))


(defun sensitize-external-editor-menus-hook ()
  ;; make the "Edit File" menu item be inactive if this buffer doesn't have a
  ;; file, and make it contain the name of the file that will be edited.
  (let* ((menu (cdr (car (find-menu-item current-menubar '("File")))))
	 (ef (car (find-menu-item menu '("Edit File")))))
    (if (null ef)
	t ; no change
      (let ((name buffer-file-name))
	(if (eq (energize-buffer-type (current-buffer))
		'energize-top-level-buffer)
	    (setq name nil))
	(aset ef 2 (if name t nil))
	(aset ef 3 (if name (file-name-nondirectory buffer-file-name) nil))
	nil))))

(add-hook 'activate-menubar-hook 'sensitize-external-editor-menus-hook)

(defun energize-edit-buffer-externally-p (buffer)
  (and energize-external-editor
       (eq 'energize-source-buffer (energize-buffer-type buffer))))

(defun energize-edit-buffer-externally-1 (buffer extent)
  (save-excursion
    (set-buffer buffer)
    (cond ((numberp extent)
	   (goto-char extent))
	  ((null extent)
	   (goto-char 1))
	  (t
	   (goto-char (extent-start-position extent))))
    (energize-begin-external-edit (buffer-file-name)
				  (save-excursion
				    (beginning-of-line)
				    (1+ (count-lines 1 (point)))))))

(defun energize-edit-buffer-externally ()
  "Edit the currently-displayed buffer in $ENERGIZE_EDIT_MODE."
  (interactive)
  (if (not buffer-file-name)
      (error "Buffer not associated with a file"))
  (energize-edit-buffer-externally-1 (current-buffer) (point)))


;;; keeping track of inferior vi processes
;;; the energize_vi command can locate and reuse a running vi, but that's
;;; an expensive operation on X terminals, since the query-tree takes a
;;; *lot* of server traffic.  So if we know that there's no vi editing the
;;; file (at least not one that *we* started) we can tell it to go ahead 
;;; and start a new one without checking for an old one.  If we used a
;;; smarter wrapper (one that told us the window id, or talked to the kernel
;;; directly) instead of xterm, we wouldn't need the query-tree at all.

(defvar energize-vi-procs nil)  ; vi is a special case (we're more clever)
(defvar energize-other-external-editor-procs nil)  ; random others

(defun energize-vi-buffer-being-edited-p (file)
  (let ((rest energize-vi-procs)
	result)
    (while rest
      (if (string-equal file (cdr (car rest)))
	  (setq result t
		rest nil)
	(setq rest (cdr rest))))
    result))

(defvar energize-vi-wrapper "energize_vi")

(defun energize-begin-external-edit (file line)
  (let* ((ez-path (getenv "ENERGIZE_PATH"))
	 (exec-path (if ez-path
			(append exec-path (list ez-path))
		      exec-path))
	 (dir (file-name-directory file))
	 (name (file-name-nondirectory file))
	 (vi-p (eq energize-external-editor 'vi))
	 (program (if vi-p energize-vi-wrapper energize-external-editor))
 	 (pretty (cond (vi-p "vi")
 		       ((string-match "[ \t]" program) program)
 		       (t (file-name-nondirectory program))))
	 (procname (format "*%s %s*" (if vi-p "energize_vi" pretty) name))
	 (edited-p (and vi-p (energize-vi-buffer-being-edited-p file)))
	 proc msg)
    (setq line (if vi-p (format "+%d" line) "")
	  msg (format "%s %s %s %s..." (if edited-p "Reselecting" "Launching")
		      pretty name line))
    (message "%s" msg)
    (let ((default-directory dir)
	  (inhibit-quit t))
      (cond (vi-p
	     (setq proc
		   (apply 'start-process procname nil program
			  dir name line
			  (cond ((eq energize-vi-terminal-emulator 'xterm)
				 "-xterm")
				((eq energize-vi-terminal-emulator 'cmdtool)
				 "-cmdtool")
				((eq energize-vi-terminal-emulator 'shelltool)
				 "-shelltool")
				(t
				 (signal 'error
					 (list
				  "energize-vi-terminal-emulator unknown"
					  energize-vi-terminal-emulator))))
			  (if edited-p '("-search") '())))
	     (setq energize-vi-procs (cons (cons proc file) energize-vi-procs))
	     (set-process-sentinel proc 'energize-vi-sentinel)
	     (set-process-filter proc 'energize-vi-filter))
	    (t
	     ;; ### hack a %-spec for line number info
;;	     (setq proc (start-process procname nil program name))
	     (setq proc (start-process procname nil "/bin/sh" "-c"
				       (concat program " " name)))
	     (setq energize-other-external-editor-procs
		   (cons (cons proc file)
			 energize-other-external-editor-procs))
	     (set-process-sentinel proc 'energize-external-editor-sentinel)
	     (set-process-filter proc 'energize-external-editor-filter))))
    (if edited-p
	(while (progn (accept-process-output)
		      (eq 'run (process-status proc)))
	  (sleep-for 1))
      (accept-process-output)
      (sit-for 1))
    (if (eq 0 (process-exit-status proc)) ; don't blow away error message
	(message "%s Done." msg))
    nil))

(defun energize-external-editor-sentinel-1 (process state name list-var)
  (let ((rest (symbol-value list-var))
	(got-it nil))
    (let ((inhibit-quit t))
      (while rest
	(if (eq process (car (car rest)))
	    (progn
	      (set list-var (delq (car rest) (symbol-value list-var)))
	      (setq got-it (car rest)
		    rest nil)))
	(setq rest (cdr rest))))
    (if got-it
	(progn
	  (energize-notice-external-editor-termination (cdr got-it))

	  (if (and state (string-match "\n+\\'" state))
	      (setq state (substring state 0 (match-beginning 0))))
	  (if (and (not (eq 0 (process-exit-status (car got-it))))
		   state
		   (not (equal state "")))
	      (error "%s: %s" (process-name process) state))

;	  (if (not (eq 0 (process-exit-status (car got-it))))
;	      (error "vi process exited with code %s" 
;		     (process-exit-status (car got-it))))

	  )
      (beep)
      (message "unknown %s process died with %S" name state))))

(defun energize-vi-sentinel (process state)
  (energize-external-editor-sentinel-1 process state "vi" 'energize-vi-procs))

(defun energize-external-editor-sentinel (process state)
  (energize-external-editor-sentinel-1
   process state "external editor" 'energize-other-external-editor-procs))

(defun energize-external-editor-filter (process output)
  (beep)
  (message (format "%s" output)))

(defun energize-vi-filter (process output)
  ;; this will only get called if energize_vi (or the xterm) print error msgs
  (energize-external-editor-filter process output))

(defun energize-notice-external-editor-termination (filename)
  ;;
  ;; when an external vi process edits, automatically revert any buffers
  ;; associated with the file that was being edited, unless those buffers
  ;; are modified (which shouldn't really happen in the vi model or doing
  ;; things.)
  ;;
  ;; ## Since one vi process may have edited several files, possibly we
  ;; ## should map over all energize files and revert as appropriate instead
  ;; ## of only checking the file that the vi in question was started to edit.
  ;;
  (let ((buffer (get-file-buffer filename)))
    (cond ((null buffer)
	   nil)
	  ((verify-visited-file-modtime buffer)
	   nil)
	  ((buffer-modified-p buffer)	; Hey, how'd that happen?
	   (if (not (file-exists-p filename))
	       (error "File %s no longer exists!  Buffer modified!"
		      (file-name-nondirectory filename))
	     (beep)
	     (if (yes-or-no-p
		  (format "File %s changed on disk.  Discard your edits? "
			  (file-name-nondirectory filename)))
		 (save-excursion
		   (set-buffer buffer)
		   (revert-buffer t t)))))
;	  ((not (file-exists-p filename)) ; File gone; kill buffer.
;	   (kill-buffer buffer))
	  (t ; not modified; just revert
	   (let* ((w (get-buffer-window buffer)) ; let's not thrash so much
		  (p (and w (window-point w)))
		  (s (and w (window-start w))))
	     (save-excursion
	       (set-buffer buffer)
	       (revert-buffer t t))
	     (if p (set-window-point w p))
	     (if s (set-window-start w s)))))))


;; evi mode

(defun energize-evi-mode ()
  (evi)
  ;; vi users like to be able to edit read-only files, but we shouldn't
  ;; let them edit the Energize non-file buffers.
  (if (and (energize-buffer-p (current-buffer))
	   (or (null buffer-file-name)
	       (memq (energize-buffer-type (current-buffer))
		     '(energize-top-level-buffer energize-browser-buffer
		       energize-error-log-buffer energize-includers-buffer))))
      (setq buffer-read-only t))
  )


(defun energize-external-editor-set-mode (buffer)
  ;; If an external editor is in use, then source buffers should be
  ;; read-only in one manner or another.
  (save-excursion
    (set-buffer buffer)
    (let* ((type (energize-buffer-type buffer))
	   (debugger-p (eq type 'energize-debugger-buffer))
	   (always-editable-p (memq type '(energize-project-buffer
					   energize-debugger-buffer
					   energize-breakpoint-buffer
					   )))
	   (editable-p (or always-editable-p
			   (and (null energize-external-editor)
				(not buffer-read-only)))))

      (or editable-p (setq buffer-read-only t))
      (cond (editable-p
	     (cond ((eq energize-internal-editor nil)
		    nil)
		   ((eq energize-internal-editor 'vi)
		    (if (not debugger-p)
			(energize-evi-mode)))
		   (t
		    (signal 'error
			    (list "unknown value for energize-internal-editor"
				  energize-internal-editor)))))
	    ((eq energize-internal-viewer 'vi)
	     (energize-evi-mode))
	    ((eq energize-internal-viewer 'less)
	     ;; put it in view-mode, but don't change the major-mode symbol
	     ;; so that the buffers go in the appropriate screens.
	     (let ((major-mode major-mode))
	       (view-mode)))
	    ((eq energize-internal-viewer 'nil)
	     nil)
	    (t
	     (signal 'error
		     (list "unknown value for energize-internal-viewer"
			   energize-internal-viewer)))))))

(defun external-editor-hack-popup (choices)
  (if energize-external-editor
      (let ((rest choices)
	    file)
	(while rest
	  (if (and (vectorp (car rest))
		   (equal "editfile" (aref (car rest) 0))
		   (>= (length (car rest)) 4)
		   (stringp (setq file (aref (car rest) 3))))
	      (progn
		(setcdr rest
			(cons (vector "View File"
				      (list 'pop-to-buffer
					    (list 'find-file-noselect file))
				      t file)
			      (cdr rest)))
		(setq rest nil)))
	  (setq rest (cdr rest)))))
  choices)



(defvar energize-search-match-data nil)

(defun energize-search-internal (args)
  (let* ((case-fold-search (not (nth 0 args)))
	 (regexp-p (nth 1 args))
	 (back-p (not (nth 2 args)))
	 (search (nth 3 args))
	 (replace (nth 4 args))
	 (fn (if back-p
		 (if regexp-p 're-search-backward 'search-backward)
	       (if regexp-p 're-search-forward 'search-forward))))
    (setq this-command 'energize-search)
    (cond ((equal search "")
	   (setq energize-search-match-data nil)
	   (error "No search string specified")))
    (cond ((consp replace)
	   ;; the "replace all" button was selected
	   (setq replace (car replace))
	   ;; replace the one just searched for, if any
	   (cond ((and (eq last-command 'energize-search)
		       energize-search-match-data)
		  (store-match-data energize-search-match-data)
		  (if back-p
		      (save-excursion
			(replace-match replace nil (not regexp-p)))
		    (replace-match replace nil (not regexp-p)))))
	   ;; now replace all the rest
	   (let ((count 0))
	     (while (funcall fn search nil t)
	       (if back-p
		   (save-excursion
		     (replace-match replace nil (not regexp-p)))
		 (replace-match replace nil (not regexp-p)))
	       (setq count (1+ count)))
	     (message "%d replacement%s done." count (if (= count 1) "" "s")))
;;	   (setq this-command nil)
	   (setq energize-search-match-data nil))
	  (t
	   ;; otherwise, one of the search buttons was selected
	   (cond (replace
		  (or (eq last-command 'energize-search) ; fuck!!
		      (error "Last command was not a successful search."))
		  (or energize-search-match-data (error "Last search failed"))
		  (store-match-data energize-search-match-data)
		  (if back-p
		      (save-excursion
			(replace-match replace nil (not regexp-p)))
		    (replace-match replace nil (not regexp-p)))))
	   (setq energize-search-match-data nil)
	   (or (funcall fn search nil t)
	       (signal 'error
		       (list (cond ((and back-p regexp-p)
				    "Reverse regexp search failed")
				   (back-p "Reverse search failed")
				   (regexp-p "Regexp search failed")
				   (t "Search failed"))
			     search)))
	   (cond (zmacs-regions
		  (push-mark (if (= (point) (match-beginning 0))
				 (match-end 0)
			       (match-beginning 0))
			     t)
		  (zmacs-activate-region)))
	   (setq energize-search-match-data (match-data))
	   ))))


(defvar energize-edit-modes-specified nil)

(defun energize-set-edit-modes-minibuf-prompt ()
  (let* ((ee (completing-read "External editor (RET for none): "
			      nil nil nil nil t))
	 (te (if (equal ee "vi")
		 (completing-read
  "Terminal emulator in which to run vi (xterm, shelltool, or cmdtool): "
		  '(("xterm") ("shelltool") ("cmdtool")) nil t nil t)
	       ""))
	 (iv (completing-read
	      "View buffers using which keybindings (emacs, vi, or less): "
	      '(("emacs") ("vi") ("less")) nil t nil t))
	 (ie (completing-read
	      "Edit other buffers using which keybindings (emacs or vi): "
	      '(("emacs") ("vi")) nil t nil t))
	 (ms (y-or-n-p "Use multiple windows? "))
	 (sp (y-or-n-p "Split screens? "))
	 )
    (if (equal ee "") (setq ee nil))
    (if (equal te "") (setq te "xterm"))
    (if (equal iv "") (setq iv "emacs"))
    (if (equal ie "") (setq ie "emacs"))
    (list ee (intern te) (intern iv) (intern ie) ms sp)))

(defun energize-set-edit-modes-dbox-prompt ()
  (let* ((ee (cond ((member energize-external-editor '("" "emacs" nil)) 0)
		   ((member energize-external-editor '("vi" vi))
		    (cond ((eq energize-vi-terminal-emulator 'xterm) 1)
			  ((eq energize-vi-terminal-emulator 'cmdtool) 2)
			  (t (error "unrecognised terminal emulator"))))
		   (t 3)))
	 (o (if (and (stringp energize-external-editor)
		     (not (equal energize-external-editor "vi")))
		energize-external-editor
	      ""))
	 (iv (cond ((memq energize-internal-viewer '(nil emacs)) 0)
		   ((eq energize-internal-viewer 'vi) 1)
		   ((eq energize-internal-viewer 'less) 2)
		   (t (error "unrecognised internal-viewer"))))
	 (ie (cond ((memq energize-internal-editor '(nil emacs)) 0)
		   ((eq energize-internal-editor 'vi) 1)
		   (t (error "unrecognised internal-editor"))))
	 (ms (cond ((memq energize-screen-mode '(nil single)) 0)
		   ((eq energize-screen-mode 'several) 1)
		   ((eq energize-screen-mode 'multi) 2)
		   (t (error "unrecognised screen-mode"))))
	 (sp (if energize-split-screens-p 1 0))

	 (result (energize-edit-mode-prompt ee ie iv o ms sp))
	 )
    (setq ee (nth 0 result)
	  iv (nth 1 result)
	  ie (nth 2 result)
	  o  (nth 3 result)
	  ms (nth 4 result)
	  sp (nth 5 result))
    (list (cond ((= ee 0) nil)
		((= ee 1) "vi")
		((= ee 2) "vi")
		((= ee 3) o)
		(t (error "ee losing")))
	  (cond ((= ee 1) 'xterm)
		((= ee 2) 'cmdtool)
		(t nil))
	  (cond ((= iv 0) 'emacs)
		((= iv 1) 'vi)
		((= iv 2) 'less)
		(t (error "iv losing")))
	  (cond ((= ie 0) 'emacs)
		((= ie 1) 'vi)
		(t (error "ie losing")))
	  (cond ((= ms 0) 'single)
		((= ms 1) 'several)
		((= ms 2) 'multi)
		(t (error "ms losing")))
	  (cond ((= sp 0) 'nil)
		((= sp 1) 't)
		(t (error "sp losing")))
	  )))

(defun energize-set-edit-modes (external-editor
				terminal-emulator
				internal-viewer
				internal-editor
				multi-screen-p
				split-screens-p)
  "Prompts for the various edit and view modes of Energize.

The \"external editor\" is the editor which Energize should use when 
you ask it to edit a file.  If you simply hit return, the files will 
be edited in Lucid Emacs.  The default for this is taken from the
environment variable $ENERGIZE_EXTERNAL_EDITOR.

If you specify \"vi\" as your external editor, then you will be asked
which terminal emulator window should be launched to run the vi (either
xterm, shelltool, or cmdtool.)  The default for this is taken from the
environment variable $ENERGIZE_VI_WRAPPER.

Many of the Energize buffers (such as the Browsers) are not editable.
You have three choices for which keybindings you would like to use for
moving and searching around in those buffers: \"emacs\", \"vi\", or 
\"less\" (which is a popular variant of \"more\").  The default for 
this is taken from the environment variable $ENERGIZE_VIEW_MODE.

Some Energize buffers (such as the Project buffers) are editable, and
an external editor cannot be used on them.  For these buffers, you have
a choice of either \"emacs\" or \"vi\" keybindings.  The default for 
this is taken from the environment variable $ENERGIZE_EDIT_MODE.

If you are not using an external editor, then specifying \"vi\" here
means that evi, the emacs vi emulator, will be used to edit your source
files as well.

You will also be asked whether Energize should automatically pop up multiple
windows (\"screens\" in emacs terminology) or should use and reuse one only
\(which is the traditional emacs model.)  The default for this is taken from
the environment variable $ENERGIZE_MULTI_SCREEN_MODE.

Sometimes Energize wants to display two buffers at once, for example, the
Debugger buffer and the source file corresponding to the current stack frame.
You have two choices for how this should happen: one is to display the source
in the debugger screen, `splitting' the screen vertically, and the other is to
use two screens, one for the debugger and one for the source.  The default for
this is taken from the envvironment variable $ENERGIZE_SPLIT_SCREENS_P."
;;  (interactive (energize-set-edit-modes-minibuf-prompt))
  (interactive (energize-set-edit-modes-dbox-prompt))
  (if (null terminal-emulator) (setq terminal-emulator 'xterm))
  (if (equal energize-external-editor "emacs")
      (setq energize-external-editor nil))

  (or (null external-editor)
      (stringp external-editor)
      (signal 'wrong-type-argument (list 'stringp external-editor)))
  (or (symbolp terminal-emulator)
      (signal 'wrong-type-argument (list 'symbolp terminal-emulator)))
  (or (symbolp internal-viewer)
      (signal 'wrong-type-argument (list 'symbolp internal-viewer)))
  (or (symbolp internal-editor)
      (signal 'wrong-type-argument (list 'symbolp internal-editor)))
  (or (memq split-screens-p '(t nil))
      (error "split-screens-p must be t or nil"))

  (cond ((equal external-editor "")   (setq energize-external-editor nil))
	((equal external-editor "vi") (setq energize-external-editor 'vi))
	(t (setq energize-external-editor external-editor)))

  (if (eq internal-viewer 'emacs) (setq internal-viewer nil))
  (if (eq internal-editor 'emacs) (setq internal-editor nil))
  (setq energize-vi-terminal-emulator terminal-emulator)
  (setq energize-internal-editor internal-editor)
  (setq energize-internal-viewer internal-viewer)

  (cond ((and (null energize-internal-viewer)
	      (null energize-internal-editor))
	 ;; Emacs all the way.  They must be clueful.
;;	 (remove-hook 'energize-disconnect-hook 'save-buffers-kill-emacs)
	 (setq pop-up-windows energize-split-screens-p))
	(t
;;	 (add-hook 'energize-disconnect-hook 'save-buffers-kill-emacs)
	 (setq pop-up-windows nil)))

  (cond (energize-external-editor
	 (add-hook 'energize-disconnect-hook 'save-buffers-kill-emacs)
	 (energize-external-editor-set-menubar))
	(t
	 (remove-hook 'energize-disconnect-hook 'save-buffers-kill-emacs)
	 (energize-internal-editor-set-menubar)))

  (cond ;((eq multi-screen-p energize-screen-mode)
	; nil)
	((memq multi-screen-p '(t multi))
	 (energize-multi-screen-mode))
	((memq multi-screen-p '(nil single))
	 (energize-single-screen-mode))
	((memq multi-screen-p '(several))
	 (energize-several-screens-mode))
	(t (error "multi-screen-p %S unrecognised" multi-screen-p)))

  (setq energize-split-screens-p split-screens-p)

  (let ((buffers (buffer-list)))
    (save-excursion
      (while buffers
	(if (energize-buffer-p (car buffers))
	    (progn
	      (set-buffer (car buffers))
	      (normal-mode)
	      (energize-buffer-creation-hook-function (car buffers))))
	(setq buffers (cdr buffers)))))
  (setq energize-edit-modes-specified t)
  nil)

(defun energize-hack-external-editor-mode ()
  (if energize-edit-modes-specified
      nil
    (condition-case c
	(let ((ee (getenv "ENERGIZE_EXTERNAL_EDITOR"))
	      (te (getenv "ENERGIZE_VI_WRAPPER"))
	      (iv (getenv "ENERGIZE_VIEW_MODE"))
	      (ie (getenv "ENERGIZE_EDIT_MODE"))
	      (ms (getenv "ENERGIZE_MULTI_SCREEN_MODE"))
	      (sp (getenv "ENERGIZE_SPLIT_SCREENS_P"))
	      )
	  (if (member ee '("" nil)) (setq ee nil))
	  (if (member te '("" nil)) (setq te "xterm"))
	  (if (member iv '("" nil)) (setq iv "emacs"))
	  (if (member ie '("" nil)) (setq ie "emacs"))
	  (if (member sp '("" nil)) (setq sp "yes"))
	  (if ms (setq ms (downcase ms)))
	  (setq sp (downcase sp))
	  (let ((standard-output (function external-debugging-output)))
	    (if (member te '("xterm" "shelltool" "cmdtool"))
		nil
	      (princ (format
      "$ENERGIZE_VI_WRAPPER is %S, not xterm, shelltool, or cmdtool.\n" te))
	      (setq te nil))
	    (if (member iv '("emacs" "vi" "less"))
		nil
	      (princ (format
		      "$ENERGIZE_VIEW_MODE is %S, not emacs, vi, or less.\n"
		      iv))
	      (setq iv nil))
	    (if (member ie '("emacs" "vi" "less"))
		nil
	      (princ (format
		      "$ENERGIZE_EDIT_MODE is %S, not emacs or vi.\n" ie))
	      (setq ie nil))
	    (cond ((member ms '("yes" "y" "true" "on" "1" "" "multi" "many"
				"often"))
		   (setq ms 'multi))
		  ((member ms '("no" "n" "false" "off" "0" "single"
				"never"))
		   (setq ms 'single))
		  ((member ms '("several" "some" "few" "sometimes"))
		   (setq ms 'several))
		  (t
		   (if ms
		       (princ
			(format
	 "$ENERGIZE_MULTI_SCREEN_MODE was %S, not often, sometimes, or never\n"
			 ms)))
		   (setq ms (or energize-screen-mode
				(if (and (null ee)
					 (equal iv "emacs")
					 (equal ie "emacs"))
				    'single
				  'multi)))))
	    (cond ((member sp '("yes" "y" "true" "on" "1" ""))
		   (setq sp t))
		  ((member sp '("no" "n" "false" "off" "0"))
		   (setq sp nil))
		  (t
		   (princ
		    (format "$ENERGIZE_SPLIT_SCREENS_P was %S, not a boolean.\n"
			    sp))))
	    )
	  (energize-set-edit-modes ee (intern te) (intern iv) (intern ie) ms sp)
	  )
      ;; condition-case
      (error
       (let ((standard-output (function external-debugging-output)))
	 (princ "Internal error: %S\n" c))))))


;;; Make the buffers menu say "view file" if in external-editor mode
;;; Originally defined in menubar.el

(defun format-buffers-menu-line (buffer)
  "Returns a string to represent the given buffer in the Buffer menu.
nil means the buffer shouldn't be listed.  You can redefine this."
  (if (string-match "\\` " (setq buffer (buffer-name buffer)))
      nil
    (if energize-external-editor
	(concat
;	 (if (buffer-file-name (get-buffer buffer))
;	     "View File "
;	   "View Buffer ")
	 "View "
	 buffer)
      buffer)))
