;;; -*- Mode:Emacs-Lisp -*-

(defmacro pop (l)
  (list 'prog1 (list 'car l) (list 'setq l (list 'cdr l))))

(defmacro push (o l)
  (list 'setq l (list 'cons o l)))

;;; Function to present buffers as requested by the energize kernel

(defvar energize-auto-raise-screen t
  "If T screens are automatically raised when Energize wants to show them.")

(defvar energize-auto-deiconify-screen ()
  "If T screens are automatically deiconified when Energize wants to
show them.")

(defun energize-request-kill-buffer-if-dead (buffer)
  (cond ((not (bufferp buffer)) t)
        ((null (buffer-name buffer))
         (if (energize-buffer-p buffer)
             (energize-request-kill-buffer buffer))
         t)
        (t nil)))

(defun energize-prune-killed-buffers-from-list (buffer-extent-list)
  (let ((rest buffer-extent-list)
        (buffer-count 0)
        (deleted-count 0))
    (while rest
      (let* ((buffer (car rest))
             (extent (car (cdr rest))))
        (setq rest (cdr (cdr rest)))
        (setq buffer-count (1+ buffer-count))
        (if (energize-request-kill-buffer-if-dead buffer)
            (progn
              (setq deleted-count (1+ deleted-count))
              (setq buffer-extent-list (delq buffer buffer-extent-list))
              (setq buffer-extent-list (delq extent buffer-extent-list))))))
    (if (> deleted-count 0)
        (progn
          (message 
            (format "Oops, confused about %s selected %s -- please try again."
                    (if (> deleted-count 1) 
                        (format "%d of the" deleted-count)
                        (if (> buffer-count 1)
                            "one of the"
                            "the"))
                    (if (> buffer-count 1)
                        "buffers"
                        "buffer")))
          (ding t)))
    buffer-extent-list))

(defun energize-list-windows (screen)
  (let ((windows ())
	(window (screen-root-window screen))
	(first-window))
    (while (null (window-buffer window))
      (setq window (next-window window 'no-minibuffer)))
    (setq first-window window
	  windows (list window))
    (while (not (eq first-window
		    (setq window (next-window window 'no-minibuffer))))
      (push window windows))
    (nreverse windows)))

(defun energize-show-one-buffer (buffer extent window go-there only-one)
  (let ((result nil)
	(name (buffer-name buffer)))
    (if window
	(set-window-buffer window buffer)
      (setq window (display-buffer buffer))
      (set-window-buffer-dedicated window buffer)
      (setq result window))
    (if extent
	(let ((pos (extent-start-position extent)))
	  (set-window-point window pos)
	  (cond ((equal name "Error Log")
		 ;; scroll the Error Log buffer so that the first error
		 ;; is at the top of the window.
		 (set-window-start window
				   (save-excursion
				     (set-buffer buffer)
				     (goto-char pos)
				     (beginning-of-line)
				     (point))))
		((and only-one
		      (eq 'energize-source-buffer
			  (energize-buffer-type buffer)))
		 ;; if only one buffer is requested to be visible and it
		 ;; is a source buffer then scroll point close to the top
		 (set-window-start window
				   (save-excursion
				     (set-buffer buffer)
				     (goto-char pos)
				     (beginning-of-line)
				     (if (> (window-height window)
					    next-screen-context-lines)
					 (vertical-motion
					  (- next-screen-context-lines)
					  window)
				       (vertical-motion -1 window))
				     (point))))))
      ;; else
      (if (or (equal name "*Debugger*") (equal name "Error Log"))
	  ;; Debugger and Error Log buffers always get scrolled to
	  ;; the bottom when displayed.
	  (set-window-point window
			    (save-excursion (set-buffer buffer)
					    (+ 1 (buffer-size))))))
    (if go-there
	(select-window window))
    result))

(defun energize-show-make-windows (n)
  ;; if more than one buffer is being displayed, then make there be
  ;; exactly enough windows to display them; no more, no less.
  (let ((windows (energize-list-windows (selected-screen))))
    (if (= (length windows) (/ n 2))
	;; right number of windows, leave their sizes alone.
	windows
      (delete-other-windows)
      (let ((i (/ n 2)))
	(while (> i 1)
	  (split-window)
	  (setq i (- i 1))))
      (energize-list-windows (selected-screen)))))

(defun energize-show-all-buffers (buffer-extent-list go-there)
  (let ((windows ())
	(only-one (null (cdr (cdr buffer-extent-list)))))
    (if (cdr (cdr buffer-extent-list))
	(setq windows
	      (energize-show-make-windows (length buffer-extent-list))))
    (let ((dedicated-windows ()))
      (unwind-protect
	  (while buffer-extent-list
	    (let ((window (energize-show-one-buffer (pop buffer-extent-list)
						    (pop buffer-extent-list)
						    (and windows (pop windows))
						    go-there
						    only-one)))
	      (if window
		  (push window dedicated-windows))
	      (setq go-there nil)))
	;; protected
	(while dedicated-windows
	  (set-window-buffer-dedicated (pop dedicated-windows) nil))))))

(defun energize-main-buffer-of-list (list)
  ;; Given an alternating list of buffers and extents, pick out the
  ;; "interesting" buffer.  If one of the buffers is in debugger-mode,
  ;; then that's the interesting one; otherwise, the last buffer in
  ;; the list is the interesting one.
  (let (buffer mode result)
    (while list
      (setq buffer (car list))
      (or (memq mode '(energize-debugger-mode))
	  (setq result buffer
		mode (save-excursion (set-buffer buffer) major-mode)))
      (setq list (cdr (cdr list))))
    result))

(defun energize-make-buffers-visible (buffer-extent-list go-there)
  (let ((main-buffer (energize-main-buffer-of-list buffer-extent-list))
	window)
    ;; This may create and/or select a screen as a side-effect.
    ;; I'm not sure it's necessary to call this, as display-buffer
    ;; calls it too.  But it can't hurt to select the appropriate
    ;; screen early...
    (cond ((get-screen-name-for-buffer main-buffer)
	   (get-screen-for-buffer main-buffer))
	  ((setq window (get-buffer-window main-buffer t))
	   (if window
	       (select-screen (window-screen window)))))
    (energize-show-all-buffers buffer-extent-list go-there)))

(defun energize-make-many-buffers-visible-function (arg)
  "First arg is a list of buffers and extents. All those should be
made visible at the same time.  If the second argument is T then point
should be moved to the first character of the extent of the first
buffer, or to the buffer if no extent is specified for this buffer.  
If second argument is NIL point should not change."
  (let ((buffer-extent-list (car arg))
	(go-there (cdr arg)))
    (setq buffer-extent-list 
	  (energize-prune-killed-buffers-from-list buffer-extent-list))
    (if buffer-extent-list
	(progn (energize-make-buffers-visible buffer-extent-list go-there)
	       (let ((screen (selected-screen)))
		 (if go-there
		     (cond ((not (screen-visible-p screen))
			    (if energize-auto-deiconify-screen
				(progn (sit-for 0)
				       (make-screen-visible screen))))
			   ((not (screen-totally-visible-p screen))
			    (if energize-auto-raise-screen
				(progn (sit-for 0)
				       (make-screen-visible screen)))))))))))

(defvar energize-make-many-buffers-visible-should-enqueue-event t
  "Special variable bound by energize-execute-command to allow the
buffers to be selected while the command is executed")

(defun energize-make-many-buffers-visible (buffer-extent-list go-there)
  "First arg is a list of buffers and extents. All those should be
made visible at the same time.  If the second argument is T then point
should be moved to the first character of the extent of the first
buffer, or to the buffer if no extent is specified for this buffer.  
If second argument is NIL point should not change."
  (if energize-make-many-buffers-visible-should-enqueue-event
      (enqueue-eval-event 'energize-make-many-buffers-visible-function
			  (cons buffer-extent-list nil))
    (energize-make-many-buffers-visible-function
     (cons buffer-extent-list t))))

;;; Multi and single screen modes

(defun energize-multi-screen-mode ()
  "Call this function to put Energize into multi-screen mode.
A single screen (emacs X Window) will be created for the debugger buffer,
and a new screen will be created for each Browser buffer."
  (interactive)
  (put 'energize-debugger-mode 'screen-name 'debugger)
  (put 'energize-browser-mode 'screen-name 'browser)
  (put 'energize-manual-entry-mode 'screen-name 'manual)
  t)

(defun energize-single-screen-mode ()
  "Call this function to put Energize into single-screen mode.
All buffers will be displayed in the currently selected screen."
  (interactive)
  (remprop 'energize-debugger-mode 'screen-name)
  (remprop 'energize-browser-mode 'screen-name)
  (remprop 'energize-manual-entry-mode 'screen-name)
  nil)

(energize-single-screen-mode)

;;; Function to initialize the energize faces

(setq energize-attributes-mapping 
  '((51 attributeSectionHeader) (54 attributeBrowserHeader)
    (68 attributeWriteProtected) (69 attributeModifiedText)
    (1 attributeBold) (2 attributeItalic) (3 attributeBoldItalic)
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

;;; Function to connect to energize

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
  (if (or (null server)  (equal server ""))
      (setq server (system-name)))
  (connect-to-energize-internal server enarg)
  ;; initialize the attribute-vector
  (energize-initialize-faces))

(defun disconnect-from-energize ()
  (interactive)
  "Close the connection to energize"
  (close-connection-to-energize))

;;; Misc Energize hook functions

;;; Buffer creation hook

(defun energize-buffer-creation-hook-function (buffer)
  (save-excursion
    (set-buffer buffer)
    (let ((type (energize-buffer-type buffer)))

      ;; ## totally evil hack; energize-buffer-type should be fixed.
      (if (eq type 'energize-top-level-buffer)
	  (cond ((string-match "^Browser" (buffer-name buffer))
		 (setq type 'energize-browser-buffer))
		((equal "Error Log" (buffer-name buffer))
		 (setq type 'energize-error-log-buffer))))
      
      (cond ((eq type 'energize-top-level-buffer)
	     (energize-top-level-mode))
	    ((eq type 'energize-browser-buffer)
	     (energize-browser-mode))
	    ((eq type 'energize-error-log-buffer)
	     (toggle-read-only 1))
	    ((eq type 'energize-project-buffer)
	     (energize-project-mode))
	    ((eq type 'energize-debugger-buffer)
	     (energize-debugger-mode))
	    ((eq type 'energize-breakpoint-buffer)
	     (energize-breakpoint-mode))
	    ((or (eq type 'energize-source-buffer)
                 (eq type 'energize-unspecified-buffer)
		 (null type))
	     (compute-buffer-file-truename)
	     (if (buffer-file-name buffer)
		 (after-find-file nil t)
	       (funcall default-major-mode))
	     ;;(energize-source-minor-mode)
	     )
	    (t
	     (signal 'error (list "unknown energize buffer type" type)))))
    ;;
    ;; If evi mode is on, then Energize-created buffers should be in
    ;; evi mode as well.  But don't turn evi-mode on in debugger-buffers,
    ;; or you won't be able to type debugger commands.  
    ;;
    (if (and (boundp 'evi-install-undo-list) evi-install-undo-list
	     (not (eq (energize-buffer-type buffer)
		      'energize-debugger-buffer)))
	(evi-mode))))

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

(setq command-switch-alist
      (append command-switch-alist
	      '(("-context"	. command-line-process-energize)
		("-energize"	. command-line-process-energize)
		("-beam-me-up"	. command-line-process-energize))))


(defun command-line-process-energize (arg)
  (let ((e-arg (car command-line-args-left))
	(e-host (getenv "ENERGIZE"))) ; maybe nil
    (if (and e-arg (string-match "\\`[0-9a-fA-F]+[,][0-9a-fA-F]+\\'" e-arg))
	(setq command-line-args-left (cdr command-line-args-left))
      (setq e-arg nil))
    (message "Connecting to Energize...") 
    (sit-for 0)
    (condition-case ()
	(connect-to-energize e-host e-arg)
      (error
       (beep)
       (message "Failed to connect to Energize at %s." e-host)
       (sit-for 1)))))
