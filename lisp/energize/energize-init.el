;;; -*- Mode:Emacs-Lisp -*-


(defun energize-multi-screen-mode ()
  "Call this function to put Energize into multi-screen mode.
A single screen (emacs X Window) will be created for the debugger buffer,
and a new screen will be created for each Browser buffer."
  (interactive)
  (put 'energize-debugger-mode 'screen-name 'debugger)
  (put 'energize-browser-mode 'screen-name 'browser)
;  (put 'browser 'instance-limit 0)
  (put 'energize-manual-entry-mode 'screen-name 'manual)
  t)

(defun energize-single-screen-mode ()
  "Call this function to put Energize into single-screen mode.
All buffers will be displayed in the currently selected screen."
  (interactive)
  (remprop 'energize-debugger-mode 'screen-name)
  (remprop 'energize-browser-mode 'screen-name)
;  (remprop 'browser 'instance-limit)
  (remprop 'energize-manual-entry-mode 'screen-name)
  nil)

(energize-single-screen-mode)

(defvar energize-auto-raise-screen t
  "If T screens are automatically raised when Energize wants to show them.")

(defvar energize-auto-deiconify-screen ()
  "If T screens are automatically deiconified when Energize wants to show them.")

(setq before-change-function 'notify-send-buffer-modified-request)
(setq first-change-function 'energize-barf-if-buffer-locked)

(defun notify-send-buffer-modified-request (start end)
  ;; This gets pretty deep, and is called when things are already pretty
  ;; deep, so we've got to bump the limits a bit.
;  (let ((max-specpdl-size (* 2 max-specpdl-size))
;	(max-lisp-eval-depth (* 2 max-lisp-eval-depth)))
    (send-buffer-modified-request t start end))
;  )

(setq energize-kernel-modification-hook nil)
(setq energize-buffer-modified-hook 'send-buffer-modified-request)
(setq energize-create-buffer-hook 'energize-buffer-creation-hook-function)


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

(defmacro pop (l)
  (list 'prog1 (list 'car l) (list 'setq l (list 'cdr l))))

(defmacro push (o l)
  (list 'setq l (list 'cons o l)))

;;
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

(defun energize::make-many-buffers-visible (buffer-extent-list go-there)
  "First arg is a list of buffers and extents. All those should be
made visible at the same time.  If the second argument is T then point
should be moved to the first character of the extent of the first
buffer, or to the buffer if no extent is specified for this buffer.  
If second argument is NIL point should not change."
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
				     (make-screen-visible screen))))))))))

;;;

(defun get-any-buffer-size (buffer)
  (if (eq buffer (current-buffer))
      (buffer-size)
      (save-excursion
        (set-buffer buffer)
        (buffer-size))))

(defun menu-extent-at (pos buffer)
  (if (null pos)
      nil
    (extent-at pos buffer 'menu)))

;;; functions to execute the menu with the keyboard

(defun default-selection-value-for-item (menu-item)
  (let ((flags (aref menu-item 3)))
    (cond ((= (logand flags 2) 2)
	   (if (x-selection-owner-p 'PRIMARY)
	       (x-get-selection-internal 'PRIMARY 'STRING)))
	  ((= (logand flags 4) 4)
	   (if (x-selection-owner-p 'PRIMARY)
	       (x-get-selection-internal 'PRIMARY 'ENERGIZE_OBJECT)))
	  ((= (logand flags 128) 128)
	   (if (x-selection-owner-p 'SECONDARY)
	       (x-get-selection-internal 'SECONDARY 'STRING)))
	  ((= (logand flags 256) 256)
	   (if (x-selection-owner-p 'SECONDARY)
	       (x-get-selection-internal 'SECONDARY 'ENERGIZE_OBJECT))))))
  

(defun energize-execute-menu-item-with-selection (buffer
						  extent
						  item
						  selection
						  no-confirm)
  (if (/= 0 (logand 1 (aref item 3)))
      (error "The `%s' command is inappropriate in this context"
	     (aref item 0)))
  (if (null selection)
      (setq selection (default-selection-value-for-item item)))
  (energize-execute-menu-item buffer extent item selection no-confirm))

  
(defun execute-energize-choice (name &optional use-background-menu selection
				     no-confirm)
  (interactive "sExecute Energize choice named: ")
  (if (not (stringp name))
      (error "Can't execute a choice, %s, that is not a string" name))
  (if energize-kernel-busy
      (error "Can't execute Energize command %s, server is busy" name))
  (let* ((b (current-buffer))
	 (extent (if use-background-menu
		     nil
		     (let ((tmp (menu-extent-at (point) b)))
		       (if (energize-extent-menu-p tmp) 
			   tmp
			   nil))))
	 (i (energize-list-menu b extent name)))
    (if (not i) (error "No choice named %s" name))
    (energize-execute-menu-item-with-selection b extent i selection
					       no-confirm)))

;;; function to connect to energize

(defun connect-to-energize (server &optional enarg)
  "Connect this emacs to a Energize server.
The SERVER argument should be the name of the host that the kernel is
running on (empty-string for localhost).  It may also be of the form
``hostname:user'' or ``:user'', meaning to use the server running with
userid USER."
  (interactive (if (connected-to-energize-p)
		   (error "Already connected to the server.") ; you bogon.
		 (list (read-string "connect to energize server: "))))
;; line info column is broken for now
;;  (x-show-lineinfo-column)
  (if (connected-to-energize-p)
      (error "Already connected to the server.")) ; you bogon.
  (if (or (null server)  (equal server ""))
      (setq server (system-name)))
  (connect-to-energize-internal server enarg))

(defun disconnect-from-energize ()
  (interactive)
  "Close the connection to energize"
;;##  (x-hide-lineinfo-column)
  (close-connection-to-energize))


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
		 (after-find-file)
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

(defun request-energize-extent-menu (event)
  (interactive "e")
  "Bring up te energize menu for the extent or beeps if there is no extent
below the mouse"
  (display-menu (function (lambda (buffer pos s e)
		  (let ((extent (if (extentp (event-glyph event))
				    (event-glyph event)
				    (menu-extent-at pos buffer))))
		    (if (or (null extent)
			    (not (energize-extent-menu-p extent)))
			(error "No energize menu here")
			(force-highlight-extent extent t)
			(sit-for 0)
			(let ((item (energize::request-menu buffer extent)))
			  (if item
			      (energize-execute-menu-item-with-selection
			       buffer extent item nil nil)))
			; do not dehighlight the extent until mouse moves
			;  -- this is bug 10602
			; (force-highlight-extent extent ())
			))))
		event))

(defun request-energize-buffer-menu (event)
  (interactive "e")
  "Bring up the energize menu for the buffer"
  (display-menu (function (lambda (buffer pos s e)
			    (let ((item (energize::request-menu buffer nil)))
			      (if item
				  (energize-execute-menu-item-with-selection
				   buffer nil item nil nil)))))
                event))

(defun display-menu (menu-proc event)
  "Call the menu-proc with information gotten from arg (the buffer, 
cursor position, and selection positions) so that it can bring up a menu."
  (if energize-kernel-busy
      (error "Can't activate Energize menu, server busy"))
  (let ((buffer (window-buffer (event-window event)))
	(pos (event-point event)))
    (or (bufferp buffer)
        (error (format "event's buffer, %s, isn't a buffer" buffer)))
    (select-window (event-window event))
    (funcall menu-proc buffer pos nil nil)))


;;; Here's a converter that makes emacs understand how to convert to
;;; selections of type ENERGIZE.  Eventually the Energize server won't
;;; be using the selection mechanism any more, I hope.

(defun xselect-convert-to-energize (selection type value)
  (let (str id start end tmp)
    (cond ((and (consp value)
		(markerp (car value))
		(markerp (cdr value)))
	   (setq id (energize-buffer-id (marker-buffer (car value)))
		 start (1- (marker-position (car value)))  ; zero based
		 end (1- (marker-position (cdr value)))))
	  ((extentp value)
	   (setq id (extent-to-generic-id value)
		 start 0
		 end 0)))
    (if (null id)
	nil
      (setq str (make-string 12 0))
      (if (< end start) (setq tmp start start end end tmp))
      (aset str 0 (logand (ash (car id) -8) 255))
      (aset str 1 (logand (car id) 255))
      (aset str 2 (logand (ash (cdr id) -8) 255))
      (aset str 3 (logand (cdr id) 255))
      (aset str 4 (logand (ash start -24) 255))
      (aset str 5 (logand (ash start -16) 255))
      (aset str 6 (logand (ash start -8) 255))
      (aset str 7 (logand start 255))
      (aset str 8 (logand (ash end -24) 255))
      (aset str 9 (logand (ash end -16) 255))
      (aset str 10 (logand (ash end -8) 255))
      (aset str 11 (logand end 255))
      (cons 'ENERGIZE_OBJECT str))))


(or (assq 'ENERGIZE_OBJECT selection-converter-alist)
    (setq selection-converter-alist
	  (cons '(ENERGIZE_OBJECT . xselect-convert-to-energize)
		selection-converter-alist)))
