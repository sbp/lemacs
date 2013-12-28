;;; -*- Mode:Emacs-Lisp -*-
;;; Copyright © 1992 by Lucid, Inc.  All Rights Reserved.

;;; Displaying buffers.  Why is this so hard?


;;; This crud is damage control, because sometimes things get confused, and
;;; the server asks us to display a buffer that has been killed.  

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


(defvar energize-auto-scroll-p t	;#### this should be nil, t is LOSING
  "*If t, energize will scroll your debugger and error log buffers
to the bottom whenever output appears with reckless abandon.  If nil,
it will behave just like normal shell and gdb-mode buffers.")

(defvar energize-error-log-context-lines 0
  "*Number of lines to skip above the current error in the Energize error log")

;;; called by energize-show-all-buffers
;;; If the extent is specified:
;;;   - scrolls the window so that point is at at the beginning of the extent.
;;;   - If the buffer is "Error Log", the extent is moved to top-of-window.
;;;   - if `only-one' and the buffer is a source buffer, then... what?
;;; If the buffer is "*Debugger*" or "Error Log", point is moved to eof,
;;;   IF and ONLY if it was at EOF already.
;;;
(defun energize-scroll-window-at-extent (window extent only-one)
  (let* ((buffer (window-buffer window))
	 (type (energize-buffer-type buffer)))
    (if (and extent (null (extent-start-position extent)))
	;; it has been detached somehow.
	(setq extent nil))
    (if extent
	(let ((pos (extent-start-position extent)))
	  (if (not (eq pos 0))
	      (progn
		(set-window-point window pos)
		(cond ((eq type 'energize-error-log-buffer)
		       ;; scroll the Error Log buffer so that the first error
		       ;; is at the top of the window.
		       (set-window-start window
					 (save-excursion
					   (set-buffer buffer)
					   (goto-char pos)
					   (forward-line
					    (-
					     energize-error-log-context-lines))
					   (beginning-of-line)
					   (point))))
		      ((and only-one (eq type 'energize-source-buffer))
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
					   (point)))))))))

    (cond ((and (memq type '(energize-error-log-buffer
			     energize-debugger-buffer))
		; don't move point if it's before the last line
		(or energize-auto-scroll-p
		    (>= (window-point window)
			(save-excursion
			  (set-buffer (window-buffer window))
			  ;;(comint-mark)
			  (energize-user-input-buffer-mark)
			  )))
		)
	   ;; Debugger and Error Log buffers generally get scrolled to
	   ;; the bottom when displayed.
	   (set-window-point window
			     (save-excursion (set-buffer buffer)
					     (+ 1 (buffer-size))))
	   ;; Careful to deactivate the selection when automatically moving
	   ;; the user to the end of the buffer.  This is suboptimal, but
	   ;; otherwise we have bad interactions with the debugger-panel
	   ;; Print button.  (Double-click on a value (point is now at the
	   ;; end of that word); hit Print; point is now at point-max, but
	   ;; the original word is still highlighted, which is incorrect -
	   ;; we're now in a state where the selection highlighting and the
	   ;; region between point and mark is out of sync.  I'm not entirely
	   ;; sure how to fix this short of using a point-motion hook of some
	   ;; kind, so we'll punt, and just deactivate the region instead.)
	   (zmacs-deactivate-region)
	   ))))


;;; called by energize-make-buffers-visible
;;; For each of the contents of an plist of buffers and extents:
;;;   - If the buffer is visible in a window
;;;     - dedicate the window
;;;     - energize-scroll-window-at-extent
;;; If we dedicated any windows, select the last one dedicated.
;;; For each of the buffers and extents:
;;;   - pop-to-buffer
;;;   - remember the first window selected in this way
;;;   - dedicate the window
;;;   - energize-scroll-window-at-extent; only-one arg is true if there
;;;     is only one buffer/extent pair in the list
;;;   - if energize-edit-buffer-externally-p make it read-only
;;; Un-dedicate the windows
;;; Select the remembered window (the first one we popped-to-buffer on)
;;; Maybe raise its screen
;;;
(defun energize-show-all-buffers (buffer-extent-list)
  (let ((pop-up-windows t)
	(dedicated-windows ())
	(buffer-extent-current)
	(window-to-select ())
	(only-one (null (cdr (cdr buffer-extent-list)))))
    (setq buffer-extent-current buffer-extent-list)
    (while buffer-extent-current
      (let* ((buffer (car buffer-extent-current))
	     (extent (car (cdr buffer-extent-current)))
	     (window (get-buffer-window buffer (selected-screen))))
	(if window
	    (progn
	      (set-window-buffer-dedicated window buffer)
	      (setq dedicated-windows (cons window dedicated-windows))
	      (energize-scroll-window-at-extent window extent only-one)))
	(setq buffer-extent-current (cdr (cdr buffer-extent-current)))))
    (if dedicated-windows
	(select-window (car dedicated-windows)))
    (setq buffer-extent-current buffer-extent-list)
    (while buffer-extent-current
      (let* ((buffer (car buffer-extent-current))
	     (extent (car (cdr buffer-extent-current))))
;; ## what was this intended to do? a screen is being passed as the
;; ## argument which means "always select a different window even if
;; ## it's visible in the selected window.
;;	(pop-to-buffer buffer nil (selected-screen))
	(pop-to-buffer buffer)
	(if (energize-edit-buffer-externally-p buffer)
	    (setq buffer-read-only t))
	(let ((window (selected-window)))
	  (if (null window-to-select)
	      (setq window-to-select window))
	  (set-window-buffer-dedicated window buffer)
	  (setq dedicated-windows (cons window dedicated-windows))
	  (energize-scroll-window-at-extent window extent only-one))
	(setq buffer-extent-current (cdr (cdr buffer-extent-current)))))
    (while dedicated-windows
      (set-window-buffer-dedicated (car dedicated-windows) ())
      (setq dedicated-windows (cdr dedicated-windows)))

    (select-window window-to-select)
    ;; now we may have to pop the screen up
    (let ((screen (selected-screen)))
      (if (and energize-auto-raise-screen
	       (or (not (screen-visible-p screen))
		   (not (screen-totally-visible-p screen))))
	  (progn
	    (sit-for 0)
	    (make-screen-visible screen))))))

;;; called by energize-make-buffers-visible
(defun energize-main-buffer-of-list (list)
  ;; Given an alternating list of buffers and extents, pick out the
  ;; "interesting" buffer.  If one of the buffers is in debugger-mode,
  ;; or in breakpoint-mode, then that's the interesting one; otherwise,
  ;; the last buffer in the list is the interesting one.
  (let (buffer mode result)
    (while list
      (setq buffer (car list))
      (or (memq mode '(energize-debugger-mode energize-breakpoint-mode))
	  (setq result buffer
		mode (save-excursion (set-buffer buffer) major-mode)))
      (setq list (cdr (cdr list))))
    result))

;;; called by energize-make-many-buffers-visible-function
;;; If there is only one buffer/extent pair, and it's a source buffer, then
;;;  edit it in vi if that's the kind of kinkiness we're into.
;;; Get the "main" buffer, and select a screen for it.
;;; Then call energize-show-all-buffers.
;;;
(defun energize-make-buffers-visible (buffer-extent-list)
  (let ((main-buffer (energize-main-buffer-of-list buffer-extent-list))
	window)
    (if (and (null (cdr (cdr buffer-extent-list)))
	     (energize-edit-buffer-externally-p main-buffer))
	(energize-edit-buffer-externally-1 main-buffer
					   (car (cdr buffer-extent-list)))
      ;; This may create and/or select a screen as a side-effect.
      ;; I'm not sure it's necessary to call this, as display-buffer
      ;; calls it too.  But it can't hurt to select the appropriate
      ;; screen early...
      (let ((hacked-screen nil))
	(cond ((null energize-split-screens-p)
	       nil)
	      ((get-screen-name-for-buffer main-buffer)
	       (setq hacked-screen t)
	       (if pre-display-buffer-function
		   (funcall pre-display-buffer-function main-buffer nil nil))
	       )
	      ((setq window (get-buffer-window main-buffer t))
	       (cond (window
		      (setq hacked-screen t)
		      (select-screen (window-screen window))))))
	(let ((pre-display-buffer-function
	       (if hacked-screen nil pre-display-buffer-function)))
	  (energize-show-all-buffers buffer-extent-list))
;;	;; kludge!!  Select the debugger screen, not the sources screen.
;;	(if (and (null energize-split-screens-p)
;;		 pre-display-buffer-function)
;;	    (funcall pre-display-buffer-function main-buffer nil nil))
	))))

;;; this is the guts of energize-make-many-buffers-visible
;;; `arg' is really two args: `buffer-extent-list' and `go-there'.
;;; go-there is specified by 
;;; Given a list of buffer/extent pairs, make them all visible at once
;;;  (presumably in the same screen?)
;;; If `go-there'
;;;  - call energize-make-buffers-visible
;;; else
;;;  - dedicate the selected window
;;;  - call energize-make-buffers-visible
;;;  - re-select and undedicate the original selected window
;;;
(defun energize-make-many-buffers-visible-function (arg)
  (let ((buffer-extent-list (car arg))
	(go-there (cdr arg)))
    ;; enqueue an history record if we're going to move
    (if go-there
	(energize-history-enqueue))
    (setq buffer-extent-list 
	  (energize-prune-killed-buffers-from-list buffer-extent-list))
    (if buffer-extent-list
	(if go-there
	    (energize-make-buffers-visible buffer-extent-list)
	  (let ((window (selected-window)))
	    (set-window-buffer-dedicated window (window-buffer window))
	    (unwind-protect 
		(energize-make-buffers-visible buffer-extent-list)
	      (set-window-buffer-dedicated window ())
	      (select-window window)))))))

(defvar energize-make-many-buffers-visible-should-enqueue-event t
  "Special variable bound by energize-execute-command to allow the
buffers to be selected while the command is executed")

;;; called by by editorside.c:MakeBufferAndExtentVisible().
;;; should-enqueue is bound by `energize-execute-command'
;;;
(defun energize-make-many-buffers-visible (buffer-extent-list go-there)
  "First arg is a list of buffers and extents. All those should be
made visible at the same time.  If the second argument is T then point
should be moved to the first character of the extent of the first
buffer, or to the buffer if no extent is specified for this buffer.  
If second argument is NIL point should not change."
  (if energize-make-many-buffers-visible-should-enqueue-event
      ;; don't do it from process filters, but wait until we come back to
      ;; top-level.  Using go-there should still be done sparingly, as we can
      ;; surprise the user and grab their keystrokes into another buffer.
      (enqueue-eval-event 'energize-make-many-buffers-visible-function
			  (cons buffer-extent-list go-there))
    ;; go-there is always true when called from energize-execute-command,
    ;; I guess under the assumption that it's always ok to select a buffer
    ;; when we're doing something in direct response to a menu selection.
    (energize-make-many-buffers-visible-function
     (cons buffer-extent-list t))))


;;; This deales with the energize history
(defvar energize-navigation-history '(nil)
  "List of places where Energize took you to.
It is a list of (file-name/buffer-name . position)")

(defvar energize-history-maximum-length 20
  "Maximum number of locations kept in the energize history")

(defvar energize-navigation-current ()
  "Current pointer into the energize-navigation-history")

(defvar energize-navigation-current-length 0)

(defun energize-history-enqueue ()
  "Memorize the current place in the history.
Trim the history if need be."
  (let ((new-item
	 (cons (or buffer-file-truename (current-buffer))
	       (1+ (count-lines 1 (point))))))
    (if (not (equal new-item (car energize-navigation-history)))
	(progn
	  (setq energize-navigation-history
		(cons new-item energize-navigation-history))
	  (setq energize-navigation-current-length
		(1+ energize-navigation-current-length))
	  (if (> energize-navigation-current-length
		 (* 2 energize-history-maximum-length))
	      (let ((tail (nthcdr energize-history-maximum-length
				  energize-navigation-history)))
		(rplacd tail nil)
		(setq energize-navigation-current-length
		      energize-history-maximum-length)))))))

(defun energize-history-dequeue ()
  "Forget the current place in the history"
  (setq energize-navigation-history (cdr energize-navigation-history)))

(defun energize-history-go-back (item)
  "Go back to the place memorized by item"
  (let ((buffer-or-file (car item))
	(position (cdr item))
	(buffer ()))
    (cond ((bufferp buffer-or-file)
	   (setq buffer buffer-or-file))
	  ((stringp buffer-or-file)
	   (setq buffer (or (get-file-buffer buffer-or-file)
			    (find-file-noselect buffer-or-file)))))
    (if (null (buffer-name buffer))
	()
      (pop-to-buffer buffer)
      (goto-line position)
      t)))

(defun energize-history-previous ()
  "Go back in the history.
If the last command was the same go back more"
  (interactive)
  (if (not (eq last-command 'energize-history-previous))
      (setq energize-navigation-current energize-navigation-history))
  (energize-history-enqueue)
  (while (and (car energize-navigation-current)
	      (not
	       (energize-history-go-back (car energize-navigation-current))))
    (rplaca energize-navigation-current
	    (car (cdr energize-navigation-current)))
    (rplacd energize-navigation-current
	    (cdr (cdr energize-navigation-current))))
  (if (null (car energize-navigation-current))
      (progn
	(energize-history-dequeue)
	(setq last-command 'beep)
	(error "You reached the beginning of the Energize history"))
    (setq energize-navigation-current
	  (cdr energize-navigation-current))))

(define-key global-map '(shift f14) 'energize-history-previous)

(defun energize-history ()
  "Show the energize history in the energize history buffer"
  (interactive)
  (pop-to-buffer "*Energize History*")
  (erase-buffer)
  (mapcar (function (lambda (item)
		      (if item
			  (progn
			    (insert (format "%s" (car item)))
			    (indent-to-column 32 1)
			    (insert (format "%s\n" (cdr item)))))))
	  energize-navigation-history)
  (goto-char (point-min))
  (energize-history-mode))

(defun energize-history-mode ()
  "Turn on energize history mode"
  )
