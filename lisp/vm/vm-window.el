;;; Window management code for VM
;;; Copyright (C) 1989, 1990, 1991, 1993, 1994 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defun vm-display (buffer display commands configs)
;; the clearinghouse VM display function.
;;
;; First arg BUFFER non-nil is a buffer to display or undisplay.
;; nil means there is no request to display or undisplay a
;; buffer.
;;
;; Second arg DISPLAY non-nil means to display the buffer, nil means
;; to undisplay it.  This function guarantees to display the
;; buffer if requested.  Undisplay is not guaranteed.
;;
;; Third arg COMMANDS is a list of symbols.  this-command must
;; match one of these symbols for a window configuration to be
;; applied.
;;
;; Fourth arg CONFIGS is a list of window configurations to try.
;; vm-set-window-configuration will step through the list looking
;; for an existing configuration, and apply the one it finds.
;;
;; Display is done this way:
;;  1. if the buffer is already displayed, quit
;;  2. if vm-display-buffer-hook in non-nil
;;        run the hooks
;;        use the selected window/frame to display the buffer
;;        quit
;;  3. apply a window configuration
;;        if the buffer is displayed now, quit
;;  4. call vm-display-buffer which will display the buffer.
;;
;; Undisplay is done this way:
;;  1. if the buffer is not displayed, quit
;;  2. if vm-undisplay-buffer-hook is non-nil
;;        run the hooks
;;        quit
;;  3. apply a window configuration
;;  4, if a window configuration was applied
;;        quit
;;  5. call vm-undisplay-buffer which will make the buffer
;;     disappear from at least one window/frame.
;;
;; If display/undisplay is not requested, only window
;; configuration is done, and only then if the value of
;; this-command is found in the COMMANDS list.
  (vm-save-buffer-excursion
    (cond ((and buffer display (null (vm-get-buffer-window buffer)))
	   (if vm-display-buffer-hook
	       (progn (run-hooks 'vm-display-buffer-hook)
		      (switch-to-buffer buffer)
		      (vm-record-current-window-configuration nil))
	     (if (not (and (memq this-command commands)
			   (apply 'vm-set-window-configuration configs)
			   (vm-get-buffer-window buffer)))
		 (vm-display-buffer buffer))))
	  ((and buffer (not display) (vm-get-buffer-window buffer))
	   (if vm-undisplay-buffer-hook
	       (progn (run-hooks 'vm-undisplay-buffer-hook)
		      (vm-record-current-window-configuration nil))
	     (if (not (and (memq this-command commands)
			   (apply 'vm-set-window-configuration configs)))
		 (vm-undisplay-buffer buffer))))
	  ((memq this-command commands)
	   (apply 'vm-set-window-configuration configs)))))

(defun vm-display-buffer (buffer)
  (let ((pop-up-windows (eq vm-mutable-windows t))
	(pop-up-frames vm-mutable-frames))
    (vm-record-current-window-configuration nil)
    (if (or pop-up-frames
	    (and (eq vm-mutable-windows t)
		 (symbolp
		  (vm-buffer-to-label
		   (window-buffer
		    (selected-window))))))
	(select-window (display-buffer buffer))
      (switch-to-buffer buffer))))

(defun vm-undisplay-buffer (buffer)
  (vm-save-buffer-excursion
    (vm-delete-windows-or-frames-on buffer)
    (let ((w (vm-get-buffer-window buffer)))
      (and w (set-window-buffer w (other-buffer))))))

(defun vm-load-window-configurations (file)
  (save-excursion
    (let ((work-buffer nil))
      (unwind-protect
	  (progn
	    (set-buffer (setq work-buffer (get-buffer-create "*vm-wconfig*")))
	    (erase-buffer)
	    (setq vm-window-configurations
		  (condition-case ()
		      (progn
			(insert-file-contents file)
			(read (current-buffer)))
		    (error nil))))
	(and work-buffer (kill-buffer work-buffer))))))

(defun vm-store-window-configurations (file)
  (save-excursion
    (let ((work-buffer nil))
      (unwind-protect
	  (progn
	    (set-buffer (setq work-buffer (get-buffer-create "*vm-wconfig*")))
	    (erase-buffer)
	    (print vm-window-configurations (current-buffer))
	    (write-region (point-min) (point-max) file nil 0))
	(and work-buffer (kill-buffer work-buffer))))))

(defun vm-set-window-configuration (&rest tags)
  (catch 'done
    (if (not vm-mutable-windows)
	(throw 'done nil))
    (let ((nonexistent " *vm-nonexistent*")
	  (nonexistent-summary " *vm-nonexistent-summary*")
	  (selected-frame (vm-selected-frame))
	  summary message composition edit config)
      (while (and tags (null config))
	(setq config (assq (car tags) vm-window-configurations)
	      tags (cdr tags)))
      (or config (setq config (assq 'default vm-window-configurations)))
      (or config (throw 'done nil))
      (setq config (vm-copy config))
      (setq composition (vm-find-composition-buffer t))
      (cond ((eq major-mode 'vm-summary-mode)
	     (if (or (null vm-mail-buffer) (null (buffer-name vm-mail-buffer)))
		 (throw 'done nil)
	       (setq summary (current-buffer))
	       (setq message vm-mail-buffer)))
	    ((eq major-mode 'vm-mode)
	     (setq message (current-buffer)))
	    ((eq major-mode 'vm-virtual-mode)
	     (setq message (current-buffer)))
	    ((eq major-mode 'mail-mode)
	     (if (or (null vm-mail-buffer) (null (buffer-name vm-mail-buffer)))
		 (throw 'done nil)
	       (setq message vm-mail-buffer)))
	    ((eq vm-system-state 'editing)
	     (if (or (null vm-mail-buffer) (null (buffer-name vm-mail-buffer)))
		 (throw 'done nil)
	       (setq edit (current-buffer))
	       (setq message vm-mail-buffer)))
	    ;; not in a VM related buffer, bail...
	    (t (throw 'done nil)))
      (set-buffer message)
      ;; the user doesn't want us to chop up the whole screen and
      ;; no root window to chop up has been provided.  bail...
      (if (not (or vm-root-window-edges (eq vm-mutable-windows t)))
	  (throw 'done nil))
      ;; if this configuration is already the current one, don't
      ;; set it up again.
      (if (or (and vm-mutable-frames (eq (car config) vm-window-configuration))
	      (and (not vm-mutable-frames)
		   (listp vm-window-configuration)
		   (eq (car config)
		       (cdr (assq selected-frame vm-window-configuration)))))
	  (throw 'done nil))
      (vm-check-for-killed-summary)
      (or summary (setq summary (or vm-summary-buffer nonexistent-summary)))
      (or composition (setq composition nonexistent))
      (or edit (setq edit nonexistent))
      (tapestry-replace-tapestry-element (nth 1 config) 'buffer-name
					 (function
					  (lambda (x)
					    (if (symbolp x)
						(symbol-value x)
					      x ))))
      (set-tapestry (nth 1 config) 1 vm-root-window-edges)
      (and (get-buffer nonexistent)
	   (vm-delete-windows-or-frames-on nonexistent))
      (if (and (vm-get-buffer-window nonexistent-summary)
	       (not (vm-get-buffer-window message)))
	  ;; user asked for summary to be displayed but doesn't
	  ;; have one, nor is the folder buffer displayed.  Help
	  ;; the user not to lose here.
	  (vm-replace-buffer-in-windows nonexistent-summary message)
	(and (get-buffer nonexistent-summary)
	     (vm-delete-windows-or-frames-on nonexistent-summary)))
      (vm-record-current-window-configuration config)
      config )))

(defun vm-record-current-window-configuration (config)
  ;; need to find some way to let VM know when the window
  ;; configuration changes from a non-VM command.  Until I figure
  ;; out a way or give up on the problem, the function def is quoted
  ;; out.
  '(let (cell)
     (if (and (listp vm-window-configuration)
	      (setq cell (assq (vm-selected-frame) vm-window-configuration)))
	 (setcdr cell (car config))
       (setq vm-window-configuration
	     (cons
	      (cons (vm-selected-frame) (car config))
	      vm-window-configuration)))))

(defun vm-save-window-configuration (tag)
  "Name and save the current window configuration.
With this command you associate the current window setup with an
action.  Each time you perform this action VM will duplicate this
window setup.

Nearly every VM command can have a window configuration
associated with it.  VM also allows some category configurations,
`startup', `reading-message', `composing-message', `editing-message',
`marking-message' and `searching-message' for the commands that
do these things.  There is also a `default' configuration that VM
will use if no other configuration is applicable.  Command
specific configurations are searched for first, then the category
configurations and then the default configuration.  The first
configuration found is the one that is applied.

The value of vm-mutable-windows must be non-nil for VM to use
window configurations.

If vm-mutable-frames is non-nil and Emacs is running under X
windows, then VM will use all existing frames.  Otherwise VM will
restrict its changes to the frame in which it was started."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (if (null vm-window-configuration-file)
	 (error "Configurable windows not enabled.  Set vm-window-configuration-file to enable."))
     (list
      (intern
       (completing-read "Name this window configuration: "
			vm-supported-window-configurations
			'identity t)))))
  (if (null vm-window-configuration-file)
      (error "Configurable windows not enabled.  Set vm-window-configuration-file to enable."))
  (let (map p)
    (setq map (tapestry (list (vm-selected-frame))))
    (tapestry-replace-tapestry-element map 'buffer-name 'vm-buffer-to-label)
    (tapestry-nullify-tapestry-elements map t nil t t t nil)
    (setq p (assq tag vm-window-configurations))
    (if p
	(setcar (cdr p) map)
      (setq vm-window-configurations
	    (cons (list tag map) vm-window-configurations)))
    (vm-store-window-configurations vm-window-configuration-file)
    (message "%s configuration recorded" tag)))

(defun vm-buffer-to-label (buf)
  (save-excursion
    (set-buffer buf)
    (cond ((eq major-mode 'vm-summary-mode)
	   'summary)
	  ((eq major-mode 'mail-mode)
	   'composition)
	  ((eq major-mode 'vm-mode)
	   'message)
	  ((eq major-mode 'vm-virtual-mode)
	   'message)
	  ((eq vm-system-state 'editing)
	   'edit)
	  (t buf))))

(defun vm-delete-window-configuration (tag)
  "Delete the configuration saved for a particular action.
This action will no longer have an associated window configuration.
The action will be read from the minibuffer."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (if (null vm-window-configuration-file)
	 (error "Configurable windows not enabled.  Set vm-window-configuration-file to enable."))
     (list
      (intern
       (completing-read "Delete window configuration: "
			(mapcar (function
				 (lambda (x)
				   (list (symbol-name (car x)))))
				vm-window-configurations)
			'identity t)))))
  (if (null vm-window-configuration-file)
      (error "Configurable windows not enabled.  Set vm-window-configuration-file to enable."))
  (let (p)
    (setq p (assq tag vm-window-configurations))
    (if p
	(if (eq p (car vm-window-configurations))
	    (setq vm-window-configurations (cdr vm-window-configurations))
	  (setq vm-window-configurations (delq p vm-window-configurations)))
      (error "No window configuration set for %s" tag)))
  (vm-store-window-configurations vm-window-configuration-file)
  (message "%s configuration deleted" tag))

(defun vm-apply-window-configuration (tag)
  "Change the current window configuration to be one
associated with a particular action.  The action will be read
from the minibuffer."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (list
      (intern
       (completing-read "Apply window configuration: "
			(mapcar (function
				 (lambda (x)
				   (list (symbol-name (car x)))))
				vm-window-configurations)
			'identity t)))))
  (vm-set-window-configuration tag))

(defun vm-window-help ()
  (interactive)
  (message "WS = save configuration, WD = delete configuration, WW = apply configuration"))

(defun vm-window-loop (action obj-1 &optional obj-2)
  (let ((start (next-window (selected-window) 'nomini))
	(delete-me nil)
	(done nil)
	(all-frames (if vm-mutable-frames t nil))
	w)
    (setq w start)
    (and obj-1 (setq obj-1 (get-buffer obj-1)))
    (while (not done)
      (if (and delete-me (not (eq delete-me (next-window delete-me 'nomini))))
	  (progn
	    (delete-window delete-me)
	    (if (eq delete-me start)
		(setq start nil))
	    (setq delete-me nil)))
      (cond ((and (eq action 'delete) (eq obj-1 (window-buffer w)))
	     ;; a deleted window has no next window, so we
	     ;; defer the deletion until after we've moved
	     ;; to the next window.
	     (setq delete-me w))
	    ((and (eq action 'replace) (eq obj-1 (window-buffer w)))
	     (set-window-buffer w obj-2)))
      (setq done (eq start
		     (setq w
			  (condition-case nil
			      (next-window w 'nomini all-frames)
			    (wrong-number-of-arguments
			     (next-window w 'nomini))))))
      (if (null start)
	  (setq start w)))
    (if (and delete-me (not (eq delete-me (next-window delete-me 'nomini))))
	(delete-window delete-me))))

(defun vm-frame-loop (action obj-1)
  (if (fboundp 'vm-next-frame)
      (let ((start (vm-selected-frame))
	    (delete-me nil)
	    (done nil)
	    f)
	(setq f start)
	(and obj-1 (setq obj-1 (get-buffer obj-1)))
	(while (not done)
	  (vm-select-frame f)
	  (if delete-me
	      (progn
		(condition-case nil
		    (progn
		      (vm-delete-frame delete-me)
		      (if (eq delete-me start)
			  (setq start nil)))
		  (error nil))
		(setq delete-me nil)))
	  (cond ((and (eq action 'delete)
		      (one-window-p t)
		      ;; the next-window/previous-window stuff is to
		      ;; avoid picking the minibuffer window
		      (eq obj-1 (window-buffer
				 (next-window
				  (vm-frame-selected-window f)
				  'nomini))))
		 ;; a deleted frame has no next frame, so we
		 ;; defer the deletion until after we've moved
		 ;; to the next frame.
		 (setq delete-me f)))
	  (setq done (eq start (setq f (vm-next-frame f))))
	  (if (null start)
	      (setq start f)))
	;; complete the circle.  the selected frame is now the
	;; same as when this function was entered unless that
	;; frame was deleted.
	(vm-select-frame start)
	(if delete-me
	    (progn
	      (vm-error-free-call 'vm-delete-frame delete-me)
	      (setq delete-me nil))))))

(defun vm-delete-windows-or-frames-on (buffer)
  (and (eq vm-mutable-windows t) (vm-window-loop 'delete buffer))
  (and vm-mutable-frames (vm-frame-loop 'delete buffer)))

(defun vm-replace-buffer-in-windows (old new)
  (vm-window-loop 'replace old new))

(defun vm-get-buffer-window (buffer &optional force-all-frames)
  (condition-case nil
      (get-buffer-window buffer (if (or vm-mutable-frames force-all-frames)
				    t
				  nil))
    (wrong-number-of-arguments
     (get-buffer-window buffer))))

(defun vm-goto-new-frame ()
  (cond ((fboundp 'make-frame)
	 (select-frame (make-frame)))
	((fboundp 'make-screen)
	 (select-screen (make-screen)))
	((fboundp 'new-screen)
	 (select-screen (new-screen)))))

(fset 'vm-selected-frame
      (symbol-function
       (cond ((fboundp 'selected-frame) 'selected-frame)
	     ((fboundp 'selected-screen) 'selected-screen)
	     (t 'ignore))))

(cond ((fboundp 'next-frame)
       (fset 'vm-next-frame (symbol-function 'next-frame))
       (fset 'vm-select-frame (symbol-function 'select-frame))
       (fset 'vm-delete-frame (symbol-function 'delete-frame))
       (fset 'vm-frame-selected-window
	     (symbol-function 'frame-selected-window)))
      ((fboundp 'next-screen)
       (fset 'vm-next-frame (symbol-function 'next-screen))
       (fset 'vm-select-frame (symbol-function 'select-screen))
       (fset 'vm-delete-frame (symbol-function 'delete-screen))
       (fset 'vm-frame-selected-window
	     (if (fboundp 'epoch::selected-window)
		 (symbol-function 'epoch::selected-window)
	       (symbol-function 'screen-selected-window)))))
