;;; Commands to move around within a VM message
;;; Copyright (C) 1989, 1990, 1991 Kyle E. Jones
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

(defun vm-scroll-forward (&optional arg)
  "Scroll forward a screenful of text.
If the current message is being previewed, the message body is revealed.
If at the end of the current message, moves to the next message iff the
value of vm-auto-next-message is non-nil.
Prefix N scrolls forward N lines."
  (interactive "P")
  (let ((mp-changed (vm-follow-summary-cursor))
	(was-invisible nil))
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (if (null (get-buffer-window (current-buffer)))
	(progn
	  (vm-display-current-message-buffer)
	  (setq was-invisible t)))
    (if (eq vm-system-state 'previewing)
	(progn (vm-show-current-message)
	       (vm-howl-if-eom))
      (if (or mp-changed was-invisible)
	  (vm-howl-if-eom)
	(let ((vmp vm-message-pointer)
	      (msg-buf (current-buffer))
	      (h-diff 0)
	      w old-w old-w-height result)
	(setq vm-system-state 'reading)
	(setq old-w (get-buffer-window msg-buf)
	      old-w-height (window-height old-w))
	(if (not (eq vm-window-configuration 'paging-message))
	    (save-excursion (vm-set-window-configuration 'paging-message)))
	(setq w (get-buffer-window msg-buf))
	(if (null w)
	    (error "paging-message configuration hides the message buffer.")
	  (setq h-diff (- (window-height w) old-w-height)))
	(setq old-w (selected-window))
	(unwind-protect
	    (progn
	      (select-window w)
	      (let ((next-screen-context-lines
		     (+ next-screen-context-lines h-diff)))
		(while (eq (setq result (vm-scroll-forward-internal arg))
			   'tryagain))
		(cond ((and (not (eq result 'next-message))
			    vm-honor-page-delimiters)
		       (vm-narrow-to-page)
		       ;; This voodoo is required!  For some
		       ;; reason the 18.52 emacs display
		       ;; doesn't immediately reflect the
		       ;; clip region change that occurs
		       ;; above without this mantra. 
		       (scroll-up 0)))))
	  (select-window old-w))
	(set-buffer msg-buf)
	(cond ((eq result 'next-message)
	       (vm-next-message)
	       (vm-set-window-configuration 'auto-next-message))
	      ((eq result 'end-of-message)
	       (vm-set-window-configuration 'end-of-message)
	       (let ((vm-message-pointer vmp))
		 (vm-emit-eom-blurb)))
	      (t
	       (and (> (prefix-numeric-value arg) 0)
		    (vm-howl-if-eom))))))))
  (if (not (or vm-startup-message-displayed vm-inhibit-startup-message))
      (vm-display-startup-message)))

(defun vm-scroll-forward-internal (arg)
  (let ((direction (prefix-numeric-value arg))
	(w (selected-window)))
    (condition-case error-data
	(progn (scroll-up arg) nil)
      (error
       (if (or (and (< direction 0)
		    (> (point-min) (vm-text-of (car vm-message-pointer))))
	       (and (>= direction 0)
		    (/= (point-max)
			(vm-text-end-of (car vm-message-pointer)))))
	   (progn
	     (vm-widen-page)
	     (if (>= direction 0)
		 (progn
		   (forward-page 1)
		   (set-window-start w (point))
		   nil )
	       (if (or (bolp)
		       (not (save-excursion
			      (beginning-of-line)
			      (looking-at page-delimiter))))
		   (forward-page -1))
	       (beginning-of-line)
	       (set-window-start w (point))
	       'tryagain))
	 (if (eq (car error-data) 'end-of-buffer)
	     (if vm-auto-next-message
		 'next-message
	       (set-window-point w (point))
	       'end-of-message)))))))

;; exploratory scrolling, what a concept.
;;
;; we do this because pos-visible-in-window-p checks the current
;; window copnfiguration, while this exploratory scrolling forces
;; Emacs to recompute the display, giving us an up to the moment
;; answer about where the end of the message is going to be
;; visible when redisplay finally does occur.
(defun vm-howl-if-eom ()
  (let ((w (get-buffer-window (current-buffer))))
    (and w
	 (save-excursion
	   (save-window-excursion
	     (condition-case ()
		 (let ((next-screen-context-lines 0))
		   (select-window w)
		   (save-excursion
		     (save-window-excursion
		       (scroll-up nil)))
		   nil)
	       (error t))))
	 (= (vm-text-end-of (car vm-message-pointer)) (point-max))
	 (vm-emit-eom-blurb))))

(defun vm-emit-eom-blurb ()
  (message "End of message %s from %s"
	   (vm-number-of (car vm-message-pointer))
	   (vm-su-full-name (car vm-message-pointer))))

(defun vm-scroll-backward (arg)
  "Scroll backward a screenful of text.
Prefix N scrolls backward N lines."
  (interactive "P")
  (vm-scroll-forward (cond ((null arg) '-)
			   ((consp arg) (list (- (car arg))))
			   ((numberp arg) (- arg))
			   ((symbolp arg) nil)
			   (t arg))))

(defun vm-preview-current-message ()
  (setq vm-system-state 'previewing)
  (if vm-real-buffers
      (vm-make-virtual-copy (car vm-message-pointer)))
  (widen)
  ;; hide as much of the message body as vm-preview-lines specifies
  (narrow-to-region
   (vm-vheaders-of (car vm-message-pointer))
   (cond ((not (eq vm-preview-lines t))
	  (min
	   (vm-text-end-of (car vm-message-pointer))
	   (save-excursion
	     (goto-char (vm-text-of (car vm-message-pointer)))
	     (forward-line (if (natnump vm-preview-lines) vm-preview-lines 0))
	     (point))))
	  (t (vm-text-end-of (car vm-message-pointer)))))
  ;; run the hooks here instead of before the narrow-to-region
  ;; for Jamie Z's highlighting hooks.  reordering the headers
  ;; screws up the extents.  the reference to vm-vheaders-of above
  ;; assures that the rordering will have occurred by this point.
  (vm-run-message-hook (car vm-message-pointer) 'vm-select-message-hook)
  (and vm-select-new-message-hook (vm-new-flag (car vm-message-pointer))
       (vm-run-message-hook (car vm-message-pointer)
			    'vm-select-new-message-hook))
  (and vm-select-unread-message-hook (vm-unread-flag (car vm-message-pointer))
       (vm-run-message-hook (car vm-message-pointer)
			    'vm-select-unread-message-hook))
  (if vm-honor-page-delimiters
      (vm-narrow-to-page))
  (goto-char (vm-text-of (car vm-message-pointer)))
  ;; If we have a window, set window start appropriately.
  (let ((w (get-buffer-window (current-buffer))))
    (if w
	(progn (set-window-start w (point-min))
	       (set-window-point w (vm-text-of (car vm-message-pointer))))))
  (if (or (null vm-preview-lines)
	  (and (not vm-preview-read-messages)
	       (not (vm-new-flag (car vm-message-pointer)))
	       (not (vm-unread-flag (car vm-message-pointer)))))
      (vm-show-current-message)
    (vm-update-summary-and-mode-line)))

(defun vm-show-current-message ()
  (save-excursion
    (save-excursion
      (goto-char (point-min))
      (widen)
      (narrow-to-region (point) (vm-text-end-of (car vm-message-pointer))))
    (if vm-honor-page-delimiters
	(progn
	  (if (looking-at page-delimiter)
	      (forward-page 1))
	  (vm-narrow-to-page))))
  ;; don't mark the message as read if the user can't see it!
  (if (get-buffer-window (current-buffer))
      (progn
	(setq vm-system-state 'showing)
	(cond ((vm-new-flag (car vm-message-pointer))
	       (vm-set-new-flag (car vm-message-pointer) nil))
	      ((vm-unread-flag (car vm-message-pointer))
	       (vm-set-unread-flag (car vm-message-pointer) nil)))
	(vm-update-summary-and-mode-line)
	(vm-howl-if-eom))
    (vm-update-summary-and-mode-line)))

(defun vm-expose-hidden-headers ()
  "Toggle exposing and hiding message headers that are normally not visible."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let* ((exposed (= (point-min) (vm-start-of (car vm-message-pointer)))))
    (vm-widen-page)
    (goto-char (point-max))
    (widen)
    (if exposed
	(narrow-to-region (point) (vm-vheaders-of (car vm-message-pointer)))
      (narrow-to-region (point) (vm-start-of (car vm-message-pointer))))
    (goto-char (point-min))
    (let (w)
      (setq w (get-buffer-window (current-buffer)))
      (and w (set-window-point w (point-min)))
      (and w
	   (= (window-start w) (vm-vheaders-of (car vm-message-pointer)))
	   (not exposed)
	   (set-window-start w (vm-start-of (car vm-message-pointer)))))
    (if vm-honor-page-delimiters
	(vm-narrow-to-page))))

(defun vm-widen-page ()
  (if (or (> (point-min) (vm-text-of (car vm-message-pointer)))
	  (/= (point-max) (vm-text-end-of (car vm-message-pointer))))
      (narrow-to-region (vm-vheaders-of (car vm-message-pointer))
			(if (or (vm-new-flag (car vm-message-pointer))
				(vm-unread-flag (car vm-message-pointer)))
			    (vm-text-of (car vm-message-pointer))
			  (vm-text-end-of (car vm-message-pointer))))))

(defun vm-narrow-to-page ()
  (save-excursion
    (let (min max (omin (point-min)) (omax (point-max)))
      (if (or (bolp) (not (save-excursion
			    (beginning-of-line)
			    (looking-at page-delimiter))))
	  (forward-page -1))
      (setq min (point))
      (forward-page 1)
      (beginning-of-line)
      (setq max (point))
      (narrow-to-region min max))))

(defun vm-beginning-of-message ()
  "Moves to the beginning of the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-widen-page)
  (push-mark)
  (goto-char (point-min))
  (if vm-honor-page-delimiters
      (vm-narrow-to-page))
  (vm-display-current-message-buffer))

(defun vm-end-of-message ()
  "Moves to the end of the current message, exposing and flagging it read
as necessary."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (eq vm-system-state 'previewing)
      (vm-show-current-message))
  (setq vm-system-state 'reading)
  (vm-widen-page)
  (push-mark)
  (goto-char (point-max))
  (if vm-honor-page-delimiters
      (vm-narrow-to-page))
  (vm-display-current-message-buffer))
