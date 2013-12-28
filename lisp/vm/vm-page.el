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

(require 'vm)

(defun vm-scroll-forward (&optional arg)
  "Scroll forward a screenful of text.
If the current message is being previewed, the message body is revealed.
If at the end of the current message, moves to the next message iff the
value of vm-auto-next-message is non-nil.
Prefix N scrolls forward N lines."
  (interactive "P")
  (let ((mp-changed (vm-follow-summary-cursor)) was-invisible do-next-message)
    (vm-select-folder-buffer)
    (vm-sanity-check-modification-flag)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (if (vm-within-current-message-buffer
	 (null (get-buffer-window (current-buffer))))
	(progn
	  (vm-display-current-message-buffer)
	  (setq was-invisible t)))
    (if (eq vm-system-state 'previewing)
	(vm-show-current-message)
      (if (or mp-changed was-invisible)
	  ()
	(setq vm-system-state 'reading)
	(let ((w (get-buffer-window (vm-current-message-buffer)))
	      (vmp vm-message-pointer)
	      (old-w (selected-window))
	      (direction (prefix-numeric-value arg))
	      error-data)
	  (vm-within-current-message-buffer
	   (unwind-protect
	       (progn
		 (select-window w)
		 (let ((vm-message-pointer vmp))
		   (while
		     (catch 'tryagain
		       (if (not
			    (eq
			     (condition-case error-data
				 (scroll-up arg)
			       (error
				(if (or (and (< direction 0)
					     (> (point-min)
						(vm-text-of
						 (car vm-message-pointer))))
					(and (>= direction 0)
					     (/= (point-max)
						 (vm-text-end-of
						  (car vm-message-pointer)))))
				    (progn
				      (vm-widen-page)
				      (if (>= direction 0)
					  (progn
					    (forward-page 1)
					    (set-window-start w (point))
					    nil )
					(if (or
					     (bolp)
					     (not
					      (save-excursion
						(beginning-of-line)
						(looking-at page-delimiter))))
					    (forward-page -1))
					(beginning-of-line)
					(set-window-start w (point))
					(throw 'tryagain t)))
				  (if (eq (car error-data) 'end-of-buffer)
				      (if vm-auto-next-message
					  (progn (setq do-next-message t)
						 'next-message)
					(message "End of message %s from %s"
						 (vm-number-of
						  (car vm-message-pointer))
						 (vm-su-full-name
						  (car vm-message-pointer)))
					nil )))))
			     'next-message))
			   (progn
			     (if vm-honor-page-delimiters
				 (progn
				   (vm-narrow-to-page)
				   ;; This voodoo is required!  For some
				   ;; reason the 18.52 emacs display
				   ;; doesn't immediately reflect the
				   ;; clip region change that occurs
				   ;; above without this mantra. 
				   (scroll-up 0)))))
		       nil ))))
	     (select-window old-w))))))
    (if do-next-message
	(vm-next-message)))
  (if (not (or vm-startup-message-displayed vm-inhibit-startup-message))
      (vm-display-startup-message)))

(defun vm-scroll-backward (arg)
  "Scroll backward a screenful of text.
Prefix N scrolls backward N lines."
  (interactive "P")
  (vm-scroll-forward (cond ((null arg) '-)
			   ((symbolp arg) nil)
			   ((consp arg) (list (- (car arg))))
			   (t arg))))

(defun vm-preview-current-message ()
  (setq vm-system-state 'previewing)
  (if (and (null (vm-within-current-message-buffer
		  (get-buffer-window (current-buffer))))
	   (eq major-mode 'vm-virtual-mode)
	   (not (one-window-p t)))
      (vm-display-current-message-buffer t))
  (let ((vmp vm-message-pointer))
    (vm-within-current-message-buffer
     (let ((vm-message-pointer vmp))
       (widen)
       (narrow-to-region
	(vm-vheaders-of (car vm-message-pointer))
	(if vm-preview-lines
	    (min
	     (vm-text-end-of (car vm-message-pointer))
	     (save-excursion
	       (goto-char (vm-text-of (car vm-message-pointer)))
	       (forward-line (if (natnump vm-preview-lines)
				 vm-preview-lines
			       0))
	       (point)))
	  (vm-text-of (car vm-message-pointer))))
       (if vm-honor-page-delimiters
	   (vm-narrow-to-page))
       (goto-char (vm-text-of (car vm-message-pointer)))
       ;; If we have a window, set window start appropriately.
       ;; Highlight appropriate headers if current buffer is visible.
       (let ((w (get-buffer-window (current-buffer))))
	 (if w (set-window-start w (point-min)))
	 (vm-highlight-headers (car vm-message-pointer) w)))))
  ;; De Morgan's Theorems could clear away most of the following negations,
  ;; but the resulting code would be horribly obfuscated.
  (if (or (null vm-preview-lines)
	  (and (not vm-preview-read-messages)
	       (not (vm-new-flag (car vm-message-pointer)))
	       (not (vm-unread-flag (car vm-message-pointer)))))
      (vm-show-current-message)
    (vm-update-summary-and-mode-line)))

(defun vm-show-current-message ()
  (setq vm-system-state 'showing)
  (let ((vmp vm-message-pointer))
    (vm-within-current-message-buffer
     (let ((vm-message-pointer vmp))
       (save-excursion
	 (goto-char (point-min))
	 (widen)
	 (narrow-to-region (point) (vm-text-end-of (car vm-message-pointer))))
       (if vm-honor-page-delimiters
	   (progn
	     (if (looking-at page-delimiter)
		 (forward-page 1))
	     (vm-narrow-to-page))))))
  (cond ((vm-new-flag (car vm-message-pointer))
	 (vm-set-new-flag (car vm-message-pointer) nil))
	((vm-unread-flag (car vm-message-pointer))
	 (vm-set-unread-flag (car vm-message-pointer) nil)))
  (vm-update-summary-and-mode-line))

(defun vm-expose-hidden-headers ()
  "Toggle exposing and hiding message headers that are normally not visible."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((vmp vm-message-pointer))
    (vm-within-current-message-buffer
     (let* ((vm-message-pointer vmp)
	    (exposed (= (point-min) (vm-start-of (car vm-message-pointer)))))
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
	   (vm-narrow-to-page))))))

(defun vm-display-current-message-buffer (&optional no-highlighting)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-within-current-message-buffer
   (if (null (get-buffer-window (current-buffer)))
       (if vm-mutable-windows
	   (let ((pop-up-windows (and pop-up-windows
				      (eq vm-mutable-windows t))))
	     (display-buffer (current-buffer)))
	 (switch-to-buffer (current-buffer))))
   (let ((w (get-buffer-window (current-buffer))))
     (if (>= (window-start w) (point-max))
	 (set-window-start w (point-min) t))))
  (if (and vm-summary-buffer (get-buffer-window vm-summary-buffer)
	   (eq vm-mutable-windows t))
      (vm-proportion-windows))
  (vm-within-current-message-buffer
   (if (not no-highlighting)
       (vm-highlight-headers (car vm-message-pointer)
			     (get-buffer-window (current-buffer))))))

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
  (let ((vmp vm-message-pointer))
    (vm-within-current-message-buffer
     (let ((vm-message-pointer vmp))
       (vm-widen-page)
       (push-mark)
       (goto-char (point-min))
       (if vm-honor-page-delimiters
	   (vm-narrow-to-page)))))
  (vm-display-current-message-buffer))

(defun vm-end-of-message ()
  "Moves to the end of the current message, exposing and marking it read
as necessary."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (eq vm-system-state 'previewing)
      (vm-show-current-message))
  (setq vm-system-state 'reading)
  (let ((vmp vm-message-pointer))
    (vm-within-current-message-buffer
     (let ((vmp vm-message-pointer))
       (vm-widen-page)
       (push-mark)
       (goto-char (point-max))
       (if vm-honor-page-delimiters
	   (vm-narrow-to-page)))))
  (vm-display-current-message-buffer t))