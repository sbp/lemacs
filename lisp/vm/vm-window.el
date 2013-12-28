;;; Window management code for VM
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

(defun vm-display-buffer (buffer)
  (let ((pop-up-windows (eq vm-mutable-windows t)))
    (if vm-mutable-windows
	(display-buffer buffer)
      (switch-to-buffer buffer))))

(defun vm-display-current-message-buffer ()
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (let (point msg-buf sized)
    (save-excursion
      (setq msg-buf (current-buffer)
	    point (point))
      (if (null (get-buffer-window (current-buffer)))
	  (if (not (setq sized (vm-set-window-configuration 'showing-message)))
	      (vm-display-buffer (current-buffer))))
      (set-buffer msg-buf)
      (let ((w (get-buffer-window msg-buf)))
	(and w
	     (progn (set-window-point w point)
		    (and (>= (window-start w) (point-max))
			 (set-window-start w (point-min)))))))
    (if (and (not sized) vm-summary-buffer
	     (get-buffer-window vm-summary-buffer)
	     (eq vm-mutable-windows t))
	(vm-proportion-windows))))

(defun vm-proportion-windows ()
  (vm-select-folder-buffer)
  (vm-within-current-message-buffer
   ;; don't attempt proportioning if there aren't exactly two windows.
   (if (and (not (one-window-p t))
	    (eq (selected-window)
		(next-window (next-window (selected-window) 0) 0)))
       (if (= (window-width) (screen-width))
	   (let ((mail-w (get-buffer-window (current-buffer)))
		 (n (- (window-height (get-buffer-window (current-buffer)))
		       (/ (* vm-mail-window-percentage
			     (- (screen-height)
				(window-height (minibuffer-window))))
			  100)))
		 (old-w (selected-window)))
	     (if mail-w
		 (save-excursion
		   (select-window mail-w)
		   (shrink-window n)
		   (select-window old-w)
		   (and (memq major-mode '(vm-summary-mode vm-virtual-mode))
			(vm-auto-center-summary)))))
	 (let ((mail-w (get-buffer-window (current-buffer)))
	       (n (- (window-width (get-buffer-window (current-buffer)))
		     (/ (* vm-mail-window-percentage (screen-width))
			100)))
	       (old-w (selected-window)))
	   (if mail-w
	       (save-excursion
		 (select-window mail-w)
		 (shrink-window-horizontally n)
		 (select-window old-w)
		 (and (memq major-mode '(vm-summary-mode vm-virtual-mode))
		      (vm-auto-center-summary)))))))))

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
    (if (not (eq vm-mutable-windows t))
	(throw 'done nil))
    (let ((scratch "*scratch*") summary message composition config p)
      (while (and tags (null config))
	(setq config (assq (car tags) vm-window-configurations)
	      tags (cdr tags)))
      (or config (throw 'done nil))
      (setq config (vm-copy config))
      (setq composition (vm-find-composition-buffer t))
      (cond ((memq major-mode '(vm-summary-mode mail-mode))
	     (and vm-mail-buffer (buffer-name vm-mail-buffer)
		  (set-buffer vm-mail-buffer))))
      (vm-check-for-killed-summary)
      (or message (setq message (current-buffer)))
      (or summary (setq summary (or vm-summary-buffer scratch)))
      (or composition (setq composition scratch))	
      (tapestry-replace-map-element (nth 1 config)
				    'buffer-name
				    (function (lambda (x)
						(if (symbolp x)
						    (symbol-value x)
						  x))))
      (set-tapestry-map (nth 1 config))
      (save-excursion
	(set-buffer message)
	(setq vm-window-configuration (car tags)))
      (car config) )))

(defun vm-save-window-configuration (tag)
  (interactive
   (progn
     (if (null vm-window-configuration-file)
	 (error "Configurable windows not enabled."))
     (list
      (intern
       (completing-read "Name this window configuration: "
			vm-supported-window-configurations
			'identity t)))))
  (if (null vm-window-configuration-file)
      (error "Configurable windows not enabled."))
  (let (map p)
    (setq map (tapestry-map))
    (tapestry-replace-map-element map 'buffer-name 'vm-screen-buffer-to-label)
    (tapestry-nullify-map-elements map t nil t t t nil)
    (setq p (assq tag vm-window-configurations))
    (if p
	(setcar (cdr p) map)
      (setq vm-window-configurations
	    (cons (list tag map) vm-window-configurations)))
    (vm-store-window-configurations vm-window-configuration-file)
    (message "%s configuration recorded" tag)))

(defun vm-screen-buffer-to-label (buf)
  (save-excursion
    (set-buffer buf)
    (cond ((memq major-mode '(vm-virtual-mode vm-summary-mode))
	   'summary)
	  ((eq major-mode 'mail-mode)
	   'composition)
	  ((eq major-mode 'vm-mode)
	   'message)
	  (t buf))))

(defun vm-delete-window-configuration (tag)
  (interactive
   (progn
     (if (null vm-window-configuration-file)
	 (error "Configurable windows not enabled."))
     (list
      (intern
       (completing-read "Delete window configuration: "
			vm-supported-window-configurations
			'identity t)))))
  (if (null vm-window-configuration-file)
      (error "Configurable windows not enabled."))
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
  (interactive
   (progn
     (if (null vm-window-configuration-file)
	 (error "Configurable windows not enabled."))
     (list
      (intern
       (completing-read "Apply window configuration: "
			vm-window-configurations
			'identity t)))))
  (if (null vm-window-configuration-file)
      (error "Configurable windows not enabled."))
  (vm-set-window-configuration tag))

(defun vm-window-help ()
  (interactive)
  (message "WS = save configuration, WD = delete configuration, WW = apply configuration"))
