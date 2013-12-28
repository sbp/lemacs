;; Mouse and font support for RMAIL running in Lucid GNU Emacs
;; written by Wilson H. Tien (wtien@urbana.mcd.mot.com); modified by jwz.
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Right button pops up a menu of commands in Rmail and Rmail summary buffers.
;;; Middle button selects indicated mail message in Rmail summary buffer

(defvar rmail-summary-mode-menu
  '("Rmail Summary Commands"
    ["Select Message" rmail-summary-goto-msg t nil]
    "----"
    ["Previous Page" scroll-down t]
    ["Next Page" scroll-up t]
    "----"
    ["Delete Message" rmail-summary-delete-forward t nil]
    ["Undelete Message" rmail-summary-undelete t nil]
    "----"
    ["Exit rmail Summary" rmail-summary-exit t]
    ["Quit rmail" rmail-summary-quit t]))

(defun rmail-summary-update-menubar ()
  ;; if min point is in visible in the window, don't make page-up menu item
  ;; selectable
  (let ((current-menubar rmail-summary-mode-menu)
	(select '("Select Message"))
	(delete '("Delete Message"))
	(undelete '("Undelete Message"))
	(prev-page '("Previous Page"))
	(next-page '("Next Page")))
    (beginning-of-line)
    (let ((curmsg (string-to-int
		 (buffer-substring (point)
				   (min (point-max) (+ 5 (point))))))
	  deleted-p)
      (if (= 0 curmsg)
	  (progn
	    (rmail-update-menu-item delete nil)
	    (rmail-update-menu-item undelete nil)
	    (rmail-update-menu-item select nil))
	(pop-to-buffer rmail-buffer)
	(setq deleted-p (rmail-message-deleted-p curmsg))
	(pop-to-buffer rmail-summary-buffer)
	(let ((delete-menu-item 
	       (car (find-menu-item current-menubar delete)))
	      (undelete-menu-item 
	       (car (find-menu-item current-menubar undelete)))
	      (select-menu-item 
	       (car (find-menu-item current-menubar select)))
	      (msg (format "#%d" curmsg)))
	  (aset delete-menu-item 2 (not deleted-p))
	  (aset delete-menu-item 3 msg)
	  (aset undelete-menu-item 2 deleted-p)
	  (aset undelete-menu-item 3 msg)
	  (aset select-menu-item 2 t)
	  (aset select-menu-item 3 msg))))
    (rmail-update-menu-item prev-page (> (window-start) (point-min)))
    (rmail-update-menu-item next-page (< (window-end) (point-max)))))
  
(defun rmail-summary-mode-menu (event)
  "Pops up a menu of applicable rmail summary commands."
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (rmail-summary-update-menubar)
  (popup-menu rmail-summary-mode-menu))

;; The following are for rmail mode 
(defconst rmail-mode-menu
  '("Rmail Commands"
    ["Previous Page" scroll-down t]
    ["Next Page" scroll-up t]
    ["Top Of This Message" rmail-beginning-of-message t]
    "----"
    "Go To Message:"
    "----"
    ["Next Nondeleted Message" rmail-next-undeleted-message t]
    ["Previous Nondeleted Message" rmail-previous-undeleted-message t]
    ["Next Message" rmail-next-message t]
    ["Previous Message" rmail-previous-message t]
    ["First Message" rmail-show-message t]
    ["Last Message" rmail-last-message t]
    "----"
    ["Delete This Message" rmail-delete-forward t]
    ["Undelete This Message" rmail-undelete-previous-message t]
    ["Save This Message" rmail-output-to-rmail-file t]
    "----"
    ["Reply This Message" rmail-reply t]
    ["Forward This Message" rmail-forward t]
;    ["Continue This Message" rmail-continue t]
    "----"
    ["Add Label" rmail-add-label t]
    ["Kill Label" rmail-kill-label t]
    ["Next Labeled Message" rmail-next-labeled-message t]
    ["Previous Labeled Message" rmail-previous-labeled-message t]
    ["Summary by Label" rmail-summary-by-labels t]
    "----"
    ["Summary" rmail-summary t]
    ["Get New Mail" rmail-get-new-mail t]
    ["rmail Input From" rmail-input t]
    ["Expunge rmail" rmail-expunge t]
    ["Expunge and Save" rmail-expunge-and-save t]
    ["Quit rmail" rmail-quit t]))

(defun rmail-update-menu-item (item p)
  "If P is true, enable the menu item. O/w disable it."
  (aset (car (or (find-menu-item current-menubar item)
		 (error "couldn't find rmail menu item %S" item)))
	2 p))

(defun rmail-update-menubar ()
  (let ((current-menubar rmail-mode-menu)
	(prev-page '("Previous Page"))
	(next-page '("Next Page"))
	(top-page '("Top Of This Message"))
	(real-next '("Next Message"))
	(real-prev '("Previous Message"))
	(undel-next '("Next Nondeleted Message"))
	(undel-prev '("Previous Nondeleted Message"))
	(delete '("Delete This Message"))
	(undelete '("Undelete This Message"))
	i)
    ;; Disable/enable page-up/page-down menu items
    (rmail-update-menu-item prev-page (> (window-start) (point-min)))
    (rmail-update-menu-item next-page (< (window-end) (point-max)))
    (rmail-update-menu-item top-page (> (window-start) (point-min)))
    (rmail-update-menu-item real-next
		      (/= rmail-current-message rmail-total-messages))
    (rmail-update-menu-item real-prev (/= rmail-current-message 1))
    (setq i (1+ rmail-current-message))
    (while (and (<= i rmail-total-messages) (rmail-message-deleted-p i))
      (setq i (1+ i)))
    (rmail-update-menu-item undel-next (<= i rmail-total-messages))
    (setq i (1- rmail-current-message))
    (while (and (>= i 1) (rmail-message-deleted-p i))
      (setq i (1- i)))
    (rmail-update-menu-item undel-prev (>= i 1))
    (rmail-update-menu-item delete 
		      (not (rmail-message-deleted-p rmail-current-message)))
    (rmail-update-menu-item undelete 
		      (rmail-message-deleted-p rmail-current-message))
    t))
  
(defun rmail-mode-menu (event)
  "Pops up a menu of applicable rmail commands."
  (interactive "e")
  (select-window (event-window event))
  (rmail-update-menubar)
  (popup-menu rmail-mode-menu))

(defun rmail-activate-menubar-hook ()
  (cond ((eq major-mode 'rmail-mode)
	 (rmail-update-menubar))
	((eq major-mode 'rmail-summary-mode)
	 (rmail-summary-update-menubar))))

(add-hook 'activate-menubar-hook 'rmail-activate-menubar-hook)

;;; Put message headers in boldface, etc...

(require 'highlight-headers)

(defun rmail-fontify-headers ()
  (highlight-headers (point-min) (point-max) t))

(add-hook 'rmail-show-message-hook 'rmail-fontify-headers)

;; MENU and MENUBAR setup for both Rmail and Rmail summary buffers
(defun rmail-install-menubar ()
  (if (and current-menubar (not (assoc (car rmail-mode-menu) current-menubar)))
      (let ((menu (cond ((eq major-mode 'rmail-mode) rmail-mode-menu)
			((eq major-mode 'rmail-summary-mode)
			 rmail-summary-mode-menu)
			(t (error "not rmail or rmail summary mode")))))
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil (car rmail-mode-menu) (cdr menu)))))

(defun rmail-mode-menu-setup ()
  (rmail-install-menubar)
  (define-key rmail-mode-map 'button3 'rmail-mode-menu))

(add-hook 'rmail-mode-hook 'rmail-mode-menu-setup)

(defun rmail-summary-mode-menu-setup ()
  (rmail-install-menubar)
  (define-key rmail-summary-mode-map 'button2 'rmail-summary-mouse-goto-msg)
  (define-key rmail-summary-mode-map 'button3 'rmail-summary-mode-menu))

(defun rmail-summary-mouse-goto-msg (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (rmail-summary-goto-msg))

(defun rmail-install-mouse-tracker ()
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line))

(add-hook 'rmail-summary-mode-hook 'rmail-install-mouse-tracker)
(add-hook 'rmail-summary-mode-hook 'rmail-summary-mode-menu-setup)


(provide 'rmail-lucid)
