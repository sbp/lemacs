;; Multi-screen management that is independent of window systems.
;; Copyright (C) 1990 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'screen)


;; Creation of the first screen.  Two styles are provided for, although a
;; third is possible.  They are:  Separate minibuffer screen, and one
;; minibuffer per screen.  Default is the latter.


;; If we can't map the initial screen, barf and die

(defun death-function ()
  (if (screenp terminal-screen)
      (progn
	(setq window-system nil)
	(select-screen terminal-screen)
	(message "Well, looks like *you* can't use emacs 19!!")
;;	(message "Must be a bug with your name on it, or jla must not like you.")
;;	(message "Send him mail, and if you're good, maybe this will change. ")
	(kill-emacs 33))
    (kill-emacs 33)))

(defvar initial-screen-alist nil
  "Alist of values used when creating the initial emacs screen.
These may be set in your init file, like this:
 (setq initial-screen-alist '((top . 1) (left . 1) (width . 80) (height . 55)))
For values which are examined upon creation of each screen,
use screen-default-alist.")

(defvar screen-default-alist nil
  "Alist of default values for screen creation.
These may be set in your init file, like this:
  (setq screen-default-alist '((width . 80) (height . 55)))
For values specific to the first emacs screen, see initial-screen-alist.")

(defvar minibuffer-alist '((minibuffer . only)
			   (height . 1)
			   (width . 80)
			   (top . -1)
			   (left . 1)
			   (vertical-scroll-bar . nil)
			   (horizontal-scroll-bar . nil)
			   (unsplittable . t))
  "Alist of switches for the appearance of the minibuffer.")

(defvar initial-screen-hooks nil
  "Hook to run after initial screen startup." )

(defvar screen-creation-func
  (intern (concat (prin1-to-string window-system) "-create-screen"))
  "Window-system dependent function for creating new screens.")

;; Try to use screen colors and font for the minibuffer, if none were
;; specified.  Also make sure the screen defaults include no minibuffer,
;; as well as having the same name--this is so they will all be treated
;; the same by the window manager.

(defun detached-minibuffer-startup (window-system-switches)
  (let ((mini-type (assq 'minibuffer screen-default-alist))
	(screen-names (assq 'name screen-default-alist))
	(extras))
    (or (assq 'foreground-color minibuffer-alist)
	(setq extras (list (assq 'foreground-color window-system-switches))))
    (or (assq 'background-color minibuffer-alist)
	(setq extras (append extras
			     (list (assq 'background-color
					 window-system-switches)))))
    (or (assq 'font minibuffer-alist)
	(setq extras (append extras
			     (list (assq 'font
					 window-system-switches)))))
    (if extras
	(setq minibuffer-alist (append extras minibuffer-alist)))
    (if mini-type
	(rplacd mini-type 'none)
      (setq screen-default-alist (append screen-default-alist
					 '((minibuffer . none)))))
    (if screen-names
	(rplacd screen-names "*emacs screen*")
      (setq screen-default-alist (append screen-default-alist
					 '((name . "*emacs screen*"))))))
  (setq	global-minibuffer-screen (funcall screen-creation-func
					  minibuffer-alist))
  (select-screen (funcall screen-creation-func
			  (append
			   '((minibuffer . none)
			     (name . "*emacs screen*"))
			   window-system-switches)) t))

;; Setup for single attached minibuffer screen style

(defun attached-minibuffer-startup (window-system-switches)
       (select-screen (setq global-minibuffer-screen
		       (funcall screen-creation-func
				window-system-switches)) t))

;; Setup for minibuffer/screen style

(defun multi-minibuffer-startup (window-system-switches)
  (select-screen (funcall screen-creation-func window-system-switches) t))

;; This is called from the window-system specific function which is attached
;; to window-setup-hook.

(defvar first-screen-user-positioned nil)
(defvar separate-minibuffer-screen nil)

(defun pop-initial-screen (window-system-switches)
  (condition-case signaled-error
      (let ((mini (assq 'minibuffer window-system-switches)))
	(setq first-screen-user-positioned
	      (and (assq 'top window-system-switches)
		   (assq 'left window-system-switches)))
	(if (or separate-minibuffer-screen
		(and mini (eq (cdr mini) 'none)))
	    (detached-minibuffer-startup window-system-switches)
	  (multi-minibuffer-startup window-system-switches)))
    (error (death-function)))
  ;;  (setq map-screen-hook 'deiconify-hook
  (run-hooks 'initial-screen-hooks))


;; Creation of additional screens.  If the user specified the position
;; of the initial screen, then specify placement of new screens as well.
;; The default function merely offsets them from the selected screen
;; by the values of new-screen-x-delta and new-screen-y-delta.

(defvar new-screen-x-delta 50
  "Horizontal displacement (in pixels) for position of new screen.")
(defvar new-screen-y-delta 50
  "Vertical displacement (in pixels) for position of new screen.")

;; This just adds the deltas to the position of the selected screen.

(defun new-screen-position (top left width height)
  (let ((new-top (+ top new-screen-y-delta))
	(new-left (+ left new-screen-x-delta))
	(top (assq 'top screen-default-alist))
	(left (assq 'left screen-default-alist)))
    (or (and top left (rplacd top new-top) (rplacd left new-left)
	     screen-default-alist)
	(setq screen-default-alist (append (list (cons 'top new-top)
						 (cons 'left new-left))
					   screen-default-alist)))))

(defun new-screen ()
  (if first-screen-user-positioned
      (let* ((s (selected-screen))
	     (this-top (assq 'top (screen-parameters s)))
	     (this-left (assq 'left (screen-parameters s)))
	     (this-width (x-pixel-width s))
	     (this-height (x-pixel-height s)))
	(and this-top this-left (new-screen-position (cdr this-top)
						     (cdr this-left)
						     this-width this-height))))
  (funcall screen-creation-func screen-default-alist))

;; Return some screen other than the current screen,
;; creating one if neccessary.  Note that the minibuffer screen, if
;; separate, is not considered (see next-screen).

(defun get-screen ()
  (let ((s (if (equal (next-screen (selected-screen)) (selected-screen))
	       (new-screen)
	     (next-screen (selected-screen)))))
    s))

(setq
      ;;auto-new-screen-function 'new-screen
      auto-new-screen-function 'get-screen
      auto-new-screen t)

(defun buffer-in-other-screen ()
  (interactive)
  (let ((auto-new-screen t))
    (pop-to-buffer nil)))

(defun next-multiscreen-window ()
  "Select the next window, regardless of which screen it is on."
  (interactive)
  (select-window (next-window (selected-window)
			      (> (minibuffer-depth) 0)
			      t)))

(defun previous-multiscreen-window ()
  "Select the previous window, regardless of which screen it is on."
  (interactive)
  (select-window (previous-window (selected-window)
				  (> (minibuffer-depth) 0)
				  t)))

;; Deal with iconification issues.  If we're using the separate minibuffer
;; screen, then iconification of that screen means iconifying all of emacs.
;; Otherwise, individual screens may be iconified.

;; If emacs is iconified, remember which screens are visible, and only
;; map those upon deiconification.


;; Set to the screen which is iconified
(defvar icon-screen nil)
(defvar visible-screens nil)

;; Hook called when window manager iconifies our lead window.

(defun iconify-hook ()
  (let ((leader (if (screenp global-minibuffer-screen)
		    global-minibuffer-screen
		  (selected-screen)))
	(screens (delq global-minibuffer-screen (screen-list))))
    (if (screen-visible-p leader)
	nil
      (progn
	(setq icon-screen leader
	      unmap-screen-hook nil)
	(while screens
	  (if (screen-visible-p (car screens))
	      (progn
		(setq visible-screens (append visible-screens
					      (list (car screens))))
		(make-screen-invisible (car screens))))
	  (setq screens (cdr screens)))
	(setq unmap-screen-hook 'iconify-hook)))))


;; Unmap all visible windows and iconify ICON-SCREEN

(defun iconify-function (icon-screen)
  (setq unmap-screen-hook nil)
  (let ((screens (delq icon-screen (screen-list))))
    (while screens
      (if (screen-visible-p (car screens))
	  (progn
		(setq visible-screens (append visible-screens
					      (list (car screens))))
		(make-screen-invisible (car screens))))
      (setq screens (cdr screens)))
    (iconify-screen icon-screen))
  (setq unmap-screen-hook 'iconify-hook))

(defun iconify-emacs ()
  "Iconify emacs, unmapping all screens in use and mapping the icon window."
  (interactive)
  (if (screenp global-minibuffer-screen)
      (iconify-function (setq icon-screen global-minibuffer-screen))
    (iconify-function (setq icon-screen (selected-screen)))))


;; Called when window manager maps a window in normal state

(defun deiconify-hook ()
  (if (and (screenp icon-screen) visible-screens)
      (progn
	(setq map-screen-hook nil)
	(while visible-screens
	  (make-screen-visible (car visible-screens))
	  (setq visible-screens (cdr visible-screens)))
	(select-screen icon-screen t)
	(setq map-screen-hook 'deiconify-hook
	      icon-screen nil))))

(setq map-screen-hook 'deiconify-hook
      unmap-screen-hook 'iconify-hook)

(define-key ctl-x-map "o" 'next-multiscreen-window)
(define-key esc-map "o" 'other-window)
(define-key global-map "\^Z" 'iconify-emacs)
(define-function-key global-function-map 'xk-f2 'buffer-in-other-screen)

;;
;;
;; Convenience functions for dynamically changing screen parameters
;;
(defun set-screen-height (h)
  (interactive "NHeight: ")
  (let* ((screen (selected-screen))
	 (width (cdr (assoc 'width (screen-parameters (selected-screen))))))
    (set-screen-size (selected-screen) width h)))

(defun set-screen-width (w)
  (interactive "NWidth: ")
  (let* ((screen (selected-screen))
	 (height (cdr (assoc 'height (screen-parameters (selected-screen))))))
    (set-screen-size (selected-screen) w height)))

(defun set-default-font (font-name)
  (interactive "sFont name: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'font font-name))))

(defun set-screen-background (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'background-color color-name))))

(defun set-screen-foreground (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'foreground-color color-name))))

(defun set-cursor-color (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'cursor-color color-name))))

(defun set-pointer-color (color-name)
  (interactive "sColor: ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'mouse-color color-name))))

(defun set-auto-raise (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'auto-raise toggle))))

(defun set-auto-lower (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'auto-lower toggle))))

(defun set-vertical-bar (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'vertical-scroll-bar toggle))))

(defun set-horizontal-bar (toggle)
  (interactive "xt or nil? ")
  (modify-screen-parameters (selected-screen)
			    (list (cons 'horizontal-scroll-bar toggle))))
