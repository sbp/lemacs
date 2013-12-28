;; Multi-screen management that is independent of window systems.
;; Copyright (C) 1990-1993 Free Software Foundation, Inc.

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

(provide 'screen)

;; these are called from select_screen()

(defvar select-screen-hook nil
  "Function or functions to run just after a new screen is selected.")

(defvar deselect-screen-hook nil
  "Function or functions to run just before selecting another screen.")


;; Creation of the first screen.  Two styles are provided for, although a
;; third is possible.  They are:  Separate minibuffer screen, and one
;; minibuffer per screen.  Default is the latter.


(defvar screen-default-alist nil
  "Alist of default values for screen creation, other than the first one.
These may be set in your init file, like this:
  (setq screen-default-alist '((width . 80) (height . 55)))
For values specific to the first emacs screen, you must use X resources.")

(defvar minibuffer-alist '((minibuffer . only)
			   (height . 1)
			   (width . 80)
			   ;;(top . -1)
			   ;;(left . 1)
			   (vertical-scroll-bar . nil)
			   (horizontal-scroll-bar . nil)
			   (unsplittable . t))
  "Alist of switches for the appearance of the detached minibuffer screen.")

(defvar initial-screen-hooks nil
  "Hook to run after initial screen startup." )

(defvar screen-creation-func '-no-window-system-yet-
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
			   window-system-switches))))

;; Setup for single attached minibuffer screen style

(defun attached-minibuffer-startup (window-system-switches)
       (select-screen (setq global-minibuffer-screen
		       (funcall screen-creation-func
				window-system-switches))))

;; Setup for minibuffer/screen style

(defun multi-minibuffer-startup (window-system-switches)
  (select-screen
   (funcall screen-creation-func window-system-switches)))

;; This is called from the window-system specific function which is attached
;; to window-setup-hook.

(defvar first-screen-user-positioned nil)
(defvar separate-minibuffer-screen nil)

(defun pop-initial-screen (window-system-switches)
  (let ((mini (assq 'minibuffer window-system-switches)))
    (setq first-screen-user-positioned
	  (and (assq 'top window-system-switches)
	       (assq 'left window-system-switches)))
    ;; jwz: disabled this, because it crashes too often.
;    (if (or separate-minibuffer-screen
;	    (and mini (eq (cdr mini) 'none)))
;	(detached-minibuffer-startup window-system-switches)
      (multi-minibuffer-startup window-system-switches))
;    )
  ;; I think this isn't useful, because .emacs hasn't been loaded yet.
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

;;  (defun next-multiscreen-window (arg)
;;    "Select the next window, regardless of which screen it is on."
;;    (interactive "p")
;;    (select-window (next-window (selected-window)
;;  			      (> (minibuffer-depth) 0)
;;  			      t)))

;;; Iconifying emacs.
;;;
;;; The function iconify-emacs replaces every non-iconified emacs window
;;; with a *single* icon.  Iconified emacs windows are left alone.  When
;;; emacs is in this globally-iconified state, de-iconifying any emacs icon
;;; will uniconify all screens that were visible, and iconify all screens
;;; that were not.  This is done by temporarily changing the value of
;;; `map-screen-hook' to `deiconify-emacs' (which should never be called 
;;; except from the map-screen-hook while emacs is iconified.)
;;;
;;; The title of the icon representing all emacs screens is controlled by
;;; the variable `icon-name'.  This is done by temporarily changing the
;;; value of `screen-icon-title-format'.  Unfortunately, this changes the
;;; titles of all emacs icons, not just the "big" icon.
;;;
;;; It would be nice if existing icons were removed and restored by
;;; iconifying the emacs process, but I couldn't make that work yet.

(defvar icon-name (concat "emacs @ " system-name))

(defvar iconification-data nil)

(defun iconify-emacs ()
  (interactive)
  (if iconification-data (error "already iconified?"))
  (let* ((screens (screen-list))
	 (rest screens)
	 (me (selected-screen))
	 screen)
    (while rest
      (setq screen (car rest))
      (setcar rest (cons screen (screen-visible-p screen)))
;      (if (memq (cdr (car rest)) '(icon nil))
;	  (progn
;	    (make-screen-visible screen) ; deiconify, and process the X event
;	    (sleep-for 500 t) ; process X events; I really want to XSync() here
;	    ))
      (or (eq screen me) (make-screen-invisible screen))
      (setq rest (cdr rest)))
    (or (boundp 'map-screen-hook) (setq map-screen-hook nil))
    (setq iconification-data
	    (list screen-icon-title-format map-screen-hook screens)
	  screen-icon-title-format icon-name
	  map-screen-hook 'deiconify-emacs)
    (iconify-screen me)))

(defun deiconify-emacs (&optional ignore)
  (or iconification-data (error "not iconified?"))
  (setq screen-icon-title-format (car iconification-data)
	map-screen-hook (car (cdr iconification-data))
	iconification-data (car (cdr (cdr iconification-data))))
  (while iconification-data
    (let ((visibility (cdr (car iconification-data))))
      (cond ((eq visibility 't)
	     (make-screen-visible (car (car iconification-data))))
;	    (t ;; (eq visibility 'icon)
;	     (make-screen-visible (car (car iconification-data)))
;	     (sleep-for 500 t) ; process X events; I really want to XSync() here
;	     (iconify-screen (car (car iconification-data))))
	    ;; (t nil)
	    ))
    (setq iconification-data (cdr iconification-data))))


;;
;; Screen-Window functions
;;

(defun other-window-any-screen (n)
  "Select the ARG'th different window on any screen.
All windows on current screen are arranged in a cyclic order.
This command selects the window ARG steps away in that order.
A negative ARG moves in the opposite order.  However, unlike
`other-window', this command will select a window on the next
\(or previous) screen instead of wrapping around to the top
\(or bottom) of this screen, when there are no more windows."
  (interactive "p")
  (other-window n t)
  ;; Click-to-type window managers do this automatically, but twm doesn't
  ;; unless you are in auto-raise mode.  It's not unreasonable to want M-o
  ;; to raise the screen without mouse-motion raising the windows the mouse
  ;; passes over, so we make raising the screen be the policy of M-o...
  ;; I think it's probably wrong for select-window to raise the screen,
  ;; that's too severe; but this is just one command.
  (raise-screen (selected-screen))
  )

(defun single-window-screen (&optional screen)
  (let* ((s (or screen (selected-screen)))
	 (w (screen-selected-window s)))
    (eq w (next-window w 0 nil))))

(defun one-screen (&optional screen)
  "Delete all screens but SCREEN (default is current screen).
Also delete all windows but the selected one on SCREEN."
  (interactive)
  (let* ((s (or screen (selected-screen)))
	 (this (next-screen s)))
    (while (not (eq this s))
      (delete-screen this)
      (setq this (next-screen s)))
    (delete-other-windows (screen-selected-window s))))

;; (define-key ctl-x-map "1" 'one-screen)

(define-key esc-map "o" 'other-window-any-screen)
(define-key global-map "\^Z" 'iconify-emacs)
;;(define-function-key global-function-map 'xk-f2 'buffer-in-other-screen)



(defun find-file-new-screen (filename)
  "Just like find-file, but creates a new screen for it first."
  (interactive "FFind file in new screen: ")
  (let* ((buf (find-file-noselect filename))
	 (scr (and screen-creation-func
		   (funcall screen-creation-func nil))))
    (if scr (select-screen scr))
    (switch-to-buffer buf)))

(defun switch-to-buffer-new-screen (buffer)
  "Just like switch-to-buffer, but creates a new screen for it first."
  (interactive "BSwitch to buffer in new screen: ")
  (if screen-creation-func
      (select-screen (funcall screen-creation-func nil)))
  (switch-to-buffer buffer))


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
  (interactive)
  (let* ((screen (selected-screen))
	 (bar (cdr (assoc 'auto-lower (screen-parameters screen)))))
    (modify-screen-parameters screen
			      (list (cons 'auto-lower (not bar))))))

(defun toggle-auto-lower ()
  (interactive)
  (let* ((screen (selected-screen))
	 (bar (cdr (assoc 'auto-lower (screen-parameters screen)))))
    (modify-screen-parameters screen
			      (list (cons 'auto-lower (not bar))))))

;(defun toggle-vertical-bar ()
;  (interactive)
;  (let* ((screen (selected-screen))
;	 (bar (cdr (assoc 'vertical-scroll-bar (screen-parameters screen)))))
;    (modify-screen-parameters screen
;			      (list (cons 'vertical-scroll-bar (not bar))))))

;(defun toggle-horizontal-bar ()
;  (interactive)
;  (let* ((screen (selected-screen))
;	 (bar (cdr (assoc 'horizontal-scroll-bar (screen-parameters screen)))))
;    (modify-screen-parameters screen
;			      (list (cons 'horizontal-scroll-bar (not bar))))))


;;; auto-raise and auto-lower

(defvar auto-raise-screen nil
  "*If true, screens will be raised to the top when selected.
Under X, most ICCCM-compliant window managers will have an option to do this 
for you, but this variable is provided in case you're using a broken WM.")

(defvar auto-lower-screen nil
  "*If true, screens will be lowered to the bottom when no longer selected.
Under X, most ICCCM-compliant window managers will have an option to do this 
for you, but this variable is provided in case you're using a broken WM.")

(defun default-select-screen-hook ()
  "Implements the `auto-raise-screen' variable.
For use as the value of `select-screen-hook'."
  (if auto-raise-screen (raise-screen (selected-screen))))

(defun default-deselect-screen-hook ()
  "Implements the `auto-lower-screen' variable.
For use as the value of `deselect-screen-hook'."
  (if auto-lower-screen (lower-screen (selected-screen))))

(or select-screen-hook
    (add-hook 'select-screen-hook 'default-select-screen-hook))

(or deselect-screen-hook
    (add-hook 'deselect-screen-hook 'default-deselect-screen-hook))


;;; Application-specific screen-management

(defvar get-screen-for-buffer-default-screen-name nil
  "The default screen to select; see doc of `get-screen-for-buffer'.")

(defun get-screen-name-for-buffer (buffer)
  (let ((mode (save-excursion (set-buffer buffer) major-mode)))
    (or (get mode 'screen-name)
	get-screen-for-buffer-default-screen-name)))

(defun get-screen-for-buffer (buffer &optional not-this-window-p on-screen)
  "Select and return a screen in which to display BUFFER.
Normally, the buffer will simply be displayed in the current screen.
But if the symbol naming the major-mode of the buffer has a 'screen-name
property (which should be a symbol), then the buffer will be displayed in
a screen of that name.  If there is no screen of that name, then one is
created.  

If the major-mode doesn't have a 'screen-name property, then the screen
named by `get-screen-for-buffer-default-screen-name' will be used.  If
that is nil (the default) then the currently selected screen will used.

If the screen-name symbol has an 'instance-limit property (an integer)
then each time a buffer of the mode in question is displayed, a new screen
with that name will be created, until there are `instance-limit' of them.
If instance-limit is 0, then a new screen will be created each time.

If a buffer is already displayed in a screen, then `instance-limit' is 
ignored, and that screen is used.

If the screen-name symbol has a 'screen-defaults property, then that is
prepended to the `screen-default-alist' when creating a screen for the
first time.

This function may be used as the value of `pre-display-buffer-hook', to 
cause the display-buffer function and its callers to exhibit the above
behavior."
  (if (or on-screen (eq (selected-window) (minibuffer-window)))
      ;; don't switch screens if a screen was specified, or to list
      ;; completions from the minibuffer, etc.
      nil
    ;; else
  (let ((name (get-screen-name-for-buffer buffer)))
    (if (null name)
	(selected-screen)
      (let ((limit (get name 'instance-limit))
	    (defaults (get name 'screen-defaults))
	    (screens (screen-list))
	    (matching-screens '())
	    screen already-visible)
	;; Sort the list so that iconic screens will be found last.  They
	;; will be used too, but mapped screens take prescedence.  And
	;; fully visible screens come before occluded screens.
	(setq screens
	      (sort screens
		    (function
		     (lambda (s1 s2)
		       (cond ((screen-totally-visible-p s2)
			      nil)
			     ((not (screen-visible-p s2))
			      (screen-visible-p s1))
			     ((not (screen-totally-visible-p s2))
			      (and (screen-visible-p s1)
				   (screen-totally-visible-p s1))))))))
	;; but the selected screen should come first, even if it's occluded,
	;; to minimize thrashing.
	(setq screens (cons (selected-screen)
			    (delq (selected-screen) screens)))

	(setq name (symbol-name name))
	(while screens
	  (setq screen (car screens))
	  (if (equal name (screen-name screen))
	      (if (get-buffer-window buffer screen)
		  (setq already-visible screen
			screens nil)
		(setq matching-screens (cons screen matching-screens))))
	  (setq screens (cdr screens)))
	(cond (already-visible
	       (select-screen already-visible)
	       (make-screen-visible already-visible)
	       already-visible)
	      ((or (null matching-screens)
		   (eq limit 0) ; means create with reckless abandon
		   (and limit (< (length matching-screens) limit)))
	       (let ((sc (funcall screen-creation-func
				  (cons (cons 'name name)
					(append defaults
						screen-default-alist)))))
		 (select-screen sc)
		 (make-screen-visible sc)
		 ;; make the one buffer being displayed in this newly created
		 ;; screen be the buffer of interest, instead of something
		 ;; random, so that it won't be shown in two-window mode.
		 (switch-to-buffer buffer)
		 sc))
	      (t
	       (select-screen (car matching-screens))
	       (make-screen-visible (car matching-screens))
	       ;; do not switch any of the window/buffer associations in an
	       ;; existing screen; this function only picks a screen; the
	       ;; determination of which windows on it get reused is up to
	       ;; display-buffer itself.
;;	       (or (window-dedicated-p (selected-window))
;;		   (switch-to-buffer buffer))
	       (car matching-screens))))))))

(defun show-temp-buffer-in-current-screen (buffer)
  "For use as the value of temp-buffer-show-function:
always displays the buffer in the current screen, regardless of the behavior
that would otherwise be introduced by the `pre-display-buffer-function', which
is normally set to `get-screen-for-buffer' (which see.)"
  (let ((pre-display-buffer-function nil)) ; turn it off, whatever it is
    (let ((window (display-buffer buffer)))
      (if (not (eq (selected-screen) (window-screen window)))
	  ;; only the pre-display-buffer-function should ever do this.
	  (error "display-buffer switched screens on its own!!"))
      (setq minibuffer-scroll-window window)
      (set-window-start window 1) ; obeys narrowing
      (set-window-point window 1)
      nil)))

(setq pre-display-buffer-function 'get-screen-for-buffer)
(setq temp-buffer-show-function 'show-temp-buffer-in-current-screen)
