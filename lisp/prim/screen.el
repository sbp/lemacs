;;; screen.el --- multi-screen management independent of window systems.

;; Copyright (C) 1990, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(define-key global-map "\^Z" 'iconify-emacs)

;; Former, inferior names for functions.
;; These may disappear at some point.

(fset 'live-screen-p 'screen-live-p)
(fset 'x-pixel-width 'screen-pixel-width)
(fset 'x-pixel-height 'screen-pixel-height)

;; These are called from select_screen()

(defvar select-screen-hook nil
  "Function or functions to run just after a new screen is selected.")

(defvar deselect-screen-hook nil
  "Function or functions to run just before selecting another screen.")

(defvar screen-creation-function '-no-window-system-yet-
  "Window-system dependent function to call to create a new screen.
The window system startup file should set this to its screen creation
function, which should take an alist of parameters as its argument.")

(defvar default-screen-alist nil
  "Alist of default values for screen creation, other than the first one.
These may be set in your init file, like this:

  (setq default-screen-alist '((width . 80) (height . 55)))

Since the first X screen is created before loading your .emacs file,
you must use the X resource database for that.

See also the variable `x-screen-defaults', which is like `default-screen-alist'
except that it applies only to X screens (wheras `default-screen-alist' applies
to all types of screens.)")

(defvar initial-screen-alist nil
  "Alist of default values for the first screen.
This may be set by the window-system-specific init file.")


;;;; Creating the initial window-system screen

(defun screen-initialize ()
  (cond ((and window-system (not (noninteractive)))
	 ;; Don't call select-screen here - focus is a matter of WM policy.
	 (make-screen initial-screen-alist)
	 (delete-screen terminal-screen)
	 (setq terminal-screen nil))
	(t
	 ;; We're not running a window system, so arrange to cause errors.
	 (setq screen-creation-function
	       #'(lambda (parameters)
		   (error
		 "Can't create multiple screens without a window system"))))))


;;;; Creation of additional screens, and other screen miscellanea

(defun get-other-screen ()
 "Return some screen other than the current screen, creating one if necessary."
  (let* ((this (selected-screen))
	 ;; search visible screens first
	 (next (next-screen this nil t)))
    ;; then search iconified screens
    (if (eq this next)
	(setq next (next-screen this nil nil)))
    (if (eq this next)
	;; otherwise, make a new screen
	(make-screen)
      next)))

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


;; Alias, kept temporarily.
(defalias 'new-screen 'make-screen)
(make-obsolete 'new-screen 'make-screen)

(defun make-screen (&optional parameters)
  "Create a new screen, displaying the current buffer.

Optional argument PARAMETERS is an alist of parameters for the new
screen.  Specifically, PARAMETERS is a list of pairs, each having one
of the following forms:

 (name . STRING)       - The screen should be named STRING.
 (height . NUMBER)     - The screen should be NUMBER text lines high.
 (width . NUMBER)      - The screen should be NUMBER columns wide.

The documentation for the function `x-create-screen' describes
additional screen parameters that Emacs recognizes for X window screens."

; (minibuffer . t)      - the screen should have a minibuffer
; (minibuffer . nil)    - the screen should have no minibuffer
; (minibuffer . only)   - the screen should contain only a minibuffer
; (minibuffer . WINDOW) - the screen should use WINDOW as its minibuffer window.
  (interactive)
  (let (nscreen)
    ;; lemacs has a more versatile hook than these
    ;;(run-hooks 'before-make-screen-hook)
    (setq nscreen (funcall screen-creation-function
			   (append parameters
				   ;; Where does FSFmacs consult this?
				   default-screen-alist)))
    ;;(run-hooks 'after-make-screen-hook)
    nscreen))

;(defun filtered-screen-list (predicate)
;  "Return a list of all live screens which satisfy PREDICATE."
;  (let ((screens (screen-list))
;	good-screens)
;    (while (consp screens)
;      (if (funcall predicate (car screens))
;	  (setq good-screens (cons (car screens) good-screens)))
;      (setq screens (cdr screens)))
;    good-screens))

;(defun minibuffer-screen-list ()
;  "Return a list of all screens with their own minibuffers."
;  (filtered-screen-list
;   (function (lambda (screen)
;	       (eq screen (window-screen (minibuffer-window screen)))))))

;(defun screen-remove-geometry-params (param-list)
;  "Return the parameter list PARAM-LIST, but with geometry specs removed.
;This deletes all bindings in PARAM-LIST for `top', `left', `width',
;and `height' parameters.
;Emacs uses this to avoid overriding explicit moves and resizings from
;the user during startup."
;  (setq param-list (cons nil param-list))
;  (let ((tail param-list))
;    (while (consp (cdr tail))
;      (if (and (consp (car (cdr tail)))
;	       (memq (car (car (cdr tail))) '(height width top left)))
;	  (setcdr tail (cdr (cdr tail)))
;	(setq tail (cdr tail)))))
;  (cdr param-list))


(defun other-screen (arg)
  "Select the ARG'th different visible screen, and raise it.
All screens are arranged in a cyclic order.
This command selects the screen ARG steps away in that order.
A negative ARG moves in the opposite order."
  (interactive "p")
  (let ((screen (selected-screen)))
    (while (> arg 0)
      (setq screen (next-screen screen nil t))
      (setq arg (1- arg)))
    (while (< arg 0)
      (setq screen (previous-screen screen nil t))
      (setq arg (1+ arg)))
    (raise-screen screen)
    (select-screen screen)
    ))

;;;; Screen configurations

;; This stuff doesn't quite work yet - feel free to fix it

;(defun current-screen-configuration ()
;  "Return a list describing the positions and states of all screens.
;Its car is `screen-configuration'.
;Each element of the cdr is a list of the form (SCREEN ALIST WINDOW-CONFIG),
;where
;  SCREEN is a screen object,
;  ALIST is an association list specifying some of SCREEN's parameters, and
;  WINDOW-CONFIG is a window configuration object for SCREEN."
;  (cons 'screen-configuration
;	(mapcar (function
;		 (lambda (screen)
;		   (list screen
;			 (screen-parameters screen)
;			 (current-window-configuration screen))))
;		(screen-list))))

;(defun set-screen-configuration (configuration &optional nodelete)
;  "Restore the screens to the state described by CONFIGURATION.
;Each screen listed in CONFIGURATION has its position, size, window
;configuration, and other parameters set as specified in CONFIGURATION.
;Ordinarily, this function deletes all existing screens not
;listed in CONFIGURATION.  But if optional second argument NODELETE
;is given and non-nil, the unwanted screens are iconified instead."
;  (or (screen-configuration-p configuration)
;      (signal 'wrong-type-argument
;	      (list 'screen-configuration-p configuration)))
;  (let ((config-alist (cdr configuration))
;	screens-to-delete)
;    (mapcar (function
;	     (lambda (screen)
;	       (let ((parameters (assq screen config-alist)))
;		 (if parameters
;		     (progn
;		       (modify-screen-parameters
;			screen
;			;; Since we can't set a screen's minibuffer status, 
;			;; we might as well omit the parameter altogether.
;			(let* ((parms (nth 1 parameters))
;			       (mini (assq 'minibuffer parms)))
;			  (if mini (setq parms (delq mini parms)))
;			  parms))
;		       (set-window-configuration (nth 2 parameters)))
;		   (setq screens-to-delete (cons screen screens-to-delete))))))
;	    (screen-list))
;    (if nodelete
;	;; Note: making screens invisible here was tried
;	;; but led to some strange behavior--each time the screen
;	;; was made visible again, the window manager asked afresh
;	;; for where to put it.
;	(mapcar 'iconify-screen screens-to-delete)
;      (mapcar 'delete-screen screens-to-delete))))

;(defun screen-configuration-p (object)
;  "Return non-nil if OBJECT seems to be a screen configuration.
;Any list whose car is `screen-configuration' is assumed to be a screen
;configuration."
;  (and (consp object)
;       (eq (car object) 'screen-configuration)))


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

(defvar icon-name nil) ; set this at run time, not load time.

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
    (or icon-name
	(setq icon-name (concat invocation-name " @ " (system-name))))
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

(defun get-screen-for-buffer-noselect (buffer
				       &optional not-this-window-p on-screen)
  "Return a screen in which to display BUFFER.
This is a subroutine of `get-screen-for-buffer' (which see.)"
  (let (name)
    (cond
     ((or on-screen (eq (selected-window) (minibuffer-window)))
      ;; don't switch screens if a screen was specified, or to list
      ;; completions from the minibuffer, etc.
      nil)

     ((setq name (get-screen-name-for-buffer buffer))
      ;;
      ;; This buffer's mode expressed a preference for a screen of a particular
      ;; name.  That always takes priority.
      ;;
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
		    #'(lambda (s1 s2)
			(cond ((screen-totally-visible-p s2)
			       nil)
			      ((not (screen-visible-p s2))
			       (screen-visible-p s1))
			      ((not (screen-totally-visible-p s2))
			       (and (screen-visible-p s1)
				    (screen-totally-visible-p s1)))))))
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
	       already-visible)
	      ((or (null matching-screens)
		   (eq limit 0) ; means create with reckless abandon
		   (and limit (< (length matching-screens) limit)))
	       (let* ((sc (funcall screen-creation-function
				   (cons (cons 'name name)
					 (append defaults
						 default-screen-alist))))
		      (w (screen-root-window sc)))
		 ;;
		 ;; Make the one buffer being displayed in this newly created
		 ;; screen be the buffer of interest, instead of something
		 ;; random, so that it won't be shown in two-window mode.
		 ;; Avoid calling switch-to-buffer here, since that's something
		 ;; people might want to call this routine from.
		 ;;
		 ;; (If the root window doesn't have a buffer, then that means
		 ;; there is more than one window on the screen, which can only
		 ;; happen if the user has done something funny on the screen-
		 ;; creation-hook.  If that's the case, leave it alone.)
		 ;;
		 (if (window-buffer w)
		     (set-window-buffer w buffer))
		 sc))
	      (t
	       ;; do not switch any of the window/buffer associations in an
	       ;; existing screen; this function only picks a screen; the
	       ;; determination of which windows on it get reused is up to
	       ;; display-buffer itself.
;;	       (or (window-dedicated-p (selected-window))
;;		   (switch-to-buffer buffer))
	       (car matching-screens)))))
     (t
      ;;
      ;; This buffer's mode did not express a preference for a screen of a
      ;; particular name.  So try to find a screen already displaying this
      ;; buffer.  
      ;;
      (let ((w (or (get-buffer-window buffer t)		; check visible first
		   (get-buffer-window buffer t t))))	; then iconic
	(cond ((null w)
	       ;; It's not in any window - return nil, meaning no screen has
	       ;; preference.
	       nil)
	      ((and not-this-window-p
		    (eq (selected-screen) (window-screen w)))
	       ;; It's in a window, but on this screen, and we have been
	       ;; asked to pick another window.  Return nil, meaning no
	       ;; screen has preference.
	       nil)
	      (t
	       ;; Otherwise, return the screen of the buffer's window.
	       (window-screen w))))))))


;; The pre-display-buffer-function is called for effect, so this needs to
;; actually select the screen it wants.  Fdisplay_buffer() takes notice of
;; changes to the selected screen.
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
prepended to the `default-screen-alist' when creating a screen for the
first time.

This function may be used as the value of `pre-display-buffer-function', 
to cause the display-buffer function and its callers to exhibit the above
behavior."
  (let ((old-screens (visible-screen-list))
	(screen (get-screen-for-buffer-noselect
		 buffer not-this-window-p on-screen)))
    (if (null screen)
	nil
      (select-screen screen)
      (or (member screen old-screens)
	  ;; If the screen was already visible, just focus on it.
	  ;; If it wasn't visible (it was just created, or it used
	  ;; to be iconified) then uniconify, raise, etc.
	  (make-screen-visible screen))
      screen)))


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


(provide 'screen)

;;; screen.el ends here
