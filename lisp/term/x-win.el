;;; x-win.el --- runtime initialization for the X window system
;; Copyright (C) 1990, 1993, 1994 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals

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

;;; Commentary:

;; X-win.el:  this file is loaded from ../lisp/startup.el when it recognizes
;; that X windows are to be used.  The X display is opened and hooks are set
;; for popping up the initial window.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window (s).

;;; Code:


;;; If you want to change this variable, this is the place you must do it.
;;; Do not set it to a string containing periods.  X doesn't like that.
;(setq x-emacs-application-class "Emacs")

(if (not (eq window-system 'x))
    (error "%s: Loading x-win.el but not compiled for X" (invocation-name)))

(require 'screen)
(require 'x-faces)
(require 'x-iso8859-1)
(require 'x-mouse)
(require 'xselect)

(setq screen-creation-function 'x-create-screen)
(setq mouse-motion-handler 'x-track-pointer)
(setq character-set-property 'x-iso8859/1) ; see x-iso8859-1.el

(add-hook 'suspend-hook
	  #'(lambda ()
	      (error "Suspending an emacs running under X makes no sense")))

(setq initial-screen-alist (if initial-screen-unmapped-p
			       '((initially-unmapped . t))
			     nil))

;; Open the X display when this file is loaded
;; (Note that the first screen is created later.)
(setq command-line-args-left
      (cdr (x-open-connection (cons (car command-line-args)
				    command-line-args-left))))


;;; selections and active regions

;;; If and only if zmacs-regions is true:
;;;
;;; When a mark is pushed and the region goes into the "active" state, we
;;; assert it as the Primary selection.  This causes it to be hilighted.
;;; When the region goes into the "inactive" state, we disown the Primary
;;; selection, causing the region to be dehilighted.
;;;
;;; Note that it is possible for the region to be in the "active" state
;;; and not be hilighted, if it is in the active state and then some other
;;; application asserts the selection.  This is probably not a big deal.

(defun x-activate-region-as-selection ()
  (if (marker-buffer (mark-marker t))
      (x-own-selection (cons (point-marker t) (mark-marker t)))))

;;; these are only ever called if zmacs-regions is true.
(setq zmacs-deactivate-region-hook 'x-disown-selection)
(setq zmacs-activate-region-hook 'x-activate-region-as-selection)
(setq zmacs-update-region-hook 'x-activate-region-as-selection)


;; Keypad type things

;; this is so that where-is says beginning-of-buffer is M-< instead of f27.
(fset 'fkey-beginning-of-buffer 'beginning-of-buffer)
(fset 'fkey-end-of-buffer	'end-of-buffer)
(fset 'fkey-scroll-down		'scroll-down)
(fset 'fkey-scroll-up		'scroll-up)
(fset 'fkey-scroll-left		'scroll-left)
(fset 'fkey-scroll-right	'scroll-right)
(fset 'fkey-backward-char	'backward-char)
(fset 'fkey-forward-char	'forward-char)
(fset 'fkey-backward-word	'backward-word)
(fset 'fkey-forward-word	'forward-word)
(fset 'fkey-backward-paragraph	'backward-paragraph)
(fset 'fkey-forward-paragraph	'forward-paragraph)
(fset 'fkey-other-window	'other-window)
(fset 'fkey-backward-other-window 'backward-other-window)
(fset 'fkey-beginning-of-line	'beginning-of-line)
(fset 'fkey-end-of-line		'end-of-line)
(fset 'fkey-repeat-complex-command 'repeat-complex-command)
(fset 'fkey-overwrite-mode	'overwrite-mode)

;; these two have to be defined in this more complicated way to make
;; current-column-tracking work, sigh...
(defun fkey-previous-line (p)
  (interactive "_p")
  (setq this-command 'previous-line)
  (previous-line p))
(defun fkey-next-line (p)
  (interactive "_p")
  (setq this-command 'next-line)
  (next-line p))

(defun fkey-popup-mode-menu ()
  (interactive)
  (call-interactively (key-binding [(button3)])))

;;; These aren't bound to kbd macros like "\C-b" so that they have the
;; expected behavior even in, for example, vi-mode.

;; We use here symbolic names, assuming that the corresponding keys will
;; generate these keysyms.  This is not true on Suns, but x-win-sun.el 
;; fixes that.  If it turns out that the semantics of these keys should
;; differ from server to server, this should be moved into server-specific
;; files, but these appear to be the standard Motif and PC bindings.

;; movement by units
(define-key global-map 'left		'fkey-backward-char)
(define-key global-map 'up		'fkey-previous-line)
(define-key global-map 'right		'fkey-forward-char)
(define-key global-map 'down		'fkey-next-line)

;; movement by larger blocks
(define-key global-map '(control left)	'fkey-backward-word)
(define-key global-map '(control up)	'fkey-backward-paragraph)
(define-key global-map '(control right)	'fkey-forward-word)
(define-key global-map '(control down)	'fkey-forward-paragraph)

;; movement by pages
(define-key global-map 'prior		'fkey-scroll-down)
(define-key global-map 'next		'fkey-scroll-up)
(define-key global-map '(control prior)	'fkey-scroll-right)
(define-key global-map '(control next)	'fkey-scroll-left)
;; potential Sunisms
(define-key global-map 'pgup		'fkey-scroll-down)
(define-key global-map 'pgdn		'fkey-scroll-up)
(define-key global-map '(control pgup)	'fkey-scroll-right)
(define-key global-map '(control pgdn)	'fkey-scroll-left)

;; movement to the limits
(define-key global-map 'home		'fkey-beginning-of-line)
(define-key global-map '(control home)	'fkey-beginning-of-buffer)
(define-key global-map 'end		'fkey-end-of-line)
(define-key global-map '(control end)	'fkey-end-of-buffer)
(define-key global-map 'begin		'fkey-beginning-of-line)
(define-key global-map '(control begin)	'fkey-beginning-of-buffer)

;; movement between windows
(define-key global-map '(control tab)	'fkey-other-window)
(define-key global-map '(control shift tab) 'fkey-backward-other-window)


;;; Miscellaneous key bindings

(define-key global-map 'again		'fkey-repeat-complex-command)
(define-key global-map 'insert		'fkey-overwrite-mode)

(define-key global-map 'kp_enter	[return]) ; do whatever RET does now
(define-key global-map 'kp_tab		[tab])

(define-key global-map 'undo		'undo)
(define-key global-map 'help		'help-for-help)
(define-key help-map   'help		'help-for-help)

;; (Are these Sunisms?)
(define-key global-map 'copy		'x-copy-primary-selection)
(define-key global-map 'paste		'x-yank-clipboard-selection)
(define-key global-map 'cut		'x-kill-primary-selection)

(define-key global-map 'menu		'fkey-popup-mode-menu)
;(define-key global-map '(shift menu)	'x-goto-menubar) ;NYI

;; if we define these this way (instead of leaving them bound to self-
;; insert-command), then the show-bindings display is hidiously cluttered.
;(define-key global-map 'kp_space	" ")
;(define-key global-map 'kp_equal	"=")
;(define-key global-map 'kp_multiply	"*")
;(define-key global-map 'kp_add		"+")
;(define-key global-map 'kp_separator	",")
;(define-key global-map 'kp_subtract	"-")
;(define-key global-map 'kp_decimal	".")
;(define-key global-map 'kp_divide	"/")


;;; OpenWindows-like "find" processing.  These functions are really Sunisms,
;;; but we put them here instead of in x-win-sun.el in case someone wants
;;; to use them when not running on a Sun console (presumably after adding
;;; the to different keys, or putting them on menus.)

(defvar ow-find-last-string nil)
(defvar ow-find-last-clipboard nil)

(defun ow-find (&optional backward-p)
  "Search forward the next occurence of the text of the selection."
  (interactive)
  (let ((sel (condition-case () (x-get-selection) (error nil)))
	(clip (condition-case () (x-get-clipboard) (error nil)))
	text)
    (setq text (cond
		(sel)
		((not (equal clip ow-find-last-clipboard))
		 (setq ow-find-last-clipboard clip))
		(ow-find-last-string)
		(t (error "No selection available"))))
    (setq ow-find-last-string text)
    (cond (backward-p
	   (search-backward text)
	   (set-mark (+ (point) (length text))))
	  (t
	   (search-forward text)
	   (set-mark (- (point) (length text)))))
    (zmacs-activate-region)))

(defun ow-find-backward ()
  "Search backward the previous occurence of the text of the selection."
  (interactive)
  (ow-find t))


;;; Load X-server specific code.
;;; Specifically, load some code to repair the grievous damage that MIT and
;;; Sun have done to the default keymap for the Sun keyboards.

(defun x-initialize-keyboard ()
  (cond (;; This is some heuristic junk that tries to guess whether this is
	 ;; a Sun keyboard.
	 ;;
	 ;; One way of implementing this (which would require C support) would
	 ;; be to examine the X keymap itself and see if the layout looks even
	 ;; remotely like a Sun - check for the Find key on a particular
	 ;; keycode, for example.  It'd be nice to have a table of this to
	 ;; recognize various keyboards; see also xkeycaps.
	 ;;
	 (let ((vendor (x-server-vendor)))
	 (or (string-match "Sun Microsystems" vendor)
	     ;; MIT losingly fails to tell us what hardware the X server
	     ;; is managing, so assume all MIT displays are Suns...  HA HA!
	     (string-equal "MIT X Consortium" vendor)))
       ;;
       ;; Ok, we think this could be a Sun keyboard.  Load the Sun code.
       ;;
       (load (concat term-file-prefix "x-win-sun") nil t)
       )
      ))

;; This runs after the first screen has been created (we can't talk to the X
;; server before that) but before the site-start-file or .emacs file, so sites
;; and users have a chance to override it.
(add-hook 'before-init-hook 'x-initialize-keyboard)


;
;; Ur, um, well, maybe.  I remain unconvinced.
;(defvar x-motif-compliant-p nil
;  "If non-nil, Emacs exhibits certain behaviors that are Motif-compliant but
;conflict with conventional Emacs usage.  Do not set this variable directly,
;except in a site-init file; use the command-line switches `-traditional'
;and `-compliant' or the function `x-make-motif-compliant'.")
;
;(defun x-make-motif-compliant ()
;  "Turn on Motif-compliant mode.
;This selects behavior in Info mode and such that is compliant with the
;Motif and CDE style guides but is not traditional for Emacs.  It also makes
;certain global key definitions and changes certain variables, so you should
;probably call this early in your .emacs file."
;  (setq x-motif-compliant-p t)
;  (setq bar-cursor 2)
;;  (define-key global-map 'f10 [(shift menu)])
;  (define-key global-map '(shift f10) [(menu)])
;)
;
;(if x-motif-compliant-p
;    (x-make-motif-compliant))



;; junk that used to be in menubar.el.

(make-obsolete 'x-new-screen 'make-screen)
(defun x-new-screen (&optional screen-name)
  "Obsolete; use `make-screen' instead."
  (interactive)
  (prog1
      (select-screen (make-screen (if screen-name
				      (list (cons 'name screen-name))
				    nil)))
    (switch-to-buffer "*scratch*")
  ))

(make-obsolete 'x-new-screen-other 'make-screen)
(defun x-new-screen-other (window-id &optional screen-params)
  "Obsolete; use `make-screen' instead."
  (prog1
      (select-screen (x-create-screen
		      (append screen-params default-screen-alist)
		      ;; #### should use a screen parameter for this
		      window-id))
    (switch-to-buffer "*scratch*")
    ))

;;; x-win.el ends here
