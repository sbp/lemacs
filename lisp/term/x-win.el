;; Parse switches controlling how Emacs interfaces with X window system.
;; Copyright (C) 1990 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; X-win.el: this file is loaded from ../lisp/startup.el when it
;; recognizes that X windows are to be used.  The X display is opened
;; and hooks are set for popping up the initial window.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window (s).

;; The daemon stuff isn't really useful at the moment.
(defvar x-daemon-mode nil
  "When set, means initially create just a minibuffer.")
	  
(defun x-establish-daemon-mode (switch)
  (setq x-daemon-mode t))

(if (eq window-system 'x)
    (progn
      (setq window-setup-hook (cons 'x-pop-initial-window window-setup-hook))
      (setq screen-creation-func 'x-create-screen)
      (require 'screen)
      (require 'x-faces)
      (require 'x-iso8859-1)
      (setq suspend-hook
	    '(lambda ()
	       (error "Suspending an emacs running under X makes no sense")))
      (setq command-line-args-left (cdr (x-open-connection command-line-args)))
      )
  (error "Loading x-win.el but not compiled for X"))


;;; selections and active regions

;;; When something is placed on the kill-ring, we assert it as the
;;; Clipboard selection.
;;;
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

;(setq kill-hooks 'x-own-clipboard)
(setq kill-hooks '(x-own-clipboard x-store-cutbuffer))

;;; these are only ever called if zmacs-regions is true.
(setq zmacs-deactivate-region-hook 'x-disown-selection)
(setq zmacs-activate-region-hook 'x-activate-region-as-selection)
(setq zmacs-update-region-hook 'x-activate-region-as-selection)

;; This is the function which creates the first X window.  It is called
;; from startup.el after the user's init file is processed.

(defun x-pop-initial-window ()
  ;; xterm.c depends on using interrupt-driven input.
  (set-input-mode t nil t)
  (require 'x-mouse)
  (require 'xselect)
  (setq mouse-motion-handler 'x-track-pointer)
  (setq character-set-property 'x-iso8859/1) ; see x-iso8859-1.el
  ;; see screen.el for this function
  (pop-initial-screen ())
  (delete-screen terminal-screen)
  )


;; Keypad type things

(define-key global-map 'home		"\C-a")
(define-key global-map 'left		"\C-b")
(define-key global-map 'up		"\C-p")
(define-key global-map 'right		"\C-f")
(define-key global-map 'down		"\C-n")
(define-key global-map 'prior		"\C-p")
(define-key global-map 'next		"\C-n")
(define-key global-map 'end		"\C-e")
(define-key global-map 'begin		"\C-a")

(define-key global-map 'kp_enter	[return])
(define-key global-map 'kp_tab		[tab])

;; if we define these this way (instead of leaving them bound to
;; self-insert-command), then the ^Hb display is hidiously cluttered.
;(define-key global-map 'kp_space	" ")
;(define-key global-map 'kp_equal	"=")
;(define-key global-map 'kp_multiply	"*")
;(define-key global-map 'kp_add		"+")
;(define-key global-map 'kp_separator	",")
;(define-key global-map 'kp_subtract	"-")
;(define-key global-map 'kp_decimal	".")
;(define-key global-map 'kp_divide	"/")

(define-key global-map 'undo		'undo)
(define-key global-map 'help		'help-for-help)
(define-key help-map 'help		'help-for-help)


;; Horizontal split window do not work in this emacs
(substitute-key-definition 'split-window-horizontally 'x-new-screen global-map)
(substitute-key-definition 'split-window-horizontally 'x-new-screen ctl-x-map)
