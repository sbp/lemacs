;;; -*-Emacs-Lisp-*-
;;;%Header
;;; Shrink-wrapped temporary windows for GNU Emacs V2.0
;;; Copyright (C) 1990 Chris McConnell, ccm@cs.cmu.edu.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.
;;;
;;; DESCRIPTION: This file is a replacement for popper.el when running
;;; under EPOCH.  It provides a dedicated screen for displaying
;;; temporary text called the popper screen.  It will work with any
;;; function that uses temporary windows.  The screen can be scrolled
;;; or buried from any other window.
;;;
;;; When a buffer is displayed using the function
;;; with-output-to-temp-buffer, the text will be displayed on the
;;; popper screen if the name of the buffer is in popper-pop-buffers
;;; or popper-pop-buffers is set to T.  Many kinds of completion and
;;; help information are displayed this way.  In general any buffer
;;; with *'s around its name will be a temporary buffer.

;;; USAGE: Load this file, preferably after byte-compiling it.  If you
;;; do not define key bindings using popper-load-hook, the bindings
;;; will be:
;;; 
;;;  C-z 1   popper-bury-output
;;;  C-z v   popper-scroll-output

;;; OPTIONS
;;;
;;; popper-screen-properties: This is an alist of window properties
;;; for the popper screen.  See the epoch manuals for properties and
;;; options. 
;;;
;;; popper-pop-buffers: By default this variable is T which causes all
;;; temporary buffers to be shown on the popper screen.  To have only
;;; specific buffers show on the popper screen, set this variable to a
;;; list of buffer names.

;;; EXAMPLE
;;;
;;; (setq popper-load-hook 
;;;      '(lambda ()
;;;        ;; Define key bindings
;;;        (define-key global-map "\C-c1" 'popper-bury-output)
;;;        (define-key global-map "\C-cv" 'popper-scroll-output)))
;;; (require 'epoch-pop)

;;;%Variables
(defvar popper-load-hook nil
  "List of functions to run when the popper module is loaded.")

;;;
(defvar popper-pop-buffers t
  "List of buffers to put in the shrink-wrapped pop-up window.  
If it is T, all temporary buffers will be put in the pop-up window.")

;;;
(defvar popper-buffers-to-skip nil
  "Not used in epoch-pop.")

;;;
(defvar popper-screen-properties
      '((icon-name . "** Popper Window **")
	(title     . "** Popper Window **")
	(font . "8x13")
	(cursor-glyph . 58)   ; pointing hand
	(reverse . nil)
	(foreground . "black")
	(background . "white")
	(cursor-foreground . "black")
	(geometry . "80x10+10+10")
	)
  "Window properties for the popper screen.")
(defvar popper-screen () "The screen being used as the popper window.")

;;; This should be in emacs, but it isn't.
(defun popper-mem (item list &optional elt=)
  "Test to see if ITEM is equal to an item in LIST.
Option comparison function ELT= defaults to equal."
  (let ((elt= (or elt= (function equal)))
	(done nil))
    (while (and list (not done))
      (if (funcall elt= item (car list))
	  (setq done list)
	  (setq list (cdr list))))
    done))

;;;
(defun popper-screen ()
  "Return the popper screen, creating if necessary."
  (if (and popper-screen (screen-information popper-screen))
      popper-screen
      (let ((epoch-mode-alist (cons (cons major-mode popper-screen-properties)
				    epoch-mode-alist)))
	(setq popper-screen (create-screen (current-buffer)))
	;; Set the bury message here
	)))

;;;
(defun popper-bury-output (&optional no-error)
  "Bury the popper output signalling an error if not there unless
optional NO-ERROR is T."
  (interactive)
  (lower-screen (popper-screen)))

;;;
(defun popper-scroll-output (&optional n)
  "Scroll text of the popper window upward ARG lines ; or near full
screen if no ARG.  When calling from a program, supply a number as
argument or nil.  If the output window is not being displayed, it will
be brought up."
  (interactive "P")
  (save-screen-excursion
    (let* ((screen (popper-screen))
	   (window (epoch::selected-window screen)))
      (raise-screen screen)
      ;; This should not scroll unless the window was already up.
      ;; Now if I could only find a way to sense that.
      (select-window window)
      (scroll-up n))))

;;;
(defun popper-show (buffer)
  "Function to display BUFFER in a popper window if it is in
popper-pop-buffers or popper-pop-buffers is T."
  (if (or (eq popper-pop-buffers t)
	  (popper-mem (buffer-name buffer) popper-pop-buffers))
      (save-screen-excursion
       (let* ((screen (popper-screen))
	      (window (epoch::selected-window screen)))
	 (set-window-buffer window buffer)
	 (mapraised-screen screen)))
      (display-buffer buffer)))

;;;
(setq temp-buffer-show-hook 'popper-show)
(run-hooks 'popper-load-hook)

;;; Default key bindings
(if (not (where-is-internal 'popper-bury-output nil t))
    (progn
      (if (not (keymapp (lookup-key global-map "\C-z")))
	  (define-key global-map "\C-z" (make-keymap)))
      (define-key global-map "\C-z1" 'popper-bury-output)
      (define-key global-map "\C-zv" 'popper-scroll-output)))

(provide 'epoch-pop)