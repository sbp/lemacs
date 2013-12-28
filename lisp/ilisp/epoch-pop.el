;;; -*-Emacs-Lisp-*-
;;;%Header
;;; Shrink-wrapped temporary windows for GNU Emacs V2.11
;;; Copyright (C) 1990, 1991, 1992 Chris McConnell, ccm@cs.cmu.edu.

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
;;; function that uses temporary windows or that has been wrapped
;;; using popper-wrap.  The screen can be scrolled or buried from any
;;; other window.  
;;;
;;; When a buffer is displayed using the function
;;; with-output-to-temp-buffer, the text will be displayed in the
;;; popper window if the name of the buffer is in popper-pop-buffers
;;; or popper-pop-buffers is set to T and the name is not in
;;; popper-no-pop-buffers.  Many kinds of completion and help
;;; information are displayed this way.  In general any buffer with
;;; *'s around its name will be a temporary buffer.  Some commands
;;; like shell-command do not use with-output-to-temp-buffer even
;;; though you might like to have their output be temporary.  For
;;; commands like this, you can define a wrapper like this using the
;;; macro popper-wrap.

;;; USAGE: Load this file, preferably after byte-compiling it.  If you
;;; do not define key bindings using popper-load-hook, the bindings
;;; will be:
;;; 
;;;  C-z 1   popper-bury-output
;;;  C-z v   popper-scroll-output

;;; See %%User variables below for possible options.  Here is a sample
;;; load hook for your .emacs:
;;;
;;; (setq popper-load-hook 
;;;      '(lambda ()
;;;        ;; Define key bindings
;;;        (define-key global-map "\C-c1" 'popper-bury-output)
;;;        (define-key global-map "\C-cv" 'popper-scroll-output)))
;;; (require 'epoch-pop)
(require 'epoch-util)

;;;%Variables
;;;%%User
(defvar popper-load-hook nil
  "*List of functions to run when the popper module is loaded.")

;;;
(defvar popper-pop-buffers t
  "*List of buffers to put in the popper window.  If it is T, all
temporary buffers not in popper-no-pop-buffers will be put there.")

(defvar popper-no-pop-buffers nil
  "*List of buffers to not put in the popper window when
popper-pop-buffers is T.")

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
  "*Window properties for the popper screen.")

(defvar popper-mode-line-text nil
  "*Minor mode text for mode line of popper buffers.  If nil, it will
be set to a short help message on first use of popper.")

;;;%%Internal
(defvar popper-screen () "The screen being used as the popper window.")

(defvar popper-buffer nil
  "Indicates buffer is a popper for minor-mode-alist.")
(make-variable-buffer-local 'popper-buffer)
(or (assq 'popper-buffer minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(popper-buffer popper-mode-line-text) minor-mode-alist)))

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
  (if (and popper-screen (epoch::screen-information popper-screen))
      popper-screen
    ;; I would love to not focus here if I could figure out how
    (setq popper-screen 
	  (create-screen nil popper-screen-properties))))

;;;
(defun popper-output-buffer ()
  "Return the buffer being displayed in the popper window."
  (if popper-screen
      (window-buffer (epoch::selected-window popper-screen))))

;;;
(defun popper-bury-output (&optional no-error)
  "Bury the popper output signalling an error if not there unless
optional NO-ERROR is T."
  (interactive)
  (epoch::iconify-screen (popper-screen)))

;;;
(defun popper-scroll-output (&optional n)
  "Scroll text of the popper window upward ARG lines ; or near full
screen if no ARG.  When calling from a program, supply a number as
argument or nil.  If the output window is not being displayed, it will
be brought up."
  (interactive "P")
  (let* ((screen (popper-screen))
	 (window (epoch::selected-window screen)))
    (save-screen-excursion
     (epoch::map-screen screen)
     (epoch::select-screen screen)
     ;; This should not scroll unless the window was already up.
     ;; Now if I could only find a way to sense that.
     (select-window window)
     (condition-case ()
	 (scroll-up n)
       (error (if (null n)
		  (set-window-start (selected-window) 0)))))))

;;;
(defun popper-show-output (buffer)
  (let* ((screen (popper-screen))
	 (window (epoch::selected-window screen)))
    (save-screen-excursion
     (epoch::select-screen screen)
     (set-window-buffer window buffer)
     (set-buffer buffer)
     (or popper-mode-line-text
	 (setq popper-mode-line-text
	       (list
		(format " %s bury, %s scroll" 
			(where-is-internal 'popper-bury-output nil t)
			(where-is-internal 'popper-scroll-output nil t)))))
     (setq popper-buffer t)
     (mapraised-screen screen))))

;;;
(defun popper-show (buffer)
  "Function to display BUFFER in a popper window if it is in
popper-pop-buffers or popper-pop-buffers is T and it is not in
popper-no-pop-buffers."
  (if (eq popper-pop-buffers t)
      (if (popper-mem (buffer-name buffer) popper-no-pop-buffers)
	  (display-buffer buffer)
	  (popper-show-output buffer))
      (if (popper-mem (buffer-name buffer) popper-pop-buffers))
	  (popper-show-output buffer)
	  (display-buffer buffer)))

;;; %Wrappers
(defun popper-unwrap (function)
  "Remove the popper wrapper for NAME."
  (let ((var (car (read-from-string (format "popper-%s" function)))))
    (if (boundp var)
	(progn (fset function (symbol-value var))
	       (makunbound var)))))

;;;
(defun popper-wrap (function buffer)
  "Define a wrapper on FUNCTION so that BUFFER will be a pop up window."
  (popper-unwrap function)
  (let* ((var (car (read-from-string (format "popper-%s" function))))
	 (defn (symbol-function function))
	 arg-spec doc int)
    (set var defn)
    (if (consp defn)
	(setq arg-spec (elt defn 1)
	      doc (elt defn 2)
	      int (elt defn 3))
	(setq arg-spec (aref defn 0)
	      doc (and (> (length defn) 4) (aref defn 4))
	      int (and (> (length defn) 5) (list 'interactive (aref defn 5)))))
    (fset function 
	  (append 
	   (list 'lambda arg-spec)
	   (if (numberp doc) (list (documentation function)))
	   (if (stringp doc) (list doc))
	   (if (eq (car int) 'interactive) (list int))
	   (list 
	    (list
	     'let '((shown nil))
	     (list 'save-window-excursion 
		   (cons 'funcall 
			 (cons 
			  var
			  (let ((args nil))
			    (while arg-spec
			      (if (not (eq (car arg-spec) '&optional))
				  (setq args (cons (car arg-spec)
						   args)))
			      (setq arg-spec (cdr arg-spec)))
			    (reverse args))))
		   (list 'setq 'shown (list 'get-buffer-window buffer)))
	     (list 'if 'shown
		   (list 'funcall 'temp-buffer-show-hook buffer))))))
    (if (not (eq popper-pop-buffers t))
	(let ((elt popper-pop-buffers))
	  (while (consp elt)
	    (if (string= (car elt) buffer) 
		(setq elt t)
		(setq elt (cdr elt))))
	  (if (not elt)
	      (setq popper-pop-buffers (cons buffer popper-pop-buffers)))))))

;;; 
(popper-wrap 'shell-command "*Shell Command Output*")
(popper-wrap 'shell-command-on-region "*Shell Command Output*")

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
