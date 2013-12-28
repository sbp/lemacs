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

;;; DESCRIPTION: This module provides a single shrink-wrapped window
;;; for displaying temporary text called the popper window.  At any
;;; time there is only one popper window on the screen.  Its size will
;;; be the minimum of the number of lines of text being displayed and
;;; half the number of lines in the currently selected window when the
;;; popper window was created.  It will work with any function that
;;; uses temporary windows.  The window can be scrolled or buried from
;;; any other window.
;;;
;;; When a buffer is displayed using the function
;;; with-output-to-temp-buffer, the text will be displayed in the
;;; popper window if the name of the buffer is in popper-pop-buffers
;;; or popper-pop-buffers is set to T.  Many kinds of completion and
;;; help information are displayed this way.  In general any buffer
;;; with *'s around its name will be a temporary buffer.

;;; USAGE: Load this file, preferably after byte-compiling it.  If you
;;; do not define key bindings using popper-load-hook, the bindings
;;; will be:
;;; 
;;;  C-z 1   popper-bury-output
;;;  C-z v   popper-scroll-output
;;;  C-z g   popper-grow-output

;;; OPTIONS
;;;
;;; window-min-height: By default, this is set to 2 so that a window
;;; can be one line big.  The number of lines in the popper window
;;; plus the mode line will never be less than this value.  (In fact
;;; no window will be less than this value.)
;;;
;;; popper-pop-buffers: By default only the *Completions* buffer will
;;; show in the popper window.  To have other buffers show in the
;;; popper window, you can add the name of the buffer to this
;;; variable, or set it to T which will cause all temporary buffers to
;;; show in the popper window.  The problem with setting this variable
;;; to T is that some temporary buffers are empty when first
;;; displayed.  This makes the window too small to be useful when
;;; information is later displayed there. 
;;;
;;; popper-buffers-to-skip: By default C-x o will skip any window
;;; displaying the *Completions* buffer.  Other buffer names can be
;;; skipped by putting their names on this variable.

;;; EXAMPLE
;;;
;;; (setq popper-load-hook 
;;;      '(lambda ()
;;;        ;; Define key bindings
;;;        (define-key global-map "\C-c1" 'popper-bury-output)
;;;        (define-key global-map "\C-cv" 'popper-scroll-output)
;;;        (define-key global-map "\C-cg" 'popper-grow-output)
;;;        ;; Add *Help* as a popper buffer
;;;	   (setq popper-pop-buffers (cons "*Help*" popper-pop-buffers)
;;;              popper-buffers-to-skip popper-pop-buffers)))
;;; (require 'popper)

;;; WARNING: This package redefines the function split-window and
;;; pop-to-buffer so that the popper window is buried before calling
;;; the old definition.
(defvar popper-load-hook nil
  "List of functions to run when the popper module is loaded.")

;;;
(defvar popper-pop-buffers '(" *Completions*")
  "List of buffers to put in the shrink-wrapped pop-up window.  
If it is T, all temporary buffers will be put in the pop-up window.")

(defvar popper-buffers-to-skip popper-pop-buffers
  "\\[popper-other-window] will skip over these buffers when they are
used in a temporary window.")

;;;
(defvar popper-output-buffers nil
  "LIFO list of buffers displayed in the popper window.")
(defvar popper-last-output-window nil
  "The window that last popped up an output window.")

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
(defun popper-select (&optional window)
  "Select WINDOW and its buffer.  WINDOW defaults to selected-window."
  (setq window (or window (selected-window)))
  (select-window window)
  (set-buffer (window-buffer window)))

;;;
(defun popper-output-buffer ()
  "Return the buffer being displayed in the popper window."
  (if (not (eq (selected-window) (next-window)))
      (let ((buffers popper-output-buffers)
	    (done nil))
	(while (and buffers (not done))
	  (let* ((buffer (car buffers))
		 (window (get-buffer-window buffer)))
	    (if window 
		(setq done buffer)
	      (setq buffers (cdr buffers)))))
	(if done (setq popper-output-buffers buffers))
	done)))

;;;
(defun popper-parent ()
  "Return the parent of the popper window."
  (let ((output (popper-output-buffer)))
    (if output 
	(next-window (get-buffer-window output) 'no))))

;;;
(defun popper-window-heights (window)
  "Return a list of the heights of all of the windows following WINDOW."
  (let ((heights nil))
    (select-window window)
    (while (progn 
	     (select-window (next-window (selected-window) 'no))
	     (not (eq (selected-window) window)))
      (setq heights (cons (window-height (selected-window)) heights)))
    (reverse heights)))

;;;
(defun popper-bury-output (&optional no-error)
  "Bury the popper output signalling an error if not there unless
optional NO-ERROR is T."
  (interactive)
  (let ((buffer (popper-output-buffer)))
    (if buffer
	(let* ((old (current-buffer))
	       (old-window (selected-window))
	       (output (get-buffer-window buffer))
	       (start (window-height output))
	       (parent (next-window output 'no))
	       (height (window-height output))
	       (heights (popper-window-heights output)))
	  (bury-buffer buffer)
	  (delete-window output)
	  (popper-select parent)
	  (while heights
	    (enlarge-window (- (+ height (car heights)) (window-height)))
	    (if start (condition-case () (scroll-down start) (error nil)))
	    (select-window (next-window (selected-window) 'no))
	    (setq height 0 start nil)
	    (setq heights (cdr heights)))
	  (set-buffer old)
	  (if (not (eq old-window output))
	      (select-window old-window)))
	(if (not no-error) (error "No output window to bury.")))))

;;;
(setq window-min-height 2)
(defun popper-shrink-window ()
  "Shrink the current window if larger than its buffer."
  (let* ((window (selected-window))
	 (window-lines (1- (window-height window))))
    (set-buffer (window-buffer window))
    (set-window-start window 0)
    (set-window-hscroll window 0)
    (let ((buffer-read-only nil)
	  (buffer-modified-p (buffer-modified-p)))
      ;; Delete trailing blank lines
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      ;; Delete leading blank lines
      (if (looking-at "\n+") (replace-match ""))
      (set-buffer-modified-p buffer-modified-p))
    (enlarge-window (- (max (1+ (vertical-motion window-lines))
			    (1- window-min-height))
		       window-lines))))

;;;
(defun popper-show-output (&optional buffer size)
  "Bring the output window up showing optional BUFFER in window of
SIZE.  If SIZE is not specified, then shrink the window.  Finally
select the original window."
  (let* ((window (selected-window))
	 (old-buffer (current-buffer))
	 (buffer (get-buffer-create
		  (or buffer (car popper-output-buffers) "*scratch*")))
	 start parent)
    (setq popper-last-output-window window)
    (if (eq window (minibuffer-window)) 
	(let* ((parent (popper-parent)))
	  (popper-bury-output t)
	  (select-window (setq window (or parent (previous-window)))))
	(popper-bury-output t))
    (setq start (window-start window))
    (split-window nil size)
    (set-window-buffer window buffer)
    (set-buffer buffer)
    (setq mode-line-process 
	  (list (format " %s bury, %s scroll" 
			(where-is-internal 'popper-bury-output nil t)
			(where-is-internal 'popper-scroll-output nil t))))
    (setq popper-output-buffers
	  (cons buffer (delq buffer popper-output-buffers)))
    (if (not size) (popper-shrink-window))
    (setq parent (next-window window 'no))
    (popper-select parent)
    ;; Move the window so that top lines get covered unless it would
    ;; cover point in which case point is at top of window
    (save-excursion
      (set-window-start parent start)
      (move-to-window-line (window-height window))
      (set-window-start parent (point)))
    (let ((point (save-excursion (beginning-of-line) (point))))
      (if (not (pos-visible-in-window-p point))
	  (set-window-start (selected-window) point)))
    (if (eq popper-last-output-window (minibuffer-window))
	(select-window (minibuffer-window)))
    (set-buffer old-buffer)))

;;;
(defun popper-scroll-output (&optional n)
  "Scroll text of the popper window upward ARG lines ; or near full
screen if no ARG.  When calling from a program, supply a number as
argument or nil.  If the output window is not being displayed, it will
be brought up."
  (interactive "P")
  (let ((buffer (popper-output-buffer)))
    (if buffer
	(let ((window (selected-window)))
	  (unwind-protect
	       (progn (select-window (get-buffer-window buffer))
		      (condition-case ()
			  (scroll-up n)
			(error (if (null n) 
				   (set-window-start (selected-window) 0)))))
	    (select-window window)))
	(popper-show-output))))

;;;
(defun popper-grow-output (&optional n)
  "Grow the popper window by ARG (default 1) lines.  If the popper
window is not being shown, it will be brought up."
  (interactive "p")
  (let ((buffer (popper-output-buffer)))
    (if buffer
	(let ((old-buffer (current-buffer))
	      (window (selected-window)))
	  (select-window (get-buffer-window buffer))
	  (enlarge-window n)
	  (popper-select (next-window (selected-window) 'no))
	  (save-excursion
	    (if (< n 0)
		(condition-case () (scroll-up n) (error nil))
		(move-to-window-line n)
		(set-window-start (selected-window) (point))))
	  (select-window window)
	  (set-buffer old-buffer))
	(popper-show-output))))

;;;***Redefine split-window to bury popper window first***
(defvar popper-split-window (symbol-function 'split-window) 
  "Original definition of split-window.")
(defun split-window (&optional window size hor-flag)
  "Split WINDOW, putting SIZE lines in the first of the pair.
WINDOW defaults to selected one and SIZE to half its size.
If optional third arg HOR-FLAG is non-nil, split side by side
and put SIZE columns in the first of the pair."
  (let ((parent (popper-parent)))
    (if (eq parent (selected-window))
	(let* ((pop-size (window-height
			  (get-buffer-window (popper-output-buffer))))
	       (size (if size (+ size pop-size))))
	  (popper-bury-output)
	  (funcall popper-split-window window size hor-flag)
	  (popper-show-output nil pop-size))
	(funcall popper-split-window window size hor-flag))))

;;; ***Redefine pop-to-buffer to skip output windows***
(defvar popper-pop-to-buffer (symbol-function 'pop-to-buffer)
  "Original pop to buffer function.")
(defun pop-to-buffer (buffer &optional other-window)
  "Select buffer BUFFER in some window, preferably a different one.
If  pop-up-windows  is non-nil, windows can be split to do this.
If second arg  OTHER-WINDOW is non-nil, insist on finding another
window even if BUFFER is already visible in the selected window."
  (let ((parent (popper-parent))
	new-window)
  (if parent
      (progn
	(popper-bury-output)
	(funcall popper-pop-to-buffer buffer other-window)
        (setq new-window (selected-window))
        (select-window parent)
        (popper-show-output)
        (if (not (eq parent new-window))
	    (select-window new-window)))
      (funcall popper-pop-to-buffer buffer other-window))))

;;;
(defun popper-other-window (arg)
  "Select the arg'th other window skipping the popper output window.
This function will skip over windows that contain buffers in
popper-buffers-to-skip."
  (interactive "p")
  (other-window arg)
  (if (popper-mem (buffer-name (current-buffer)) popper-buffers-to-skip)
      (other-window 1)))
(define-key ctl-x-map "o" 'popper-other-window)

;;;
(defun popper-show (buffer)
  "Function to display BUFFER in a popper window if it is in
popper-pop-buffers or popper-pop-buffers is T."
  (if (or (eq popper-pop-buffers t)
	  (popper-mem (buffer-name buffer) popper-pop-buffers))
      (popper-show-output buffer)
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
      (define-key global-map "\C-zv" 'popper-scroll-output)
      (define-key global-map "\C-zg" 'popper-grow-output)))

(provide 'popper)