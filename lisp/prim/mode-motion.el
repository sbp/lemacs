;; Mode-specific mouse-highlighting of text.
;; Copyright (C) 1992 Free Software Foundation, Inc.

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

(defvar mode-motion-hook nil
  "Function or functions which are called whenever the mouse moves.
You should normally use this rather than `mouse-motion-handler', which 
does some additional window-system-dependent things.  This hook is local
to every buffer, and should normally be set up by major-modes which want
to use special highlighting.  Every time the mouse moves over a window,
the mode-motion-hook of the buffer of that window is run.")

(make-variable-buffer-local 'mode-motion-hook)


(defvar mode-motion-extent nil)
(make-variable-buffer-local 'mode-motion-extent)


(defun mode-motion-highlight-internal (event backward forward)
  (let* ((window (event-window event))
	 (screen (if window (window-screen window) (selected-screen)))
	 (buffer (and window (window-buffer window)))
	 (point (and buffer (event-point event))))
    (if point
	(save-excursion
	  (set-buffer buffer)
	  (goto-char point)
	  (condition-case () (funcall backward) (error nil))
	  (setq point (point))
	  (condition-case () (funcall forward) (error nil))
	  (if (and mode-motion-extent (extent-buffer mode-motion-extent))
	      (set-extent-endpoints mode-motion-extent point (point))
	    (setq mode-motion-extent (make-extent point (point)))
	    (set-extent-attribute mode-motion-extent 'highlight))))))


(defun mode-motion-highlight-line (event)
  "For use as the value of `mode-motion-hook' -- highlight line under mouse."
  (mode-motion-highlight-internal event 'beginning-of-line 'end-of-line))

(defun mode-motion-highlight-word (event)
  "For use as the value of `mode-motion-hook' -- highlight word under mouse."
  (mode-motion-highlight-internal
   event
   (function (lambda () (mouse-track-beginning-of-word nil)))
   (function (lambda () (mouse-track-end-of-word nil)))))

(defun mode-motion-highlight-symbol (event)
  "For use as the value of `mode-motion-hook' -- highlight symbol under mouse."
  (mode-motion-highlight-internal
   event
   (function (lambda () (mouse-track-beginning-of-word t)))
   (function (lambda () (mouse-track-end-of-word t)))))

(defun mode-motion-highlight-sexp (event)
  "For use as the value of `mode-motion-hook' -- highlight form under mouse."
  (mode-motion-highlight-internal
   event
   (function (lambda ()
	       (if (= (char-syntax (following-char)) ?\()
		   nil
		 (goto-char (scan-sexps (point) -1)))))
   (function (lambda ()
	       (if (= (char-syntax (following-char)) ?\))
		   (forward-char 1))
	       (goto-char (scan-sexps (point) 1))))))


;;; Minibuffer hackery

(defun minibuf-mouse-tracker (event)
  ;; Used as the mode-motion-hook of the minibuffer window, which is the
  ;; value of `mouse-grabbed-buffer' while the minibuffer is active.  If
  ;; the word under the mouse is a valid minibuffer completion, then it
  ;; is highlighted.
  ;;
  ;; We do some special voodoo when we're reading a pathname, because
  ;; the way filename completion works is funny.  Possibly there's some
  ;; more general way this could be dealt with...
  ;;
  (let ((filename-kludge-p (eq minibuffer-completion-table
			       'read-file-name-internal)))
    (mode-motion-highlight-internal
     event
     (function (lambda () (mouse-track-beginning-of-word
			   (if filename-kludge-p 'nonwhite t))))
     (function (lambda ()
		 (let ((p (point)) string)
		   (mouse-track-end-of-word (if filename-kludge-p 'nonwhite t))
		   (if (or (= p (point)) (null minibuffer-completion-table))
		       (goto-char p)
		     (setq string (buffer-substring p (point)))
		     (if filename-kludge-p
			 (setq string (minibuf-select-kludge-filename string)))
		     (or (try-completion string minibuffer-completion-table
					 minibuffer-completion-predicate)
			 (goto-char p)))))))))


(defun minibuf-select-kludge-filename (string)
  (save-excursion
    (set-buffer mouse-grabbed-buffer) ; the minibuf
    (concat (file-name-directory (buffer-string)) string)))


(defun minibuf-select-highlighted-completion (event)
  "Select the highlighted text under the mouse as a minibuffer response.
When the minibuffer is being used to prompt the user for a completion,
any valid completions which are visible on the screen will highlight
when the mouse moves over them.  Clicking \\<minibuffer-local-map>\
\\[minibuf-select-highlighted-completion] will select the
highlighted completion under the mouse."
  (interactive "e")
  (save-excursion
    (minibuf-mouse-tracker event) ; make sure we're sync'd
    (set-buffer (window-buffer (event-window event)))
    (if (or (null mode-motion-extent)
	    (= (extent-start-position mode-motion-extent)
	       (extent-end-position mode-motion-extent)))
	(if minibuffer-completion-table
	    (error
	     "Highlighted words are valid completions. You may select one.")
	  (error "no completions")))
    (let ((completion
	   (buffer-substring (extent-start-position mode-motion-extent)
			     (extent-end-position mode-motion-extent))))
      (if (eq minibuffer-completion-table 'read-file-name-internal)
	  (setq completion (minibuf-select-kludge-filename completion)))
      (set-buffer mouse-grabbed-buffer)
      (erase-buffer)
      (insert completion))
    (throw 'exit nil)))

(define-key minibuffer-local-map 'button2
  'minibuf-select-highlighted-completion)


(provide 'mode-motion)
