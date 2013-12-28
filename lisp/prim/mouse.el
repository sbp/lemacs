;; Mouse support that is independent of window systems.
;; Copyright (C) 1988, 1992 Free Software Foundation, Inc.

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

(provide 'mouse)

(require 'mode-motion)

(global-set-key 'button1 'mouse-track)
(global-set-key '(shift button1) 'mouse-track-adjust)
(global-set-key '(control button1) 'mouse-track-insert)
(global-set-key '(control shift button1) 'mouse-track-delete-and-insert)


(defun mouse-select ()
  "Select Emacs window the mouse is on."
  (interactive "@"))

(defun mouse-delete-window ()
  "Delete the Emacs window the mouse is on."
  (interactive "@")
  (delete-window))

(defun mouse-keep-one-window ()
  "Select Emacs window mouse is on, then kill all other Emacs windows."
  (interactive "@")
  (delete-other-windows))

(defun mouse-select-and-split ()
  "Select Emacs window mouse is on, then split it vertically in half."
  (interactive "@")
  (split-window-vertically nil))

(defun mouse-set-point (event)
  "Select Emacs window mouse is on, and move point to mouse position."
  (interactive "@e")
  (let ((window (event-window event))
	(pos (event-point event)))
    (or window (error "not in a window"))
    (select-window window)
    (if (and pos (> pos 0))
	(goto-char pos)
      (move-to-window-line (- (event-y event) (nth 1 (window-edges window))))
      (end-of-line))))

(defun mouse-eval-last-sexpr (event)
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (eval-last-sexp nil)))

(defun mouse-line-length (event)
  "Print the length of the line indicated by the pointer."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (message "Line length: %d"
	     (- (progn (end-of-line) (point))
		(progn (beginning-of-line) (point)))))
  (sleep-for 1))

(defun mouse-set-mark (event)
  "Select Emacs window mouse is on, and set mark at mouse position.
Display cursor at that position for a second."
  (interactive "@e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point event)
	       (push-mark nil t)
	       (sit-for 1))
      (goto-char point-save))))

(defun mouse-scroll (event)
  "Scroll point to the mouse position."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (recenter 0)
    (scroll-right (event-x event))))

(defun mouse-del-char (event)
  "Delete the char pointed to by the mouse."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (delete-char 1 nil)))

(defun mouse-kill-line (event)
  "Kill the line pointed to by the mouse."
  (interactive "@e")
  (save-excursion
    (mouse-set-point event)
    (kill-line nil)))


(defun narrow-window-to-region (m n)
  "Narrow window to region between point and last mark"
  (interactive "r")
  (save-excursion
    (save-restriction
      (if (eq (selected-window) (next-window))
	  (split-window))
      (goto-char m)
      (recenter 0)
      (if (eq (selected-window)
	      (if (zerop (minibuffer-depth))
		  (next-window)))
	  ()
	(shrink-window (- (- (window-height) (count-lines m n)) 1))))))

(defun mouse-window-to-region (event)
  "Narrow window to region between cursor and mouse pointer."
  (interactive "@e")
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point event)
	       (push-mark nil t)
	       (sit-for 1))
      (goto-char point-save)
      (narrow-window-to-region (region-beginning) (region-end)))))

(defun mouse-ignore ()
  "Don't do anything."
  (interactive))


;;
;; Commands for the scroll bar.
;;

;; Vertical bar

(defun mouse-scroll-down (nlines)
  (interactive "@p")
  (scroll-down nlines))

(defun mouse-scroll-up (nlines)
  (interactive "@p")
  (scroll-up nlines))

(defun mouse-scroll-down-full ()
  (interactive "@")
  (scroll-down nil))

(defun mouse-scroll-up-full ()
  (interactive "@")
  (scroll-up nil))

(defun mouse-scroll-move-cursor (nlines)
  (interactive "@p")
  (move-to-window-line nlines))

(defun mouse-scroll-absolute (event)
  (interactive "@e")
  (let* ((position (event-x event))
	 (length (event-y event))
	 (size (buffer-size))
	 (scale-factor (max 1 (/ 8000000 size)))
	 (newpos (* (/ (* (/ size scale-factor) position) length)
		    scale-factor)))
    (goto-char newpos)
    (recenter '(4))))

;; These scroll while the invoking button is depressed.

(defvar scrolled-lines 0)
(defvar scroll-speed 1)

(defun incr-scroll-down (event)
  (interactive "@e")
  (setq scrolled-lines 0)
  (incremental-scroll scroll-speed))

(defun incr-scroll-up (event)
  (interactive "@e")
  (setq scrolled-lines 0)
  (incremental-scroll (- scroll-speed)))

(defun incremental-scroll (n)
  (let ((event (allocate-event))
	(down t))
    (while down
      (sit-for mouse-track-scroll-delay)
      (cond ((input-pending-p)
	     (next-event event)
	     (if (or (button-press-event-p event)
		     (button-release-event-p event))
		 (setq down nil))
	     (dispatch-event event)))
      (setq scrolled-lines (1+ (* scroll-speed scrolled-lines)))
      (scroll-down n))))

(defun incr-scroll-stop (event)
  (interactive "@e")
  (setq scrolled-lines 0)
  (sleep-for 1))


(defun mouse-scroll-left (ncolumns)
  (interactive "@p")
  (scroll-left ncolumns))

(defun mouse-scroll-right (ncolumns)
  (interactive "@p")
  (scroll-right ncolumns))

(defun mouse-scroll-left-full ()
  (interactive "@")
  (scroll-left nil))

(defun mouse-scroll-right-full ()
  (interactive "@")
  (scroll-right nil))

(defun mouse-scroll-move-cursor-horizontally (ncolumns)
  (interactive "@p")
  (move-to-column ncolumns))

(defun mouse-scroll-absolute-horizontally (event)
  (interactive "@e")
  (let* ((position (event-x event))
	 (length (event-y event)))
  (set-window-hscroll (selected-window) 33)))



;;; mouse/selection tracking

(defvar mouse-track-up-time 0)
(defvar mouse-track-up-x 0)
(defvar mouse-track-up-y 0)
(defvar mouse-track-type nil)
(defvar mouse-track-multiclick-time 400)
(defvar mouse-track-timeout-id nil)
(defvar mouse-track-scroll-delay
  (if (featurep 'lisp-float-type) 
      ;; so that the .elc file can load in an emacs without LISP_FLOAT_TYPE
      (car (read-from-string "0.3"))
    1))

(defun mouse-track-set-point-in-window (event window)
  (if (eq (event-window event) window)
      (let ((point (event-point event)))
	(if point
	    (goto-char point)
	  (move-to-window-line (- (event-y event)
				  (nth 1 (window-edges window))))
	  (if (> (event-x-pixel event)
		 (or (cdr (assoc 'internal-border-width x-screen-defaults)) 5))
	      (end-of-line)))
	t)))

(defun mouse-track-scroll-and-set-point (event window)
  (let ((edges (window-edges window))
	(row (/ (event-y-pixel event)
		(/ (x-pixel-height (selected-screen)) (screen-height)))))
    (cond ((<= row (nth 1 edges))
	   (let ((delta (- (nth 1 edges) row)))
	     (condition-case () (scroll-down delta) (error))
	     (goto-char (window-start))))
	  ((>= (point) (point-max)))
	  ((>= row (1- (nth 3 edges)))
	   (let ((delta  (- (+ row 2) (nth 3 edges))))
	     (condition-case () (scroll-up delta) (error))
	     (goto-char (window-end))
	     (vertical-motion delta)
	     (backward-char 1))))))

(defun mouse-track-set-point-and-timeout (event window)
  (if (mouse-track-set-point-in-window event window)
      nil
    (or mouse-track-timeout-id ; no more than one timeout at a time
	(setq mouse-track-timeout-id
	      (add-timeout mouse-track-scroll-delay
			   'mouse-track-scroll-undefined
			   (copy-event event))))
    (mouse-track-scroll-and-set-point event window)))

(defun mouse-track-cleanup-timeout ()
  (if mouse-track-timeout-id
      (progn
	(disable-timeout mouse-track-timeout-id)
	(setq mouse-track-timeout-id nil))))

(defsubst mouse-track-beginning-of-word (symbolp)
  (let ((word-constituent (cond ((eq symbolp t) "\\w\\|\\s_\\|\\s'")
				((null symbolp) "\\w")
				(t "[^ \t\n]")))
	(white-space "[ \t]"))
    (cond ((looking-at word-constituent)
	   (backward-char)
	   (while (looking-at word-constituent)
	     (backward-char))
	   (forward-char))
	  ((looking-at white-space)
	   (backward-char)
	   (while (looking-at white-space)
	     (backward-char))
	   (forward-char)))))

(defun mouse-track-end-of-word (symbolp)
  (let ((word-constituent (cond ((eq symbolp t) "\\w\\|\\s_\\|\\s'")
				((null symbolp) "\\w")
				(t "[^ \t\n]")))
	(white-space "[ \t]"))
    (cond ((looking-at word-constituent) ; word or symbol constituent
	   (while (looking-at word-constituent)
	     (forward-char)))
	  ((looking-at white-space) ; word or symbol constituent
	   (while (looking-at white-space)
	     (forward-char))))))

(defun mouse-track-normalize-point (type forwardp)
  (cond ((eq type 'word)
	 ;; trap the beginning and end of buffer errors
	 (condition-case ()
	     (if forwardp
		 (mouse-track-end-of-word t)
	       (mouse-track-beginning-of-word t))
	   (error ())))
	((eq type 'line)
	 (if forwardp (end-of-line) (beginning-of-line)))))

(defun mouse-track-next-move (min-anchor max-anchor extent)
  (let ((anchor (if (<= (point) min-anchor) max-anchor min-anchor)))
    (mouse-track-normalize-point mouse-track-type (> (point) anchor))
    (if (<= anchor (point))
	(update-extent extent anchor (point))
      (update-extent extent (point) anchor))))

(defun mouse-track-has-selection-p (buffer)
  (and (or (not (eq window-system 'x))
	   (x-selection-owner-p))
       (extentp primary-selection-extent)
       (eq buffer (extent-buffer primary-selection-extent))))

(defun mouse-track-anchor (adjust previous-point)
  (if adjust
      (if (mouse-track-has-selection-p (current-buffer))
	  (let ((start (extent-start-position primary-selection-extent))
		(end (extent-end-position primary-selection-extent)))
	    (cond ((< (point) start) end)
		  ((> (point) end) start)
		  ((> (- (point) start) (- end (point))) start)
		  (t end)))
	previous-point)
    (point)))

(defun mouse-track-next-type (type)
  (cond ((null type) 'char)
	((eq type 'char) 'word)
	((eq type 'word) 'line)
	((eq type 'line) 'char)))

(defun mouse-track-select (event adjust face)
  (or (button-press-event-p event)
      (error "%s must be invoked by a mouse-press" this-command))
  (let* ((window (event-window event))
	 (extent (set-extent-face (make-extent 1 1 (window-buffer window))
				  face))
	 (mouse-down t)
	 min-anchor max-anchor result previous-point)
    ;;
    ;; process double and triple clicks
    (cond ((and (< (- (event-timestamp event) mouse-track-up-time)
		   mouse-track-multiclick-time)
		(= (event-x event) mouse-track-up-x)
		(= (event-y event) mouse-track-up-y))
	   (setq mouse-track-type (mouse-track-next-type mouse-track-type)))
	  ((not adjust)
	   (setq mouse-track-type 'char)))
    (select-window window)
    (setq previous-point (point))

    (mouse-track-set-point-and-timeout event window)
    ;;
    ;; adjust point to a word or line boundary if appropriate
    (let ((anchor (mouse-track-anchor adjust previous-point)))
      (setq min-anchor
	    (save-excursion (goto-char anchor)
			    (mouse-track-normalize-point mouse-track-type nil)
			    (point)))
      (setq max-anchor
	    (save-excursion (goto-char anchor)
			    (mouse-track-normalize-point mouse-track-type t)
			    (point))))
    ;;
    ;; remove the existing selection to unclutter the display
    (cond (zmacs-regions
	   (zmacs-deactivate-region))
	  ((eq window-system 'x)
	   (x-disown-selection)))

    (unwind-protect
	(progn
	  (while mouse-down
	    (mouse-track-next-move min-anchor max-anchor extent)
	    (next-event event)
	    (mouse-track-cleanup-timeout)
	    (cond ((motion-event-p event)
		   (mouse-track-set-point-and-timeout event window))
		  ((and (timeout-event-p event)
			(eq (event-function event)
			    'mouse-track-scroll-undefined))
		   (mouse-track-set-point-and-timeout (event-object event)
						      window))
		  ((button-release-event-p event)
		   (setq mouse-track-up-time (event-timestamp event))
		   (setq mouse-track-up-x (event-x event))
		   (setq mouse-track-up-y (event-y event))
		   (mouse-track-set-point-in-window event window)
		   (mouse-track-next-move min-anchor max-anchor extent)
		   (setq mouse-down nil))
		  ((key-press-event-p event)
		   (error "Selection aborted"))
		  (t
		   (dispatch-event event))))
	  (setq result (cons (extent-start-position extent)
			     (extent-end-position extent))))
      ;; protected
      (delete-extent extent)
      (mouse-track-cleanup-timeout))
    result))

(defun mouse-track-maybe-own-selection (pair type)
  (let ((start (car pair))
	(end (cdr pair)))
    (or (= start end) (push-mark (if (= (point) start) end start)))
    (cond (zmacs-regions
	   (if (= start end)
	       nil
	     (zmacs-activate-region)
	     (setq zmacs-region-stays t)))
	  ((eq window-system 'x)
	   (if (= start end)
	       (x-disown-selection type)
	     (x-own-selection (cons (set-marker (make-marker) start)
				    (set-marker (make-marker) end))
			      type))))
    (if (and (eq window-system 'x)
	     (not (= start end)))
	(x-store-cutbuffer (buffer-substring start end)))))


;;; interactive commands

(defun mouse-track (event)
  "Make a selection with the mouse.  This should be bound to a mouse button.
If you click-and-drag, the selection will be set to the region between the
point of the initial click and the point at which you release the button.
These positions need not be ordered.

If you click-and-release without moving the mouse, then the point is moved,
and the selection is disowned (there will be no selection owner.)  The mark
will be set to the previous position of point.

If you double-click, the selection will extend by symbols instead of by
characters.  If you triple-click, the selection will extend by lines.

If you drag the mouse off the top or bottom of the window, you can select
pieces of text which are larger than the visible part of the buffer; the 
buffer will scroll as necessary.

The selected text becomes the current X Selection, and is also copied to the
top of the kill ring.  The point will be left at the position at which you
released the button, and the mark will be left at the initial click position.

See also the `mouse-track-adjust' command, on \\[mouse-track-adjust]."
  (interactive "e")
  (if (eq window-system 'x)
      (x-focus-screen (window-screen (event-window event))))
  (mouse-track-maybe-own-selection
   (mouse-track-select event nil 'primary-selection)
   'PRIMARY))

(defun mouse-track-adjust (event)
  "Extend the existing selection.  This should be bound to a mouse button.
The selection will be enlarged or shrunk so that the point of the mouse
click is one of its endpoints.  This is only really meaningful after the
`mouse-track' command (\\[mouse-track]) has been executed."
  (interactive "e")
  (if (eq window-system 'x)
      (x-focus-screen (window-screen (event-window event))))
  (mouse-track-maybe-own-selection
   (mouse-track-select event t 'primary-selection)
   'PRIMARY))
  
(defun mouse-track-insert (event &optional delete)
  "Make a selection with the mouse and insert it at point.
This is exactly the same as the `mouse-track' command on \\[mouse-track], 
except that point is not moved; the selected text is immediately inserted
after being selected\; and the selection is immediately disowned afterwards."
  (interactive "*e")
  (let ((s (save-excursion
	     (save-window-excursion
	       (let ((pair (mouse-track-select event nil 'primary-selection)))
		 (prog1
		     (buffer-substring (car pair) (cdr pair))
		   (if delete
		       (kill-region (car pair) (cdr pair)))))))))
    (or (equal s "") (insert s))))

(defun mouse-track-delete-and-insert (event)
  "Make a selection with the mouse and insert it at point.
This is exactly the same as the `mouse-track' command on \\[mouse-track], 
except that point is not moved; the selected text is immediately inserted
after being selected\; and the text of the selection is deleted."
  (interactive "*e")
  (mouse-track-insert event t))
