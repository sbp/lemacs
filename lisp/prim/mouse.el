;; Mouse support that is independent of window systems.
;; Copyright (C) 1988, 1992, 1993, 1994 Free Software Foundation, Inc.

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

(define-key mode-line-map 'button1 'mouse-drag-modeline)
(define-key mode-line-map 'button3 'mode-line-menu)

(defvar mouse-track-rectangle-p nil
  "*If true, then dragging out a region with the mouse selects rectangles
instead of simple start/end regions.")


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
    (or window (error (gettext "not in a window")))
    (select-window window)
    (if (and pos (> pos 0))
	;; If the event was over a text char, it's easy.
	(goto-char pos)
      ;; Otherwise we have to do a bunch of semi-heuristic crap to figure
      ;; out where the "nearest" buffer position to the click is.
      (let ((ypos (- (event-y event) (nth 1 (window-edges window)))))
	(if (and (< (move-to-window-line ypos) ypos) (> ypos 0))
	    ;; If target line was past end of buffer, go to eol of last line.
	    (end-of-line)
	  ;; Otherwise we need to decide whether the point should be at the
	  ;; beginning of the line, or the end, by determining whether it's
	  ;; off the left side of the window, or the right.
	  (if (> (event-x-pixel event)
		 (+ (buffer-left-margin-pixwidth (window-buffer window))
		    (or (cdr (assoc 'internal-border-width x-screen-defaults))
			5)))
	      (end-of-line)))))))


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
    (message (gettext "Line length: %d")
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
  (let ((down t))
    (while down
      (sit-for mouse-track-scroll-delay)
      (cond ((input-pending-p)
	     (let ((event (next-command-event)))
	       (if (or (button-press-event-p event)
		       (button-release-event-p event))
		   (setq down nil))
	       (dispatch-event event))))
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
  (set-window-hscroll (selected-window) 33))



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
      (purecopy (string-to-number "0.3"))
    1))

(defun mouse-track-set-point-in-window (event window)
  (if (not (eq (event-window event) window))
      nil
    (mouse-set-point event)
    t))

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
    (cond ((bobp) nil)
	  ((looking-at word-constituent)
	   (backward-char)
	   (while (and (not (bobp)) (looking-at word-constituent))
	     (backward-char))
	   (if (or (not (bobp)) (not (looking-at word-constituent)))
	       (forward-char)))
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
	 (if forwardp (end-of-line) (beginning-of-line)))
	((eq type 'buffer)
	 (if forwardp (end-of-buffer) (beginning-of-buffer)))))

(defun mouse-track-next-move (min-anchor max-anchor extent)
  (let ((anchor (if (<= (point) min-anchor) max-anchor min-anchor)))
    (mouse-track-normalize-point mouse-track-type (> (point) anchor))
    (if (consp extent)
	(mouse-track-next-move-rect anchor (point) extent)
      (if (<= anchor (point))
	  (set-extent-endpoints extent anchor (point))
	(set-extent-endpoints extent (point) anchor)))))

(defun mouse-track-next-move-rect (start end extents &optional pad-p)
  (if (< end start)
      (let ((tmp start)) (setq start end end tmp)))
  (cond
   ((= start end)		; never delete the last remaining extent
    (mapcar 'delete-extent (cdr extents))
    (setcdr extents nil)
    (set-extent-endpoints (car extents) start start))
   (t
    (let ((indent-tabs-mode nil)	; if pad-p, don't use tabs
	  (rest extents)
	  left right last p)
      (save-excursion
	(save-restriction
	  (goto-char end)
	  (setq right (current-column))
	  (goto-char start)
	  (setq left (current-column))
	  (if (< right left)
	      (let ((tmp left))
		(setq left right right tmp)
		(setq start (- start (- right left))
		      end (+ end (- right left)))))
	  (beginning-of-line)
	  (narrow-to-region (point) end)
	  (goto-char start)
	  (while (and rest (not (eobp)))
	    (setq p (point))
	    (move-to-column right pad-p)
	    (set-extent-endpoints (car rest) p (point))
	    (if (= 0 (forward-line 1))
		(move-to-column left pad-p))
	    (setq last rest
		  rest (cdr rest)))
	  (cond (rest
		 (mapcar 'delete-extent rest)
		 (setcdr last nil))
		((not (eobp))
		 (while (not (eobp))
		   (setq p (point))
		   (move-to-column right pad-p)
		   (let ((e (make-extent p (point))))
		     (set-extent-face e (extent-face (car extents)))
		     (set-extent-priority e (extent-priority (car extents)))
		     (setcdr last (cons e nil))
		     (setq last (cdr last)))
		   (if (= 0 (forward-line 1))
		       (move-to-column left pad-p))
		   )))))
      ))))


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
	((eq type 'line) 'char)
;;	((eq type 'line) 'buffer)
;;	((eq type 'buffer) 'char)
	))

;; This remembers the last position at which the user clicked, for the
;; benefit of mouse-track-adjust (for example, button1; scroll until the
;; position of the click is off the screen; then Sh-button1 to select the
;; new region.
(defvar mouse-track-previous-point nil)

(defun mouse-track-select (event adjust face)
  (or (button-press-event-p event)
      (error (gettext "%s must be invoked by a mouse-press") this-command))
  (let* ((window (event-window event))
	 (extent (make-extent 1 1 (window-buffer window)))
	 (mouse-down t)
	 min-anchor max-anchor result previous-point)
    ;; Note that the extent used in this function is NOT the extent which
    ;; ends up as the value of primary-selection-extent - this one is used
    ;; just during mouse-dragging.  
    (set-extent-face extent face)
    ;; While the selection is being dragged out, give the selection extent
    ;; slightly higher priority than any mouse-highlighted extent, so that
    ;; the exact endpoints of the selection will be visible while the mouse
    ;; is down.  Normally, the selection and mouse highlighting have the same
    ;; priority, so that conflicts between the two of them are resolved by
    ;; the usual size-and-endpoint-comparison method.
    (set-extent-priority extent (1+ mouse-highlight-priority))
    (if mouse-track-rectangle-p (setq extent (list extent)))
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

    (setq previous-point
	  (if (and adjust
		   (markerp mouse-track-previous-point)
		   (eq (current-buffer)
		       (marker-buffer mouse-track-previous-point)))
	      (marker-position mouse-track-previous-point)
	    (point)))

    (mouse-track-set-point-and-timeout event window)

    (if (not adjust)
	(if (markerp mouse-track-previous-point)
	    (set-marker mouse-track-previous-point (point))
	  (setq mouse-track-previous-point (point-marker))))
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
	    (setq event (next-event event))
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
		   (error (gettext "Selection aborted")))
		  (t
		   (dispatch-event event))))
	  (cond ((consp extent) ; rectangle-p
		 (let ((first (car extent))
		       (last (car (setq extent (nreverse extent)))))
		   (setq result (cons (extent-start-position first)
				      (extent-end-position last)))
		   ;; kludge to fix up region when dragging backwards...
		   (if (and (/= (point) (extent-start-position first))
			    (/= (point) (extent-end-position last))
			    (= (point) (extent-end-position first)))
		       (goto-char (car result)))))
		(t
		 (setq result (cons (extent-start-position extent)
				    (extent-end-position extent)))))
	  ;; Minor kludge: if we're selecting in line-mode, include the
	  ;; final newline.  It's hard to do this in *-normalize-point.
	  (if (eq mouse-track-type 'line)
	      (let ((end-p (= (point) (cdr result))))
		(goto-char (cdr result))
		(if (not (eobp))
		    (setcdr result (1+ (cdr result))))
		(goto-char (if end-p (cdr result) (car result)))))
;;	  ;; Minor kludge sub 2.  If in char mode, and we drag the
;;	  ;; mouse past EOL, include the newline.
;;	  ;;
;;	  ;; Major problem: can't easily distinguish between being
;;	  ;; just past the last char on a line, and well past it,
;;	  ;; to determine whether or not to include it in the region
;;	  ;;
;;	  (if nil ; (eq mouse-track-type 'char)
;;	      (let ((after-end-p (and (not (eobp))
;; 				      (eolp)
;;				      (> (point) (car result)))))
;;		(if after-end-p
;;		    (progn
;;		      (setcdr result (1+ (cdr result)))
;;		      (goto-char (cdr result))))))
	  )
      ;; protected
      (if (consp extent) ; rectangle-p
	  (mapcar 'delete-extent extent)
	(delete-extent extent))
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
and the selection is disowned (there will be no selection owner.)

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
  (or (event-window event) (error (gettext "not in a window")))
  (select-screen (window-screen (event-window event)))
  (let (;(p (point))
	;(b (current-buffer))
	(pair (mouse-track-select event nil 'primary-selection)))
    ;; if no region was selected, but point has changed, but current
    ;; buffer has not, push a mark at the previous point.
    ;; (This has been disabled because it interacts badly with scrollbars -
    ;; the mark tends to get pushed at window-start or window-end instead of
    ;; the previous position of point.  So don't bother.)
;    (if (and (equal (car pair) (cdr pair))
;	     (eq b (current-buffer))
;	     (not (equal p (car pair))))
;	(push-mark p))
    (mouse-track-maybe-own-selection pair 'PRIMARY)))

(defun mouse-track-adjust (event)
  "Extend the existing selection.  This should be bound to a mouse button.
The selection will be enlarged or shrunk so that the point of the mouse
click is one of its endpoints.  This is only really meaningful after the
`mouse-track' command (\\[mouse-track]) has been executed."
  (interactive "e")
  (select-screen (window-screen (event-window event)))
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


;;; Modeline hackery

(defun mouse-drag-modeline (event)
  "Resize the window by dragging the modeline.
This should be bound to a mouse button in `mode-line-map'."
  (interactive "e")
  (or (button-press-event-p event)
      (error (gettext "%s must be invoked by a mouse-press") this-command))
  (or (null (event-window event))
      (error (gettext "not over a modeline")))
  (let (;;(y (event-y event))
	(mouse-down t)
	(window (locate-window-from-coordinates
		 (event-screen event) (list (event-x event) (event-y event))))
	(old-window (selected-window))
	ny delta)
    (if (= (- (screen-height) 1) (nth 3 (window-edges window)))
	(error (gettext "can't drag bottommost modeline")))
    (while mouse-down
      (setq event (next-event event))
;      (and (motion-event-p event)
;	   (message "%S %S %S" event (event-y event) (event-y-pixel event)))
      (cond ((motion-event-p event)
	     ;; window-edges and event-y both count lines starting from 0.
	     ;; But, window-edges actually returns one more than the
	     ;; bottommost row used by a window including its modeline.
	     ;; Thus, we have to normalize the values which is why we
	     ;; add 1 to the return value of event-y.
	     (setq ny (+ 1 (event-y event)))
	     (if (and (= ny 0) (>= (event-y-pixel event) 20)) ;kludgoriffic
		 (setq ny 999))
	     (if (eq nil (event-window event))
		 (setq delta 0)
	       (setq delta (- ny (nth 3 (window-edges window)))))
	     ;; cough sputter hack kludge.  It shouldn't be possible
	     ;; to get in here when we are over the minibuffer.  But
	     ;; it is happening and that cause next-vertical-window to
	     ;; return nil which does not lead to window-height returning
	     ;; anything remotely resembling a sensible value.  So catch
	     ;; the situation and die a happy death.
	     ;;
	     ;; Oh, and the BLAT FOOP error messages suck as well but
	     ;; I don't know what should be there.  This should be
	     ;; looked at again when the new redisplay is done.
	     (if (not (next-vertical-window window))
		 (error (gettext "BLAT FOOP")))
	     (cond ((and (> delta 0)
			 (<= (- (window-height (next-vertical-window window))
				delta)
			     window-min-height))
		    (setq delta (- (window-height
				    (next-vertical-window window))
				   window-min-height))
		    (if (< delta 0) (error (gettext "BLAT"))))
		   ((and (< delta 0)
			 (< (+ (window-height window) delta)
			    window-min-height))
		    (setq delta (- window-min-height
				   (window-height window)))
		    (if (> delta 0) (error (gettext "FOOP")))))
	     (if (= delta 0)
		 nil
	       (select-window window)
	       (enlarge-window delta)
	       (select-window old-window)
	       (sit-for 0)
	       ;;(setq y ny)
	       ))
	    ((button-release-event-p event)
	     (setq mouse-down nil))
	    ((or (button-press-event-p event)
		 (key-press-event-p event))
	     (error ""))
	    (t
	     (dispatch-event event)))
      )))

(defconst mode-line-menu
  '("Window Commands"
    ["Delete Window"		 delete-window			t]
    ["Delete Other Windows"	 delete-other-windows		t]
    ["Split Window"		 split-window-vertically	t]
    ["Split Window Horizontally" split-window-horizontally	t]
    ["Balance Windows"		 balance-windows		t]
    ))

(defun mode-line-menu (event)
  (interactive "e")
  (let* ((window (locate-window-from-coordinates
		  (event-screen event)
		  (list (event-x event) (event-y event)))))
    ;; kludge; don't select the minibuffer window...
    (if (eq window (minibuffer-window (event-screen event)))
	(setq window (previous-window window)))
    (select-window window)
    (let ((popup-menu-titles t))
      (popup-menu (cons (format (gettext "Window Commands for %S:")
				(buffer-name (window-buffer window)))
			(cdr mode-line-menu))))))
