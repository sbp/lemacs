;; Mouse support for X window system.
;; Copyright (C) 1985, 1987, 1988 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'x-mouse)
(require 'mouse)

(define-key global-map 'button2 'x-set-point-and-insert-selection)
(define-key global-map '(control button2) 'x-mouse-kill)

(defun x-mouse-kill (event)
  "Kill the text between the point and mouse and copy it to the clipboard and
to the cut buffer"
  (interactive "@e")
  (let ((old-point (point)))
    (mouse-set-point event)
    (let ((s (buffer-substring old-point (point))))
      (x-own-clipboard s)
      (x-store-cutbuffer s))
    (kill-region old-point (point))))

(defun x-set-point-and-insert-selection (event)
  "Sets point where clicked and insert the primary selection or the cut buffer"
  (interactive "e")
  (let ((text (or (condition-case () (x-get-selection) (error ()))
		  (x-get-cutbuffer))))
    (if (null text)
	(error "No selection or cut buffer available")
      (mouse-set-point event)
      (insert text))))

(defun mouse-track-and-copy-to-cutbuffer (event)
  "Makes a selection like `mouse-track', but also copies it to the cutbuffer."
  (interactive "e")
  (mouse-track event)
  (and primary-selection-extent
       (save-excursion
	 (set-buffer (extent-buffer primary-selection-extent))
	 (x-store-cutbuffer
	  (buffer-substring (extent-start-position primary-selection-extent)
			    (extent-end-position primary-selection-extent))))))

(defun x-insert-selection ()
  "Insert the current selection into buffer at point."
  (interactive)
  (let ((text (x-get-selection)))
    (if text
	(insert text))))


;;; Pointer shape.
;;; This code doesn't allow the mouse cursor and mouse color to be per-screen,
;;; but that wouldn't be hard to do.

(defvar x-pointer-shape nil
  "*The shape of the mouse-pointer when over text.

This string may be any of the standard cursor names from appendix B 
of the Xlib manual (also known as the file <X11/cursorfont.h>) minus 
the XC_ prefix, or it may be a font name and glyph index of the form 
\"FONT fontname index [[font] index]\", or it may be the name of a
bitmap file acceptable to XmuLocateBitmapFile().  If it is a bitmap
file, and if a bitmap file whose name is the name of the cursor with
\"msk\" exists, then it is used as the mask.  For example, a pair of
files may be named \"cursor.xbm\" and \"cursor.xbmmsk\".")

(defvar x-nontext-pointer-shape nil
  "*The shape of the mouse-pointer when over a buffer, but not over text.  
If this is nil, then `x-pointer-shape' is used.")

(defvar x-mode-pointer-shape nil
  "*The shape of the mouse-pointer when over the modeline.
If this is nil, then either `x-nontext-pointer-shape' or `x-pointer-shape'
will be used.")

(defvar x-pointer-foreground-color nil
  "*The foreground color of the mouse pointer.")

(defvar x-pointer-background-color nil
  "*The background color of the mouse pointer.")

(defun x-track-pointer (event)
  (let* ((window (event-window event))
	 (screen (if window (window-screen window) (selected-screen)))
	 (buffer (and window (window-buffer window)))
	 (point (and buffer (event-point event)))
	 (extent (and point (extent-at point buffer 'highlight))))
    (x-set-screen-pointer
     screen
     (cond (point x-pointer-shape)
	   (buffer (or x-nontext-pointer-shape x-pointer-shape))
	   (t (or x-mode-pointer-shape x-nontext-pointer-shape
		  x-pointer-shape)))
     x-pointer-foreground-color
     x-pointer-background-color)
    (if extent
	(highlight-extent extent t)
      (highlight-extent nil nil))))


(defvar x-pointers-initialized nil)

(defun x-initialize-pointer-shape (screen)
  "Initializes the mouse-pointers of the given screen from the resource
database."
  (if x-pointers-initialized  ; only do it when the first screen is created
      nil
    (setq x-pointer-shape
	  (or (x-get-resource "textPointer" "Cursor" 'string screen)
	      "xterm"))
    (setq x-nontext-pointer-shape
	  (or (x-get-resource "spacePointer" "Cursor" 'string screen)
	      "crosshair"))
    (setq x-mode-pointer-shape
	  (or (x-get-resource "modeLinePointer" "Cursor" 'string screen)
	      "sb_v_double_arrow"))
    (setq x-gc-pointer-shape
	  (or (x-get-resource "gcPointer" "Cursor" 'string screen)
	      "watch"))
    (setq x-pointer-foreground-color
	  (x-get-resource "pointerColor" "Foreground" 'string screen))
    (setq x-pointer-background-color
	  (x-get-resource "pointerBackground" "Background" 'string screen))
    (setq x-pointers-initialized t)))
