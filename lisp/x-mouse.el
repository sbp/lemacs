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

(defun track-select-and-also-copy-to-cutbuffer (event)
  "Does a selection and also copies it to the cutbuffer"
  (interactive "e")
  (energize-mouse-track-select event)
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
