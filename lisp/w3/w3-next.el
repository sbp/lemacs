;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NeXT Emacs Specific Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar w3-NeXT-mousemap (make-mousemap)
  "Mousemap for use on NeXT Emacs
<left> follows link, <shift left> views the URL, <ctrl left> quits")

(define-mouse w3-NeXT-mousemap '(text left) 'w3-follow-mouse)
(define-mouse w3-NeXT-mousemap '(text shift left) 'w3-view-url-at-mouse)
(define-mouse w3-NeXT-mousemap '(text control left) 'w3-quit-from-mouse)

(defun w3-follow-mouse (window x y)
  "Follow the link under the mouse cursor"
  (mouse-move-point window x y)
  (if (w3-zone-at (point)) (w3-follow-link)))

(defun w3-view-url-at-mouse (window x y)
  "View the URL under the mouse cursor"
  (mouse-move-point window x y)
  (if (w3-zone-at (point)) (w3-view-this-url)))

(defun w3-quit-from-mouse (window x y)
  "Invoke w3-quit from mouse"
  (w3-quit))

(provide 'w3-next)
