;; Management of intervals as logical units.
;; Copyright (C) 1991 Free Software Foundation, Inc.

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

(provide 'extents)

(defun extentp (e)
  (and (listp e)
       (vectorp (car (cdr e)))
       (intervalp (aref (car (cdr e)) 0))))

(defun make-extent (from to tag)
  (list tag (make-contiguous-intervals from to)))

(defun delete-extent (e)
  (let* ((s (car e))
	 (i (car (cdr e)))
	 (l (1- (length i))))
    (while (>= l 0)
      (let ((interval (aref i l)))
	(iremprop interval s)
	(if (not (interval-plist interval))
	    (delete-interval interval)))
      (setq l (1- l)))))

(defun extent-set-attributes (e attributes)
  (let* ((i (car (cdr e)))
	 (l (1- (length i))))
    (while (>= l 0)
      (set-interval-attributes (aref i l) attributes)
      (setq l (1- l)))))

(defun extent-put (e prop value)
  (let* ((i (car (cdr e)))
	 (l (1- (length i))))
    (while (>= l 0)
      (interval-put (aref i l) prop value)
      (setq l (1- l)))))

(defun extent-position (e)
  (let ((i (aref 0 (car (cdr e)))))
    (interval-get i 'position)))

(defun extent-length (e)
  (let* ((i (car (cdr e)))
	 (l (1- (length i)))
	 (p1 (interval-get (aref 0 i) 'position))
	 (p2 (+ (interval-get (aref l i) 'position)
		(interval-get (aref l i) 'length))))
    (- p2 p1)))
