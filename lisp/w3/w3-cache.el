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
;;; Cacheing for documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-find-in-cache (url)
  "Return the document referenced by URL if it is in the document cache."
  (let ((x (cdr (assoc url w3-document-cache))))
    (set-buffer w3-working-buffer)
    (erase-buffer)
    (insert x)
    (shell-command-on-region (point-min) (point-max) "gunzip -d -c" t)))

(defun w3-store-in-cache (url text)
  "Store the docuemnt in the cache."
  (save-excursion
    (set-buffer (get-buffer-create " *W3TEMPC*"))
    (erase-buffer)
    (insert text)
    (shell-command-on-region (point-min) (point-max) "gzip -c" t)
    (setq text (buffer-string))
    (kill-buffer (current-buffer)))
  (let ((x (length w3-document-cache))
	(y (assoc url w3-document-cache)))
    (cond
     (y (setcdr y text))
     ((< x w3-cache-size)
      (setq w3-document-cache
	    (nconc w3-document-cache (list (cons url text)))))
     (t (setq w3-document-cache
	      (nconc (cdr w3-document-cache) (list (cons url text))))))))
