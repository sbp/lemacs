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
;;; Normal Emacs Specific Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

(defun w3-create-faces ()
  "Create faces, the dumb emacs 18 way"
  nil)

(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let* ((thezones w3-zones-list))
    (while (and thezones
		(not
		 (equal link
			(car-safe (cdr (car (cdr (cdr (car thezones)))))))))
      (setq thezones (cdr thezones)))
    (if thezones (goto-char (car (car thezones)))
      (message "Link %s was not found." link))))

(defun w3-view-this-url (&optional no-show)
  "View the url for the current link."
  (interactive)
  (let ((zn (w3-zone-at (point))))
    (if zn
	(if (not no-show)
	    (message "%s" (car (cdr (cdr (car (cdr (cdr zn)))))))
	  (car (cdr (cdr (car (cdr (cdr zn)))))))
      (error "Not on a link!"))))

(defun w3-add-zone (start end style data &optional highlight)
  "Add a zone (normal emacs)"
  (if (memq (car data) '(w3 w3form))
      (cond
       ((or (null w3-zones-list)
	    (< start (car (car w3-zones-list))))
	(setq w3-zones-list (cons (list (copy-marker start)
					(copy-marker end)
					data) w3-zones-list)))
       (t
	(let ((zones w3-zones-list))
	  (while (and (cdr zones)
		      (< (car (car (cdr zones))) start))
	    (setq zones (cdr zones)))
	  (setcdr zones (cons (list
			       (copy-marker start)
			       (copy-marker end)
			       data)
			      (cdr zones))))))))

(defun w3-all-zones ()
  "Return all the zones in this buffer."
  w3-zones-list)

(defun w3-zones-list ()
  "Return a list of zones for this buffer"
  w3-zones-list)

(defun w3-zone-at (pt)
  "Return zone (if any) at buffer position PT"
  (let ((zones w3-zones-list))
    (while (and zones
		(not (and
		      (>= (car (cdr (car zones))) pt)
		      (<= (car (car zones)) pt))))
      (setq zones (cdr zones)))
    (if zones (car zones)
      nil)))

(defun w3-delete-zone (zone)
  "Delete zone ZONE in this buffer."
  (let ((tmp w3-zones-list)
	(val '()))
    (while tmp
      (if (not (eq (car tmp) zone))
	  (setq val (append val (list (car tmp)))))
      (setq tmp (cdr tmp)))
    (setq w3-zones-list val)))

(defun w3-zone-end (zone)
  "Return the ending position of zone ZONE"
  (marker-position (car (cdr zone))))

(defun w3-zone-start (zone)
  "Return the starting position of zone ZONE"
  (marker-position (car zone)))

(defun w3-fix-extent-endpoints ()
  "Not done yet"
  (let ((x (w3-all-zones))
        st nd ch st-marker nd-marker)
    (while x
      (setq st-marker (car (car x))
            nd-marker (car (cdr (car x)))
            st (marker-position st-marker)
            nd (marker-position nd-marker))
      (while (memq (char-after (1- nd)) '(9 13 10 32))
        (setq nd (1- nd)
              ch t))
      (if ch (set-marker nd-marker nd))
      (setq ch nil)
      (while (memq (char-after st) '(9 13 10 32))
	(setq st (1+ st)
	      ch t))
      (if ch (set-marker st-marker st))
      (setq x (cdr x)))))

(defun w3-next-zone (zone)
  "Return zone (if any) after ZONE"
  (let ((zones w3-zones-list))
    (while (and zones
		(not (and (equal (car (cdr (car zones)))
				 (car (cdr zone)))
			  (equal (car (car zones))
				 (car zone)))))
      (setq zones (cdr zones)))
    (if (cdr zones)
	(car (cdr zones))
      nil)))

(defun w3-previous-zone (zone)
  "Return zone (if any) before ZONE"
  (let ((zones w3-zones-list)
	(last nil))
    (while (not (eql (car zones) zone))
      (setq last (car zones)
	    zones (cdr zones)))
    (if zones
	last
      nil)))

(defun w3-zone-data (zone)
  "Return the data segment from zone ZONE"
  (car (cdr (cdr zone))))

(defun w3-forward-link ()
  "Go forward 1 link"
  (interactive)
  (let ((zones w3-zones-list))
    (while (and zones
		(<= (car (car zones)) (point)))
      (setq zones (cdr zones)))
    (if zones
	(progn
	  (goto-char (car (car zones)))
	  (while (looking-at "[ \\\t\\\n]+") (forward-char 1)))
      (error "No more links."))))

(defun w3-back-link ()
  "Go back 1 link"
  (interactive)
  (cond
   ((null w3-zones-list)
    (error "No links in this document."))
   ((> (car (car (cdr w3-zones-list))) (point))
    (error "No previous link"))
   (t
    (let ((zones w3-zones-list))
      (while (and (cdr zones)
		  (< (car (cdr (car (cdr zones)))) (point)))
	(setq zones (cdr zones)))
      (goto-char (car (car zones)))
      (while (looking-at "[ \\\t\\\n]+") (forward-char 1))))))
    

(defun w3-follow-link ()
  "Follow a link"
  (interactive)
  (let ((zn (w3-zone-at (point))))
    (if zn
	(if (eq (car (car (cdr (cdr zn)))) 'w3form)
	    (w3-do-form-entry (w3-zone-data zn) zn)
	  (w3-maybe-relative (car (cdr (cdr (w3-zone-data zn))))))
      (message "Not on a link!"))))

(defun w3-complete-link ()
  "Choose a link from this buffer.  This will do a completing-read on all
the links in this buffer."
  (interactive)
  (let ((x (mapcar 'w3-zone-data (w3-all-zones)))
	y z)
    (if (not x) (error "No links in current document."))
    (while x
      (setq y (cons (cons (w3-strip-leading-spaces (nth 3 (car x)))
			  (nth 2 (car x))) y)
	    x (cdr x)))
    (setq z (completing-read "Link: " y nil t))
    (w3-fetch (cdr (assoc z y)))))

(defun w3-setup-version-specifics ()
  "Set up routine for emacs 18/NeXTemacs"
  (cond
   ((fboundp 'define-mouse) (require 'w3-next))
   ((boundp 'MULE) (require 'w3-mule))
   (t (fset 'w3-x-popup-menu 'x-popup-menu))))

(defun w3-store-in-x-clipboard (str)
  "Store string STR in the window systems cut buffer"
  (and window-system (x-store-cut-buffer str)))

(provide 'w3-emacs)
