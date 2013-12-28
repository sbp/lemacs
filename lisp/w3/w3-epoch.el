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
;;; Epoch Enhancements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

(defun w3-make-face (new-name def-fn def-fg def-bg def-ulp)
  "Create a style named NAME, and read in some standard resources.  Default
to font DEF-FN, foreground DEF-FG, background DEF-FG, and underlining to
DEF-ULP"
  (let* ((face (make-style))
	 (name (symbol-name new-name))
	 (fn   (or (epoch::get-default (concat "emacs*" name ".attributeFont"))
		   def-fn))
	 (fg   (or (epoch::get-default
		    (concat "emacs*" name ".attributeForeground")) def-fg))
	 (bg   (or (epoch::get-default
		    (concat "emacs*" name ".attributeBackground")) def-bg))
	 (ulp  (or (epoch::get-default 
		    (concat "emacs*" name ".attributeUnderline")) def-ulp)))
    (if fn
	(condition-case ()
	    (set-style-font face fn)
	  (error (message "Font `%s' not found for face `%s'" fn name))))
    (if fg
	(condition-case ()
	    (set-style-foreground face fg)
	  (error (message "Color `%s' not allocated for face `%s'" fg name))))
    (if bg
	(condition-case ()
	    (set-style-background face bg)
	  (error (message "Color `%s' not allocated for face `%s'" bg name))))
    (if ulp (set-style-underline face "white"))
    (set-variable new-name face)))

(defun w3-create-faces ()
  "Create the faces, the Epoch way"
  (w3-make-face 'w3-tt-style "6x13" nil nil nil)
  (w3-make-face 'w3-bold-style nil "red" nil nil)
  (w3-make-face 'w3-italic-style nil "green" nil nil)
  (w3-make-face 'w3-underline-style nil nil nil t)
  (w3-make-face 'w3-header-style nil "red" nil  t)  
  (w3-make-face 'w3-node-style nil "yellow" nil t)
  (w3-make-face 'w3-address-style nil "green" nil  nil)
  (w3-make-face 'w3-superscript-style nil "pink" nil nil)
  (w3-make-face 'w3-subscript-style nil "pink" nil t)
  (w3-make-face 'w3-strikethru-style nil "red" nil t)
  (w3-make-face 'w3-default-style nil nil nil nil))

(defvar w3-mouse-map (create-mouse-map))
(define-key w3-mode-map "i" 'w3-load-delayed-images)

(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let* ((thezones (epoch::zones-in-region (point-min) (point-max))))
    (while (and thezones
		(not (equal link
			    (car-safe
			     (cdr (epoch::zone-data (car thezones)))))))
      (setq thezones (cdr thezones)))
    (if thezones
	(goto-char (zone-start (car thezones)))
      (message "Link %s was not found." link))))

(fset 'w3-delete-zone 'epoch::delete-zone)
(fset 'w3-zone-data 'epoch::zone-data)
(fset 'w3-zone-start 'epoch::zone-start)
(fset 'w3-zone-end 'epoch::zone-end)
(fset 'w3-zone-eq 'eq)

(defun w3-all-zones ()
  "Return all the zones in this buffer."
  (epoch::zones-in-region (point-min) (point-max)))

(defun w3-forward-link ()
  "Go forward 1 link"
  (interactive)
  (let* ((thezones (epoch::zones-in-region 
		    (if (epoch::zone-at (point))
			(1+ (epoch::zone-end (epoch::zone-at (point))))
		      (point)) (point-max))))
    (while (and thezones
		(not (and
		      (or 
		       (eq (car (epoch::zone-data (car thezones))) 'w3)
		       (eq (car (epoch::zone-data (car thezones))) 'w3form)))))
      (setq thezones (cdr thezones)))
    (if (car thezones)
	(goto-char (epoch::zone-start (car thezones))))))

(defun w3-back-link ()
  "Go back 1 link"
  (interactive)
  (let* ((thezones (epoch::zones-in-region
		    (point-min)
		    (if (epoch::zone-at (point))
			(1- (epoch::zone-start (epoch::zone-at (point))))
		      (point)))))
    (while (and thezones
		(and
		 (equal (car-safe (epoch::zone-data (car (last thezones))))
			'w3)
		 (memq (cdr-safe (epoch::zone-data (car (last thezones))))
		       '(style address header))))
      (setq thezones (butlast thezones 1)))
    (if (car thezones)
	(goto-char (epoch::zone-start (car (last thezones)))))))

(defun w3-follow-mouse (mouse-data)
  "Follow the link under the mouse cursor"
  (interactive)
  (mouse::set-point mouse-data)
  (w3-follow-link))

(defun w3-fix-extent-endpoints ()
  "Make sure no extents have whitespace at the end of them."
  (let ((x (epoch::zones-in-region (point-min) (point-max))))
    (while x
      (let ((st (epoch::zone-start (car x)))
	    (nd (epoch::zone-end (car x))))
	(while (memq (char-after (1- nd)) '(9 13 10 32 ?1 ?2 ?3 ?4 ?5 ?6 ?7
					      ?8 ?9 ?0 ?. ?*))
	  (setq nd (1- nd)))
	(while (memq (char-after st) '(9 13 10 32 ?1 ?2 ?3 ?4 ?5 ?6 ?7
					 ?8 ?9 ?0 ?. ?*))
	  (setq st (1+ st)))
	(epoch::move-zone (car x) st nd))
      (setq x (cdr x)))))

(defun w3-follow-link ()
  "Attempt to follow link under cursor"
  (interactive)
  (let ((x (zones-in-region (point) (if (= (point) (point-max)) (point-max)
				      (1+ (point))) t))
	(data nil))
    (if x
	(progn
	  (while x
	    (setq data (epoch::zone-data (car x)))
	    (if (eq (car-safe data) 'w3form)
		(w3-do-form-entry data (car x))
	      (if (and (equal (car-safe data) 'w3)
		       (not (memq (car (cdr data))
				  '(address header style pic))))
		  (w3-maybe-relative (car (cdr (cdr data))))))
	    (setq x (cdr x))))
      (progn
	(setq x (zone-at (point)))
	(if x
	    (progn
	      (setq data (zone-data x))
	      (if (eq (car-safe data) 'w3form) (w3-do-form-entry data x)
		(if (and (equal (car-safe data) 'w3)
			 (not (memq (car (cdr data))
				    '(address header style pic))))
		    (w3-maybe-relative (car (cdr (cdr data)))))))
	  (message "Not on a link!"))))))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (epoch)"
  (let ((zone (add-zone start end style)))
    (epoch::set-zone-data zone data)))

(define-mouse w3-mouse-map mouse-middle mouse-down 'w3-follow-mouse)

(defun w3-view-this-url (&optional no-show)
  "View the URL of the link (if any) under point"
  (interactive)
  (if (epoch::zone-at (point))
      (let ((data (epoch::zone-data (epoch::zone-at (point)))))
	(if (and (equal (car data) 'w3)
		 (not (memq (cdr data) '(address style header))))
	    (if (not no-show)
		(message "%s" (car (cdr (cdr data))))
	      (car (cdr (cdr data))))
	  (error "Not on a link!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-insert-graphic (name pt align alt)
  "Insert the graphic pointed to by the URL NAME, at buffer position POINT,
with alignment specified by ALIGN (one of 'center 'top or 'bottom).  If the
conversion of the picture fails for any reason, use ALT as the alternative
text.  If the reading of the pixmap is successful, the url and a pointer to
the pixmap are stored in w3-graphics-list for possible re-use later.

  If I can ever figure out how to get the color _NAME_ from epoch, I will
change this to grok bitmaps/pixmaps and change their background color to
that of the emacs screen.  Will look better that way.

  If epoch was not compiled with graphics zone support, this function
does nothing."
  (goto-char pt)
  (if (fboundp 'epoch::read-pixmap-file) (save-excursion
    (let ((bit nil)
	  (converter nil)
	  (fname (w3-generate-unique-filename)))
      (if (string-match w3-nonrelative-link name)
	  nil
	(setq name (w3-parse-relative-link name)))
      (let ((w3-working-buffer " *W3GRAPH*"))
	(if (assoc name w3-graphics-list)
	    (progn
	      (message "Reusing image...")
	      (setq bit (cdr (assoc name w3-graphics-list))))
	  (progn
	    (w3-retrieve name)		; this should leave us in *W3GRAPH*
	    (if (not w3-current-mime-type)
		(setq w3-current-mime-type
		      (w3-extension-to-mime
		       (w3-file-extension w3-current-file))))
	    (setq converter
		  (assoc w3-current-mime-type w3-graphic-converter-alist))
	    (if (not converter)
		(message "Cannot convert %s to www/present!" w3-current-mime-type)
	      (progn
		(message "Converting image %s (%s)..." w3-current-file
			 w3-current-mime-type)
		(shell-command-on-region
		 (point-min) (point-max)
		 (format (concat (cdr converter) " %s > %s")
			 (if w3-max-colors
			     (format "ppmquant %d |" w3-max-colors) "")
			 ""
;			 (if (equal "image/xbm" w3-current-mime-type)
;			     "| sed 's/c white/c grey80/g'" "")
			 fname) t)
		(message "Reading image %s..." w3-current-file)
		(setq bit (epoch::read-pixmap-file fname))
		(delete-file fname))))))
      (set-buffer w3-working-buffer)
      (if bit
	  (progn
	    (add-graphic-zone bit pt (1+ pt)
			      (cond
			       ((eq align 'top) 0)
			       ((eq align 'center) 50)
			       ((eq align 'bottom) 100)
			       (t 50))
			      '(w3 pic) (current-buffer))
	    (setq w3-graphics-list
		  (cons (cons name bit) w3-graphics-list)))
	(progn
	  (message "Conversion failed, probably because of colormap.")
	  (goto-char pt)
	  (delete-region pt (1+ pt))
	  (insert alt)))))))

(defun w3-create-hrule ()
  "Create a pixmap that is the width of the current buffer.  This
could use some work - not extremely pretty right now, but it works.

  If epoch was not compiled with graphics zone support, this function
returns nil, causing the function which calls it (w3-fix-horizontal-rules)
to draw a line with dashes."
  (if (not (fboundp 'read-pixmap-file)) nil
  (let ((width (- (window-pixwidth) 10))
	x bit f)
    (setq x (concat "/* XPM */\nstatis char * scratch [] = {\n"
		    (format "\"%d 4 2 1\",\n" width)
		    (format "\"       c %s\",\n" "gray80") 
		    (format "\".      c %s\",\n" "black")
		    (format "\"%s\",\n" (make-string width 32))
		    (format "\"%s\",\n" (make-string width ?.))
		    (format "\"%s\",\n" (make-string width ?.))
		    (format "\"%s\"};\n" (make-string width 32)))
	  f (w3-generate-unique-filename)
	  bit (progn
		(save-excursion
		  (set-buffer (get-buffer-create " *tmp*"))
		  (erase-buffer)
		  (insert x)
		  (write-region (point-min) (point-max) f nil 5)
		  (kill-buffer " *tmp*")
		  (read-pixmap-file f))))
    bit)))

(defun w3-complete-link ()
  "Choose a link from this buffer."
  (interactive)
  (let ((all (w3-all-zones))
	x y z data)
    (while all
      (setq data (epoch::zone-data (car all)))
      (if (and (memq (car data) '(w3 w3form))
	       (not (symbolp (cdr data))))
	  (setq x (cons data x)))
      (setq all (cdr all)))
    (if (not x) (error "No links in current document."))
    (while x
      (setq y (cons (cons (w3-strip-leading-spaces (nth 3 (car x)))
			  (nth 2 (car x))) y)
	    x (cdr x)))
    (setq z (completing-read "Link: " y nil t))
    (w3-fetch (cdr (assoc z y)))))

(defun w3-load-delayed-images ()
  "Load inlined images that were delayed, if necessary.
This function searches through w3-delayed-images and fetches the
appropriate picture for each point in the buffer and inserts it."
  (let (url pt align alt tmp)
    (while w3-delayed-images
      (setq tmp (car w3-delayed-images)
	    url (nth 0 tmp)
	    pt (nth 1 tmp)
	    align (nth 2 tmp)
	    alt (nth 3 tmp)
	    w3-delayed-images (cdr w3-delayed-images))
      (w3-insert-graphic url pt align alt))))  

(defun w3-setup-version-specifics ()
  "Set up routine for Lucid emacs 19.9"
  nil)

(fset 'w3-store-in-x-clipboard 'epoch::store-cut-buffer)

(provide 'w3-epoch)
