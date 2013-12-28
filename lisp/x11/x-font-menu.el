;; x-font-menu.el --- Managing menus of X fonts.

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>

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

;;; Commentary:
;;;
;;; Creates three menus, "Font", "Size", and "Weight", and puts them on the
;;; "Options" menu.  The contents of these menus are the superset of those
;;; parameters available on any fonts, but only the intersection of the three
;;; sets is selectable at one time.
;;;
;;; Known Problems:
;;; ===============
;;; Items on the Font menu are selectable if and only if that font exists in
;;; the same size and weight as the current font.  This means that some fonts
;;; are simply not reachable from some other fonts - if only one font comes
;;; in only one point size (like "Nil", which comes only in 2), you will never
;;; be able to select it.  It would be better if the items on the Fonts menu
;;; were always selectable, and selecting them would set the size to be the
;;; closest size to the current font's size.
;;;
;;; This attempts to change all other faces in an analagous way to the change
;;; that was made to the default face; if it can't, it will skip over the face.
;;; However, this could leave incongruous font sizes around, which may cause
;;; some nonreversibility problems if further changes are made.  Perhaps it
;;; should remember the initial fonts of all faces, and derive all subsequent
;;; fonts from that initial state.
;;;
;;; xfontsel(1) is a lot more flexible (but probably harder to understand.)
;;;
;;; This code uses the before-init-hook to build its list of fonts once.
;;; If the font path changes after emacs has started up, this won't notice.
;;; This also could cause startup to take even longer, sigh...
;;;
;;; There is knowledge here about the regexp match numbers in `x-font-regexp',
;;; `x-font-regexp-foundry-and-family', and
;;; `x-font-regexp-registry-and-encoding' defined in x-faces.el.
;;;
;;; There are at least three kinds of fonts under X11r5:
;;;
;;; - bitmap fonts, which can be assumed to look as good as possible;
;;; - bitmap fonts which have been (or can be) automatically scaled to
;;;   a new size, and which almost always look awful;
;;; - and true outline fonts, which should look ok any any size, but in
;;;   practice (on at least some systems) look awful at any size, and
;;;   even in theory are unlikely ever to look as good as non-scaled
;;;   bitmap fonts.
;;;
;;; It would be nice to get this code to look for non-scaled bitmap fonts
;;; first, then outline fonts, then scaled bitmap fonts as a last resort.
;;; But it's not clear to me how to tell them apart based on their truenames
;;; and/or the result of XListFonts().  I welcome any and all explanations
;;; of the subtleties involved...
;;;
;;;
;;; If You Think You'Re Seeing A Bug:
;;; =================================
;;; When reporting problems, send the following information:
;;;
;;; - Exactly what behavior you're seeing;
;;; - The output of the `xlsfonts' program;
;;; - The value of the variable `fonts-menu-cache';
;;; - The values of the following expressions, both before and after
;;;   making a selection from any of the fonts-related menus:
;;;	(face-font 'default)
;;;	(font-truename (face-font 'default))
;;;	(x-font-properties (face-font 'default))
;;; - The values of the following variables after making a selection:
;;;	font-menu-preferred-resolution
;;;	font-menu-preferred-registry
;;;
;;; There is a common misconception that "*-courier-medium-r-*-11-*", also
;;; known as "-adobe-courier-medium-r-normal--11-80-100-100-m-60-iso8859-1",
;;; is an 11-point font.  It is not -- it is an 11-pixel font at 100dpi,
;;; which is an 8-point font (the number after -11- is the size in tenths
;;; of points.)  So if you expect to be seeing an "11" entry in the "Size"
;;; menu and are not, this may be why.

;;; Code:

(defconst fonts-menu-cache nil) ; so we only call XListFonts (and parse) once.

(defconst font-menu-preferred-resolution nil)
(defconst font-menu-preferred-registry nil)

(defconst fonts-menu-junk-families
  (purecopy
   (mapconcat
    #'identity
    '("cursor" "glyph" "symbol"	; Obvious losers.
      "\\`Ax...\\'"		; FrameMaker fonts - there are just way too
				;  many of these, and there is a different
				;  font family for each font face!  Losers.
				;  "Axcor" -> "Applix Courier Roman",
				;  "Axcob" -> "Applix Courier Bold", etc.
      )
    "\\|"))
  "A regexp matching font families which are uninteresting (cursor fonts.)")

(defun install-font-menus (&optional debug)
  "Generates and installs the `Font', `Size', and `Weight' submenus of Options.
This is run once just after emacs starts up.  If you add fonts to your system,
or if you change your font path, you can call this to re-initialize the menus."
  (if (or noninteractive (not (eq window-system 'x)))
      nil
    (let ((menus (compute-font-menus debug))
	  (menubars '(current-menubar default-menubar	; kludge!!!!
		      energize-menubar big-menubar)))
      (while menubars
	(if (boundp (car menubars))
	    (let ((current-menubar (symbol-value (car menubars)))
		  (rest menus))
	      (while rest
		(add-menu '("Options") (car (car rest)) (cdr (car rest)))
		(setq rest (cdr rest)))))
	(setq menubars (cdr menubars))))))

(defun compute-font-menus (&optional debug)
  ;; Runs x-list-fonts (which is slow), fills the cache, generates the menus.
  (let ((all-fonts nil)
	(case-fold-search t)
	name family size weight entry
	(cache nil)
	(families nil)
	(sizes nil)
	(weights nil))
    (cond ((stringp debug)	; kludge
	   (let ((i -1) j)
	     (while (prog1 i
		      (and i (setq j (1+ i)
				   i (string-match "\n" debug j))))
	       (setq all-fonts (cons (substring debug j i) all-fonts)))
	     (setq all-fonts (nreverse all-fonts))))
	  (t
	   (setq all-fonts
		 (or debug (x-list-fonts "*-*-*-*-*-*-*-*-*-*-*-*-*-*")))))
    (while all-fonts
      (setq name (car all-fonts))
      (cond ((string-match x-font-regexp name)
	     (setq weight (capitalize
			   (substring name (match-beginning 1) (match-end 1)))
		   size   (substring name (match-beginning 6) (match-end 6)))
	     (setq size (string-to-int size))
	     (or (string-match x-font-regexp-foundry-and-family name)
		 (error "internal error"))
	     (setq family (capitalize
			   (substring name (match-beginning 1) (match-end 1))))
	     (if (string-match fonts-menu-junk-families family)
		 nil
	       (setq entry (or (assoc family cache)
			       (car (setq cache (cons (list family nil nil)
						      cache)))))
	       (or (member family families)
		   (setq families (cons family families)))
	       (or (member weight weights)
		   (setq weights (cons weight weights)))
	       (or (member weight (nth 1 entry))
		   (setcar (cdr entry) (cons weight (nth 1 entry))))
	       (or (member size sizes)
		   (setq sizes (cons size sizes)))
	       (or (member size (nth 2 entry))
		   (setcar (cdr (cdr entry)) (cons size (nth 2 entry))))
	       )))
      (setq all-fonts (cdr all-fonts)))
    ;;
    ;; Hack scalable fonts.
    ;; Some fonts come only in scalable versions (the only size is 0)
    ;; and some fonts come in both scalable and non-scalable versions
    ;; (one size is 0).  If there are any scalable fonts at all, make
    ;; sure that the union of all point sizes contains at least some
    ;; common sizes - it's possible that some sensible sizes might end
    ;; up not getting mentioned explicitly.
    ;;
    (if (member 0 sizes)
	(let ((common '(60 80 100 120 140 160 180 240)))
	  (while common
	    (or ;;(member (car common) sizes)   ; not enough slack
		(let ((rest sizes)
		      (done nil))
		  (while (and (not done) rest)
		    (if (and (> (car common) (- (car rest) 5))
			     (< (car common) (+ (car rest) 5)))
			(setq done t))
		    (setq rest (cdr rest)))
		  done)
		(setq sizes (cons (car common) sizes)))
	    (setq common (cdr common)))
	  (setq sizes (delq 0 sizes))))

    (setq families (sort families 'string-lessp)
	  weights (sort weights 'string-lessp)
	  sizes (sort sizes '<))
    (let ((rest cache))
      (while rest
	(setcar (cdr (car rest)) (sort (nth 1 (car rest)) 'string-lessp))
	(setcar (cdr (cdr (car rest))) (sort (nth 2 (car rest)) '<))
	(setq rest (cdr rest))))
    (setq fonts-menu-cache cache)
    (list
     (cons "Font"
	   (mapcar #'(lambda (x)
		       (vector x
			       (list 'font-menu-set-font x nil nil)
			       ':style 'radio ':active nil ':selected nil))
		   families))
     (cons "Size"
	   (mapcar #'(lambda (x)
		       (vector (if (/= 0 (% x 10))
				   ;; works with no LISP_FLOAT_TYPE
				   (concat (int-to-string (/ x 10)) "."
					   (int-to-string (% x 10)))
				 (int-to-string (/ x 10)))
			       (list 'font-menu-set-font nil nil x)
			       ':style 'radio ':active nil ':selected nil))
		   sizes))
     (cons "Weight"
	   (mapcar #'(lambda (x)
		       (vector x
			       (list 'font-menu-set-font nil x nil)
			       ':style 'radio ':active nil ':selected nil))
		   weights))
     )))

(defun sensitize-fonts-menus-hook ()
  ;; For use as a value of activate-menubar-hook.
  ;; This function changes the sensitivity of the Font, Size, and Weight
  ;; submenus of the Options menu to correspond to the current state.
  ;;
  ;; This could have been implemented by putting `eval'ble forms in the
  ;; menu items themselves, but since there can be a lot of menu items,
  ;; and since such forms are evaluated every time the menubar is
  ;; displayed, it's more efficient to do them all at once.
  ;;
  (let* ((opt-menu (cdr (car (find-menu-item current-menubar '("Options")))))
	 (font-menu
	  (and opt-menu (cdr (car (find-menu-item opt-menu '("Font"))))))
	 (size-menu
	  (and opt-menu (cdr (car (find-menu-item opt-menu '("Size"))))))
	 (weight-menu
	  (and opt-menu (cdr (car (find-menu-item opt-menu '("Weight"))))))

	 (name (font-truename (face-font 'default)))
	 (case-fold-search t)
	 family weight size
	 f w s
	 item entry)
    (if (or (not (or font-menu size-menu weight-menu))
	    (not (string-match x-font-regexp name))
	    (stringp (car size-menu))) ; gag
	;; If we can't parse the curent font, or if the menus don't exist,
	;; then do nothing.
	t
      ;; Otherwise, sensitize the menus.
      (setq weight (capitalize ; dammit!
		    (substring name (match-beginning 1) (match-end 1))))
      (setq size (string-to-number
		  (substring name (match-beginning 6) (match-end 6))))
      (and (string-match x-font-regexp-foundry-and-family name)
	   (setq family
		 (capitalize ; dammit!
		  (substring name (match-beginning 1) (match-end 1)))))
      (setq entry (assoc family fonts-menu-cache))
      ;;
      ;; Items on the Size menu are enabled iff current font has that size.
      ;; Only the size of the current font is selected.
      ;; (If the current font comes in size 0, it is scalable, and thus
      ;; has every size.)
      ;;
      (while size-menu
	(setq item (car size-menu)
	      ;; s (string-to-int (aref item 0))
	      s (nth 3 (aref item 1)))
	(if (or (member s (nth 2 entry))
		(member 0 (nth 2 entry)))
	    (enable-menu-item item)
	  (disable-menu-item item))
	(if (eq size s)
	    (select-toggle-menu-item item)
	  (deselect-toggle-menu-item item))
	(setq size-menu (cdr size-menu)))
      ;;
      ;; Items on the Weight menu are enabled iff current font has that weight.
      ;; Only the weight of the current font is selected.
      ;;
      (while weight-menu
	(setq item (car weight-menu)
	      w (aref item 0))
	(if (member w (nth 1 entry))
	    (enable-menu-item item)
	  (disable-menu-item item))
	(if (equal weight w)
	    (select-toggle-menu-item item)
	  (deselect-toggle-menu-item item))
	(setq weight-menu (cdr weight-menu)))
      ;;
      ;; Items on the Font menu are enabled iff that font exists in the same
      ;; size and weight as the current font (scalable fonts exist in every
      ;; size).  Only the current font is marked as selected.
      ;;
      (while font-menu
	(setq item (car font-menu)
	      f (aref item 0)
	      entry (assoc f fonts-menu-cache))
	(if (and (member weight (nth 1 entry))
		 (or (member size (nth 2 entry))
		     (member 0 (nth 2 entry))))
	    (enable-menu-item item)
	  (disable-menu-item item))
	(if (equal family f)
	    (select-toggle-menu-item item)
	  (deselect-toggle-menu-item item))
	(setq font-menu (cdr font-menu)))
      ;;
      ;; Return nil, meaning "we may have made changes."
      nil)))


;;; Changing font sizes

(defun font-menu-set-font (family weight size)
  ;; This is what gets run when an item is selected from any of the three
  ;; fonts menus.  It needs to be rather clever.
  ;; (size is measured in 10ths of points.)
  (let ((faces (delq 'default (list-faces)))
	(default-name (font-truename (face-font 'default)))
	(case-fold-search t)
	new-default-face-font
	from-family from-weight from-size)
    ;;
    ;; First, parse out the default face's font.
    ;;
    (or (string-match x-font-regexp-foundry-and-family default-name)
	(signal 'error (list "couldn't parse font name" default-name)))
    (setq from-family (capitalize
		       (substring default-name
				  (match-beginning 1) (match-end 1))))
    (or (string-match x-font-regexp default-name)
	(signal 'error (list "couldn't parse font name" default-name)))
    (setq from-weight (capitalize
		       (substring default-name
				  (match-beginning 1) (match-end 1))))
    (setq from-size (substring default-name (match-beginning 6) (match-end 6)))
    (setq new-default-face-font
	  (font-menu-load-font (or family from-family)
			       (or weight from-weight)
			       (or size   from-size)
			       default-name))
    (while faces
      (cond ((face-font (car faces))
	     (message "Changing font of `%s'..." (car faces))
	     (condition-case c
		 (font-menu-change-face (car faces)
					from-family from-weight from-size
					family weight size)
	       (error
		(display-error c nil)
		(sit-for 1)))))
      (setq faces (cdr faces)))
    ;; Set the default face's font after hacking the other faces, so that
    ;; the screen size doesn't change until we are all done.
    (set-face-font 'default new-default-face-font)
    (message "Font %s" (face-font-name 'default))))


(defun font-menu-change-face (face
			      from-family from-weight from-size
			      to-family   to-weight   to-size)
  (or (symbolp face) (signal 'wrong-type-argument (list 'symbolp face)))
  (let* ((font (face-font face))
	 (name (font-truename font))
	 (case-fold-search t)
	 face-family
	 face-weight
	 face-size)
    ;; First, parse out the face's font.
    (or (string-match x-font-regexp-foundry-and-family name)
	(signal 'error (list "couldn't parse font name" name)))
    (setq face-family (capitalize
		       (substring name (match-beginning 1) (match-end 1))))
    (or (string-match x-font-regexp name)
	(signal 'error (list "couldn't parse font name" name)))
    (setq face-weight (substring name (match-beginning 1) (match-end 1)))
    (setq face-size (substring name (match-beginning 6) (match-end 6)))

    ;; If this face matches the old default face in the attribute we are
    ;; changing, then change it to the new attribute along that dimension.
    ;; Otherwise leave it alone.
    (if (cond (to-family (equal face-family from-family))
	      (to-weight (equal face-weight from-weight))
	      (to-size   (equal face-size from-size)))
	(set-face-font face
		       (font-menu-load-font (or to-family face-family)
					    (or to-weight face-weight)
					    (or to-size   face-size)
					    name))
      nil)))


(defun font-menu-load-font (family weight size from-font)
  (let ((case-fold-search t)
	slant other-slant
	registry encoding resx resy)
    (or (string-match x-font-regexp-registry-and-encoding from-font)
	(signal 'error (list "couldn't parse font name" from-font)))
    (setq registry (substring from-font (match-beginning 1) (match-end 1))
	  encoding (substring from-font (match-beginning 2) (match-end 2)))

    (or (string-match x-font-regexp from-font)
	(signal 'error (list "couldn't parse font name" from-font)))
    (setq slant (capitalize
		 (substring from-font (match-beginning 2) (match-end 2)))
	  resx  (substring from-font (match-beginning 7) (match-end 7))
	  resy  (substring from-font (match-beginning 8) (match-end 8)))
    (cond ((equal slant "O") (setq other-slant "I")) ; oh, bite me.
	  ((equal slant "I") (setq other-slant "O"))
	  (t (setq other-slant nil)))
    ;;
    ;; Remember these values for the first font we switch away from
    ;; (the original default font.)
    ;;
    (or font-menu-preferred-resolution
	(setq font-menu-preferred-resolution (cons resx resy)))
    (or font-menu-preferred-registry
	(setq font-menu-preferred-registry (cons registry encoding)))
    ;;
    ;; Now we know all the interesting parameters of the font we want.
    ;; Let's see what we can actually *get*.
    ;;
    (or ;; First try the default resolution, registry, and encoding.
        (try-font (concat "-*-" family "-" weight "-" slant "-*-*-*-" size
			  "-" (car font-menu-preferred-resolution)
			  "-" (cdr font-menu-preferred-resolution)
			  "-*-*-"
			  (car font-menu-preferred-registry) "-"
			  (cdr font-menu-preferred-registry)))
	;; Then try that in the other slant.
	(and other-slant
	     (try-font (concat "-*-" family "-" weight "-" other-slant
			       "-*-*-*-" size
			       "-" (car font-menu-preferred-resolution)
			       "-" (cdr font-menu-preferred-resolution)
			       "-*-*-"
			       (car font-menu-preferred-registry) "-"
			       (cdr font-menu-preferred-registry))))
	;; Then try the default resolution and registry, any encoding.
	(try-font (concat "-*-" family "-" weight "-" slant "-*-*-*-" size
			  "-" (car font-menu-preferred-resolution)
			  "-" (cdr font-menu-preferred-resolution)
			  "-*-*-"
			  (car font-menu-preferred-registry) "-*"))
	;; Then try that in the other slant.
	(and other-slant
	     (try-font (concat "-*-" family "-" weight "-" other-slant
			       "-*-*-*-" size
			       "-" (car font-menu-preferred-resolution)
			       "-" (cdr font-menu-preferred-resolution)
			       "-*-*-"
			       (car font-menu-preferred-registry) "-*")))
	;; Then try the default registry and encoding, any resolution.
	(try-font (concat "-*-" family "-" weight "-" slant "-*-*-*-" size
			  "-*-*-*-*-"
			  (car font-menu-preferred-registry) "-"
			  (cdr font-menu-preferred-registry)))
	;; Then try that in the other slant.
	(and other-slant
	     (try-font (concat "-*-" family "-" weight "-" other-slant
			       "-*-*-*-" size
			       "-*-*-*-*-"
			       (car font-menu-preferred-registry) "-"
			       (cdr font-menu-preferred-registry))))
	;; Then try the default registry, any encoding or resolution.
	(try-font (concat "-*-" family "-" weight "-" slant "-*-*-*-" size
			  "-*-*-*-*-"
			  (car font-menu-preferred-registry) "-*"))
	;; Then try that in the other slant.
	(and other-slant
	     (try-font (concat "-*-" family "-" weight "-" slant "-*-*-*-"
			       size "-*-*-*-*-"
			       (car font-menu-preferred-registry) "-*")))
	;; Then try anything in the same slant, and error if it fails...
	(and other-slant
	     (make-font (concat "-*-" family "-" weight "-" slant "-*-*-*-"
				size "-*-*-*-*-*-*")))
	(make-font (concat "-*-" family "-" weight "-" (or other-slant slant)
			   "-*-*-*-" size "-*-*-*-*-*-*"))
	)))

;(defun OLD-font-menu-set-font (family weight size)
;  ;; This is what gets run when an item is selected from any of the three
;  ;; fonts menus.  It needs to be rather clever.
;  ;; (size is measured in 10ths of points.)
;  (let* ((font (face-font 'default))
;	 (name (font-truename font))
;	 (case-fold-search t)
;	 registry encoding resx resy)
;    ;;
;    ;; First, parse out the current font.
;    ;;
;    (or family
;	(progn
;	  (or (string-match x-font-regexp-foundry-and-family name)
;	      (signal 'error (list "couldn't parse font name" name)))
;	  (setq family (substring name (match-beginning 1) (match-end 1)))))
;
;    (or (string-match x-font-regexp-registry-and-encoding name)
;	(signal 'error (list "couldn't parse font name" name)))
;    (setq registry (substring name (match-beginning 1) (match-end 1))
;	  encoding (substring name (match-beginning 2) (match-end 2)))
;
;    (or (string-match x-font-regexp name)
;	(signal 'error (list "couldn't parse font name" name)))
;    (setq resx (substring name (match-beginning 7) (match-end 7))
;	  resy (substring name (match-beginning 8) (match-end 8)))
;
;    (or weight
;	(setq weight (substring name (match-beginning 1) (match-end 1))))
;    (or size
;	(setq size (substring name (match-beginning 6) (match-end 6))))
;    ;;
;    ;; Remember these values for the first font we switch away from (the
;    ;; original default font.)
;    ;;
;    (or font-menu-preferred-resolution
;	(setq font-menu-preferred-resolution (cons resx resy)))
;    (or font-menu-preferred-registry
;	(setq font-menu-preferred-registry (cons registry encoding)))
;    ;;
;    ;; Now we know all the interesting parameters of the font we want.
;    ;; Let's see what we can actually *get*.
;    ;;
;    (let ((font
;	   (or
;	    ;; First try the default resolution, registry, and encoding.
;	    (try-font (concat "-*-" family "-" weight "-r-*-*-*-" size
;			      "-" (car font-menu-preferred-resolution)
;			      "-" (cdr font-menu-preferred-resolution)
;			      "-*-*-"
;			      (car font-menu-preferred-registry) "-"
;			      (cdr font-menu-preferred-registry)))
;	    ;; Then try the default resolution and registry, any encoding.
;	    (try-font (concat "-*-" family "-" weight "-r-*-*-*-" size
;			      "-" (car font-menu-preferred-resolution)
;			      "-" (cdr font-menu-preferred-resolution)
;			      "-*-*-"
;			      (car font-menu-preferred-registry) "-*"))
;	    ;; Then try the default registry and encoding, any resolution.
;	    (try-font (concat "-*-" family "-" weight "-r-*-*-*-" size
;			      "-*-*-*-*-"
;			      (car font-menu-preferred-registry) "-"
;			      (cdr font-menu-preferred-registry)))
;	    ;; Then try the default registry, any encoding or resolution.
;	    (try-font (concat "-*-" family "-" weight "-r-*-*-*-" size
;			      "-*-*-*-*-"
;			      (car font-menu-preferred-registry) "-*"))
;	    ;; Then try anything in Roman...
;	    (try-font (concat "-*-" family "-" weight "-r-*-*-*-" size
;			      "-*-*-*-*-*-*"))
;	    ;; Then try anything at all.
;	    (make-font (concat "-*-" family "-" weight "-*-*-*-*-" size
;			       "-*-*-*-*-*-*"))
;	    )))
;    (set-face-font 'default font)
;    (set-face-font 'modeline font)
;    (message "Font %s" (font-name font))
;    font)))


(add-hook 'activate-menubar-hook 'sensitize-fonts-menus-hook)

(add-hook 'before-init-hook 'install-font-menus t)
(install-font-menus)

(provide 'font-menu)

;;; x-font-menu.el ends here
