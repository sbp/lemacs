;;; x-faces.el --- X-specific face frobnication.

;;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>

;;;This file is part of GNU Emacs.

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

;; This file does the magic to parse X font names, and make sure that the
;; default and modeline attributes of new screens are specified enough.
;;
;;  The resource-manager syntax for faces is
;;
;;	 Emacs*bold.attributeFont:		font-name
;;	 Emacs*bold.attributeColor:		color
;;	 Emacs*bold.attributeForeground:	fg
;;	 Emacs*bold.attributeBackground:	bg
;;	 Emacs*bold.attributeBackgroundPixmap:	file
;;	 Emacs*bold.attributeUnderline:		true/false
;;
;;  You can specify the parameters of a face on a per-screen basis.  For 
;;  example, to have the "isearch" face use a red foreground on screens
;;  named "emacs" (the default) but use a blue foreground on screens that
;;  you create named "debugger", you could do
;;
;;	 Emacs*emacs.isearch.attributeForeground:	red
;;	 Emacs*debugger.isearch.attributeForeground:	blue
;;
;;  Generally things that make faces won't set any of the face attributes if
;;  you have already given them values via the resource database.  You can
;;  also change this stuff from your .emacs file, by using the functions
;;  set-face-foreground, set-face-font, etc.  See the code in this file, and
;;  in faces.el.


(eval-when-compile
 ;; these used to be defsubsts, now they're subrs.  Avoid losing if we're
 ;; being compiled with an emacs that has the old interpretation.
 ;; (Warning, proclaim-notinline seems to be broken in 19.8.)
 (remprop 'facep 'byte-optimizer)
 (remprop 'face-name 'byte-optimizer)
 (remprop 'face-id 'byte-optimizer)
 (remprop 'face-font 'byte-optimizer)
 (remprop 'face-foreground 'byte-optimizer)
 (remprop 'face-background 'byte-optimizer)
 (remprop 'face-background-pixmap 'byte-optimizer)
 (remprop 'face-underline-p 'byte-optimizer)
 (remprop 'face-font-name 'byte-optimizer)
 (remprop 'set-face-font 'byte-optimizer)
 (remprop 'set-face-foreground 'byte-optimizer)
 (remprop 'set-face-background 'byte-optimizer)
 (remprop 'set-face-background-pixmap 'byte-optimizer)
 (remprop 'set-face-underline-p 'byte-optimizer)
 )


(defconst x-font-regexp nil)
(defconst x-font-regexp-head nil)
(defconst x-font-regexp-head-2 nil)
(defconst x-font-regexp-weight nil)
(defconst x-font-regexp-slant nil)
(defconst x-font-regexp-pixel nil)
(defconst x-font-regexp-point nil)

;;; Regexps matching font names in "Host Portable Character Representation."
;;;
(let ((- 		"[-?]")
      (foundry		"[^-]+")
      (family 		"[^-]+")
      (weight		"\\(bold\\|demibold\\|medium\\|black\\)")	; 1
;     (weight\?		"\\(\\*\\|bold\\|demibold\\|medium\\|\\)")	; 1
      (weight\?		"\\([^-]*\\)")					; 1
      (slant		"\\([ior]\\)")					; 2
;     (slant\?		"\\([ior?*]?\\)")				; 2
      (slant\?		"\\([^-]?\\)")					; 2
;     (swidth		"\\(\\*\\|normal\\|semicondensed\\|\\)")	; 3
      (swidth		"\\([^-]*\\)")					; 3
;     (adstyle		"\\(\\*\\|sans\\|\\)")				; 4
      (adstyle		"\\([^-]*\\)")					; 4
      (pixelsize	"\\(\\*\\|[0-9]+\\)")				; 5
      (pointsize	"\\(\\*\\|0\\|[0-9][0-9]+\\)")			; 6
      (resx		"\\(\\*\\|[0-9][0-9]+\\)")			; 7
      (resy		"\\(\\*\\|[0-9][0-9]+\\)")			; 8
      (spacing		"[cmp?*]")
      (avgwidth		"\\(\\*\\|[0-9]+\\)")				; 9
      (registry		"[^-]+")
;      (encoding	".+")		; note that encoding may contain "-"...
      (encoding	"[^-]+")		; false!
      )
  (setq x-font-regexp
	(purecopy 
	 (concat "\\`\\*?[-?*]"
		 foundry - family - weight\? - slant\? - swidth - adstyle -
		 pixelsize - pointsize - resx - resy - spacing - avgwidth -
		 registry - encoding "\\'"
		 )))
  (setq x-font-regexp-head
	(purecopy
          (concat "\\`[-?*]" foundry - family - weight\? - slant\?
		  "\\([-*?]\\|\\'\\)")))
  (setq x-font-regexp-head-2
	(purecopy
          (concat "\\`[-?*]" foundry - family - weight\? - slant\?
		  - swidth - adstyle - pixelsize - pointsize
		  "\\([-*?]\\|\\'\\)")))
  (setq x-font-regexp-slant (purecopy (concat - slant -)))
  (setq x-font-regexp-weight (purecopy (concat - weight -)))
  ;; if we can't match any of the more specific regexps (unfortunate) then
  ;; look for digits; assume 2+ digits is 10ths of points, and 1-2 digits
  ;; is pixels.  Bogus as hell.
  (setq x-font-regexp-pixel (purecopy "[-?*]\\([0-9][0-9]?\\)[-?*]"))
  (setq x-font-regexp-point (purecopy "[-?*]\\([0-9][0-9]+\\)[-?*]"))
  )


(defun try-font (font &optional screen)
  "Like `make-font', but returns nil if the font can't be loaded."
  (condition-case nil
      (make-font font screen)
    (error nil)))

(defun x-frob-font-weight (font which)
  (if (fontp font) (setq font (font-name font)))
  (if (or (string-match x-font-regexp font)
	  (string-match x-font-regexp-head font)
	  (string-match x-font-regexp-weight font))
      (concat (substring font 0 (match-beginning 1)) which
	      (substring font (match-end 1)))
    nil))

(defun x-frob-font-slant (font which)
  (if (fontp font) (setq font (font-name font)))
  (cond ((or (string-match x-font-regexp font)
	     (string-match x-font-regexp-head font))
	 (concat (substring font 0 (match-beginning 2)) which
		 (substring font (match-end 2))))
	((string-match x-font-regexp-slant font)
	 (concat (substring font 0 (match-beginning 1)) which
		 (substring font (match-end 1))))
	(t nil)))

(defun x-make-font-bold (font &optional screen)
  "Given an X font specification, this attempts to make a `bold' font.
If it fails, it returns nil."
  ;; Certain Type1 fonts know "bold" as "black"...
  (or (try-font (x-frob-font-weight font "bold") screen)
      (try-font (x-frob-font-weight font "black") screen)))

(defun x-make-font-demibold (font &optional screen)
  "Given an X font specification, this attempts to make a `demibold' font.
If it fails, it returns nil."
  (try-font (x-frob-font-weight font "demibold") screen))

(defun x-make-font-unbold (font &optional screen)
  "Given an X font specification, this attempts to make a non-bold font.
If it fails, it returns nil."
  (try-font (x-frob-font-weight font "medium") screen))

(defun x-make-font-italic (font &optional screen)
  "Given an X font specification, this attempts to make an `italic' font.
If it fails, it returns nil."
  (try-font (x-frob-font-slant font "i") screen))

(defun x-make-font-oblique (font &optional screen) ; you say tomayto...
  "Given an X font specification, this attempts to make an `italic' font.
If it fails, it returns nil."
  (try-font (x-frob-font-slant font "o") screen))

(defun x-make-font-unitalic (font &optional screen)
  "Given an X font specification, this attempts to make a non-italic font.
If it fails, it returns nil."
  (try-font (x-frob-font-slant font "r") screen))


(defun x-font-size (font)
  "Returns the nominal size of the given font.
This is done by parsing its name, so it's likely to lose.
X fonts can be specified (by the user) in either pixels or 10ths of points,
 and this returns the first one it finds, so you have to decide which units
 the returned value is measured in yourself..."
  (if (fontp font) (setq font (font-name font)))
  (cond ((or (string-match x-font-regexp font)
	     (string-match x-font-regexp-head-2 font))
	 (string-to-int (substring font (match-beginning 6) (match-end 6))))
	((or (string-match x-font-regexp-pixel font)
	     (string-match x-font-regexp-point font))
	 (string-to-int (substring font (match-beginning 1) (match-end 1))))
	(t nil)))

(defun x-available-font-sizes (font &optional screen)
  (if (fontp font) (setq font (font-name font)))
  (cond ((string-match x-font-regexp font)
	 ;; turn pixelsize, pointsize, and avgwidth into wildcards
	 (setq font
	       (concat (substring font 0 (match-beginning 5)) "*"
		       (substring font (match-end 5) (match-beginning 6)) "*"
		       (substring font (match-end 6) (match-beginning 9)) "*"
		       (substring font (match-end 9) (match-end 0)))))
	((string-match x-font-regexp-head-2 font)
	 ;; turn pixelsize and pointsize into wildcards
	 (setq font
	       (concat (substring font 0 (match-beginning 5)) "*"
		       (substring font (match-end 5) (match-beginning 6)) "*"
		       (substring font (match-end 6) (match-end 0)))))
	((string-match  "[-?*]\\([0-9]+\\)[-?*]" font)
	 ;; Turn the first integer we match into a wildcard.
	 ;; This is pretty dubious...
	 (setq font
	       (concat (substring font 0 (match-beginning 1)) "*"
		       (substring font (match-end 1) (match-end 0))))))
  (sort
   (delq nil
	 (mapcar (function
		  (lambda (name)
		    (and (string-match x-font-regexp name)
			 (list
			  (string-to-int (substring name (match-beginning 5)
						    (match-end 5)))
			  (string-to-int (substring name (match-beginning 6)
						    (match-end 6)))
			  name))))
		 (x-list-fonts font screen)))
   (function (lambda (x y) (if (= (nth 1 x) (nth 1 y))
			       (< (nth 0 x) (nth 0 y))
			       (< (nth 1 x) (nth 1 y)))))))

(defun x-frob-font-size (font up-p screen)
  (if (stringp font) (setq font (make-font font screen)))
  (setq font (font-truename font))
  (let ((available (x-available-font-sizes font screen)))
    (cond
     ((null available) nil)
     ((or (= 0 (nth 0 (car available)))
	  (= 0 (nth 1 (car available))))
      ;; R5 scalable fonts: change size by 1 point.
      ;; If they're scalable the first font will have pixel or point = 0.
      ;; Sometimes one is 0 and the other isn't (if it's a bitmap font that
      ;; can be scaled), sometimes both are (if it's a true outline font.)
      (let ((name (nth 2 (car available)))
	    old-size)
	(or (string-match x-font-regexp font) (error "can't parse %S" font))
	(setq old-size (string-to-int
			(substring font (match-beginning 6) (match-end 6))))
	(or (> old-size 0) (error "font truename has 0 pointsize?"))
	(or (string-match x-font-regexp name) (error "can't parse %S" name))
	;; turn pixelsize into a wildcard, and make pointsize be +/- 10,
	;; which is +/- 1 point.  All other fields stay the same as they
	;; were in the "template" font returned by x-available-font-sizes.
	;;
	;; #### But this might return the same font: for example, if the
	;;      truename of "-*-courier-medium-r-normal--*-230-75-75-m-0-*"
	;;      is "...-240-..." (instead of 230) then this loses, because
	;;      the 230 that was passed in as an arg got turned into 240
	;;      by the call to font-truename; then we decrement that by 10
	;;      and return the result which is the same.  I think the way to
	;;      fix this is to make this be a loop that keeps trying
	;;      progressively larger pointsize deltas until it finds one
	;;      whose truename differs.  Have to be careful to avoid infinite
	;;      loops at the upper end...
	;;
	(concat (substring name 0 (match-beginning 5)) "*"
		(substring name (match-end 5) (match-beginning 6))
		(int-to-string (+ old-size (if up-p 10 -10)))
		(substring name (match-end 6) (match-end 0)))))
     (t
      ;; non-scalable fonts: take the next available size.
      (let ((rest available)
	    (last nil)
	    result)
	(while rest
	  (cond ((and (not up-p) (equal font (nth 2 (car rest))))
		 (setq result last
		       rest nil))
		((and up-p (equal font (nth 2 last)))
		 (setq result (car rest)
		       rest nil)))
	  (setq last (car rest))
	  (setq rest (cdr rest)))
	(nth 2 result))))))


(defun x-find-smaller-font (font &optional screen)
  "Loads a new, slightly smaller version of the given font (or font name).
Returns the font if it succeeds, nil otherwise.
If scalable fonts are available, this returns a font which is 1 point smaller.
Otherwise, it returns the next smaller version of this font that is defined."
  (let ((name (x-frob-font-size font nil screen)))
    (if name (make-font name screen))))

(defun x-find-larger-font (font &optional screen)
  "Loads a new, slightly larger version of the given font (or font name).
Returns the font if it succeeds, nil otherwise.
If scalable fonts are available, this returns a font which is 1 point larger.
Otherwise, it returns the next larger version of this font that is defined."
  (let ((name (x-frob-font-size font t screen)))
    (if name (make-font name screen))))

;;; non-X-specific interface

(defun make-face-bold (face &optional screen)
  "Make the font of the given face be bold, if possible.  
Returns nil on failure."
  (interactive (list (read-face-name "Make which face bold: ")))
  (let ((ofont (or (face-font face screen)
		   (face-font face t)
		   (face-font 'default screen))))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (make-face-bold face (car screens))
	    (setq screens (cdr screens))))
      (setq face (get-face face screen))
      (let ((font (or (face-font face screen)
		      (face-font face t)
		      (face-font 'default screen))))
	(and (setq font (or (x-make-font-bold font screen)
			    (x-make-font-demibold font screen)))
	     (set-face-font face font screen))))
    (not (equal (font-name ofont)
		(font-name (or (face-font face) ofont))))))

(defun make-face-italic (face &optional screen)
  "Make the font of the given face be italic, if possible.  
Returns nil on failure."
  (interactive (list (read-face-name "Make which face italic: ")))
  (let ((ofont (or (face-font face screen)
		   (face-font face t)
		   (face-font 'default screen))))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (make-face-italic face (car screens))
	    (setq screens (cdr screens))))
      (setq face (get-face face screen))
      (let ((font (or (face-font face screen)
		      (face-font face t)
		      (face-font 'default screen))))
	(and (setq font (or (x-make-font-italic font screen)
			    (x-make-font-oblique font screen)))
	     (set-face-font face font screen))))
    (not (equal (font-name ofont)
		(font-name (or (face-font face) ofont))))))

(defun make-face-bold-italic (face &optional screen)
  "Make the font of the given face be bold and italic, if possible.  
Returns nil on failure."
  (interactive (list (read-face-name "Make which face bold-italic: ")))
  (let ((ofont (or (face-font face screen)
		   (face-font face t)
		   (face-font 'default screen))))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (make-face-bold-italic face (car screens))
	    (setq screens (cdr screens))))
      (setq face (get-face face screen))
      (let ((font (or (face-font face screen)
		      (face-font face t)
		      (face-font 'default screen)))
	    f2 f3)
	(setq font
	      ;; This is haired up to avoid loading the "intermediate" fonts.
	      (or (and (setq f2 (x-frob-font-slant font "i"))
		       (not (equal font f2))
		       (setq f3 (x-frob-font-weight f2 "bold"))
		       (not (equal f2 f3))
		       (try-font f3 screen))
		  (and (setq f2 (x-frob-font-slant font "o"))
		       (not (equal font f2))
		       (setq f3 (x-frob-font-weight f2 "bold"))
		       (not (equal f2 f3))
		       (try-font f3 screen))
		  (and (setq f2 (x-frob-font-slant font "i"))
		       (not (equal font f2))
		       (setq f3 (x-frob-font-weight f2 "demibold"))
		       (not (equal f2 f3))
		       (try-font f3 screen))
		  (and (setq f2 (x-frob-font-slant font "o"))
		       (not (equal font f2))
		       (setq f3 (x-frob-font-weight f2 "demibold"))
		       (not (equal f2 f3))
		       (try-font f3 screen))))
	(if font (set-face-font face font screen))))
    (not (equal (font-name ofont)
		(font-name (or (face-font face) ofont))))))

(defun make-face-unbold (face &optional screen)
  "Make the font of the given face be non-bold, if possible.  
Returns nil on failure."
  (interactive (list (read-face-name "Make which face non-bold: ")))
  (let ((ofont (or (face-font face screen)
		   (face-font face t)
		   (face-font 'default screen))))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (make-face-unbold face (car screens))
	    (setq screens (cdr screens))))
      (setq face (get-face face screen))
      (let ((font (x-make-font-unbold
		   (or (face-font face screen)
		       (face-font face t)
		       (face-font 'default screen))
		   screen)))
	(if font (set-face-font face font screen))))
    (not (equal (font-name ofont)
		(font-name (or (face-font face screen) ofont))))))

(defun make-face-unitalic (face &optional screen)
  "Make the font of the given face be non-italic, if possible.  
Returns nil on failure."
  (interactive (list (read-face-name "Make which face non-italic: ")))
  (let ((ofont (or (face-font face screen)
		   (face-font face t)
		   (face-font 'default screen))))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (make-face-unitalic face (car screens))
	    (setq screens (cdr screens))))
      (setq face (get-face face screen))
      (let ((font (x-make-font-unitalic
		   (or (face-font face screen)
		       (face-font face t)
		       (face-font 'default screen))
		   screen)))
	(if font (set-face-font face font screen))))
    (not (equal (font-name ofont)
		(font-name (or (face-font face screen) ofont))))))

(defun make-face-smaller (face &optional screen)
  "Make the font of the given face be smaller, if possible.  
Returns nil on failure."
  (interactive (list (read-face-name "Shrink which face: ")))
  (let ((ofont (or (face-font face screen)
		   (face-font face t)
		   (face-font 'default screen))))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (make-face-smaller face (car screens))
	    (setq screens (cdr screens))))
      (setq face (get-face face screen))
      (let ((font (x-find-smaller-font
		   (or (face-font face screen)
		       (face-font face t)
		       (face-font 'default screen))
		   screen)))
	(if font (set-face-font face font screen))))
    (not (equal (font-truename ofont)
		(font-truename (or (face-font face screen) ofont))))))

(defun make-face-larger (face &optional screen)
  "Make the font of the given face be larger, if possible.  
Returns nil on failure."
  (interactive (list (read-face-name "Enlarge which face: ")))
  (let ((ofont (or (face-font face screen)
		   (face-font face t)
		   (face-font 'default screen))))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (make-face-larger face (car screens))
	    (setq screens (cdr screens))))
      (setq face (get-face face screen))
      (let ((font (x-find-larger-font
		   (or (face-font face screen)
		       (face-font face t)
		       (face-font 'default screen))
		   screen)))
	(if font (set-face-font face font screen))))
    (not (equal (font-truename ofont)
		(font-truename (or (face-font face screen) ofont))))))

;;; Some random other X utils that probably should be in another file.

(defun x-grayscale-display-p (&optional screen)
  ;; is it more appropriate to check whether there are more than two colors
  ;; with (> (x-display-planes) 1) or (> (x-display-color-cells) 2) ?
  (and (> (x-display-planes screen) 1)
       (memq (x-display-visual-class screen) '(StaticGray GrayScale))))

;(defun x-color-display-p (&optional screen)
;  "Returns t if the X display of the given screen supports color."
;  (> (x-display-planes screen) 1))

(defun x-color-display-p (&optional screen)
  "Returns t if the X display of the given screen supports color."
  (and (> (x-display-planes screen) 1)
       (memq (x-display-visual-class screen)
	     '(StaticColor PseudoColor TrueColor DirectColor))))

;; If compiled with support for XPM files, add an `xpm' feature to make this
;; easy for use code to test for.

(if (boundp 'xpm-color-symbols)
    (provide 'xpm))

;; Define some logical color names to be used when reading the pixmap files.
(if (featurep 'xpm)
    (setq xpm-color-symbols
	  (list
	   (purecopy '("foreground" (face-foreground 'default)))
	   (purecopy '("background" (face-background 'default)))
	   )))

;;; internal routines

;;; x-resource-face is responsible for initializing a newly-created face from
;;; the resource database.
;;;
;;; When a new face is created, it is called from Fmake_face() with a screen
;;; argument of nil.  It then initializes this face on all exising screens.
;;;
;;; When a new screen is created, it is called from `x-initialize-screen-faces'
;;; called from `make-screen-initial-faces' called from init_screen_faces()
;;; from Fx_create_screen().  In this case it is called once for each existing
;;; face, with the newly-created screen as the argument.  It then initializes
;;; the newly-created faces on that screen.
;;;
;;; This had better not signal an error.  The screen is in an intermediate
;;; state where signalling an error or entering the debugger would likely
;;; result in a crash.

(defun x-resource-face (face &optional screen set-anyway)
  (cond
   ((null screen)
    (let ((screens (screen-list)))
      (while screens
	(x-resource-face (face-name face) (car screens) set-anyway)
	(setq screens (cdr screens)))))
   (t
    (setq face (get-face (face-name face) screen))
    ;;
    ;; These are things like "attributeForeground" instead of simply
    ;; "foreground" because people tend to do things like "*foreground",
    ;; which would cause all faces to be fully qualified, making faces
    ;; inherit attributes in a non-useful way.  So we've made them slightly
    ;; less obvious to specify in order to make them work correctly in
    ;; more random environments.
    ;;
    ;; I think these should be called "face.faceForeground" instead of
    ;; "face.attributeForeground", but they're the way they are for
    ;; hysterical reasons.
    ;; 
    (let* ((name (symbol-name (face-name face)))
	   (fn  (or (x-get-resource (concat name ".attributeFont")
				    "Face.AttributeFont"
				    'string screen)
		    (and set-anyway (face-font face))))
	   (fg  (or (x-get-resource (concat name ".attributeForeground")
				    "Face.AttributeForeground" 'string screen)
		    (and set-anyway (face-foreground face))))
	   (bg  (or (x-get-resource (concat name ".attributeBackground")
				    "Face.AttributeBackground" 'string screen)
		    (and set-anyway (face-background face))))
	   (bgp (or (x-get-resource (concat name ".attributeBackgroundPixmap")
				    "Face.AttributeBackgroundPixmap" 'string
				    screen)
		    (and set-anyway (face-background-pixmap face))))
	   (ulp (or (x-get-resource (concat name ".attributeUnderline")
				    "Face.AttributeUnderline" 'boolean screen)
		    (and set-anyway (list (face-underline-p face)))))
	   )
      ;;
      ;; If this is the default face, then any unspecified parameters should
      ;; be defaulted from the properties of the "screen".  This is a hack...
      ;;
      (if (eq (face-name face) 'default)
	  (progn
	    (or fn (setq fn (x-get-resource "font" "Font" 'string screen)))
	    (or fg (setq fg (x-get-resource "foreground" "Foreground"
					    'string screen)))
	    (or bg (setq bg (x-get-resource "background" "Background"
					    'string screen)))))
      (if fn
	  (condition-case ()
	      (set-face-font face fn screen)
	    (error (message "font %s not found for face %s" fn name))))
      (if fg
	  (condition-case ()
	      (set-face-foreground face fg screen)
	    (error (message "color %s not allocated for face %s" fg name))))
      (if bg
	  (condition-case ()
	      (set-face-background face bg screen)
	    (error (message "color %s not allocated for face %s" bg name))))
      (if bgp
	  (condition-case ()
	      (set-face-background-pixmap face bgp screen)
	    (error (message "pixmap %s not found for face %s" bgp name))))
      (if (or ulp set-anyway)
	  (set-face-underline-p face (car ulp) screen))
      )))
  face)


;;; x-initialize-screen-faces is responsible for initializing all the faces
;;; on a newly-created screen from the resource database.  It does this by
;;; calling x-resource-face on each of the new screen's faces.
;;;
;;; It is called from `make-screen-initial-faces', which called from 
;;; init_screen_faces() from Fx_create_screen().
;;;
;;; This is called from make-screen-initial-faces to make sure that the
;;; "default" and "modeline" faces for this screen have enough attributes
;;; specified for emacs to be able to display anything on it.  This had
;;; better not signal an error.
;;;
(defun x-initialize-screen-faces (screen)
  ;;
  ;; First initialize all faces from the resource database.
  ;;
  (let ((faces (list-faces)))
    (while faces
      (x-resource-face (car faces) screen)
      (setq faces (cdr faces))))
  ;;
  ;; If the "default" face didn't have a font specified, try to pick one.
  ;;
  (or
   (face-font 'default screen)
   ;;
   ;; No font specified in the resource database; try to cope.
   ;;
   ;; At first I wanted to do this by just putting a font-spec in the
   ;; fallback resources passed to XtAppInitialize(), but that fails
   ;; if there is an Emacs app-defaults file which doesn't specify a
   ;; font: apparently the fallback resources are not consulted when
   ;; there is an app-defaults file, which seems pretty bogus to me.
   ;;
   ;; We should also probably try "*xtDefaultFont", but I think that it
   ;; might be legal to specify that as "xtDefaultFont:", that is, at
   ;; top level, instead of "*xtDefaultFont:", that is, applicable to
   ;; every application.  `x-get-resource' can't handle that right now.
   ;;
   (set-face-font
    'default
    (or
     ;; I18N4 change
;;     (try-font "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*" screen)
;;     (try-font "-*-courier-*-r-*-*-*-120-*-*-*-*-iso8859-*" screen)
;;     (try-font "-*-*-medium-r-*-*-*-120-*-*-m-*-iso8859-*" screen)
;;     (try-font "-*-*-medium-r-*-*-*-120-*-*-c-*-iso8859-*" screen)
;;     (try-font "-*-*-*-r-*-*-*-120-*-*-m-*-iso8859-*" screen)
;;     (try-font "-*-*-*-r-*-*-*-120-*-*-c-*-iso8859-*" screen)
;;     (try-font "-*-*-*-r-*-*-*-120-*-*-*-*-iso8859-*" screen)
     (try-font "-*-*-medium-r-*-*-*-120-*-*-m-*-*-*" screen)
     (try-font "-*-*-medium-r-*-*-*-120-*-*-c-*-*-*" screen)
     (try-font "-*-*-*-r-*-*-*-120-*-*-m-*-*-*" screen)
     (try-font "-*-*-*-r-*-*-*-120-*-*-c-*-*-*" screen)
     (try-font "-*-*-*-r-*-*-*-120-*-*-*-*-*-*" screen)
     ;; if we get to here we're screwed, and faces.c will fatal()...
     )))
  ;;
  ;; If the "default" face didn't have both colors specified, then pick
  ;; some, taking into account the "reverseVideo" resource, as well as
  ;; whether one of the colors was specified.  
  ;;
  (let ((fg (face-foreground 'default screen))
	(bg (face-background 'default screen)))
    (if (not (and fg bg))
	(if (or (and fg (equal (downcase (pixel-name fg)) "white"))
		(and bg (equal (downcase (pixel-name bg)) "black"))
		(car (x-get-resource "reverseVideo" "ReverseVideo"
				     'boolean screen)))
	    (progn
	      (or fg (set-face-foreground 'default "white" screen))
	      (or bg (set-face-background 'default "black" screen)))
	  (or fg (set-face-foreground 'default "black" screen))
	  (or bg (set-face-background 'default "white" screen)))))
  ;;
  ;; Now let's try to pick some reasonable defaults for a few other faces.
  ;; This kind of stuff should normally go on the create-screen-hook, but
  ;; this way we won't be in danger of the user screwing things up by not
  ;; adding hooks in a safe way.
  ;;
  (let ((pre-display-buffer-function nil) ; we're on thin ice here...
	(stack-trace-on-error nil)
	(debug-on-error nil))
    (x-initialize-other-random-faces screen)
    (x-initialize-pointer-shape screen)  ; from x-mouse.el
    ))

(defvar x-inhibit-font-complaints nil
  "Whether to suppress complaints about incomplete sets of fonts.")

(defun x-complain-about-font (face)
  (if (symbolp face) (setq face (symbol-name face)))
  (if (not x-inhibit-font-complaints)
      (princ (format "%s: couldn't deduce %s %s version of %S\n"
		     invocation-name
		     (if (string-match "\\`[aeiouAEIOU]" face) "an" "a")
		     face
		     (face-font-name 'default))
	     (function external-debugging-output))))

(defun x-initialize-other-random-faces (screen)
  "Initializes the colors and fonts of the bold, italic, bold-italic, 
primary-selection, secondary-selection, and isearch faces when each
screen is created.  If you want to add code to do stuff like this, use
the create-screen-hook."

  (or (face-differs-from-default-p 'bold screen)
      (make-face-bold 'bold screen)
      ;; if default font is bold, then make the `bold' face be unbold.
      (make-face-unbold 'bold screen)
      ;; otherwise the luser specified one of the bogus font names
      (x-complain-about-font 'bold)
      )

  (or (face-differs-from-default-p 'italic screen)
      (make-face-italic 'italic screen)
      (progn
	(make-face-bold 'italic screen) ; bold if possible, then complain
	(x-complain-about-font 'italic))
      )

  (or (face-differs-from-default-p 'bold-italic screen)
      (make-face-bold-italic 'bold-italic screen)
      ;; if we couldn't get a bold-italic version, try just bold.
      (make-face-bold 'bold-italic screen)
      ;; if we couldn't get bold or bold-italic, then that's probably because
      ;; the default font is bold, so make the `bold-italic' face be unbold.
      (and (make-face-unbold 'bold-italic screen)
	   (make-face-italic 'bold-italic screen))
      ;; if that didn't work, try italic (can this ever happen? what the hell.)
      (progn
	(make-face-italic 'bold-italic screen)
	;; then bitch and moan.
	(x-complain-about-font 'bold-italic))
      )

  (or (find-face 'primary-selection)
      (make-face 'primary-selection))

  (or (find-face 'secondary-selection)
      (make-face 'secondary-selection))

  (or (face-differs-from-default-p 'highlight screen)
      (condition-case ()
	  (if (x-color-display-p)
              (condition-case ()
		  (set-face-background 'highlight "darkseagreen2" screen)
                (error (set-face-background 'highlight "green" screen)))
	    (set-face-background-pixmap 'highlight "gray1" screen))
	(error (invert-face 'highlight screen))))

  (or (face-differs-from-default-p 'primary-selection screen)
      (condition-case ()
	  (if (x-color-display-p)
	      (set-face-background 'primary-selection "gray" screen)
	    (set-face-background-pixmap 'primary-selection "gray3" screen))
	(error (invert-face 'primary-selection screen))))

  (or (face-differs-from-default-p 'secondary-selection screen)
      (condition-case ()
	  (if (x-color-display-p)
              (condition-case ()
		  ;; some older X servers don't have this one.
		  (set-face-background 'secondary-selection "paleturquoise"
				       screen)
		(error
		 (set-face-background 'secondary-selection "green" screen)))
	    (set-face-background-pixmap 'secondary-selection "gray1" screen))
	(error (invert-face 'secondary-selection screen))))

  (or (face-differs-from-default-p 'isearch screen)
      (if (x-color-display-p)
	  (condition-case ()
	      (set-face-background 'isearch "paleturquoise" screen)
	    (error
	     (condition-case ()
		 (set-face-background 'isearch "green" screen)
	       (error nil))))
	nil)
      (make-face-bold 'isearch screen)
      ;; if default font is bold, then make the `isearch' face be unbold.
      (make-face-unbold 'isearch screen))
  )

(provide 'x-faces)
