;; X -specific face frobnication.
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

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

(defconst x-font-regexp nil)
(defconst x-font-regexp-head nil)
(defconst x-font-regexp-weight nil)
(defconst x-font-regexp-slant nil)

;;; Regexps matching font names in "Host Portable Character Representation."
;;;
(let ((- 		"[-?]")
      (foundry		"[^-]+")
      (family 		"[^-]+")
      (weight		"\\(bold\\|demibold\\|medium\\)")		; 1
;     (weight\?		"\\(\\*\\|bold\\|demibold\\|medium\\|\\)")	; 1
      (weight\?		"\\([^-]*\\)")					; 1
      (slant		"\\([ior]\\)")					; 2
;     (slant\?		"\\([ior?*]?\\)")				; 2
      (slant\?		"\\([^-]?\\)")					; 2
;     (swidth		"\\(\\*\\|normal\\|semicondensed\\|\\)")	; 3
      (swidth		"\\([^-]*\\)")					; 3
;     (adstyle		"\\(\\*\\|sans\\|\\)")				; 4
      (adstyle		"[^-]*")					; 4
      (pixelsize	"[0-9]+")
      (pointsize	"[0-9][0-9]+")
      (resx		"[0-9][0-9]+")
      (resy		"[0-9][0-9]+")
      (spacing		"[cmp?*]")
      (avgwidth		"[0-9]+")
      (registry		"[^-]+")
      (encoding		"[^-]+")
      )
  (setq x-font-regexp
	(concat "\\`\\*?[-?*]"
		foundry - family - weight\? - slant\? - swidth - adstyle -
		pixelsize - pointsize - resx - resy - spacing - registry -
		encoding "[-?*]\\*?\\'"
		))
  (setq x-font-regexp-head
	(concat "\\`[-?*]" foundry - family - weight\? - slant\?
		"\\([-*?]\\|\\'\\)"))
  (setq x-font-regexp-slant (concat - slant -))
  (setq x-font-regexp-weight (concat - weight -))
  nil)	    

(defun x-frob-font-weight (font which)
  (if (or (string-match x-font-regexp font)
	  (string-match x-font-regexp-head font)
	  (string-match x-font-regexp-weight font))
      (concat (substring font 0 (match-beginning 1)) which
	      (substring font (match-end 1)))
    nil))

(defun x-frob-font-slant (font which)
  (cond ((or (string-match x-font-regexp font)
	     (string-match x-font-regexp-head font))
	 (concat (substring font 0 (match-beginning 2)) which
		 (substring font (match-end 2))))
	((string-match x-font-regexp-slant font)
	 (concat (substring font 0 (match-beginning 1)) which
		 (substring font (match-end 1))))
	(t nil)))


(defun x-make-font-bold (font)
  "Given an X font specification, this attempts to make a `bold' version
of it.  If it fails, it returns nil."
  (x-frob-font-weight font "bold"))

(defun x-make-font-demibold (font)
  "Given an X font specification, this attempts to make a `demibold' version
of it.  If it fails, it returns nil."
  (x-frob-font-weight font "demibold"))

(defun x-make-font-unbold (font)
  "Given an X font specification, this attempts to make a non-bold version
of it.  If it fails, it returns nil."
  (x-frob-font-weight font "medium"))

(defun x-make-font-italic (font)
  "Given an X font specification, this attempts to make an `italic' version
of it.  If it fails, it returns nil."
  (x-frob-font-slant font "i"))

(defun x-make-font-oblique (font) ; you say tomayto...
  "Given an X font specification, this attempts to make an `italic' version
of it.  If it fails, it returns nil."
  (x-frob-font-slant font "o"))

(defun x-make-font-unitalic (font)
  "Given an X font specification, this attempts to make a non-italic version
of it.  If it fails, it returns nil."
  (x-frob-font-slant font "r"))


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
		      (face-font 'default screen)))
	    f2)
	(or (and (setq f2 (x-make-font-bold font))
		 (try-face-font face f2))
	    (and (setq f2 (x-make-font-demibold font))
		 (try-face-font face f2)))))
    (not (equal ofont (or (face-font face) ofont)))))

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
		      (face-font 'default screen)))
	    f2)
	(or (and (setq f2 (x-make-font-italic font))
		 (try-face-font face f2))
	    (and (setq f2 (x-make-font-oblique font))
		 (try-face-font face f2)))))
    (not (equal ofont (or (face-font face) ofont)))))

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
	(or (and (setq f2 (x-make-font-italic font))
		 (not (equal font f2))
		 (setq f3 (x-make-font-bold f2))
		 (not (equal f2 f3))
		 (try-face-font face f3))
	    (and (setq f2 (x-make-font-oblique font))
		 (not (equal font f2))
		 (setq f3 (x-make-font-bold f2))
		 (not (equal f2 f3))
		 (try-face-font face f3))
	    (and (setq f2 (x-make-font-italic font))
		 (not (equal font f2))
		 (setq f3 (x-make-font-demibold f2))
		 (not (equal f2 f3))
		 (try-face-font face f3))
	    (and (setq f2 (x-make-font-oblique font))
		 (not (equal font f2))
		 (setq f3 (x-make-font-demibold f2))
		 (not (equal f2 f3))
		 (try-face-font face f3)))))
    (not (equal ofont (or (face-font face screen) ofont)))))

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
		       (face-font 'default screen)))))
	(if font (try-face-font face font))))
    (not (equal ofont (or (face-font face screen) ofont)))))

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
		       (face-font 'default screen)))))
	(if font (try-face-font face font))))
    (not (equal ofont (or (face-font face screen) ofont)))))


;;; internal routines

;;; This is called from make-face to read the initial values of the face
;;; from the resource database.  (Later calls to set-face-mumble may override
;;; these values.)  This is also called from make-screen-initial-faces before
;;; the initial X screen is mapped, so it had better not signal an error.
;;;
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
		    (and set-anyway (face-underline-p face))))
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
	  (set-face-underline-p face ulp screen))
      )))
  face)


;;; This is called from make-screen-initial-faces to make sure that the
;;; "default" and "modeline" faces for this screen have enough attributes
;;; specified for emacs to be able to display anything on it.  This had
;;; better not signal an error.
;;;
(defun x-initialize-screen-faces (screen)
  (let ((default (get-face 'default screen))
	(modeline (get-face 'modeline screen)))
    (or
     (face-font default screen)
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
     (try-face-font default "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"
		    screen)
     (try-face-font default "-*-courier-*-r-*-*-*-120-*-*-*-*-iso8859-*"
		    screen)
     (try-face-font default "-*-*-medium-r-*-*-*-120-*-*-m-*-iso8859-*" screen)
     (try-face-font default "-*-*-medium-r-*-*-*-120-*-*-c-*-iso8859-*" screen)
     (try-face-font default "-*-*-*-r-*-*-*-120-*-*-m-*-iso8859-*" screen)
     (try-face-font default "-*-*-*-r-*-*-*-120-*-*-c-*-iso8859-*" screen)
     (try-face-font default "-*-*-*-r-*-*-*-120-*-*-*-*-iso8859-*" screen)
     ;; if we get to here we're screwed, and faces.c will fatal()...
     )
    ;;
    ;; If the "default" face didn't have both colors specified, then pick
    ;; some, taking into account the "reverseVideo" resource, as well as
    ;; whether one of the colors was specified.  
    ;;
    (let ((fg (face-foreground default screen))
	  (bg (face-background default screen)))
      (if (not (and fg bg))
	  (if (or (and fg (equal (downcase fg) "white"))
		  (and bg (equal (downcase bg) "black"))
		  (car (x-get-resource "reverseVideo" "ReverseVideo"
				       'boolean screen)))
	      (progn
		(or fg (set-face-foreground default "white" screen))
		(or bg (set-face-background default "black" screen)))
	    (or fg (set-face-foreground default "black" screen))
	    (or bg (set-face-background default "white" screen)))))
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
      )))


(defun x-complain-about-font (face)
  (if (symbolp face) (setq face (symbol-name face)))
  (princ (format "%s: couldn't deduce %s %s version of %S\n"
		 invocation-name
		 (if (string-match "\\`[aeiouAEIOU]" face) "an" "a")
		 face
		 (face-font 'default))
	 (function external-debugging-output)))

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
