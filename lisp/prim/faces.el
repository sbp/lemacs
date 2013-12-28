;; Lisp interface to the c "face" structure.
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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

(defun face-font-name (face &optional screen)
  "Returns the font name of the given face, or nil if it is unspecified."
  (let ((f (face-font face screen)))
    (and f (font-name f))))

(defun set-face-2 (face name value screen)
  ;; This maps set-face-attribute-internal over the face on the appropriate
  ;; screens, does some implicit type-conversion of the new value, and
  ;; maybe updates the nonscreen face data as well.

  (if (stringp value)
      (let ((screen (if (eq screen 't) nil screen)))
	(cond
	 ((eq name 'font) (setq value (make-font value screen)))
	 ((eq name 'foreground) (setq value (make-pixel value screen)))
	 ((eq name 'background) (setq value (make-pixel value screen)))
	 ((eq name 'background-pixmap) (setq value (make-pixmap value screen)))
	 )))

  (let ((inhibit-quit t))
    ;; If screen is nil, do it to all screens.
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (set-face-2 (face-name face) name value (car screens))
	    (setq screens (cdr screens)))
	  ;; set it in the default set too.
	  (set-face-attribute-internal (get-face face t) name value)
	  value)
      ;; otherwise screen is a screen, or t (the default, non-screen faces)
      (set-face-attribute-internal (get-face face screen) name value))

    ;; If the default, non-screen face doesn't have a value for this attribute
    ;; yet, use this one.  This is kind of a kludge, and I'm not sure it's the
    ;; right thing, but otherwise new screens tend not to get any attributes
    ;; set on them in, for example, Info and Webster.
    (let ((def (get-face face t)))
      (or (funcall (cond
		    ((eq name 'font) 'face-font)
		    ((eq name 'foreground) 'face-foreground)
		    ((eq name 'background) 'face-background)
		    ((eq name 'background-pixmap) 'face-background-pixmap)
		    ((eq name 'underline) 'face-underline-p)
		    (t "internal error in set-face-2"))
		   def)
	  (set-face-attribute-internal def name value)))
    )
  value)

;;; Prior to 19.9, set-face-font and friends were defsubsts that called
;;; set-face-1.  Well, things have changed, but that interface is retained
;;; for compatibility, because otherwise all callers of set-face-* would
;;; need to be recompiled.  They're not defsubsts any more, so code compiled
;;; in the new world will still work in the old.
(defun set-face-1 (face name value ignored-index screen)
  (set-face-2 face name value screen))

(defun read-face-name (prompt)
  (let (face)
    (while (= (length face) 0) ; nil or ""
      (setq face (completing-read prompt
				  (mapcar '(lambda (x) (list (symbol-name x)))
					  (list-faces))
				  nil t)))
    (intern face)))

(defun face-interactive (what &optional bool)
  (let* ((fn (intern (concat "face-" what)))
	 (face (read-face-name (format "Set %s of face: " what)))
	 (default (if (fboundp fn)
		      (or (funcall fn face (selected-screen))
			  (funcall fn 'default (selected-screen)))))
	 (value (if bool
		    (y-or-n-p (format "Should face %s be %s? "
				      (symbol-name face) bool))
		  (read-string (format "Set %s of face %s to: "
				       what (symbol-name face))
		   (cond ((fontp default) (font-name default))
			 ((pixelp default) (pixel-name default))
			 ((pixmapp default) (pixmap-file-name default))
			 (t default))))))
    (list face (if (equal value "") nil value))))


(defun set-face-font (face font &optional screen)
  "Change the font of the given face.
The font should be a string, the name string, the name of the font, or a
 font object as returned by `make-font'.
If the optional SCREEN argument is provided, this face will be changed only
 in that screen\; otherwise it will be changed in all screens."
  (interactive (face-interactive "font"))
  (set-face-2 face 'font font screen))

(defun set-face-foreground (face color &optional screen)
  "Change the foreground color of the given face.
The color should be a string, the name of a color, or a `pixel' object
 as returned by `make-pixel'.
If the optional SCREEN argument is provided, this face will be changed only
 in that screen; otherwise it will be changed in all screens."
  (interactive (face-interactive "foreground"))
  (set-face-2 face 'foreground color screen))

(defun set-face-background (face color &optional screen)
  "Change the background color of the given face.
The color should be a string, the name of a color, or a `pixel' object
 as returned by `make-pixel'.
If the optional SCREEN argument is provided, this face will be changed only
in that screen; otherwise it will be changed in all screens."
  (interactive (face-interactive "background"))
  (set-face-2 face 'background color screen))

(defun set-face-background-pixmap (face name &optional screen)
  "Change the background pixmap of the given face.  
The pixmap name should be a string, the name of a file of pixmap data.  
The directories listed in the x-bitmap-file-path variable will be searched.
The bitmap may also be a list of the form (width height data) where width and
 height are the size in pixels, and data is a string, containing the raw bits
 of the bitmap.  
If the optional SCREEN argument is provided, this face will be changed only
in that screen\; otherwise it will be changed in all screens."
  (interactive (face-interactive "background-pixmap"))
  (set-face-2 face 'background-pixmap name screen))

(defun set-face-underline-p (face underline-p &optional screen)
  "Change whether the given face is underlined.  
If the optional SCREEN argument is provided, this face will be changed only
in that screen\; otherwise it will be changed in all screens."
  (interactive (face-interactive "underline-p" "underlined"))
  (set-face-2 face 'underline underline-p screen))


(defun copy-face (old-face new-name &optional screen)
  "Defines and returns a new face which is a copy of an existing one,
or makes an already-existing face be exactly like another."
  (setq old-face (get-face old-face screen))
  (let* ((inhibit-quit t)
	 (new-face (or (find-face new-name screen)
		       (make-face new-name))))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (copy-face old-face new-name (car screens))
	    (setq screens (cdr screens)))
	  (copy-face old-face new-name t))
      (set-face-font new-face (face-font old-face screen) screen)
      (set-face-foreground new-face (face-foreground old-face screen) screen)
      (set-face-background new-face (face-background old-face screen) screen)
      (set-face-background-pixmap
       new-face (face-background-pixmap old-face screen) screen)
      (set-face-underline-p new-face (face-underline-p old-face screen)
			    screen))
    new-face))



(defun face-equal (face1 face2 &optional screen)
  "True if the given faces will display in the the same way."
  (setq face1 (get-face face1 screen)
	face2 (get-face face2 screen))
  (and (equal (face-foreground face1 screen) (face-foreground face2 screen))
       (equal (face-background face1 screen) (face-background face2 screen))
       (equal (face-font face1 screen) (face-font face2 screen))
       (equal (face-background-pixmap face1 screen)
	      (face-background-pixmap face2 screen))))

(defun face-differs-from-default-p (face &optional screen)
  "True if the given face will display differently from the default face.
A face is considered to be ``the same'' as the default face if it is 
actually specified in the same way (equivalent fonts, etc) or if it is 
fully unspecified, and thus will inherit the attributes of any face it 
is displayed on top of."
  (let ((default (get-face 'default screen)))
    (setq face (get-face face screen))
    (not (and (or (equal (face-foreground default screen)
			 (face-foreground face screen))
		  (null (face-foreground face screen)))
	      (or (equal (face-background default screen)
			 (face-background face screen))
		  (null (face-background face screen)))
	      (or (equal (face-font default screen) (face-font face screen))
		  (null (face-font face screen)))
	      (or (equal (face-background-pixmap default screen)
			 (face-background-pixmap face screen))
		  (null (face-background-pixmap face screen)))
	      (equal (face-underline-p default screen)
		     (face-underline-p face screen))
	      ))))


(defun invert-face (face &optional screen)
  "Swap the foreground and background colors of the given face.
If the face doesn't specify both foreground and background, then
its foreground and background are set to the background and
foreground of the default face."
  (interactive (list (read-face-name (gettext "Invert face: "))))
  (setq face (get-face face screen))
  (let ((fg (or (face-foreground face screen)
		(face-foreground 'default screen)))
	(bg (or (face-background face screen)
		(face-background 'default screen))))
    (set-face-foreground face bg screen)
    (set-face-background face fg screen))
  face)


(defun set-default-font (font)
  "Sets the font used for normal text and the modeline to FONT in all screens.
For finer-grained control, use set-face-font."
  (interactive (list (read-string (gettext "Set default font: ")
				  (font-name (face-font 'default (selected-screen))))))
  (set-face-font 'default font)
  (set-face-font 'modeline font))

(defun try-face-font (face font &optional screen)
  "Like set-face-font, but returns nil on failure instead of an error."
  (if (stringp font) (setq font (try-font font screen)))
  (set-face-font face font screen))


;;; make-screen-initial-faces is responsible for initializing the 
;;; newly-created faces on a newly-created screen.  It does this by
;;; calling out to window-system-specific code.
;;;
;;; It is called from init_screen_faces() called from Fx_create_screen().
;;;
;;; This had better not signal an error.  The screen is in an intermediate
;;; state where signalling an error or entering the debugger would likely
;;; result in a crash.

(defun make-screen-initial-faces (screen)
  (let ((default (get-face 'default screen))
	(modeline (get-face 'modeline screen)))
    
    (cond ((eq 'x (screenp screen))
	   ;; First read the resource database for the faces on this screen...
	   (let ((faces (list-faces)))
	     (x-resource-face (car faces) screen)
	     (setq faces (cdr faces)))
	   ;; Then ensure that the default and modeline faces are sensible.
	   (x-initialize-screen-faces screen)))
    ;;
    ;; If the "default" face and the "modeline" face would display the same
    ;; (meaning they have the same values, or the modeline values are
    ;; unspecified and would thus inherit the "default" values) then change
    ;; the modeline to be inverse-video w.r.t. the default face.  If the
    ;; user really wants the modeline to look just like buffer text, which
    ;; I can't imagine anyone actually wanting, they can do it by calling
    ;; set-face-{fore,back}ground on 'modeline from their .emacs file.
    ;;
    (or (face-differs-from-default-p modeline screen)
	(progn
	  (set-face-foreground modeline (face-background default screen)
			       screen)
	  (set-face-background modeline (face-foreground default screen)
			       screen)))

    ;; Now make sure the modeline, left, and right faces are fully qualified.
    ;; The C code requires this.
    (let ((rest '(modeline left-margin right-margin))
	  face)
      (while rest
	(setq face (get-face (car rest) screen))
	(if (and (not (face-font face screen)) (face-font default screen))
	    (set-face-font face (face-font default screen) screen))
	(if (and (not (face-background face screen))
		 (face-background default screen))
	    (set-face-background face (face-background default screen) screen))
	(if (and (not (face-foreground face screen))
		 (face-foreground default screen))
	    (set-face-foreground face (face-foreground default screen) screen))
	(setq rest (cdr rest))))
    ))


;;; Make the builtin faces; the C code knows these as faces 0 through 4
;;; respectively, so they must be the first five faces made.

(if (find-face 'default)
    nil
  (make-face 'default)
  (make-face 'modeline)
  (make-face 'highlight)
  (make-face 'left-margin)
  (make-face 'right-margin)
  ;;
  ;; These aren't really special in any way, but they're nice to have around.
  ;; The X-specific code is clever at them.
  ;;
  (make-face 'bold)
  (make-face 'italic)
  (make-face 'bold-italic))
