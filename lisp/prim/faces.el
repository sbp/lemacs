;; Lisp interface to the c "face" structure.
;; Copyright (C) 1992 Free Software Foundation, Inc.

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

(defsubst facep (x)
  (and (vectorp x) (= (length x) 8) (eq (aref x 0) 'face)))

(defmacro check-face (face)
  (` (while (not (facep (, face)))
       (setq (, face) (signal 'wrong-type-argument (list 'facep (, face)))))))


(defvar global-face-data nil "do not use this")
(defvar face-id-tick 0 "don't even think of using this")

(defun list-faces ()
  "Returns a list of the names of all of the defined faces."
  (mapcar 'car global-face-data))

(defun find-face (name &optional screen)
  "Retrieve the face of the given name.
If NAME is a symbol and SCREEN is provided, the face is looked up on
that screen; otherwise, the selected screen is used.
If there is no such face, returns nil.
If SCREEN is the symbol t, then the global, non-screen face is returned.
If NAME is already a face, it is simply returned."
  (if (and (eq screen t) (not (symbolp name)))
      (setq name (face-name name)))
  (if (symbolp name)
      (cdr (assq name
		 (if (eq screen t)
		     global-face-data
		   (screen-face-alist (or screen (selected-screen))))))
    (check-face name)
    name))

(defun get-face (name &optional screen)
  "Retrieve the face of the given name.
If NAME is a symbol and SCREEN is provided, the face is looked up on
that screen; otherwise, the selected screen is used.
If there is no such face, an error is signalled.  See also `find-face'.
If SCREEN is the symbol t, then the global, non-screen face is returned.
If NAME is already a face, it is simply returned."
  (or (find-face name screen)
      (check-face name)))

(defsubst face-name (face)
  "Returns the name of the given face."
  (aref (get-face face) 1))

(defsubst face-id (face)
  "Returns the internal ID number of the given face."
  (aref (get-face face) 2))

(defsubst face-font (face &optional screen)
  "Returns the font name of the given face, or nil if it is unspecified."
  (aref (get-face face screen) 3))

(defsubst face-foreground (face &optional screen)
  "Returns the foreground color name of the given face, or nil if unspecified."
  (aref (get-face face screen) 4))

(defsubst face-background (face &optional screen)
  "Returns the background color name of the given face, or nil if unspecified."
  (aref (get-face face screen) 5))

(defsubst face-background-pixmap (face &optional screen)
 "Returns the background pixmap name of the given face, or nil if unspecified."
 (aref (get-face face screen) 6))

(defsubst face-underline-p (face &optional screen)
 "Returns whether the given face is underlined."
 (aref (get-face face screen) 7))


(defun set-face-1 (face name value index screen)
  (let ((inhibit-quit t))
    (if (null screen)
	(let ((screens (screen-list)))
	  (while screens
	    (set-face-1 (face-name face) name value index (car screens))
	    (setq screens (cdr screens)))
	  (aset (get-face (if (symbolp face) face (face-name face)) t)
		index value)
	  value)
      (or (eq screen t)
	  (set-face-attribute-internal (face-id face) name value screen))
      (aset (get-face face screen) index value))))


(defun read-face-name (prompt)
  (let (face)
    (while (= (length face) 0)
      (setq face (completing-read prompt
				  (mapcar '(lambda (x) (list (symbol-name x)))
					  (list-faces))
				  nil t)))
    (intern face)))

(defun face-interactive (what &optional bool)
  (let* ((fn (intern (concat "face-" what)))
	 (prompt (concat "Set " what " of face"))
	 (face (read-face-name (concat prompt ": ")))
	 (default (if (fboundp fn)
		      (funcall fn face (selected-screen))))
	 (value (if bool
		    (y-or-n-p (concat "Should face " (symbol-name face)
				      " be " bool "? "))
		  (read-string (concat prompt " " (symbol-name face) " to: ")
			       default))))
    (list face (if (equal value "") nil value))))


(defsubst set-face-font (face font &optional screen)
  "Change the font of the given face.  The font should be a string, the name
string, the name of the font.  If the optional SCREEN argument is provided, 
this face will be changed only in that screen\; otherwise it will be changed
in all screens."
  (interactive (face-interactive "font"))
  (set-face-1 face 'font font 3 screen))

(defsubst set-face-foreground (face color &optional screen)
  "Change the foreground color of the given face.  The color should be a 
string, the name of a color.  If the optional SCREEN argument is provided, 
this face will be changed only in that screen; otherwise it will be changed 
in all screens."
  (interactive (face-interactive "foreground"))
  (set-face-1 face 'foreground color 4 screen))

(defsubst set-face-background (face color &optional screen)
  "Change the background color of the given face.  The color should be a 
string, the name of a color.  If the optional SCREEN argument is provided, 
this face will be changed only in that screen; otherwise it will be changed 
in all screens."
  (interactive (face-interactive "background"))
  (set-face-1 face 'background color 5 screen))

(defsubst set-face-background-pixmap (face name &optional screen)
  "Change the background pixmap of the given face.  The pixmap name should be
a string, the name of a file of pixmap data.  The directories listed in the
x-bitmap-file-path variable will be searched.  The bitmap may also be a list
of the form (width height data) where width and height are the size in pixels,
and data is a string, containing the raw bits of the bitmap.  
If the optional SCREEN argument is provided, this face will be changed only
in that screen\; otherwise it will be changed in all screens."
  (interactive (face-interactive "background-pixmap"))
  (set-face-1 face 'background-pixmap name 6 screen))

(defsubst set-face-underline-p (face underline-p &optional screen)
  "Change whether the given face is underlined.  
If the optional SCREEN argument is provided, this face will be changed only
in that screen\; otherwise it will be changed in all screens."
  (interactive (face-interactive "underline-p" "underlined"))
  (set-face-1 face 'underline underline-p 7 screen))


(defun make-face (name)
  "Defines and returns a new FACE on all screens.  
You can modify the font, color, etc of this face with the set-face- functions."
  (let ((face (make-vector 8 nil)))
    (aset face 0 'face)
    (aset face 1 name)
    (let* ((screens (screen-list))
	   (inhibit-quit t)
	   (id face-id-tick))
      (make-face-internal name face id) ; may error
      (setq face-id-tick (1+ face-id-tick)) ; now it's safe
      (while screens
	(aset (get-face name (car screens)) 2 id)
	(setq screens (cdr screens)))
      (setq face (copy-sequence face))
      (aset face 2 id)
      (setq global-face-data (cons (cons name face) global-face-data)))
    ;; when making a face after screens already exist
    (if (eq window-system 'x)
	(x-resource-face face))
    face))

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

(defun set-extent-face (extent face)
  "Make the given EXTENT have the graphic attributes specified by FACE."
  (set-extent-attribute extent (face-id face)))


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
  "True if the given face will display the same as the default face.
This may mean either that the face is specified in the same way as the 
default face, or that the face is fully unspecified, and thus will
inherit the attributes of any face it is displayed on top of."
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
  (interactive (list (read-face-name "Invert face: ")))
  (setq face (get-face face screen))
  (let ((fg (face-foreground face screen))
	(bg (face-background face screen)))
    (if (or fg bg)
	(progn
	  (set-face-foreground face bg screen)
	  (set-face-background face fg screen))
      (set-face-foreground face (face-background 'default screen) screen)
      (set-face-background face (face-foreground 'default screen) screen)))
  face)


(defun try-face-font (face font &optional screen)
  "Like set-face-font, but returns nil on failure instead of an error."
  (condition-case ()
      (set-face-font face font screen)
    (error nil)))


(defun set-default-font (font)
  "Sets the font used for normal text and the modeline to FONT in all screens.
For finer-grained control, use set-face-font."
  (interactive (list (read-string "Set default font: "
				  (face-font 'default (selected-screen)))))
  (set-face-font 'default font)
  (set-face-font 'modeline font))


;;; This is called from make-screen (well, x-create-screen) just before
;;; the create-screen-hook is run.  This is responsible for making sure
;;; that the "default" and "modeline" faces for this screen have enough
;;; attributes specified for emacs to be able to display anything on it.
;;; This had better not signal an error.

(defun make-screen-initial-faces ()
  (let* ((faces (copy-alist global-face-data))
	 (screen (selected-screen))
	 (rest faces)
	 default modeline)
    (set-screen-face-alist screen faces)
    (while rest
      (setcdr (car rest) (copy-sequence (cdr (car rest))))
      (if (eq window-system 'x)
	  (x-resource-face (cdr (car rest)) screen t))
      (setq rest (cdr rest)))

    (setq default (get-face 'default screen)
	  modeline (get-face 'modeline screen))
	
    (if (eq window-system 'x)
	(x-initialize-screen-faces screen))
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

    ;; now make sure the modeline face is fully qualified.
    (if (and (not (face-font modeline screen)) (face-font default screen))
	(set-face-font modeline (face-font default screen) screen))
    (if (and (not (face-background modeline screen))
	     (face-background default screen))
	(set-face-background modeline (face-background default screen) screen))
    (if (and (not (face-foreground modeline screen))
	     (face-foreground default screen))
	(set-face-foreground modeline (face-foreground default screen) screen))
    ))


;;; Make the builtin faces; the C code knows these as faces 0, 1, and 2,
;;; respectively, so they must be the first three faces made.

(if (find-face 'default)
    nil
  (make-face 'default)
  (make-face 'modeline)
  (make-face 'highlight)
  ;;
  ;; These aren't really special in any way, but they're nice to have around.
  ;; The X-specific code is clever at them.
  ;;
  (make-face 'bold)
  (make-face 'italic)
  (make-face 'bold-italic))
