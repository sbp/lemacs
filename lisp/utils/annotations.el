;; Marginal Annotations for Era (Emacs Rewritten Again)
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.
;;
;; Created: 10-Oct-93, Chuck Thompson <cthomp@cs.uiuc.edu>
;; Last modified: 23-Dec-93

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

;;
;; The annotations are implemented on top of extents.  The extent data
;; field of an extent being used as an annotation is a list of size 3.
;;	('annotation <data> <action>)
;;

(defvar make-annotation-hook nil
  "*Function or functions to run immediately after creating an annotation.")

(defvar before-delete-annotation-hook nil
  "*Function or functions to run immediately before deleting an annotation.")

(defvar after-delete-annotation-hook nil
  "*Function or functions to run immediately after deleting an annotation.")

(defvar annotation-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'annotation-local-map)
    (define-key map 'button1 'annotation-activate-function)
    map)
  "Keymap used to activate annotations.")

;;
;; When the mouse is pressed and released over an annotation glyph
;; this will run the annotation action passing a single arg, the value
;; of the annotation data field.
;;
(defun annotation-activate-function (event)
  (interactive "e")
  (let ((p (point))
	(b (current-buffer))
	(extent (event-glyph event))
	(mouse-down t))
    (while mouse-down
      (setq event (next-event event))
      (if (button-release-event-p event)
	  (setq mouse-down nil)))
    (if (eq extent (event-glyph event))
	(if (annotation-action extent)
	    (funcall (annotation-action extent) (annotation-data extent))))))

(defun make-annotation (glyph &optional pos layout buffer)
  "Create a marginal annotation with symbol GLYPH at position POS.
GLYPH may be either a pixmap object or a string.  Use layout policy
LAYOUT and place the annotation in buffer BUFFER.  If POS is nil, point is
used.  If LAYOUT is nil, `whitespace' is used.  If BUFFER is nil, the
current buffer is used."
  (let ((new-annotation))
    ;; get the buffer to add the annotation at
    (if (not buffer)
	(setq buffer (current-buffer))
      (setq buffer (get-buffer buffer)))
    ;; get the position to put it at
    (if (not pos)
	(save-excursion
	  (set-buffer buffer)
	  (setq pos (point))))
    ;; make sure it gets some layout policy
    (if (not layout)
	(setq layout 'whitespace))

    ;; create the actual annotation
    (setq new-annotation (make-extent pos pos buffer))
    (detach-extent new-annotation)
    (set-extent-endpoints new-annotation pos pos)
    (set-extent-begin-glyph new-annotation glyph layout)
    (set-extent-property new-annotation 'annotation (list nil nil))
    (set-extent-property new-annotation 'end-closed t)
    (set-extent-property new-annotation 'duplicable t)
    (set-extent-property new-annotation 'keymap annotation-local-map)
    (run-hook-with-args 'make-annotation-hook new-annotation)
    new-annotation))

(fset 'make-graphic-annotation 'make-annotation)
(make-obsolete 'make-graphic-annotation 'make-annotation)

(defun delete-annotation (annotation)
  "Remove ANNOTATION from its buffer.  This does not modify the buffer text."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (progn
      (run-hook-with-args 'before-delete-annotation-hook annotation)
      (delete-extent annotation)
      (run-hooks 'after-delete-annotation-hook))))

(defun annotationp (annotation)
  "T if OBJECT is an annotation"
  (and (extentp annotation)
       (not (null (extent-property annotation 'annotation)))))

(defun annotation-visible (annotation)
  "T if there is enough available space to display ANNOTATION."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (not (extent-property annotation 'glyph-invisible))))

(defun annotation-at (&optional pos buffer)
  "Find annotation at POS in BUFFER.  BUFFER defaults to the current buffer.
POS defaults to point in BUFFER"
  (let (annotation)
    ;; get the buffer to check for an annotation
    (if (not buffer)
	(setq buffer (current-buffer))
      (setq buffer (get-buffer buffer)))
    ;; get the point to check for an annotation
    (if (not pos)
	(save-excursion
	  (set-buffer buffer)
	  (setq pos (point))))
    (setq annotation (extent-at pos buffer nil nil))
    (if (annotationp annotation)
	annotation
      nil)))

(defun annotation-layout (annotation)
  "Return the layout policy of annotation ANNOTATION.  The layout policy
is set using `set-annotation-layout'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (extent-layout annotation)))

(defun set-annotation-layout (annotation layout)
  "Set the layout policy of ANNOTATION to LAYOUT.  The function
`annotation-layout' returns the current layout policy."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (set-extent-layout annotation layout)))

(defun annotation-type (annotation)
  "Return the display type of the annotation ANNOTATION.  The type will
be one of the following symbols:

	pixmap
	bitmap
	string
	nil	(the object is not an annotation)"
  (if (not (annotationp annotation))
      nil
    (let ((glyph (extent-glyph annotation)))
      (if (stringp (extent-glyph annotation))
	  'stringp
	(if (not (pixmapp glyph))
	    (error "%s is a corrupt annotation" annotation)
	  (if (> (pixmap-depth (extent-glyph annotation)) 0)
	      'pixmap
	    'bitmap))))))

(defun annotation-width (annotation)
  "Return the width of the annotation ANNOTATION in pixels."  
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (cond ((not (eq 'string (annotation-type annotation)))
	   (pixmap-width (extent-glyph annotation)))
	  (t 0))))

(defun annotation-glyph (annotation)
  "If ANNOTATION is of type `string' return the string.  Otherwise, return
the bitmap or pixmap object of the glyph representing ANNOTATION.
The glyph is set using `set-annotation-glyph'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (extent-glyph annotation)))

(defun set-annotation-glyph (annotation glyph &optional layout)
  "Set the representation of ANNOTATION to GLYPH.  GLYPH may be either
a string or a bitmap/pixmap object.  If LAYOUT is non-nil set the layout
policy of the annotation to LAYOUT.  The function `annotation-glyph'
returns the current glyph."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (progn
      (if (not layout)
	  (setq layout (extent-layout annotation)))
      (set-extent-begin-glyph annotation glyph layout)
      (annotation-glyph annotation))))

(fset 'annotation-graphic 'annotation-glyph)
(fset 'set-annotation-graphic 'set-annotation-glyph)
(make-obsolete 'annotation-graphic 'annotation-glyph)
(make-obsolete 'set-annotation-graphic 'set-annotation-glyph)
  
(defun annotation-data (annotation)
  "Return the data associated with annotation ANNOTATION.  The data is
set using `set-annotation-data'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (nth 0 (extent-property annotation 'annotation))))

(defun set-annotation-data (annotation data)
  "Set the data field of ANNOTATION to DATA.
The function `annotation-data' returns the current data."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (set-extent-property annotation 'annotation
     (list data (nth 1 (extent-property annotation 'annotation))))))

(defun annotation-action (annotation)
  "Return the action associated with annotation ANNOTATION.  The action
is set using `set-annotation-action'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (nth 1 (extent-property annotation 'annotation))))

(defun set-annotation-action (annotation action)
  "Set the action field of ANNOTATION to ACTION.  The function
`annotation-action' returns the current action."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (set-extent-property annotation 'annotation
			 (list (nth 1 (extent-property annotation 'annotation))
			       action))))

(defun annotation-face (annotation)
  "Return the face associated with annotation ANNOTATION.  The face is
set using `set-annotation-face'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (extent-face annotation)))

(defun set-annotation-face (annotation face)
  "Set the face associated with annotation ANNOTATION to FACE.  The function
`annotation-face' returns the current face."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (set-extent-face annotation face)))

(defun annotations-in-region (start end buffer)
  "Return all annotations in BUFFER which are between START and END
inclusively."
  (save-excursion
    (set-buffer buffer)

    (if (< start (point-min))
      (error "<start> not in range of buffer"))
    (if (> end (point-max))
      (error "<end> not in range of buffer"))

    (let (note-list)
      (map-extents
       (function (lambda (extent dummy)
		   (progn
		     (if (annotationp extent)
			 (setq note-list (cons extent note-list)))
		     nil)))
       buffer start end nil 't)
      note-list)))

(defun annotations-at (&optional pos buffer)
  "Return a list of all annotations at POS in BUFFER.  If BUFFER is nil,
the current buffer is used.  If POS is nil, point is used."
  (if (not buffer)
      (setq buffer (current-buffer)))
  (if (not pos)
      (save-excursion
	(set-buffer buffer)
	(setq pos (point))))

  (annotations-in-region pos pos buffer)
)

(defun annotation-list (&optional buffer)
  "Return a list of all annotations in BUFFER.  If BUFFER is nil, the
current buffer is used."
  (if (not buffer)
    (setq buffer (current-buffer)))

  (save-excursion
    (set-buffer buffer)
    (annotations-in-region (point-min) (point-max) buffer)))

(defun all-annotations ()
  "Return a list of all annotations in existence."
  (let ((b (buffer-list))
	result)
    (while b
      (setq result (nconc result (annotation-list (car b))))
      (setq b (cdr b)))
    result))


(provide 'annotations)
