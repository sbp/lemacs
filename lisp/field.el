;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Copyright (c) 1988 Microelectronics and Computer Technology Corporation
;;;
;;; File: 	field.el
;;; Created: 	Sun Jan 10 10:16:17 1988
;;; Author: 	Frank Halasz (halasz@babyhalasz)
;;;
;;; Description: Implementation of fields which are markers that span regions.
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  A field is like a marker but it defines a region rather than a
;;;  point.  Like a marker, a field is aasocated with a buffer.
;;;  The field mechanism uses the marker mechanism in the
;;;  sense that its start and end points are maintained as markers
;;;  updated in the usual way as the buffer changes.
;;;  A field can be protected or unprotected.  If it is protected,
;;;  no modifications can be made that affect the field in its buffer 
;;;  (see insdel.c and field.c).  Finally, an arbitrary data object
;;;  can be associated with a field.

;;;  Fields are currently implemented using Lisp vectors.  This is to
;;;  avoid having to make a new lisp type and deal with all the
;;;  garbage collection issues, etc.  At some later point, it may make
;;;  sense to make them into a lisp data type of their own.

(defvar buffer-fields nil)

(defun add-field (start end &optional buffer protected data)
  "Add a new field to BUFFER that starts at START (inclusive)and ends at
END (exclusive).
START and END can be character numbers or markers.
If PROTECTED is non-nil, then the field
will be protected from insertion and deletion.
ALIST (optional) is the initial value for the field's alist.
Returns the field object (which is actually a vector)."

  ;; nil buffer means current buffer
  (if (null buffer)
      (setq buffer (current-buffer)))

  ;; transform marker args (if any) in character positions
  (if (markerp start)
      (setq start (marker-position start)))
  (if (markerp end)
      (setq end (marker-position end)))

  ;; Reverse start and end if necessary
  (if (> start end)
      (let (temp)
	(setq temp start
	      start end
	      end temp)))
   
  ;; Make the field, fill in the slots, hook the field into buffer's
  ;; field chain.
  (let ((field (vector 'field buffer
		       ;; markers stick to the preceeding character!!!!!
		       (set-marker (make-marker) (1+ start) buffer)
		       (set-marker (make-marker) (+ 0 end) buffer)
		       protected
		       data)))
    (save-excursion
      (set-buffer buffer)
      (setq buffer-fields (cons field buffer-fields)))
  
    field))

(defun region-field ()
  (add-field (region-beginning) (region-end)))

(defun protected-region-field ()
  (add-field (region-beginning) (region-end) (current-buffer) t nil))

(defmacro field-buffer (field)
  "Return the buffer that FIELD is associated with."
  (list 'aref field 1))
  
(defmacro field-start (field)
  "Return the character number of the current starting point of FIELD"
  ;; markers stick to the preceeding character
  (list '1- (list 'aref field 2)))

(defmacro field-end (field)
  "Return the character number of the current end point of FIELD"
  (list 'aref field 3))

(defmacro field-protected (field)
  "Return t is FIELD is protected, nil otherwise."
  (list 'aref field 4))

(defmacro field-alist (field)
  "Return the data associated with FIELD."
  (list 'aref field 5))

(defmacro set-field-protected (field protected)
  "Set the protection status of FIELD to PROTECTED."
  (list 'aset field 4 protected))
  
(defmacro set-field-alist (field alist)
  "Set the data associate with FIELD to be ALIST."
  (list 'aset field 5 alist))

(defun delete-field (field)
  "Delete field FIELD, in whichever buffer it belongs to."
  (save-excursion
    (set-buffer (field-buffer field))
    (setq buffer-field-list
	  (delq field buffer-field-list))))
    
