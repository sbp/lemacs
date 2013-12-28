;;; text-props.el --- implements properties of characters.
;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

;;; This is a nearly complete implementation of the FSF19 text properties API,
;;; except that this code currently only works on buffers, not strings.
;;; Please let me know if you notice any differences in behavior between
;;; this implementation and the FSF implementation.
;;;
;;; However, keep in mind that this interface has been implemented because it
;;; is useful.  Compatibility with code written for FSF19 is a secondary goal
;;; to having a clean and useful interface interface.
;;;
;;; The cruftier parts of the FSF API, such as the special handling of
;;; properties like `mouse-face', `front-sticky', and other properties whose
;;; value is a list of names of *other* properties set at this position, are
;;; not implemented.  The reason for this is that if you feel you need that
;;; kind of functionality, it's a good hint that you should be using extents
;;; instead of text properties.
;;;
;;; When should I use Text Properties, and when should I use Extents?
;;; ==================================================================
;;;
;;; If you are putting a `button' or `hyperlink' of some kind into a buffer,
;;; the most natural interface is one which deals with properties of regions
;;; with explicit endpoints that behave more-or-less like markers.  That is
;;; what `make-extent', `extent-at', and `extent-property' are for.
;;;
;;; If you are dealing with styles of text, where things do not have explicit
;;; endpoints (as is done in font-lock.el and shell-font.el) or if you want to
;;; partition a buffer (that is, change some attribute of a range from one
;;; value to another without disturbing the properties outside of that range)
;;; then an interface that deals with properties of characters may be most
;;; natural.  
;;;
;;; Another way of thinking of it is, do you care where the endpoints of the
;;; region are?  If you do, then you should use extents.  If it's ok for the
;;; region to become divided, and for two regions with identical properties to
;;; be merged into one region, then you might want to use text properties.
;;;
;;; Some applications want the attributes they add to be copied by the killing
;;; and yanking commands, and some do not.  This is orthogonal to whether text
;;; properties or extents are used.  Remember that text properties are
;;; implemented in terms of extents, so anything you can do with one you can
;;; do with the other.  It's just a matter of which way of creating and
;;; managing them is most appropriate to your application.
;;;
;;; Implementation details:
;;; =======================
;;;
;;; This package uses extents with a non-nil 'text-prop property.  It assumes
;;; free reign over the endpoints of any extent with that property.  It will
;;; not alter any extent which does not have that property.
;;;
;;; Right now, the text-property functions create one extent for each distinct
;;; property; that is, if a range of text has two text-properties on it, there
;;; will be two extents.  As the set of text-properties is going to be small,
;;; this is probably not a big deal.  It would be possible to share extents.
;;;
;;; One tricky bit is that undo/kill/yank must be made to not fragment things:
;;; these extents must not be allowed to overlap.  We accomplish this by using
;;; a custom `paste-function' property on the extents.
;;;
;;; shell-font.el and font-lock.el could put-text-property to attach fonts to
;;; the buffer.  However, what these packages are interested in is the
;;; efficient extent partitioning behavior which this code exhibits, not the
;;; duplicability aspect of it.  In fact, either of these packages could be be
;;; implemented by creating a one-character non-expandable extent for each
;;; character in the buffer, except that that would be extremely wasteful of
;;; memory.  (Redisplay performance would be fine, however.)
;;;
;;; If these packages were to use put-text-property to make the extents, then
;;; when one copied text from a shell buffer or a font-locked source buffer
;;; and pasted it somewhere else (a sendmail buffer, or a buffer not in
;;; font-lock mode) then the fonts would follow, and there's no easy way to
;;; get rid of them (other than pounding out a call to put-text-property by
;;; hand.)  This is annoying.  Maybe it wouldn't be so annoying if there was a
;;; more general set of commands for handling styles of text (in fact, if
;;; there were such a thing, copying the fonts would probably be exactly what
;;; one wanted) but we aren't there yet.  So these packages use the interface
;;; of `put-nonduplicable-text-property' which is the same, except that it
;;; doesn't make duplicable extents.
;;;
;;; `put-text-property' and `put-nonduplicable-text-property' don't get along:
;;; they will interfere with each other, reusing each others' extents without
;;; checking that the "duplicableness" is correct.  This is a bug, but it's
;;; one that I don't care enough to fix this right now.


;;; Code:


;; The following functions were ported to C for speed; the overhead of doing
;; this many full lisp function calls was not small.

;;;;###autoload
;(defun put-text-property (start end prop value &optional buffer)
;  "Adds the given property/value to all characters in the specified region.
;The property is conceptually attached to the characters rather than the
;region.  The properties are copied when the characters are copied/pasted."
;  (put-text-property-1 start end prop value buffer t)
;  prop)
;
;;;;###autoload
;(defun put-nonduplicable-text-property (start end prop value &optional buffer)
;  "Adds the given property/value to all characters in the specified region.
;The property is conceptually attached to the characters rather than the
;region, however the properties will not be copied the characters are copied."
;  (put-text-property-1 start end prop value buffer nil)
;  prop)
;
;(defun put-text-property-1 (start end prop value buffer duplicable)
;  ;; returns whether any property of a character was changed
;  (if (= start end)
;      nil
;    (save-excursion
;      (and buffer (set-buffer buffer))
;      (let ((the-extent nil)
;	    (changed nil))
;	;; prop, value, the-extent, start, end, and changed are of dynamic
;	;; scope.  changed and the-extent are assigned.
;	(map-extents (function put-text-property-mapper) nil
;		     (max 1 (1- start))
;		     (min (buffer-size) (1+ end)))
;
;	;; If we made it through the loop without reusing an extent
;	;; (and we want there to be one) make it now.
;	(cond ((and value (not the-extent))
;	       (setq the-extent (make-extent start end))
;	       (set-extent-property the-extent 'text-prop prop)
;	       (set-extent-property the-extent prop value)
;	       (setq changed t)
;	       (cond (duplicable
;		      (set-extent-property the-extent 'duplicable t)
;		      (set-extent-property the-extent 'paste-function
;					   'text-prop-extent-paste-function)))
;	       ))
;	changed))))
;
;(defun put-text-property-mapper (e ignore)
;  ;; prop, value, the-extent, start, end, and changed are of dynamic scope.
;  ;; changed and the-extent are assigned.
;  (let ((e-start (extent-start-position e))
;	(e-end (extent-end-position e))
;	(e-val (extent-property e prop)))
;    (cond ((or (null e-val)
;	       (null (extent-property e 'text-prop)))
;	   ;; not one of ours, or not for this prop
;	   nil)
;
;	  ((and value
;		(not the-extent)
;		(eq value e-val))
;	   ;; we want there to be an extent here at the end, and we haven't
;	   ;; picked one yet, so use this one.  Extend it as necessary.
;	   ;; We only reuse an extent which has an EQ value for the prop in
;	   ;; question to avoid side-effecting the kill ring (that is, we
;	   ;; never change the property on an extent after it has been
;	   ;; created.)
;	   (cond
;	    ((or (/= e-start start) (/= e-end end))
;	     (set-extent-endpoints e (min e-start start) (max e-end end))
;	     (setq changed t)))
;	   (setq the-extent e))
;
;	  ;; Even if we're adding a prop, at this point, we want all other
;	  ;; extents of this prop to go away (as now they overlap.)
;	  ;; So the theory here is that, when we are adding a prop to a
;	  ;; region that has multiple (disjoint) occurences of that prop
;	  ;; in it already, we pick one of those and extend it, and remove
;	  ;; the others.
;
;	  ((eq e the-extent)
;	   ;; just in case map-extents hits it again (does that happen?)
;	   nil)
;
;	  ((and (>= e-start start)
;		(<= e-end end))
;	   ;; extent is contained in region; remove it.  Don't destroy or
;	   ;; modify it, because we don't want to change the attributes
;	   ;; pointed to by the duplicates in the kill ring.
;	   (setq changed t)
;	   (detach-extent e))
;
;	  ((and the-extent
;		(eq value e-val)
;		(<= e-start end)
;		(>= e-end start))
;	   ;; this extent overlaps, and has the same prop/value as the
;	   ;; extent we've decided to reuse, so we can remove this existing
;	   ;; extent as well (the whole thing, even the part outside of the
;	   ;; region) and extend the-extent to cover it, resulting in the
;	   ;; minimum number of extents in the buffer.
;	   (cond
;	    ((and (/= (extent-start-position the-extent) e-start)
;		  (/= (extent-end-position the-extent) e-end))
;	     (set-extent-endpoints the-extent
;				   (min (extent-start-position the-extent)
;					e-start)
;				   (max (extent-end-position the-extent)
;					e-end))
;	     (setq changed t)))
;	   (detach-extent e))
;
;	  ((<= (extent-end-position e) end)
;	   ;; extent begins before start but ends before end,
;	   ;; so we can just decrease its end position.
;	   (if (and (= (extent-start-position e) e-start)
;		    (= (extent-end-position e) start))
;	       nil
;	     (set-extent-endpoints e e-start start)
;	     (setq changed t)))
;
;	  ((>= (extent-start-position e) start)
;	   ;; extent ends after end but begins after start,
;	   ;; so we can just increase its start position.
;	   (if (and (= (extent-start-position e) end)
;		    (= (extent-start-position e) e-end))
;	       nil
;	     (set-extent-endpoints e end e-end)
;	     (setq changed t)))
;
;	  (t
;	   ;; Otherwise, the extent straddles the region.
;	   ;; We need to split it.
;	   (set-extent-endpoints e e-start start)
;	   (setq e (copy-extent e))
;	   (set-extent-endpoints e end e-end)
;	   (setq changed t))))
;  ;; return nil to continue mapping over region.
;  nil)
;
;
;(defun text-prop-extent-paste-function (extent from to)
;  ;; Whenever a text-prop extent is pasted into a buffer (via `yank' or
;  ;; `insert' or whatever) we attach the properties to the buffer by calling
;  ;; `put-text-property' instead of by simply alowing the extent to be copied
;  ;; or re-attached.  Then we return nil, telling the C code not to attach
;  ;; it again. By handing the insertion hackery in this way, we make kill/yank
;  ;; behave consistently iwth put-text-property and not fragment the extents
;  ;; (since text-prop extents must partition, not overlap.)
;  (let* ((prop (or (extent-property extent 'text-prop)
;		   (error "internal error: no text-prop on %S" extent)))
;	 (val (or (extent-property extent prop)
;		  (error "internal error: no text-prop %S on %S"
;			 prop extent))))
;    (put-text-property from to prop val)
;    nil))
;		     
;;;;###autoload
;(defun add-text-properties (start end props &optional buffer)
;  "Add properties to the characters from START to END.
;The third argument PROPS is a property list specifying the property values
;to add.  The optional fourth argument, OBJECT, is the buffer containing the
;text.  Returns t if any property was changed, nil otherwise."
;  (let ((changed nil))
;    (while props
;      (setq changed
;	    (or (put-text-property-1 start end (car props) (car (cdr props))
;				     buffer t)
;		changed))
;      (setq props (cdr (cdr props))))
;    changed))
;
;;;;###autoload
;(defun remove-text-properties (start end props &optional buffer)
;  "Remove the given properties from all characters in the specified region.
;PROPS should be a plist, but the values in that plist are ignored (treated
;as nil.)  Returns t if any property was changed, nil otherwise."
;  (let ((changed nil))
;    (while props
;      (setq changed
;	    (or (put-text-property-1 start end (car props) nil buffer t)
;		changed))
;      (setq props (cdr (cdr props))))
;    changed))
;
;;;;###autoload
;(defun set-text-properties (start end props &optional buffer)
;  "Completely replace properties of text from START to END.
;This function is not implemented because it is fundamentally unclean.
;If an application were to set all properties on a region, it would interfere
;with other applications which had also stored properties there.  Applications
;should only remove properties which they created, or explicitly manipulate.
;Use `remove-text-properties' instead."
;  (error "ideological error: set-text-properties"))


;;; The following functions can probably stay in lisp, since they're so simple.

;;;###autoload
(defun get-text-property (pos prop &optional buffer)
  "Returns the value of the PROP property at the given position."
  (let ((e (extent-at pos buffer prop)))
    (if e
	(extent-property e prop)
      nil)))

(defun extent-properties-at-1 (position buffer text-props-only)
  (let ((extent nil)
	(props nil)
	new-props)
    (while (setq extent (extent-at position buffer 'text-prop extent))
      (setq new-props
	    (if text-props-only
		(let ((prop (extent-property extent 'text-prop)))
		  (list prop (extent-property extent prop)))
	      (extent-properties extent)))
      (cond ((null props)
	     (setq props new-props))
	    (t
	     (while new-props
	       (or (getf props (car new-props))
		   (setq props (cons (car new-props)
				     (cons (car (cdr new-props))
					   props))))
	       (setq new-props (cdr (cdr new-props)))))))
    props))

;;;###autoload
(defun extent-properties-at (position &optional buffer)
  "Returns the properties of the character at the given position,
by merging the properties of overlapping extents.  The returned value
is a property list, some of which may be shared with other structures.
You must not modify it.

This returns all properties on all extents."
  (extent-properties-at-1 position buffer nil))

;;;###autoload
(defun text-properties-at (position &optional buffer)
  "Returns the properties of the character at the given position,
by merging the properties of overlapping extents.  The returned value
is a property list, some of which may be shared with other structures.
You must not modify it.

This returns only those properties added with `put-text-property'.
See also `extent-properties-at'."
  (extent-properties-at-1 position buffer t))


;;;###autoload
(defun text-property-any (start end prop value &optional buffer)
  "Check text from START to END to see if PROP is ever `eq' to VALUE.
If so, return the position of the first character whose PROP is `eq'
to VALUE.  Otherwise return nil."
  ;; #### what should (text-property-any x y 'foo nil) return when there
  ;; is no foo property between x and y?  Either t or nil seems sensible,
  ;; since a character with a property of nil is indistinguishable from
  ;; a character without that property set.
  (map-extents
   #'(lambda (e ignore)
       (if (eq value (extent-property e prop))
	   ;; return non-nil to stop mapping
	   (max start (extent-start-position e))
	 nil))
   nil start end buffer))

;;;###autoload
(defun text-property-not-all (start end prop value &optional buffer)
  "Check text from START to END to see if PROP is ever not `eq' to VALUE.
If so, return the position of the first character whose PROP is not
`eq' to VALUE.  Otherwise, return nil."
  (map-extents
   #'(lambda (e ignore)
       ;;### no, actually, this is harder.  We need to collect all props
       ;; for a given character, and then determine whether no extent
       ;; contributes the given value.  Doing this without consing lots
       ;; of lists is the tricky part.
       (if (not (eq value (extent-property e prop)))
	   ;; return non-nil to stop mapping
	   (max start (extent-start-position e))
	 nil))
   nil start end buffer))


;; Possibly this should be ported to C; as there are no applications of it yet,
;; I don't know how important its performance is.
(defun next-property-change-1 (pos limit buffer prop forward-p)
  (let ((best nil)
	this start end)
    (save-excursion
      (and buffer (set-buffer buffer))
      ;; #### Bug: this assumes that the buffer is optimially partitioned,
      ;; but that's not necessarily true.  If we're in a buffer that has
      ;; text props, but doesn't do anything on the after-insertion-hooks
      ;; (that is, it's not font-locked) then deletions won't re-partition
      ;; the extents, so it's possible to get two adjascent extents with
      ;; the same props.  We should ignore this extent if it's equal to
      ;; the one before/after (where "equal" is only slightly tricky.)
      (map-extents
       #'(lambda (e ignore)
	   (if (and prop (not (extent-property e prop)))
	       nil
	     (setq start (extent-start-position e)
		   end (extent-end-position e)
		   this (if (if forward-p (> start pos) (< end pos))
			    (if forward-p start end)
			  (if forward-p end start))
		   best (if best
			    (if forward-p (min best this) (max best this))
			  this)))
	   nil)
       nil
       (if forward-p pos (or limit (point-min)))
       (if forward-p (or limit (point-max)) pos)
       buffer))
    best))

;;;###autoload
(defun next-property-change (pos &optional buffer limit)
  "Return the position of next property change.
Scans forward from POS until it finds a change in some text property,
then returns the position of the change.  In other words, it returns the
position of the first character beyond POS whose properties are not
identical to those of the character just after POS.
LIMIT bounds the search (defaults to point-max.)
Returns nil if the properties remain unchanged all the way to the end."
  (next-property-change-1 pos limit buffer nil t))

;;;###autoload
(defun next-single-property-change (pos prop &optional buffer limit)
  "Return the position of next property change for a specific property.
Scans forward from POS until it finds a change in PROPERTY,
then returns the position of the change.  In other words, it returns the
position of the first character beyond POS whose PROP property differs from
that of the character just after POS.
LIMIT bounds the search (defaults to point-max.)
Returns nil if the property is the same all the way to the end."
  (next-property-change-1 pos limit buffer prop t))

;;;###autoload
(defun previous-property-change (pos &optional buffer limit)
  "Return the position of previous property change.
Scans backward from POS until it finds a change in some text property,
then returns the position of the change.  In other words, it returns the
position of the first character before POS whose properties are not
identical to those of the character just after POS.
LIMIT bounds the search (defaults to point-min.)
Returns nil if the properties remain unchanged all the way to the beginning."
  (next-property-change-1 pos limit buffer nil nil))

;;;###autoload
(defun previous-single-property-change (pos prop &optional buffer limit)
  "Return the position of previous property change for a specific property.
Scans backward from POS until it finds a change in PROPERTY,
then returns the position of the change.  In other words, it returns the
position of the first character before POS whose PROP property differs from
that of the character just after POS.
LIMIT bounds the search (defaults to point-min.)
Returns nil if the property is the same all the way to the beginning."
  (next-property-change-1 pos limit buffer prop nil))


;(defun clear-all ()
;  (map-extents #'(lambda (x i) (detach-extent x) nil)
;	       nil (point-min) (point-max)))

;(defun bufex ()
;  (let* ((e (next-extent (current-buffer)))
;	 (rest (list e)))
;    (while (setq e (next-extent e)) (setq rest (cons e rest)))
;    (nreverse rest)))


(provide 'text-props)

;;; text-props.el ends here
