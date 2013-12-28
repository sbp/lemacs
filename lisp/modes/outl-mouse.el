;;; outl-mouse.el --- outline mode mouse commands for Emacs

;; Copyright 1994 (C) Andy Piper <ajp@eng.cam.ac.uk>

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
;; outl-mouse.el v1.2.3:
;;
;; Defines button one to hide blocks when clicked on outline-up-glyph-bitmap
;; and expand blocks when clicked on outline-down-glyph-bitmap.
;; Features are activated when outline-minor-mode or outline-mode are turned
;; on.
;;
;; Only works in lemacs 19.10 and onwards. 

(defvar outline-up-glyph-bitmap
  (make-pixmap	; an up-arrow
   (list 10 10 (concat "\000\000\000\000\060\000\060\000\150\000"
		       "\150\000\324\000\324\000\376\001\376\001")))
  "Bitmap object for outline up glyph.")

(defvar outline-up-depressed-glyph-bitmap
  (make-pixmap	; an up-arrow
   (list 10 10 (concat "\000\000\000\000\060\000\060\000\130\000"
		       "\130\000\254\000\274\000\006\001\376\001")))
  "Bitmap object for outline depressed up glyph.")

(defvar outline-down-glyph-bitmap
  (make-pixmap	; a down-arrow
   (list 10 10 (concat "\000\000\000\000\376\001\202\001\364\000"
		       "\324\000\150\000\150\000\060\000\060\000")))
  "Bitmap object for outline down glyph.")

(defvar outline-down-depressed-glyph-bitmap
  (make-pixmap	; a down-arrow
   (list 10 10 (concat "\000\000\000\000\376\001\376\001\254\000"
		       "\254\000\130\000\130\000\060\000\060\000")))
  "Bitmap object for outline depressed down glyph.")

(defvar outline-glyph-menu
  '("Outline Commands"
    ["Hide all"		hide-body			t]
    ["Hide subtrees"    hide-subtree                    t]
    ["Hide body"        hide-body                       t]
    ["Show subtrees"    show-subtree                    t]
    ["Show body"        show-entry                      t])
  "Menu of commands for outline glyphs.")

(if (fboundp 'set-pixmap-contributes-to-line-height)
    (progn
      (set-pixmap-contributes-to-line-height outline-down-glyph-bitmap nil)
      (set-pixmap-contributes-to-line-height outline-up-glyph-bitmap nil)))

(require 'annotations)
(require 'advice)			; help me doctor !
(require 'outline)

(add-hook 'outline-mode-hook 'outline-mouse-hooks)
(add-hook 'outline-minor-mode-hook 'outline-mouse-hooks)

(defadvice outline-minor-mode (after outline-mode-mouse activate)
  "Advise outline-minor-mode to delete glyphs when switched off."
  (if (not outline-minor-mode)
      (progn 
	(outline-delete-glyphs)
	(show-all))))

(defadvice show-all (after show-all-ad activate)
  "Advise show-all to sync headings."
  (outline-sync-visible-sub-headings-in-region (point-min) (point-max)))

(defadvice hide-subtree (after hide-subtree-ad activate)
  "Advise hide-subtree to sync headings."
  (outline-sync-visible-sub-headings))

(defadvice hide-entry (after hide-entry-ad activate)
  "Advise hide-entry to sync headings."
  (outline-sync-visible-sub-headings))

(defadvice hide-body (after hide-body-ad activate)
  "Advise hide-body to sync headings."
  (outline-sync-visible-sub-headings-in-region (point-min) (point-max)))

(defadvice show-subtree (after show-subtree-ad activate)
  "Advise show-subtree to sync headings."
  (outline-sync-visible-sub-headings))

(defadvice show-entry (after show-entry-ad activate)
  "Advise shown-entry to sync headings."
  (outline-sync-visible-sub-headings))

(defun outline-mouse-hooks ()
  "Hook for installing outlining with the mouse."
  (outline-add-glyphs)
  (let ((outline (cond ((keymapp (lookup-key (current-local-map)
					     outline-minor-mode-prefix))
			(lookup-key (current-local-map)
				    outline-minor-mode-prefix))
		       (t
			(define-key (current-local-map)
			  outline-minor-mode-prefix (make-sparse-keymap))
			(lookup-key (current-local-map) 
				    outline-minor-mode-prefix)))))
    (define-key outline "\C-a" 'outline-heading-add-glyph)
    (define-key outline-mode-map "\C-c\C-a" 'outline-heading-add-glyph)))

(defun outline-add-glyphs ()
  "Add annotations and glyphs to all heading lines that don't have them."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (not (outline-on-heading-p)) (outline-next-heading))
    (while (not (eobp))
      (outline-heading-add-glyph-1)
      (outline-next-heading))))

(defun outline-delete-glyphs ()
  "Remove annotations and glyphs from heading lines."
  (save-excursion
    (mapcar 'outline-heading-delete-glyph (annotation-list))))

(defun outline-heading-delete-glyph (ext)
  "Delete annotation and glyph from a heading with annotation EXT."
  (if (and 
       (progn
	 (goto-char (extent-start-position ext))
	 (beginning-of-line)
	 (outline-on-heading-p))
       (extent-property ext 'outline))
      (delete-annotation ext))
  nil)

(defun outline-heading-add-glyph ()
  "Interactive version of outline-heading-add-glyph-1."
  (interactive)
  (save-excursion
    (outline-heading-add-glyph-1)))

(defun outline-heading-add-glyph-1 ()
  "Add glyph to the end of heading line which point is on.
 Returns nil if point is not on a heading or glyph already exists."
  (if (or (not (outline-on-heading-p))
	  (outline-heading-has-glyph-p))
      nil
    (outline-back-to-heading)
    (let ((anot2 
	   (make-annotation outline-down-glyph-bitmap 
			    (save-excursion (outline-end-of-heading) (point))
			    'text nil t outline-down-depressed-glyph-bitmap))
	  (anot1 
	   (make-annotation outline-up-glyph-bitmap 
			    (save-excursion (outline-end-of-heading) (point))
			    'text nil t outline-up-depressed-glyph-bitmap)))
      ;; we cunningly make the annotation data point to its twin.
      (set-annotation-data anot1 anot2)
      (set-extent-property anot1 'outline 'up)
      (set-annotation-action anot1 'outline-up-click)
      (set-annotation-menu anot1 outline-glyph-menu)
      (set-extent-priority anot1 1)
      (set-annotation-data anot2 anot1)
      (set-extent-property anot2 'outline 'down)
      (set-annotation-menu anot2 outline-glyph-menu)
      (set-annotation-action anot2 'outline-down-click)
      (annotation-hide anot2))
    t))

(defun outline-heading-has-glyph-p ()
  "Return t if heading has an outline glyph."
  (catch 'found
    (mapcar
     '(lambda(a)
	(if (extent-property a 'outline)
	    (throw 'found t)))
     (annotations-in-region (save-excursion (outline-back-to-heading) (point))
			    (save-excursion (outline-end-of-heading) 
					    (+ 1 (point)))
			    (current-buffer)))
    nil))

(defun outline-sync-visible-sub-headings-in-region (pmin pmax)
  "Make sure all anotations on headings in region PMIN PMAX are 
displayed correctly."
  (mapcar '(lambda (x) 
	     (goto-char (extent-start-position x))
	     (beginning-of-line)
	     (cond ((and (eq (extent-property x 'outline) 'down)
			 ;; skip things we can't see
			 (not (eq (preceding-char) ?\^M)))
		    (if (outline-more-to-hide)
			;; reveal my twin
			(annotation-reveal (annotation-data x))
		      (annotation-hide (annotation-data x)))
		    (if (not (outline-hidden-p))
			;; hide my self
			(annotation-hide x)
		      (annotation-reveal x)))))
	  (annotations-in-region pmin pmax (current-buffer))))

(defun outline-sync-visible-sub-headings ()
  "Make sure all anotations on sub-headings below the one point is on are 
displayed correctly."
  (outline-sync-visible-sub-headings-in-region 
   (point) 
   (progn (outline-end-of-subtree) (point))))

(defun outline-fold-out (annotation)
  "Fold out the current heading."
  (beginning-of-line)
  (if (not (equal (condition-case nil
		      (save-excursion (outline-next-visible-heading 1)
				      (point))
		    (error nil))
		  (save-excursion (outline-next-heading) 
				  (if (eobp) nil (point)))))
      (progn 
	(save-excursion (show-children))
	(outline-sync-visible-sub-headings))
    ;; mess with single entry
    (if (outline-hidden-p) 
	(progn 
	  (save-excursion (show-entry))
	  ;; reveal my twin and hide me
	  (annotation-hide annotation)
	  (annotation-reveal (annotation-data annotation))))))

(defun outline-fold-in (annotation)
  "Fold in the current heading."
  (beginning-of-line)
  ;; mess with single entries
  (if (not (outline-hidden-p))
      (progn
	(save-excursion (hide-entry))
	(if (not (outline-more-to-hide))
	    (annotation-hide annotation))
	(annotation-reveal (annotation-data annotation)))
    ;; otherwise look for more leaves
    (save-excursion 
      (if (outline-more-to-hide t)
	  (hide-subtree)
	(hide-leaves)))
    ;; sync everything
    (outline-sync-visible-sub-headings)))

(defun outline-more-to-hide (&optional arg)
  "Return t if there are more visible sub-headings or text.
With ARG return t only if visible sub-headings have no visible text."
  (if (not (outline-hidden-p))
      (if arg nil t)
    (save-excursion
      (and (< (funcall outline-level) (condition-case nil
					  (progn 
					    (outline-next-visible-heading 1)
					    (funcall outline-level))
					(error 0)))
	   (if (and (not (outline-hidden-p)) arg)
	       nil t)))))

(defun outline-hidden-p ()
  "Return t if point is on the header of a hidden subtree."
  (save-excursion
    (let ((end-of-entry (save-excursion (outline-next-heading))))
      ;; Make sure that the end of the entry really exists.
      (if (not end-of-entry)
	  (setq end-of-entry (point-max)))
      (outline-back-to-heading)
      ;; If there are ANY ^M's, the entry is hidden.
      (search-forward "\^M" end-of-entry t))))

(defun outline-up-click (data ev)
  "Annotation action for clicking on an up arrow.
DATA is the annotation data. EV is the mouse click event."
  (save-excursion
    (goto-char (extent-end-position (event-glyph ev)))
    (outline-fold-in (event-glyph ev))))

(defun outline-down-click (data ev)
  "Annotation action for clicking on a down arrow.
DATA is the annotation data. EV is the mouse click event."
  (save-excursion
    (goto-char (extent-end-position (event-glyph ev)))
    (outline-fold-out (event-glyph ev))))

(provide 'outl-mouse)
(provide 'outln-18)			; fool auctex - outline is ok now.

;; Local Variables:
;; outline-regexp: ";;; \\|(def.."
;; End:

