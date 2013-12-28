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
;;; Enhancements For Emacs 19
;;; Based on enhancements for Lucid Emacs, by jsc@mit.edu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)
(if (eq window-system 'x)    ;; Only load this up in x, otherwise emacs will
    (progn                   ;; barf
      (require 'menubar "lmenu")
      (require 'lucid)))

(defvar w3-links-menu nil "Menu for w3-mode in emacs 19")
(make-variable-buffer-local 'w3-links-menu)

(defvar w3-popup-menu
  '("WWW Browser"
    ["Open Local File" w3-open-local t]
    ["Open URL" w3-fetch t]
    ["Show this documents address" w3-view-url t]
    ["Show address of link under cursor" w3-view-this-url t]
    ["Information on link under cursor" w3-link-info t]
    ["Copy document's address into kill-ring/X clipboard" w3-save-url t]
    ["Copy address under point into kill-ring/X clipboard" (w3-save-url t) t]
    "----------------------------------"
    ["View Source" w3-document-source t]
    ["Edit Document Source" w3-find-this-file t]
    ["Reload Current Document" w3-reload-document t]
    ["Mail Formatted Document " w3-mail-current-document t]
    ["Print Current Document" w3-print-this-url t]
    ["Print Document At Link" w3-print-url-under-point t]
    ["Submit Bug Report" w3-submit-bug t])
  "Another menu for emacs19")

(defvar w3-help-menu
  '("Help"
    ["On W3 Mode" (w3-fetch "http://cs.indiana.edu/elisp/w3/docs.html") t]
    ["On HTML" (w3-fetch "http://www.ncsa.uiuc.edu/General/Internet/WWW/HTMLPrimer.html") t]
    ["On URLs" (w3-fetch "http://www.ncsa.uiuc.edu/demoweb/url-primer.html") t]
    ["Mail Developer(s)" w3-submit-bug t])
  "The help menu for w3")     

(defvar w3-annotation-menu
  '("Annotations"
    ("Group Annotations"
     ["Add Annotation" w3-add-group-annotation t]
     ["Edit Annotation" w3-edit-group-annotation t]
     ["Delete Annotation" w3-delete-group-annotation t])
    ("Personal Annotations"
     ["Add Annotation" w3-add-personal-annotation t]
     ["Edit Annotation" w3-edit-personal-annotation t]
     ["Delete Annotation" w3-delete-personal-annotation t]))
  "The annotation menu for w3")

(defvar w3-navigate-menu
  '("Navigate"
    ["Back" nil nil]
    ["Forward" nil nil]
    "-----------------------------"
    ["Goto Home Document" w3 t]
    ["Local History" w3-show-history-list w3-keep-history]
    ["Use Hotlist" w3-show-hotlist w3-hotlist]
    ("Hotlist Maintenance"
     ["Add this document to hotlist" w3-add-document-to-hotlist t]
     ["Delete item from hotlist" w3-remove-from-hotlist t]
     ["Rename item in hotlist" w3-rename-hotlist-entry t]
     ["Append new hotlist file" w3-append-hotlist t])
    "------------------------------"
    ["Internet Starting Points" (w3-fetch "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/StartingPoints/NetworkStartingPoints.html") t]
    ["Internet Resources Meta-index" (w3-fetch "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/MetaIndex.html") t]
    ["NCSA's What's New" (w3-fetch "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/whats-new.html") t]
    "-------------------------------")
    "The navigation menu")

(defvar w3-options-menu
  '("Options"
    ["Delay Image Load" (setq w3-delay-image-loads (not w3-delay-image-loads))
     nil]
    ["Flush Image Cache" (setq w3-graphics-list nil) t]
    ("Hypertext Gopher Mode"
     ["Turn On" (setq w3-use-hypertext-gopher t) t]
     ["Turn Off" (setq w3-use-hypertext-gopher nil) t])
    ("Hypertext Dired Mode"
     ["Turn On" (setq w3-directory-format 'hypertext) t]
     ["Turn Off" (setq w3-directory-format 'dired) t])
    ["Clear History" (setq w3-history-list nil) t])
  "The options menu for w3")

(defun w3-create-faces ()
  "Create faces, the emacs 19 way"
  (if (or (eq window-system 'x)		; X-windows?
	  (eq window-system 'dps)	; NeXTstep?
	  (eq window-system 'pm)	; OS/2 Presentation manager?
	  )
      (progn
	(make-face w3-default-style)
	(make-face w3-header-style)
	(make-face w3-bold-style)
	(make-face w3-italic-style)
	(make-face w3-underline-style)
	(make-face w3-node-style)
	(make-face w3-address-style)
	(make-face w3-tt-style)
	(if (not (face-differs-from-default-p w3-header-style))
	    (copy-face 'bold-italic w3-header-style))
	(if (not (face-differs-from-default-p w3-node-style))
	    (copy-face 'bold w3-node-style))
	(if (not (face-differs-from-default-p w3-address-style))
	    (copy-face 'italic w3-address-style))
	(if (not (face-differs-from-default-p w3-bold-style))
	    (copy-face 'bold w3-bold-style))
	(if (not (face-differs-from-default-p w3-italic-style))
	    (copy-face 'italic w3-italic-style))
	(if (not (face-differs-from-default-p w3-tt-style))
	    (copy-face 'default w3-tt-style))
	(if (not (face-differs-from-default-p w3-underline-style))
	    (copy-face 'underline w3-underline-style)))))

(defun w3-zone-start (zone)
  "This is just a hack to get the beginning of a text-property region"
  (let ((link (get-text-property zone 'w3))
	(form (get-text-property zone 'w3form)))
    (cond
     (link (previous-single-property-change zone 'w3))
     (form (previous-single-property-change zone 'w3form))
     (t (error "No zone contains %d!" zone)))))

(defun w3-zone-end (zone)
  "This is just a hack to get the end of a text-property region"
  (let ((link (get-text-property zone 'w3))
	(form (get-text-property zone 'w3form)))
    (cond
     (link (next-single-property-change zone 'w3))
     (form (next-single-property-change zone 'w3form))
     (t (error "No zone contains %d!" zone)))))

(defun w3-zone-eq (zone1 zone2)
  "Are two zones equal?"
  (and
   (= (w3-zone-start zone1) (w3-zone-start zone2))
   (= (w3-zone-end zone1) (w3-zone-end zone2))))

;;; I don't think this is needed in emacs 19
(defun w3-fix-extent-endpoints ()
  "Make sure no extents have whitespace/newlines at the end of them"
;  (let ((x (w3-all-forms-zones)))
;    (while x
;      (let ((st (w3-zone-start x))
;	    (nd (w3-zone-end x)))
;	(while (memq (char-after (1- nd)) '(9 13 10 32))
;	  (remove-text-properties nd nd '(w3form nil))
;	  (setq nd (1- nd))))
;      (setq x (cdr x)))))
  nil)

(defun w3-delete-zone (zone)
  "Remove a zone from a buffer.  This is just a hack to remove some
text properties."
  (let ((st (w3-zone-start zone))
	(nd (w3-zone-end zone)))
    (remove-text-properties st nd
			    (list 'w3 nil
				  'w3form nil
				  'face nil
				  'mouse-face nil))))

(defun w3-zone-data (zone)
  "Return the data from a zone (fake text property area"
  (let ((link (get-text-property zone 'w3))
	(form (get-text-property zone 'w3form)))
    (cond
     (link (cons 'w3 (get-text-property zone 'w3)))
     (form (cons 'w3form (get-text-property zone 'w3form)))
     (t nil))))

(defun w3-only-links ()
  "Get all the zones from a buffer.  This is really just a list of positions
that have a text-property set correctly."
  (let ((cur (point-min))
	(result '()))
    (while cur
      (if (get-text-property cur 'w3)
	  (setq result (cons cur result)))
      (setq cur (next-single-property-change cur 'w3)))
    (nreverse result)))

(defun w3-all-forms-zones ()
  "Get all the zones from a buffer.  This is really just a list of positions
that have a text-property set correctly."
  (let ((cur (point-min))
	(result '()))
    (while cur
      (if (get-text-property cur 'w3form)
	  (setq result (cons (1+ cur) result)))
      (setq cur (next-single-property-change cur 'w3form)))
    (nreverse result)))

(defun w3-all-zones ()
  "Get all the zones from a buffer.  This is really just a list of positions
that have a text-property set correctly."
  (nconc (w3-only-links) (w3-all-forms-zones)))
   
(defun w3-add-hotlist-menu ()
  "Add the hotlist menu to this buffer - used when it changes."
  (if (eq major-mode 'w3-mode)
      (let ((hot-menu nil)
	    (hot w3-hotlist))
	(while hot
	  (setq hot-menu 
		(cons (vector (car (car hot))
			      (list 'w3-fetch (car (cdr (car hot))))
			      t) hot-menu)
		hot (cdr hot)))
	(add-menu '("Navigate") "Hotlist..." hot-menu))))

(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let ((cur (point-min))
	found
	next-change)
    (while (or (setq next-change (next-single-property-change cur 'w3-ident))
	       (not found))
      (if (equal link (get-text-property cur 'w3-ident))
	  (progn
	    (setq found (goto-char cur)
		  cur (point-max))
	    (message "Found link %s" link))
	(setq cur next-change)))))

(defun w3-link-at (pt)
  "Return the link(s) at point"
  (get-text-property pt 'w3))

(defun w3-forward-form-internal (&optional pt)
  "Go forward 1 link"
  (if (null pt) (setq pt (point)))
  (let ((next (next-single-property-change pt 'w3form)))
    (cond
     ((null next) nil)
     ((get-text-property next 'w3form) next)
     (t (w3-forward-form-internal next)))))

(defun w3-forward-link-internal (&optional pt)
  "Go forward 1 link"
  (if (null pt) (setq pt (point)))
  (let ((next (next-single-property-change pt 'w3)))
    (cond
     ((null next) nil)
     ((get-text-property next 'w3) next)
     (t (w3-forward-link-internal next)))))

(defun w3-forward-link ()
  "Go forward 1 link."
  (interactive)
  (let ((link (w3-forward-link-internal))
	(form (w3-forward-form-internal)))
    (cond
     ((and (null link) (null form)) (error "No more links."))
     (t
      (goto-char (min (or link (point-max)) (or form (point-max))))
      (while (looking-at "[ \\\t\\\n]") (forward-char 1))))))

(defun w3-back-link-internal (&optional pt)
  (if (and (null pt) (get-text-property (point) 'w3)
	   (previous-single-property-change (point) 'w3))
      (setq pt (1- (previous-single-property-change (point) 'w3))))
  (if (null pt) (setq pt (point)))
  (if (null (previous-single-property-change pt 'w3)) nil
    (let ((prev (1- (previous-single-property-change pt 'w3))))
      (cond
       ((null prev) nil)
       ((get-text-property prev 'w3)
	(let ((prev2 (previous-single-property-change prev 'w3)))
	  (if prev2 prev2 (point-min))))
       (t (w3-back-link-internal prev))))))

(defun w3-back-form-internal (&optional pt)
  (if (and (null pt) (get-text-property (point) (quote w3form)))
      (setq pt (previous-single-property-change (1- (point)) (quote w3form))))
  (if (null pt) (setq pt (point)))
  (let ((prev (previous-single-property-change pt (quote w3form))))
    (cond
     ((null prev) nil)
     ((get-text-property prev (quote w3form))
      (let ((prev2 (previous-single-property-change prev (quote w3form))))
	(if prev2 prev2 (point-min))))
     (t (w3-back-form-internal (1- prev))))))

(defun w3-back-link ()
  "Go back 1 link"
  (interactive)
  (let ((link (w3-back-link-internal))
	(form (w3-back-form-internal)))
    (cond
     ((and (null link) (null form)) (error "No previous links."))
     (t
      (goto-char (max (or link (point-min)) (or form (point-min))))
      (while (looking-at "[ \\\t\\\n]") (forward-char 1))))))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (emacs19)"
  (add-text-properties start end
		       (list 'face style ; Set the font, etc
			     (car data) (cdr data)))
  (if (and (eq (car data) 'w3) (nth 1 data))
      (add-text-properties start end (list 'w3-ident (nth 1 data))))
  (if (and (eq (car-safe data) 'w3form)
	   (not (or
		 (equal (nth 2 data) "CHECKBOX")
		 (equal (nth 2 data) "RADIO"))))
      (add-text-properties start end (list ;'local-map text-mode-map
					   'front-sticky nil
					   'rear-nonsticky t)))
  )

(defun w3-follow-mouse (e)
  "Function suitable to being bound to a mouse key.  Follows the link under
the mouse click."
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link))

(defun w3-follow-link ()
  "Attempt to follow the link under the cursor"
  (interactive)
  (let ((form (get-text-property (point) 'w3form))
	(link (get-text-property (point) 'w3)))
    (cond
     (form (w3-do-form-entry (cons 'w3form form) (point)))
     ((and link (nth 1 link)) (w3-maybe-relative (nth 1 link)))
     (t (message "No link!")))))

(defun w3-view-this-url (&optional no-show)
  "View the URL of the link under point"
  (interactive)
  (let ((form (get-text-property (point) 'w3form))
	(link (get-text-property (point) 'w3)))
    (cond
     (form
      (if (not no-show)
	  (message "Form entry (name=%s, type=%s)" (nth 2 form)
		   (if (equal "" (nth 1 form)) "TEXT" (nth 1 form))) nil))
     (link (message "%s" (nth 1 link)))
     (t (error "No link at point.")))))

(define-key w3-mode-map [mouse-2] 'w3-follow-mouse)
(define-key w3-mode-map [mouse-3] 'w3-popup-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-build-FSF19-menu ()
  "Build emacs19 menus from w3-links-list"
  (let* ((hot w3-hotlist)
	 (hot-menu nil)
	 (ovls (w3-only-links)))
    (while ovls
      (let ((data (w3-zone-data (car ovls))))
	(if (and (eq (car-safe data) 'w3) (nth 2 data))
	    (setq w3-links-menu 
		  (nconc w3-links-menu
			 (list
			  (vector
			   (format "%s" (w3-strip-leading-spaces (nth 3 data)))
			   (list 'w3-maybe-relative (nth 2 data)) t))))))
      (setq ovls (cdr ovls)))
    (setq w3-links-menu (cons "Links..." w3-links-menu))
    (while hot
      (setq hot-menu
	    (cons (vector (car (car hot))
			  (list 'w3-maybe-relative (car (cdr (car hot))))
			  t) hot-menu))
      (setq hot (cdr hot)))
    (setq hot-menu (cons "Hotlist..." hot-menu))
    (set-buffer-menubar (copy-sequence current-menubar))
					;    (add-menu '("Help") "WWW" (cdr w3-help-menu))
    (add-menu nil "WWW" (cdr w3-popup-menu))
    (add-menu nil "Navigate" (append
			      (cdr w3-navigate-menu)
			      (list hot-menu w3-links-menu)))
    (add-menu nil "Options" (cdr w3-options-menu))
    (add-menu nil "Annotate" (cdr w3-annotation-menu))))

(defun w3-popup-menu (e)
  "Pop up a menu of common w3 commands"
  (interactive "e")
  (mouse-set-point e)
  (popup-menu w3-popup-menu))

(defun w3-complete-link ()
  "Choose a link from the current buffer and follow it."
  (interactive)
  (let ((x (cdr w3-links-menu))
	(y nil)
	z)
    (if (and (null window-system) (null x))
	(let ((ovls (w3-only-links)))
	  (while ovls
	    (let ((data (w3-zone-data (car ovls))))
	      (setq w3-links-menu 
		    (nconc w3-links-menu
			   (list
			    (vector
			     (format "%s" (car (cdr (cdr (cdr data)))))
			     (list 'w3-maybe-relative
				   (car (cdr (cdr data)))) t)))))
	    (setq ovls (cdr ovls)))
	  (setq w3-links-menu (cons "Links...      >" w3-links-menu))
	  (setq x (cdr w3-links-menu))))
    (if (not x) (error "No links in current document."))
    (while x
      (setq y (cons (cons (aref (car x) 0)
			  (car (cdr (aref (car x) 1)))) y)
	    x (cdr x)))
    (setq z (completing-read "Link: " y nil t))
    (w3-fetch (cdr (assoc z y)))))

(defun w3-setup-version-specifics ()
  "Set up routine for emacs 19"
  (cond 
   ((or (eq window-system 'x) (eq window-system 'pm))
    (fset 'w3-x-popup-menu 'x-popup-menu))))

(defun w3-store-in-x-clipboard (str)
  "Store string STR in the Xwindows clipboard"
  (and window-system (x-set-selection 'PRIMARY str)))

(provide 'w3-emacs19)
