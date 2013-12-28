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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; New default is to use overlays
(put 'w3-emacs19 'use-overlays t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File-name-handler-alist functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (w3-rassoc 'w3-file-handler file-name-handler-alist) nil
  (setq file-name-handler-alist
 	(let ((new-handler (cons (concat "^/"
					 (substring w3-nonrelative-link 1 nil))
 				 'w3-file-handler)))
 	  (if file-name-handler-alist
 	      (append (list new-handler) file-name-handler-alist)
 	    (list new-handler)))))

(defun w3-file-handler (operation &rest args)
  "Function called from the file-name-handler-alist routines.  OPERATION
is what needs to be done ('file-exists-p, etc).  args are the arguments
that would have been passed to OPERATION."
  (let ((fn (get operation 'w3-mode))
	(url (substring (car args) 1 nil))
	(myargs (cdr args)))
    (if (string-match (concat w3-nonrelative-link "/[^/]") url)
	(setq url (concat (w3-match url 1) "://"
			  (substring url (1- (match-end 0)) nil))))
    (if fn (apply fn url myargs)
      (let (file-name-handler-alist)
	(apply operation url myargs)))))

(put 'insert-file-contents 'w3-mode 'w3-insert-file-contents)
(put 'directory-files 'w3-mode 'w3-directory-files)
(put 'file-directory-p 'w3-mode 'w3-file-directory-p)
(put 'file-writable-p 'w3-mode 'w3-file-writable-p)
(put 'file-readable-p 'w3-mode 'w3-file-exists)
(put 'file-executable-p 'w3-mode 'null)
(put 'file-symlink-p 'w3-mode 'null)
(put 'file-exists-p 'w3-mode 'w3-file-exists)
(put 'copy-file 'w3-mode 'w3-copy-file)
(put 'file-attributes 'w3-mode 'w3-file-attributes)
(put 'file-name-all-completions 'w3-mode 'w3-file-name-all-completions)
(put 'file-name-completion 'w3-mode 'w3-file-name-completion)
(put 'file-local-copy 'w3-mode 'w3-file-local-copy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some hackery to get emacs19 to do bold/underline/etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tips for creating a new w3-emacs19-hack-XXX function:
;;; use /etc/termcap, and look for these fields in the definition for your
;;; terminal:
;;; us = turn on underline
;;; ue = turn off underline
;;; md = bold
;;; se = normal

(defun w3-emacs19-hack-vt100 ()
  "Hack 'faces' for ttys (vt100)"
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table 1 (vector (create-glyph "[4m")))
  (aset standard-display-table 2 (vector (create-glyph "[m")))
  (aset standard-display-table 3 (vector (create-glyph "[1m")))
  (aset standard-display-table 4 (vector (create-glyph "[m")))
  )

(fset 'w3-emacs19-hack-vt102 'w3-emacs19-hack-vt100)
(fset 'w3-emacs19-hack-vt200 'w3-emacs19-hack-vt100)
(fset 'w3-emacs19-hack-vt220 'w3-emacs19-hack-vt100)
(fset 'w3-emacs19-hack-vt320 'w3-emacs19-hack-vt100)

(defun w3-emacs19-hack-xterm ()
  "Hack 'faces' for ttys (xterm)"
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table 1 (vector (create-glyph "[4m")))
  (aset standard-display-table 2 (vector (create-glyph "[m")))
  (aset standard-display-table 3 (vector (create-glyph "[5m")))
  (aset standard-display-table 4 (vector (create-glyph "[m")))
  )

(defun w3-emacs19-hack-console ()
  "Hack 'faces' for ttys (linux-console)"
  ;; This isn't exactly right, but close enough
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table 1 (vector (create-glyph "[1m")))
  (aset standard-display-table 2 (vector (create-glyph "[m")))
  (aset standard-display-table 3 (vector (create-glyph "[4m")))
  (aset standard-display-table 4 (vector (create-glyph "[m")))
  )

(defun w3-emacs19-unhack-faces ()
  "Remove faces hacks"
  (interactive)
  (standard-display-default 1 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menubars, etc.  Cheat and use the lucid compatibility package, because
;;; I find it much more readable then the emacs19 keymap defs. ugh.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar w3-links-menu nil "Menu for w3-mode in emacs 19")
(make-variable-buffer-local 'w3-links-menu)

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

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (interactive "@e")
  (let* ((pt (posn-point (event-start e)))
	 (props (and pt (text-properties-at pt)))
	 (link (nth 1 (nth 1 (memq 'w3 props))))
	 (form (nth 1 (memq 'w3form props)))
	 (imag (nth 1 (memq 'w3graphic props))))
    (cond
     (link (message (w3-quotify-percents link)))
     (form (message "Form entry (name=%s, type=%s)"
		    (w3-quotify-percents (nth 2 form))
		    (w3-quotify-percents (if (equal "" (nth 1 form)) "text"
					   (downcase (nth 1 form))))))
     (imag (message "Inlined image (%s)" (w3-quotify-percents (car imag))))
     (t (message "")))))
     
(defun w3-create-faces ()
  "Create faces, the emacs 19 way"
  (if (or (eq window-system 'x)		; X-windows?
	  (eq window-system 'ns)	; NeXTstep?
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
	(make-face w3-visited-node-style)
	(if (not (face-differs-from-default-p w3-header-style))
	    (copy-face 'bold-italic w3-header-style))
	(if (not (face-differs-from-default-p w3-visited-node-style))
	    (copy-face 'bold-italic w3-visited-node-style))
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
     (link (or (previous-single-property-change zone 'w3) (point-min)))
     (form (or (previous-single-property-change zone 'w3form) (point-min)))
     (t (error "No zone contains %d!" zone)))))

(defun w3-zone-end (zone)
  "This is just a hack to get the end of a text-property region"
  (let ((link (get-text-property zone 'w3))
	(form (get-text-property zone 'w3form)))
    (cond
     (link (or (next-single-property-change zone 'w3) (point-max)))
     (form (or (next-single-property-change zone 'w3form) (point-max)))
     (t (error "No zone contains %d!" zone)))))

(defun w3-zone-eq (zone1 zone2)
  "Are two zones equal?"
  (and
   (= (w3-zone-start zone1) (w3-zone-start zone2))
   (= (w3-zone-end zone1) (w3-zone-end zone2))))

(defun w3-fix-extent-endpoints ()
  "Make sure no extents have whitespace/newlines at the end of them"
  (let ((zones (w3-all-zones)) nd)
    (while zones
      (setq nd (1- (w3-zone-end (car zones))))
      (while (memq (char-after nd) '(?\n ? ?\t))
	(set-text-properties nd (1+ nd) nil)
	(setq nd (1- nd)))
      (setq zones (cdr zones)))))
      
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
	(form (get-text-property zone 'w3form))
	(grph (get-text-property zone 'w3graphic))
	(list (get-text-property zone 'w3expandlist)))
    (cond
     (link (cons 'w3 link))
     (form (cons 'w3form form))
     (grph (cons 'w3graphic grph))
     (list (cons 'w3expandlist list))
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
    (while (and (setq next-change (next-single-property-change cur 'w3-ident))
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

(defun w3-forward-link (p)
  "Go forward 1 link."
  (interactive "P")
  (if (and p (/= 1 p))
      (w3-forward-link (1- p)))
  (let ((link (w3-forward-link-internal))
	(form (w3-forward-form-internal)))
    (cond
     ((and (null link) (null form)) (error "No more links."))
     (t
      (goto-char (min (or link (point-max)) (or form (point-max))))
      (skip-chars-forward " \t\n")))))

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

(defun w3-back-link (p)
  "Go back 1 link"
  (interactive "P")
  (if (and p (/= 1 p))
      (w3-back-link (1- p)))
  (let ((link (w3-back-link-internal))
	(form (w3-back-form-internal)))
    (if (equal form (point)) (setq form nil))
    (if (equal link (point)) (setq link nil))
    (cond
     ((and (null link) (null form)) (error "No previous links."))
     (t
      (goto-char (max (or link (point-min)) (or form (point-min))))
      (skip-chars-forward " \t\n")))))

(defun w3-zone-hidden-p (start end)
  "Return t iff the region from start to end is invisible."
  (get-text-property (1+ start) 'invisible))

(defun w3-unhide-zone (start end)
  "Make a region from START TO END visible. (emacs19)"
  (add-text-properties start end (list 'invisible nil)))

(defun w3-hide-zone (start end)
  "Make a region from START to END invisible. (emacs19)"
  (add-text-properties start end (list 'invisible t)))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (emacs19)"
  (add-text-properties start end
		       (list 'face style ; Set the font, etc
			     (car data) (cdr data)))
  (cond
   (window-system nil)			; Don't hack faces
   ((not w3-emacs19-hack-faces-p) nil)	; Don't hack faces
   ((or (eq style 'w3-node-style)
	(eq style 'w3-visited-node-style)
	(eq style 'w3-bold-style))
    (goto-char end) (insert 4)
    (goto-char start) (insert 3))
   ((or (eq style 'w3-header-style)
	(eq style 'w3-underline-style))
    (goto-char end) (insert 2)
    (goto-char start) (insert 1)))
  (if (and (eq (car data) 'w3) (nth 1 data))
      (add-text-properties start end (list 'w3-ident (nth 1 data)
					   'mouse-face 'modeline)))
  (if (eq (car-safe data) 'w3form)
      (add-text-properties start end (list 'front-sticky nil
					   'rear-nonsticky '(t)
					   'mouse-face 'region))))

(defun w3-follow-mouse (e)
  "Function suitable to being bound to a mouse key.  Follows the link under
the mouse click."
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link))

(defun w3-zone-at (pt)
  "Return the extent at point PT that is either a link or a forms area."
  (let ((link (get-text-property pt 'w3))
	(form (get-text-property pt 'w3form))
	(grph (get-text-property pt 'w3graphic))
	(list (get-text-property pt 'w3expandlist)))
    (cond
     (link pt)
     (form pt)
     (grph pt)
     (list pt)
     (t nil))))

(defun w3-follow-inlined-image-mouse (e)
  "Follow the inlined image under the mouse - ignore any hyperlinks or
form entry areas and blindly try to find an image."
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-inlined-image))

(defun w3-follow-inlined-image ()
  "Follow an inlined image, regardless of whether it is a hyperlink or not."
  (interactive)
  (let ((grph (get-text-property (point) 'w3graphic)))
    (cond
     (grph (w3-maybe-relative (nth 0 grph)))
     (t (error "No inlined image at point.")))))

(if (boundp 'hyperb:version)
    nil
  (define-key w3-mode-map [mouse-2] 'w3-follow-mouse)
  (define-key w3-mode-map [mouse-3] 'w3-popup-menu))

(and window-system
     (boundp 'emacs-minor-version)
     (>= emacs-minor-version 23)
     (define-key w3-mode-map [mouse-movement] 'w3-mouse-handler))

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
    (add-menu nil "WWW" (cdr w3-main-menu))
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

(defun w3-insert (&rest args)
  "Insert without inheriting any text properties"
  (let ((start (point)))
    (apply 'insert-before-markers args)
    (set-text-properties start (point) nil)))

(defun w3-setup-version-specifics ()
  "Set up routine for emacs 19"
  (if (and (not window-system) w3-emacs19-hack-faces-p)
      (let ((hack-fn (intern (concat "w3-emacs19-hack-" (getenv "TERM")))))
	(if (fboundp hack-fn)
	    (funcall hack-fn)
	  (message "Don't know how to hack faces for %s..." (getenv "TERM"))
	  (setq w3-emacs19-hack-faces-p nil))))
  (if (memq window-system
	    '(x pm ns))       ;; Only load this up in X, presentation manager,
			      ;; or NeXTstep, otherwise emacs will barf
      (condition-case ()
	  (require 'lmenu)
	(error
	 (let ((saved-menu-info
		(mapcar
		 (function
		  (lambda (x)
		    (cons x (cdr (assoc 'menu-bar-lines
					(frame-parameters x))))))
		 (frame-list))))
	   (require 'menubar "lmenu")
	   (mapcar (function
		    (lambda (x)
		      (modify-frame-parameters (car x)
					       (list (cons 'menu-bar-lines
							   (cdr x))))))
		   saved-menu-info)))))
  (cond 
   ((or (eq window-system 'x) (eq window-system 'pm))
    (fset 'w3-x-popup-menu 'x-popup-menu))
   ((eq window-system 'ns)
    (fset 'w3-x-popup-menu 'ns-popup-menu))))

(defun w3-store-in-x-clipboard (str)
  "Store string STR in the Xwindows clipboard"
  (cond
   ((eq window-system 'x) (x-set-selection 'PRIMARY str))
   ((eq window-system 'pm) (x-set-selection 'PRIMARY str))
   ((eq window-system 'ns) (ns-store-pasteboard-internal str))
   (t nil)))

(defun w3-mode-version-specifics ()
  "Emacs 19 specific stuff for w3-mode"
  (if (and (not window-system) w3-emacs19-hack-faces-p)
      (recenter 1))
  (if (or (memq window-system '(x pm ns)))
      (w3-build-FSF19-menu)))

(defun w3-map-links (function &optional buffer from to maparg)
  "Map FUNCTION over the hypertext links which overlap region in BUFFER,
starting at FROM and ending at TO.  FUNCTION is called with the arguments
linkdata, MAPARG.
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of
BUFFER, the end of BUFFER, nil, and (current-buffer), respectively.

In emacs19, FROM, TO, and BUFFER are ignored.... working on it."
  (mapcar (function (lambda (x)
		      (funcall function (w3-zone-data x) maparg)
		      nil)) (w3-only-links))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alternate zone-functions for emacs 19.2x - these use overlays instead
;;; of text properties.  In emacs 19.22, text props cause lots of garbage
;;; collection.  Overlays don't appear to cause this problem.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (get 'w3-emacs19 'use-overlays))
    (message "Emacs 19 is using text properties.")
  (message "Emacs 19 is using overlays.")
(fset 'w3-zone-start 'overlay-start)
(fset 'w3-zone-end 'overlay-end)
(fset 'w3-zone-eq 'eq)
(fset 'w3-delete-zone 'delete-overlay)
(fset 'w3-insert 'insert-before-markers)

(defun w3-fix-extent-endpoints ()
  "Make sure no extents have whitespace/newlines at the end of them"
  (let ((ovls (overlay-lists)) st nd cur)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq cur (car ovls)
	    ovls (cdr ovls)
	    st (overlay-start cur)
	    nd (overlay-end cur))
      (while (memq (char-after st) '(?  ?\n ?\t))
	(setq st (1+ st)))
      (move-overlay cur st nd))))

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (interactive "e")
  (let* ((pt (posn-point (event-start e)))
	 (props (and pt
		     (not (eq pt 'mode-line))
		     (apply 'nconc
			       (mapcar 'overlay-properties (overlays-at pt)))))
	 (link (nth 1 (nth 1 (memq 'w3 props))))
	 (form (nth 1 (memq 'w3form props)))
	 (imag (nth 1 (memq 'w3graphic props))))
    (cond
     (link (message (w3-quotify-percents link)))
     (form (message "Form entry (name=%s, type=%s)"
		    (w3-quotify-percents (nth 2 form))
		    (w3-quotify-percents (if (equal "" (nth 1 form)) "text"
					   (downcase (nth 1 form))))))
     (imag (message "Inlined image (%s)" (w3-quotify-percents (car imag))))
     (t (message "")))))

(defun w3-zone-data (zone)
  "Return the data from a zone"
  (let ((link (overlay-get zone 'w3))
	(form (overlay-get zone 'w3form))
	(grph (overlay-get zone 'w3graphic))
	(list (overlay-get zone 'w3expandlist)))
    (cond
     (link (cons 'w3 link))
     (form (cons 'w3form form))
     (grph (cons 'w3graphic grph))
     (list (cons 'w3expandlist list))
     (t nil))))
  
(defun w3-zone-at (pt)
  "Return the extent at point PT that is either a link or a forms area."
  (let ((ovls (overlays-at pt)) cur link form grph list)
    (while ovls
      (setq cur (car ovls)
	    ovls (cdr ovls)
	    link (or link (and (overlay-get cur 'w3) cur))
	    form (or form (and (overlay-get cur 'w3form) cur))
	    grph (or grph (and (overlay-get cur 'w3graphic) cur))
	    list (or list (and (overlay-get cur 'w3expandlist) cur))))
    (cond
     (link link)
     (form form)
     (grph grph)
     (list list)
     (t nil))))
  
(defun w3-only-links ()
  "Get all the zones from a buffer"
  (let ((ovls (overlay-lists)) cur result)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq cur (car ovls) ovls (cdr ovls))
      (if (overlay-get cur 'w3) (setq result (cons cur result))))
    (nreverse result)))
  
(defun w3-all-forms-zones ()
  "Get all the zones from a buffer."
  (let ((ovls (overlay-lists)) cur result)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq cur (car ovls) ovls (cdr ovls))
      (if (overlay-get cur 'w3form) (setq result (cons cur result))))
    (nreverse result)))
  
(defun w3-all-zones ()
  "Get all the zones from a buffer."
  (let ((ovls (overlay-lists)) cur result)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq cur (car ovls) ovls (cdr ovls))
      (if (or (overlay-get cur 'w3) (overlay-get cur 'w3form))
	  (setq result (cons cur result))))
    (nreverse result)))
  
(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let ((ovls (overlay-lists))
	cur found)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while (and (not found) ovls)
      (setq cur (car ovls) ovls (cdr ovls))
      (if (equal link (overlay-get cur 'w3-ident))
	  (setq found (or (goto-char (overlay-start cur)) t))))))
  
(defun w3-forward-link (p)
  "Go forward 1 link."
  (interactive "P")
  (if (and p (/= 1 p)) (w3-forward-link (1- p)))
  (let (found (next (next-overlay-change (point))) ovl done)
    (while (and (not done) (/= (point-max) next))
      (setq found (overlays-at next)
	    next (next-overlay-change next))
      (while (and found (not done))
	(if (or (overlay-get (car found) 'w3)
		(overlay-get (car found) 'w3form))
	    (setq done (car found)))
	(setq found (cdr found))))
    (cond
     ((null done) (error "No more links."))
     (t
      (goto-char (overlay-start done))
      (skip-chars-forward " \t\n")))))
  
(defun w3-back-link (p)
  "Go back 1 link"
  (interactive "P")
  (setq p (or p 1))
  (let ((x (w3-zone-at (point))))
    (and x (goto-char (w3-zone-start x))))
  (let ((ovls (overlay-lists)) tmp cur)
    (setq ovls (nconc (car ovls) (cdr ovls))
	  ovls (sort ovls
		     (function (lambda (x y)
				 (< (overlay-start x) (overlay-start y))))))
    (while (< (overlay-start (car ovls)) (point))
      (if (or (overlay-get (car ovls) 'w3)
	      (overlay-get (car ovls) 'w3form))
	  (setq tmp (cons (car ovls) tmp)))
      (setq ovls (cdr ovls)))
    (cond
     ((nth (1- p) tmp)
      (goto-char (overlay-start (nth (1- p) tmp)))
      (skip-chars-forward " \t\n"))
     (t
      (error "No more links.")))))
  
(defun w3-zone-hidden-p (start end)
  "Return t iff the region from start to end is invisible."
  (let ((x (overlays-at (1+ start))) y)
    (while x
      (if (overlay-get (car x) 'invisible)
	  (setq y t))
      (setq x (cdr x)))
    y))
  
(defun w3-unhide-zone (start end)
  "Make a region from START TO END visible. (emacs19)"
  (let ((x (overlays-at (1+ start))))
    (while x
      (if (overlay-get (car x) 'invisible)
	  (overlay-put (car x) 'invisible nil))
      (setq x (cdr x)))))

(defun w3-hide-zone (start end)
  "Make a region from START to END invisible. (emacs19)"
  (overlay-put (make-overlay start end) 'invisible t))
  
(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (emacs19)"
  (let ((ovl (make-overlay start end)))
    (overlay-put ovl 'face style)
    (overlay-put ovl (car data) (cdr data))
    (overlay-put ovl 'rear-nonsticky t)
    (overlay-put ovl 'front-sticky t)
    (cond
     (window-system nil)		; Don't hack faces
     ((not w3-emacs19-hack-faces-p) nil) ; Don't hack faces
     ((or (eq style 'w3-node-style)
	  (eq style 'w3-visited-node-style)
	  (eq style 'w3-bold-style))
      (goto-char end) (insert 4)
      (goto-char start) (insert 3))
     ((or (eq style 'w3-header-style)
	  (eq style 'w3-underline-style))
      (goto-char end) (insert 2)
      (goto-char start) (insert 1)))
    (if (and (eq (car data) 'w3) (nth 1 data))
	(overlay-put ovl 'w3-ident (nth 1 data)))
    (overlay-put ovl 'mouse-face
		 (cond
		  ((eq (car data) 'w3) 'modeline)
		  ((eq (car data) 'w3form) 'region)
		  ((eq (car data) 'w3graphic) 'secondary-selection)))))

(defun w3-follow-inlined-image ()
  "Follow an inlined image, regardless of whether it is a hyperlink or not."
  (interactive)
  (let ((ovls (overlays-at (point))) done)
    (while (and ovls (not done))
      (if (not (overlay-get (car ovls) 'w3graphic))
	  nil
	(w3-maybe-relative (nth 0 (overlay-get (car ovls) 'w3graphic)))
	(setq done t))
      (setq ovls (cdr ovls)))
    (if (not done) (error "No inlined image at point."))))
)

(provide 'w3-emacs19)
  
