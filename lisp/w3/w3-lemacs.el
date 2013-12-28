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
;;; Enhancements For Lucid Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar w3-links-menu nil "Menu for w3-mode in lemacs")
(defvar w3-image-type-restriction nil)
(defvar w3-image-size-restriction nil)

(defvar w3-allowed-image-types
  (mapcar (function (lambda (x) (list (car x)))) w3-graphic-converter-alist))
(make-variable-buffer-local 'w3-links-menu)

(defun w3-ins-or-del-graphic (typ)
  (if (assoc typ w3-allowed-image-types)
      (setq w3-allowed-image-types
	    (mapcar (function (lambda (x) (if (equal typ (car x)) nil x)))
		    w3-allowed-image-types))
    (setq w3-allowed-image-types (cons (list typ) w3-allowed-image-types))))

(defun w3-setup-options-menu ()
  "Setup the complicated 19.10 options menu"
  (defvar w3-options-menu nil "The options menu for w3")
  (let ((formats
	 (mapcar
	  (function
	   (lambda (data)
	     (let ((typ (car data)))
	       (vector typ
		       (list 'w3-ins-or-del-graphic typ)
		       ':style 'radio
		       ':selected
		       (list 'assoc typ 'w3-allowed-image-types)))))
	  w3-graphic-converter-alist)))
    (setq w3-options-menu
	  (list "Options"
		(list "Image Loading"
		      ["Delay all images"
		       (setq w3-delay-image-loads t
			     w3-graphics-always-show-entities nil)
		       :style radio :selected
		       (and w3-delay-image-loads
			    (not w3-graphics-always-show-entities))]
		      ["Show only graphic entities"
		       (setq w3-delay-image-loads t
			     w3-graphics-always-show-entities t)
		       :style radio :selected
		       (and w3-delay-image-loads
			    w3-graphics-always-show-entities)]
		      ["Show all images"
		       (setq w3-delay-image-loads nil
			     w3-graphics-always-show-entities t)
		       :style radio :selected (not w3-delay-image-loads)]
		      ["Show inlined MPEGs"
		       (setq w3-delay-mpeg-loads (not w3-delay-mpeg-loads))
		       :style toggle :selected (not w3-delay-mpeg-loads)]
		      )
		(cons "Restrict Image Loads By Type" formats)
		["Flush Image Cache" (setq w3-graphics-list nil)
		 w3-graphics-list]
		["Hypertext Gopher Mode" (setq w3-use-hypertext-gopher
					       (not w3-use-hypertext-gopher))
		 :style toggle :selected w3-use-hypertext-gopher]
		["Hypertext Dired Mode" w3-toggle-hypertext-dired
		 :style toggle :selected w3-use-hypertext-dired]
		["Clear History" (setq w3-history-list nil) w3-history-list]))
    ))

(if (and (boundp 'emacs-major-version)
	 (>= emacs-minor-version 10))
    (w3-setup-options-menu)
  (defvar w3-options-menu
    '("Options"
      ["Delay Image Load" (setq w3-delay-image-loads (not w3-delay-image-loads))
       t]
      ["Flush Image Cache" (setq w3-graphics-list nil) w3-graphics-list]
      ("Hypertext Gopher Mode"
       ["Turn On" (setq w3-use-hypertext-gopher t) (not w3-use-hypertext-gopher)]
       ["Turn Off" (setq w3-use-hypertext-gopher nil) w3-use-hypertext-gopher])
      ("Hypertext Dired Mode"
       ["Turn On" (setq w3-directory-format 'hypertext)
	(eq 'dired w3-directory-format)]
       ["Turn Off" (setq w3-directory-format 'dired)
	(eq 'hypertext w3-directory-format)])
      ["Clear History" (setq w3-history-list nil) w3-history-list])
    "The options menu for w3"))

(defvar w3-use-hypertext-dired (eq w3-directory-format 'hypertext)
  "Whether to display directory listings in hypertext or not.")  

(defun w3-toggle-hypertext-dired ()
  "Toggle how to display directory listings"
  (interactive)
  (setq w3-use-hypertext-dired (not w3-use-hypertext-dired)
	w3-directory-format (if w3-use-hypertext-dired 'hypertext 'dired)))  

(defvar w3-lucid-max-menu-item-size 40
  "*Maximum size of a menu item in the link or hotlist menus.")

(defun w3-create-faces ()
  "Create faces, the lucid way"
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
  (if (not (face-differs-from-default-p w3-visited-node-style))
      (copy-face 'bold-italic w3-visited-node-style))
  (if (not (face-differs-from-default-p w3-underline-style))
      (set-face-underline-p w3-underline-style t)))

(fset 'w3-delete-zone 'delete-extent)
(fset 'w3-zone-end 'extent-end-position)
(fset 'w3-zone-start 'extent-start-position)
(fset 'w3-zone-eq 'eq)
(fset 'w3-insert 'insert)

(defun w3-zone-hidden-p (start end)
  "Return t iff the region from start to end is invisible."
  (and (extent-at (1+ start))
       (extent-property (extent-at (1+ start)) 'invisible)))

(defun w3-unhide-zone (start end)
  "Make a region from START TO END visible. (lemacs)"
  (map-extents
   (function
    (lambda (ext)
      (if (and (= start (extent-start-position ext))
	       (= end   (extent-end-position ext))
	       (extent-property ext 'invisible))
	  (progn (delete-extent ext) t)
	nil))) start end))

(defun w3-hide-zone (start end)
  "Make a region from START to END invisible. (lemacs)"
  (set-extent-property (make-extent start end) 'invisible t))

(defun w3-fix-extent-endpoints ()
  "Make sure no extents contain trailing whitespace/newlines"
  nil)

(defun w3-all-zones ()
  (let ((cur (next-extent (current-buffer)))
	(all nil))
    (while cur
      (setq all (cons cur all))
      (setq cur (next-extent cur)))
    all))

(defun w3-truncate-menu-item (string)
  (if (<= (length string) w3-lucid-max-menu-item-size)
      string
    (concat (substring string 0 w3-lucid-max-menu-item-size) "$")))

(defun w3-add-hotlist-menu ()
  (if (eq major-mode 'w3-mode)
      (let ((hot-menu nil)
	    (hot w3-hotlist))
	(while hot
	  (setq hot-menu (cons (vector
				(w3-truncate-menu-item (car (car hot)))
				(list 'w3-fetch (car (cdr (car hot))))
				t) hot-menu)
		hot (cdr hot)))
	(if (cdr w3-links-menu)
	    (add-menu '("Navigate") "Links" (w3-breakup-menu
					     (cdr w3-links-menu)
					     w3-max-menu-length))
	  (condition-case ()
	      (delete-menu-item '("Navigate" "Links"))
	    (error nil)))
	(if hot-menu (add-menu '("Navigate") "Hotlist"
			       (w3-breakup-menu hot-menu
						w3-max-menu-length))
	  (condition-case ()
	      (delete-menu-item '("Navigate" "Hotlist")))))))

(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let ((dat (map-extents
	      (function
	       (lambda (ext maparg)
		 (if (equal link (extent-property ext 'w3-ident))
		     (cons ext (extent-start-position ext))
		   nil))))))
    (cond
     (dat
      (goto-char (cdr dat))
      (message "Found link %s" link)
      (force-highlight-extent (car dat) t)
      (while (not (input-pending-p))
	(sit-for 1))
      (force-highlight-extent (car dat) nil)))))     

(defun w3-zone-data (zone)
  "Return the data associated with zone"
  (let ((link (extent-property zone 'w3))
	(grph (extent-property zone 'w3graphic))
	(form (extent-property zone 'w3form))
	(list (extent-property zone 'w3expandlist))
	(mpeg (extent-property zone 'w3mpeg))
	(dely (extent-property zone 'w3delayed)))
    (cond
     (link (cons 'w3 link))
     (form (cons 'w3form form))
     (dely (cons 'w3delayed dely))
     (grph (cons 'w3graphic grph))
     (mpeg (cons 'w3mpeg mpeg))
     (list (cons 'w3expandlist list))
     (t nil))))

(defun w3-zone-at (pt)
  "Return the extent at point PT that is either a link or a forms area."
  (let ((link (extent-at pt (current-buffer) 'w3))
	(form (extent-at pt (current-buffer) 'w3form))
	(grph (extent-at pt (current-buffer) 'w3graphic))
	(list (extent-at pt (current-buffer) 'w3expandlist))
	(mpeg (extent-at pt (current-buffer) 'w3mpeg))
	(dely (extent-at pt (current-buffer) 'w3delayed)))
    (cond
     (link link)
     (form form)
     (dely dely)
     (grph grph)
     (list list)
     (mpeg mpeg)
     (t nil))))

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (let* ((pt (event-point e))
	 (props (and pt (extent-properties-at pt)))
	 (link (nth 1 (nth 1 (memq 'w3 props)))) ; The link info if it exists
	 (form (nth 1 (memq 'w3form props))) 	 ; The form info it it exists
	 (dely (nth 0 (nth 1 (memq 'w3delayed props))))	 ; The delayed img info
	 (mpeg (nth 1 (memq 'w3mpeg props)))     ; the delayed mpeg info
	 (imag (nth 1 (memq 'w3graphic props)))) ; The image info if it exists
    (cond
     (link (message (w3-quotify-percents link)))
     (form (message "Form entry (name=%s, type=%s)"
		    (w3-quotify-percents (nth 2 form))
		    (w3-quotify-percents (if (equal "" (nth 1 form)) "text"
					   (downcase (nth 1 form))))))
     (dely (message "Delayed image (%s)" (w3-quotify-percents (car dely))))
     (imag (message "Inlined image (%s)" (w3-quotify-percents (car imag))))
     (mpeg (message "Delayed mpeg (%s)" (w3-quotify-percents (car mpeg))))
     (t (message "")))))

(defun w3-next-extent (xt)
  "Return the next extent after XT that is a link or a forms area."
  (let ((x nil))
    (map-extents (function (lambda (extent maparg)
			     (if (or (extent-property extent 'w3)
				     (extent-property extent 'w3form))
				 (setq x extent) nil)))
		 (current-buffer)
		 (if xt (1+ (extent-end-position xt)) (point))
		 (point-max))
    x))

(defun w3-forward-link (p)
  "Move forward to the next link in the document.  Error if no more links."
  (interactive "P")
  (if (and p (/= 1 p))
      (w3-forward-link (1- p)))
  (let ((x (w3-next-extent (or (extent-at (point) nil 'w3)
			       (extent-at (point) nil 'w3form)))))
    (if x (goto-char (extent-start-position x))
      (error "No more links."))))

(defun w3-previous-extent (xt)
  (let ((x nil))
    (map-extents (function (lambda (extent maparg)
			     (if (or (extent-property extent 'w3)
				     (extent-property extent 'w3form))
			       (setq x extent)) nil))
		 (current-buffer) (point-min)
		 (if xt (extent-start-position xt) (point)))
    x))

(defun w3-back-link (p)
  "Go back link"
  (interactive "P")
  (if (and p (/= 1 p))
      (w3-back-link (1- p)))
  (let ((x (w3-previous-extent (extent-at (point)))))
    (if x (goto-char (extent-start-position x))
      (error "No previous link."))))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (lucid)"
  (let ((ext (make-extent start end)))
    (set-extent-property ext 'face style)
    (set-extent-property ext 'highlight highlight)
    (set-extent-property ext (car data) (cdr data))
    (set-extent-property ext 'start-open t)
    (set-extent-property ext 'end-open t)
    (if (eq (car data) 'w3) (set-extent-property ext 'priority 2))
    (if (nth 1 data)
	(set-extent-property ext 'w3-ident (nth 1 data)))))

(defun w3-follow-mouse (e)
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link))

(defun w3-follow-inlined-image-mouse (e)
  "Follow an inlined image from the mouse"
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-inlined-image))

(defun w3-follow-inlined-image ()
  "Follow an inlined image, regardless of whether it is a hyperlink or not."
  (interactive)
  (let ((grph (extent-at (point) (current-buffer) 'w3graphic)))
    (cond
     (grph (w3-maybe-relative (nth 0 (extent-property grph 'w3graphic))))
     (t (message "No inlined image at point.")))))

(cond
 ((boundp 'hyperb:version) nil)		; Don't mess with hyperbole's bindings
 ((boundp 'emacs-major-version)
  (define-key w3-mode-map 'button2 'w3-follow-mouse)
  (define-key w3-mode-map '(control button2) 'w3-follow-inlined-image-mouse))
 (t
  (define-key w3-mode-map 'button2 'w3-follow-mouse)
  (define-key w3-mode-map '(control button2) 'w3-follow-inlined-image-mouse)
  (define-key w3-mode-map 'button3 'w3-popup-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-toplevel-menu-exists-p (name)
  "Search for a top level menu called NAME.  Return non-nil iff it exists"
  (assoc name current-menubar))

(defun w3-build-lemacs-menu ()
  "Build lemacs menus from w3-links-list"
  (let* ((hot w3-hotlist)
	 (hot-menu nil))
    (or current-menubar
	(set-menubar default-menubar))
    (map-extents 'w3-build-links-helper)
    (setq w3-links-menu (cons "Links" w3-links-menu))
    (while hot
      (setq hot-menu
	    (cons (vector (car (car hot))
			  (list 'w3-maybe-relative (car (cdr (car hot))))
			  t) hot-menu))
      (setq hot (cdr hot)))
    (setq hot-menu (cons "Hotlist" hot-menu))
    (set-buffer-menubar (copy-sequence current-menubar))
    (add-menu '("Help") "WWW" (cdr w3-help-menu))
    (add-menu nil "WWW" (cdr w3-main-menu))
    (add-menu nil "Navigate" (cdr w3-navigate-menu))
    (if (cdr hot-menu)
	(add-menu '("Navigate") "Hotlist"
		  (w3-breakup-menu (cdr hot-menu) w3-max-menu-length)))
    (if (cdr w3-links-menu)
	(add-menu '("Navigate") "Links"
		  (w3-breakup-menu (cdr w3-links-menu) w3-max-menu-length)))
    (if (w3-toplevel-menu-exists-p "Options")
	(add-menu '("Options") "WWW"  (cdr w3-options-menu))
      (add-menu nil "Options" (cdr w3-options-menu)))
    (add-menu nil "Annotate" (cdr w3-annotation-menu))))

(defun w3-build-links-helper (extent maparg)
  "Build a list of links using map-extents for lucid"
  (let ((x (extent-property extent 'w3)))
    (if (and x (not (null (nth 1 x))))
	(setq w3-links-menu
	      (nconc w3-links-menu
		     (list
		      (vector (w3-truncate-menu-item
			       (w3-strip-leading-spaces (nth 2 x)))
			      (list 'w3-maybe-relative (nth 1 x)) t)))))
    nil))

(defun w3-popup-menu (e)
  "Pop up a menu of common w3 commands"
  (interactive "e")
  (mouse-set-point e)
  (popup-menu w3-popup-menu))

(defun w3-x-popup-dialog (pos descr)
  "My hacked up function to do a blocking popup menu..."
  (let ((echo-keystrokes 0)
	event dialog)
    (setq dialog (cons (car descr) dialog)
	  descr (cdr descr))
    (while descr
      (setq dialog (nconc dialog
			  (list (vector (car descr)
					(list (car descr)) t)))
	    descr (cdr descr)))
    (popup-dialog-box dialog)
    (catch 'dialog-done
      (while t
	(setq event (next-command-event event))
	(cond
	 ((and (menu-event-p event) (stringp (car-safe (event-object event))))
	  (throw 'dialog-done (car-safe (event-object event))))
	 ((and (menu-event-p event)
	       (or (eq (event-object event) 'abort)
		   (eq (event-object event) 'menu-no-selection-hook)))
	  (signal 'quit nil))
	 ((button-release-event-p event) nil)
	 (t
	  (beep)
	  (message "Please make a choice from the dialog")))))))
      
(defun w3-x-popup-menu (pos menudesc)
  "If last command was a mouse command use a popup-menu, otherwise do a
completing read"
  (if (or (button-press-event-p last-command-event)
	  (button-release-event-p last-command-event)
	  (menu-event-p last-command-event))
      (w3-x-really-popup-menu pos menudesc)
    (completing-read "Please choose: " (cdr (cdr (car (cdr menudesc))))
		     nil t)))

(defun w3-x-really-popup-menu (pos menudesc)
  "My hacked up function to do a blocking popup menu..."
  (let ((echo-keystrokes 0)
	event menu)
    (setq menudesc (cdr (car (cdr menudesc)))) ; remove the title
    (while menudesc
      (setq menu (cons (vector (car (car menudesc))
			       (list (car (car menudesc))) t) menu)
	    menudesc (cdr menudesc)))
    (setq menu (cons "WWW" menu))
    (popup-menu menu)
    (catch 'popup-done
      (while t
	(setq event (next-command-event event))
	(cond ((and (menu-event-p event) (stringp (car-safe
						   (event-object event))))
	       (throw 'popup-done (event-object event)))
	      ((and (menu-event-p event)
		    (or (eq (event-object event) 'abort)
			(eq (event-object event) 'menu-no-selection-hook)))
	       (signal 'quit nil))
	      ((button-release-event-p event);; don't beep twice
	       nil)
	      (t
	       (beep)
	       (message "please make a choice from the menu.")))))))

(defun w3-setup-version-specifics ()
  "Set up routine for Lucid emacs 19.9 and newer"
  )

(defun w3-store-in-x-clipboard (str)
  "Store string STR into the clipboard in X"
  (x-own-selection str 'PRIMARY)
  (x-selection-owner-p 'PRIMARY))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-view-graphic (src)
  "View the graphic specified by SRC"
  (interactive (list
		(completing-read "View Graphic: " w3-graphics-list
				 nil t)))
  (let ((x (assoc src w3-graphics-list))
	y)
    (if x
	(progn
	  (setq y (x-create-screen (list (cons "iconic" "true"))))
	  (x-set-screen-icon-pixmap y (cdr x))
	  (message "Press a key to remove the picture")
	  (while (not (input-pending-p))
	    (sit-for 1))
	  (delete-screen y))
      (error "Could not find %s in image cache." src))))

(defun w3-load-delayed-images ()
  "Load inlined images that were delayed, if necessary."
  (interactive)
  (let ((buffer-read-only nil) rest)
    (map-extents
     (function
      (lambda (ext maparg)
	(if (extent-property ext 'w3delayed)
	    (setq rest (cons ext rest)))
	nil)))
    (while rest
      (delete-region (extent-start-position (car rest))
		     (extent-end-position (car rest)))
      (delete-extent (car rest))
      (setq rest (cdr rest)))
    (mapcar (function (lambda (data) (apply 'w3-insert-graphic data)))
	    w3-delayed-images))
  (setq w3-delayed-images nil))

(defun w3-load-delayed-mpegs ()
  "Load all delayed mpeg movies for this buffer"
  (interactive)
  (let ((buffer-read-only nil) rest)
    (map-extents
     (function
      (lambda (ext maparg)
	(if (extent-property ext 'w3delayed)
	    (setq rest (cons ext rest)))
	nil)))
    (while rest
      (delete-region (extent-start-position (car rest))
		     (extent-end-position (car rest)))
      (delete-extent (car rest))
      (setq rest (cdr rest)))
    (mapcar (function (lambda (data) (w3-insert-mpeg (car data) (cdr data))))
	    w3-delayed-movies)
    (setq w3-delayed-movies nil)))

(defun w3-insert-mpeg (src pt)
  "Insert an mpeg file SRC at point PT"
  (let* ((ext (make-extent pt pt))
	 (win (make-x-window-glyph w3-mpeg-size w3-mpeg-size))
	 (w3-mpeg-args (append w3-mpeg-args
			       (list "-window" (int-to-string
						(x-window-glyph-xid win))
				     src))))
    (set-extent-begin-glyph ext win)
    (set-extent-property ext 'w3-mpeg
			 (cons (apply 'start-process src nil
				      w3-mpeg-program w3-mpeg-args)
			       win))))

(defun w3-mpeg-kill-processes (&optional buffer)
  "Kill all mpeg_play processes associated with this buffer"
  (interactive)
  (map-extents
   (function
    (lambda (ext maparg)
      (let ((data (extent-property ext 'w3-mpeg)))
	(if (not data)
	    nil
	  (delete-process (car data))
	  (delete-extent ext)
	  nil))))))	    

(defun w3-load-single-delayed-mpeg (st nd src pt)
  "Load a single delayed mpeg"
  (let ((buffer-read-only nil))
    (delete-region st nd)
    (w3-insert-mpeg src st)))

(defun w3-load-single-delayed-graphic (st nd src pt align alt)
  "Load a single delayed image."
  (let ((buffer-read-only nil))
    (delete-region st nd)
    (w3-insert-graphic src pt align alt)))  

(defun w3-insert-graphic (name pt align alt &optional force)
  "Insert the graphic pointed to by the URL NAME, at buffer position POINT,
with alignment specified by ALIGN (one of 'center 'top or 'bottom).  If the
conversion of the picture fails for any reason, use ALT as the alternative
text.  If the reading of the pixmap is successful, the url and a pointer to
the pixmap are stored in w3-graphics-list for possible re-use later."
  (let ((bit nil)
	(add-to-list nil)
	(buffer-read-only nil)
	(w3-request-method "GET")
	(w3-be-asynchronous nil)
	(w3-request-data nil)
	(w3-request-extra-headers nil)
	(w3-source t)
	(err nil)
	(lnk (cdr name))
	(fname (w3-generate-unique-filename)))
    (setq name (car name))
    (if (string-match w3-nonrelative-link name)
	nil
      (setq name (w3-parse-relative-link name)))
    (save-excursion
      (let ((w3-working-buffer " *W3GRAPH*")
	    (attribs (w3-file-attributes name)))
	(cond
	 ((assoc name w3-graphics-list)
	  (message "Reusing image...")
	  (setq bit (cdr (assoc name w3-graphics-list))))
	 ((and (not force)
	       (not (assoc (nth 8 attribs) w3-allowed-image-types)))
	  (w3-lazy-message "Skipping image %s [%s]" 
			   (w3-basepath name t) (nth 8 attribs))
	  (let ((anno (make-annotation alt pt 'text)))
	    (set-annotation-data anno (list (cons name lnk) pt align alt t))
	    (set-annotation-action anno 'w3-annotation-action-2)))
	 ((and (not force)
	       (numberp w3-image-size-restriction)
	       (> 0 (nth 7 attribs))
	       (> (nth 7 attribs) w3-image-size-restriction))
	  (w3-lazy-message "Skipping image %s [%s bytes]" 
			   (w3-basepath name t) (nth 7 attribs))
	  (let ((anno (make-annotation alt pt 'text)))
	    (set-annotation-data anno (list (cons name lnk) pt align alt t))
	    (set-annotation-action anno 'w3-annotation-action-2)))
	 (t
	  (setq add-to-list t
		err t)
	  (w3-retrieve name)
	  (w3-convert-graphic-to-useable-format w3-working-buffer
						fname
						(not (featurep 'xpm)))
	  (message "Reading image %s..." w3-current-file)
	  (if (equal 0 (nth 7 (file-attributes fname)))
	      (save-excursion
		(set-buffer (get-buffer-create "Conversion errors"))
		  (goto-char (point-max))
		  (insert "\n------------------------\n"
			  "Log of: " name "\n")
		  (insert-buffer w3-working-buffer)
		  (message "Got a 0 length pixmap!"))
	    (condition-case ()
		(setq bit (make-pixmap fname))
	      (error (save-excursion
		       (set-buffer
			(get-buffer-create "Conversion errors"))
		       (goto-char (point-max))
		       (insert "\n------------------------\n"
			       "Log of: " name "\n")
		       (insert-buffer w3-working-buffer)
		       (message "Reading of pixmap failed!")))))
	  (condition-case ()
	      (delete-file fname)
	    (error nil))))))
    (and add-to-list
	 (setq w3-graphics-list (cons (cons name bit) w3-graphics-list)))
    (cond 
     (bit
      (if (= (or (char-after pt) 0) ?\t) (setq pt (1- pt)))
      (if (>= (pixmap-width bit) (* 0.50 (screen-pixel-width)))
	  (save-excursion
	    (goto-char pt)
	    (insert "\n\n")
	    (setq pt (1+ pt))))
      (let ((anno (make-annotation bit pt 'text nil t)))
	(set-annotation-data anno lnk)
	(set-annotation-action anno 'w3-annotation-action-3)))
     (err
      (let ((anno (make-annotation alt pt 'text)))
	(set-annotation-data anno (cons name lnk))
	(set-annotation-action anno 'w3-annotation-action-1)))
     (t nil))))

(defun w3-annotation-action-1 (data)
  "Annotation function that passes a failed image off to an external viewer"
  (w3-fetch (car data)))

(defun w3-annotation-action-2 (data)
  "Annotation function that tries to load 1 delayed image."
  (set-buffer (extent-buffer extent))
  (delete-annotation extent)
  (apply 'w3-insert-graphic data))

(defun w3-annotation-action-3 (data event)
  "Annotation function that tries send off an imagemap click"
  (let ((url (nth 2 data))
;	(top (cdr (assq 'top (screen-parameters (event-screen event)))))
;	(left (cdr (assq 'left (screen-parameters (event-screen event)))))
	(x (event-x-pixel event))
	(y (event-y-pixel event))
	)
    (cond
     ((and (eq (nth 0 data) 'ismap)
	   (stringp url))
      (w3-fetch (concat url "?" x "," y)))
     ((stringp url) (w3-fetch url))
     (t nil))))

(defun w3-mode-version-specifics ()
  "Lucid emacs specific stuff for w3-mode"
  (w3-build-lemacs-menu)
  (if w3-track-mouse (setq mode-motion-hook 'w3-mouse-handler))
  (add-hook 'activate-menubar-hook 'w3-add-hotlist-menu)
  (setq mode-popup-menu w3-popup-menu))

(defun w3-lookup-image (pixmap)
  "Return the URL that pixmap was grabbed from"
  (car
   (rassq
    (extent-property (extent-at (point)) 'begin-glyph) w3-graphics-list)))

(defun w3-write-zones (st nd &optional buf)
  "Write the zones in buffer BUF from ST to ND into lisp expressions
that can be 'eval'd."
  (save-excursion
    (setq buf (or buf (current-buffer)))
    (set-buffer (get-buffer-create " *w3-cache*"))
    (goto-char (point-max))
    (map-extents (function
		  (lambda (ext maparg)
		    (let ((props (extent-properties ext)))
		      (insert
		       (format "(let ((x (make-extent %d %d)))\n"
			       (extent-start-position ext)
			       (extent-end-position ext)))
		      (while props
			(if (eq (car props) 'begin-glyph)
			    (insert
			     (format "(setq w3-delayed-images (cons (list \"%s\" (set-marker (make-marker) %d) 'center \"\") w3-delayed-images))"
				     (car
				      (rassq
				       (extent-property ext 'begin-glyph)
				       w3-graphics-list))
				     (extent-start-position ext)))
			  (insert
			   (format "(set-extent-property x '%S '%S)\n"
				   (nth 0 props) (nth 1 props))))
			(setq props (cdr (cdr props))))
		      (insert ")\n"))))
		 buf st nd)
    (insert "(rename-buffer \"" (buffer-name buf) "\")\n"
	    "(display-buffer (current-buffer))\n")
    (insert ")")))

(defun w3-map-links (function &optional buffer from to maparg)
  "Map FUNCTION over the hypertext links which overlap region in BUFFER,
starting at FROM and ending at TO.  FUNCTION is called with the arguments
linkdata, MAPARG.
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of
BUFFER, the end of BUFFER, nil, and (current-buffer), respectively."
  (map-extents (function (lambda (x y)
			   (if (extent-property x 'w3)
			       (funcall function (w3-zone-data x) y))
			   nil)) buffer from to maparg))

(provide 'w3-lemacs)
