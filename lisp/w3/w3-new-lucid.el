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
(require 'w3-vars)
(defvar w3-links-menu nil "Menu for w3-mode in lemacs")
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
    "-----------"
    ["View Source" w3-document-source t]
    ["Edit Document Source" w3-find-this-file t]
    ["Reload Current Document" w3-reload-document t]
    ["Mail Formatted Document " w3-mail-current-document t]
    ["Print Current Document" w3-print-this-url t]
    ["Print Document At Link" w3-print-url-under-point t]
    ["Submit Bug Report" w3-submit-bug t])
  "Another menu for lucid emacs.")

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
  "Popup menu for lucid emacs")

(defvar w3-navigate-menu
  '("Navigate"
    ["Back" nil nil]
    ["Forward" nil nil]
    "----------"
    ["Goto Home Document" w3 t]
    ["Local History" w3-show-history-list w3-keep-history]
    ["Use Hotlist" w3-show-hotlist w3-hotlist]
    ("Hotlist Maintenance"
     ["Add this document to hotlist" w3-add-document-to-hotlist t]
     ["Delete item from hotlist" w3-remove-from-hotlist t]
     ["Rename item in hotlist" w3-rename-hotlist-entry t]
     ["Append new hotlist file" w3-append-hotlist t])
    "----------"
    ["Internet Starting Points" (w3-fetch "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/StartingPoints/NetworkStartingPoints.html") t]
    ["Internet Resources Meta-index" (w3-fetch "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/MetaIndex.html") t]
    ["NCSA's What's New" (w3-fetch "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/whats-new.html") t]
    "----------")
  "The navigation menu")

(defvar w3-options-menu
  '("Options"
    ["Delay Image Load" (setq w3-delay-image-loads (not w3-delay-image-loads))
     nil]
    ["Flush Image Cache" (setq w3-graphics-list nil) t]
    ("Hypertext Gopher Mode"
     ["Turn On" (setq w3-use-hypertext-gopher t) (not w3-use-hypertext-gopher)]
     ["Turn Off" (setq w3-use-hypertext-gopher nil) w3-use-hypertext-gopher])
    ("Hypertext Dired Mode"
     ["Turn On" (setq w3-directory-format 'hypertext)
      (eq 'dired w3-directory-format)]
     ["Turn Off" (setq w3-directory-format 'dired)
      (eq 'hypertext w3-directory-format)])
    ["Clear History" (setq w3-history-list nil) w3-history-list])
  "The options menu for w3")

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
      (set-face-underline-p w3-underline-style t)))

(fset 'w3-delete-zone 'delete-extent)
(fset 'w3-zone-end 'extent-end-position)
(fset 'w3-zone-start 'extent-start-position)
(fset 'w3-zone-eq 'eq)

(defun w3-fix-extent-endpoints ()
  "Make sure no extents contain trailing whitespace/newlines"
  (let ((skip-chars (list ?\t ?\r ?\n ?\ )))
    (map-extents (function
		  (lambda (ext maparg)
		    (let ((st (extent-start-position ext))
			  (nd (extent-end-position ext))
			  (ch nil))
		      (while (memq (char-after (1- nd)) skip-chars)
			(setq nd (1- nd)
			      ch t))
		      (while (memq (char-after st) skip-chars)
			(setq st (1+ st)
			      ch t))
		      (if ch
			  (if (<= nd st)
			      (delete-extent ext)
			    (set-extent-endpoints ext st nd))))
		    nil)))))

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
  (map-extents (function (lambda (extent link)
			   (if (equal link
				      (extent-property extent 'w3-ident))
			       (goto-char (extent-start-position extent))
			     nil)))
	       (current-buffer) (point-min) (point-max) link))

(defun w3-zone-data (zone)
  "Return the data associated with zone"
  (let ((link (extent-property zone 'w3))
	(form (extent-property zone 'w3form)))
    (cond
     (link (cons 'w3 link))
     (form (cons 'w3form form))
     (t nil))))

(defun w3-extent-at (pt)
  "Return the extent at point PT that is either a link or a forms area."
  (let ((link (extent-at pt (current-buffer) 'w3))
	(form (extent-at pt (current-buffer) 'w3form)))
    (cond
     (link link)
     (form form)
     (t nil))))

(defun w3-mouse-handler (e)
  "Experimental function to message the url under the mouse cursor"
  (save-excursion
    (let ((x (progn
	       (mouse-set-point e)
	       (w3-view-this-url t))))
      (if x
	  (if (eq w3-track-mouse 'descriptive)
	      (w3-link-info x)
	    (message x))))))

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

(defun w3-forward-link ()
  "Move forward to the next link in the document.  Error if no more links."
  (interactive)
  (let ((x (w3-next-extent (extent-at (point)))))
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

(defun w3-back-link ()
  "Go back link"
  (interactive)
  (let ((x (w3-previous-extent (extent-at (point)))))
    (if x (goto-char (extent-start-position x))
      (error "No previous link."))))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (lucid)"
  (let ((ext (make-extent start end)))
    (set-extent-property ext 'end-open nil)
    (set-extent-property ext 'start-open nil)
    (set-extent-property ext 'face style)
    (set-extent-property ext 'highlight highlight)
    (set-extent-property ext (car data) (cdr data))
    (if (eq (car data) 'w3)
	(progn
	  (set-extent-property ext 'priority 2)
	  (if (nth 1 data)
	      (set-extent-property ext 'w3-ident (nth 1 data)))))))

(defun w3-follow-mouse (e)
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link))

(defun w3-follow-link ()
  "Attempt to follow the link under the cursor" 
  (interactive)
  (let ((link (extent-at (point) (current-buffer) 'w3))
	(form (extent-at (point) (current-buffer) 'w3form)))
    (cond
     (link (w3-maybe-relative (nth 1 (extent-property link 'w3))))
     (form
      (w3-do-form-entry (cons 'w3form (extent-property form 'w3form)) form))
     (t (message "No link or form entry area at point.")))))

(defun w3-view-this-url (&optional no-show)
  (interactive)
  (let ((link (extent-at (point) (current-buffer) 'w3))
	(form (extent-at (point) (current-buffer) 'w3form)))
    (cond
     (link
      (setq link (extent-property link 'w3))
      (if (not no-show) (message "%s" (nth 1 link)) (nth 1 link)))
     (form
      (setq form (extent-property form 'w3form))
      (if (not no-show)
	  (message "Form entry (name=%s, type=%s)" (nth 2 form)
		   (if (equal "" (nth 1 form)) "TEXT" (nth 1 form)))
	nil))
     (t (if (not no-show) (message "No link or form entry area.") nil)))))

(define-key w3-mode-map 'button2 'w3-follow-mouse)
(define-key w3-mode-map 'button3 'w3-popup-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (add-menu nil "WWW" (cdr w3-popup-menu))
    (add-menu nil "Navigate" (cdr w3-navigate-menu))
    (if (cdr hot-menu)
	(add-menu '("Navigate") "Hotlist"
		  (w3-breakup-menu (cdr hot-menu) w3-max-menu-length)))
    (if (cdr w3-links-menu)
	(add-menu '("Navigate") "Links"
		  (w3-breakup-menu (cdr w3-links-menu) w3-max-menu-length)))
    (add-menu nil "Options" (cdr w3-options-menu))
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

(defun w3-complete-link ()
  "Choose a link from the current buffer and follow it"
  (interactive)
  (let ((x (cdr w3-links-menu))
	(y nil)
	z)
    (if (not x) (error "No links in current document."))
    (while x
      (setq y (cons (cons (aref (car x) 0)
			  (car (cdr (aref (car x) 1)))) y)
	    x (cdr x)))
    (setq z (completing-read "Link: " y nil t))
    (w3-fetch (cdr (assoc z y)))))

(defun w3-setup-version-specifics ()
  "Set up routine for Lucid emacs 19.9"
  nil)

(defun w3-store-in-x-clipboard (str)
  "Store string STR into the clipboard in X"
  (x-own-selection str 'PRIMARY)
  (x-selection-owner-p 'PRIMARY))

(provide 'w3-new-lucid)
