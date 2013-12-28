;; Menubar support.
;; Copyright (C) 1991, 1992 Free Software Foundation, Inc.

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

(defconst default-menubar
  '(("File"	["New Screen"		x-new-screen		t]
		["Open File..."		find-file		t]
		["Save Buffer"		save-buffer		t]
		["Save Buffer As..."	write-file		t]
		["Revert Buffer"	revert-buffer		t]
		"-----"
		["Print Buffer"		lpr-buffer		t]
		"-----"
		["Delete Screen"	delete-screen		t]
		["Kill Buffer..."	kill-buffer		t]
		["Exit Emacs"		save-buffers-kill-emacs	t]
		)
    ("Edit"	["Undo"			advertised-undo		   t]
		["Cut"			x-kill-primary-selection   t]
		["Copy"			x-copy-primary-selection   t]
		["Paste"		x-yank-clipboard-selection t]
		["Clear"		x-delete-primary-selection t]
		)
    ("Buffers"	"")

    nil		; the partition: menus after this are flushright

    ("Help"	["Info"			info			t]
		["Describe Mode"	describe-mode		t]
		["Command Apropos..."	command-apropos		t]
		["List Keybindings"	describe-bindings	t]
		["Describe Key..."	describe-key		t]
		["Describe Function..."	describe-function	t]
		["Describe Variable..."	describe-variable	t]
		"-----"
		["Unix Manual..."	manual-entry		t]
		["Emacs Tutorial"	help-with-tutorial	t]
		["Emacs News"		view-emacs-news		t]
		)
    ))


(defun x-new-screen ()
  "Creates a new emacs screen (that is, a new X window.)"
  (interactive)
  (select-screen (x-create-screen nil))
  (switch-to-buffer (get-buffer-create "*scratch*"))
  ;; hack: if evi mode is loaded and in use, put the new screen in evi mode.
  (if (and (boundp 'evi-install-undo-list) evi-install-undo-list)
      (evi-mode))
  )


(defun set-menubar (menubar)
  "Set the default menubar to be menubar."
  (setq-default current-menubar (copy-sequence menubar))
  (set-menubar-dirty-flag))

(defun set-buffer-menubar (menubar)
  "Set the buffer-local menubar to be menubar."
  (make-local-variable 'current-menubar)
  (setq current-menubar (copy-sequence menubar))
  (set-menubar-dirty-flag))


;;; menu manipulation functions

(defun find-menu-item (menubar path &optional parent)
  (or parent (setq path (mapcar 'downcase path)))
  (if (not (consp menubar))
      nil
    (let ((rest menubar)
	  result)
      (while rest
	(if (and (car rest)
		 (equal (car path)
			(downcase (if (vectorp (car rest))
				      (aref (car rest) 0)
				    (if (stringp (car rest))
					(car rest)
				      (car (car rest)))))))
	    (setq result (car rest) rest nil)
	  (setq rest (cdr rest))))
      (if (cdr path)
	  (if (consp result)
	      (find-menu-item (cdr result) (cdr path) result)
	    (if result
		(signal 'error (list "not a submenu" result))
	      (signal 'error (list "no such submenu" (car path)))))
	(cons result parent)))))


(defun disable-menu-item (path)
  "Make the named menu item be unselectable.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (cdr pair)))
    (or item
	(signal 'error (list (if menu "No such menu item" "No such menu")
			     path)))
    (aset item 2 nil)
    (set-menubar-dirty-flag)
    item))


(defun enable-menu-item (path)
  "Make the named menu item be selectable.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (cdr pair)))
    (or item
	(signal 'error (list (if menu "No such menu item" "No such menu")
			     path)))
    (aset item 2 t)
    (set-menubar-dirty-flag)
    item))


(defun add-menu-item-1 (item-p menu-path item-name item-data enabled-p before)
  (if before (setq before (downcase before)))
  (let* ((menubar current-menubar)
	 (menu (condition-case ()
		   (car (find-menu-item menubar menu-path))
		 (error nil)))
	 (item (if (listp menu)
		   (car (find-menu-item (cdr menu) (list item-name)))
		 (signal 'error (list "not a submenu" menu-path)))))
    (or menu
	(let ((rest menu-path)
	      (so-far menubar))
	  (while rest
	    (setq menu (car (find-menu-item (cdr so-far) (list (car rest)))))
	    (or menu
		(let ((rest2 so-far))
		  (while (and (cdr rest2) (car (cdr rest2)))
		    (setq rest2 (cdr rest2)))
		  (setcdr rest2
		  (nconc (list (setq menu (list (car rest))))
			 (cdr rest2)))))
	    (setq so-far menu)
	    (setq rest (cdr rest)))))
    (or menu (setq menu menubar))
    (if item
	nil	; it's already there
      (if item-p
	  (setq item (vector item-name item-data enabled-p))
	(setq item (cons item-name item-data)))
      ;; if BEFORE is specified, try to add it there.
      (if before
	  (setq before (car (find-menu-item menu (list before)))))
      (let ((rest menu)
	    (added-before nil))
	(while rest
	  (if (eq before (car (cdr rest)))
	      (progn
		(setcdr rest (cons item (cdr rest)))
		(setq rest nil added-before t))
	    (setq rest (cdr rest))))
	(if (not added-before) ; add the item to the end.
	    (nconc menu (list item)))))
    (if item-p
	(progn
	  (aset item 1 item-data)
	  (aset item 2 (not (null enabled-p))))
      (setcar item item-name)
      (setcdr item item-data))
    (set-menubar-dirty-flag)
    item))

(defun add-menu-item (menu-path item-name function enabled-p &optional before)
  "Add a menu item to some menu, creating the menu first if necessary.
If the named item exists already, it is changed.
MENU-PATH identifies the menu under which the new menu item should be inserted.
 It is a list of strings; for example, (\"File\") names the top-level \"File\"
 menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
ITEM-NAME is the string naming the menu item to be added.
FUNCTION is the command to invoke when this menu item is selected.
 If it is a symbol, then it is invoked with `call-interactively', in the same
 way that functions bound to keys are invoked.  If it is a list, then the 
 list is simply evaluated.
ENABLED-P controls whether the item is selectable or not.
BEFORE, if provided, is the name of a menu item before which this item should
 be added, if this item is not on the menu already.  If the item is already
 present, it will not be moved."
  (or menu-path (error "must specify a menu path"))
  (or item-name (error "must specify an item name"))
  (add-menu-item-1 t menu-path item-name function enabled-p before))


(defun delete-menu-item (path)
  "Remove the named menu item from the menu hierarchy.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (or (cdr pair) menubar)))
    (if (not item)
	nil
      (delq item menu) ; menus begin with name, so this is ok.
      (set-menubar-dirty-flag)
      item)))


(defun relabel-menu-item (path new-name)
  "Change the string of the specified menu item.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\".
NEW-NAME is the string that the menu item will be printed as from now on."
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (cdr pair)))
    (or item
	(signal 'error (list (if menu "No such menu item" "No such menu")
			     path)))
    (aset item 0 new-name)
    (set-menubar-dirty-flag)
    item))

(defun add-menu (menu-path menu-name menu-items &optional before)
  "Add a menu to the menubar or one of its submenus.
If the named menu exists already, it is changed.
MENU-PATH identifies the menu under which the new menu should be inserted.
 It is a list of strings; for example, (\"File\") names the top-level \"File\"
 menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
 If MENU-PATH is nil, then the menu will be added to the menubar itself.
MENU-NAME is the string naming the menu to be added.
MENU-ITEMS is a list of menu item descriptions.
 Each menu item should be a vector of three elements:
   - a string, the name of the menu item;
   - a symbol naming a command, or a form to evaluate;
   - and t or nil, whether this item is selectable.
BEFORE, if provided, is the name of a menu before which this menu should
 be added, if this menu is not on its parent already.  If the menu is already
 present, it will not be moved."
  (or menu-name (error "must specify a menu name"))
  (or menu-items (error "must specify some menu items"))
  (add-menu-item-1 nil menu-path menu-name menu-items t before))



(defun sensitize-edit-menu-hook (nothing)
  "For use as a value of activate-menubar-hook.
This function changes the sensitivity of the Edit menu items:
  Cut   sensitive only when emacs owns the primary X Selection.
  Copy  sensitive only when emacs owns the primary X Selection.
  Clear sensitive only when emacs owns the primary X Selection.
  Paste sensitive only when there is an owner for the X Clipboard Selection."
  ;;
  ;; the hair in here to not update the menubar unless something has changed
  ;; isn't really necessary (the menubar code is fast enough) but it makes
  ;; me feel better (and creates marginally less list garbage.)
  (let* ((edit-menu (cdr (car (find-menu-item current-menubar '("Edit")))))
	 (cut   (car (find-menu-item edit-menu '("Cut"))))
	 (copy  (car (find-menu-item edit-menu '("Copy"))))
	 (paste (car (find-menu-item edit-menu '("Paste"))))
	 (clear (car (find-menu-item edit-menu '("Clear"))))
	 (undo  (or (car (find-menu-item edit-menu '("Undo")))
		    (car (find-menu-item edit-menu '("Undo More")))))
	 (emacs-owns-selection-p (x-selection-owner-p))
	 (clipboard-exists-p (x-selection-exists-p 'CLIPBOARD))
	 undo-available undoing-more
	 (undo-info-available (not (null (and (not (eq t buffer-undo-list))
				   (if (eq last-command 'undo)
				       (setq undoing-more
					     (and (boundp 'pending-undo-list)
					    pending-undo-list)
				     buffer-undo-list))))))
	 undo-name undo-state
	 (change-p
	  (or (and cut   (not (eq emacs-owns-selection-p (aref cut 2))))
	      (and copy  (not (eq emacs-owns-selection-p (aref copy 2))))
	      (and clear (not (eq emacs-owns-selection-p (aref clear 2))))
	      (and paste (not (eq clipboard-exists-p (aref paste 2)))))))
    (if cut   (aset cut   2 emacs-owns-selection-p))
    (if copy  (aset copy  2 emacs-owns-selection-p))
    (if clear (aset clear 2 emacs-owns-selection-p))
    (if paste (aset paste 2 clipboard-exists-p))

    (if (eq last-command 'undo)
	(setq undo-name "Undo More"
	      undo-state (not (null (and (boundp 'pending-undo-list)
					 pending-undo-list))))
      (setq undo-name "Undo"
	    undo-state (and (not (eq buffer-undo-list t))
			    (not (null
				  (or buffer-undo-list
				      (and (boundp 'pending-undo-list)
					   pending-undo-list)))))))
    (if buffer-read-only (setq undo-state nil))
    (if (and undo
	     (or (not (equal undo-name (aref undo 0)))
		 (not (eq undo-state (aref undo 2)))))
	(progn (aset undo 0 undo-name)
	       (aset undo 2 undo-state)
	       (setq change-p t)))
    ;; if we made any changes, return nil
    ;; otherwise return t to indicate that we haven't done anything.
    (not change-p)))

;; this version is too slow
(defun format-buffers-menu-line (buffer)
  "Returns a string to represent the given buffer in the Buffer menu.
nil means the buffer shouldn't be listed.  You can redefine this."
  (if (string-match "\\` " (buffer-name buffer))
      nil
    (save-excursion
     (set-buffer buffer)
     (let ((size (buffer-size)))
       (format "%s%s %-19s %6s %-15s %s"
	       (if (buffer-modified-p) "*" " ")
	       (if buffer-read-only "%" " ")
	       (buffer-name)
	       size
	       mode-name
	       (or (buffer-file-name) ""))))))
	   
(defun format-buffers-menu-line (buffer)
  (if (string-match "\\` " (setq buffer (buffer-name buffer)))
      nil
    buffer))

(defun build-buffers-menu-hook (nothing)
  "For use as a value of activate-menubar-hook.
This function changes the contents of the \"Buffers\" menu to correspond
to the current set of buffers.  You can control how the text of the menu
items are generated by redefining the function `format-buffers-menu-line'."
  (let ((buffer-menu (car (find-menu-item current-menubar '("Buffers"))))
	name
	buffers)
    (if (not buffer-menu)
	nil
      (setq buffers (buffer-list))
      (if (> (length buffers) 10)
	  (setcdr (nthcdr 10 buffers) nil))
      (setq buffers
	    (mapcar (function
		     (lambda (buffer)
		       (if (setq name (format-buffers-menu-line buffer))
			   (vector name
			     (list 'switch-to-buffer (buffer-name buffer))
			     t))))
		    buffers))
      (setq buffers (nconc (delq nil buffers)
			   '("----" ["List All Buffers" list-buffers t])))
      ;; slightly (only slightly) more efficient to not install the menubar
      ;; if it hasn't visibly changed.
      (if (equal buffers (cdr buffer-menu))
	  t  ; return t meaning "no change"
	(setcdr buffer-menu buffers)
	;; return the now-modified menubar to install.
	nil))))

(or (memq 'build-buffers-menu-hook activate-menubar-hook)
    (setq activate-menubar-hook
	  (cons 'build-buffers-menu-hook activate-menubar-hook)))

(or (memq 'sensitize-edit-menu-hook activate-menubar-hook)
    (setq activate-menubar-hook
	  (cons 'sensitize-edit-menu-hook activate-menubar-hook)))

(set-menubar default-menubar)

(provide 'menubar)
