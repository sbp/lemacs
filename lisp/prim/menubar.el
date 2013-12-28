;; Menubar and popup-menu support.
;; Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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


;; menubar-religion is 'winning or 'losing.  It is symbolic of ongoing
;; internal strife.  Pay no attention to the man behind the curtain.
(defconst menubar-religion 'losing)
(setq menubar-religion 'winning)

(defun purecopy-menubar (x)
  ;; this calls purecopy on the strings, and the contents of the vectors,
  ;; but not on the vectors themselves, or the conses - those must be
  ;; writable.
  (cond ((vectorp x)
	 (let ((i (length x)))
	   (while (> i 0)
	     (aset x (1- i) (purecopy (aref x (1- i))))
	     (setq i (1- i))))
	 x)
	((consp x)
	 (let ((rest x))
	   (while rest
	     (setcar rest (purecopy-menubar (car rest)))
	     (setq rest (cdr rest))))
	 x)
	(t
	 (purecopy x))))

(defconst default-menubar
  (purecopy-menubar
  (cons
   (if (eq menubar-religion 'winning)
       '("File"
		["Open..."		find-file		 t]
		["Open in New Screen..." find-file-other-screen  t]
		["Insert File..." 	insert-file		 t]
		"------"
		["Save"			save-buffer		 t  nil]
		["Save As..."		write-file		 t]
		"-----"
		["Print Buffer"		lpr-buffer		 t  nil]
		"-----"
		["New Screen"		make-screen		t]
		["Delete Screen"	delete-screen		t]
		["Split Screen"		split-window-vertically t]
		["Un-Split (Keep This)"	delete-other-windows
							(not (one-window-p t))]
		["Un-Split (Keep Others)"	delete-window
							(not (one-window-p t))]
		"-----"
		["Revert Buffer"	revert-buffer		 t  nil]
;;		["Kill Buffer..."	kill-buffer		 t]
		["Kill Buffer"		kill-this-buffer	 t  nil]
		"-----"
		["Exit Emacs"		save-buffers-kill-emacs	t]
		)
     '("File"
		["New Screen"		make-screen		t]
		["Open..."		find-file		t]
		["Save"			save-buffer		t  nil]
		["Save As..."		write-file		t]
		"------"
		["Insert File..." 	insert-file		t]
		"-----"
		["Print Buffer"		lpr-buffer		t  nil]
		"-----"
		["Delete Screen"	delete-screen		t]
;;		["Kill Buffer..."	kill-buffer		t]
		["Delete Buffer"	kill-this-buffer	t  nil]
		["Revert Buffer"	revert-buffer		t  nil]
		"-----"
		["Exit Emacs"		save-buffers-kill-emacs	t]
		)
     )
  '(
    ("Edit"	["Undo"			advertised-undo		   t]
		["Cut"			x-kill-primary-selection   t]
		["Copy"			x-copy-primary-selection   t]
		["Paste"		x-yank-clipboard-selection t]
		["Clear"		x-delete-primary-selection t]
		"----"
		["Start Macro Recording" start-kbd-macro
		 (not defining-kbd-macro)]
		["End Macro Recording"	end-kbd-macro
		 defining-kbd-macro]
		["Execute Last Macro"	call-last-kbd-macro
		 last-kbd-macro]
		)
    
    ("Options"
     ["Read Only" toggle-read-only :style toggle :selected buffer-read-only]
     ["Case Sensitive Search" (setq case-fold-search (not case-fold-search))
      :style toggle :selected (not case-fold-search)]
     ["Overstrike" overwrite-mode :style toggle :selected overwrite-mode]
     ["Auto Delete Selection" (if (memq 'pending-delete-pre-hook
					pre-command-hook)
				  (pending-delete-off nil)
				(pending-delete-on nil))
      :style toggle :selected (memq 'pending-delete-pre-hook pre-command-hook)]
     ["Teach Extended Commands" (setq teach-extended-commands-p
				      (not teach-extended-commands-p))
      :style toggle :selected teach-extended-commands-p]
;     ["Line Numbers" (line-number-mode nil)
;      :style toggle :selected line-number-mode]
     ("Syntax Highlighting" 
      ["None" (font-lock-mode 0) :style radio :selected (null font-lock-mode)]
      ["Fonts" (progn (require 'font-lock)
		      (font-lock-use-default-fonts)
		      (font-lock-mode 1))
       :style radio
       :selected (and font-lock-mode
		      (equal (find-face 'italic)  ; kind of a kludge...
			     (find-face 'font-lock-comment-face)))]
      ["Colors" (progn (require 'font-lock)
		       (font-lock-use-default-colors)
		       (font-lock-mode 1))
       :style radio
       :selected (and font-lock-mode
		      (not (equal (find-face 'italic)
				  (find-face 'font-lock-comment-face))))]
      "-----"
      ["Less" (progn (require 'font-lock)
		     (font-lock-use-default-minimal-decoration)
		     (font-lock-mode 0)
		     (font-lock-mode 1))
       :style radio
       :selected (and font-lock-mode
		      (eq c++-font-lock-keywords c-font-lock-keywords-1))]
      ["More" (progn (require 'font-lock)
		     (font-lock-use-default-maximal-decoration)
		     (font-lock-mode 0)
		     (font-lock-mode 1))
       :style radio
       :selected (and font-lock-mode
		      (eq c++-font-lock-keywords c-font-lock-keywords-2))]
      )
     ("Paren Highlighting"
      ["None" (blink-paren 0)
       :style radio
       :selected (not (memq 'blink-paren-pre-command pre-command-hook))]
      ["Blink" (progn
		 (setq highlight-paren-expression nil)
		 (blink-paren 1))
       :style radio
       :selected (and (not highlight-paren-expression)
		      (memq 'blink-paren-pre-command pre-command-hook))]
      ["Highlight" (progn
		     (setq highlight-paren-expression t)
		     (blink-paren 1))
       :style radio
       :selected (and highlight-paren-expression
		      (memq 'blink-paren-pre-command pre-command-hook))]
      )
     "------"
     ("Font"	"initialized later")
     ("Size"	"initialized later")
     ("Weight"	"initialized later")
     "-----"
     ["Buffers Menu Length..."
      (progn
	(setq buffers-menu-max-size
	      (read-number
	       "Enter number of buffers to display (or 0 for unlimited): "))
	(if (eq buffers-menu-max-size 0) (setq buffers-menu-max-size nil)))
      t]
     ["Buffers Sub-Menus" (setq complex-buffers-menu-p
				(not complex-buffers-menu-p))
      :style toggle :selected complex-buffers-menu-p]
     "-----"
     ["Save Options" save-options-menu-settings t]
     )

    ("Buffers"	"")

    nil		; the partition: menus after this are flushright

    ("Help"	["About Lucid Emacs..."	about-lucid-emacs	t]
		"-----"
		["Info"			info			t]
		["Describe Mode"	describe-mode		t]
		["Command Apropos..."	command-apropos		t]
		["Full Apropos..."	apropos			t]
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
  ))


(defun kill-this-buffer ()	; for the menubar
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; #### shouldn't this perhaps be `copy-tree'?
(defun set-menubar (menubar)
  "Set the default menubar to be MENUBAR.
See `current-menubar' for a description of the syntax of a menubar."
  (check-menu-syntax menubar t)
  (setq-default current-menubar (copy-sequence menubar))
  (set-menubar-dirty-flag))

(defun set-buffer-menubar (menubar)
  "Set the buffer-local menubar to be MENUBAR.
See `current-menubar' for a description of the syntax of a menubar."
  (check-menu-syntax menubar t)
  (make-local-variable 'current-menubar)
  (setq current-menubar (copy-sequence menubar))
  (set-menubar-dirty-flag))

(defun check-menu-syntax (menu &optional menubar-p)
  ;; The C code does syntax checking on the value of `current-menubar',
  ;; but it's better to do it early, before things have gotten messed up.
  (if menubar-p
      nil
    (or (stringp (car menu))
	(signal 'error
		(list "menu name (first element) must be a string" menu)))
    ;;(or (cdr menu) (signal 'error (list "menu is empty" menu)))
    (setq menu (cdr menu)))
  (while menu
    (cond
     ((stringp (car menu)))
     ((null (car menu)))
     ((consp (car menu))
      (check-menu-syntax (car menu)))
     ((vectorp (car menu))
      (let ((L (length (car menu)))
	    plistp)
      (cond ((< L 3)
	     (signal 'error
		     (list "button descriptors must be at least 3 long"
			   (car menu))))
	    ((= L 3)
	     (setq plistp nil))
	    ((= L 4)
	     (setq plistp
		   (and (symbolp (aref (car menu) 2))
			(= ?: (aref (symbol-name (aref (car menu) 2)) 0)))))
	    (t (setq plistp t)))
      (or (stringp (aref (car menu) 0))
	  (signal 'error
		  (list
		   "first element of a button must be a string (the label)"
		   (car menu))))
      (or plistp
	  (< L 4)
	  (null (aref (car menu) 3))
	  (stringp (aref (car menu) 3))
	  (signal 'error
		  (list
	      "fourth element of a button must be a string (the label suffix)"
		   (car menu))))
      (if plistp
	  (let ((i 2)
		selp
		style
		item)
	    (while (< i L)
	      (cond ((not (memq (setq item (aref (car menu) i))
				'(:active :suffix :keys :style :selected)))
		     (signal 'error
			     (list (if (symbolp item)
				       "unknown menu item keyword"
				     "not a keyword")
				   item (car menu))))
		    ((eq item ':style)
		     (setq style (aref (car menu) (1+ i)))
		     (or (memq style '(nil toggle radio text))
			 (signal 'error (list "unknown style" style
					      (car menu)))))
		    ((eq item ':selected) (setq selp t))
		    )
	      (setq i (+ i 2)))
	    (if (and selp (not (memq style '(toggle radio))))
		(signal 'error
			(list
		      ":selected only makes sense with :style toggle or radio"
		      (car menu))))
	    )))
      )
     (t (signal 'error (list "unrecognised menu descriptor" (car menu)))))
    (setq menu (cdr menu))))


;;; menu manipulation functions

(defun find-menu-item (menubar item-path-list &optional parent)
  "Search MENUBAR for item given by ITEM-PATH-LIST starting from PARENT.
Returns (ITEM . PARENT), where PARENT is the immediate parent of
 the item found.
If the item does not exist, the car of the returned value is nil.
If some menu in the ITEM-PATH-LIST does not exist, an error is signalled."
  (or (listp item-path-list)
      (signal 'wrong-type-argument (list 'listp item-path-list)))
  (or parent (setq item-path-list (mapcar 'downcase item-path-list)))
  (if (not (consp menubar))
      nil
    (let ((rest menubar)
	  result)
      (while rest
	(if (and (car rest)
		 (equal (car item-path-list)
			(downcase (if (vectorp (car rest))
				      (aref (car rest) 0)
				    (if (stringp (car rest))
					(car rest)
				      (car (car rest)))))))
	    (setq result (car rest) rest nil)
	  (setq rest (cdr rest))))
      (if (cdr item-path-list)
	  (if (consp result)
	      (find-menu-item (cdr result) (cdr item-path-list) result)
	    (if result
		(signal 'error (list (gettext "not a submenu") result))
	      (signal 'error (list (gettext "no such submenu") (car item-path-list)))))
	(cons result parent)))))


(defun enable-menu-item-1 (path toggle-p on-p)
  (let (menu item)
    (if (and (vectorp path) (> (length path) 2)) ; limited syntax checking...
	(setq item path)
      (let* ((menubar current-menubar)
	     (pair (find-menu-item menubar path)))
	(setq item (car pair)
	      menu (cdr pair))
	(or item
	    (signal 'error (list (if menu
				     "No such menu item"
				   "No such menu")
				 path)))
	(if (consp item)
	    (error "%S is a menu, not a menu item" path))))
    (if (or (> (length item) 4)
	    (and (symbolp (aref item 2))
		 (= ?: (aref (symbol-name (aref item 2)) 0))))
	;; plist-like syntax
	(let ((i 2)
	      (keyword (if toggle-p ':selected ':active))
	      (ok nil))
	  (while (< i (length item))
	    (cond ((eq (aref item i) keyword)
		   (aset item (1+ i) on-p)
		   (setq ok t)))
	    (setq i (+ i 2)))
	  (cond (ok nil)
		(toggle-p
		 (signal 'error (list "not a toggle menu item" item)))
		(t
		 ;; Need to copy the item to extend it, sigh...
		 (let ((cons (memq item menu))
		       (new-item (vconcat item (list keyword on-p))))
		   (if cons
		       (setcar cons (setq item new-item))
		     (if menu
			 (error "couldn't find %S on its parent?" item)
		       (error "no %S slot to set: %S" keyword item)))))))
      ;; positional syntax
      (if toggle-p
	  (signal 'error (list "not a toggle menu item" item))
	(aset item 2 on-p)))
    (set-menubar-dirty-flag)
    item))

(defun enable-menu-item (path)
  "Make the named menu item be selectable.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (enable-menu-item-1 path nil t))

(defun disable-menu-item (path)
  "Make the named menu item be unselectable.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (enable-menu-item-1 path nil nil))

(defun select-toggle-menu-item (path)
  "Make the named toggle- or radio-style menu item be in the `selected' state.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (enable-menu-item-1 path t t))

(defun deselect-toggle-menu-item (path)
 "Make the named toggle- or radio-style menu item be in the `unselected' state.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\"."
  (enable-menu-item-1 path t nil))


(defun add-menu-item-1 (item-p menu-path item-name item-data enabled-p before)
  (if before (setq before (downcase before)))
  (let* ((item-name-tail (and (consp item-name)
			      (prog1 (cdr item-name)
				(setq item-name (car item-name)))))
	 (menubar current-menubar)
	 (menu (condition-case ()
		   (car (find-menu-item menubar menu-path))
		 (error nil)))
	 (item (cond ((not (listp menu))
		      (signal 'error (list (gettext "not a submenu")
					   menu-path)))
		     (menu
		      (car (find-menu-item (cdr menu) (list item-name))))
		     (t
		      (car (find-menu-item menubar (list item-name))))
		     )))
    (or menubar
	(error "current-menubar is nil: can't add menus to it."))
    (or menu
	(let ((rest menu-path)
	      (so-far menubar))
	  (while rest
;;;	    (setq menu (car (find-menu-item (cdr so-far) (list (car rest)))))
	    (setq menu
		  (if (eq so-far menubar)
		      (car (find-menu-item so-far (list (car rest))))
		    (car (find-menu-item (cdr so-far) (list (car rest))))))
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
	  (setq item (if item-name-tail
			 (vector item-name item-data enabled-p item-name-tail)
		       (vector item-name item-data enabled-p)))
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
	(if (not added-before)
	    ;; adding before the first item on the menubar itself is harder
	    (if (and (eq menu menubar) (eq before (car menu)))
		(setq menu (cons item menu)
		      current-menubar menu)
	      ;; otherwise, add the item to the end.
	      (nconc menu (list item))))))
    (if item-p
	(progn
	  (aset item 0 item-name)
	  (aset item 1 item-data)
	  (aset item 2 enabled-p))
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
ENABLED-P controls whether the item is selectable or not.  It may be t, nil,
 or a form to evaluate.  It will be evaluated each time the menubar is 
 activated.
BEFORE, if provided, is the name of a menu item before which this item should
 be added, if this item is not on the menu already.  If the item is already
 present, it will not be moved."
;  (or menu-path (error "must specify a menu path"))
  (or item-name (error (gettext "must specify an item name")))
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
      ;; the menubar is the only special case, because other menus begin
      ;; with their name.
      (if (eq menu current-menubar)
	  (setq current-menubar (delq item menu))
	(delq item menu))
      (set-menubar-dirty-flag)
      item)))


(defun relabel-menu-item (path new-name)
  "Change the string of the specified menu item.
PATH is a list of strings which identify the position of the menu item in 
the menu hierarchy.  (\"File\" \"Save\") means the menu item called \"Save\"
under the toplevel \"File\" menu.  (\"Menu\" \"Foo\" \"Item\") means the 
menu item called \"Item\" under the \"Foo\" submenu of \"Menu\".
NEW-NAME is the string that the menu item will be printed as from now on."
  (or (stringp new-name)
      (setq new-name (signal 'wrong-type-argument (list 'stringp new-name))))
  (let* ((menubar current-menubar)
	 (pair (find-menu-item menubar path))
	 (item (car pair))
	 (menu (cdr pair)))
    (or item
	(signal 'error (list (if menu (gettext "No such menu item")
			       (gettext "No such menu"))
			     path)))
    (if (and (consp item)
	     (stringp (car item)))
	(setcar item new-name)
      (aset item 0 new-name))
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
 See documentation of variable `current-menubar' for the syntax.
BEFORE, if provided, is the name of a menu before which this menu should
 be added, if this menu is not on its parent already.  If the menu is already
 present, it will not be moved."
  (or menu-name (error (gettext "must specify a menu name")))
  (or menu-items (error (gettext "must specify some menu items")))
  (check-menu-syntax menu-items t)
  (add-menu-item-1 nil menu-path menu-name menu-items t before))


;;; The File and Edit menus

(defvar put-buffer-names-in-file-menu t)

;; The sensitivity part of this function could be done by just adding forms
;; to evaluate to the menu items themselves; that would be marginally less
;; efficient but not perceptibly so (I think.)  But in order to change the
;; names of the Undo menu item and the various things on the File menu item,
;; we need to use a hook.
;;
(defun sensitize-file-and-edit-menus-hook ()
  "For use as a value of activate-menubar-hook.
This function changes the sensitivity of these File and Edit menu items:

  Cut    sensitive only when emacs owns the primary X Selection.
  Copy   sensitive only when emacs owns the primary X Selection.
  Clear  sensitive only when emacs owns the primary X Selection.
  Paste  sensitive only when there is an owner for the X Clipboard Selection.
  Undo   sensitive only when there is undo information.
         While in the midst of an undo, this is changed to \"Undo More\".

  Kill Buffer    has the name of the current buffer appended to it.
  Print Buffer   has the name of the current buffer appended to it.
  Save           has the name of the current buffer appended to it, and is
                 sensitive only when the current buffer is modified.
  Revert Buffer  has the name of the current buffer appended to it, and is
                 sensitive only when the current buffer has a file.
  Delete Screen  sensitive only when there is more than one screen."
  ;;
  ;; the hair in here to not update the menubar unless something has changed
  ;; isn't really necessary (the menubar code is fast enough) but it makes
  ;; me feel better (and creates marginally less list garbage.)
  (let* ((file-menu (cdr (car (find-menu-item current-menubar '("File")))))
	 (edit-menu (cdr (car (find-menu-item current-menubar '("Edit")))))
	 (save	(or (car (find-menu-item file-menu '("Save")))
		    ;; menubar-religion=losing
		    (car (find-menu-item file-menu '("Save Buffer")))))
	 (rvt   (car (find-menu-item file-menu '("Revert Buffer"))))
	 (del   (car (find-menu-item file-menu '("Delete Screen"))))
	 (print (car (find-menu-item file-menu '("Print Buffer"))))
	 (kill  (or (car (find-menu-item file-menu '("Kill Buffer")))
		    ;; menubar-religion=losing
		    (car (find-menu-item file-menu '("Delete Buffer")))))
	 (cut   (car (find-menu-item edit-menu '("Cut"))))
	 (copy  (car (find-menu-item edit-menu '("Copy"))))
	 (paste (car (find-menu-item edit-menu '("Paste"))))
	 (clear (car (find-menu-item edit-menu '("Clear"))))
	 (undo  (or (car (find-menu-item edit-menu '("Undo")))
		    (car (find-menu-item edit-menu '("Undo More")))))
	 (name (buffer-name))
	 (emacs-owns-selection-p (x-selection-owner-p))
	 (clipboard-exists-p (x-selection-exists-p 'CLIPBOARD))
;;	 undo-available undoing-more
;;	 (undo-info-available (not (null (and (not (eq t buffer-undo-list))
;;				   (if (eq last-command 'undo)
;;				       (setq undoing-more
;;					     (and (boundp 'pending-undo-list)
;;					    pending-undo-list)
;;				     buffer-undo-list))))))
	 undo-name undo-state
	 (change-p
	  (or (and cut   (not (eq emacs-owns-selection-p (aref cut 2))))
	      (and copy  (not (eq emacs-owns-selection-p (aref copy 2))))
	      (and clear (not (eq emacs-owns-selection-p (aref clear 2))))
	      (and paste (not (eq clipboard-exists-p (aref paste 2))))
	      (and save  (not (eq (buffer-modified-p) (aref save 2))))
	      (and rvt   (not (eq (not (not buffer-file-name)) (aref rvt 2))))
	      (and del   (eq (eq (next-screen) (selected-screen))
			     (aref del 2)))
	      )))
    (if (not put-buffer-names-in-file-menu)
	nil
      (if (= (length save)  4) (progn (aset save  3 name) (setq change-p t)))
      (if (= (length rvt)   4) (progn (aset rvt   3 name) (setq change-p t)))
      (if (= (length print) 4) (progn (aset print 3 name) (setq change-p t)))
      (if (= (length kill)  4) (progn (aset kill  3 name) (setq change-p t))))
    (if save  (aset save  2 (buffer-modified-p)))
    ;; revert is sensitive if there is a file name, or a revert method (since
    ;; who knows what that might do.)
    (if rvt   (aset rvt   2 (not (not (or buffer-file-name
					  revert-buffer-function)))))
    (if del   (aset del   2 (not (eq (next-screen) (selected-screen)))))
    (if cut   (aset cut   2 emacs-owns-selection-p))
    (if copy  (aset copy  2 emacs-owns-selection-p))
    (if clear (aset clear 2 emacs-owns-selection-p))
    (if paste (aset paste 2 clipboard-exists-p))

    ;; we could also do this with the third field of the item.
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

(add-hook 'activate-menubar-hook 'sensitize-file-and-edit-menus-hook)


;;; The Buffers menu

;; this version is too slow
(defun slow-format-buffers-menu-line (buffer)
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
  "Returns a string to represent the given buffer in the Buffer menu.
nil means the buffer shouldn't be listed.  You can redefine this."
  (if (string-match "\\` " (setq buffer (buffer-name buffer)))
      nil
    buffer))

(defvar buffers-menu-max-size 20
  "*Maximum number of entries which may appear on the \"Buffers\" menu.
If this is 10, then only the ten most-recently-selected buffers will be
shown.  If this is nil, then all buffers will be shown.  Setting this to
a large number or nil will slow down menu responsiveness.")

(defvar complex-buffers-menu-p nil
  "*If true, the buffers menu will contain several commands, as submenus
of each buffer line.  If this is false, then there will be only one command:
select that buffer.")

(defvar buffers-menu-switch-to-buffer-function 'switch-to-buffer
  "*The function to call to select a buffer from the buffers menu.
`switch-to-buffer' is a good choice, as is `pop-to-buffer'.")


(defun buffer-menu-save-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (save-buffer)))

(defun buffer-menu-write-file (buffer)
  (save-excursion
    (set-buffer buffer)
    (write-file (read-file-name
		 (format (gettext "Write %s to file: ")
			 (buffer-name (current-buffer)))))))

(defsubst build-buffers-menu-internal (buffers)
  (let (name line)
    (mapcar
     (if complex-buffers-menu-p
	 #'(lambda (buffer)
	     (if (setq line (format-buffers-menu-line buffer))
		 (delq nil
		   (list line
		       (vector "Switch to Buffer"
			       (list buffers-menu-switch-to-buffer-function
				     (setq name (buffer-name buffer)))
			       t)
		       (if (eq buffers-menu-switch-to-buffer-function
			       'switch-to-buffer)
			   (vector "Switch to Buffer, Other Screen"
				   (list 'switch-to-buffer-other-screen
					 (setq name (buffer-name buffer)))
				   t)
			 nil)
		       (if (and (buffer-modified-p buffer)
				(buffer-file-name buffer))
			   (vector (if (eq menubar-religion 'winning)
				       "Save"
				     "Save Buffer")
				   (list 'buffer-menu-save-buffer name) t)
			 (if (eq menubar-religion 'winning)
			     ["Save" nil nil]
			   ["Save Buffer" nil nil]
			   ))
		       (vector (if (eq menubar-religion 'winning)
				   "Save As..."
				 "Save Buffer As...")
			       (list 'buffer-menu-write-file name) t)
		       (vector "Kill Buffer" (list 'kill-buffer name) t)))))
       #'(lambda (buffer)
	   (if (setq line (format-buffers-menu-line buffer))
	       (vector line
		       (list buffers-menu-switch-to-buffer-function
			     (buffer-name buffer))
		       t))))
     buffers)))

(defun build-buffers-menu-hook ()
  "For use as a value of activate-menubar-hook.
This function changes the contents of the \"Buffers\" menu to correspond
to the current set of buffers.  Only the most-recently-used few buffers
will be listed on the menu, for efficiency reasons.  You can control how
many buffers will be shown by setting `buffers-menu-max-size'.
You can control the text of the menu items by redefining the function
`format-buffers-menu-line'."
  (let ((buffer-menu (car (find-menu-item current-menubar '("Buffers"))))
	buffers)
    (if (not buffer-menu)
	nil
      (setq buffers (buffer-list))

      (if (and (integerp buffers-menu-max-size)
	       (> buffers-menu-max-size 1))
	  (if (> (length buffers) buffers-menu-max-size)
	      (setcdr (nthcdr buffers-menu-max-size buffers) nil)))

      (setq buffers (build-buffers-menu-internal buffers))
      (setq buffers (append (delq nil buffers)
			   '("----" ["List All Buffers" list-buffers t])))
      ;; slightly (only slightly) more efficient to not install the menubar
      ;; if it hasn't visibly changed.
      (if (equal buffers (cdr buffer-menu))
	  t  ; return t meaning "no change"
	(setcdr buffer-menu buffers)
	nil))))

(add-hook 'activate-menubar-hook 'build-buffers-menu-hook)


;;; The Options menu

;; make sure these have a value, even if not loaded
(defvar c++-mode-hook nil)
(defvar lisp-mode-hook nil)
(defvar emacs-lisp-mode-hook nil)

(defconst options-menu-saved-forms
  ;; This is really quite a kludge, but it gets the job done.
  (purecopy
   '(highlight-paren-expression
     overwrite-mode
     teach-extended-commands-p
     complex-buffers-menu-p
     buffers-menu-max-size
     case-fold-search
     (if (memq 'blink-paren-pre-command pre-command-hook)
	 '(blink-paren 1)
       '(if (featurep 'blink-paren) (blink-paren 0)))
     (if (memq 'pending-delete-pre-hook pre-command-hook)
	 '(pending-delete 1)
       '(if (featurep 'pending-del)
	    (pending-delete 0)))
     (list 'set-face-font ''default (face-font-name 'default))
     (list 'set-face-font ''modeline (face-font-name 'modeline))
     (if (memq 'turn-on-font-lock c-mode-hook)
	 '(add-hook 'c-mode-hook 'turn-on-font-lock)
       '(remove-hook 'c-mode-hook 'turn-on-font-lock))
     (if (memq 'turn-on-font-lock c++-mode-hook)
	 '(add-hook 'c++-mode-hook 'turn-on-font-lock)
       '(remove-hook 'c++-mode-hook 'turn-on-font-lock))
     (if (memq 'turn-on-font-lock lisp-mode-hook)
	 '(add-hook 'lisp-mode-hook 'turn-on-font-lock)
       '(remove-hook 'lisp-mode-hook 'turn-on-font-lock))
     (if (memq 'turn-on-font-lock emacs-lisp-mode-hook)
	 '(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
       '(remove-hook 'emacs-lisp-mode-hook 'turn-on-font-lock))
     (if (featurep 'font-lock)
	 '(require 'font-lock))
     (cond ((eq c-font-lock-keywords c-font-lock-keywords-1)
	    '(setq c-font-lock-keywords c-font-lock-keywords-1))
	   ((eq c-font-lock-keywords c-font-lock-keywords-2)
	    '(setq c-font-lock-keywords c-font-lock-keywords-2)))
     (cond ((eq c++-font-lock-keywords c-font-lock-keywords-1)
	    '(setq c++-font-lock-keywords c-font-lock-keywords-1))
	   ((eq c++-font-lock-keywords c-font-lock-keywords-2)
	    '(setq c++-font-lock-keywords c-font-lock-keywords-2)))
     (cond ((eq lisp-font-lock-keywords lisp-font-lock-keywords-1)
	    '(setq lisp-font-lock-keywords lisp-font-lock-keywords-1))
	   ((eq lisp-font-lock-keywords lisp-font-lock-keywords-2)
	    '(setq lisp-font-lock-keywords lisp-font-lock-keywords-2)))
     (cons 'progn
	   (apply 'nconc
		  (mapcar
		   #'(lambda (face)
		       (delq nil
			     (list
			      (if (face-foreground face)
				  (list 'set-face-foreground (list 'quote face)
					(pixel-name (face-foreground face))))
			      (if (face-background face)
				  (list 'set-face-background (list 'quote face)
					(pixel-name (face-background face))))
			      (if (face-font face)
				  (list 'set-face-font (list 'quote face)
					(face-font-name face)))
			      )))
		   '(font-lock-comment-face font-lock-string-face
		     font-lock-doc-string-face font-lock-function-name-face
		     font-lock-keyword-face font-lock-type-face))))
     ))
  "The variables to save; or forms to evaluate to get forms to write out.")


(defun save-options-menu-settings ()
  "Saves the current settings of the `Options' menu to your `.emacs' file."
  (interactive)
  (let ((output-buffer (find-file-noselect
			(concat "~" init-file-user "/.emacs")))
	output-marker)
    (save-excursion
      (set-buffer output-buffer)
      ;;
      ;; Delete the old format saved data, if any.
      ;; (This is for the old, Energize-specific, Options menu and should
      ;; go away once Energize 3.0 is released.)
      ;;
      (goto-char (point-min))
      (if (re-search-forward "^(and (fboundp 'energize-menu-restore-saved-options)\n     (energize-menu-restore-saved-options '.*))[ \t]*\n?" nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
      ;;
      ;; Find and delete the previously saved data, and position to write.
      ;;
      (goto-char (point-min))
      (if (re-search-forward "^;; Options Menu Settings *\n" nil 'move)
	  (let ((p (match-beginning 0)))
	    (goto-char p)
	    (or (re-search-forward
		 "^;; End of Options Menu Settings *\\(\n\\|\\'\\)"
		 nil t)
		(error "can't find END of saved state in .emacs"))
	    (delete-region p (match-end 0)))
	(goto-char (point-max))
	(insert "\n"))
      (setq output-marker (point-marker))
      )

    ;; run with current-buffer unchanged so that variables are evaluated in
    ;; the current context, instead of in the context of the ".emacs" buffer.
    (let ((print-readably t)
	  (print-escape-newlines t)
	  (standard-output output-marker))
      (princ ";; Options Menu Settings\n")
      (princ ";; =====================\n")
      (princ "(cond\n")
      (princ " ((and (string-match \"Lucid\" emacs-version)\n")
      (princ "       (boundp 'emacs-major-version)\n")
      (princ "       (= emacs-major-version 19)\n")
      (princ "       (>= emacs-minor-version 10))\n")
      (mapcar #'(lambda (var)
		  (princ "  ")
		  (if (symbolp var)
		      (prin1 (list 'setq-default var
				   (let ((val (symbol-value var)))
				     (if (or (memq val '(t nil))
					     (not (symbolp val)))
					 val
				       (list 'quote val)))))
		    (setq var (eval var))
		    (cond ((eq (car-safe var) 'progn)
			   (while (setq var (cdr var))
			     (prin1 (car var))
			     (princ "\n")
			     (if (cdr var) (princ "  "))
			     ))
			  (var
			   (prin1 var))))
		  (if var (princ "\n")))
	      options-menu-saved-forms)
      (princ "  ))\n")
      (princ ";; ============================\n")
      (princ ";; End of Options Menu Settings\n")
      )
    (set-marker output-marker nil)
    (save-excursion
      (set-buffer output-buffer)
      (save-buffer))
    ))


(set-menubar default-menubar)


;;; Popup menus.

(defconst default-popup-menu
  '("Emacs Commands"
    ["Undo"		advertised-undo		t]
    ["Cut"		x-kill-primary-selection   t]
    ["Copy"		x-copy-primary-selection   t]
    ["Paste"		x-yank-clipboard-selection t]
    "-----"
    ["Select Block"	mark-paragraph 		t]
    ["Split Window"	(split-window)		t]
    ["Unsplit Window" 	delete-other-windows	t]
    ))

(defvar global-popup-menu default-popup-menu
  "The global popup menu.  This is present in all modes.
See the function `popup-menu' for a description of menu syntax.")

(defvar mode-popup-menu nil
  "The mode-specific popup menu.  Automatically buffer local.
This is appended to the default items in `global-popup-menu'.
See the function `popup-menu' for a description of menu syntax.")
(make-variable-buffer-local 'mode-popup-menu)

(defvar activate-popup-menu-hook nil
  "Function or functions run before a mode-specific popup menu is made visible.
These functions are called with no arguments, and should interrogate and
modify the value of `global-popup-menu' or `mode-popup-menu' as desired.
Note: this hook is only run if you use `popup-mode-menu' for activating the
global and mode-specific commands; if you have your own binding for button3,
this hook won't be run.")

(defun popup-mode-menu ()
  "Pop up a menu of global and mode-specific commands.
The menu is computed by combining `global-popup-menu' and `mode-popup-menu'."
  (interactive "@")
  (run-hooks 'activate-popup-menu-hook)
  (popup-menu
   (cond ((and global-popup-menu mode-popup-menu)
	  (check-menu-syntax mode-popup-menu)
	  (let ((title (car mode-popup-menu))
		(items (cdr mode-popup-menu)))
	    (append global-popup-menu
		    '("---" "---")
		    (if popup-menu-titles (list title))
		    (if popup-menu-titles '("---" "---"))
		    items)))
	 (t
	  (or mode-popup-menu
	      global-popup-menu
	      (error "No menu here."))))))

(global-set-key 'button3 'popup-mode-menu)

(provide 'menubar)
