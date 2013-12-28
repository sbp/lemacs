;; Menubar support.
;; Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.

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
    ;;>>> purecopy??
  '(("File"	["New Screen"		x-new-screen		t]
		["Open File..."		find-file		t]
		["Save Buffer"		save-buffer		t  nil]
		["Save Buffer As..."	write-file		t]
		["Revert Buffer"	revert-buffer		t  nil]
		"-----"
		["Print Buffer"		lpr-buffer		t  nil]
		"-----"
		["Delete Screen"	delete-screen		t]
;;		["Kill Buffer..."	kill-buffer		t]
		["Kill Buffer"		kill-this-buffer	t  nil]
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


(defun kill-this-buffer ()	; for the menubar
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun x-new-screen (&optional screen-name)
  "Creates a new emacs screen (that is, a new X window.)"
  (interactive)
  (select-screen (x-create-screen
		  (append (if screen-name
			      (list (cons 'name screen-name))
			    nil)
			  screen-default-alist)))
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

(defun find-menu-item (menubar item-path-list &optional parent)
  "Searches MENUBAR for item given by ITEM-PATH-LIST starting from PARENT.
Returns (ITEM . PARENT), where PARENT is the immediate parent of
 the item found.
Signals an error if the item is not found."
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
		(signal 'error (list "not a submenu" result))
	      (signal 'error (list "no such submenu" (car item-path-list)))))
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
    (if (consp item) (error "can't disable menus, only menu items"))
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
    (if (consp item) (error "%S is a menu, not a menu item" path))
    (aset item 2 t)
    (set-menubar-dirty-flag)
    item))


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
		      (signal 'error (list "not a submenu" menu-path)))
		     (menu
		      (car (find-menu-item (cdr menu) (list item-name))))
		     (t
		      (car (find-menu-item menubar (list item-name))))
		     )))
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
ENABLED-P controls whether the item is selectable or not.  It may be t, nil,
 or a form to evaluate.  It will be evaluated each time the menubar is 
 activated.
BEFORE, if provided, is the name of a menu item before which this item should
 be added, if this item is not on the menu already.  If the item is already
 present, it will not be moved."
;  (or menu-path (error "must specify a menu path"))
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
	(signal 'error (list (if menu "No such menu item" "No such menu")
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
 Each menu item should be a vector of three elements:
   - a string, the name of the menu item;
   - a symbol naming a command, or a form to evaluate;
   - and t, nil, or a form to evaluate: whether this item is selectable.
BEFORE, if provided, is the name of a menu before which this menu should
 be added, if this menu is not on its parent already.  If the menu is already
 present, it will not be moved."
  (or menu-name (error "must specify a menu name"))
  (or menu-items (error "must specify some menu items"))
  (add-menu-item-1 nil menu-path menu-name menu-items t before))



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
  Save Buffer    has the name of the current buffer appended to it, and is
                 sensitive only when the current buffer is modified.
  Revert Buffer  has the name of the current buffer appended to it, and is
                 sensitive only when the current buffer has a file.
  Delete Screen  sensitive only when there is more than one visible screen."
  ;;
  ;; the hair in here to not update the menubar unless something has changed
  ;; isn't really necessary (the menubar code is fast enough) but it makes
  ;; me feel better (and creates marginally less list garbage.)
  (let* ((file-menu (cdr (car (find-menu-item current-menubar '("File")))))
	 (edit-menu (cdr (car (find-menu-item current-menubar '("Edit")))))
	 (save	(car (find-menu-item file-menu '("Save Buffer"))))
	 (rvt   (car (find-menu-item file-menu '("Revert Buffer"))))
	 (del   (car (find-menu-item file-menu '("Delete Screen"))))
	 (print (car (find-menu-item file-menu '("Print Buffer"))))
	 (kill  (car (find-menu-item file-menu '("Kill Buffer"))))
	 (cut   (car (find-menu-item edit-menu '("Cut"))))
	 (copy  (car (find-menu-item edit-menu '("Copy"))))
	 (paste (car (find-menu-item edit-menu '("Paste"))))
	 (clear (car (find-menu-item edit-menu '("Clear"))))
	 (undo  (or (car (find-menu-item edit-menu '("Undo")))
		    (car (find-menu-item edit-menu '("Undo More")))))
	 (name (buffer-name))
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
	      (and paste (not (eq clipboard-exists-p (aref paste 2))))
	      (and save  (not (eq (buffer-modified-p) (aref save 2))))
	      (and rvt   (not (eq (not (not buffer-file-name)) (aref rvt 2))))
	      (and del   (eq (eq (next-screen nil nil t) (selected-screen))
			     (aref del 2)))
	      )))
    (if (not put-buffer-names-in-file-menu)
	nil
      (if (= (length save)  4) (progn (aset save  3 name) (setq change-p t)))
      (if (= (length rvt)   4) (progn (aset rvt   3 name) (setq change-p t)))
      (if (= (length print) 4) (progn (aset print 3 name) (setq change-p t)))
      (if (= (length kill)  4) (progn (aset kill  3 name) (setq change-p t))))
    (if save  (aset save  2 (buffer-modified-p)))
    (if rvt   (aset rvt   2 (not (not buffer-file-name))))
    (if del   (aset del   2 (not (eq (next-screen () () t) (selected-screen)))))
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

(defvar buffers-menu-max-size 10
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
		 (concat "Write " (buffer-name (current-buffer))
			 " to file: ")))))


(defsubst build-buffers-menu-internal (buffers)
  (let (name line)
    (mapcar
     (if complex-buffers-menu-p
	 #'(lambda (buffer)
	     (if (setq line (format-buffers-menu-line buffer))
		 (list line
		       (vector "Switch to Buffer"
			       (list buffers-menu-switch-to-buffer-function
				     (setq name (buffer-name buffer)))
			       t)
		       (if (and (buffer-modified-p buffer)
				(buffer-file-name buffer))
			   (vector "Save Buffer"
				   (list 'buffer-menu-save-buffer name) t)
			 ["Save Buffer" nil nil])
		       (vector "Save Buffer As..."
			       (list 'buffer-menu-write-file name) t)
		       (vector "Kill Buffer" (list 'kill-buffer name) t))))
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
	name
	buffers)
    (if (not buffer-menu)
	nil
      (setq buffers (buffer-list))

      (if (and (integerp buffers-menu-max-size)
	       (> buffers-menu-max-size 1))
	  (if (> (length buffers) buffers-menu-max-size)
	      (setcdr (nthcdr buffers-menu-max-size buffers) nil)))

      (setq buffers (build-buffers-menu-internal buffers))
      (setq buffers (nconc (delq nil buffers)
			   '("----" ["List All Buffers" list-buffers t])))
      ;; slightly (only slightly) more efficient to not install the menubar
      ;; if it hasn't visibly changed.
      (if (equal buffers (cdr buffer-menu))
	  t  ; return t meaning "no change"
	(setcdr buffer-menu buffers)
	nil))))

(add-hook 'activate-menubar-hook 'build-buffers-menu-hook)
(add-hook 'activate-menubar-hook 'sensitize-file-and-edit-menus-hook)

(set-menubar default-menubar)



(defun yes-or-no-p-dialog-box (prompt)
  "Ask user a \"y or n\" question with a popup dialog box.
Returns t if answer is \"yes\".
Takes one argument, which is the string to display to ask the question."
  (let ((echo-keystrokes 0)
	event)	 
    (popup-dialog-box
     (cons prompt '(["Yes" yes t] ["No" no t] nil ["Abort" abort t])))
    (catch 'ynp-done
      (while t
	(setq event (next-command-event event))
	(cond ((and (menu-event-p event) (eq (event-object event) 'yes))
	       (throw 'ynp-done t))
	      ((and (menu-event-p event) (eq (event-object event) 'no))
	       (throw 'ynp-done nil))
	      ((and (menu-event-p event)
		    (or (eq (event-object event) 'abort)
			(eq (event-object event) 'menu-no-selection-hook)))
	       (signal 'quit nil))
	      ((button-release-event-p event) ;; don't beep twice
	       nil)
	      (t
	       (beep)
	       (message "please answer the dialog box")))))))

(defun yes-or-no-p-maybe-dialog-box (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.
The question is asked with a dialog box or the minibuffer, as appropriate.
Takes one argument, which is the string to display to ask the question.
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
The user must confirm the answer with RET,
and can edit it until it as been confirmed."
  (if (or (button-press-event-p last-command-event)
	  (button-release-event-p last-command-event)
	  (menu-event-p last-command-event))
      (yes-or-no-p-dialog-box prompt)
    (yes-or-no-p-minibuf prompt)))

(defun y-or-n-p-maybe-dialog-box (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
The question is asked with a dialog box or the minibuffer, as appropriate.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no."
  (if (or (button-press-event-p last-command-event)
	  (button-release-event-p last-command-event)
	  (menu-event-p last-command-event))
      (yes-or-no-p-dialog-box prompt)
    (y-or-n-p-minibuf prompt)))

(if (fboundp 'popup-dialog-box)
    (progn
      (fset 'yes-or-no-p 'yes-or-no-p-maybe-dialog-box)
      (fset 'y-or-n-p 'y-or-n-p-maybe-dialog-box)))


(provide 'menubar)
