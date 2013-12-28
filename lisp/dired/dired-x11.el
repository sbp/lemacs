;;;; dired-x11.el - X11 support for Dired under Epoch

(defconst dired-x11-version (substring "$Revision: 1.26 $" 11 -2)
  "$Id: dired-x11.el,v 1.26 1992/05/20 05:34:50 jwz Exp $")

(require 'dired)

;;; Copyright (C) 1991 Tim Wilson and Sebastian Kremer
;;; Tim.Wilson@cl.cam.ac.uk
;;; Sebastian Kremer <sk@thp.uni-koeln.de>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to the above address) or from
;;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; LISPDIR ENTRY for the Elisp Archive ===============================
;;;    LCD Archive Entry:
;;;    dired-x11|Tim Wilson and Sebastian Kremer|Tim.Wilson@cl.cam.ac.uk, sk@thp.uni-koeln.de
;;;    |X11 mouse and color support for Dired under Epoch
;;;    |$Date: 1992/05/20 05:34:50 $|$Revision: 1.26 $|

;; OVERVIEW ===========================================================

;; Alter the appearance (e.g. color) of directories, symlinks,
;; executables, sockets, setuid/setgid files, and boring
;; (backup/autosave) files.

;; Clicking with the mouse:
;;   shift-left		toggle mark of file
;;   shift-middle	dired-up-directory
;;   shift-right	dired directory or find file

;; INSTALLATION =======================================================
;;
;; To install, add the following to your `dired-load-hook':
;;
;;	   (and (boundp 'epoch::version)
;;		(or (string-match "^Epoch 3.2" epoch::version)
;;		    (string-match "^Epoch 4" epoch::version))
;;		(load "dired-x11"))
;;
;; It is recommended to load dired-x11 after dired-x because then your
;; settings for to be omitted files will be used for the set of
;; `boring' files (see below).

;; Note that you need Epoch 3.2 or later.  These functions do not work
;; with Epoch 3.1.  They are known to work with Epoch 3.2 and 
;; Epoch 4.0 Beta patchlevel 0.

;; This package will not work with standard (e.g. 18.57) Dired, you
;; need sk's Tree Dired, available for ftp from
;;
;;     ftp.cs.buffalo.edu:pub/Emacs/diredall.tar.Z
;;
;; or
;;
;;     ftp.thp.uni-koeln.de[134.95.64.1]:/pub/gnu/emacs/diredall.tar.Z

;; TO DO ==============================================================
;;
;;  * Generalize: use a general mapping from file `type' to attribute.
;;    File type two-dimensional -- mode of file and properties of name?
;;
;;  * Add useful set of actions when button is moused.
;;
;;  * Think about deallocating and or clearing buttons.
;;
;;  * Allow button function to supply context args.

;; CUSTOMIZATION ======================================================

;; The default colors work well for dark text on white background.
;; Are there existing conventions for these colors?

;; Backup/autosave files, directories, symbolic links, executables,
;; setuid and setgid files, and sockets are distinguished from other
;; types of file (which appear in the default font, color, etc).

;; On mono displays, directories, setuid and setgid files are underlined.
;; Files may also be stippled.

;; If you want to change the default colors, you have to set the
;; following variables in your ~/.emacs as other variables are
;; computed from them at load time.  Changing them afterwards has no
;; effect.

;; The *-color variables should be set to the desired color for that
;; type of file.

;; The *-mono variables may be set to a list of effects to be applied.
;; The possible effects are `underline' and `stipple'.

(defvar dired-x11-re-boring (if (fboundp 'dired-omit-regexp)
				(dired-omit-regexp)
			      "^#\\|~$")
  "Regexp to match backup, autosave and otherwise boring files.
Those files are displayed in a boring color such as grey (see
variable `dired-x11-boring-color').")

(defvar dired-x11-boring-color "Grey"
  "Color used for backup, autosave and otherwise boring files.
See also `dired-x11-re-boring'.")

(defvar dired-x11-boring-mono nil
  ;; Depending on your font and display, you may like '(stipple)
  "Effects used for backup, autosave and otherwise boring files on mono displays.
See also `dired-x11-re-boring'.")

(defvar dired-x11-directory-color "Firebrick"
  "Color used for directories.")

(defvar dired-x11-directory-mono '(underline)
  "Effects used for directories on mono displays.")

(defvar dired-x11-executable-color "SeaGreen"
  "Color used for executable plain files.")

(defvar dired-x11-executable-mono nil
  "Effects used for executable plain files on mono displays.")

(defvar dired-x11-setuid-color "Red"
  "Color used for setuid and setgid plain files.")

(defvar dired-x11-setuid-mono '(underline)
  "Effects used for setuid and setgid plain files on mono displays.")

(defvar dired-x11-socket-color "Gold"
  "Color used for sockets.")

(defvar dired-x11-socket-mono nil
  "Effects used for sockets on mono displays.")

(defvar dired-x11-symlink-color	 "DarkSlateBlue"
  "Color used for symbolic links.")

(defvar dired-x11-symlink-mono nil
  "Effects used for symbolic links on mono displays.")

(defvar dired-x11-stipple '(32 2 "\125\125\125\125\252\252\252\252")
  "The stipple used for the mono effect")

;; If you need more elaborate customization, use function
;; dired-x11-edit-file-type-style and save the setting afterwards.

(defvar dired-epoch-highlight-threshold (* 100 1024)
  "If non-nil, a buffer size threshold (in bytes) above which
highlighting will not take place (because it would be too slow).")

;;; End of customization

;;; Install ourselves in the right hooks:

(defun dired-x11-add-hook (hook-var function &optional at-end)
  "Add a function to a hook if it is not already present.
First argument HOOK-VAR (a symbol) is the name of a hook, second
argument FUNCTION is the function to add.
Optional third argument AT-END means add at end of HOOK-VAR.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
value of HOOK-VAR."
  (interactive "SAdd to hook-var (symbol): \naAdd which function to %s? ")
  (if (not (boundp hook-var)) (set hook-var nil))
  (if (or (not (listp (symbol-value hook-var)))
	  (eq (car (symbol-value hook-var)) 'lambda))
      (set hook-var (list (symbol-value hook-var))))
  (if (memq function (symbol-value hook-var))
      nil
    (set hook-var
	 (if at-end
	     (append (symbol-value hook-var) (list function))
	   (cons function (symbol-value hook-var))))))

;; If dired-x.el is also loaded, arrange it so that highlighting will
;; be done after omitting uninteresting files, thus saving time:

(dired-x11-add-hook 'dired-after-readin-hook 'dired-epoch-highlight 'at-end)

(dired-x11-add-hook 'dired-mode-hook 'dired-x11-startup)


;;; Handling the gory X11 details

(defvar dired-x11-color (> (number-of-colors) 2)
  "Whether we have a color display under X11.")

(defconst dired-x11-all-types
  '(boring directory executable setuid socket symlink)
  "List of all types of files that Dired will highlight under X11.

The types are represented by the following symbols:

    boring	- backup, autosave or otherwise boring files
    directory	- directories
    executable	- executable plain files
    setuid	- setuid or setgid plain files
    socket	- sockets in the file system
    symlink	- symbolic links
")

;;; There's no reason why these effects shouldn't be used for
;;; color too -- but with all those lovely colors, who would want
;;; to stipple or underline?

(defconst dired-x11-mono-effects-alist
  (list '(underline . "foreground")
	(cons 'stipple dired-x11-stipple))
  "Effects which may be selected by the dired-x11-*-mono variables")

(defun dired-x11-mono-effects (effects)
  ;; Return an alist of style fields according the the elements of
  ;; EFFECTS.  If any styles are selected (ie the result is not nil)
  ;; the list also includes foreground and background colors.
  ;; (This doesn't work properly if an element of EFFECTS is not
  ;; a proper value.)
  (let ((style-fields
	 (mapcar (function (lambda (x) (assq x dired-x11-mono-effects-alist)))
		 effects)))
    (if style-fields
	(append '((foreground . "foreground")
		  (background . "background"))
		style-fields)
      nil)))
			  
(defvar dired-x11-alist
  ;; Rather than complicating the code later we always explicitly set
  ;; the foreground and background here (the defaults are not usually
  ;; suitable).

  ;; By allowing the special ``colors'' "background" and "foreground"
  ;; we achieve that dired-x11-alist can be set in ~/.emacs as a
  ;; _constant_ list (without having to splice in the value of
  ;; function foreground etc.), possibly with the help of
  ;; dired-x11-edit-file-type-style.

  ;; Thus, the user in his ~./emacs doesn't need to do what we do
  ;; here: splicing in the values of the color customization
  ;; variables.

  (list
   (list 'boring
	 (list 'color
	       (list (cons 'foreground dired-x11-boring-color)
		     (cons 'background "background")))
	 (list 'mono
	       (dired-x11-mono-effects dired-x11-boring-mono)))
   (list 'directory
	 (list 'color
	       (list (cons 'foreground dired-x11-directory-color)
		     (cons 'background "background")))
	 (list 'mono
	       (dired-x11-mono-effects dired-x11-directory-mono)))
   (list 'executable
	 (list 'color
	       (list (cons 'foreground dired-x11-executable-color)
		     (cons 'background "background")))
	 (list 'mono
	       (dired-x11-mono-effects dired-x11-executable-mono)))
   (list 'setuid
	 (list 'color
	       (list (cons 'foreground dired-x11-setuid-color)
		     (cons 'background "background")))
	 (list 'mono
	       (dired-x11-mono-effects dired-x11-setuid-mono)))
   (list 'socket
	 (list 'color
	       (list (cons 'foreground dired-x11-socket-color)
		     (cons 'background "background")))
	 (list 'mono
	       (dired-x11-mono-effects dired-x11-setuid-mono)))
   (list 'symlink
	 (list 'color
	       (list (cons 'foreground dired-x11-symlink-color)
		     (cons 'background "background")))
	 (list 'mono
	       (dired-x11-mono-effects dired-x11-symlink-mono)))
   )
  "Alist describing file types and their styles in Dired under X11.
Each element looks like

   \(TYPE (color ((STYLE-FIELD1 . VALUE1)
                  (STYLE-FIELD2 . VALUE2)
                   ...))
	  (mono  ((STYLE-FIELD1 . VALUE1)
             	  (STYLE-FIELD2 . VALUE2)
                   ...))\)

TYPE is one of the symbols in the variable `dired-x11-all-types', e.g.
`directory'.

The `color' alist describes attributes used on a color display, the
optional `mono' alist those used on a monochrome display.

The possible STYLE-FIELDs (symbols, cf. function `make-style') and
VALUEs (names of colors (as string), stipple patterns etc.) are
described in `dired-x11-all-style-fields'.

See also function `dired-x11-edit-file-type-style' for advanced customization.
")

;; Access functions.

(defun dired-x11-get-style-alist-elt (type)
  ;; Get the element whose car is TYPE (e.g. `directory').
  ;; Its second element (`cadr', or `nth 1') is TYPE's style alist.
  (assq (if dired-x11-color
		  'color
		'mono)
	(assq type dired-x11-alist)))

(defun dired-x11-get-style-alist (type)
  ;; Get the style-alist for the file type TYPE (e.g. `directory').
  (nth 1 (dired-x11-get-style-alist-elt type)))


;;; Styles control the appearance of text.
;;;
;;; In Epoch 3.2:
;;;   Buttons are created with attributes.
;;;   An attribute is an index into a table of styles.
;;;   Buttons are placed in buffers with `add-button'.
;;;
;;; In Epoch 4:
;;;   Zones are created with styles
;;;   Zones are placed in buffers with `add-zone'.


(defconst dired-x11-style-field-function-alist
  '(
    (foreground         dired-x11-set-style-foreground        )
    (background         dired-x11-set-style-background        )
    (cursor-foreground  dired-x11-set-style-cursor-foreground )
    (cursor-background  dired-x11-set-style-cursor-background )
    (underline          dired-x11-set-style-underline         )
    (stipple            dired-x11-set-style-stipple           )
    (cursor-stipple     dired-x11-set-style-cursor-stipple    )
    (background-stipple dired-x11-set-style-background-stipple)
    (font               dired-x11-set-style-font              )
    )
  "Alist of style-fields and functions to set a style field to a value.")

(defconst dired-x11-all-style-fields
  (mapcar 'car dired-x11-style-field-function-alist)
  "List of all style fields known to dired.
The symbols and their meanings are:

    foreground
      The text foreground color, as a string or X-Cardinal
      representing the color.

    background
      The text background color.  You almost always want to set this
      to the special string `"background"', which is replaced by the
      value of `(background)' by dired.

    cursor-foreground
      The character foreground color when the text cursor is on the
      character.

    cursor-background
      The character background color when the text cursor is on the
      character.  You almost always want to set this to the string
      `"background"'.

    stipple
      The stipple pattern to use for the text.	This is an X-Bitmap
      resource or list (WIDTH HEIGHT STRING).

    cursor-stipple
      The stipple to use when the cursor is on the tex.

    background-stipple
      The stipple to use for the background. Bits that are set in the
      stipple are displayed in the screen background color.  Cleared
      bits are displayed in the style background color.	 See stipple.

    underline
      The color to use for underlining. The value `"foreground"' is
      useful.

    font
      The font for the text, as an X-Font resource or string.  The
      display will be messy unless this is a character-cell font of
      the same pixel width as the default font.
")


;;; Select appropriate function for adding button/zone to buffer

(if (string-match "^Epoch 4" epoch::version)
    (fset 'dired-x11-add-zone (function epoch::add-zone))
  (fset 'dired-x11-add-zone (function epoch::add-button)))

;; Convert color name ready for set-style-*.
;; (12 Sep 91) I don't think this is necessary.  See `Conventions'
;; section of Epoch manual.  Leave it for the moment since it works.

(defun dired-x11-get-color (color)
  ;; Return X-Cardinal for COLOR, which should be an X-Cardinal or
  ;; string, including the special case "foreground" or "background"
  ;; for the current value of (foreground) and (background).
  ;; We could check what type of resource, but don't bother since this
  ;; error will be caught when we try and use the alleged color.
  (cond ((resourcep color)
	 color)
	((and (stringp color)
	      (equal "foreground" (downcase color)))
	 (foreground))
	((and (stringp color)
	      (equal "background" (downcase color)))
	 (background))
	(t
	 (get-color color))))

(defun dired-x11-get-bitmap (bitmap)
  ;; Return X-Bitmap for BITMAP, which should be an X-Bitmap or list
  ;; (WIDTH HEIGHT STRING).
  (if (resourcep bitmap)
      bitmap
    (apply 'make-bitmap bitmap)))

(defun dired-x11-get-font (font)
  ;; Return X-Font for FONT, which should be an X-Font or string.
  (if (resourcep font)
      font
    (get-font font)))

;; colors

(defun dired-x11-set-style-foreground (style color)
  (set-style-foreground style (dired-x11-get-color color)))

(defun dired-x11-set-style-background (style color)
  (set-style-background style (dired-x11-get-color color)))

(defun dired-x11-set-style-cursor-foreground(style color)
  (set-style-cursor-foreground style (dired-x11-get-color color)))

(defun dired-x11-set-style-cursor-background(style color)
  (set-style-cursor-background style (dired-x11-get-color color)))

(defun dired-x11-set-style-underline (style color)
  (set-style-underline style  (dired-x11-get-color color)))

;; stipples

(defun dired-x11-set-style-stipple (style stipple)
  (set-style-stipple style (dired-x11-get-bitmap stipple)))

(defun dired-x11-set-style-cursor-stipple (style stipple)
  (set-style-cursor-stipple style (dired-x11-get-bitmap stipple)))

(defun dired-x11-set-style-background-stipple (style stipple)
  (set-style-background-stipple style (dired-x11-get-bitmap stipple)))

;; fonts

(defun dired-x11-set-style-font (style font)
  (set-style-font style (dired-x11-get-font font)))


;; Functions for getting and setting colors, bitmaps, and fonts.

(defun dired-x11-make-new-style (style-alist)
  ;; Make new style, initialize from alist STYLE-ALIST
  ;; STYLE-ALIST is a table of style-field and value, e.g
  ;; ((foreground . "Grey")
  ;;  (background . #<X-Cardinal 0>))
  ;; Note that the values may be raw or cooked.

  (if (string-match "^Epoch 4" epoch::version)
    ;; Epoch Buttons reference styles directly in epoch 4
    (let ((style (make-style)))
      (dired-x11-set-style style style-alist)
      style)
    ;; else use highlighting as per epoch version 3 with attributes.
    ;; As with epoch 4 but additionally allocate and return corresponding
    ;; attribute.
    (let ((style (make-style))
	(attr (reserve-attribute)))
      (dired-x11-set-style style style-alist)
      (set-attribute-style attr style)
      attr)))

(defun dired-x11-set-style (style style-alist)
  ;; Set style-fields for the style STYLE from the values in STYLE-ALIST.
  ;; You can find the style for a given type of highlighting (e.g.
  ;; `directory') with the help of var `dired-x11-attribute-alist' and
  ;; function `attribute-style'.
  (let* ((style-field-value nil))
    (mapcar
     (function
      (lambda (x)
	;; Arrange for style-field X (e.g.  `foreground') to be
	;; displayed as specified in STYLE-ALIST (e.g. color #3 if
	;; `(foreground . 3)' is an element of STYLE-ALIST),
	;; e.g. (set-style-foreground style #<X-cardinal for color 3>)
	(and (setq style-field-value (cdr (assq x style-alist)))
	     (funcall (nth 1 (assq x dired-x11-style-field-function-alist))
		      style style-field-value))))
     dired-x11-all-style-fields)))

(defconst dired-x11-attribute-alist
  (mapcar
   (function;; returns e.g. `(directory . 3)' if directories are to be
	    ;; highlighted with color #3.
    (lambda (x)
      (let ((style-alist (dired-x11-get-style-alist x)))
	;; This test prevents dired-epoch-highlight placing a button
	;; with default attributes over non-special files -- the
	;; default attributes are not necessarily the same as no
	;; attributes, so this may lead to unintentional highlighting.
 	(and style-alist
 	     (cons x (dired-x11-make-new-style style-alist))))))
   dired-x11-all-types)
  "Alist with elements

    \(TYPE ATTRIBUTE)

TYPE is a symbol describing a file type, see `dired-x11-all-types'.
ATTRIBUTE describes how files of type TYPE are treated under X11 and
is computed at load time from `dired-x11-alist'.")


;; Interactive changing of the appearance of file types

(defun dired-x11-edit-file-type-style (file-type)
  "Edit interactively the style of highlighting for files of type FILE-TYPE.
Useful to try out different colors.
See variable `dired-x11-all-style-fields' for an explanation of the
allowed fields and their meanings.

This function changes the value of `dired-x11-alist' to reflect the changes.
You may want to set this variable to its new value in your ~/.emacs
for future sessions if the normal customization variables don't
suffice for you."
  (interactive
   (list (dired-x11-read-file-type "Change appearance of which file type? ")))
  (let* ((style (attribute-style
		 (cdr (assq file-type dired-x11-attribute-alist))))
	 (style-alist-elt (dired-x11-get-style-alist-elt file-type))
	 (style-alist (nth 1 style-alist-elt)))
    (setq style-alist
	 (dired-x11-read-style-alist (symbol-name file-type) style-alist))
    ;; if the alist has been enlarged we have to store it back into
    ;; dired-x11-alist:
    (setcdr style-alist-elt (list style-alist))
    (dired-x11-set-style style style-alist)))

(defun dired-x11-read-style-alist (type alist)
  ;; Let user edit the current fields of ALIST or add new fields.
  ;; TYPE is the file-type.  It is used for prompts only.
  ;; Changes ALIST destructively and returns its new value.
  ;; ALIST's keys must be symbols (i.e. assq instead of assoc will be used).
  (let ((key-table (dired-x11-symbol-list-to-table dired-x11-all-style-fields))
	key-str key elt value)
    (while (not (equal ""
		       (setq key-str
			     (completing-read
			      (concat "Edit which key of "
				      type
				      " (RET=end, ?=show): ")
			      key-table nil nil))))
      (if (equal "?" key-str)
	  (with-output-to-temp-buffer "*Dired X11 Alist*"
	    (princ (format
		    "Dired X11 appearance for files of type `%s':\n\n" type))
	    (if (fboundp 'pp-to-string)	; from pp.el by Randal Schwartz
		(princ (pp-to-string alist)) ; pretty print it
	      (prin1 alist)))
	(setq key (intern key-str))
	(setq value
	      (read-string (format "Set %s of %s to (current is %s): "
				   key type (cdr (assq key alist)))))
	(if (setq elt (assq key alist))
	    ;; modify in place
	    (setcdr elt value)
	  ;; add a new element to alist
	  (setq alist (cons (cons key value) alist)))))
    alist))

(defun dired-x11-symbol-list-to-table (list)
  ;; Convert a list of symbols to a table suitable for completing-read.
  (mapcar (function (lambda (x) (list (symbol-name x))))
	  list))

(defun dired-x11-read-file-type (prompt)
  (intern (completing-read
	   prompt (dired-x11-symbol-list-to-table dired-x11-all-types) nil t)))

;;; Regexps to match file types.

;; Not all of them are used in highlighting.
;; On some systems the setgid and sticky bits of directories mean
;; something but we don't provide regexps for them.

(defvar dired-re-socket
  (concat dired-re-maybe-mark dired-re-inode-size "s"))

(defvar dired-re-block-device
  (concat dired-re-maybe-mark dired-re-inode-size "b"))

(defvar dired-re-character-device
  (concat dired-re-maybe-mark dired-re-inode-size "c"))

(defvar dired-re-named-pipe
  (concat dired-re-maybe-mark dired-re-inode-size "p"))

(defvar dired-re-setuid;; setuid plain file (even if not executable)
  (concat dired-re-maybe-mark dired-re-inode-size
	  "-[-r][-w][Ss][-r][-w][sx][-r][-w][xst]"))

(defvar dired-re-setgid;; setgid plain file (even if not executable)
  (concat dired-re-maybe-mark dired-re-inode-size
	  "-[-r][-w][-x][-r][-w][Ss][-r][-w][xst]"))

(defvar dired-re-sticky;; sticky plain file (even if not executable)
  (concat dired-re-maybe-mark dired-re-inode-size
				"-[-r][-w][-x][-r][-w]s[-r][-w][Tt]"))

;;; Functions to actually highlight the files

;; This is nice, but too slow to use it for highlighting:
;;(defun dired-map (fun)
;;  "Run FUN, a function of zero args, at the beginning of each dired file line."
;;  (save-excursion
;;    (let (file buffer-read-only)
;;	(goto-char (point-min))
;;	(while (not (eobp))
;;	(save-excursion
;;	  (and (not (eolp))
;;	       (not (dired-between-files))
;;	       (progn (beginning-of-line)
;;		      (funcall fun))))
;;	(forward-line 1)))))

(defun dired-epoch-no-highlight-p ()
  "Function to decide whether to highlight current dired buffer.
If it returns non-nil, highlighting is suppressed."
  (or
   ;; we depend on the ls -l permission bit info for highlighting
   (let (case-fold-search)
     (not (string-match "l" dired-actual-switches)))
   ;; we don't want to highlight if it would take too long
   (and (integerp dired-epoch-highlight-threshold)
	(> (buffer-size) dired-epoch-highlight-threshold))))

(defun dired-epoch-highlight ()
  ;; Look at each file name and (if special) place a button over it
  ;; with appropriate attribute.
  (if (dired-epoch-no-highlight-p)
      nil				
    (message "Highlighting...")
    (let (buffer-read-only beg end pathname type attr)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (and (not (eolp))
	       ;;(not (dired-between-files)); not needed
	       (setq beg (dired-move-to-filename)
		     end (and beg (dired-move-to-end-of-filename t))
		     pathname (and beg end (buffer-substring beg end)))
	       ;; here if pathname non-nil
	       (progn
		 (beginning-of-line)	; for the re matches below
		 (setq type
		       (cond
			;; -- Is it a backup, autosave or otherwise boring file?
			;; Test this first because we don't want to draw
			;; attention to backup files even if they are e.g.
			;; executable.
			((or (string-match dired-x11-re-boring pathname))
			 ;; Here, being fast is more important than
			 ;; always being correct.
			 ;; (or (backup-file-name-p pathname)
			 ;;     (auto-save-file-name-p pathname))
			 'boring)
			;; -- Is it a directory?
			((looking-at dired-re-dir)
			 'directory)
			;; -- Is it a symbolic link?
			((looking-at dired-re-sym)
			 'symlink)
			;; Is it a setuid or setgid plain file?
			;; Test this before the test for being executable
			((or (looking-at dired-re-setuid)
			     (looking-at dired-re-setgid))
			 'setuid)
			;; -- Is it an executable file?
			((looking-at dired-re-exe)
			 'executable)
			;; -- Is it a socket?
			((looking-at dired-re-socket)
			 'socket)
			;; -- Else leave it alone.
			;; Plain file, or block or character special file.
			;; We don't need to draw attention to these.
			)
		       attr
		       (cdr (assq type dired-x11-attribute-alist)))
		 (if attr
		     (dired-x11-add-zone beg end attr))))
	  (forward-line 1))))
    (message "Highlighting...done")))


;;; Mouse handling

;;; This is only to test the concept (and code): a start, a pump-primer.
;;; Please extend!

;;  shift-left		toggle mark of file
;;  shift-middle	dired-up-directory
;;  shift-right		visit file or directory

(defvar dired-mouse-map (create-mouse-map))

;; The dired-PACKAGE-startup name is conventional (like in dired-x.el)
(defun dired-x11-startup ()
  "\
Automatically put on `dired-mode-hook' to get highlighting in Dired under X11."
  ;; Maybe we do more things here later.
  (use-local-mouse-map dired-mouse-map))

(define-mouse dired-mouse-map
  mouse-left mouse-shift-up 'dired-shift-mouse-left-handler)

(define-mouse dired-mouse-map
  mouse-middle mouse-shift-up 'dired-shift-mouse-middle-handler)

(define-mouse dired-mouse-map
  mouse-right mouse-shift-up 'dired-shift-mouse-right-handler)

(defun dired-shift-mouse-left-handler (mouse-data)
  ;; MOUSE-DATA is '(POINT BUFFER WINDOW SCREEN),
  ;; ie just right for the mouse::set-point function
  (mouse::set-point mouse-data)
  (dired-move-to-filename)
  (save-excursion;; don't want to move to next line
    (dired-toggle-file 1)))

(defun dired-shift-mouse-middle-handler (mouse-data)
  (mouse::set-point mouse-data)
  (dired-move-to-filename)
  (dired-up-directory))

(defun dired-shift-mouse-right-handler (mouse-data)
  (mouse::set-point mouse-data)
  (dired-move-to-filename)
  (dired-find-file))

(defun dired-toggle-file (arg)		; bind this to `t'?
  "In dired, toggle mark of the current file line.
With arg, repeat over several lines."
  (interactive "p")
  (let (buffer-read-only char)
    (dired-repeat-over-lines
     arg
     (function (lambda ()
		 (setq char (following-char))
		 (delete-char 1)
		 (insert (if (eq char ?\040)
			     dired-marker-char
			   ?\040)))))))

(run-hooks 'dired-x11-load-hook)

;;; End of dired-x11.el
