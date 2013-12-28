;; Keymap functions.
;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

;; Used by C code (lookup-key and friends) but defined here.
(defvar minor-mode-map-alist nil
  "Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings iff VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.")


(defun keymap-parent (keymap)
  "Returns the first parent of the given keymap.
This is obsolete; use `keymap-parents' instead."
  (car (keymap-parents keymap)))

(defun set-keymap-parent (keymap parent)
  "Makes the given keymap have (only) the given parent.
This is obsolete; use `set-keymap-parents' instead."
  (set-keymap-parents keymap (if parent (list parent) '()))
  parent)

(defvar mode-line-map (let ((m (make-sparse-keymap)))
			(set-keymap-name m 'mode-line-map)
			m)
  "Keymap consulted for mouse-clicks on the modeline of a window.
This variable may be buffer-local; its value will be looked up in
the buffer of the window whose modeline was clicked upon.")


;Prevent the \{...} documentation construct
;from mentioning keys that run this command.
(put 'undefined 'suppress-keymap t)

(defun undefined ()
  (interactive)
  (ding))

(defun suppress-keymap (map &optional nodigits)
  "Make MAP override all normally self-inserting keys to be undefined.
Normally, as an exception, digits and minus-sign are set to make prefix args,
but optional second arg NODIGITS non-nil treats them like other chars."
  (map-keymap #'(lambda (key binding)
		  (if (eq binding 'self-insert-command)
		      (define-key map (vector key) 'undefined)))
	      global-map)
  (or nodigits
      (let ((string (make-string 1 ?0)))
	(define-key map "-" 'negative-argument)
	;; Make plain numbers do numeric args.
	(while (<= (aref string 0) ?9)
	  (define-key map string 'digit-argument)
	  (aset string 0 (1+ (aref string 0)))))))

;;;>>> FSF19 takes arguments (olddef newdef keymap &optional oldmap prefix),
;;;>>> where "If optional fourth argument OLDMAP is specified, we redefine
;;;>>> in KEYMAP as NEWDEF those chars which are defined as OLDDEF in OLDMAP."
(defun substitute-key-definition (olddef newdef keymap)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.
Prefix keymaps reached from KEYMAP are not checked recursively;
perhaps they ought to be."
  (map-keymap #'(lambda (key binding)
		  (if (eq binding olddef)
		      (define-key keymap key newdef)))
	      keymap))


(defun local-key-binding (keys)
  "Return the binding for command KEYS in current local keymap only.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
The binding is probably a symbol with a function definition; see
the documentation for `lookup-key' for more information."
  (let ((map (current-local-map)))
    (if map
        (lookup-key map keys)
        nil)))

(defun global-key-binding (keys)
  "Return the binding for command KEYS in current global keymap only.
KEYS is a string or vector of events, a sequence of keystrokes.
The binding is probably a symbol with a function definition; see
the documentation for `lookup-key' for more information."
  (lookup-key (current-global-map) keys))

(defun global-set-key (keys function)
  "Give KEY a global binding as COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
Note that if KEY has a local binding in the current buffer
that local binding will continue to shadow any global binding."
  (interactive "kSet key globally: \nCSet key %s to command: ")
  (define-key (current-global-map) keys function))

(defun local-set-key (keys function)
  "Give KEY a local binding as COMMAND.
COMMAND is a symbol naming an interactively-callable function.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function.
The binding goes in the current buffer's local map,
which is shared with other buffers in the same major mode."
  (interactive "kSet key locally: \nCSet key %s locally to command: ")
  (if (null (current-local-map))
      (use-local-map (make-sparse-keymap)))
  (define-key (current-local-map) keys function))

(defun global-unset-key (keys)
  "Remove global binding of KEY.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function."
  (interactive "kUnset key globally: ")
  (global-set-key keys nil))

(defun local-unset-key (keys)
  "Remove local binding of KEY.
KEYS is a string, a vector of events, or a vector of key-description lists
as described in the documentation for the `define-key' function."
  (interactive "kUnset key locally: ")
  (if (current-local-map)
      (define-key (current-local-map) keys nil)))


;; Yet more RMS brain-death.
(defun minor-mode-key-binding (key &optional accept-default)
  "Find the visible minor mode bindings of KEY.
Return an alist of pairs (MODENAME . BINDING), where MODENAME is the
the symbol which names the minor mode binding KEY, and BINDING is
KEY's definition in that mode.  In particular, if KEY has no
minor-mode bindings, return nil.  If the first binding is a
non-prefix, all subsequent bindings will be omitted, since they would
be ignored.  Similarly, the list doesn't include non-prefix bindings
that come after prefix bindings.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details about this."
  (let ((tail minor-mode-map-alist)
        a s v)
    (while tail
      (setq a (car tail)
            tail (cdr tail))
      (and (consp a)
           (symbolp (setq s (car a)))
           (boundp s)
           (symbol-value s)
           ;; indirect-function deals with autoloadable keymaps
           (setq v (indirect-function (cdr a)))
           (setq v (lookup-key v key accept-default))
           ;; Terminate loop, with v set to non-nil value
           (setq tail nil)))
    v))
    

(defun current-minor-mode-maps ()
  "Return a list of keymaps for the minor modes of the current buffer."
  (let ((l '())
        (tail minor-mode-map-alist)
        a s v)
    (while tail
      (setq a (car tail)
            tail (cdr tail))
      (and (consp a)
           (symbolp (setq s (car a)))
           (boundp s)
           (symbol-value s)
           ;; indirect-function deals with autoloadable keymaps
           (setq v (indirect-function (cdr a)))
           (setq l (cons v l))))
    (nreverse l)))


;;>>> What a crock
(defun define-prefix-command (name &optional mapvar)
  "Define COMMAND as a prefix command.
A new sparse keymap is stored as COMMAND's function definition.
If second optional argument MAPVAR is not specified,
 COMMAND's value (as well as its function definition) is set to the keymap.
If a second optional argument MAPVAR is given and is not `t',
  the map is stored as its value.
Regardless of MAPVAR, COMMAND's function-value is always set to the keymap."
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map name)
    (fset name map)
    (cond ((not mapvar)
           (set name map))
          ((eq mapvar 't)
           )
          (t
           (set mapvar map)))
    name))


;;; Converting vectors of events to a read-equivalent form.
;;; This is used both by call-interactively (for the command history)
;;; and by macros.el (for saving keyboard macros to a file.)

(defun events-to-keys (events &optional no-mice)
 "Given a vector of event objects, returns a vector of key descriptors,
or a string (if they all fit in the ASCII range.)
Optional arg NO-MICE means that button events are not allowed."
 (if (and events (symbolp events)) (setq events (vector events)))
 (cond
  ((stringp events)
   events)
  ((not (vectorp events))
   (signal 'wrong-type-argument (list 'vectorp events)))
  ((let* ((length (length events))
	  (string (make-string length 0))
	  c ce
	  (i 0))
     (while (< i length)
       (setq ce (aref events i))
       (or (eventp ce) (setq ce (character-to-event ce)))
       ;; Normalize `c' to `?c' and `(control k)' to `?\C-k'
       ;; By passing t for the `allow-meta' arg we could get kbd macros
       ;; with meta in them to translate to the string form instead of
       ;; the list/symbol form; but I expect that would cause confusion,
       ;; so let's use the list/symbol form whenever there's any ambiguity.
       (setq c (event-to-character ce))
       (if (and character-set-property
		(key-press-event-p ce)
		(symbolp (event-key ce))
		(get (event-key ce) character-set-property))
	   ;; Don't use a string for `backspace' and `tab' to avoid that
	   ;; pleasant little ambiguity.
	   (setq c nil))
       (if c
	   (aset string i c)
	 (setq i length string nil))
       (setq i (1+ i)))
     string))
  (t
   (let* ((length (length events))
	  (new (copy-sequence events))
	  event mods key
	  (i 0))
     (while (< i length)
       (setq event (aref events i))
       (cond ((key-press-event-p event)
	      (setq mods (event-modifiers event)
		    key (event-key event))
	      (if (numberp key)
		  (setq key (intern (make-string 1 key))))
	      (aset new i (if mods
			      (nconc mods (cons key nil))
			    key)))
	     ((menu-event-p event)
	      (aset new i (list 'menu-selection
				(event-function event)
				(event-object event))))
	     ((or (button-press-event-p event)
		  (button-release-event-p event))
	      (if no-mice
		  (error "Mouse events can't be saved in keyboard macros."))
	      (setq mods (event-modifiers event)
		    key (intern (concat "button" (event-button event)
					(if (button-release-event-p event)
					    "up"))))
	      (aset new i (if mods
			      (nconc mods (cons key nil))
			    key)))
	     ((or (and event (symbolp event))
		  (and (consp event) (symbolp (car event))))
	      (aset new i event))
	     (t
	      (signal 'wrong-type-argument (list 'eventp event))))
       (setq i (1+ i)))
     new))))


;;; Deciding which map's binding to use.

;; Moved back to C.  Sigh.
;;  This definition has also bit-rotted WRT the C code:
;;   no extent-local keymaps, no overriding-local-map, no minor-mode maps
;(defun key-binding (keys)
;  "Return the binding for command KEYS in current keymaps.
;KEYS is a string, a vector of events, or a vector of key-description lists
;as described in the documentation for the `define-key' function.
;The binding is probably a symbol with a function definition; see
;the documentation for `lookup-key' for more information."
;  ;;
;  ;; This algorithm is (unfortunately) duplicated in command_builder_find_leaf()
;  ;; from event-stream.c.  Wouldn't it be nice if it was shared.
;  ;;
;  (let ((mouse (and (vectorp keys)
;                    (> (length keys) 0)
;                    ;; If this is a mouse-click event, then the "local" 
;                    ;;  keymap is considered to be the local map 
;                    ;;  of the buffer in the window over which
;                    ;;  the mouse was clicked, not necessarily the window
;                    ;;  which point is in.
;                    (let ((e (aref keys 0)))
;                      (and (eventp e)
;                           (or (button-press-event-p e)
;                               (button-release-event-p e))
;                           e))))
;        (frob #'(lambda (map)
;		  (and map
;		       (let ((tem (lookup-key map keys)))
;			 (if (integerp tem) nil tem))))))
;    (or ;;
;        ;; If this is a mouse click, and there is a mouse-grabbed-buffer,
;        ;; check the local map of that buffer first.
;        ;;
;        (and mouse
;             mouse-grabbed-buffer
;             (funcall frob (save-excursion
;                             (set-buffer mouse-grabbed-buffer)
;                             (current-local-map))))
;	;;
;	;; If this is a mouse click, and it was over a modeline, and there
;	;; is a mode-line-map for the buffer of that window, then check
;	;; there next.
;	;;
;	(and mouse
;	     (not (event-window mouse))
;	     (let ((w (locate-window-from-coordinates
;		       (selected-screen)
;		       (list (event-x mouse) (event-y mouse)))))
;	       (and w
;		    (save-excursion
;		      (set-buffer (window-buffer w))
;		      (and mode-line-map
;			   (funcall frob mode-line-map))))))
;	;;
;	;; If this is a mouse click, and it was over a window, check the
;	;; local map of the buffer of that window next.
;	;;
;	;; Otherwise, check the local map of the current buffer.
;	;;
;        (let ((b (and mouse
;                      (event-window mouse)
;                      (window-buffer (event-window mouse)))))
;          (funcall frob (if b
;                            (save-excursion
;                              (set-buffer b)
;                              (current-local-map))
;                            (current-local-map))))
;	;;
;	;; Lastly, check the global map.
;	;;
;        (funcall frob (current-global-map)))))
