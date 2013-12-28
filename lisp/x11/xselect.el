;; Elisp interface to X Selections.
;; Copyright (C) 1990 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; The selection code requires us to use certain symbols whose names are
;;; all upper-case; this may seem tasteless, but it makes there be a 1:1
;;; correspondence between these symbols and X Atoms (which are upcased.)

(or (find-face 'primary-selection)
    (make-face 'primary-selection))

(or (find-face 'secondary-selection)
    (make-face 'secondary-selection))

(defun x-get-selection ()
  "Return text selected from some X window."
  (x-get-selection-internal 'PRIMARY 'STRING))

(defun x-get-secondary-selection ()
  "Return text selected from some X window."
  (x-get-selection-internal 'SECONDARY 'STRING))

(defun x-get-clipboard ()
  "Return text pasted to the clipboard."
  (x-get-selection-internal 'CLIPBOARD 'STRING))


(defvar primary-selection-extent nil
  "The extent of the primary selection; don't use this.")

(defvar secondary-selection-extent nil
  "The extent of the secondary selection; don't use this.")


(defun x-select-make-extent-for-selection (selection previous-extent face)
  ;; Given a selection, this makes an extent in the buffer which holds that
  ;; selection, for highlighting purposes.  If the selection isn't associated
  ;; with a buffer, this does nothing.
  (let ((buffer nil)
	(valid (and (extentp previous-extent)
		    (extent-buffer previous-extent)
		    (buffer-name (extent-buffer previous-extent))))
	start end)
    (cond ((stringp selection))
	  ((consp selection)
	   (setq start (min (car selection) (cdr selection))
		 end (max (car selection) (cdr selection))
		 valid (and valid
			    (eq (marker-buffer (car selection))
				(extent-buffer previous-extent)))
		 buffer (marker-buffer (car selection))))
	  ((extentp selection)
	   (setq start (extent-start-position selection)
		 end (extent-end-position selection)
		 valid (and valid
			    (eq (extent-buffer selection)
				(extent-buffer previous-extent)))
		 buffer (extent-buffer selection)))
	  )
    (if (and (not valid)
	     (extentp previous-extent)
	     (extent-buffer previous-extent)
	     (buffer-name (extent-buffer previous-extent)))
	(delete-extent previous-extent))
    (if (not buffer)
	;; string case
	nil
      ;; normal case
      (if valid
	  (set-extent-endpoints previous-extent start end)
	(set-extent-face (make-extent start end buffer) face)))))


(defun x-own-selection (selection &optional type)
  "Make a primary X Selection of the given argument.  
The argument may be a string, a cons of two markers or an extent.  In the 
latter cases the selection is considered to be the text between 
the markers or the between extents endpoints"
  (interactive (if (not current-prefix-arg)
		   (list (read-string "Store text for pasting: "))
		 (list (cons ;; these need not be ordered.
			(copy-marker (point-marker))
			(copy-marker (mark-marker))))))
  (or (stringp selection)
      (extentp selection)
      (and (consp selection)
	   (markerp (car selection))
	   (markerp (cdr selection))
	   (marker-buffer (car selection))
	   (marker-buffer (cdr selection))
	   (eq (marker-buffer (car selection))
	       (marker-buffer (cdr selection)))
	   (buffer-name (marker-buffer (car selection)))
	   (buffer-name (marker-buffer (cdr selection))))
      (signal 'error (list "invalid selection" selection)))
  (or type (setq type 'PRIMARY))
  (x-own-selection-internal type selection)
  (cond ((eq type 'PRIMARY)
	 (setq primary-selection-extent
	       (x-select-make-extent-for-selection
		selection primary-selection-extent 'primary-selection)))
	((eq type 'SECONDARY)
	 (setq secondary-selection-extent
	       (x-select-make-extent-for-selection
		selection secondary-selection-extent 'secondary-selection))))
  selection)


(defun x-own-secondary-selection (selection &optional type)
  "Make a secondary X Selection of the given argument.  The argument may be a 
string or a cons of two markers (in which case the selection is considered to
be the text between those markers.)"
  (interactive (if (not current-prefix-arg)
		   (list (read-string "Store text for pasting: "))
		 (list (cons ;; these need not be ordered.
			(copy-marker (point-marker))
			(copy-marker (mark-marker))))))
  (x-own-selection selection 'SECONDARY))


(defun x-own-clipboard (string)
  "Paste the given string to the X Clipboard."
  (x-own-selection string 'CLIPBOARD))


(defun x-disown-selection (&optional secondary-p)
  "Assuming we own the selection, disown it.  With an argument, discard the
secondary selection instead of the primary selection."
  (x-disown-selection-internal (if secondary-p 'SECONDARY 'PRIMARY)))

(defun x-dehilight-selection (selection)
  "for use as a value of x-lost-selection-hooks."
  (cond ((eq selection 'PRIMARY)
	 (if primary-selection-extent
	     (let ((inhibit-quit t))
	       (delete-extent primary-selection-extent)
	       (setq primary-selection-extent nil)))
	 (if zmacs-regions (zmacs-deactivate-region)))
	((eq selection 'SECONDARY)
	 (if secondary-selection-extent
	     (let ((inhibit-quit t))
	       (delete-extent secondary-selection-extent)
	       (setq secondary-selection-extent nil)))))
  nil)

(setq x-lost-selection-hooks 'x-dehilight-selection)

(defun x-notice-selection-requests (selection type successful)
  "for possible use as the value of x-sent-selection-hooks."
  (if (not successful)
      (message "Selection request failed to convert %s to %s"
	       selection type)
    (message "Sent selection %s as %s" selection type)))

(defun x-notice-selection-failures (selection type successful)
  "for possible use as the value of x-sent-selection-hooks."
  (or successful
      (message "Selection request failed to convert %s to %s"
	       selection type)))

;(setq x-sent-selection-hooks 'x-notice-selection-requests)
;(setq x-sent-selection-hooks 'x-notice-selection-failures)


;;; Selections in killed buffers
;;; this function is called by kill-buffer as if it were on the 
;;; kill-buffer-hook (though it isn't really.)

(defun xselect-kill-buffer-hook ()
  ;; Probably the right thing is to write a C function to return a list
  ;; of the selections which emacs owns, since it could concievably own
  ;; a user-defined selection type that we've never heard of.
  (xselect-kill-buffer-hook-1 'PRIMARY)
  (xselect-kill-buffer-hook-1 'SECONDARY)
  (xselect-kill-buffer-hook-1 'CLIPBOARD))

(defun xselect-kill-buffer-hook-1 (selection)
  (let (value)
    (if (and (x-selection-owner-p selection)
	     (setq value (x-get-selection-internal selection '_EMACS_INTERNAL))
	     ;; The _EMACS_INTERNAL selection type has a converter registered
	     ;; for it that does no translation.  This only works if emacs is
	     ;; requesting the selection from itself.  We could have done this
	     ;; by writing a C function to return the raw selection data, and
	     ;; that might be the right way to do this, but this was easy.
	     (or (and (consp value)
		      (markerp (car value))
		      (eq (current-buffer) (marker-buffer (car value))))
		 (and (extentp value)
		      (eq (current-buffer) (extent-buffer value)))))
	(x-disown-selection-internal selection))))


;;; Cut Buffer support

(defun x-get-cutbuffer (&optional which-one)
  "Returns the value of one of the 8 X server cut-buffers.  Optional arg
WHICH-ONE should be a number from 0 to 7, defaulting to 0.
Cut buffers are considered obsolete\; you should use selections instead."
  (x-get-cutbuffer-internal
   (if which-one
       (aref [CUT_BUFFER0 CUT_BUFFER1 CUT_BUFFER2 CUT_BUFFER3
	      CUT_BUFFER4 CUT_BUFFER5 CUT_BUFFER6 CUT_BUFFER7]
	     which-one)
     'CUT_BUFFER0)))

(defun x-store-cutbuffer (string)
  "Store the given string into the X server's primary cut buffer.
The previous value of the primary cut buffer is rotated to the secondary
cut buffer, and the second to the third, and so on (there are 8 buffers.)
Cut buffers are considered obsolete\; you should use selections instead."
  (or (stringp string) (error "must be a string"))
  (x-rotate-cutbuffers-internal 1)
  (x-store-cutbuffer-internal 'CUT_BUFFER0 string))


;;; Random utility functions

(defun x-kill-primary-selection ()
  "If there is a selection, delete the text it covers, and copy it to 
both the kill ring and the Clipboard."
  (interactive)
  (or (x-selection-owner-p) (error "emacs does not own the primary selection"))
  (setq last-command nil)
  (or primary-selection-extent
      (error "the primary selection is not an extent?"))
  (save-excursion
    (set-buffer (extent-buffer primary-selection-extent))
    (kill-region (extent-start-position primary-selection-extent)
		 (extent-end-position primary-selection-extent)))
  (x-disown-selection nil))

(defun x-delete-primary-selection ()
  "If there is a selection, delete the text it covers *without* copying it to
the kill ring or the Clipboard."
  (interactive)
  (or (x-selection-owner-p) (error "emacs does not own the primary selection"))
  (setq last-command nil)
  (or primary-selection-extent
      (error "the primary selection is not an extent?"))
  (save-excursion
    (set-buffer (extent-buffer primary-selection-extent))
    (delete-region (extent-start-position primary-selection-extent)
		   (extent-end-position primary-selection-extent)))
  (x-disown-selection nil))

(defun x-copy-primary-selection ()
  "If there is a selection, copy it to both the kill ring and the Clipboard."
  (interactive)
  (setq last-command nil)
  (or (x-selection-owner-p) (error "emacs does not own the primary selection"))
  (or primary-selection-extent
      (error "the primary selection is not an extent?"))
  (save-excursion
    (set-buffer (extent-buffer primary-selection-extent))
    (copy-region-as-kill (extent-start-position primary-selection-extent)
			 (extent-end-position primary-selection-extent))))

(defun x-yank-clipboard-selection ()
  "If someone owns a Clipboard selection, insert it at point."
  (interactive)
  (setq last-command nil)
  (let ((clip (x-get-clipboard)))
    (or clip (error "there is no clipboard selection"))
    (push-mark)
    (insert clip)))


;;; Functions to convert the selection into various other selection types.
;;; Every selection type that emacs handles is implemented this way, except
;;; for TIMESTAMP, which is a special case.

(defun xselect-convert-to-string (selection type value)
  (cond ((stringp value)
	 value)
	((extentp value)
	 (save-excursion
	   (set-buffer (extent-buffer value))
	   (buffer-substring (extent-start-position value)
			     (extent-end-position value))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (or (eq (marker-buffer (car value)) (marker-buffer (cdr value)))
	     (signal 'error
		     (list "markers must be in the same buffer"
			   (car value) (cdr value))))
	 (save-excursion
	   (set-buffer (or (marker-buffer (car value))
			   (error "selection is in a killed buffer")))
	   (buffer-substring (car value) (cdr value))))
	(t nil)))

(defun xselect-convert-to-length (selection type value)
  (let ((value
	 (cond ((stringp value)
		(length value))
	       ((extentp value)
		(extent-length value))
	       ((and (consp value)
		     (markerp (car value))
		     (markerp (cdr value)))
		(or (eq (marker-buffer (car value))
			(marker-buffer (cdr value)))
		    (signal 'error
			    (list "markers must be in the same buffer"
				  (car value) (cdr value))))
		(abs (- (car value) (cdr value)))))))
    (if value ; force it to be in 32-bit format.
	(cons (ash value -16) (logand value 65535))
      nil)))

(defun xselect-convert-to-targets (selection type value)
  ;; return a vector of atoms, but remove duplicates first.
  (let* ((all (cons 'TIMESTAMP (mapcar 'car selection-converter-alist)))
	 (rest all))
    (while rest
      (cond ((memq (car rest) (cdr rest))
	     (setcdr rest (delq (car rest) (cdr rest))))
	    ((eq (car (cdr rest)) '_EMACS_INTERNAL)  ; shh, it's a secret
	     (setcdr rest (cdr (cdr rest))))
	    (t
	     (setq rest (cdr rest)))))
    (apply 'vector all)))

(defun xselect-convert-to-delete (selection type value)
  (x-disown-selection-internal selection)
  ;; A return value of nil means that we do not know how to do this conversion,
  ;; and replies with an "error".  A return value of NULL means that we have
  ;; done the conversion (and any side-effects) but have no value to return.
  'NULL)

(defun xselect-convert-to-filename (selection type value)
  (cond ((extentp value)
	 (buffer-file-name (or (extent-buffer value)
			       (error "selection is in a killed buffer"))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (buffer-file-name (or (marker-buffer (car value))
			       (error "selection is in a killed buffer"))))
	(t nil)))

(defun xselect-convert-to-charpos (selection type value)
  (let (a b tmp)
    (cond ((cond ((extentp value)
		  (setq a (extent-start-position value)
			b (extent-end-position value)))
		 ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (car value)
			b (cdr value))))
	   (setq a (1- a) b (1- b)) ; zero-based
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun xselect-convert-to-lineno (selection type value)
  (let (a b buf tmp)
    (cond ((cond ((extentp value)
		  (setq buf (extent-buffer value)
			a (extent-start-position value)
			b (extent-end-position value)))
		 ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (marker-position (car value))
			b (marker-position (cdr value))
			buf (marker-buffer a))))
	   (save-excursion
	     (set-buffer buf)
	     (setq a (count-lines 1 a)
		   b (count-lines 1 b)))
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun xselect-convert-to-colno (selection type value)
  (let (a b buf tmp)
    (cond ((cond ((extentp value)
		  (setq buf (extent-buffer value)
			a (extent-start-position value)
			b (extent-end-position value)))
		 ((and (consp value)
		       (markerp (car value))
		       (markerp (cdr value)))
		  (setq a (car value)
			b (cdr value)
			buf (marker-buffer a))))
	   (save-excursion
	     (set-buffer buf)
	     (goto-char a)
	     (setq a (current-column))
	     (goto-char b)
	     (setq b (current-column)))
	   (if (< b a) (setq tmp a a b b tmp))
	   (cons 'SPAN
		 (vector (cons (ash a -16) (logand a 65535))
			 (cons (ash b -16) (logand b 65535))))))))

(defun xselect-convert-to-os (type size)
  (symbol-name system-type))

(defun xselect-convert-to-host (type size)
  (system-name))

(defun xselect-convert-to-user (type size)
  (user-full-name))

(defun xselect-convert-to-class (type size)
  "XEmacs") ; This is correct because this isn't currently customizable...

(defun xselect-convert-to-name (type size)
  invocation-name)

(defun xselect-convert-to-integer (selection type value)
  (and (integerp value)
       (cons (ash value -16) (logand value 65535))))

(defun xselect-convert-to-atom (selection type value)
  (and (symbolp value) value))

(defun xselect-convert-to-identity (selection type value) ; used internally
  (vector value))

(setq selection-converter-alist
      '((TEXT . xselect-convert-to-string)
	(STRING . xselect-convert-to-string)
	(TARGETS . xselect-convert-to-targets)
	(LENGTH . xselect-convert-to-length)
	(DELETE . xselect-convert-to-delete)
	(FILE_NAME . xselect-convert-to-filename)
	(CHARACTER_POSITION . xselect-convert-to-charpos)
	(LINE_NUMBER . xselect-convert-to-lineno)
	(COLUMN_NUMBER . xselect-convert-to-colno)
	(OWNER_OS . xselect-convert-to-os)
	(HOST_NAME . xselect-convert-to-host)
	(USER . xselect-convert-to-user)
	(CLASS . xselect-convert-to-class)
	(NAME . xselect-convert-to-name)
	(ATOM . xselect-convert-to-atom)
	(INTEGER . xselect-convert-to-integer)
	(_EMACS_INTERNAL . xselect-convert-to-identity)
	))


(provide 'xselect)
