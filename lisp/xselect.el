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
  "The extent of the primary selection; don't change this.")

(defvar secondary-selection-extent nil
  "The extent of the secondary selection; don't change this.")

(defvar primary-selection-attribute 70
  "The attribute used to display the primary selection")

(defvar secondary-selection-attribute 70
  "The attribute used to display the secondary selection")


(defun x-select-make-extent-for-selection (selection previous-extent attribute)
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
	  (update-extent previous-extent start end)
	(set-extent-attribute (make-extent start end buffer) attribute)))))


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
		selection
		primary-selection-extent
		primary-selection-attribute)))
	((eq type 'SECONDARY)
	 (setq secondary-selection-extent
	       (x-select-make-extent-for-selection
		selection
		secondary-selection-extent
		secondary-selection-attribute))))
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


;;; Functions to convert the selection into various other selection types.
;;; Every selection type that emacs handles is implemented this way, except
;;; for TIMESTAMP, which is a special case.

(defun xselect-convert-to-string (selection type value)
  (cond ((stringp value)
	 value)
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
	       ((and (consp value)
		     (markerp (car value))
		     (markerp (cdr value)))
		(or (eq (marker-buffer (car value)) (marker-buffer (cdr value)))
		    (signal 'error
			    (list "markers must be in the same buffer"
				  (car value) (cdr value))))
		(abs (- (car value) (cdr value)))))))
    (if value ; force it to be in 32-bit format.
	(cons (ash value -16) (logand value 65535))
      nil)))

(defun xselect-convert-to-targets (selection type value)
  ;; remove duplicates first.
  (let* ((all (cons 'TIMESTAMP (mapcar 'car selection-converter-alist)))
	 (rest all))
    (while rest
      (if (memq (car rest) (cdr rest))
	  (setcdr rest (delq (car rest) (cdr rest)))
	(setq rest (cdr rest))))
    (apply 'vector all)))

(defun xselect-convert-to-delete (selection type value)
  (x-disown-selection-internal selection)
  ;; A return value of nil means that we do not know how to do this conversion,
  ;; and replies with an "error".  A return value of NULL means that we have
  ;; done the conversion (and any side-effects) but have no value to return.
  'NULL)

(defun xselect-convert-to-filename (selection type value)
  (and (consp value)
       (markerp (car value))
       (markerp (cdr value))
       (buffer-file-name (or (marker-buffer (car value))
			     (error "selection is in a killed buffer")))))

(defun xselect-convert-to-charpos (selection type value)
  (and (consp value)
       (markerp (car value))
       (markerp (cdr value))
       (let ((a (1- (marker-position (car value)))) ; zero-based
	     (b (1- (marker-position (cdr value))))
	     tmp)
	 (if (< b a) (setq tmp a a b b tmp))
	 (cons 'SPAN
	       (vector (cons (ash a -16) (logand a 65535))
		       (cons (ash b -16) (logand b 65535)))))))

(defun xselect-convert-to-lineno (selection type value)
  (and (consp value)
       (markerp (car value))
       (markerp (cdr value))
       (let ((a (count-lines 1 (marker-position (car value))))
	     (b (count-lines 1 (marker-position (cdr value))))
	     tmp)
	 (if (< b a) (setq tmp a a b b tmp))
	 (cons 'SPAN
	       (vector (cons (ash a -16) (logand a 65535))
		       (cons (ash b -16) (logand b 65535)))))))

(defun xselect-convert-to-colno (selection type value)
  (and (consp value)
       (markerp (car value))
       (markerp (cdr value))
       (let ((a (save-excursion (goto-char (car value)) (current-column)))
	     (b (save-excursion (goto-char (cdr value)) (current-column)))
	     tmp)
	 (if (< b a) (setq tmp a a b b tmp))
	 (cons 'SPAN
	       (vector (cons (ash a -16) (logand a 65535))
		       (cons (ash b -16) (logand b 65535)))))))

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
	))


;;; The following stuff makes emacs understand how to convert to and from
;;; selections of type ENERGIZE, and allows the local selection-value to
;;; be an Extent as well as a string or a cons of two markers.

(defun xselect-energize-convert-to-string (selection type value)
  (if (extentp value)
      (save-excursion
	(set-buffer (extent-buffer value))
	(buffer-substring (extent-start-position value)
			  (extent-end-position value)))
    (xselect-convert-to-string selection type value)))


(defun xselect-convert-to-energize (selection type value)
  (let (str id start end tmp)
    (cond ((and (consp value)
		(markerp (car value))
		(markerp (cdr value)))
	   (setq id (energize-buffer-id (marker-buffer (car value)))
		 start (1- (marker-position (car value)))  ; zero based
		 end (1- (marker-position (cdr value)))))
	  ((extentp value)
	   (setq id (extent-to-generic-id value)
		 start 0
		 end 0)))
    (if (null id)
	nil
      (setq str (make-string 12 0))
      (if (< end start) (setq tmp start start end end tmp))
      (aset str 0 (logand (ash (car id) -8) 255))
      (aset str 1 (logand (car id) 255))
      (aset str 2 (logand (ash (cdr id) -8) 255))
      (aset str 3 (logand (cdr id) 255))
      (aset str 4 (logand (ash start -24) 255))
      (aset str 5 (logand (ash start -16) 255))
      (aset str 6 (logand (ash start -8) 255))
      (aset str 7 (logand start 255))
      (aset str 8 (logand (ash end -24) 255))
      (aset str 9 (logand (ash end -16) 255))
      (aset str 10 (logand (ash end -8) 255))
      (aset str 11 (logand end 255))
      (cons 'ENERGIZE_OBJECT str))))


(defun xselect-energize-convert-to-length (selection type value)
  (if (not (extentp value))
      (xselect-convert-to-length selection type value)
    (setq value (extent-length value))
    (cons (ash value -16) (logand value 65535))))

(defun xselect-energize-convert-to-filename (selection type value)
  (if (extentp value)
      (buffer-file-name (extent-buffer value))
    (xselect-convert-to-filename selection type value)))

(defun xselect-energize-convert-to-charpos (selection type value)
  (if (extentp value)
      (save-excursion
	(set-buffer (extent-buffer value))
	(setq value
	      (cons (set-marker (make-marker) (extent-start-position value))
		    (set-marker (make-marker) (extent-end-position value))))))
  (xselect-convert-to-charpos selection type value))

(defun xselect-energize-convert-to-lineno (selection type value)
  (if (extentp value)
      (save-excursion
	(set-buffer (extent-buffer value))
	(setq value
	      (cons (set-marker (make-marker) (extent-start-position value))
		    (set-marker (make-marker) (extent-end-position value))))))
  (xselect-convert-to-lineno selection type value))

(defun xselect-energize-convert-to-colno (selection type value)
  (if (extentp value)
      (save-excursion
	(set-buffer (extent-buffer value))
	(setq value
	      (cons (set-marker (make-marker) (extent-start-position value))
		    (set-marker (make-marker) (extent-end-position value))))))
  (xselect-convert-to-colno selection type value))

(setq selection-converter-alist
      (append '((TEXT . xselect-energize-convert-to-string)
		(STRING . xselect-energize-convert-to-string)
		(LENGTH . xselect-energize-convert-to-length)
		(FILE_NAME . xselect-energize-convert-to-filename)
		(CHARACTER_POSITION . xselect-energize-convert-to-charpos)
		(LINE_NUMBER . xselect-energize-convert-to-lineno)
		(COLUMN_NUMBER . xselect-energize-convert-to-colno)
		(ENERGIZE_OBJECT . xselect-convert-to-energize)
		)
	      selection-converter-alist))
