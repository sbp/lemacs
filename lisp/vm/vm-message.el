;;; Macros and functions dealing with accessing VM message struct fields
;;; Copyright (C) 1989, 1990, 1991 Kyle E. Jones
;;;
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
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; data that is always shared with virtual folders
(defmacro vm-location-data-of (message) (list 'aref message 0))
;; where message begins (From_ line)
(defmacro vm-start-of (message) (list 'aref (list 'aref message 0) 0))
;; where visible headers start
(defun vm-vheaders-of (message)
  (or (aref (aref message 0) 1)
      (progn (vm-reorder-message-headers message)
	     (aref (aref message 0) 1))))
;; where text section starts
(defun vm-text-of (message)
  (or (aref (aref message 0) 2) (progn (vm-find-and-set-text-of message)
				       (aref (aref message 0) 2))))
;; where message ends
(defmacro vm-end-of (message) (list 'aref (list 'aref message 0) 3))
;; soft data vector
(defmacro vm-softdata-of (message) (list 'aref message 1))
(defmacro vm-number-of (message) (list 'aref (list 'aref message 1) 0))
(defmacro vm-mark-of (message) (list 'aref (list 'aref message 1) 1))
;; start of summary line
(defmacro vm-su-start-of (message) (list 'aref (list 'aref message 1) 2))
;; end of summary line
(defmacro vm-su-end-of (message) (list 'aref (list 'aref message 1) 3))
;; message attribute vector
(defmacro vm-attributes-of (message) (list 'aref message 2))
(defmacro vm-new-flag (message) (list 'aref (list 'aref message 2) 0))
(defmacro vm-unread-flag (message) (list 'aref (list 'aref message 2) 1))
(defmacro vm-deleted-flag (message) (list 'aref (list 'aref message 2) 2))
(defmacro vm-filed-flag (message) (list 'aref (list 'aref message 2) 3))
(defmacro vm-replied-flag (message) (list 'aref (list 'aref message 2) 4))
(defmacro vm-written-flag (message) (list 'aref (list 'aref message 2) 5))
(defmacro vm-forwarded-flag (message) (list 'aref (list 'aref message 2) 6))
(defmacro vm-edited-flag (message) (list 'aref (list 'aref message 2) 7))
;; message cached data
(defmacro vm-cache-of (message) (list 'aref message 3))
;; message size in bytes (as a string)
(defmacro vm-byte-count-of (message) (list 'aref (list 'aref message 3) 0))
;; weekday sent
(defmacro vm-weekday-of (message) (list 'aref (list 'aref message 3) 1))
;; month day
(defmacro vm-monthday-of (message) (list 'aref (list 'aref message 3) 2))
;; month sent
(defmacro vm-month-of (message) (list 'aref (list 'aref message 3) 3))
;; year sent
(defmacro vm-year-of (message) (list 'aref (list 'aref message 3) 4))
;; hour sent
(defmacro vm-hour-of (message) (list 'aref (list 'aref message 3) 5))
;; timezone
(defmacro vm-zone-of (message) (list 'aref (list 'aref message 3) 6))
;; message author's full name (Full-Name: or gouged from From:)
(defmacro vm-full-name-of (message) (list 'aref (list 'aref message 3) 7))
;; message author address (gouged from From:)
(defmacro vm-from-of (message) (list 'aref (list 'aref message 3) 8))
;; message ID (Message-Id:)
(defmacro vm-message-id-of (message) (list 'aref (list 'aref message 3) 9))
;; number of lines in message (as a string)
(defmacro vm-line-count-of (message) (list 'aref (list 'aref message 3) 10))
;; message subject (Subject:)
(defmacro vm-subject-of (message) (list 'aref (list 'aref message 3) 11))
;; Regexp that can be used to find the start of the already ordered headers.
(defmacro vm-vheaders-regexp-of (message)
  (list 'aref (list 'aref message 3) 12))
;; Addresses of recipients in a comma separated list
(defmacro vm-to-of (message) (list 'aref (list 'aref message 3) 13))
;; Full names of recipients in a comma separated list.  Addresses if
;; full names not available.
(defmacro vm-to-names-of (message) (list 'aref (list 'aref message 3) 14))
;; numeric month sent
(defmacro vm-month-number-of (message) (list 'aref (list 'aref message 3) 15))
;; extra data shared by virtual messages if vm-virtual-mirror is non-nil
(defmacro vm-mirror-data-of (message) (list 'aref message 4))
;; if message is being edited, this is the buffer being used.
(defmacro vm-edit-buffer-of (message) (list 'aref (list 'aref message 4) 0))
;; list of virtual messages referencing the above real message
(defmacro vm-virtual-messages-of (message) (list 'aref (list 'aref message 4) 1))
;; modificaiton flag for this message
(defmacro vm-modflag-of (message) (list 'aref message 5))
;; link to real message
(defmacro vm-real-message-of (message) (list 'symbol-value (list 'aref message 6)))
;; list to previous message in the message list
(defmacro vm-reverse-link-of (message) (list 'symbol-value (list 'aref message 7)))
;; message type; if nil, then default to folder type
(defmacro vm-message-type-of (message) (list 'aref message 8))

(defmacro vm-set-location-data-of (message vdata) (list 'aset message 0 vdata))
(defmacro vm-set-start-of (message start) (list 'aset (list 'aref message 0) 0 start))
(defmacro vm-set-vheaders-of (message vh) (list 'aset (list 'aref message 0) 1 vh))
(defmacro vm-set-text-of (message text) (list 'aset (list 'aref message 0) 2 text))
(defmacro vm-set-end-of (message end) (list 'aset (list 'aref message 0) 3 end))
(defmacro vm-set-softdata-of (message data) (list 'aset message 1 data))
(defmacro vm-set-number-of (message n) (list 'aset (list 'aref message 1) 0 n))
(defmacro vm-set-mark-of (message val) (list 'aset (list 'aref message 1) 1 val))
(defmacro vm-set-su-start-of (message pos) (list 'aset (list 'aref message 1) 2 pos))
(defmacro vm-set-su-end-of (message pos) (list 'aset (list 'aref message 1) 3 pos))
(defmacro vm-set-attributes-of (message attrs) (list 'aset message 2 attrs))
;; The other routines in attributes group are part of the undo system.
(defmacro vm-set-edited-flag (message flag)
  (list 'aset (list 'aref message 2) 7 flag))
(defmacro vm-set-cache-of (message cache) (list 'aset message 3 cache))
(defmacro vm-set-byte-count-of (message count)
  (list 'aset (list 'aref message 3) 0 count))
(defmacro vm-set-weekday-of (message val)
  (list 'aset (list 'aref message 3) 1 val))
(defmacro vm-set-monthday-of (message val)
  (list 'aset (list 'aref message 3) 2 val))
(defmacro vm-set-month-of (message val)
  (list 'aset (list 'aref message 3) 3 val))
(defmacro vm-set-year-of (message val)
  (list 'aset (list 'aref message 3) 4 val))
(defmacro vm-set-hour-of (message val)
  (list 'aset (list 'aref message 3) 5 val))
(defmacro vm-set-zone-of (message val)
  (list 'aset (list 'aref message 3) 6 val))
(defmacro vm-set-full-name-of (message author)
  (list 'aset (list 'aref message 3) 7 author))
(defmacro vm-set-from-of (message author)
  (list 'aset (list 'aref message 3) 8 author))
(defmacro vm-set-message-id-of (message id)
  (list 'aset (list 'aref message 3) 9 id))
(defmacro vm-set-line-count-of (message count)
  (list 'aset (list 'aref message 3) 10 count))
(defmacro vm-set-subject-of (message subject)
  (list 'aset (list 'aref message 3) 11 subject))
(defmacro vm-set-vheaders-regexp-of (message regexp)
  (list 'aset (list 'aref message 3) 12 regexp))
(defmacro vm-set-to-of (message recips)
  (list 'aset (list 'aref message 3) 13 recips))
(defmacro vm-set-to-names-of (message recips)
  (list 'aset (list 'aref message 3) 14 recips))
(defmacro vm-set-month-number-of (message val)
  (list 'aset (list 'aref message 3) 15 val))
(defmacro vm-set-mirror-data-of (message data)
  (list 'aset message 4 data))
(defmacro vm-set-edit-buffer-of (message buf)
  (list 'aset (list 'aref message 4) 0 buf))
(defmacro vm-set-virtual-messages-of (message list)
  (list 'aset (list 'aref message 4) 1 list))
(defmacro vm-set-modflag-of (message val) (list 'aset message 5 val))
(defmacro vm-set-real-message-sym-of (message sym) (list 'aset message 6 sym))
(defmacro vm-set-reverse-link-of (message link) (list 'set (list 'aref message 7) link))
(defmacro vm-set-reverse-link-sym-of (message sym) (list 'aset message 7 sym))
(defmacro vm-set-message-type-of (message type) (list 'aset message 8 type))

(defun vm-text-end-of (message)
  (- (vm-end-of message)
     (cond ((eq vm-folder-type 'mmdf) 5)
	   (t 1))))

(defun vm-make-message ()
  (let ((v (make-vector 9 nil)) sym)
    (vm-set-softdata-of v (make-vector vm-softdata-vector-length nil))
    (vm-set-location-data-of
     v (make-vector vm-location-data-vector-length nil))
    (vm-set-mirror-data-of v (make-vector vm-mirror-data-vector-length nil))
    ;; We use an uninterned symbol here as a level of indirection
    ;; from a purely self-referential structure.  This is
    ;; necessary so that Emacs debugger can be used on this
    ;; program.
    (setq sym (make-symbol "<<>>"))
    (set sym v)
    (vm-set-real-message-sym-of v sym)
    ;; Another uninterned symbol for the reverse link 
    ;; into the message list.
    (setq sym (make-symbol "<--"))
    (vm-set-reverse-link-sym-of v sym)
    v ))

(defun vm-find-and-set-text-of (m)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (vm-start-of m))
      (forward-line 1)
      (search-forward "\n\n" (vm-text-end-of m) t)
      (vm-set-text-of m (point-marker)))))

