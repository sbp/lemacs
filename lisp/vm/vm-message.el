;;; Macros and functions dealing with accessing VM message struct fields
;;; Copyright (C) 1989, 1990, 1991, 1993, 1994 Kyle E. Jones
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
;; where message begins starting at the message separator in the folder
(defmacro vm-start-of (message) (list 'aref (list 'aref message 0) 0))
;; where headers start (From_ line)
(defmacro vm-headers-of (message) (list 'aref (list 'aref message 0) 1))
;; where visible headers start
(defun vm-vheaders-of (message)
  (or (aref (aref message 0) 2)
      (progn (vm-reorder-message-headers message nil nil)
	     (aref (aref message 0) 2))))
;; where text section starts
(defun vm-text-of (message)
  (or (aref (aref message 0) 3) (progn (vm-find-and-set-text-of message)
				       (aref (aref message 0) 3))))
;; where text portion of message ends
(defmacro vm-text-end-of (message) (list 'aref (list 'aref message 0) 4))
;; where message ends
(defmacro vm-end-of (message) (list 'aref (list 'aref message 0) 5))
;; soft data vector
(defmacro vm-softdata-of (message) (list 'aref message 1))
(defmacro vm-number-of (message) (list 'aref (list 'aref message 1) 0))
(defmacro vm-padded-number-of (message) (list 'aref (list 'aref message 1) 1))
(defmacro vm-mark-of (message) (list 'aref (list 'aref message 1) 2))
;; start of summary line
(defmacro vm-su-start-of (message) (list 'aref (list 'aref message 1) 3))
;; end of summary line
(defmacro vm-su-end-of (message) (list 'aref (list 'aref message 1) 4))
;; symbol whose value is the real message.
(defmacro vm-real-message-sym-of (message)
  (list 'aref (list 'aref message 1) 5))
;; real message
(defmacro vm-real-message-of (message)
  (list 'symbol-value (list 'aref (list 'aref message 1) 5)))
;; link to previous message in the message list
(defmacro vm-reverse-link-of (message)
  (list 'symbol-value (list 'aref (list 'aref message 1) 6)))
;; message type
(defmacro vm-message-type-of (message) (list 'aref (list 'aref message 1) 7))
;; number that uniquely identifies each message
;; this is for the set handling stuff
(defmacro vm-message-id-number-of (message)
  (list 'aref (list 'aref message 1) 8))
;; folder buffer of this message
(defmacro vm-buffer-of (message)
  (list 'aref (list 'aref message 1) 9))
;; cache thread indention value
(defmacro vm-thread-indention-of (message)
  (list 'aref (list 'aref message 1) 10))
;; list of symbols from vm-thread-obarray that give this message's lineage
(defmacro vm-thread-list-of (message)
  (list 'aref (list 'aref message 1) 11))
;; babyl header frob flag (0 or 1 at beginning of message)
(defmacro vm-babyl-frob-flag-of (message)
  (list 'aref (list 'aref message 1) 12))
;; saved attributes, if message was switched from unmirrored to mirrored
(defmacro vm-saved-virtual-attributes-of (message)
  (list 'aref (list 'aref message 1) 13))
;; saved mirror data, if message was switched from unmirrored to mirrored
(defmacro vm-saved-virtual-mirror-data-of (message)
  (list 'aref (list 'aref message 1) 14))
;; summary for unmirrored virtual message
(defmacro vm-virtual-summary-of (message)
  (list 'aref (list 'aref message 1) 15))
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
(defmacro vm-redistributed-flag (message) (list 'aref (list 'aref message 2) 8))
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
;; sortable date string (used for easy sorting, naturally)
(defmacro vm-sortable-datestring-of (message)
  (list 'aref (list 'aref message 3) 16))
;; sortable subject, re: garbage removed
(defmacro vm-sortable-subject-of (message)
  (list 'aref (list 'aref message 3) 17))
(defmacro vm-summary-of (message)
  (list 'aref (list 'aref message 3) 18))
(defmacro vm-parent-of (message)
  (list 'aref (list 'aref message 3) 19))
;; extra data shared by virtual messages if vm-virtual-mirror is non-nil
(defmacro vm-mirror-data-of (message) (list 'aref message 4))
;; if message is being edited, this is the buffer being used.
(defmacro vm-edit-buffer-of (message) (list 'aref (list 'aref message 4) 0))
;; list of virtual messages referencing the above real message
(defmacro vm-virtual-messages-of (message)
  (list 'symbol-value (list 'aref (list 'aref message 4) 1)))
;; modification flag for this message
;; nil if all attribute changes have been stuffed into the folder buffer
(defmacro vm-modflag-of (message) (list 'aref (list 'aref message 4) 2))
;; list of labels attached to this message
(defmacro vm-labels-of (message) (list 'aref (list 'aref message 4) 3))
;; comma list of labels
(defmacro vm-label-string-of (message) (list 'aref (list 'aref message 4) 4))

(defmacro vm-set-location-data-of (message vdata) (list 'aset message 0 vdata))
(defmacro vm-set-start-of (message start)
  (list 'aset (list 'aref message 0) 0 start))
(defmacro vm-set-headers-of (message h)
  (list 'aset (list 'aref message 0) 1 h))
(defmacro vm-set-vheaders-of (message vh)
  (list 'aset (list 'aref message 0) 2 vh))
(defmacro vm-set-text-of (message text)
  (list 'aset (list 'aref message 0) 3 text))
(defmacro vm-set-text-end-of (message text)
  (list 'aset (list 'aref message 0) 4 text))
(defmacro vm-set-end-of (message end)
  (list 'aset (list 'aref message 0) 5 end))
(defmacro vm-set-softdata-of (message data)
  (list 'aset message 1 data))
(defmacro vm-set-number-of (message n)
  (list 'aset (list 'aref message 1) 0 n))
(defmacro vm-set-padded-number-of (message n)
  (list 'aset (list 'aref message 1) 1 n))
(defmacro vm-set-mark-of (message val)
  (list 'aset (list 'aref message 1) 2 val))
(defmacro vm-set-su-start-of (message pos)
  (list 'aset (list 'aref message 1) 3 pos))
(defmacro vm-set-su-end-of (message pos)
  (list 'aset (list 'aref message 1) 4 pos))
(defmacro vm-set-real-message-sym-of (message sym)
  (list 'aset (list 'aref message 1) 5 sym))
(defmacro vm-set-reverse-link-of (message link)
  (list 'set (list 'aref (list 'aref message 1) 6) link))
(defmacro vm-set-reverse-link-sym-of (message sym)
  (list 'aset (list 'aref message 1) 6 sym))
(defmacro vm-set-message-type-of (message type)
  (list 'aset (list 'aref message 1) 7 type))
(defmacro vm-set-message-id-number-of (message number)
  (list 'aset (list 'aref message 1) 8 number))
(defmacro vm-set-buffer-of (message buffer)
  (list 'aset (list 'aref message 1) 9 buffer))
(defmacro vm-set-thread-indention-of (message val)
  (list 'aset (list 'aref message 1) 10 val))
(defmacro vm-set-thread-list-of (message list)
  (list 'aset (list 'aref message 1) 11 list))
(defmacro vm-set-babyl-frob-flag-of (message flag)
  (list 'aset (list 'aref message 1) 12 flag))
(defmacro vm-set-saved-virtual-attributes-of (message attrs)
  (list 'aset (list 'aref message 1) 13 attrs))
(defmacro vm-set-saved-virtual-mirror-data-of (message data)
  (list 'aset (list 'aref message 1) 14 data))
(defmacro vm-set-virtual-summary-of (message summ)
  (list 'aset (list 'aref message 1) 15 summ))
(defmacro vm-set-attributes-of (message attrs) (list 'aset message 2 attrs))
;; The other routines in attributes group are part of the undo system.
(defmacro vm-set-edited-flag-of (message flag)
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
(defmacro vm-set-sortable-datestring-of (message val)
  (list 'aset (list 'aref message 3) 16 val))
(defmacro vm-set-sortable-subject-of (message val)
  (list 'aset (list 'aref message 3) 17 val))
(defmacro vm-set-summary-of (message val)
  (list 'aset (list 'aref message 3) 18 val))
(defmacro vm-set-parent-of (message val)
  (list 'aset (list 'aref message 3) 19 val))
(defmacro vm-set-mirror-data-of (message data)
  (list 'aset message 4 data))
(defmacro vm-set-edit-buffer-of (message buf)
  (list 'aset (list 'aref message 4) 0 buf))
(defmacro vm-set-virtual-messages-of (message list)
  (list 'set (list 'aref (list 'aref message 4) 1) list))
(defmacro vm-set-virtual-messages-sym-of (message sym)
  (list 'aset (list 'aref message 4) 1 sym))
(defmacro vm-set-modflag-of (message val)
  (list 'aset (list 'aref message 4) 2 val))
(defmacro vm-set-labels-of (message labels)
  (list 'aset (list 'aref message 4) 3 labels))
(defmacro vm-set-label-string-of (message string)
  (list 'aset (list 'aref message 4) 4 string))

(defun vm-make-message ()
  (let ((v (make-vector 5 nil)) sym)
    (vm-set-softdata-of v (make-vector vm-softdata-vector-length nil))
    (vm-set-location-data-of
     v (make-vector vm-location-data-vector-length nil))
    (vm-set-mirror-data-of v (make-vector vm-mirror-data-vector-length nil))
    (vm-set-message-id-number-of v (int-to-string vm-message-id-number))
    (vm-increment vm-message-id-number)
    (vm-set-buffer-of v (current-buffer))
    ;; We use an uninterned symbol here as a level of indirection
    ;; from a purely self-referential structure.  This is
    ;; necessary so that Emacs debugger can be used on this
    ;; program.
    (setq sym (make-symbol "<<>>"))
    (set sym v)
    (vm-set-real-message-sym-of v sym)
    ;; Another uninterned symbol for the virtual messages list.
    (setq sym (make-symbol "<v>"))
    (set sym nil)
    (vm-set-virtual-messages-sym-of v sym)
    ;; Another uninterned symbol for the reverse link 
    ;; into the message list.
    (setq sym (make-symbol "<--"))
    (vm-set-reverse-link-sym-of v sym)
    v ))

(defun vm-find-and-set-text-of (m)
  (save-excursion
    (set-buffer (vm-buffer-of m))
    (save-restriction
      (widen)
      (goto-char (vm-headers-of m))
      (search-forward "\n\n" (vm-text-end-of m) 0)
      (vm-set-text-of m (point-marker)))))

(defun vm-virtual-message-p (m)
  (not (eq m (vm-real-message-of m))))
