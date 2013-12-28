;;; Commands for handling messages marks
;;; Copyright (C) 1990, 1993, 1994 Kyle E. Jones
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

(defun vm-mark-status (markp count)
  (cond ((and markp (= vm-mark-count count))
	 (message "%d message%s marked" count (if (= 1 count) "" "s")))
	(markp
	 (message "%d message%s marked (total %d)"
		  count (if (= 1 count) "" "s") vm-mark-count))
	(t
	 (message "%d message%s unmarked (%d remaining)"
		  count (if (= 1 count) "" "s") vm-mark-count))))

(defun vm-clear-all-marks ()
  "Removes all message marks in the current folder."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mp vm-message-list))
    (while mp
      (if (vm-mark-of (car mp))
	  (progn
	    (vm-set-mark-of (car mp) nil)
	    (vm-mark-for-summary-update (car mp) t)))
      (setq mp (cdr mp))))
  (let ((omc vm-mark-count))
    (setq vm-mark-count 0)
    (vm-mark-status nil omc))
  (vm-display nil nil '(vm-clear-all-marks)
	      '(vm-clear-all-marks marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-all-messages ()
  "Mark all messages in the current folder."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mp vm-message-list))
    (while mp
      (vm-set-mark-of (car mp) t)
      (vm-mark-for-summary-update (car mp) t)
      (setq mp (cdr mp))))
  (setq vm-mark-count (length vm-message-list))
  (vm-mark-status t vm-mark-count)
  (vm-display nil nil '(vm-mark-all-messages)
	      '(vm-mark-all-messages marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-message (count)
  "Mark the current message.
Numeric prefix argument N means mark the current message and the next
N-1 messages.  A negative N means mark the current message and the
previous N-1 messages."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((direction (if (< count 0) 'backward 'forward))
	(count (vm-abs count))
	(omc vm-mark-count)
	;;(oldmp vm-message-pointer) ;unused
	(vm-message-pointer vm-message-pointer))
    (while (not (zerop count))
      (if (not (vm-mark-of (car vm-message-pointer)))
	  (progn
	    (vm-set-mark-of (car vm-message-pointer) t)
	    (vm-increment vm-mark-count)
	    (vm-mark-for-summary-update (car vm-message-pointer) t)))
      (vm-decrement count)
      (if (not (zerop count))
	  (vm-move-message-pointer direction)))
    (vm-mark-status t (- vm-mark-count omc)))
  (vm-display nil nil '(vm-mark-message)
	      '(vm-mark-message marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-unmark-message (count)
  "Remove the mark from the current message.
Numeric prefix argument N means unmark the current message and the next
N-1 messages.  A negative N means unmark the current message and the
previous N-1 messages."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mlist (vm-select-marked-or-prefixed-messages count))
	(omc vm-mark-count))
    (while mlist
      (if (vm-mark-of (car mlist))
	  (progn
	    (vm-set-mark-of (car mlist) nil)
	    (vm-decrement vm-mark-count)
	    (vm-mark-for-summary-update (car mlist) t)))
      (setq mlist (cdr mlist)))
    (vm-mark-status nil (- omc vm-mark-count)))
  (vm-display nil nil '(vm-unmark-message)
	      '(vm-unmark-message marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-or-unmark-messages-with-selector (val selector arg)
  (let ((mlist (if vm-mark-search-whole-folder
		   vm-message-list
		 vm-message-pointer))
	(virtual (eq major-mode 'vm-virtual-mode))
	(arglist (if arg (list arg) nil))
	(count 0)
	(omc vm-mark-count))
    (setq selector (intern (concat "vm-vs-" (symbol-name selector))))
    (while mlist
      (if (and (not (eq val (vm-mark-of (car mlist)))) ; already (un)marked
	       (if virtual
		   (save-excursion
		     (set-buffer
		      (vm-buffer-of
		       (vm-real-message-of
			(car mlist))))
		     (apply selector (vm-real-message-of (car mlist)) arglist))
		 (apply selector (car mlist) arglist)))
	  (progn
	    (vm-set-mark-of (car mlist) val)
	    (if val
		(vm-increment vm-mark-count)
	      (vm-decrement vm-mark-count))
	    (vm-mark-for-summary-update (car mlist) t)
	    (vm-increment count)))
      (setq mlist (cdr mlist)))
    (vm-mark-status val (vm-abs (- omc vm-mark-count))))
  (vm-display nil nil
	      '(vm-mark-matching-messages vm-unmark-matching-messages)
	      (list this-command 'marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-matching-messages (selector &optional arg)
  "Mark messages matching some criterion.
You can use any of the virtual folder selectors, except for the
`and', `or' and `not' selectors.  See the documentation for the
variable vm-virtual-folder-alist for more information."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-select-folder-buffer)
     (vm-read-virtual-selector "Mark messages: ")))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-with-selector t selector arg))

(defun vm-unmark-matching-messages (selector &optional arg)
  "Unmark messages matching some criterion.
You can use any of the virtual folder selectors, except for the
`and', `or' and `not' selectors.  See the documentation for the
variable vm-virtual-folder-alist for more information."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-select-folder-buffer)
     (vm-read-virtual-selector "Unmark messages: ")))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-with-selector nil selector arg))

(defun vm-mark-thread-subtree ()
  "Mark the all messages in the thread tree rooted at the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-thread-subtree t))

(defun vm-unmark-thread-subtree ()
  "Unmark the all messages in the thread tree rooted at the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-thread-subtree nil))

(defun vm-mark-or-unmark-thread-subtree (mark)
  (vm-build-threads-if-unbuilt)
  (let ((list (list (car vm-message-pointer)))
	(omc vm-mark-count)
	subject-sym id-sym)
    (while list
      (if (not (eq (vm-mark-of (car list)) mark))
	  (progn
	    (vm-set-mark-of (car list) mark)
	    (if mark
		(vm-increment vm-mark-count)
	      (vm-decrement vm-mark-count))
	    (vm-mark-for-summary-update (car list))))
      (setq id-sym (car (vm-last (vm-th-thread-list (car list)))))
      (nconc list (copy-sequence (get id-sym 'children)))
      (setq subject-sym (intern (vm-so-sortable-subject (car list))
				vm-thread-subject-obarray))
      (if (and (boundp subject-sym) 
	       (eq id-sym (aref (symbol-value subject-sym) 0)))
	  (nconc list (copy-sequence (aref (symbol-value subject-sym) 2))))
      (setq list (cdr list)))
    (vm-mark-status mark (vm-abs (- omc vm-mark-count))))
  (vm-display nil nil
	      '(vm-mark-thread-subtree vm-unmark-thread-subtree)
	      (list this-command 'marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-messages-same-subject ()
  "Mark all messages with the same subject as the current message."
  (interactive)
  (vm-mark-or-unmark-messages-same-subject-or-author t nil))

(defun vm-unmark-messages-same-subject ()
  "Unmark all messages with the same subject as the current message."
  (interactive)
  (vm-mark-or-unmark-messages-same-subject-or-author nil nil))

(defun vm-mark-messages-same-author ()
  "Mark all messages with the same author as the current message."
  (interactive)
  (vm-mark-or-unmark-messages-same-subject-or-author t t))

(defun vm-unmark-messages-same-author ()
  "Unmark all messages with the same author as the current message."
  (interactive)
  (vm-mark-or-unmark-messages-same-subject-or-author nil t))

(defun vm-mark-or-unmark-messages-same-subject-or-author (mark author-p)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mp (if vm-mark-search-whole-folder
		vm-message-list
	      vm-message-pointer))
	(omc vm-mark-count)
	(subject (if (not author-p) (vm-so-sortable-subject
				     (car vm-message-pointer))))
	(name (if author-p (vm-su-full-name (car vm-message-pointer))))
	(from (if author-p (vm-su-from (car vm-message-pointer)))))
    (if (equal name "") (setq name nil)) ; don't match empty names
    (while mp
      (if (and (not (eq (vm-mark-of (car mp)) mark))
	       (if author-p
		   ;; I'm not quite sure what the right thing to do here is:
		   ;; my first thought was to match all messages with the same
		   ;; name or address as the current one.  But that means that
		   ;;    Mailer-Daemon@foo.com (Mail Delivery Subsystem)
		   ;; will match
		   ;;    Mailer-Daemon@bar.com (Mail Delivery Subsystem)
		   ;; which I don't necessarily want.  So let's only match the
		   ;; address.
		   (string-equal from (vm-su-from (car mp)))
		   ;;(or (and name
		   ;;        (string-equal name (vm-su-full-name (car mp))))
		   ;;    (string-equal from (vm-su-from (car mp))))
		 (string-equal subject (vm-so-sortable-subject (car mp)))))
	  (progn
	    (vm-set-mark-of (car mp) mark)
	    (if mark
		(vm-increment vm-mark-count)
	      (vm-decrement vm-mark-count))
	    (vm-mark-for-summary-update (car mp) t)))
      (setq mp (cdr mp)))
    (vm-mark-status mark (vm-abs (- omc vm-mark-count))))
  (vm-display nil nil
	      '(vm-mark-messages-same-subject
		vm-mark-messages-same-author
		vm-unmark-messages-same-subject
		vm-unmark-messages-same-author)
	      (list this-command 'marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-next-command-uses-marks ()
  "Does nothing except insure that the next VM command will operate only
on the marked messages in the current folder."
  (interactive)
  (message "Next command uses marks...")
  (vm-display nil nil '(vm-next-command-uses-marks)
	      '(vm-next-command-uses-marks)))

(defun vm-marked-messages ()
  (let (list (mp vm-message-list))
    (while mp
      (if (vm-mark-of (car mp))
	  (setq list (cons (car mp) list)))
      (setq mp (cdr mp)))
    (nreverse list)))

(defun vm-mark-help ()
  (interactive)
  (vm-display nil nil '(vm-mark-help) '(vm-mark-help))
  (message "MM = mark, MU = unmark, Mm = mark all, Mu = unmark all, MN = use marks, ..."))
