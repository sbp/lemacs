;;; Mouse and font support for VM running in Lucid GNU Emacs
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

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

;; This requires a vm-summary-mode-hook, and vm-select-message-hook.
;; The version of VM distributed by Lucid has these, and future versions
;; of VM distributed by Kyle probably will have them.


;;; Right button pops up a menu of commands in all VM buffers.
;;; Middle button selects indicated article in VM Summary buffers.

(defvar vm-menu
  '("VM Commands"
    ["Next Nondeleted Message"		vm-next-message t]
    ["Previous Nondeleted Message"	vm-previous-message t]
    ["Scroll Message Forward"		vm-scroll-forward t]
    ["Scroll Message Backward"		vm-scroll-backward t]
    ["Beginning Of Message"		vm-beginning-of-message t]
    ["End Of Message"			vm-end-of-message t]
    "----"
    ["Display Summary"			vm-summarize t]
    ["Get New Mail"			vm-get-new-mail t]
    "----"
    ("Motion..."
     ["Goto Last Seen Message"		vm-goto-message-last-seen t]
     ["Goto Message"			vm-goto-message	t]
     ["Next Message"			vm-Next-message t]
     ["Next Unread Message"		vm-next-unread-message t]
     ["Previous Message"		vm-Previous-message t]
     ["Previous Unread Message"		vm-previous-unread-message t]
     )
    ("Folders..."
     ["Save Message"			vm-save-message t]
     ["Save Message sans Headers"	vm-save-message-sans-headers t]
     ["Auto-archive Messages"		vm-auto-archive-messages t]
     ["Delete Message"			vm-delete-message t]
     ["Undelete Message"		vm-undelete-message t]
     ["Mark Message as Unread"		vm-unread-message t]
     ["Expunge Folder"			vm-expunge-folder t]
     "----"
     ["Visit Folder"			vm-visit-folder t]
     ["Visit Virtual Folder"		vm-visit-virtual-folder t]
     ["Save Buffer"			vm-save-buffer t]
     ["Save Folder"			vm-save-folder t]
     ["Save Folder As..."		vm-write-file t]
     )
    ("Sending Messages..."
     ["Send Mail"			vm-mail t]
     ["Reply to Sender"			vm-reply t]
     ["Reply to Sender (Citing Original)" vm-reply-include-text t]
     ["Reply to All"			vm-followup t]
     ["Reply to All (Citing Original)"	vm-followup-include-text t]
     ["Forward Message"			vm-forward-message t]
     ["Resend Message"			vm-resend-message t]
     ["Retry Bounced Message"		vm-resend-bounced-message t]
     ["Continue Composing Message"	vm-continue-composing-message t]
     )
    ("Marking Messages..."
     ["Mark Message"			vm-mark-message t]
     ["Unmark Message"			vm-unmark-message t]
     ["Mark All Messages"		vm-mark-all-messages t]
     ["Unmark All Messages"		vm-clear-all-marks t]
     ;; this doesn't work from menu items for some reason...
;     ["Next Command Uses Marks..."	vm-next-command-uses-marks t]
     ["Next Command Uses Marks..."	vm-next-command-uses-marks nil]
     )
    ("Sorting..."
     ["Sort by Date"		    (vm-sort-messages "date"             t) t]
     ["Sort by Subject"		    (vm-sort-messages "subject"          t) t]
     ["Sort by Author"		    (vm-sort-messages "author"           t) t]
     ["Sort by Recipients"	    (vm-sort-messages "recipients"       t) t]
     ["Sort by Lines"		    (vm-sort-messages "line-count"       t) t]
     ["Sort by Bytes"		    (vm-sort-messages "byte-count"       t) t]
     "---"
     ["Sort by Date (backward)"     (vm-sort-messages "reversed-date"    t) t]
     ["Sort by Subject (backward)"  (vm-sort-messages "reversed-subject" t) t]
     ["Sort by Author (backward)"   (vm-sort-messages "reversed-author"  t) t]
     ["Sort by Recipients (backward)" (vm-sort-messages
				                  "reversed-recipients"  t) t]
     ["Sort by Lines (backwards)"   (vm-sort-messages
						  "reversed-line-count"  t) t]
     ["Sort by Bytes (backward)"    (vm-sort-messages
						  "reversed-byte-count"  t) t]
     "---"
     ["Move Message Backward"		vm-move-message-backward t]
     ["Move Message Forward"		vm-move-message-forward t]

;;     ["Sort by Date"			vm-sort-by-date t]
;;     ["Sort by Subject"		vm-sort-by-subject t]
;;     ["Sort by Author"		vm-sort-by-author t]
;;     ["Sort by Author/Recipient"	vm-sort-by-author-dwim t]
;;     ["Sort by Size"			vm-sort-by-lines t]
;;     ["Group Messages..."		vm-group-messages t]
     )
    ("Digests..."
     ["Send Folder as Digest"		vm-send-digest t]
     ["Burst Message as Digest"		vm-burst-digest t]
     )
    ("Window Configurations..."
     ["Apply Window Configuration"	vm-apply-window-configuration t]
     ["Delete Window Configuration"	vm-delete-window-configuration t]
     ["Save Window Configuration"	vm-save-window-configuration t]
     )
    ("Miscellaneous..."
     ["Edit Message"			vm-edit-message t]
     ["Toggle Hidden Headers"		vm-expose-hidden-headers t]
     ["Isearch Folder..."		vm-isearch-forward t]
;     ["Discard Cached Data"		vm-discard-cached-data t]
     ["Kill Subject..."			vm-kill-subject t]
     ["Reload ~/.vm"			vm-load-rc t]
     ["Pipe Message To Command..."	vm-pipe-message-to-command t]
     ["Shell Command..."		shell-command t]
     ["Show Copying Restrictions"	vm-show-copying-restrictions t]
     ["Show No Warranty"		vm-show-no-warranty t]
     ("BBDB"
      ["Show Sender"			bbdb/vm-show-sender t]
      ["Edit Notes"			bbdb/vm-edit-notes t])
     )
    "----"
    ["Help"				vm-help t]
    ["Undo"				vm-undo t]
    ["Quit"				vm-quit t]
    ["Quit Without Saving"		vm-quit-no-change t]
    ))


(defun vm-menu (e)
  (interactive "e")
  (let ((marks-p (eq last-command 'vm-next-command-uses-marks)))
    (mouse-set-point e)
    (beginning-of-line)
    (popup-menu vm-menu)
    (if marks-p (setq this-command 'vm-next-command-uses-marks))))

(defun vm-mouse-select (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (or (eq major-mode 'vm-summary-mode) (error ""))
  (let ((vm-follow-summary-cursor t))
    (vm-scroll-forward)))

(define-key vm-mode-map 'button3 'vm-menu)
(define-key vm-summary-mode-map 'button2 'vm-mouse-select)


;;; originally defined in vm-folder.el

(require 'highlight-headers)

(or vm-highlighted-header-regexp
    (setq vm-highlighted-header-regexp "^Subject: "))

(defun vm-highlight-headers ()
  (let ((highlight-headers-regexp (or vm-highlighted-header-regexp
				      highlight-headers-regexp)))
    (highlight-headers (marker-position (vm-start-of (car vm-message-pointer)))
		       (vm-text-end-of (car vm-message-pointer))
		       t)))

(add-hook 'vm-select-message-hook 'vm-highlight-headers)


;;; Highlight the line under the mouse in the folder and summary buffers.

(defun vm-install-mouse-tracker ()
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line))

(add-hook 'vm-summary-mode-hook 'vm-install-mouse-tracker)


;;; Put the VM menu in the menubar

(defun vm-install-menubar ()
  (if (and current-menubar (not (assoc "VM" current-menubar)))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "VM" (cdr vm-menu)))))

(add-hook 'vm-mode-hook 'vm-install-menubar)
(add-hook 'vm-summary-mode-hook 'vm-install-menubar)


;;; `vm-mail' versus `mail'...

(require 'sendmail)

(defvar vm-mail-mode-menu	; derived from mail-mode-menu in sendmail.el
  (let* ((menu (copy-sequence mail-mode-menu))
	 (rest menu)
	 item)
    (while rest
      (setq item (car rest))
      (cond ((not (vectorp item)) nil)
	    ((eq (aref item 1) 'mail-send-and-exit)
	     (aset (setq item (copy-sequence item)) 1 'vm-mail-send-and-exit))
	    ((eq (aref item 1) 'mail-send)
	     (aset (setq item (copy-sequence item)) 1 'vm-mail-send))
	    ((eq (aref item 1) 'mail-yank-original)
	     (aset (setq item (copy-sequence item)) 1 'vm-yank-message)
	     (setcdr rest (cons '["Yank Message (Other Folder)" 
				  vm-yank-message-other-folder t]
				(cdr rest))))
	    )
      (setcar rest item)
      (setq rest (cdr rest)))
    menu))

(or (fboundp 'orig-mail-mode-menu)
    (fset 'orig-mail-mode-menu (symbol-function 'mail-mode-menu)))

(defun mail-mode-menu (event)
  "Pop up the mail mode menu, defined by the variable `mail-mode-menu'
or `vm-mail-mode-menu', as appropriate."
  (interactive "e")
  (let ((mail-mode-menu
	 ;; complete kludge!! VM should use its own version of mail-mode-map!!
	 (if (where-is-internal 'vm-yank-message-other-folder
				(current-local-map) t)
	     vm-mail-mode-menu
	   mail-mode-menu)))
    (orig-mail-mode-menu event)))
      

(provide 'vm-lucid)
