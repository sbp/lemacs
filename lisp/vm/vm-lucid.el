;;; Mouse and font support for VM running in Lucid GNU Emacs
;; Copyright (C) 1992 Free Software Foundation, Inc.

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

(require 'vm)

(defvar vm-menu
  '("View Mail"
    ["Next Nondeleted Message"		 vm-next-message t]
    ["Previous Nondeleted Message"	 vm-previous-message t]
    ("Motion ..."
     ["Goto Last Seen Message"		vm-goto-message-last-seen t]
     ["Goto Message"			vm-goto-message	t]
     ["Move Message Backward"		vm-move-message-backward t]
     ["Move Message Forward"		vm-move-message-forward t]
     ["Next Message"			vm-Next-message t]
     ["Next Unread Message"		vm-next-unread-message t]
     ["Previous Message"		vm-Previous-message t]
     ["Previous Unread Message"		vm-previous-unread-message t]
     )
    "----"
    ["Scroll Message Forward"		vm-scroll-forward t]
    ["Scroll Message Backward"		vm-scroll-backward t]
    ["Beginning Of Message"		vm-beginning-of-message t]
    ["End Of Message"			vm-end-of-message t]
    "----"
    ["Display Summary"			vm-summarize t]
    ["Get New Mail"			vm-get-new-mail t]

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
     ["Write File"			vm-write-file t]
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
    ("Marking Message..."
     ["Mark Message"			vm-mark-message t]
     ["Unmark Message"			vm-unmark-message t]
     ["Unmark All Messages"		vm-mark-all-messages t]
     ["Clear All Marks"			vm-clear-all-marks t]
     ;;["Next Command Uses Marks"	vm-next-command-uses-marks t]
     )
    ("Digests..."
     ["Send Digest"			vm-send-digest t]
     ["Burst Digest"			vm-burst-digest t]
     )
    ("Window Configurations..."
     ["Apply Window Configuration"	vm-apply-window-configuration t]
     ["Delete Window Configuration"	vm-delete-window-configuration t]
     ["Save Window Configuration"	vm-save-window-configuration t]
     ["Window Help"			vm-window-help t]
     )
    ("Miscellaneous..."
     ["Edit Message"			vm-edit-message t]
     ["Group Messages"			vm-group-messages t]
     ["Expose Hidden Headers"		vm-expose-hidden-headers t]
     ["Isearch Forward"			vm-isearch-forward t]
     ["Discard Cached Data"		vm-discard-cached-data t]
     ["Kill Subject"			vm-kill-subject t]
     ["Load Rc"				vm-load-rc t]
     ["Pipe Message To Command"		vm-pipe-message-to-command t]
     ["Shell Command"			shell-command t]
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
  (mouse-set-point e)
  (beginning-of-line)
  (popup-menu vm-menu))

(defun vm-mouse-select (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (or (eq major-mode 'vm-summary-mode) (error ""))
  (let ((vm-follow-summary-cursor t))
    (vm-scroll-forward)))

(define-key vm-mode-map 'button2 'vm-mouse-select)
(define-key vm-mode-map 'button3 'vm-menu)


;;; Highlighting message headers

(or (find-face 'vm-header-face) (make-face 'vm-header-face))
(or (face-differs-from-default-p 'vm-header-face)
    (copy-face 'bold 'vm-header-face))

(or (find-face 'vm-header-contents-face) (make-face 'vm-header-contents-face))
(or (face-differs-from-default-p 'vm-header-contents-face)
    (copy-face 'italic 'vm-header-contents-face))

(or (find-face 'vm-header-highlight-face) (make-face 'vm-header-highlight-face))
(or (face-differs-from-default-p 'vm-header-highlight-face)
    (progn
      (copy-face 'vm-header-contents-face 'vm-header-highlight-face)
      (set-face-underline-p 'vm-header-highlight-face t)))

;; originally defined in vm-folder.el

(defun vm-highlight-headers (message window)
  (save-restriction
    (save-excursion
      (widen)
      (narrow-to-region (vm-start-of (car vm-message-pointer))
			(vm-text-of (car vm-message-pointer)))
      ;; delete extents on current message headers.
      (map-extents (function (lambda (x y)
			       (if (eq (extent-data x) 'vm)
				   (delete-extent x))))
		   (current-buffer) (point-min) (point-max) nil)
      (goto-char (point-min))
      (while (not (= (following-char) ?\n))
	(if (looking-at vm-generic-header-regexp)
	    (let ((start (match-beginning 2))
		  (end (match-end 2))
		  extent)
	      (set-extent-face
	       (setq extent (make-extent (match-beginning 1) (match-end 1)))
	       'vm-header-face)
	      (set-extent-data extent 'vm)
	      (set-extent-face (setq extent (make-extent start end))
			       (if (and vm-highlighted-header-regexp
					(looking-at
					 vm-highlighted-header-regexp))
				   'vm-header-highlight-face
				 'vm-header-contents-face))
	      (set-extent-data extent 'vm)
	      (goto-char end))
	  (forward-line 1))))))


;;; Highlight the line under the mouse in the folder and summary buffers.

(defun vm-install-mouse-tracker ()
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line))

(add-hook 'vm-summary-mode-hooks 'vm-install-mouse-tracker)


;;; Put the VM menu in the menubar

(defun vm-install-menubar ()
  (if default-menubar
      (progn
	(set-buffer-menubar (copy-sequence default-menubar))
	(add-menu nil "VM" (cdr vm-menu)))))

(add-hook 'vm-mode-hooks 'vm-install-menubar)
(add-hook 'vm-summary-mode-hooks 'vm-install-menubar)


(provide 'vm-lucid)
