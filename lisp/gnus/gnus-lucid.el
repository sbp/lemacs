;;; Mouse and font support for GNUS running in Lucid GNU Emacs
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


;;; Right button pops up a menu of commands in Newsgroup and Subject buffers.
;;; Middle button selects indicated newsgroup or article.

(defvar gnus-Subject-menu
  '("GNUS Subject Commands"
    ["Select Article / Next Page" gnus-Subject-next-page t]
    ["Prev Page" gnus-Subject-prev-page t]
    ["Select Parent Article" gnus-Subject-refer-parent-article t]
    "----"
    ["Beginning of Article" gnus-Subject-beginning-of-article t]
    ["End of Article" gnus-Subject-end-of-article t]
    ["Show all Headers" gnus-Subject-show-all-headers t]
    ["ROT13 Article" gnus-Subject-caesar-message t]
    ["Save Article to Mail File" gnus-Subject-save-in-mail t]
    "----"
    ["Mail Reply" gnus-Subject-mail-reply t]
    ["Mail Reply (Citing Original)" gnus-Subject-mail-reply-with-original t]
    ["Post Reply" gnus-Subject-post-reply t]
    ["Post Reply (Citing Original)" gnus-Subject-post-reply-with-original t]
    "----"
    ["Mark Article as Read" gnus-Subject-mark-as-read-forward t]
    ["Mark Article as Unread" gnus-Subject-mark-as-unread-backward t]
    ["Mark Similar Subjects as Read" gnus-Subject-kill-same-subject t]
    ["Quit this Newsgroup" gnus-Subject-exit t]
    ["Quit this Newsgroup (mark everything as read)"
     gnus-Subject-catch-up-and-exit t]
    ))

(defvar gnus-Group-menu
  '("GNUS Group Commands"
    ["Select Newsgroup" gnus-Group-read-group t]
    ["Unsubscribe Newsgroup" gnus-Group-unsubscribe-current-group t]
    ["Get New News" gnus-Group-get-new-news t]
    "----"
    ["Mark Newsgroup as Read" gnus-Group-catch-up t]
    ["Mark All Newsgroups as Read" gnus-Group-catch-up-all t]
    ["Show All Newsgroups" gnus-Group-list-all-groups t]
    ["Show Subscribed Nonempty Newsgroups" gnus-Group-list-groups t]
    ["Check Bogosity" gnus-Group-check-bogus-groups t]
    "----"
    ["Save .newsrc" gnus-Group-force-update t]
    ["GNUS Manual" gnus-Info-find-node t]
    ["Suspend GNUS" gnus-Group-suspend t]
    ["Quit GNUS" gnus-Group-exit t]
    ))

(defun gnus-Subject-menu (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (search-forward ":" nil t)
  (popup-menu gnus-Subject-menu))

(defun gnus-Group-menu (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (search-forward ":" nil t)
  (popup-menu gnus-Group-menu))

(defun gnus-Group-mouse-read-group (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (search-forward ":" nil t)
  (gnus-Group-read-group nil))

(defun gnus-Subject-mouse-next-page (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (search-forward ":" nil t)
  (gnus-Subject-next-page nil))

(define-key gnus-Subject-mode-map 'button2 'gnus-Subject-mouse-next-page)
(define-key gnus-Group-mode-map   'button2 'gnus-Group-mouse-read-group)

(define-key gnus-Subject-mode-map 'button3 'gnus-Subject-menu)
(define-key gnus-Group-mode-map   'button3 'gnus-Group-menu)


;;; Put message headers in boldface, etc...

(defun gnus-fontify-headers ()
  (let* ((current 'italic)
	 p)
    (save-excursion
      (set-buffer gnus-Article-buffer)
      (goto-char (point-min))
      (while (and (not (eobp)) (not (looking-at "\n")))
	(cond
	 ((looking-at "^[^ \t\n]+[ \t]*:")
	  (set-extent-face
	   (make-extent (match-beginning 0) (match-end 0))
	   'bold)
	  (setq p (match-end 0))
	  (cond
	   ((looking-at "Subject[ \t]*:")
	    (setq current 'bold-italic)
	    (end-of-line)
	    (set-extent-face (make-extent p (point)) current))
	   ((looking-at "\\(From\\|Resent-From\\)[ \t]*:")
	    (setq current 'bold-italic)
	    (goto-char (match-end 0))
	    (or (looking-at ".*(\\(.*\\))")
		(looking-at "\\(.*\\)<")
		(looking-at "\\(.*\\)[@%]")
		(looking-at "\\(.*\\)"))
	    (end-of-line)
	    (set-extent-face (make-extent p (match-beginning 1)) 'italic)
	    (set-extent-face (make-extent (match-beginning 1) (match-end 1))
			     current)
	    (set-extent-face (make-extent (match-end 1) (point)) 'italic)
	    )
	   (t
	    (setq current 'italic)
	    (end-of-line)
	    (set-extent-face (make-extent p (point)) current))))
	 (t
	  (setq p (point))
	  (end-of-line)
	  (set-extent-face (make-extent p (point)) current)))
	(forward-line 1))
      (save-restriction
	(widen)
	(let* ((start (point))
	       (end (save-excursion
		      (goto-char (point-max))
		      (re-search-backward "\n--+ *\n" start t)
		      (point))))
	  (while (< (point) end)
	    (cond ((looking-at "^[ \t]*[A-Z]*[]}<>|][ \t]*")
		   (goto-char (match-end 0))
		   (or (save-excursion
			 (beginning-of-line)
			 (let ((case-fold-search nil)) ; aaaaah, unix...
			   (looking-at "^>From ")))
		       (setq current 'italic)))
		  ((or (looking-at "^In article\\|^In message")
		       (looking-at
		"^[^ \t].*\\(writes\\|wrote\\|said\\):\n[ \t]+[A-Z]*[]}<>|]"))
		   (setq current 'bold-italic))
		  (t (setq current nil)))
	    (cond (current
		   (setq p (point))
		   (end-of-line)
		   (set-extent-face (make-extent p (point)) current)))
	    (forward-line 1))))
      )))

(or (find-face 'gnus-underline) (make-face 'gnus-underline))
(or (face-differs-from-default-p 'gnus-underline)
    (set-face-underline-p 'gnus-underline t))

(defun gnus-hack-underlining ()
  "replaces underscore-backspace with an extent.
Also removes the extra blank lines from the article."
  (save-excursion
    (set-buffer gnus-Article-buffer)
    (goto-char (point-min))
    (while (re-search-forward "\\(\\(_\^H.\\) ?\\)+" nil t)
      (set-extent-face (make-extent (match-beginning 0) (match-end 0))
		       'gnus-underline))
    (goto-char (point-min))
    (while (re-search-forward "_\^H" nil t) (replace-match ""))))

(defun gnus-hack-clarinews ()
  (if (string-match "^clari\\.*\\|^biz\\.clarinet" gnus-newsgroup-name)
      (save-excursion
	(gnus-hack-underlining)
	(set-buffer gnus-Article-buffer)
	(goto-char (point-min))
        (while (re-search-forward "\n\n\n\n*" nil t)
          (replace-match "\n\n")))))

(add-hook 'gnus-Select-article-hook 'gnus-fontify-headers)
(add-hook 'gnus-Article-prepare-hook 'gnus-hack-clarinews)


;;; Highlight the line under the mouse in the Newsgroup and Subject buffers.

(defun gnus-install-mouse-tracker ()
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line))

(add-hook 'gnus-Subject-mode-hook 'gnus-install-mouse-tracker)
(add-hook 'gnus-Group-mode-hook   'gnus-install-mouse-tracker)


;;; Put the GNUS menus in the menubar

(defun gnus-install-menubar ()
  (if default-menubar
      (let ((menu (cond ((eq major-mode 'gnus-Group-mode) gnus-Group-menu)
			((eq major-mode 'gnus-Subject-mode) gnus-Subject-menu)
			(t (error "not GNUS Group or Subject mode")))))
	(set-buffer-menubar (copy-sequence default-menubar))
	(add-menu nil "GNUS" (cdr menu)))))

(add-hook 'gnus-Subject-mode-hook 'gnus-install-menubar)
(add-hook 'gnus-Group-mode-hook   'gnus-install-menubar)


(provide 'gnus-lucid)
