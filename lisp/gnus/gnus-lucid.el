;;; Mouse and font support for GNUS running in Lucid GNU Emacs
;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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


;;; Right button pops up a menu of commands in Newsgroup and Summary buffers.
;;; Middle button selects indicated newsgroup or article.

(defvar gnus-summary-menu
  '("GNUS Summary Commands"
    ["Select Article / Next Page" gnus-summary-next-page t]
    ["Prev Page" gnus-summary-prev-page t]
    ["Select Parent Article" gnus-summary-refer-parent-article t]
    "----"
    ["Beginning of Article" gnus-summary-beginning-of-article t]
    ["End of Article" gnus-summary-end-of-article t]
    ["Show All Headers" gnus-summary-show-all-headers t]
    ["ROT13 Article" gnus-summary-caesar-message t]
    ["Save Article to Mail File" gnus-summary-save-in-mail t]
    ("Sort Articles"
     ["Sort By Author" gnus-summary-sort-by-author t]
     ["Sort By Date" gnus-summary-sort-by-date t]
     ["Sort By Number" gnus-summary-sort-by-number t]
     ["Sort By Subject" gnus-summary-sort-by-subject t])
    "----"
    ["Mail Reply" gnus-summary-reply t]
    ["Mail Reply (Citing Original)" gnus-summary-reply-with-original t]
    ["Post Reply" gnus-summary-followup t]
    ["Post Reply (Citing Original)" gnus-summary-followup-with-original t]
    ["Forward Article" gnus-summary-mail-forward t]
    "----"
    ["Mark Article as Read" gnus-summary-mark-as-read-forward t]
    ["Mark Article as Unread" gnus-summary-mark-as-unread-backward t]
    ["Mark Similar Subjects as Read" gnus-summary-kill-same-subject t]
    ["Quit this Newsgroup" gnus-summary-exit t]
    ["Quit this Newsgroup (mark everything as read)"
     gnus-summary-catchup-and-exit t]
    ))

(defvar gnus-group-menu
  '("GNUS Group Commands"
    ["Select Newsgroup" gnus-group-read-group t]
    ["Unsubscribe Newsgroup" gnus-group-unsubscribe-current-group t]
    ["Get New News" gnus-group-get-new-news t]
    "----"
    ["Mark Newsgroup as Read" gnus-group-catchup t]
    ["Mark All Newsgroups as Read" gnus-group-catchup-all t]
    ["Show All Newsgroups" gnus-group-list-all-groups t]
    ["Show Subscribed Nonempty Newsgroups" gnus-group-list-groups t]
    ["Check Bogosity" gnus-group-check-bogus-groups t]
    "----"
    ["Save .newsrc" gnus-group-force-update t]
    ["GNUS Manual" gnus-info-find-node t]
    ["Suspend GNUS" gnus-group-suspend t]
    ["Quit GNUS" gnus-group-exit t]
    ))

(defvar gnus-article-menu 
  '("GNUS Article Commands"
    ["Next Page" gnus-article-next-page t]
    ["Previous Page" gnus-article-prev-page t]
    ["Pop Article History" gnus-article-pop-article t]
    ["Show Referenced Article" gnus-article-refer-article t]
    ["Show Summary" gnus-article-show-summary t]))

(defun gnus-summary-menu (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (search-forward ":" nil t)
  (popup-menu gnus-summary-menu))

(defun gnus-group-menu (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (search-forward ":" nil t)
  (popup-menu gnus-group-menu))

(defun gnus-article-menu (e)
  (interactive "@e")
  (popup-menu gnus-article-menu))

(defun gnus-group-mouse-read-group (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (search-forward ":" nil t)
  (gnus-group-read-group nil))

(defun gnus-summary-mouse-next-page (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (search-forward ":" nil t)
  (gnus-summary-next-page nil))

(define-key gnus-summary-mode-map 'button2 'gnus-summary-mouse-next-page)
(define-key gnus-group-mode-map   'button2 'gnus-group-mouse-read-group)

(define-key gnus-summary-mode-map 'button3 'gnus-summary-menu)
(define-key gnus-group-mode-map   'button3 'gnus-group-menu)
(define-key gnus-article-mode-map 'button3 'gnus-article-menu)


;;; Put message headers in boldface, etc...

(require 'highlight-headers)

(defun gnus-fontify-headers ()
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-excursion
      (save-restriction
	(widen)
	(highlight-headers (point-min) (point-max) t)))))

(make-face 'gnus-underline)
(or (face-differs-from-default-p 'gnus-underline)
    (set-face-underline-p 'gnus-underline t))

(defun gnus-hack-underlining ()
  "replaces underscore-backspace with an extent.
Also removes the extra blank lines from the article."
  (save-excursion
    (set-buffer gnus-article-buffer)
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
	(set-buffer gnus-article-buffer)
	(goto-char (point-min))
        (while (re-search-forward "\n\n\n\n*" nil t)
          (replace-match "\n\n")))))

(add-hook 'gnus-select-article-hook 'gnus-fontify-headers)
(add-hook 'gnus-article-prepare-hook 'gnus-hack-clarinews)


;;; Fontify the Newsgroups and Summary buffers
;;; Enable this either of these by turning on font-lock-mode:
;;;
;;;	(add-hook 'gnus-group-mode-hook   'turn-on-font-lock)
;;;	(add-hook 'gnus-summary-mode-hook 'turn-on-font-lock)
;;;
;;; Fontifying the *Newsgroups* buffer makes `gnus-group-list-all-groups'
;;; be awfully slow (about 50 seconds to display 2782 groups on a Sparc10.)
;;; But it's fairly fast for day-to-day use if you only subscribe to a few
;;; hundred newsgroups.
;;;
;;; Fontifying the *Summary* buffer is about the same speed (per line) as
;;; the *Newsgroups* buffer, but since it's rare to ever select more than
;;; a few hundred articles, it's not so bad. (For ~100 articles it only 
;;; takes ~2 seconds.)
;;;
;;; Possibly this could be optimized by doing the same sort of trick that
;;; we did with dired-indent-rigidly (that is, inhibit the after-change-
;;; function until the whole buffer has been generated) but preliminary
;;; tests suggest that what this would actually save is negligible.

(defconst gnus-summary-font-lock-keywords
  '(
    ;; This is how you put the article number in another face
    ;;("^..[^0-9*]*\\([0-9]+\\):"
    ;; 1 message-highlighted-header-contents)
    ;; This matches the part between [] after optional something-digits-colon
    ("^[^[]+\\[\\([^A-Za-z\n]*[0-9]+:\\)?\\([^[\n]*\\)\\]"
     2 message-headers)
    ;; This matches the part after the first ]
    ("^[^]\n]+\\]\\(.*\\)" 1 message-header-contents)
    ))

(defconst gnus-group-font-lock-keywords
  '(
    ;; This is how you put the number of  articles in another face
    ;;("^..[^0-9*]*\\([0-9]+\\):" 1 message-headers)
    ;; This matches the part after the first :
    (": \\(.*\\)" 1 message-header-contents)
    ))

;;; Highlight the line under the mouse in the Newsgroup and Summary buffers.

(defun gnus-install-mouse-tracker ()
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-line))

(add-hook 'gnus-summary-mode-hook 'gnus-install-mouse-tracker)
(add-hook 'gnus-group-mode-hook   'gnus-install-mouse-tracker)


;;; Put the GNUS menus in the menubar

(defun gnus-install-menubar ()
  (if (and current-menubar (not (assoc "GNUS" current-menubar)))
      (let ((menu (cond ((eq major-mode 'gnus-group-mode) gnus-group-menu)
			((eq major-mode 'gnus-summary-mode) gnus-summary-menu)
			(t (error "not GNUS Group or Summary mode")))))
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "GNUS" (cdr menu)))))

(add-hook 'gnus-summary-mode-hook 'gnus-install-menubar)
(add-hook 'gnus-group-mode-hook   'gnus-install-menubar)


(provide 'gnus-lucid)
