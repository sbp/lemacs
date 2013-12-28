;;; Miscellaneous commands for GNUS newsreader
;; Copyright (C) 1989 Fujitsu Laboratories LTD.
;; Copyright (C) 1989, 1990 Masanobu UMEDA

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

(provide 'gnusmisc)
(require 'gnus)

(defvar gnus-Browse-killed-mode-hook nil
  "*A hook for GNUS Browse-Killed Mode.")

(defvar gnus-Browse-killed-buffer "*Killed Newsgroup*")
(defvar gnus-Browse-killed-mode-map nil)
(defvar gnus-winconf-browse-killed nil)

(autoload 'timezone-make-date-arpa-standard "timezone")

(put 'gnus-Browse-killed-mode 'mode-class 'special)

;;;
;;; GNUS Browse-Killed Mode
;;;

;; Some ideas are due to roland@wheaties.ai.mit.edu (Roland McGrath).
;; I'd like to thank him very much.

;; Make the buffer to be managed by GNUS.

(or (memq gnus-Browse-killed-buffer gnus-buffer-list)
    (setq gnus-buffer-list
	  (cons gnus-Browse-killed-buffer gnus-buffer-list)))

(if gnus-Browse-killed-mode-map
    nil
  (setq gnus-Browse-killed-mode-map (make-keymap))
  (suppress-keymap gnus-Browse-killed-mode-map t)
  (define-key gnus-Browse-killed-mode-map " " 'gnus-Group-next-group)
  (define-key gnus-Browse-killed-mode-map "\177" 'gnus-Group-prev-group)
  (define-key gnus-Browse-killed-mode-map "\C-n" 'gnus-Group-next-group)
  (define-key gnus-Browse-killed-mode-map "\C-p" 'gnus-Group-prev-group)
  (define-key gnus-Browse-killed-mode-map "n" 'gnus-Group-next-group)
  (define-key gnus-Browse-killed-mode-map "p" 'gnus-Group-prev-group)
  (define-key gnus-Browse-killed-mode-map "y" 'gnus-Browse-killed-yank)
  (define-key gnus-Browse-killed-mode-map "\C-y" 'gnus-Browse-killed-yank)
  (define-key gnus-Browse-killed-mode-map "l" 'gnus-Browse-killed-groups)
  (define-key gnus-Browse-killed-mode-map "q" 'gnus-Browse-killed-exit)
  (define-key gnus-Browse-killed-mode-map "\C-c\C-c" 'gnus-Browse-killed-exit)
  (define-key gnus-Browse-killed-mode-map "\C-c\C-i" 'gnus-Info-find-node))

(defun gnus-Browse-killed-mode ()
  "Major mode for browsing the killed newsgroups.
All normal editing commands are turned off.
Instead, these commands are available:
\\{gnus-Browse-killed-mode-map}

The killed newsgroups are saved in the quick startup file (.newsrc.el)
unless it against the options line in the startup file (.newsrc).

Entry to this mode calls gnus-Browse-killed-mode-hook with no arguments,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  ;; Gee.  Why don't you upgrade?
  (cond ((boundp 'mode-line-modified)
	 (setq mode-line-modified "--- "))
	((listp (default-value 'mode-line-format))
	 (setq mode-line-format
	       (cons "--- " (cdr (default-value 'mode-line-format)))))
	(t
	 (setq mode-line-format
	       "--- GNUS: Killed Newsgroups  %[(%m)%]----%3p-%-")))
  (setq major-mode 'gnus-Browse-killed-mode)
  (setq mode-name "Browse-Killed")
  (setq mode-line-buffer-identification	"GNUS: Killed Newsgroups")
  (use-local-map gnus-Browse-killed-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-Browse-killed-mode-hook))

(defun gnus-Browse-killed-groups ()
  "Browse the killed newsgroups.
The keys y and C-y yank the newsgroup on the current line into the
Newsgroups buffer."
  (interactive)
  (or gnus-killed-assoc
      (error "No killed newsgroups"))
  ;; Save current window configuration if this is first invocation..
  (or (get-buffer-window gnus-Browse-killed-buffer)
      (setq gnus-winconf-browse-killed
	    (current-window-configuration)))
  ;; Prepare browsing buffer.
  (gnus-pop-to-buffer (get-buffer-create gnus-Browse-killed-buffer))
  (gnus-Browse-killed-mode)
  (let ((buffer-read-only nil)
	(killed-assoc gnus-killed-assoc))
    (erase-buffer)
    (while killed-assoc
      (insert (gnus-Group-prepare-line (car killed-assoc)))
      (setq killed-assoc (cdr killed-assoc)))
    (goto-char (point-min))
    ))

(defun gnus-Browse-killed-yank ()
  "Yank current newsgroup to Newsgroup buffer."
  (interactive)
  (let ((group (gnus-Group-group-name)))
    (if group
	(let* ((buffer-read-only nil)
	       (killed (assoc group gnus-killed-assoc)))
	  (gnus-pop-to-buffer gnus-Group-buffer) ;Needed to adjust point.
	  (if killed
	      (gnus-Group-insert-group killed))
	  (gnus-pop-to-buffer gnus-Browse-killed-buffer)
	  (beginning-of-line)
	  (delete-region (point)
			 (progn (forward-line 1) (point)))
	  )))
  (gnus-Browse-killed-check-buffer))

(defun gnus-Browse-killed-check-buffer ()
  "Exit if the buffer is empty by deleting the window and killing the buffer."
  (and (null gnus-killed-assoc)
       (get-buffer gnus-Browse-killed-buffer)
       (gnus-Browse-killed-exit)))

(defun gnus-Browse-killed-exit ()
  "Exit this mode by deleting the window and killing the buffer."
  (interactive)
  (and (get-buffer-window gnus-Browse-killed-buffer)
       (delete-window (get-buffer-window gnus-Browse-killed-buffer)))
  (kill-buffer gnus-Browse-killed-buffer)
  ;; Restore previous window configuration if available.
  (and gnus-winconf-browse-killed
       (set-window-configuration gnus-winconf-browse-killed))
  (setq gnus-winconf-browse-killed nil))


;;;
;;; kill/yank newsgroup commands of GNUS Group Mode
;;;

(defun gnus-Group-kill-region (begin end)
  "Kill newsgroups in current region (excluding current point).
The killed newsgroups can be yanked by using \\[gnus-Group-yank-group]."
  (interactive "r")
  (let ((lines
	 ;; Exclude a line where current point is on.
	 (1-
	  ;; Count lines.
	  (save-excursion
	    (count-lines
	     (progn
	       (goto-char begin)
	       (beginning-of-line)
	       (point))
	     (progn
	       (goto-char end)
	       (end-of-line)
	       (point)))))))
    (goto-char begin)
    (beginning-of-line)			;Important when LINES < 1
    (gnus-Group-kill-group lines)))

(defun gnus-Group-kill-group (n)
  "Kill newsgroup on current line, repeated prefix argument N times.
The killed newsgroups can be yanked by using \\[gnus-Group-yank-group]."
  (interactive "p")
  (let ((buffer-read-only nil)
	(group nil))
    (while (> n 0)
      (setq group (gnus-Group-group-name))
      (or group
	  (signal 'end-of-buffer nil))
      (beginning-of-line)
      (delete-region (point)
		     (progn (forward-line 1) (point)))
      (gnus-kill-newsgroup group)
      (setq n (1- n))
      ;; Add to killed newsgroups in the buffer if exists.
      (if (get-buffer gnus-Browse-killed-buffer)
	  (save-excursion
	    (set-buffer gnus-Browse-killed-buffer)
	    (let ((buffer-read-only nil))
	      (goto-char (point-min))
	      (insert (gnus-Group-prepare-line (car gnus-killed-assoc)))
	      )))
      )
    (search-forward ":" nil t)
    ))

(defun gnus-Group-yank-group ()
  "Yank the last newsgroup killed with \\[gnus-Group-kill-group],
inserting it before the newsgroup on the line containging point."
  (interactive)
  (gnus-Group-insert-group (car gnus-killed-assoc))
  ;; Remove killed newsgroups from the buffer if exists.
  (if (get-buffer gnus-Browse-killed-buffer)
      (save-excursion
	(set-buffer gnus-Browse-killed-buffer)
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (delete-region (point-min)
			 (progn (forward-line 1) (point)))
	  )))
  (gnus-Browse-killed-check-buffer))

(defun gnus-Group-insert-group (info)
  "Insert newsgroup at current line using gnus-newsrc-assoc INFO."
  (if (null gnus-killed-assoc)
      (error "No killed newsgroups"))
  (if (not gnus-have-all-newsgroups)
      (error
       (substitute-command-keys
	"Not all newsgroups are displayed.  Type \\[gnus-Group-list-all-groups] to display all newsgroups.")))
  (let ((buffer-read-only nil)
	(group (gnus-Group-group-name)))
    (gnus-insert-newsgroup info group)
    (beginning-of-line)
    (insert (gnus-Group-prepare-line info))
    (forward-line -1)
    (search-forward ":" nil t)
    ))


;;; Rewrite Date: field in GMT to local

(defun gnus-gmt-to-local ()
  "Rewrite Date: field described in GMT to local in current buffer.
The variable gnus-local-timezone is used for local time zone.
Intended to be used with gnus-Article-prepare-hook."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (narrow-to-region (point-min)
			(progn (search-forward "\n\n" nil 'move) (point)))
      (goto-char (point-min))
      (if (re-search-forward "^Date:[ \t]\\(.*\\)$" nil t)
	  (let ((buffer-read-only nil)
		(date (buffer-substring (match-beginning 1) (match-end 1))))
	    (delete-region (match-beginning 1) (match-end 1))
	    (insert
	     (timezone-make-date-arpa-standard date nil gnus-local-timezone))
	    ))
      )))
