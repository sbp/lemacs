;;;  dired-lucid.el: A menu for the dired-mode.
;;;  v2.90; 7 Dec 1993
;;;  Copyright (C) 1993  Heiko Muenkel
;;;  email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 1, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'mode-motion)

;; Popup and Pulldown Menu

(defvar dired-menu
  '("Dired Commands"
    ["Open File" dired-find-file t]
    ["Open File Other Window" dired-find-file-other-window t]
    ["View File" dired-view-file t]
    "----"
    ("Mark"
     ["Mark File" dired-mark-subdir-or-file t]
     ["Mark Files in Region" dired-mark-region (mark)]
     ["Mark Files by Regexp..." dired-mark-files-regexp t]
     ["Mark All Directories" dired-mark-directories t]
     ["Mark All Executables" dired-mark-executables t]
     ["Mark All Symbolic Links" dired-mark-symlinks t]
     "----"
     ["Unmark File" dired-unmark-subdir-or-file t]
     ["Unmark All Files" (dired-unflag-all-files nil) t]
     ["Unmark All Files (Query)..." (dired-unflag-all-files nil t) t]
     ["Unmark Files in Region" dired-unmark-region (mark)]
     )
    ("Copy/Link"
     ["Copy Files..." dired-do-copy t]
     ["Copy Files by Regexp..." dired-do-copy-regexp t]
     "----"
     ["Symlink Files in Directory..." dired-do-symlink t]
     ["Symlink Files in Directory by Regexp..." dired-do-symlink-regexp t]
     "----"
     ["Hard-Link Files in Directory..." dired-do-hardlink t]
     ["Hard-Link Files in Directory by Regexp..." dired-do-hardlink-regexp t]
     )
    ("Rename"
     ["Rename Marked Files..." dired-do-move t]
     ["Rename Files by Regexp..." dired-do-rename-regexp t]
     "----"
     ["Downcase Names of Marked Files..." dired-downcase t]
     ["Upcase Names of Marked Files..." dired-upcase t]
     )
    ("Delete"
     ["Delete Marked Files..." dired-do-delete t]
     ["Delete Flaged Files..." dired-do-deletions t]
     "----"
     ["Flag Marked Files for Deletion" dired-flag-file-deleted t]
     ["Flag Files in Region for Deletion" dired-flag-region (mark)]
     ["Flag Files for Deletion by Regexp..."  dired-flag-regexp-files t]
     ["Flag Backup Files for Deletion" dired-clean-directory t]
     ["Flag Autosave Files for Deletion" dired-flag-auto-save-files t]
     "----"
     ["Unflag Marked Files" dired-unflag t]
     ["Unflag Backup Files" dired-backup-unflag t]
     ["Unflag All Files" (dired-unflag-all-files nil) t]
     ["Unflag All Files (Query)..." (dired-unflag-all-files nil) t]
     ["Unflag Files in Region" dired-unflag-region (mark)]
     )
    ("Shell commands"
     ["Compress Marked Files..." dired-do-compress t]
     ["Uncompress Marked Files..." dired-do-uncompress t]
     ["Print Marked Files..." dired-do-print t]
     ["Shell Command on Marked Files..." dired-do-shell-command t]
     "----"
     ["Load Marked Files" dired-do-load t]
     ["Byte-Compile Marked Files..." dired-do-byte-compile t]
     "----"
     ["Diff File against Backup" dired-backup-diff t]
     ["Diff File..." dired-diff t]
     "----"
     ["Change Permissions of Marked Files..." dired-do-chmod t]
     ["Change Group of Marked Files..." dired-do-chgrp t]
     ["Change Owner of Marked Files..." dired-do-chown t]
     )
    "----"
    ("Directory"
     ["Up Directory" dired-up-directory t]
     ["Home Directory" (dired "~/") t]
     "----"
     ["Dired..." dired t]
     ["Dired Other Window..." dired-other-window t]
     ["Redisplay All Files" revert-buffer t]
     "----"
     ["Create Directory..." dired-create-directory t]
     "----"
     ["Insert Subdirectory" dired-insert-subdir t]
     ["Hide Subdirectory" dired-kill-subdir t]
     ["Hide All Subdirectories..." dired-kill-tree t]
     )
    ("Goto"
     ["Next Directory Line" dired-next-dirline t]
     ["Previous Directory Line" dired-prev-dirline t]
     ["Next Marked File" dired-next-marked-file t]
     ["Previous Marked File" dired-prev-marked-file t]
     "----"
     ["File..." dired-goto-file t]
     ["Top of Directory..." dired-goto-subdir t]
     ["Down Directory" dired-tree-down t]
     ["Up Directory" dired-tree-up t]
     )
    ("Display"
     ["Undisplay Line or Subdirectory" dired-kill-line-or-subdir t]
     ["Undisplay Tree" dired-kill-tree t]
     ["Undisplay Marked Lines" dired-do-kill t]
     "----"
     ["Redisplay All Files" revert-buffer t]
     ["Redisplay All Marked Files" dired-do-redisplay t]
     ["Undo" dired-undo t]
     "----"
     ["Sort by Date/Name (Toggle)" dired-sort-toggle-or-edit t]
     ["Edit `ls' Switches..." (dired-sort-toggle-or-edit t) t]
     )
    "----"
    ["Explain Last Failure" dired-why t]
    )
  "*The menu for Dired.")

(defun dired-install-menubar ()
  "Installs the Dired menu at the menubar."
  (if (and current-menubar (not (assoc "Dired" current-menubar)))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-menu nil "Dired" (cdr dired-menu))))
  (make-local-variable 'mode-motion-hook)
  (setq mode-motion-hook 'mode-motion-highlight-line)
  )
(add-hook 'dired-mode-hook 'dired-install-menubar)

(defun dired-popup-menu (event)
  "Display the Dired Menu."
  (interactive "@e")
  (mouse-set-point event)
  (dired-next-line 0)
  (popup-menu dired-menu))

(defun dired-mouse-find-file (event)
  "Edit the file under the mouse."
  (interactive "e")
  (mouse-set-point event)
  (dired-next-line 0)
  (dired-find-file))

(defun dired-mark-region (&optional form-to-eval)
  "Mark all files in the region."
  (interactive)
  (or form-to-eval (setq form-to-eval '(dired-mark-subdir-or-file nil)))
  (save-excursion
    (let ((end (region-end)))
      (goto-char (region-beginning))
      (beginning-of-line)
      (while (<= (point) end)
	(save-excursion (eval form-to-eval))
	(forward-line 1)))))

(defun dired-unmark-region ()
  "Unmark all files in the region."
  (interactive)
  (dired-mark-region '(dired-unmark-subdir-or-file nil)))

(defun dired-flag-region ()
  "Flag all files in the region for deletion."
  (interactive)
  (dired-mark-region '(dired-flag-file-deleted nil)))

(defun dired-unflag-region ()
  "Unflag all files in the region for deletion."
  (interactive)
  (dired-mark-region '(dired-unflag 1)))


(define-key dired-mode-map 'button2 'dired-mouse-find-file)
(define-key dired-mode-map 'button3 'dired-popup-menu)


(provide 'dired-lucid)
