;;; Saving and piping messages under VM
;;; Copyright (C) 1989, 1990 Kyle E. Jones
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

(require 'vm)

;; (match-data) returns the match data as MARKERS, often corrupting
;; it in the process due to buffer narrowing, and the fact that buffers are
;; indexed from 1 while strings are indexed from 0. :-(
(defun vm-match-data ()
  (let ((index '(9 8 7 6 5 4 3 2 1 0))
        (list))
    (while index
      (setq list (cons (match-beginning (car index))
		       (cons (match-end (car index)) list))
	    index (cdr index)))
    list ))

(defun vm-auto-select-folder (mp auto-folder-alist)
  (condition-case ()
      (catch 'match
	(let (header alist tuple-list)
	  (setq alist auto-folder-alist)
	  (while alist
	    (setq header (vm-get-header-contents (car mp) (car (car alist))))
	    (if (null header)
		()
	      (setq tuple-list (cdr (car alist)))
	      (while tuple-list
		(if (let ((case-fold-search vm-auto-folder-case-fold-search))
		      (string-match (car (car tuple-list)) header))
		    ;; Don't waste time eval'ing an atom.
		    (if (atom (cdr (car tuple-list)))
			(throw 'match (cdr (car tuple-list)))
		      (let* ((match-data (vm-match-data))
			     (buf (get-buffer-create " *VM scratch*"))
			     (result))
			;; Set up a buffer that matches our cached
			;; match data.
			(save-excursion
			  (set-buffer buf)
			  (widen)
			  (erase-buffer)
			  (insert header)
			  ;; It appears that get-buffer-create clobbers the
			  ;; match-data.
			  ;;
			  ;; The match data is off by one because we matched
			  ;; a string and Emacs indexes strings from 0 and
			  ;; buffers from 1.
			  ;;
			  ;; Also store-match-data only accepts MARKERS!!
			  ;; AUGHGHGH!!
			  (store-match-data
			   (mapcar
			    (function (lambda (n) (and n (vm-marker n))))
			    (mapcar
			     (function (lambda (n) (and n (1+ n))))
			     match-data)))
			  (setq result (eval (cdr (car tuple-list))))
			  (throw 'match (if (listp result)
					    (vm-auto-select-folder mp result)
					  result ))))))
		(setq tuple-list (cdr tuple-list))))
	    (setq alist (cdr alist)))
	  nil ))
    (error nil)))

(defun vm-auto-archive-messages (&optional arg)
  "Save all unfiled messages that auto-match a folder via vm-auto-folder-alist
to their appropriate folders.  Deleted message are not saved.

Prefix arg means to ask user for confirmation before saving each message."
  (interactive "P")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((auto-folder)
	(archived 0))
    ;; Need separate (let ...) so vm-message-pointer can revert back
    ;; in time for (vm-update-summary-and-mode-line).
    ;; vm-last-save-folder is tucked away here since archives shouldn't affect
    ;; its value.
    (let ((vm-message-pointer vm-message-list)
	  (vm-last-save-folder vm-last-save-folder)
	  (vm-move-after-deleting))
      (while vm-message-pointer
	(and (not (vm-filed-flag (car vm-message-pointer)))
	     ;; don't archive deleted messages
	     (not (vm-deleted-flag (car vm-message-pointer)))
	     (setq auto-folder (vm-auto-select-folder vm-message-pointer
						      vm-auto-folder-alist))
	     (or (null arg)
		 (y-or-n-p
		  (format "Save message %s in folder %s? "
			  (vm-number-of (car vm-message-pointer))
			  auto-folder)))
	     (progn (vm-save-message auto-folder)
		    (and vm-delete-after-archiving (vm-delete-message 1))
		    (vm-increment archived)))
	(setq vm-message-pointer (cdr vm-message-pointer))))
    (if (zerop archived)
	(message "No messages archived")
      (message "%d message%s archived" archived (if (= 1 archived) "" "s"))
      (vm-update-summary-and-mode-line))))

(defun vm-save-message (folder &optional count)
  "Save the current message to a mail folder.
If the folder already exists, the message will be appended to it.
Prefix arg COUNT means save the next COUNT messages.  A negative COUNT means
save the previous COUNT messages.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked message in the current folder are saved; other messages are
ignored.

The saved messages are flagged as `filed'."
  (interactive
   (list
    ;; protect value of last-command
    (let ((last-command last-command))
      (vm-follow-summary-cursor)
      (let ((default (save-excursion
		       (vm-select-folder-buffer)
		       (vm-check-for-killed-summary)
		       (or (vm-auto-select-folder vm-message-pointer
						  vm-auto-folder-alist)
			   vm-last-save-folder)))
	    (dir (or vm-folder-directory default-directory)))
	(if default
	    (read-file-name (format "Save in folder: (default %s) "
				    default)
			    dir default nil )
	  (read-file-name "Save in folder: " dir nil nil))))
    (prefix-numeric-value current-prefix-arg)))
  (let (unexpanded-folder)
    (setq unexpanded-folder folder)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-empty)
    (or count (setq count 1))
    ;; Expand the filename, forcing relative paths to resolve
    ;; into the folder directory.
    (let ((default-directory
	    (expand-file-name (or vm-folder-directory default-directory))))
      (setq folder (expand-file-name folder)))
    ;; Confirm new folders, if the user requested this.
    (if (and vm-confirm-new-folders (interactive-p)
	     (not (file-exists-p folder))
	     (or (not vm-visit-when-saving) (not (get-file-buffer folder)))
	     (not (y-or-n-p (format "%s does not exist, save there anyway? "
				    folder))))
	(error "Save aborted"))
    ;; Check and see if we are currently visiting the folder
    ;; that the user wants to save to.
    (if (and (not vm-visit-when-saving) (get-file-buffer folder))
	(error "Folder %s is being visited, cannot save." folder))
    (let ((mlist (vm-select-marked-or-prefixed-messages count))
	  folder-buffer)
      (cond ((eq vm-visit-when-saving t)
	     (setq folder-buffer (or (get-file-buffer folder)
				     (find-file-noselect folder))))
	    (vm-visit-when-saving
	     (setq folder-buffer (get-file-buffer folder))))
      (save-excursion
	(while mlist
	  (set-buffer (marker-buffer (vm-start-of (car mlist))))
	  (vm-save-restriction
	   (widen)
	   (if (vm-modflag-of (car mlist))
	       (vm-stuff-attributes (car mlist) t))
	   (if (null folder-buffer)
	       (write-region (vm-start-of (car mlist))
			     (vm-end-of (car mlist))
			     folder t 'quiet)
	     (let ((start (vm-start-of (car mlist)))
		   (end (vm-end-of (car mlist))))
	       (save-excursion
		 (set-buffer folder-buffer)
		 (let (buffer-read-only)
		   (vm-save-restriction
		    (widen)
		    (save-excursion
		      (goto-char (point-max))
		      (insert-buffer-substring
		       (marker-buffer (vm-start-of (car mlist)))
		       start end))
		    ;; vars should exist and be local
		    ;; but they may have strange values
		    (cond ((eq major-mode 'vm-mode)
			   (vm-increment vm-messages-not-on-disk)
			   (vm-set-buffer-modified-p (buffer-modified-p))
			   (vm-clear-modification-flag-undos))))))))
	   (if (null (vm-filed-flag (car mlist)))
	       (vm-set-filed-flag (car mlist) t))
	   (setq mlist (cdr mlist)))))
      (if folder-buffer
	  (progn
	    (save-excursion
	      (set-buffer folder-buffer)
	      (if (eq major-mode 'vm-mode)
		  (let (buffer-read-only)
		    (vm-check-for-killed-summary)
		    (vm-assimilate-new-messages)
		    (vm-update-summary-and-mode-line))))
	    (message "Message%s saved to buffer %s" (if (/= 1 count) "s" "")
		     (buffer-name folder-buffer)))
	(message "Message%s saved to %s" (if (/= 1 count) "s" "") folder)))
    (setq vm-last-save-folder unexpanded-folder)
    (if vm-delete-after-saving
	(vm-delete-message count))
    (vm-update-summary-and-mode-line)))

(defun vm-save-message-sans-headers (file &optional count)
  "Save the current message to a file, without its header section.
If the file already exists, the message will be appended to it.
Prefix arg COUNT means save the next COUNT messages.  A negative COUNT means
save the previous COUNT.

When invoked on marked messages (via vm-next-command-uses-marks),
all marked messages in the current folder are saved; other messages are
ignored.

The saved messages are flagged as `written'.

This command should NOT be used to save message to mail folders; use
vm-save-message instead (normally bound to `s')."
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command))
     (vm-follow-summary-cursor)
     (list
      (read-file-name "Write text to file: " nil nil nil)
      (prefix-numeric-value current-prefix-arg))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (or count (setq count 1))
  (setq file (expand-file-name file))
  ;; Check and see if we are currently visiting the file
  ;; that the user wants to save to.
  (if (and (not vm-visit-when-saving) (get-file-buffer file))
      (error "File %s is being visited, cannot save." file))
  (let ((mlist (vm-select-marked-or-prefixed-messages count))
	file-buffer)
    (cond ((eq vm-visit-when-saving t)
	   (setq file-buffer (or (get-file-buffer file)
				 (find-file-noselect file))))
	  (vm-visit-when-saving
	   (setq file-buffer (get-file-buffer file))))
    (save-excursion
      (while mlist
	(set-buffer (marker-buffer (vm-start-of (car mlist))))
	(vm-save-restriction
	 (widen)
	 (if (null file-buffer)
	     (write-region (vm-text-of (car mlist))
			   (vm-text-end-of (car mlist))
			   file t 'quiet)
	   (let ((start (vm-text-of (car mlist)))
		 (end (vm-text-end-of (car mlist))))
	     (save-excursion
	       (set-buffer file-buffer)
	       (save-excursion
		 (let (buffer-read-only)
		   (vm-save-restriction
		    (widen)
		    (save-excursion
		      (goto-char (point-max))
		      (insert-buffer-substring
		       (marker-buffer (vm-start-of (car mlist)))
		       start end))))))))
	(if (null (vm-written-flag (car mlist)))
	    (vm-set-written-flag (car mlist) t))
	(setq mlist (cdr mlist)))))
    (if file-buffer
	(message "Message%s written to buffer %s" (if (/= 1 count) "s" "")
		 (buffer-name file-buffer))
      (message "Message%s written to %s" (if (/= 1 count) "s" "") file)))
  (vm-update-summary-and-mode-line))

(defun vm-pipe-message-to-command (command prefix-arg)
  "Run shell command with the some or all of the current message as input.
By default the entire message is used.
With one \\[universal-argument] the text portion of the message is used.
With two \\[universal-argument]'s the header portion of the message is used.

When invoked on marked messages (via vm-next-command-uses-marks),
each marked message is successively piped to the shell command,
one message per command invocation.

Output, if any, is displayed.  The message is not altered."
  (interactive
   ;; protect value of last-command
   (let ((last-command last-command))
     (vm-follow-summary-cursor)
     (list (read-string "Pipe to command: " vm-last-pipe-command)
	   current-prefix-arg)))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (setq vm-last-pipe-command command)
  (let ((buffer (get-buffer-create "*Shell Command Output*"))
	(pop-up-windows (and pop-up-windows (eq vm-mutable-windows t)))
	;; prefix arg doesn't have "normal" meaning here, so only call
	;; vm-select-marked-or-prefixed-messages if we're using marks.
	(mlist (if (eq last-command 'vm-next-command-uses-marks)
		   (vm-select-marked-or-prefixed-messages 0)
		 (list (car vm-message-pointer)))))
    (save-excursion (set-buffer buffer) (erase-buffer))
    (while mlist
      (set-buffer (marker-buffer (vm-start-of (car mlist))))
      (save-restriction
	(widen)
	(cond ((equal prefix-arg nil)
	       (narrow-to-region (vm-start-of (car mlist))
				 (vm-end-of (car mlist))))
	      ((equal prefix-arg '(4))
	       (narrow-to-region (vm-text-of (car mlist))
				 (vm-text-end-of (car mlist))))
	      ((equal prefix-arg '(16))
	       (narrow-to-region (vm-start-of (car mlist))
				 (vm-text-of (car mlist))))
	      (t (narrow-to-region (vm-start-of (car mlist))
				   (vm-end-of (car mlist)))))
	(let ((pop-up-windows (and pop-up-windows (eq vm-mutable-windows t))))
	  (call-process-region (point-min) (point-max)
			       (or shell-file-name "sh")
			       nil buffer nil "-c" command)))
      (setq mlist (cdr mlist)))
    (set-buffer buffer)
    (if (not (zerop (buffer-size)))
	(display-buffer buffer))))
