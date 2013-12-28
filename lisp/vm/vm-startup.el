;;; Entry points for VM
;;; Copyright (C) 1994 Kyle E. Jones
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

(defun vm (&optional folder read-only)
  "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox causes any contents of the system mailbox to
be moved and appended to the resulting buffer.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' expunges deleted messages and saves the buffered folder to
disk.

See the documentation for vm-mode for more information."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  ;; set inhibit-local-variables non-nil to protect
  ;; against letter bombs.
  ;; set enable-local-variables to nil for newer Emacses
  (catch 'done
    (let ((full-startup (not (bufferp folder)))
	  folder-buffer first-time totals-blurb
	  preserve-auto-save-file)
      (setq folder-buffer
	    (if (bufferp folder)
		folder
	      (let ((file (or folder (expand-file-name vm-primary-inbox))))
		(if (file-directory-p file)
		    ;; MH code perhaps... ?
		    (error "%s is a directory" file)
		  (or (vm-get-file-buffer file)
		      (let ((default-directory
			      (or (and vm-folder-directory
				       (expand-file-name vm-folder-directory))
				  default-directory))
			    (inhibit-local-variables t)
			    (enable-local-variables nil))
			(message "Reading %s..." file)
			(prog1 (find-file-noselect file)
			  (message "Reading %s... done" file))))))))
      (set-buffer folder-buffer)
      (vm-check-for-killed-summary)
      ;; If the buffer's not modified then we know that there can be no
      ;; messages in the folder that are not on disk.
      (or (buffer-modified-p) (setq vm-messages-not-on-disk 0))
      (setq first-time (not (eq major-mode 'vm-mode))
	    preserve-auto-save-file (and buffer-file-name
					  (not (buffer-modified-p))
					  (file-newer-than-file-p
					   (make-auto-save-file-name)
					   buffer-file-name)))
      ;; Force the folder to be read only if the auto
      ;; save file contains information the user might not
      ;; want overwritten, i.e. recover-file might be
      ;; desired.  What we want to avoid is an auto-save.
      ;; Making the folder read only will keep it
      ;; subsequent actions from modifying the buffer in a
      ;; way that triggers an auto save.
      ;;
      ;; Also force the folder read-only if it was read only and
      ;; not already in vm-mode, since there's probably a good
      ;; reason for this.
      (setq vm-folder-read-only (or preserve-auto-save-file read-only
				    (and first-time buffer-read-only)))
      ;; If this is not a VM mode buffer then some initialization
      ;; needs to be done 
      (if first-time
	  (progn
	    (if (fboundp 'buffer-disable-undo)
		(buffer-disable-undo (current-buffer))
	      ;; obfuscation to make the v19 compiler not whine
	      ;; about obsolete functions.
	      (let ((x 'buffer-flush-undo))
		(funcall x (current-buffer))))
	    (abbrev-mode 0)
	    (auto-fill-mode 0)
	    (vm-mode-internal)))
      (vm-assimilate-new-messages nil t)
      (if first-time
	  (progn
	    (vm-gobble-visible-header-variables)
	    (vm-gobble-bookmark)
	    (vm-gobble-summary)
	    (vm-gobble-labels)
	    (vm-start-itimers-if-needed)))

      ;; say this NOW, before the non-previewers read a message,
      ;; alter the new message count and confuse themselves.
      (if full-startup
	  ;; save blurb so we can repeat it later as necessary.
	  (setq totals-blurb (vm-emit-totals-blurb)))

      (vm-thoughtfully-select-message)
      (if vm-message-list
	  (vm-preview-current-message)
	(vm-update-summary-and-mode-line))
      (if full-startup
	  (save-excursion
	    (vm-display (current-buffer) t nil nil)
	    (if	(and (vm-should-generate-summary)
		     ;; don't generate a summary if recover-file is
		     ;; likely to happen, since recover-file does
		     ;; nothing useful in a summary buffer.
		     (not preserve-auto-save-file))
		(progn (vm-summarize nil)
		       (vm-display vm-summary-buffer t nil nil)))
	    ;; nil in the command list handles
	    ;; "emacs -f vm" from the command line.
	    (vm-display nil nil
			'(vm vm-other-frame vm-other-window
			  vm-visit-folder
			  vm-visit-folder-other-frame
			  vm-visit-folder-other-window
			  recover-file revert-buffer nil)
			(list (or this-command 'vm) 'startup))))
      (if full-startup
	  (message totals-blurb))
      ;; Warn user about auto save file, if appropriate.
      (if (and full-startup preserve-auto-save-file)
	  (message 
	   (substitute-command-keys
	    "Auto save file is newer; consider \\[recover-file].  FOLDER IS READ ONLY.")))
      ;; if we're not doing a full startup or if doing more would
      ;; trash the auto save file that we need to preserve,
      ;; stop here.
      (if (or (not full-startup) preserve-auto-save-file)
	  (throw 'done t))
      (if (and vm-auto-get-new-mail
	       (not vm-block-new-mail)
	       (not vm-folder-read-only))
	  (progn
	    (message "Checking for new mail for %s..."
		     (or buffer-file-name (buffer-name)))
	    (if (vm-get-spooled-mail)
		(progn
		  (vm-assimilate-new-messages t)
		  (setq totals-blurb (vm-emit-totals-blurb))
		  (if (vm-thoughtfully-select-message)
		      (vm-preview-current-message)
		    (vm-update-summary-and-mode-line))))
	    (message totals-blurb)))

      (run-hooks 'vm-visit-folder-hook)

      ;; Display copyright and copying info unless
      ;; user says no.
      ;; Check this-command so we don't make the user wait if
      ;; they call vm non-interactively from some other program.
      (if (and (not vm-inhibit-startup-message)
	       (not vm-startup-message-displayed)
	       (or (memq this-command '(vm vm-visit-folder))
		   ;; for emacs -f vm
		   (null last-command)))
	  (progn
	    (vm-display-startup-message)
	    (if (not (input-pending-p))
		(message totals-blurb)))))))

(defun vm-other-frame (&optional folder read-only)
  "Like vm, but run in a newly created frame."
  (interactive (list nil current-prefix-arg))
  (vm-goto-new-frame)
  (vm folder read-only))

(defun vm-other-window (&optional folder read-only)
  "Like vm, but run in a different window."
  (interactive (list nil current-prefix-arg))
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (vm folder read-only))

(put 'vm-mode 'mode-class 'special)

(defun vm-mode (&optional read-only)
  "Major mode for reading mail.

Commands:
   h - summarize folder contents
 C-t - toggle threads display

   n - go to next message
   p - go to previous message
   N - like `n' but ignores skip-variable settings
   P - like `p' but ignores skip-variable settings
 M-n - go to next unread message
 M-p - go to previous unread message
 RET - go to numbered message (uses prefix arg or prompts in minibuffer)
 TAB - go to last message seen
   ^ - go to parent of this message
 M-s - incremental search through the folder

   t - display hidden headers
 SPC - expose message body or scroll forward a page
   b - scroll backward a page
   < - go to beginning of current message
   > - go to end of current message

   d - delete message, prefix arg deletes messages forward
 C-d - delete message, prefix arg deletes messages backward
   u - undelete
   k - flag for deletion all messages with same subject as the current message

   r - reply (only to the sender of the message)
   R - reply with included text from the current message
 M-r - extract and resend bounced message
   f - followup (reply to all recipients of message)
   F - followup with included text from the current message
   z - forward the current message
   m - send a message
   B - resend the current message to another user.
   c - continue composing the most recent message you were composing

   @ - digestify and mail entire folder contents (the folder is not modified)
   * - burst a digest into individual messages, and append and assimilate these
       message into the current folder.

   G - sort messages by various keys

   g - get any new mail that has arrived in the system mailbox
       (new mail is appended to the disk and buffer copies of the
       primary inbox.)
   v - visit another mail folder

   e - edit the current message
   j - discard cached information about the current message

   s - save current message in a folder (appends if folder already exists)
   w - write current message to a file without its headers (appends if exists)
   S - save entire folder to disk, does not expunge
   A - save unfiled messages to their vm-auto-folder-alist specified folders
   # - expunge deleted messages (without saving folder)
   q - quit VM, deleted messages are not expunged, folder is
       saved to disk if it is modified.  new messages are changed
       to be flagged as just unread.
   x - exit VM with no change to the folder

 M N - use marks; the next vm command will affect only marked messages
       if it makes sense for the command to do so

       M M - mark the current message
       M U - unmark the current message
       M m - mark all messages
       M u - unmark all messages
       M ? - help for the mark commands
       M C - mark messages matches by a virtual folder selector
       M c - unmark messages matches by a virtual folder selector
       M T - mark thread tree rooted at the current message
       M t - unmark thread tree rooted at the current message
       M S - mark messages with the same subject as the current message
       M s - unmark messages with the same subject as the current message
 M ? - partial help for mark commands

 W S - save the current window configuration to a name
 W D - delete a window configuration
 W W - apply a configuration
 W ? - help for the window configuration commands

 V V - visit a virtual folder (must be defined in vm-virtual-folder-alist)
 V C - create a virtual folder composed of a subset of the
       current folder's messages.
 V A - apply the selectors of a named virtual folder to the
       messages in the current folder and create a virtual folder
       containing the selected messages.
 V M - toggle whether this virtual folder's messages mirror the
       underlying real messages' attributes.
 V ? - help for virtual folder commands

 C-_ - undo, special undo that retracts the most recent
             changes in message attributes and labels.  Expunges
             message edits, and saves cannot be undone.  C-x u is
             also bound to this command.

   a - set message attributes

 l a - add labels to message
 l d - delete labels from message

   L - reload your VM init file, ~/.vm

   % - change a folder to another type

   ? - help

   ! - run a shell command
   | - run a shell command with the current message as input

 M-C - view conditions under which you may redistribute VM
 M-W - view the details of VM's lack of a warranty

Use M-x vm-submit-bug-report to submit a bug report.

Variables:
   vm-arrived-message-hook
   vm-auto-center-summary
   vm-auto-folder-alist
   vm-auto-folder-case-fold-search
   vm-auto-get-new-mail
   vm-auto-next-message
   vm-berkeley-mail-compatibility
   vm-check-folder-types
   vm-convert-folder-types
   vm-circular-folders
   vm-confirm-new-folders
   vm-confirm-quit
   vm-crash-box
   vm-default-folder-type
   vm-delete-after-archiving
   vm-delete-after-bursting
   vm-delete-after-saving
   vm-delete-empty-folders
   vm-digest-burst-type
   vm-digest-center-preamble
   vm-digest-preamble-format
   vm-digest-send-type
   vm-display-buffer-hook
   vm-edit-message-hook
   vm-folder-directory
   vm-folder-read-only
   vm-follow-summary-cursor
   vm-forward-message-hook
   vm-forwarded-headers
   vm-forwarding-digest-type
   vm-forwarding-subject-format
   vm-highlighted-header-face
   vm-highlighted-header-regexp
   vm-honor-page-delimiters
   vm-in-reply-to-format
   vm-included-text-attribution-format
   vm-included-text-discard-header-regexp
   vm-included-text-headers
   vm-included-text-prefix
   vm-inhibit-startup-message
   vm-invisible-header-regexp
   vm-jump-to-new-messages
   vm-jump-to-unread-messages
   vm-keep-sent-messages
   vm-keep-crash-boxes
   vm-mail-header-from
   vm-mail-mode-hook
   vm-mode-hook
   vm-move-after-deleting
   vm-move-after-undeleting
   vm-move-messages-physically
   vm-mutable-windows
   vm-mutable-frames
   vm-preview-lines
   vm-preview-read-messages
   vm-primary-inbox
   vm-quit-hook
   vm-recognize-pop-maildrops
   vm-reply-hook
   vm-reply-ignored-reply-tos
   vm-reply-ignored-addresses
   vm-reply-subject-prefix
   vm-resend-bounced-discard-header-regexp
   vm-resend-bounced-headers
   vm-resend-bounced-message-hook
   vm-resend-discard-header-regexp
   vm-resend-headers
   vm-resend-message-hook
   vm-retrieved-spooled-mail-hook
   vm-rfc1153-digest-discard-header-regexp
   vm-rfc1153-digest-headers
   vm-rfc934-digest-discard-header-regexp
   vm-rfc934-digest-headers
   vm-search-using-regexps
   vm-select-message-hook
   vm-select-new-message-hook
   vm-select-unread-message-hook
   vm-send-digest-hook
   vm-skip-deleted-messages
   vm-skip-read-messages
   vm-spool-files
   vm-startup-with-summary
   vm-strip-reply-headers
   vm-summary-arrow
   vm-summary-format
   vm-summary-highlight-face
   vm-summary-mode-hook
   vm-summary-redo-hook
   vm-summary-show-threads
   vm-summary-subject-no-newlines
   vm-summary-thread-indent-level
   vm-trust-From_-with-Content-Length
   vm-undisplay-buffer-hook
   vm-unforwarded-header-regexp
   vm-virtual-folder-alist
   vm-virtual-mirror
   vm-visible-headers
   vm-visit-folder-hook
   vm-visit-when-saving
   vm-window-configuration-file
"
  (interactive "P")
  (vm (current-buffer) read-only)
  (vm-display nil nil '(vm-mode) '(vm-mode)))

(defun vm-visit-folder (folder &optional read-only)
  "Visit a mail file.
VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-select-folder-buffer)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (read-file-name
	      (format "Visit%s folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil) current-prefix-arg))))
  (vm-session-initialization)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (setq vm-last-visit-folder folder)
  (let ((default-directory (or vm-folder-directory default-directory)))
    (setq folder (expand-file-name folder)))
  (vm folder read-only))

(defun vm-visit-folder-other-frame (folder &optional read-only)
  "Like vm-visit-folder, but run in a newly created frame."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-select-folder-buffer)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (read-file-name
	      (format "Visit%s folder in other frame:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil) current-prefix-arg))))
  (vm-goto-new-frame)
  (vm-visit-folder folder read-only))

(defun vm-visit-folder-other-window (folder &optional read-only)
  "Like vm-visit-folder, but run in a different window."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-select-folder-buffer)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (read-file-name
	      (format "Visit%s folder in other window:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil) current-prefix-arg))))
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (vm-visit-folder folder read-only))

(put 'vm-virtual-mode 'mode-class 'special)

(defun vm-virtual-mode (&rest ignored)
  "Mode for reading multiple mail folders as one folder.

The commands available are the same commands that are found in
vm-mode, except that a few of them are not applicable to virtual
folders.

vm-virtual-mode is not a normal major mode.  If you run it, it
will not do anything.  The entry point to vm-virtual-mode is
vm-visit-virtual-folder.")

(defun vm-visit-virtual-folder (folder-name &optional read-only)
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (completing-read "Visit virtual folder: " vm-virtual-folder-alist nil t)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (not (assoc folder-name vm-virtual-folder-alist))
      (error "No such virtual folder, %s" folder-name))
  (let ((buffer-name (concat "(" folder-name ")"))
	first-time blurb)
    (set-buffer (get-buffer-create buffer-name))
    (setq first-time (not (eq major-mode 'vm-virtual-mode)))
    (if first-time
	(progn
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (setq mode-name "VM Virtual"
		mode-line-format vm-mode-line-format
		buffer-read-only t
		vm-folder-read-only read-only
		vm-label-obarray (make-vector 29 0)
		vm-virtual-folder-definition
		  (assoc folder-name vm-virtual-folder-alist))
	  (vm-build-virtual-message-list nil)
	  (use-local-map vm-mode-map)
	  ;; save this for last in case the user interrupts.
	  ;; an interrupt anywhere before this point will cause
	  ;; everything to be redone next revisit.
	  (setq major-mode 'vm-virtual-mode)
	  (setq blurb (vm-emit-totals-blurb))
	  (if vm-summary-show-threads
	      (vm-sort-messages "thread"))
	  (if (vm-thoughtfully-select-message)
	      (vm-preview-current-message)
	    (vm-update-summary-and-mode-line))
	  (if (vm-should-generate-summary)
	      (progn (vm-summarize nil)
		     (vm-display vm-summary-buffer t nil nil)))
	  (message blurb)))
    (vm-display (current-buffer) t nil nil)
    (vm-display nil nil '(vm-visit-virtual-folder
			  vm-visit-virtual-folder-other-frame
			  vm-visit-virtual-folder-other-window
			  vm-create-virtual-folder
			  vm-apply-virtual-folder)
		(list this-command 'startup))
    ;; check interactive-p so as not to bog the user down if they
    ;; run this function from within another function.
    (and (interactive-p) (not vm-inhibit-startup-message)
	 (not vm-startup-message-displayed)
	 (vm-display-startup-message)
	 (message blurb))))

(defun vm-visit-virtual-folder-other-frame (folder-name &optional read-only)
  "Like vm-visit-virtual-folder, but run in a newly created frame."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (completing-read "Visit virtual folder in other frame: "
		       vm-virtual-folder-alist nil t)
      current-prefix-arg)))
  (vm-goto-new-frame)
  (vm-visit-virtual-folder folder-name read-only))

(defun vm-visit-virtual-folder-other-window (folder-name &optional read-only)
  "Like vm-visit-virtual-folder, but run in a different window."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (completing-read "Visit virtual folder in other window: "
		       vm-virtual-folder-alist nil t)
      current-prefix-arg)))
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (vm-visit-virtual-folder folder-name read-only))

(defun vm-mail ()
  "Send a mail message from within VM, or from without."
  (interactive)
  (vm-session-initialization)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-mail-internal)
  (run-hooks 'vm-mail-hook)
  (run-hooks 'vm-mail-mode-hook))

(defun vm-mail-other-frame ()
  "Like vm-mail, but run in a newly created frame."
  (interactive)
  (vm-goto-new-frame)
  (vm-mail))

(defun vm-mail-other-window ()
  "Like vm-mail, but run in a different window."
  (interactive)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (vm-mail))

(defun vm-submit-bug-report ()
  "Submit a bug report, with pertinent information to the VM bug list."
  (interactive)
  (require 'reporter)
  ;; make sure the user doesn't try to use vm-mail here.
  (let ((reporter-mailer '(mail))
	(pop-up-windows (eq vm-mutable-windows t))
	(pop-up-frames vm-mutable-frames))
    (reporter-submit-bug-report
     vm-maintainer-address
     (concat "VM " vm-version)
     (list
      'vm-arrived-message-hook
      'vm-auto-center-summary
;; don't send this by default, might be personal stuff in here.
;;      'vm-auto-folder-alist
      'vm-auto-folder-case-fold-search
      'vm-auto-get-new-mail
      'vm-auto-next-message
      'vm-berkeley-mail-compatibility
      'vm-check-folder-types
      'vm-circular-folders
      'vm-confirm-new-folders
      'vm-confirm-quit
      'vm-convert-folder-types
      'vm-crash-box
      'vm-default-folder-type
      'vm-delete-after-archiving
      'vm-delete-after-bursting
      'vm-delete-after-saving
      'vm-delete-empty-folders
      'vm-digest-burst-type
      'vm-digest-identifier-header-format
      'vm-digest-center-preamble
      'vm-digest-preamble-format
      'vm-digest-send-type
      'vm-edit-message-hook
      'vm-edit-message-mode
      'vm-flush-interval
      'vm-folder-directory
      'vm-folder-read-only
      'vm-follow-summary-cursor
      'vm-forward-message-hook
      'vm-forwarded-headers
      'vm-forwarding-digest-type
      'vm-forwarding-subject-format
      'vm-highlighted-header-regexp
      'vm-honor-page-delimiters
      'vm-in-reply-to-format
      'vm-included-text-attribution-format
      'vm-included-text-discard-header-regexp
      'vm-included-text-headers
      'vm-included-text-prefix
      'vm-inhibit-startup-message
      'vm-init-file
      'vm-invisible-header-regexp
      'vm-jump-to-new-messages
      'vm-jump-to-unread-messages
      'vm-keep-crash-boxes
      'vm-keep-sent-messages
      'vm-mail-header-from
      'vm-mail-hook
      'vm-mail-mode-hook
      'vm-mode-hook
      'vm-mode-hooks
      'vm-move-after-deleting
      'vm-move-after-undeleting
      'vm-move-messages-physically
      'vm-movemail-program
      'vm-mutable-frames
      'vm-mutable-windows
      'vm-pop-md5-program
      'vm-preview-lines
      'vm-preview-read-messages
      'vm-primary-inbox
      'vm-quit-hook
      'vm-recognize-pop-maildrops
      'vm-reply-hook
      'vm-reply-ignored-addresses
      'vm-reply-ignored-reply-tos
      'vm-reply-subject-prefix
      'vm-resend-bounced-discard-header-regexp
      'vm-resend-bounced-headers
      'vm-resend-bounced-message-hook
      'vm-resend-discard-header-regexp
      'vm-resend-headers
      'vm-resend-message-hook
      'vm-retrieved-spooled-mail-hook
      'vm-rfc1153-digest-discard-header-regexp
      'vm-rfc1153-digest-headers
      'vm-rfc934-digest-discard-header-regexp
      'vm-rfc934-digest-headers
      'vm-search-using-regexps
      'vm-select-message-hook
      'vm-select-new-message-hook
      'vm-select-unread-message-hook
      'vm-send-digest-hook
      'vm-skip-deleted-messages
      'vm-skip-read-messages
;; don't send vm-spool-files by default, might contain passwords
;;      'vm-spool-files
      'vm-startup-with-summary
      'vm-strip-reply-headers
      'vm-summary-format
      'vm-summary-highlight-face
      'vm-summary-mode-hook
      'vm-summary-mode-hooks
      'vm-summary-redo-hook
      'vm-summary-show-threads
      'vm-summary-subject-no-newlines
      'vm-summary-thread-indent-level
      'vm-summary-uninteresting-senders
      'vm-summary-uninteresting-senders-arrow
      'vm-tale-is-an-idiot
      'vm-unforwarded-header-regexp
      'vm-virtual-folder-alist
      'vm-virtual-mirror
      'vm-visible-headers
      'vm-visit-folder-hook
      'vm-visit-when-saving
      'vm-window-configuration-file
;; not a user variable technically, but useful to know
      'vm-window-configurations
;; non-VM variables, but related to the display, useful to know
      'pop-up-windows
      (if (vm-fsf-emacs-19-p) 'pop-up-frames)
      'next-screen-context-lines
;; see what the user had loaded
      'features
      )
     nil
     nil
     "Please change the Subject header to a concise bug description.\nRemember to cover the basics, that is, what you expected to\nhappen and what in fact did happen.")
    (save-excursion
      (goto-char (point-min))
      (mail-position-on-field "Subject")
      (beginning-of-line)
      (delete-region (point) (progn (forward-line) (point)))
      (insert "Subject: VM " vm-version " induces a brain tumor in the viewer.\n         It is the tumor that creates the hallucinations.\n"))))

(defun vm-load-init-file (&optional interactive)
  (interactive "p")
  (if (or (not vm-init-file-loaded) interactive)
      (load vm-init-file (not interactive) (not interactive) t))
  (setq vm-init-file-loaded t)
  (vm-display nil nil '(vm-load-init-file) '(vm-load-init-file)))

(defun vm-session-initialization ()
  ;; If this is the first time VM has been run in this Emacs session,
  ;; do some necessary preparations.
  (if (or (not (boundp 'vm-session-beginning))
	  vm-session-beginning)
      (progn
	(random t)
	(vm-load-init-file)
	(if (not vm-window-configuration-file)
	    (setq vm-window-configurations vm-default-window-configuration)
	  (or (vm-load-window-configurations vm-window-configuration-file)
	      (setq vm-window-configurations vm-default-window-configuration)))
	(setq vm-buffers-needing-display-update (make-vector 29 0))
	(setq vm-session-beginning nil))))

(autoload 'reporter-submit-bug-report "reporter")
(autoload 'timezone-make-date-sortable "timezone")
(autoload 'rfc822-addresses "rfc822")
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'mail-fetch-field "mail-utils")
(autoload 'mail-position-on-field "mail-utils")
(autoload 'mail-send "sendmail")
(autoload 'mail-mode "sendmail")
(autoload 'mail-extract-address-components "mail-extr")
(autoload 'set-tapestry "tapestry")
(autoload 'tapestry "tapestry")
(autoload 'tapestry-replace-tapestry-element "tapestry")
(autoload 'tapestry-nullify-tapestry-elements "tapestry")
(autoload 'tapestry-remove-frame-parameters "tapestry")
