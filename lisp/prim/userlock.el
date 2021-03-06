;;; userlock.el --- handle file access contention between multiple users

;; Copyright (C) 1985, 1986, 1993 Free Software Foundation, inc.

;; Maintainer: FSF
;; Keywords: internal

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

;;; Commentary:

;; This file is autoloaded to handle certain conditions
;; detected by the file-locking code within Emacs.
;; The two entry points are `ask-user-about-lock' and
;; `ask-user-about-supersession-threat'.

;;; Code:

(put 'file-locked 'error-conditions '(file-locked file-error error))

(defun ask-user-about-lock-minibuf (fn opponent)
  (save-window-excursion
    (let (answer)
      (while (null answer)
	(message "%s is locking %s: action (s, q, p, ?)? " opponent fn)
	(let ((tem (let ((inhibit-quit t)
			 (cursor-in-echo-area t))
		     (prog1 (downcase (read-char))
		            (setq quit-flag nil)))))
	  (if (= tem help-char)
	      (ask-user-about-lock-help)
	    (setq answer (assoc tem '((?s . t)
				      (?q . yield)
				      (?\C-g . yield)
				      (?p . nil)
				      (?? . help))))
	    (cond ((null answer)
		   (beep)
		   (message "Please type q, s, or p; or ? for help")
		   (sit-for 3))
		  ((eq (cdr answer) 'help)
		   (ask-user-about-lock-help)
		   (setq answer nil))
		  ((eq (cdr answer) 'yield)
		   (signal 'file-locked (list "File is locked" fn opponent)))))))
      (cdr answer))))

(defun ask-user-about-lock-help ()
  (with-output-to-temp-buffer "*Help*"
    (princ "It has been detected that you want to modify a file that someone else has
already started modifying in EMACS.

You can <s>teal the file; The other user becomes the
  intruder if (s)he ever unmodifies the file and then changes it again.
You can <p>roceed; you edit at your own (and the other user's) risk.
You can <q>uit; don't modify this file.")))

(put
 'file-supersession 'error-conditions '(file-supersession file-error error))

(defun ask-user-about-supersession-threat-minibuf (fn)
  (save-window-excursion
    (let (answer)
      (while (null answer)
	(message "%s changed on disk; really edit the buffer? (y, n or C-h) "
                 (file-name-nondirectory fn))
	(let ((tem (downcase (let ((cursor-in-echo-area t))
			       (read-char)))))
	  (setq answer
		(if (= tem help-char)
		    'help
		  (cdr (assoc tem '((?n . yield)
				    (?\C-g . yield)
				    (?y . proceed)
				    (?? . help))))))
	  (cond ((null answer)
		 (beep)
		 (message "Please type y or n; or ? for help")
		 (sit-for 3))
		((eq answer 'help)
		 (ask-user-about-supersession-help)
		 (setq answer nil))
		((eq answer 'yield)
		 (signal 'file-supersession
			 (list "File changed on disk" fn))))))
      (message
        "File on disk now will become a backup file if you save these changes.")
      (setq buffer-backed-up nil))))

(defun ask-user-about-supersession-help ()
  (with-output-to-temp-buffer "*Help*"
    (princ "You want to modify a buffer whose disk file has changed
since you last read it in or saved it with this buffer.

If you say `y' to go ahead and modify this buffer,
you risk ruining the work of whoever rewrote the file.
If you say `n', the change you started to make will be aborted.

Usually, you should type `n' and then `M-x revert-buffer',
to get the latest version of the file, then make the change again.")))



;;; dialog-box versions

(defun ask-user-about-lock-dbox (fn opponent)
  (let ((echo-keystrokes 0)
	(dbox
	 (cons
	  (format "%s is locking %s\n
	It has been detected that you want to modify a file that
	someone else has already started modifying in Emacs."
		  opponent fn)
	  '(["Steal Lock\n\nThe other user will\nbecome the intruder" steal t]
	    ["Proceed\n\nEdit file at your own\n\(and the other user's) risk"
	     proceed t]
	    nil
	    ["Abort\n\nDon't modify the buffer\n" yield t]))))
    (popup-dialog-box dbox)
    (catch 'aual-done
      (while t
	(let ((event (next-command-event)))
	  (cond ((and (menu-event-p event) (eq (event-object event) 'proceed))
		 (throw 'aual-done nil))
		((and (menu-event-p event) (eq (event-object event) 'steal))
		 (throw 'aual-done t))
		((and (menu-event-p event) (eq (event-object event) 'yield))
		 (signal 'file-locked (list "File is locked" fn opponent)))
		((button-release-event-p event) ;; don't beep twice
		 nil)
		(t
		 (beep)
		 (message "please answer the dialog box"))))))))

(defun ask-user-about-supersession-threat-dbox (fn)
  (let ((echo-keystrokes 0)
	(dbox
	 (cons
	  (format "File %s has changed on disk
since its buffer was last read in or saved.

Do you really want to edit the buffer? " fn)
	  '(["Yes\n\nEdit the buffer anyway,\nignoring the disk file"
	     proceed t]
	    ["No\n\nDon't modify the buffer\n" yield t]
	    nil
	    ["No\n\nDon't modify the buffer\nbut revert it" revert t]
	    ))))
    (popup-dialog-box dbox)
    (catch 'auast-done
      (while t
	(let ((event (next-command-event)))
	  (cond ((and (menu-event-p event) (eq (event-object event) 'proceed))
		 (throw 'auast-done nil))
		((and (menu-event-p event) (eq (event-object event) 'yield))
		 (signal 'file-supersession (list "File changed on disk" fn)))
		((and (menu-event-p event) (eq (event-object event) 'revert))
		 (or (equal fn (buffer-file-name))
		     (error
		      "ask-user-about-supersession-threat called bogusly"))
		 (revert-buffer nil t)
		 (signal 'file-supersession
			 (list "File changed on disk; reverted" fn)))
		((button-release-event-p event) ;; don't beep twice
		 nil)
		(t
		 (beep)
		 (message "please answer the dialog box"))))))))


;;; top-level

;;;###autoload
(defun ask-user-about-lock (fn opponent)
  "Ask user what to do when he wants to edit FILE but it is locked by USER.
This function has a choice of three things to do:
  do (signal 'buffer-file-locked (list FILE USER))
    to refrain from editing the file
  return t (grab the lock on the file)
  return nil (edit the file even though it is locked).
You can rewrite it to use any criterion you like to choose which one to do."
  (discard-input)
  (if (and (fboundp 'popup-dialog-box)
	   (or (button-press-event-p last-command-event)
	       (button-release-event-p last-command-event)
	       (menu-event-p last-command-event)))
      (ask-user-about-lock-dbox fn opponent)
    (ask-user-about-lock-minibuf fn opponent)))

;;;###autoload
(defun ask-user-about-supersession-threat (fn)
  "Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called."
  (discard-input)
  (if (and (fboundp 'popup-dialog-box)
	   (or (button-press-event-p last-command-event)
	       (button-release-event-p last-command-event)
	       (menu-event-p last-command-event)))
      (ask-user-about-supersession-threat-dbox fn)
    (ask-user-about-supersession-threat-minibuf fn)))
