;;; chat.el --- a method for talking to asynchronous processes.

;;; Copyright (C) 1993 Free Software Foundation, Inc.
;;;
;; Author: Felix Lee <flee@cse.psu.edu>
;; Version: !Id: chat.el,v 1.7 1993/02/05 01:49:31 flee Exp !
;; Modified by jwz.

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

;;; Code:

;;(require 'backquote)	; only needed at compile-time

;; Data from an asynchronous process gets appended to the process's
;; buffer as soon as it's available.

;; The data may come in pieces smaller than the units a caller wants
;; to deal with, so here are functions that wait for conditions, like
;; receiving a particular string, or receiving N bytes.

;; The functions also keep a caller from seeing more data than it
;; wants: each process buffer has a data mark that keeps track of how
;; much of the buffer has actually been waited for.

;; A typical use is:
;;	(chat/with-data-until-string "\n" proc
;;	  (buffer-substring (point-min) (1- (point-max))))
;; which waits until we receive a line of text, and returns the line
;; without the "\n".

;; XXX It would be nice if we could provide timeouts for waiting on
;; data, but this is really awkward to do.

;; XXX There's something that's not quite right about this module; but
;; let's run with it for a little while and see what happens.

;; XXX point in the process buffer is left alone, up to the caller to
;; modify.  Maybe it should always be set to the beginning of input?

;;;;;;;;;;;;;;;;
;;; The data mark.

(defvar chat/data-marker nil
  "A buffer's data marker.")
(make-variable-buffer-local 'chat/data-marker)

(defmacro chat/set-data-marker (location)
  "Set the current buffer's data marker to LOCATION.  Returns the data
marker."
  (` (set-marker
      (if (markerp chat/data-marker)
	  chat/data-marker
	(setq chat/data-marker (make-marker)))
      (, location))))

;;;;;;;;;;;;;;;;
;;; Waiting for data.

;; XXX Need to add some comments on efficiency.
;; wait-for-length is linear,
;; wait-for-string is rectangular,
;; wait-for-regexp is quadratic or worse,
;; wait-for-dot-crlf is linear.

;; This is the error that's signalled when you try to chat with a
;; process that's gone.
(put 'no-process 'error-conditions '(error no-process))
(put 'no-process 'error-message "Connection is broken")

(defmacro chat/with-buffer-of (proc &rest forms)
  "Set the current buffer to PROC's buffer, and evaluate FORMS."
  (` (save-excursion
       (set-buffer (process-buffer (, proc)))
       (,@ forms))))
(put 'chat/with-buffer-of 'lisp-indent-hook 1)

;; XXX should we consider stopped processes as runnable?
(defmacro chat/accept-from (proc)
  (` (if (memq (process-status (, proc)) '(open run))
	 (accept-process-output (, proc))
       (signal 'no-process (, proc)))))

(defun chat/wait-for-length (size proc)
  "Wait until we have SIZE characters of data from PROC.  When
successful, returns true and sets PROC's data mark to the location
after SIZE.  Does not change the match data.  Signals 'no-process if
PROC has died."
  (chat/with-buffer-of proc
    (while (< (point-max) (+ (point-min) size))
      (chat/accept-from proc))
    (chat/set-data-marker (+ (point-min) size))))

(defun chat/wait-for-string (string proc)
  "Wait until we see STRING in PROC's data.  When successful, returns
true and sets PROC's data mark to the end of the STRING match.  Also
sets the match data.  Signals 'no-process if PROC has died."
  (chat/with-buffer-of proc
    (goto-char (point-min))
    (while (not (search-forward string nil 'eob))
      ;; This mess is mostly because 'accept-process-output does nasty
      ;; things to point.
      (goto-char (prog1 (- (point) (length string))
		   (chat/accept-from proc))))
    (chat/set-data-marker (point))))

(defun chat/wait-for-regexp (regexp proc)
  "Wait until we see REGEXP in PROC's data.  When successful, returns
true and sets PROC's data mark to the end of the REGEXP match.  Also
sets the match data.  Signals 'no-process if PROC has died."
  (chat/with-buffer-of proc
    (save-excursion
      (goto-char (point-min))
      (while (not (re-search-forward regexp nil t))
	(chat/accept-from proc)
	;; We can't optimize the next search, because we don't know
	;; anything about what the regexp won't match.
	(goto-char (point-min)))
      (chat/set-data-marker (point)))))

(defun chat/wait-for-dot-crlf (proc)
  "The same as (chat/wait-for-regexp \"^\\\\.\\r\\n\" PROC), but
considerably faster."
  (chat/with-buffer-of proc
    (save-excursion
      (goto-char (point-min))
      (if (not (looking-at ".\r?\n"))
	  (while (not (and (search-forward "\n." nil 'eob)
			   (looking-at "\r?\n")))
	    (if (eobp)
		;; This mess is mainly because 'accept-process-output
		;; does nasty things with point.
		(goto-char (prog1 (- (point) 3)
			     (chat/accept-from proc))))))
      (forward-line)
      (chat/set-data-marker (point)))))

;;;;;;;;;;;;;;;;
;;; Processing the data.

(defmacro chat/with-data-of (proc &rest forms)
  "Set the current buffer to PROC's buffer, narrowed to the region up
to PROC's data mark, and evaluate FORMS.  And then the data up to the
data mark is deleted.  Returns the value of FORMS.

If you discover you didn't really need all the data and want to push
some back, use 'chat/set-data-marker to change the data mark.  Or
consider using 'chat/with-buffer-of instead."
  (` (chat/with-buffer-of (, proc)
       (prog1
	   (save-restriction
	     (narrow-to-region (point-min) chat/data-marker)
	     (,@ forms))
	 (delete-region (point-min) chat/data-marker)))))
(put 'chat/with-data-of 'lisp-indent-hook 1)

(defun chat/data-of (proc)
  "Returns a string that contains PROC's data up to its data mark, and
deletes the data.  If you need to do any parsing, you probably want to
be using 'chat/with-data-of instead."
  (chat/with-data-of proc
    (prog1 (buffer-string)
      (delete-region (point-min) (point-max)))))

(defun chat/delete-pending-data (proc)
  "Clear out as much of PROC's pending data that we can without
blocking.  Returns nothing."
  (chat/with-buffer-of proc
    (widen)
    (while (< (point-min) (point-max))
      (delete-region (point-min) (point-max))
      (accept-process-output))))

;;;;;;;;;;;;;;;;
;;; Waiting and processing.

;; XXX factor the expansions of these routines for efficiency?

(defmacro chat/with-data-for-length (length proc &rest forms)
  "(LENGTH PROC FORMS ...).  Equivalent to
	(chat/wait-for-length LENGTH PROC)
	(chat/with-data-of PROC FORMS ...)
"
  (` (progn
       (chat/wait-for-length (, length) (, proc))
       (chat/with-data-of (, proc) (,@ forms)))))
(put 'chat/with-data-for-length 'lisp-indent-hook 2)

(defmacro chat/with-data-until-string (string proc &rest forms)
  "(STRING PROC FORMS ...).  Equivalent to
	(chat/wait-for-string STRING PROC)
	(chat/with-data-of PROC FORMS ...)
"
  (` (progn
       (chat/wait-for-string (, string) (, proc))
       (chat/with-data-of (, proc) (,@ forms)))))
(put 'chat/with-data-until-string 'lisp-indent-hook 2)

(defmacro chat/with-data-until-regexp (regexp proc &rest forms)
  "(REGEXP PROC FORMS ...).  Equivalent to
	(chat/wait-for-regexp REGEXP PROC)
	(chat/with-data-of PROC FORMS ...)
"
  (` (progn
       (chat/wait-for-regexp (, regexp) (, proc))
       (chat/with-data-of (, proc) (,@ forms)))))
(put 'chat/with-data-until-regexp 'lisp-indent-hook 2)

(defmacro chat/with-data-until-dot-crlf (proc &rest forms)
  "(PROC FORMS ...).  Equivalent to
	(chat/wait-for-dot-crlf PROC)
	(chat/with-data-of PROC FORMS ...)
"
  (` (progn
       (chat/wait-for-dot-crlf (, proc))
       (chat/with-data-of (, proc) (,@ forms)))))
(put 'chat/with-data-until-dot-crlf 'lisp-indent-hook 1)

(provide 'chat)

;;; chat.el ends here
