;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Access authorization functions for the w3 browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base 64 encoding functions
;;; This code was converted to lisp code by me from the C code in
;;; ftp://cs.utk.edu/pub/MIME/b64encode.c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-b64-encoding
 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
 "The string to use to encode with base 64.")

(defun b0 (x) (aref w3-b64-encoding (lsh x -18)))
(defun b1 (x) (aref w3-b64-encoding (logand (lsh x -12) 63)))
(defun b2 (x) (aref w3-b64-encoding (logand (lsh x -6) 63)))
(defun b3 (x) (aref w3-b64-encoding (logand x 63)))

(defun b64-encode (str)
  "Do base64 encoding on string STR and return the encoded string.
This code was converted to lisp code by me from the C code in
ftp://cs.utk.edu/pub/MIME/b64encode.c."
  (let (
	(word 0)			; The word to translate
	w1 w2 w3
	)
    (cond
     ((> (length str) 3)
      (format "%s%s"
	      (b64-encode (substring str 0 3))
	      (b64-encode (substring str 3 nil))))
     ((= (length str) 3)
      (setq w1 (aref str 0)
	    w2 (aref str 1)
	    w3 (aref str 2)
	    word (logior
		  (lsh (logand w1 255) 16)
		  (lsh (logand w2 255) 8)
		  (logand w3 255)))
      (format "%c%c%c%c" (b0 word) (b1 word) (b2 word) (b3 word)))
     ((= (length str) 2)
      (setq w1 (aref str 0)
	    w2 (aref str 1)
	    word (logior
		  (lsh (logand w1 255) 16)
		  (lsh (logand w2 255) 8)
		  0))
      (format "%c%c%c=" (b0 word) (b1 word) (b2 word)))
     ((= (length str) 1)
      (setq w1 (aref str 0)
	    word (logior
		  (lsh (logand w1 255) 16)
		  0))
      (format "%c%c==" (b0 word) (b1 word)))
     (t ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The authorization code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-basic-auth-storage nil
  "Where usernames and passwords are stored.  Its value is an assoc list of
assoc lists.  The first assoc list is keyed by the server name.  The cdr of
this is an assoc list based on the 'directory' specified by the url we are
looking up.")

(defun w3-basic-auth (url &optional prompt overwrite)
  "Get the username/password for the specified URL.
If optional argument PROMPT is non-nil, ask for the username/password
to use for the url and its descendants.  If optional third argument
OVERWRITE is non-nil, overwrite the old username/password pair if it
is found in the assoc list."
  (let (server path user pass byserv retval)
    (if (string-match ":/*\\([^/]+\\)/+\\(.*\\)" url)
	(setq server (substring url (match-beginning 1) (match-end 1))
	      path (substring url (match-beginning 2) (match-end 2))))
    (setq byserv (cdr-safe (assoc server w3-basic-auth-storage)))
    (if (not byserv)			; Server not found
	(if prompt
	    (progn
	      (setq user (read-string "Username: " (user-real-login-name))
		    pass (ange-ftp-read-passwd "Password: "))
	      (setq w3-basic-auth-storage
		    (cons (list server
				(cons path
				      (setq retval
					    (b64-encode (format "%s:%s"
								user pass)))))
			  w3-basic-auth-storage)))
	  (setq retval nil))
      (progn				; Found the server
	(setq retval (cdr-safe (assoc path byserv)))
	(if (not retval)		; No exact match, check directories
	    (while (and byserv (not retval))
	      (if (string-match (concat (regexp-quote (car (car byserv)))
					"/*[^/]+") path)
		  (setq retval (cdr (car byserv)))
		(setq byserv (cdr byserv)))))
	(if (or (and (not retval) prompt) overwrite)
	    (progn
	      (setq user (read-string "Username: " (user-real-login-name))
		    pass (ange-ftp-read-passwd "Password: ")
		    retval (b64-encode (format "%s:%s" user pass))
		    byserv (assoc server w3-basic-auth-storage))
	      (setcdr byserv
		      (cons (cons path retval) (cdr byserv)))))))
    retval))

(defun w3-pubkey-auth (url &optional prompt overwrite)
  "Same as w3-basic-auth, but for public key encryption.  For right now,
this function always returns nil.  Need to read up on public key
authorization schemes in general before I do."
  nil)

(provide 'w3-auth)
