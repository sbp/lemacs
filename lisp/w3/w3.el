;;; LCD Archive Entry:
;;; w3-mode|William M. Perry|wmperry@indiana.edu|
;;; Major mode for browsing World Wide Web nodes|
;;; 94-5-12|2.1.25|Location undetermined
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
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
;;; This is a major mode for browsing documents written in Hypertext Markup ;;;
;;; Language (HTML).  These documents are typicallly part of the World Wide ;;;
;;; Web (WWW), a project to create a global information net in hypertext    ;;;
;;; format.				                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)	    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (not noninteractive) (progn
			   (or (featurep 'efs)
			       (featurep 'efs-auto)
			       (require 'ange-ftp))))

(if (not (fboundp 'gopher-dispatch-object))
    (autoload 'gopher-dispatch-object "gopher" "Get a gopher doc." t))
(if (not (fboundp 'html-mode))
    (autoload 'html-mode "html-mode" "Edit html documents." t))

(require 'w3-vars)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions that might not exist in old versions of emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(or (fboundp 'add-hook)
    (defun add-hook (hook-var function &optional at-end)
      "Add a function to a hook.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to add.
Third (optional) argument AT-END means to add the function at the end
 of the hook list instead of the beginning.  If the function is already
 present, this has no effect.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
 value of HOOK-VAR."
      (if (not (boundp hook-var)) (set hook-var nil))
      (let ((old (symbol-value hook-var)))
	(if (or (not (listp old)) (eq (car old) 'lambda))
	    (setq old (list old)))
	(if (w3-member function old)
	    nil
	  (set hook-var
	       (if at-end
		   (append old (list function)) ; don't nconc
		 (cons function old)))))))

(or (fboundp 'setenv)
    (defun setenv (variable &optional value)
      "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.  
This function works by modifying `process-environment'."
      (interactive "sSet environment variable: \nsSet %s to value: ")
      (if (string-match "=" variable)
	  (error "Environment variable name `%s' contains `='" variable)
	(let ((pattern (concat "\\`" (regexp-quote (concat variable "="))))
	      (case-fold-search nil)
	      (scan process-environment))
	  (while scan
	    (cond
	     ((string-match pattern (car scan))
	      (if (eq nil value)
		  (setq process-environment
			(delq (car scan) process-environment))
		(setcar scan (concat variable "=" value)))
	      (setq scan nil))
	     ((null (setq scan (cdr scan)))
	      (setq process-environment
		    (cons (concat variable "=" value) 
			  process-environment)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various nntp-related macros that are useful from gnus.el, but I don't
;;; want to have to (require 'gnus) just for them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro nntp-header-number (header)
  "Return article number in HEADER."
  (` (aref (, header) 0)))

(defmacro nntp-header-subject (header)
  "Return subject string in HEADER."
  (` (aref (, header) 1)))

(defmacro nntp-header-from (header)
  "Return author string in HEADER."
  (` (aref (, header) 2)))

(defmacro nntp-header-xref (header)
  "Return xref string in HEADER."
  (` (aref (, header) 3)))

(defmacro nntp-header-lines (header)
  "Return lines in HEADER."
  (` (aref (, header) 4)))

(defmacro nntp-header-date (header)
  "Return date in HEADER."
  (` (aref (, header) 5)))

(defmacro nntp-header-id (header)
  "Return Id in HEADER."
  (` (aref (, header) 6)))

(defmacro nntp-header-references (header)
  "Return references in HEADER."
  (` (aref (, header) 7)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Access authorization functions for the w3 browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base 64 encoding functions
;;; This code was converted to lisp code by me from the C code in
;;; ftp://cs.utk.edu/pub/MIME/b64encode.c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-b64-encoding
 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
 "The string to use to encode with base 64.")

(defun b0 (x) (aref w3-b64-encoding (logand (lsh x -18) 63)))
(defun b1 (x) (aref w3-b64-encoding (logand (lsh x -12) 63)))
(defun b2 (x) (aref w3-b64-encoding (logand (lsh x -6) 63)))
(defun b3 (x) (aref w3-b64-encoding (logand x 63)))

(defun b64-encode (str)
  "Do base64 encoding on string STR and return the encoded string.
This code was converted to lisp code by me from the C code in
ftp://cs.utk.edu/pub/MIME/b64encode.c.  Returns a string that is
broken into 76 byte lines."
  (let ((x (b64-encode-internal str))
	(y ""))
    (while (> (length x) 76)
      (setq y (concat y (substring x 0 76) "\n")
	    x (substring x 76 nil)))
    (setq y (concat y x))
    y))  

(defun b64-encode-internal (str)
  "Do base64 encoding on string STR and return the encoded string.
This code was converted to lisp code by me from the C code in
ftp://cs.utk.edu/pub/MIME/b64encode.c.  Returns the entire string,
not broken up into 76 byte lines."
  (let (
	(word 0)			; The word to translate
	w1 w2 w3
	)
    (cond
     ((> (length str) 3)
      (concat
       (b64-encode-internal (substring str 0 3))
       (b64-encode-internal (substring str 3 nil))))
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
;;; UUencoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-uuencode-buffer (&optional buff)
  "UUencode buffer BUFF, with a default of the current buffer."
  (setq buff (or buff (current-buffer)))
  (save-excursion
    (set-buffer buff)
    (w3-lazy-message "UUencoding...")
    (call-process-region (point-min) (point-max)
			 w3-uuencode-program t t nil "w3-temp-file")
    (w3-lazy-message "UUencoding... done.")))


(defun w3-uudecode-buffer (&optional buff)
  "UUdecode buffer BUFF, with a default of the current buffer."
  (setq buff (or buff (current-buffer)))
  (let ((newname (w3-generate-unique-filename)))
    (save-excursion
      (set-buffer buff)
      (goto-char (point-min))
      (re-search-forward "^begin [0-9][0-9][0-9] \\(.*\\)$" nil t)
      (replace-match (concat "begin 600 " newname))
      (w3-lazy-message "UUdecoding...")
      (call-process-region (point-min) (point-max) w3-uudecode-program)
      (w3-lazy-message "UUdecoding...")
      (erase-buffer)
      (insert-file-contents newname)
      (w3-lazy-message "UUdecoding... done.")
      (condition-case ()
	  (delete-file newname)
	(error nil)))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decoding PGP/PEM responses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-decode-pgp/pem (arg)
  "Decode a pgp/pem response from an HTTP/1.0 server.
This expects the decoded message to contain all the necessary HTTP/1.0 headers
to correctly act on the decoded message (new content-type, etc)."
  (mc-decrypt-message)
  (w3-parse-mime-headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The authorization code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-basic-auth-storage nil
  "Where usernames and passwords are stored.  Its value is an assoc list of
assoc lists.  The first assoc list is keyed by the server name.  The cdr of
this is an assoc list based on the 'directory' specified by the url we are
looking up.")

(defun w3-public-key-exists (entity scheme)
  "Return t iff a key for ENTITY exists using public key system SCHEME.
ENTITY is the username/hostname combination we are checking for.
SCHEME is a symbol representing what public key encryption program to use.
       Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
       recognized."
  (let (retval)
    (save-excursion
      (cond
       ((eq 'pgp scheme)			; PGP encryption
	(set-buffer (get-buffer-create " *keytmp*"))
	(erase-buffer)
	(call-process mc-pgp-path nil t nil "+batchmode" "-kxaf" entity)
	(goto-char (point-min))
	(setq retval (search-forward mc-pgp-key-begin-line nil t)))
       ((eq 'pem scheme)			; PEM encryption
	(set-buffer (find-file-noselect mc-ripem-pubkeyfile))
	(goto-char (point-min))
	(setq retval (search-forward entity nil t)))
       (t
	(message "Bad value for SCHEME in w3-public-key-exists %S" scheme)))
      (kill-buffer (current-buffer)))
    retval))

(defun w3-get-server-keys (entity &optional scheme)
  "Make sure the key for ENTITY exists using SCHEME.
ENTITY is the username/hostname combination to get the info for.  
       This should be a string you could pass to 'finger'.
SCHEME is a symbol representing what public key encryption program to use.
       Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
       recognized."
  (or scheme (setq scheme mc-default-scheme))
  (save-excursion
    (cond
     ((w3-public-key-exists entity scheme) nil)
     (t
      (string-match "\\([^@]+\\)@\\(.*\\)" entity)
      (let ((w3-working-buffer " *w3-get-keys*"))
	(w3-retrieve (format "gopher://%s:79/0%s/w" (w3-match entity 1)
			     (w3-match entity 2)))
	(mc-snarf-keys)
	(kill-buffer w3-working-buffer))))))
   
(defun w3-fetch-with-pgp (url recipient type)
  "Retrieve a document with public-key authentication.
      URL is the url to request from the server.
RECIPIENT is the server's entity name (usually webmaster@host)
     TYPE is a symbol representing what public key encryption program to use.
          Currently only 'pgp (Pretty Good Privacy) and 'pem (RIPEM) are
          recognized."
  (or noninteractive (require 'mailcrypt))
  (let ((request (w3-create-mime-request url (w3-view-this-url t)))
	(w3-request-data nil)
	(w3-request-extra-headers nil))
    (save-excursion
      (w3-get-server-keys recipient type)
      (set-buffer (get-buffer-create " *w3-encryption*"))
      (erase-buffer)
      (insert "\n\n" mail-header-separator "\n" request)
      (mc-encrypt-message recipient type)
      (goto-char (point-min))
      (if (re-search-forward (concat "\n" mail-header-separator "\n") nil t)
	  (delete-region (point-min) (point)))
      (setq w3-request-data (buffer-string)
	    w3-request-extra-headers
	    (list (cons "Authorized" (format "%s entity=\"%s\""
					     (cond
					      ((eq type 'pgp) "PGP")
					      ((eq type 'pem) "PEM"))
					     w3-pgp/pem-entity))
		  (cons "Content-type" (format "application/x-www-%s-reply"
					       (cond
						((eq type 'pgp) "pgp")
						((eq type 'pem) "pem")))))))
    (kill-buffer " *w3-encryption*")
    (w3-retrieve (w3-parse-relative-link "/"))))
     
(defun w3-basic-auth (url &optional prompt overwrite)
  "Get the username/password for the specified URL.
If optional argument PROMPT is non-nil, ask for the username/password
to use for the url and its descendants.  If optional third argument
OVERWRITE is non-nil, overwrite the old username/password pair if it
is found in the assoc list."
  (let* ((href (w3-grok-http-href url))
	 (server (concat (nth 0 href) ":" (nth 1 href)))
	 (path (nth 2 href))
	 user pass byserv retval)
    (setq byserv (cdr-safe (assoc server w3-basic-auth-storage)))
    (if (not byserv)			; Server not found
	(if prompt
	    (progn
	      (setq user (read-string "Username: " (user-real-login-name))
		    pass (funcall w3-passwd-entry-func "Password: "))
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
	      (cond
	       ((string-match (concat (regexp-quote (car (car byserv)))
				      "/*[^/]+") path)
		(setq retval (cdr (car byserv))))
	       ((string-match (concat (regexp-quote
				       (w3-basepath (car (car byserv))))
				      "/*[^/]+") path)
		(setq retval (cdr (car byserv)))))
	      (setq byserv (cdr byserv))))
	(if (or (and (not retval) prompt) overwrite)
	    (progn
	      (setq user (read-string "Username: " (user-real-login-name))
		    pass (funcall w3-passwd-entry-func "Password: ")
		    retval (b64-encode (format "%s:%s" user pass))
		    byserv (assoc server w3-basic-auth-storage))
	      (setcdr byserv
		      (cons (cons path retval) (cdr byserv)))))))
    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMS processing for html+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(or (boundp 'MULE) (fset 'string-width 'length))

(defun w3-truncate-string (str len &optional pad)
  "Truncate string STR so that string-width of STR is not greater than LEN.
If width of the truncated string is less than LEN, and if a character PAD is
defined, add padding end of it."
  (if (boundp 'MULE)
      (let ((cl (string-to-char-list str)) (n 0) (sw 0))
	(if (<= (string-width str) len) str
	  (while (<= (setq sw (+ (char-width (nth n cl)) sw)) len)
	    (setq n (1+ n)))
	  (string-match (make-string n ?.) str)
	  (setq str (substring str 0 (match-end 0))))
	(if pad (concat str (make-string (- len (string-width str)) pad)) str))
    (concat (if (> (length str) len) (substring str 0 len) str)
	    (if (or (null pad) (> (length str) len))
		""
	      (make-string (- len (length str)) pad)))))

(defun w3-split (str del)
  "Split the string STR, with DEL (a regular expression) as the delimiter.
Returns an assoc list that you can use with completing-read."
  (let (x y)
    (while (string-match del str)
      (setq y (substring str 0 (match-beginning 0))
	    str (substring str (match-end 0) nil))
      (if (not (string-match "^[ \\\t]+$" y))
	  (setq x (cons (list y y) x))))
    (if (not (equal str ""))
	(setq x (cons (list str str) x)))
    x))

(defun w3-handle-form (num)
  "Parse a form, expecting the region to be narrowed between the <FORM>
and </FORM> tags."
  (goto-char (point-min))
  (let ((args (if (re-search-forward "<FORM\\([^>]*\\)>" nil t)
		  (w3-parse-args (match-beginning 1) (match-end 1))
		""))
	action method tmp
	st nd input type name default value checked size maxlength prompt
	options formatfun)
    (setq method (or (cdr (assoc "method" args)) "GET")
	  action (or (cdr (assoc "action" args)) (w3-view-url t))
	  action (cons method action))
    (while (re-search-forward "<INPUT" nil t)
      (setq st (cons (match-beginning 0) (match-end 0))
	    nd (progn
		 (goto-char (car st))
		 (if (re-search-forward ">" nil t)
		     (cons (match-end 0) (match-beginning 0))
		   (progn (end-of-line) (cons (point) (point)))))
	    input (w3-parse-args (cdr st) (cdr nd)))
      (delete-region (car st) (car nd))
      (setq type (upcase (or (cdr (assoc "type" input)) "text"))
	    name (or (cdr (assoc "name" input)) type)
	    value (or (cdr (assoc "value" input)) "")
	    size (string-to-int (or (cdr (assoc "size" input)) "20"))
	    maxlength (string-to-int
		       (or (cdr (assoc "maxlength" input)) "10000"))
	    default value
	    checked (assoc "checked" input))
      (if (null tmp)
	  nil
	(setq action tmp
	      tmp nil))
      (if (and (string= type "SUBMIT")
	       (assoc "action" input))
	  (setq tmp action
		action (cons method (cdr (assoc "src" input)))))
      (if (or (equal type "CHECKBOX")
	      (equal type "RADIO"))
	  (setq default checked))
      (if (and (equal type "CHECKBOX")
	       (equal "" value))
	  (setq value "on"))
      (cond
       ((equal type "HIDDEN")
	(setq w3-hidden-forms (cons (list 'w3form action type name default
					  value checked size maxlength num
					  options) w3-hidden-forms)))
       (t 
	(setq formatfun (intern (concat "w3-form-format-" (downcase type))))
	(if (not (fboundp formatfun))
	    (setq formatfun 'w3-form-format-unknown))
	(setq prompt (funcall formatfun value size checked))
	(goto-char (car st))
	(w3-insert prompt)
	(w3-add-zone (car st) (point) w3-node-style
		     (list 'w3form
			   action type name default value
			   checked size maxlength num options) t)
	(w3-insert (if (w3-member type '("CHECKBOX" "RADIO")) "" " ")))))))

(defun w3-form-format-int (&rest args)
  "Format an integer entry field"
  (w3-truncate-string (or (nth 0 args) "") (nth 1 args) ?_))

(fset 'w3-form-format-url 'w3-form-format-int)
(fset 'w3-form-format-float 'w3-form-format-int)
(fset 'w3-form-format-date 'w3-form-format-int)

(defun w3-form-format-reset (&rest args)
  "Format a reset button"
  (if (string= (nth 0 args) "") "Reset fields" (nth 0 args)))

(defun w3-form-format-password (&rest args)
  "Format a password entry field"
  (let ((value (or (nth 0 args) ""))
	(size (nth 1 args)))
    (concat (if (>= (length value) size) (make-string size ?*)
	      (make-string (length value) ?*))
	    (if (>= (length value) size) ""
	      (make-string (- size (length value)) ?.)))))

(defun w3-form-format-checkbox (&rest args)
  "Format a checkbox entry"
  (let ((checked (nth 2 args)))
    (format "[%s]" (if checked "X" " "))))

(fset 'w3-form-format-radio 'w3-form-format-checkbox)

(defun w3-form-format-submit (&rest args)
  "Format a form submit button"
  (if (string= (nth 0 args) "") "Submit this form" (nth 0 args)))

(defun w3-form-format-text (&rest args)
  "Format a text field"
  (w3-truncate-string (nth 0 args) (nth 1 args) ?_))

(defun w3-form-format-textarea (&rest args)
  "Format a multiline text box"
  "Multiline text entry")

(fset 'w3-form-format- 'w3-form-format-text)
(fset 'w3-form-format-unknown 'w3-form-format-text)

(defun w3-handle-textareas (num action)
  "Handle <SELECT> tags in a form"
  (let (
	(type "TEXTAREA")
	name
	default
	value
	checked
	size
	(maxlength 100)
	options
	st
	nd
	input)
    (goto-char (point-min))
    (while (re-search-forward "<TEXTAREA\\([^>]*\\)>\\\n*" nil t)
      (setq st (match-beginning 0)
	    input (prog1
		      (w3-parse-args (match-beginning 1) (match-end 1))
		    (replace-match ""))
	    nd (if (re-search-forward "\\n*</TEXTAREA[^>]*>\\\n*" nil t)
		   (progn
		     (replace-match "")
		     (match-beginning 0))
		 (progn
		   (end-of-line)
		   (point)))
	    value (buffer-substring st nd)
	    options value
	    default value)
      (setq name (or (cdr (assoc "name" input)) ""))
      (delete-region st nd)
      (w3-insert "Multiline Text Entry Area")
      (w3-add-zone st (point) w3-node-style
		   (list 'w3form
			 action type name default value checked size
			 maxlength num options) t))))

(defun w3-handle-selections (num action)
  "Handle <SELECT> tags in a form"
  (let (
	(type "OPTION")
	name
	default
	value
	checked
	size
	(maxlength 100)
	options
	parm
	st
	nd
	input
	mult				; Multiple input?
	(longest 0)			; Longest selection?
	sel)
    (goto-char (point-min))
    (while (re-search-forward "<SELECT\\([^>]*\\)>\\\n*" nil t)
      (setq st (match-beginning 0)
	    input (prog1
		      (w3-parse-args (match-beginning 1) (match-end 1))
		    (replace-match ""))
	    options nil
	    mult nil
	    value nil
	    default nil
	    nd (if (re-search-forward "\\n*</SELECT[^>]*>\\\n*" nil t)
		   (progn
		     (replace-match "")
		     (match-beginning 0))
		 (progn
		   (end-of-line)
		   (point))))
      (goto-char st)
      (while (re-search-forward "[\\\n ]*<OPTION\\([^>]*\\)> *\\([^<]*\\)"
				nd t)
	(setq parm (w3-parse-args (match-beginning 1) (match-end 1))
	      sel (w3-eat-trailing-space
		   (buffer-substring (match-beginning 2) (match-end 2)))
	      options (cons (cons sel sel) options))
	(if (> (string-width sel) longest) (setq longest (string-width sel)))
	(if (assoc "selected" parm) (setq default sel)))
      (setq longest (+ 5 longest))
      (delete-region st nd)
      (setq name (or (cdr (assoc "name" input)) "")
	    size (string-to-int (or (cdr (assoc "size" input)) "20"))
	    mult (or (assoc "several" input)
		     (assoc "multiple" input)))
      (cond
       (mult
	(goto-char st)
	(w3-insert "<UL>\n")
	(mapcar
	 (function
	  (lambda (x)
	    (w3-insert
	     (format 
	      "<LI><INPUT TYPE=\"CHECKBOX\" NAME=\"%s\" VALUE=\"%s\" %s>%s\n"
	      name (car x) (if (equal default (car x)) "CHECKED" "")
	      (car x))))) options)
	(w3-insert "</UL>\n"))
       (t
	(if (not default)
	    (setq value (car (nth (1- (length options)) options)))
	  (setq value default))
	(setq default value)
	(goto-char st)
	(setq value (w3-truncate-string value maxlength))
	(w3-insert (w3-form-format-text value size nil))
	(w3-add-zone st (point) w3-node-style
		     (list 'w3form
			   action type name default value checked size
			   maxlength num options) t))))))

(defun w3-handle-forms ()
  "Take care of parsing an entire buffer for <FORM> tags."
  (set-buffer w3-working-buffer)
  (let ((num 1)
	x y z)
    (goto-char (point-min))
    (while (re-search-forward "<FORM\\([^>]*\\)>" nil t)
      (setq y (match-beginning 0)
	    x (w3-parse-args (match-beginning 1) (match-end 1)))
      (narrow-to-region y
			(if (re-search-forward "</FORM>" nil t) (match-end 0)
			  (point-max)))
      (setq y (or (cdr (assoc "method" x)) "GET")
	    z (or (cdr (assoc "action" x)) (w3-view-url t)))
      (w3-handle-selections num (cons y z))
      (w3-handle-textareas num (cons y z))
      (w3-handle-form num)
      (setq num (1+ num))
      (w3-replace-regexp "</*FORM[^>]*>" "<p>")
      (widen))))

(defun w3-do-text-entry (formobj zone)
  "Read in a multiline text entry area."
  (let ((data (list formobj zone (current-buffer)))
	(buff (get-buffer-create (format "%d:%s" (nth 9 formobj)
					 (nth 3 formobj)))))
    (switch-to-buffer-other-window buff)
    (indented-text-mode)
    (erase-buffer)
    (w3-insert (nth 5 formobj))
    (setq w3-current-last-buffer data)
    (message "Press C-c C-c when finished with text entry.")
    (local-set-key "\C-c\C-c" 'w3-finish-text-entry)))

(defun w3-finish-text-entry ()
  "Finish a text entry area"
  (interactive)
  (if w3-current-last-buffer
      (let* ((formobj (nth 0 w3-current-last-buffer))
	     (zone (nth 1 w3-current-last-buffer))
	     (buff (nth 2 w3-current-last-buffer))
	     (actn (nth 1 formobj))
	     (type (nth 2 formobj))
	     (name (nth 3 formobj))
	     (deft (nth 4 formobj))
	     (valu (buffer-string))
	     (chkd (nth 6 formobj))
	     (size (nth 7 formobj))
	     (maxl (nth 8 formobj))
	     (ident (nth 9 formobj))
	     (options (nth 10 formobj))
	     (st nil)
	     (nd nil))
	(local-set-key "\C-c\C-c" 'undefined)
	(kill-buffer (current-buffer))
	(delete-window)
	(if (not (and buff (bufferp buff) (buffer-name buff)))
	    (message "Could not find the form buffer for this text!")
	  (switch-to-buffer buff)
	  (if buffer-read-only (toggle-read-only))
	  (setq st (w3-zone-start zone)
		nd (w3-zone-end zone))
	  (w3-delete-zone zone)
	  (w3-add-zone st nd w3-node-style
		       (list 'w3form actn type name deft valu chkd
			     size maxl ident options) t)))
    (if (not buffer-read-only) (toggle-read-only))
    nil))

(defun w3-do-form-entry (formobj zone)
  "Read in a form entry field.
FORMOBJ is the data returned by w3-zone-at, and contains all the information
        about the entry area (size, type, value, etc)
   ZONE is the actual zone object.  This should be able to be passed to
        w3-delete-zone."
  (let* ((actn (nth 1 formobj))
	 (type (nth 2 formobj))
	 (name (nth 3 formobj))
	 (deft (nth 4 formobj))
	 (valu (nth 5 formobj))
	 (chkd (nth 6 formobj))
	 (size (nth 7 formobj))
	 (maxl (nth 8 formobj))
	 (ident (nth 9 formobj))
	 (options (nth 10 formobj))
	 (st (w3-zone-start zone))
	 (nd (w3-zone-end zone))
	 (submit-it nil)
	 (formatfun (intern (concat "w3-form-format-" (downcase type)))))
    (if (not (equal "SUBMIT" type))
	(progn
	  (if (equal "TEXTAREA" type)
	      (progn
		(if (not buffer-read-only) (toggle-read-only))
		(w3-do-text-entry formobj zone)))
	  (save-excursion
	    (if (not (fboundp formatfun))
		(setq formatfun 'w3-form-format-unknown))
	    (if buffer-read-only (toggle-read-only))
	    (cond
	     ((equal "CHECKBOX" type) (setq chkd (not chkd)))
	     ((equal "RADIO" type) nil)
	     ((equal "TEXTAREA" type) nil)
	     ((equal "RESET" type) (w3-revert-form ident))
	     (t (setq valu
		      (w3-read-correct-format type name options ident valu))))
	    (cond
	     ((equal "RESET" type) nil)
	     ((equal "RADIO" type) (w3-set-radio-button zone))
	     ((equal "TEXTAREA" type) nil)
	     (t
	      (w3-delete-zone zone)
	      (delete-region st nd)
	      (goto-char st)
	      (w3-insert (funcall formatfun valu size chkd))
	      (w3-add-zone st (point) w3-node-style
			   (list 'w3form actn type name deft valu chkd
				 size maxl ident options) t)
	      (if (not buffer-read-only) (toggle-read-only))
	      (if w3-running-FSF19 (setq w3-zones-list (w3-only-links)))
	      (if (boundp 'MULE)
		  (w3-mule-attribute-zones w3-zones-list w3-mule-attribute))
	      ))
	    (cond
	     ((string-match "^isindex$" name) (setq submit-it 'isindex))
	     ((string-match "^internal-gopher$" name) (setq submit-it 'gopher))
	     ((string-match "^internal-wais$" name) (setq submit-it 'wais))
	     ((equal (length (w3-zones-matching ident)) 1)
	      (setq submit-it t)))))
      (w3-submit-form ident nil actn))
    (if submit-it (w3-submit-form ident submit-it actn))))

(defun w3-zones-matching (actn &optional raw)
  "Return a list of data entry zones in form number ACTN
With optional second argument raw, don't grab the data of the zone, but
return the actual zone."
  (let* ((big (w3-all-zones))
	 (data nil)
	 (result nil))
    (while big
      (setq data (w3-zone-data (car big)))
      (if (and (eq (nth 0 data) 'w3form) (equal (nth 9 data) actn))
	  (setq result (cons (if raw (car big) data) result)))
      (setq big (cdr big)))
    (if raw
	nil
      (setq big w3-hidden-forms)
      (while big
	(setq data (car big))
	(if (and (eq (nth 0 data) 'w3form) (equal (nth 9 data) actn))
	    (setq result (cons data result)))
	(setq big (cdr big))))
    result))

(defun w3-revert-form (actn)
  "Revert all values for form ACTN to their defaults"
  (save-excursion
    (let* ((zones (w3-zones-matching actn t))
	   actn data type name deft valu chkd size maxl idnt strt end cur
	   options formatfun
	   )
      (if buffer-read-only (toggle-read-only))
      (mapcar
       (function
	(lambda (cur)
	  (setq data (w3-zone-data cur)
		actn (nth 1 data)
		type (nth 2 data)
		name (nth 3 data)
		deft (nth 4 data)
		valu (nth 5 data)
		chkd (nth 6 data)
		size (nth 7 data)
		maxl (nth 8 data)
		idnt (nth 9 data)
		options (nth 10 data)
		strt (w3-zone-start cur)
		end  (w3-zone-end cur)
		formatfun (intern (concat "w3-form-format-" (downcase type))))
	  (if (not (fboundp formatfun))
	      (setq formatfun 'w3-form-format-unknown))
	  (cond
	   ((or (w3-member type '("SUBMIT" "RESET"))) nil)
	   (t
	    (if (w3-member type '("RADIO" "CHECKBOX"))
		(setq chkd deft)
	      (setq valu deft))
	    (if w3-running-FSF19 (goto-char strt)
	      (w3-delete-zone cur))
	    (delete-region strt end)
	    (goto-char strt)
	    (w3-insert (funcall formatfun valu size chkd))
	    (w3-add-zone strt (point) w3-node-style
			 (list 'w3form actn type name deft valu chkd
			       size maxl idnt options) t))))
	(if (not buffer-read-only) (toggle-read-only))) zones))
    (if w3-running-FSF19
	(setq w3-zones-list (w3-only-links)))
    (if (boundp 'MULE)
	(w3-mule-attribute-zones w3-zones-list w3-mule-attribute))
    ))

(defun w3-form-encode-multipart (formobjs &optional sep)
  "Create a multipart form submission.
Returns a cons of two strings.  Car is the separator used.
cdr is the body of the MIME message."
  (let ((separator (or sep "--some-separator")))
    (cons separator
	  (mapconcat
	   (function
	    (lambda (formobj)
	      (if (or (and (w3-member (nth 2 formobj) '("CHECKBOX" "RADIO"))
			   (not (nth 6 formobj)))
		      (w3-member (nth 2 formobj) '("RESET")))
		  "" (concat separator "\nContent-id: " (nth 3 formobj) "\n\n"
			     (nth 5 formobj))))) formobjs "\n"))))

(defun w3-form-encode (result &optional isindex-query)
  "Create a string suitably enocoded for a URL request."
  (let ((query ""))
    (cond
     ((eq isindex-query 'gopher)	; Gopher searching by hypertext
      (setq query (concat "\t" (nth 5 (car result)))))
     ((eq isindex-query 'isindex)	; Isindex handling by hypertext
      (while result
	(if (equal (downcase (nth 3 (car result))) "isindex")
	    (setq query (w3-hexify-string (nth 5 (car result)))
		  result nil))
	(setq result (cdr result))))
     (t					; Normal submission of form
      (while result			; This is a little convoluted, but
					; gets only checkboxes that are set
					; and ignores submit & reset buttons
	(if (and (not
		  (and (w3-member (nth 2 (car result)) '("CHECKBOX" "RADIO"))
		       (not (nth 6 (car result)))))
		 (not (w3-member (nth 2 (car result)) '("RESET" "SUBMIT"))))
	    (setq query (concat (nth 3 (car result)) "="
				(w3-hexify-string (nth 5 (car result)))
				(if (not (equal query "")) "&" "") query)))
	(setq result (cdr result)))))
    query))

(defun w3-form-encode-ask-block (result)
  "Submit a gopher ask block to the server."
  (let ((query ""))
    ;;; This is different than the w3-form-encode function, because
    ;;; gopher+ will expect all the checkboxes/etc, even if they are
    ;;; not turned on.  Should still ignore RADIO boxes that are not
    ;;; active though.
  (while result	      
    (if (and (not (and (string= (nth 2 (car result)) "RADIO")
		       (not (nth 6 (car result)))))
	     (not (w3-member (nth 2 (car result)) '("SUBMIT" "RESET"))))
	(setq query (format "%s\r\n%s" query (nth 5 (car result)))))
    (setq result (cdr result)))
  (concat query "\r\n.\r\n")))

(defun w3-submit-form (ident isindex &optional actn)
  "Submit form entry fields matching ACTN as their action identifier."
  (let* ((result (w3-zones-matching ident))
	 (query (w3-form-encode result isindex))
	 (themeth (upcase (or (car actn) "GET")))
	 (theurl (cdr actn)))
    (if (string-match "\\([^\\?]*\\)\\?" theurl)
	(setq theurl (w3-match theurl 1)))
    (cond
     ((eq isindex 'gopher) (w3-fetch (concat theurl query)))
     ((eq isindex 'wais)
      (w3-perform-wais-query w3-current-server w3-current-port
			     w3-current-file query)
      (w3-sentinel nil nil))
     ((string= "GOPHER-ASK" themeth)
      (setq query (w3-form-encode-ask-block result))
      (w3-fetch (concat theurl (w3-hexify-string (concat "\t+\t1\n+-1\r\n"
							 query)))))
     ((string= "POST" themeth)
      (let ((w3-request-method themeth)
	    (w3-request-data query)
	    (w3-request-extra-headers
	     (list (cons "Content-type"
			 "application/x-www-form-urlencoded"))))
	(w3-maybe-relative theurl)))
     ((string= "GET" themeth)
      (w3-maybe-relative (format "%s?%s" theurl query)))
     (t (message "Unknown submit method: %s" themeth)))))

(defun w3-matching-radios (ext)
  "Return a list of all zones containing radio buttons with the same name
as that in EXT."
  (let* ((big (w3-all-zones))
	 (idnt (nth 9 (w3-zone-data ext)))
	 (name (nth 3 (w3-zone-data ext)))
	 data cur result)
    (mapcar
     (function
      (lambda (cur)
	(setq data (w3-zone-data cur))
	(if (and
	     (eq (nth 0 data) 'w3form)
	     (equal (nth 9 data) idnt)
	     (equal (nth 3 data) name))
	    (setq result (cons cur result))))) big)
    result))
      
(defun w3-set-radio-button (ext)
  "Set the radio button at EXT to be on.  Will automatically
toggle other radio butons with the same name to be off."
  (save-excursion
    (let* ((result (w3-matching-radios ext))
	   (idnt (nth 9 (w3-zone-data ext)))
	   (name (nth 3 (w3-zone-data ext)))
	   actn type deft valu chkd size maxl strt end data options)
      (while result
	(setq data (w3-zone-data (car result))
	      actn (nth 1 data)
	      type (nth 2 data)
	      name (nth 3 data)
	      deft (nth 4 data)
	      valu (nth 5 data)
	      chkd (nth 6 data)
	      size (nth 7 data)
	      maxl (nth 8 data)
	      idnt (nth 9 data)
	      options (nth 10 data)
	      strt (w3-zone-start (car result))
	      end (w3-zone-end (car result)))
	(cond
	 ((and chkd (not (w3-zone-eq
			  ext (car result)))) ; Not supposed to be chkd
	  (w3-delete-zone (car result))	      ; but is.
	  (goto-char strt)
	  (delete-region strt end)
	  (setq chkd nil)
	  (w3-insert (funcall 'w3-form-format-radio valu size chkd))
	  (w3-add-zone strt (point) w3-node-style
		       (list 'w3form actn type name deft valu chkd size maxl
			     idnt options) t))
	 ((and (not chkd) (w3-zone-eq
			   ext (car result))) ; Supposed to be chkd
	  (w3-delete-zone (car result))       ; but isn't.
	  (goto-char strt)
	  (delete-region strt end)
	  (setq chkd t)
	  (w3-insert (funcall 'w3-form-format-radio valu size chkd))
	  (w3-add-zone strt (point) w3-node-style
		       (list 'w3form actn type name deft valu chkd size maxl
			     idnt options) t))
	 (t nil)) ; not supposed to be checked, and isn't
	(setq result (cdr result))))
    (if (not buffer-read-only) (toggle-read-only))
    (if w3-running-FSF19 (setq w3-zones-list (w3-only-links)))
    (if (boundp 'MULE)
	(w3-mule-attribute-zones w3-zones-list w3-mule-attribute))
    ))
			

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type checking for FORMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Date checking, taken from edb.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst weekday-alist
 '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3)
   ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6)
   ("Tues" . 2) ("Thurs" . 4)
   ("Sun" . 0) ("Mon" . 1) ("Tue" . 2) ("Wed" . 3)
   ("Thu" . 4) ("Fri" . 5) ("Sat" . 6)))

(defconst full-monthname-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)))


(defconst monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defconst monthname-alist
  (append monthabbrev-alist
	  full-monthname-alist
	  '(("Sept" . 9))))

(defconst monthname-regexp
  (concat "\\("
	  (mapconcat (function car)
		     monthname-alist
		     "\\|")
	  "\\)\\.?"))

(defconst weekday-regexp
  (concat "\\("
	  (mapconcat (function car)
		     weekday-alist
		     "\\|")
	  "\\)\\.?"))

(defconst monthnumber-regexp "\\(0?[1-9]\\|1[0-2]\\)")
(defconst monthnumber-regexp-two-char "\\(0[1-9]\\|1[0-2]\\)")

(defconst monthday-regexp "\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)")
(defconst monthday-regexp-two-char "\\([0-2][0-9]\\|3[01]\\)")

(defconst full-year-regexp "[0-2][0-9][0-9][0-9]")
(defconst short-year-regexp "[0-9][0-9]")

(defconst year-regexp (concat "\\(" full-year-regexp
			      "\\|" short-year-regexp "\\)"))

(defconst elt-separator-regexp "[ -.,/']+")

(defconst date-regexps
  (list
   ;; MMDDYY
   (cons (concat monthname-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 nil 1 2))
   (cons (concat monthnumber-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 1 nil 2))
   ;; DDMMYY
   (cons (concat monthday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 nil 2 1))
   (cons (concat "\\("
		 monthday-regexp
		 elt-separator-regexp
		 "\\)?"
		 monthname-regexp
		 elt-separator-regexp
		 year-regexp)
	 '(4 nil 3 2))
   (cons (concat monthday-regexp
		 elt-separator-regexp
		 monthnumber-regexp
		 elt-separator-regexp
		 "\\(" full-year-regexp "\\)")
	 '(3 2 nil 1))
   ;; YYMMDD
   ;; Using year-regexp instead of full-year-regexp is ambiguous (consider
   ;; 11-11-11), but we already tried MMDDYY and it failed.
   (cons (concat year-regexp
		 elt-separator-regexp
		 monthname-regexp
		 elt-separator-regexp
		 monthday-regexp)
	 '(1 nil 2 3))
   (cons (concat year-regexp
		 elt-separator-regexp
		 monthnumber-regexp
		 elt-separator-regexp
		 monthday-regexp)
	 '(1 2 nil 3))
   ;; YYMMDD, no separators
   ;; This is ambiguous.
   (cons (concat year-regexp
		 monthnumber-regexp-two-char "?"
		 monthday-regexp-two-char "?")
	 '(1 2 nil 3))
   ;; WWMMDDYY
   (cons (concat weekday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(5 nil 2 3))
   ;; WWDDMMYY
   (cons (concat weekday-regexp
		 elt-separator-regexp
		 monthday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(5 nil 3 2))
   ;; ctime
   (cons (concat
	  weekday-regexp
	  " "
	  monthname-regexp
	  "  ?"
	  monthday-regexp
	  ;; time of day
	  " [0-9:]+ "
	  "\\(" full-year-regexp "\\)")
	 '(4 nil 2 3))
   )
  "Assoc list of regexps and match locators.
A match locator is a list of four numbers indicating which submatch of the
regexp contains the year, month number, month name, and day of the month.
The list elements may be nil if that information is not available.")

(defun w3-datep (date-string)
  "Parse DATE-STRING, and return a date object; err if the parse is invalid.
If DATE-STRING contains only whitespace, return a null date object.
If DATE-STRING is nil, use the result of `parse-date-default-function' instead."
  (let ((regexp-alist date-regexps)
	result)
    (if (zerop (length date-string))	;if empty string,
	(setq result t)			;empty date is kosher
      ;; regexp-alist is nulled if a match is found
      (progn
	(while regexp-alist
	  (if (string-match (concat "^" (car (car regexp-alist)) "$")
			    date-string)
	      (setq regexp-alist nil
		    result t)
	    ;; string-match failed
	    (setq regexp-alist (cdr regexp-alist))))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Integer checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-intp (str)
  "Integer checker"
  (string-match "^[0-9]+$" str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Floating point checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-floatp (str)
  "Floating point checker"
  (let (x y)
    (if (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)$" str)
	(progn
	  (setq x (substring str (match-beginning 1) (match-end 1))
		y (substring str (match-beginning 2) (match-end 2)))
	  (and (w3-intp x) (w3-intp y)))
      (w3-intp str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; URL Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-urlp (str)
  "URL checker..."
  (string-match w3-nonrelative-link str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Option list checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-optionp (val)
  "Option list checker"
  (if (null val)
      (progn
	(message "Please make a selection from the menu")
	nil)
    t))

(defun w3-textp (str) t)		; don't care whats in a text field
(fset 'w3-p 'w3-textp)			; for default of "" to be text
(fset 'w3-passwordp 'w3-textp)		; don't care whats in a paswd either
(fset 'w3-textareap 'w3-textp)		; try this - might work

(defun w3-read-correct-format (type name options num value)
  "Read in a FORMS entry with type TYPE, and do typechecking"
  (let ((func (read (format "w3-%sp" (downcase type))))
	(valu value) exitp)
    (while (not exitp)
      (cond
       ((or (equal "TEXT" type)
	    (equal "" type))
	(setq valu (read-string "Enter text: " valu)))
       ((or (equal "FLOAT" type)
	    (equal "INT" type))
	(setq valu (read-string "Enter numeric value: " valu)))
       ((equal "PASSWORD" type)
	(setq valu (funcall w3-passwd-entry-func "Enter password:" valu)))
       ((equal "OPTION" type)
	(if (or (not window-system)
		(not (fboundp 'w3-x-popup-menu)))
	    (setq valu (completing-read "Please choose: " options nil t valu))
	  (setq valu (w3-x-popup-menu
		      (if (and (boundp 'last-input-event)
			       (listp last-input-event))
			  last-input-event
			(list (list (current-column) 1)
			      (selected-window)))
		      (list "WWW"
			    (cons "Select An Item" options)))))
	(if (consp valu) (setq valu (car valu))))
       ((equal "DATE" type)
	(setq valu (read-string "Enter date: " valu)))
       ((equal "URL" type)
	(setq valu (read-string "Enter valid URL: " valu))))
      (if (not (fboundp func)) (setq func 'w3-textp))
      (if (funcall func valu)
	  (setq exitp t)
	(progn
	  (message "Wrong format for type %s, try again." (downcase type))
	  (sit-for 2))))
    valu))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gopher and Gopher+ support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-convert-ask-to-form (ask)
  "Convert a Gopher+ ASK block into a form.  Returns a string to be inserted
into a buffer to create the form."
  (let ((form "<FORM METHOD=\"GOPHER-ASK\"><UL PLAIN>")
	(type "")
	(x 0)
	(parms ""))
    (while (string-match "^\\([^:]+\\): +\\(.*\\)" ask)
      (setq parms (w3-match ask 2)
	    type (w3-strip-leading-spaces (downcase (w3-match ask 1)))
	    x (1+ x)
	    ask (substring ask (if (= (length ask) (match-end 0))
				   (match-end 0) (1+ (match-end 0))) nil))
      (cond
       ((string= "note" type) (setq form (concat form parms)))
       ((or (string= "ask" type)
	    (string= "askf" type)
	    (string= "choosef" type))
	(setq parms (mapcar 'car (nreverse (w3-split parms "\\\t")))
	      form (format "%s\n<LI>%s<INPUT name=\"%d\" DEFAULT=\"%s\">"
			   form (or (nth 0 parms) "Text:")
			   x (or (nth 1 parms) ""))))
       ((string= "askp" type)
	(setq parms (mapcar 'car (nreverse (w3-split parms "\\\t")))
	      form (format
		    "%s\n<LI>%s<INPUT name=\"%d\" TYPE=\"PASSWORD\" DEFAULT=\"%s\">"
		    form			   ; Earlier string
		    (or (nth 0 parms) "Password:") ; Prompt
		    x				   ; Name
		    (or (nth 1 parms) "") 	   ; Default value
		    )))
       ((string= "askl" type)
	(setq parms (mapcar 'car (nreverse (w3-split parms "\\\t")))
	      form (format "%s\n<LI>%s<TEXTAREA NAME=\"%d\">%s</TEXTAREA>"
			   form			 ; Earlier string
			   (or (nth 0 parms) "") ; Prompt string
			   x			 ; Name
			   (or (nth 1 parms) "") ; Default value
			   )))
       ((or (string= "select" type)
	    (string= "choose" type))
	(setq parms (mapcar 'car (nreverse (w3-split parms "\\\t")))
	      form (format "%s\n<LI>%s<SELECT NAME=\"%d\">" form (car parms) x)
	      parms (cdr parms))
	(if (null parms) (setq parms (list "Yes" "No")))
	(while parms
	  (setq form (concat form "<OPTION>" (car parms) "\n")
		parms (cdr parms)))
	(setq form (concat form "</SELECT>")))))
    (concat form "\n<LI><INPUT TYPE=\"SUBMIT\""
	    " VALUE=\"Submit Gopher+ Ask Block\"></UL></FORM>")))

(defun w3-grok-gopher-link (st nd)
  "Return a list of link attributes from a gopher string.  Order is:
title, type, selector string, server, port, gopher-plus?"
  (let (type selector server port gopher+)
    (save-excursion
      (mapcar (function
	       (lambda (var)
		 (goto-char st)
		 (skip-chars-forward "^\t\n" nd)
		 (set-variable var (buffer-substring st (point)))
		 (setq st (1+ (point)))))
	      '(type selector server port))
      (setq gopher+ (and (/= (1- st) nd) (buffer-substring st nd)))
      (list type (concat (substring type 0 1) selector) server port gopher+))))

(defun w3-format-gopher-link (gophobj)
  "Insert a gopher link as an <A> tag"
  (let ((title (nth 0 gophobj))
	(ref   (nth 1 gophobj))
	(type  (if (> (length (nth 0 gophobj)) 0)
		   (substring (nth 0 gophobj) 0 1) ""))
	(serv  (nth 2 gophobj))
	(port  (nth 3 gophobj))
	(plus  (nth 4 gophobj))
	(desc  nil))
    (if (and (equal type "")
	     (> (length title) 0))
	(setq type (substring title 0 1)))
    (setq title (and title (substring title 1 nil)))
    (setq desc (or (cdr (assoc type w3-gopher-labels)) "(UNK)"))
    (if (fboundp 'w3-insert-graphic)
	(setq desc (cdr (assoc type w3-gopher-icons))))
    (cond
     ((null ref) "")
     ((equal type "8")
      (format "<LI> %s <A HREF=\"telnet://%s:%s/%s\">%s</A>\n"
	      desc serv (concat port plus) ref title))
     ((equal type "T")
      (format "<LI> %s <A HREF=\"tn3270://%s:%s/%s\">%s</A>\n"
	      desc serv (concat port plus) ref title))
     (t (format "<LI> %s <A METHODS=%s HREF=\"gopher://%s:%s/%s\">%s</A>\n"
		desc type serv (concat port plus)
		(w3-hexify-string ref) title)))))

(defun w3-gopher-clean-text (&optional buffer)
  "Clean up text from gopher retrieval"
  (set-buffer (or buffer w3-working-buffer))
  (w3-replace-regexp "\r$" "")
  (w3-replace-regexp "^\\\.\\\n" "")
  (w3-replace-regexp "^\\\.\r*$\\\n*" ""))

(defun w3-parse-gopher (&optional buffer)
  "Parse out a gopher response"
  (save-excursion
    (w3-replace-regexp (regexp-quote "&") "&amp;")
    (w3-replace-regexp (regexp-quote ">") "&gt;")
    (w3-replace-regexp (regexp-quote "<") "&lt;")
    (w3-replace-regexp "\\\n*\\.\\\n*\\'" "\n")
    (goto-char (point-min))
    (while (looking-at "\n") (delete-char 1))
    (let ((objs nil))
      (while (not (eobp))
	(setq objs (cons
		    (w3-grok-gopher-link (save-excursion (beginning-of-line)
							 (point))
					 (save-excursion (end-of-line)
							 (point)))
		    objs))
	(forward-line 1))
      (setq objs (nreverse objs))
      (erase-buffer)
      (w3-insert "<TITLE>"
		 (cond
		  ((or (string= "" w3-current-file)
		       (string= "1/" w3-current-file)
		       (string= "1" w3-current-file)) "Gopher root")
		  ((string-match (format "^[%s]+/" w3-gopher-types)
				 w3-current-file)
		   (substring w3-current-file 2 nil))
		  (t w3-current-file))
		 "</TITLE><OL>")
      (w3-insert (mapconcat 'w3-format-gopher-link objs ""))
      (w3-insert "</OL>"))))	

(defun w3-gopher-retrieve (host port selector &optional wait-for)
  "Fetch a gopher object and don't mess with it at all"
  (let ((proc (w3-open-stream "*gopher*" w3-working-buffer
			      host (if (stringp port) (string-to-int port)
				     port)))
	(len nil)
	(parsed nil)
	(buff (current-buffer)))
    (w3-clear-tmp-buffer)
    (setq w3-current-file selector
	  w3-current-port port
	  w3-current-server host
	  w3-current-last-buffer buff
	  w3-current-type "gopher")
    (if (> (length selector) 0)
	(setq selector (substring selector 1 nil)))
    (if (stringp proc)
	(message proc)
      (save-excursion
	(process-send-string proc (concat selector "\n"))
	(while (and (or (not wait-for)
			(progn
			  (goto-char (point-min))
			  (not (re-search-forward wait-for nil t))))
		    (memq (w3-process-status proc) '(run open)))
	  (if (not parsed)
	      (cond
	       ((and (eq ?+ (char-after 1))
		     (memq (char-after 2)
			   (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
		(setq parsed (copy-marker 2)
		      len (read parsed))
		(delete-region (point-min) parsed))
	       ((and (eq ?+ (char-after 1))
		     (eq ?- (char-after 2)))
		(setq len nil
		      parsed t)
		(goto-char (point-min))
		(delete-region (point-min) (progn
					     (end-of-line)
					     (point))))
	       ((and (eq ?- (char-after 1))
		     (eq ?- (char-after 2)))
		(setq parsed t
		      len nil)
		(goto-char (point-min))
		(delete-region (point-min) (progn
					     (end-of-line)
					     (point))))))
	  (if len (w3-lazy-message "Read %d of %d bytes (%d%%)" (point-max) len
				   (/ (* (point-max) 100) len))
	    (w3-lazy-message "Read %d bytes." (point-max)))
	  (w3-accept-process-output proc))
	(condition-case ()
	    (w3-kill-process proc)
	  (error nil))
	(w3-replace-regexp "\\\n*Connection closed.*\\\n*" "")
	(w3-replace-regexp "\\\n*Process .*gopher.*\\\n*" "")
	(while (looking-at "\r") (delete-char 1))))))

(defun w3-do-gopher-cso-search (descr)
  "Do a gopher CSO search and return a plaintext document"
  (let ((host (nth 0 descr))
	(port (nth 1 descr))
	(file (nth 2 descr))
	search-type search-term)
    (string-match "search-by=\\([^&]+\\)" file)
    (setq search-type (w3-match file 1))
    (string-match "search-term=\\([^&]+\\)" file)
    (setq search-term (w3-match file 1))
    (w3-gopher-retrieve host port (format "2query %s=%s"
					  search-type search-term) "^[2-9]")
    (goto-char (point-min))
    (w3-replace-regexp "^-[0-9][0-9][0-9]:[0-9]*:" "")
    (w3-replace-regexp "^[^15][0-9][0-9]:.*" "")
    (w3-replace-regexp "^[15][0-9][0-9]:\\(.*\\)" "<H1>\\1</H1>&ensp;<PRE>")
    (goto-char (point-min))
    (w3-insert "<TITLE>Results of CSO search</TITLE>\n"
	    "<H1>" search-type " = " search-term "</H1>\n")
    (goto-char (point-max))
    (w3-insert "</PRE>")))

(defun w3-do-gopher (descr)
  "Fetch a gopher object"
  (let ((host (nth 0 descr))
	(port (nth 1 descr))
	(file (nth 2 descr))
	(type (nth 3 descr))
	(extr (nth 4 descr))
	(buff (current-buffer))
	parse-gopher)
    (cond
     ((and				; Gopher CSO search
       (equal type "www/gopher-cso-search")
       (string-match "search-by=" file)) ; With a search term in it
      (w3-do-gopher-cso-search descr)
      (setq type "text/html"))
     ((equal type "www/gopher-cso-search") ; Blank CSO search
      (w3-clear-tmp-buffer)
      (w3-insert "<TITLE> CSO SEARCH </TITLE>\n"
	      "<H1> This is a CSO search </H1>\n"
	      "<HR>\n"
	      "<FORM><LI> Search by: <SELECT NAME=\"search-by\">\n"
	      "<OPTION>Name<OPTION>Phone<OPTION>Email<OPTION>Address"
	      "</SELECT>\n<LI> Search for: <INPUT NAME=\"search-term\">\n"
	      "<LI>&ensp;<INPUT TYPE=\"submit\" VALUE=\"Submit query\">\n"
	      "</UL></FORM><HR>")
      (setq type "text/html"
	    parse-gopher t))
     ((and
       (equal type "www/gopher-search")	; Ack!  Mosaic-style search href
       (string-match "\\\t" file))	; and its got a search term in it!
      (w3-gopher-retrieve host port file)
      (setq type "www/gopher"
	    parse-gopher t))
     ((and
       (equal type "www/gopher-search")	; Ack!  Mosaic-style search href
       (string-match "\\?" file))	; and its got a search term in it!
      (setq file (concat (substring file 0 (match-beginning 0)) "\t"
			 (substring file (match-end 0) nil)))
      (w3-gopher-retrieve host port file)
      (setq type "www/gopher"
	    parse-gopher t))
     ((equal type "www/gopher-search")	; Ack!  Mosaic-style search href
      (setq type "text/html"
	    parse-gopher t)
      (w3-clear-tmp-buffer)
      (w3-insert "<TITLE>Gopher Server</TITLE>\n")
      (w3-insert "<H1>Searchable Gopher Index</H1>")
      (w3-insert "<HR>Enter the search keywords below<P>")
      (w3-insert "<FORM><INPUT NAME=\"internal-gopher\">&ensp;</FORM><HR>"))
     ((null extr)			; Normal Gopher link
      (w3-gopher-retrieve host port file)
      (setq parse-gopher t))
     ((eq extr 'gopher+)		; A gopher+ link
      (w3-gopher-retrieve host port (concat file "\t+"))
      (setq parse-gopher t))
     ((eq extr 'ask-block)		; A gopher+ interactive query
      (w3-gopher-retrieve host port (concat file "\t!")) ; Fetch the info
      (goto-char (point-min))
      (cond
       ((re-search-forward "^\\+ASK:[ \\\t\\\r]*" nil t) ; There is an ASK
	(let ((x (buffer-substring (1+ (point))
				   (or (re-search-forward "^\\+[^:]+:" nil t)
				       (point-max)))))
	  (erase-buffer)
	  (w3-insert (w3-convert-ask-to-form x))
	  (setq type "text/html" parse-gopher t)))
       (t (setq parse-gopher t)))))
    (if (or (equal type "www/gopher")
	    (equal type "text/plain")
	    (equal file "")
	    (equal type "text/html"))
	(w3-gopher-clean-text))
    (if (and parse-gopher (or (equal type "www/gopher")
			      (equal file "")))
	(progn
	  (funcall (w3-mime-viewer "www/gopher"))
	  (setq type "text/html"
		w3-current-mime-viewer (w3-mime-viewer type))))
    (setq w3-current-mime-type (or type "text/plain")
	  w3-current-mime-viewer (w3-mime-viewer type)
	  w3-current-file file
	  w3-current-port port
	  w3-current-server host
	  w3-current-last-buffer buff
	  w3-current-type "gopher")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for printing out roman numerals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-decimal-to-roman (n)
  "Convert from decimal to roman numerals"
  (let ((curmod 1000)
	(str "")
	(j 7)
	i2 k curcnt)
    (while (>= curmod 1)
      (if (>= n curmod)
	  (progn
	    (setq curcnt (/ n curmod)
		  n (- n (* curcnt curmod)))
	    (if (= 4 (% curcnt 5))
		(setq i2 (+ j (if (> curcnt 5) 1 0))
		      str (format "%s%c%c" str
				  (aref w3-roman-characters (1- j))
				  (aref w3-roman-characters i2)))
	      (progn
		(if (>= curcnt 5)
		    (setq str (format "%s%c" str (aref w3-roman-characters j))
			  curcnt (- curcnt 5)))
		(setq k 0)
		(while (< k curcnt)
		  (setq str (format "%s%c" str
				    (aref w3-roman-characters (1- j)))
			k (1+ k)))))))
      (setq curmod (/ curmod 10)
	    j (- j 2)))
    str))		     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for formatting nested lists in html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-expand-list (data)
  "Expand a list that has been hidden."
  (let ((buffer-read-only nil))
    (w3-unhide-zone (nth 1 data) (nth 2 data))))

(defun w3-rehide-list (data)
  "Hide a list that was viewable."
  (let ((buffer-read-only nil))
    (w3-hide-zone (nth 1 data) (nth 2 data))))
   
(defun w3-build-table (indent-level attributes)
  "Build a Definition List"
  (set-buffer w3-working-buffer)
  (let ((x (make-string indent-level 9))
	(y (or (nth (1- indent-level) (cdr (assoc "DL" w3-list-chars-assoc)))
	       "*")))
    (goto-char (point-min))
    (w3-replace-regexp "[ \\\t\\\n]*<DD[^>]*>[ \\\t\\\n]*"
		       (concat "\n" x "  "))
    (w3-replace-regexp "[ \\\t\\\n]*<DT[^>]*>[ \\\t\\\n]*"
		       (concat "\n" x y " "))
    (w3-replace-regexp "</*DL[^>]*>" "\n")))

(defun w3-build-ordered-list (indent-level &optional attributes)
  "Builds ordered lists"
  (let ((roman (assoc "roman" attributes))
	(hidden (equal (downcase (or (cdr (assoc "folded" attributes)) "no"))
		       "yes"))
	(label (or (cdr (assoc "label" attributes)) "\\/ Expand List \\/")))
    (set-buffer w3-working-buffer)
    (goto-char (point-min))
    (let ((x 1) y
	  (z (or (nth (1- indent-level) (cdr (assoc "OL" w3-list-chars-assoc)))
		 "."))
	  parm url alt
	  (tabs (make-string indent-level 9)))
      (goto-char (point-min))
      (while (re-search-forward "<LI\\([^>]*\\)>[ \\\t]*" nil t)
	(setq parm (prog1
		       (w3-parse-args (match-beginning 1) (match-end 1))
		     (replace-match ""))
	      url (cdr (assoc "src" parm))
	      alt (cdr (assoc "alt" parm))
	      y (format "\n%s%3s%s " tabs
			(if roman (w3-decimal-to-roman x)
			  (format "%d" x)) z))
	(cond
	 ((and (null alt) (null url)) (w3-insert y))
	 ((and url (fboundp 'w3-insert-graphic))
	  (w3-insert-graphic (list url) (1- (point)) 'center
			     (or alt (nth (1- indent-level)
					  (cdr (assoc "OL" 
						      w3-list-chars-assoc)))
				 ".")))
	 (alt (w3-insert alt)))
	(setq x (1+ x))))
    (goto-char (point-min))
    (w3-replace-regexp "</*OL[^>]*>" "\n")
    (if (not hidden) nil
      (goto-char (point-min))
      (w3-insert label)
      (w3-hide-zone (point) (point-max))
      (w3-add-zone (point-min) (point) nil
		   (list 'w3expandlist (set-marker (make-marker) (point))
			 (set-marker (make-marker) (point-max))) t))))

(defun w3-build-unordered-list (indent-level attributes)
  "Build unordered lists"
  (let ((hidden (equal (downcase (or (cdr (assoc "folded" attributes)) "no"))
		       "yes"))
	(plain (assoc "plain" attributes))
	(label (or (cdr (assoc "label" attributes)) "\\/ Expand List \\/")))
    (setq plain (and plain (not (equal "no" (cdr plain)))))
    (set-buffer w3-working-buffer)
    (goto-char (point-min))
    (let ((x (concat "\n" (make-string indent-level 9)))
	  (y (or (nth (1- indent-level)
		      (cdr (assoc "UL" w3-list-chars-assoc))) "*"))
	  parm url alt)
      (while (re-search-forward "<LI\\([^>]*\\)>" nil t)
	(setq parm (prog1
		       (w3-parse-args (match-beginning 1) (match-end 1))
		     (replace-match ""))
	      url (cdr (assoc "src" parm))
	      alt (cdr (assoc "alt" parm)))
	(cond
	 ((and (null alt) (null url) (null plain)) 	; Not a plain list
	  (w3-insert x y " "))
	 ((and (null alt) (null url) plain) 		; Plain list
	  (w3-insert x " "))
	 ((and url (fboundp 'w3-insert-graphic))	; Replace bullet
	  (w3-insert-graphic				; with a graphic img
	   (list url) (1- (point)) 'center
	   (or alt (nth (1- indent-level)
			(cdr (assoc "UL" w3-list-chars-assoc)))
	       "*")))
	 (alt (w3-insert alt)))))			; Use alt instd of img
    (goto-char (point-min))
    (w3-replace-regexp "</*\\(UL\\|DIR\\|MENU\\)[^>]*>" "\n")
    (if (not hidden) nil
      (goto-char (point-min))
      (w3-insert label)
      (w3-hide-zone (point) (point-max))
      (w3-add-zone (point-min) (point) nil
		   (list 'w3expandlist (set-marker (make-marker) (point))
			 (set-marker (make-marker) (point-max))) t))))

(defun w3-handle-lists (indent-level)
  "Handle building of lists - INDENT-LEVEL is how many tabs to use
to indent from the left margin."
  (let ((type (upcase (buffer-substring (match-beginning 1) (match-end 1))))
	(parm (w3-parse-args (match-beginning 2) (match-end 2)))
	(pos nil))
    (while (setq pos (w3-sublists-exist type))
      (goto-char pos)
      (setq indent-level (1+ indent-level)
	    type (upcase (buffer-substring (match-beginning 1) (match-end 1)))
	    parm (w3-parse-args (match-beginning 2) (match-end 2))))
    (narrow-to-region (- (point) (+ 2 (length type) (length parm)))
		      (if (re-search-forward (format "</%s>" type) nil t)
			  (point)
			(point-max)))
    (cond
     ((equal "OL" type) (w3-build-ordered-list indent-level parm))
     ((equal "DL" type) (w3-build-table indent-level parm))
     (t (w3-build-unordered-list indent-level parm)))
    (w3-fill-paragraphs-in-list indent-level type)
    (widen)))

(defun w3-fill-paragraphs-in-list (indent-level type)
  "This will fill all the paragraphs within the current list.  INDENT-LEVEL
is the number of tabs to use as the leading fill."
  (w3-replace-regexp "\\\n\\\n+" "\n")
  (goto-char (point-min))
  (let ((fill-prefix (concat (make-string indent-level 9)
			     (if (equal type "OL") "     " "  ")))
	st nd ptag)
    (w3-replace-regexp "<[bB][Rr]> *" (concat "<X>\n" fill-prefix "<W3BR>"))
    (goto-char (point-min))
    (while (re-search-forward "^[^\\\n]" nil t)
      (setq st (progn (beginning-of-line) (point))
 	    nd (progn (end-of-line) (point)))
      (save-restriction
 	(narrow-to-region st nd)
 	(goto-char (point-min))
 	(while (re-search-forward " *<P\\([^>]*\\)> *" nil t)
 	  (setq ptag (buffer-substring (match-beginning 1) (match-end 1)))
 	  (setq st (match-beginning 0))
 	  (if (and (>= (length ptag) 2)
 		   (equal "re" (downcase (substring ptag 0 2))))
 	      (re-search-forward "</PRE>" nil t)
 	    (replace-match (concat "\n\n" fill-prefix))
 	    (if (string-match "ID=\"\\([^\"]+\\)\"" ptag)
 		(w3-add-zone st (progn (end-of-line) (point))
 			     w3-default-style
 			     (cons 'w3par
 				   (list (substring ptag (match-beginning 1)
 						    (match-end 1))
 					 nil nil nil nil nil nil nil))))))
 	(while (re-search-forward "^[^\\\t]" nil t)
	  (beginning-of-line)
	  (insert-before-markers fill-prefix))
 	(fill-region (point-min) (point-max))))))

(defun w3-sublists-exist (type)
  "Figure out if there are sublists in the current list.  Expects point to
be _AFTER_ the current list tag, and type to be bound to what sort of
list it is (OL, UL, DL, MENU, etc)"
  (save-excursion
    (let* ((thestart  (point))
	   (newend (if (re-search-forward (format "</%s>" type) nil t)
		       (point)
		     (point-max))))
      (goto-char thestart)
      (if (re-search-forward "<\\(DL\\|OL\\|UL\\|DIR\\|MENU\\)\\([^>]*\\)>"
			     newend t)
	  (point)
	nil))))

(defun w3-do-lists ()
  (let ((tmp 0)
	(last (point-min)))
    (while (progn
	     (goto-char last)
	     (re-search-forward "<\\(DL\\|OL\\|UL\\|DIR\\|MENU\\)\\([^>]*\\)>"
				nil t))
      (setq last (match-beginning 0))
      (w3-handle-lists 1)
      (setq tmp (1+ tmp))
      (w3-lazy-message "Building lists...%s" (make-string tmp ?.)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for HTTP/1.0 MIME messages                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-mime-types (&optional fname)
  "Parse out the ~/.mime-types files"
  (let (mtype extns x)
    (setq fname (or fname (expand-file-name "~/.mime-types")))
    (and fname (not (and (file-exists-p fname)
			 (file-readable-p fname)))
	 (error "%s is non-existant or unreadable." fname))
    (save-excursion
      (set-buffer (get-buffer-create " *mime-types*"))
      (erase-buffer)
      (insert-file-contents fname)
      (w3-replace-regexp "#.*" "")
      (w3-replace-regexp "\\\n+" "\n")
      (w3-replace-regexp "[ \\\t]+$" "")
      (goto-char (point-min))
      (while (re-search-forward "^\\([^ \\\t\\\n]+\\)[ \\\t]+\\(.*\\)[ \\\t]*\\\n+"
				nil t)
	(setq mtype (buffer-substring (match-beginning 1) (match-end 1))
	      extns (buffer-substring (match-beginning 2) (match-end 2)))
	(replace-match "")
	(setq extns (mapcar (function (lambda (x) (concat "." (car x))))
			    (w3-split extns "[ \\\t\\.]+")))
	(while extns
	  (setq x (w3-extension-to-mime (car extns)))
	  (if (or (not x) w3-mime-mimetypes-overrides)
	      (setq w3-mime-extensions (cons (cons (car extns) mtype)
					     w3-mime-extensions)))
	  (setq extns (cdr extns))))
      (kill-buffer (current-buffer)))))

(defun w3-parse-mailcap (&optional fname)
  "Parse out the ~/.mailcap file"
  (let (major				; The major mime type (image/audio/etc)
	minor				; The minor mime type (gif, basic, etc)
	save-pos			; Misc saved positions used in parsing
	viewer				; How to view this mime type
	info				; Misc info about this mime type
	old-major			; Does the major area exist in the
					; w3-mime-viewers assoc list yet?
	)
    (if (not fname)
	(if (and (file-exists-p  (expand-file-name "~/.mailcap"))
		 (file-readable-p (expand-file-name "~/.mailcap")))
	    (setq fname (expand-file-name "~/.mailcap"))
	  (error "%s is non-existant or unreadable"
		 (expand-file-name "~/.mailcap"0))))
    (save-excursion
      (set-buffer (get-buffer-create " *mailcap*"))
      (if (string-match w3-nonrelative-link fname)
	  (let ((w3-working-buffer " *mailcap*"))
	    (w3-retrieve fname))
	(erase-buffer)
	(insert-file-contents fname))
      (w3-replace-regexp "#.*" "")	         ; Remove all comments
      (w3-replace-regexp "\\\n+" "\\\n")         ; And blank lines
      (w3-replace-regexp "\\\\[ \\\t\\\n]+" " ") ; And collapse spaces
      (w3-replace-regexp (concat (regexp-quote "\\") "[ \\\t]*\\\n") "")
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \\\t\\\n")
	(setq save-pos (point)
	      info nil)
	(skip-chars-forward "^/;")
	(downcase-region save-pos (point))
	(setq major (buffer-substring save-pos (point)))
	(skip-chars-forward "/ \\\t\\\n")
	(setq save-pos (point))
	(skip-chars-forward "^;")
	(downcase-region save-pos (point))
	(setq minor
	      (cond
	       ((= ?* (or (char-after save-pos) 0)) ".*")
	       ((= (point) save-pos) ".*")
	       (t (buffer-substring save-pos (point)))))
	(skip-chars-forward "; \\\t\\\n")
	;;; Got the major/minor chunks, now for the viewers/etc
	;;; The first item _must_ be a viewer, according to the
	;;; RFC for mailcap files (#1343)
	(skip-chars-forward "; \\\t\\\n")
	(setq save-pos (point))
	(skip-chars-forward "^;\\\n")
	(if (= (or (char-after save-pos) 0) ?')
	    (setq viewer (progn
			   (narrow-to-region (1+ save-pos) (point))
			   (goto-char (point-min))
			   (prog1
			       (read (current-buffer))
			     (goto-char (point-max))
			     (widen))))
	  (setq viewer (buffer-substring save-pos (point))))
	(setq save-pos (point))
	(end-of-line)
	(setq info (w3-parse-mailcap-extras save-pos (point))
	      old-major (assoc major w3-mime-viewers))
	(if (not (w3-mailcap-entry-passes-test info))
	    (message "%s/%s failed test clause..." major minor)
	  (and info (w3-mime-mailcap-handle-info major minor info))
	  (cond
	   ((null old-major)		; New major area
	    (setq w3-mime-viewers
		  (cons (cons major (list (cons minor viewer)))
			w3-mime-viewers)))
	   ((not (assoc minor (cdr old-major))) ; not in minor area either
	    (setcdr old-major (cons (cons minor viewer) (cdr old-major))))
	   (w3-mime-mailcap-overrides
	    (let ((tmp (w3-in-assoc minor (cdr old-major))))
	      (setcar tmp minor)
	      (setcdr tmp viewer)))
	   (t (message "Skipping %s/%s from mailcap..." major minor))))))))

(defun w3-mailcap-entry-passes-test (info)
  "Return t iff a mailcap entry passes its test clause
or no test clause is present."
  (let (status				; Call-process-regions return value
	(test (cdr (assoc "test" info))); The test clause
	)
    (setq status (and test (nreverse (mapcar 'car (w3-split test " +")))))
    (cond
     ((and (equal (nth 0 status) "test")
	   (equal (nth 1 status) "-n")
	   (or (equal (nth 2 status) "$DISPLAY")
	       (equal (nth 2 status) "\"$DISPLAY\"")))
      (setq status (if (getenv "DISPLAY") 0 1)))
     ((and (equal (nth 0 status) "test")
	   (equal (nth 1 status) "-z")
	   (or (equal (nth 2 status) "$DISPLAY")
	       (equal (nth 2 status) "\"$DISPLAY\"")))
      (setq status (if (getenv "DISPLAY") 1 0)))
     (test
      (setq status
	    (apply 'call-process (car status) nil nil nil (cdr status))))
     (t (setq status 0)))
    (equal status 0)))

(defun w3-parse-mailcap-extras (st nd)
  "Grab all the extra stuff from a mailcap entry"
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	)
    (save-restriction
      (narrow-to-region st nd)
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \\\n\\\t")
	(setq name-pos (point))
	(skip-chars-forward "^ \\\n\\\t=")
	(downcase-region name-pos (point))
	(setq name (buffer-substring name-pos (point)))
	(skip-chars-forward " \\\t\\\n")
	(if (/= (or (char-after (point)) 0)  ?=) ; There is no value
	    (setq value nil)
	  (skip-chars-forward " \\\t\\\n=")
	  (setq val-pos (point)
		value (buffer-substring val-pos
					(progn
					  (skip-chars-forward "^;")
					  (point)))))
	(setq results (cons (cons name value) results)))
      results)))  

(defun w3-mime-mailcap-handle-info (major minor info)
  (let ((tmp nil)
	(typ (concat major "/" minor)))
    (if (setq tmp (cdr (assoc "label" info)))
	(setq w3-mime-descriptions (cons (cons typ tmp)
					 w3-mime-descriptions)))
    (if (setq tmp (cdr (assoc "description" info)))
	(setq w3-mime-descriptions (cons (cons typ tmp)
					 w3-mime-descriptions)))
    (if (setq tmp (cdr (assoc "compose" info)))
	(setq w3-mime-composers (cons (cons typ tmp) w3-mime-composers)))
    (if (setq tmp (cdr (assoc "edit" info)))
	(setq w3-mime-editors (cons (cons typ tmp) w3-mime-editors)))
    (if (setq tmp (cdr (assoc "print" info)))
	(setq w3-mime-printers (cons (cons typ tmp) w3-mime-printers)))
    (if (setq tmp (cdr (assoc "x11-bitmap" info)))
	(setq w3-mime-bitmaps (cons (cons typ tmp) w3-mime-bitmaps)))
  ;;; We don't really need to worry about copiousoutput, but perhaps
  ;;; this could be used to guess whether we should show the output
  ;;; or not?

  ;;; Should I find a good way to support the needsterminal flag?
  ;;; Don't need to worry about it on a dumb tty, but in X it should
  ;;; be used to store whether we need to do an xterm -e <command>
  ;;; otherwise, text processes could be spawned under X, where they
  ;;; can't be seen.
    ))

(defun w3-mime-viewer (encoding)
  "Get the mime viewer command for ENCODING, return nil if none found.
Expects an argument like text/html, or application/dvi"
  (if (not encoding) (setq encoding "/"))
  (string-match "/" encoding)
  (let* ((major (substring encoding 0 (match-beginning 0)))
	 (minor (substring encoding (1+ (match-beginning 0)) nil))
	 (alist (cdr-safe (assoc major w3-mime-viewers)))
	 (viewr nil))
    (if alist
	(setq viewr (cdr-safe (w3-in-assoc minor alist))))
    viewr))

(defun w3-parse-viewer-types ()
  "Create a string usable for an Accept: header from w3-mime-viewere"
  (let ((tmp w3-mime-viewers)
	mjr mnr (str ""))
    (while tmp
      (setq mnr (cdr (car tmp))
	    mjr (car (car tmp))
	    tmp (cdr tmp))
      (while mnr
	(if (> (+ (% (length str) 60)
		  (length (concat ", " mjr "/" (car (car mnr))))) 60)
	    (setq str (format "%s\nAccept: %s/%s" str mjr
			      (if (string= ".*" (car (car mnr))) "*"
				(car (car mnr)))))
	  (setq str (format "%s, %s/%s" str mjr
			    (if (string= ".*" (car (car mnr))) "*"
			      (car (car mnr))))))
	(setq mnr (cdr mnr))))
    (substring str 2 nil)))

(defun w3-create-multipart-request (file-list)
  "Create a multi-part MIME request for all files in FILE-LIST"
  (let ((separator (current-time-string))
	(content "message/http-request")		   
	(ref-url nil))
    (setq separator
	  (concat "separator-"
		  (mapconcat
		   (function
		    (lambda (char)
		      (if (memq char w3-mime-separator-chars)
			  (char-to-string char) ""))) separator "")))
    (cons separator
	  (concat
	   (mapconcat
	    (function
	     (lambda (file)
	       (concat "--" separator "\nContent-type: " content "\n\n"
		       (w3-create-mime-request file ref-url)))) file-list "\n")
	   "--" separator))))
              
(defun w3-create-mime-request (fname ref-url)
  "Create a MIME request for fname, referred to by REF-URL."
  (if (not (w3-member w3-current-server w3-bad-server-list))
      (let* ((url (w3-view-url t))
	     (extra-headers)
	     (request nil))
	(setq extra-headers (mapconcat
			     (function (lambda (x)
					 (concat (car x) ": " (cdr x))))
			     w3-request-extra-headers "\n"))
	(if (not (equal extra-headers ""))
	    (setq extra-headers (concat extra-headers "\n")))
	(setq request
	      (format
	       (concat
		"%s %s HTTP/1.0\n"			; The request
		"From: %s\n"				; Who its from
		"Accept-encoding: x-compress; x-gzip\n"	; Encoding
		"%s"					; Authentication
		"Accept: %s\n"				; Accept-string
		"User-Agent: Emacs-W3/%s\n"		; User agent
		"%s"					; Where we came from
		"%s"					; Any extra headers
		"%s"					; Any data
		"\n")					; End request
	       (or w3-request-method "GET")
	       fname
	       w3-personal-mail-address
	      (if (w3-basic-auth url)
		  (format "Authorization: Basic %s\n" (w3-basic-auth url))
		"")
	      w3-mime-accept-string
	      w3-version-number
	      (if ref-url (concat "Referer: " ref-url "\n") "")
	      extra-headers
	      (if w3-request-data
		  (format "Content-length: %d\n\n%s"
			  (length w3-request-data) w3-request-data) "")))
	request)
    (format "GET %s\n" fname)))

(defun w3-parse-mime-headers (&optional no-delete)
  "Parse mime headers and remove them from the html"
  (set-buffer w3-working-buffer)
  (let* ((st (point-min))
	 (nd (progn
	       (goto-char (point-min))
	       (skip-chars-forward " \\\t\\\n")
	       (if (re-search-forward "^\r*$" nil t)
		   (1+ (point))
		 (point-max))))
	 save-pos
	 status
	 hname
	 hvalu
	 result
	 )
    (narrow-to-region st nd)
    (goto-char (point-min))
    (skip-chars-forward " \\\t\\\n")	; Get past any blank crap
    (skip-chars-forward "^ \\\t")	; Skip over the HTTP/xxx
    (setq status (read (current-buffer)); Quicker than buffer-substring, etc.
	  result (cons (cons "status" status) result))
    (end-of-line)
    (while (not (eobp))
      (skip-chars-forward " \\\t\\\n")
      (setq save-pos (point))
      (skip-chars-forward "^:\\\n")
      (downcase-region save-pos (point))
      (setq hname (buffer-substring save-pos (point)))
      (skip-chars-forward ": \\\t ")
      (setq save-pos (point))
      (skip-chars-forward "^;\\\n")
      (setq hvalu (buffer-substring save-pos (point))
	    result (cons (cons hname hvalu) result)))
    (or no-delete (delete-region st nd))
    (setq w3-current-mime-type (cdr (assoc "content-type" result))
	  w3-current-mime-encoding (cdr (assoc "content-encoding" result))
	  w3-current-mime-viewer (w3-mime-viewer w3-current-mime-type)
	  w3-current-mime-headers result)
    (cond
     ((= status 500) nil)		; Internal server error
     ((= status 501) nil)		; Facility not supported
     ((= status 400) nil)		; Bad request - syntax
     ((= status 401)			; Unauthorized access, retry w/auth.
      (let* ((y (cdr (assoc "www-authenticate" result)))
	     (type (downcase (if (string-match "[ \\\t]" y)
				 (substring y 0 (match-beginning 0))
			       y)))
	     (x (intern (concat "w3-" type "-auth"))))
	(cond
	 ((or (equal "pem" type) (equal "pgp" type))
	  (string-match "entity=\"\\([^\"]+\\)\"" y)
	  (w3-fetch-with-pgp w3-current-file (w3-match y 1) (intern type)))
	 ((fboundp x)
	  (funcall x (w3-view-url t) t (funcall x (w3-view-url t)))
	  (w3-retrieve (w3-view-url t)))
	 (t
	  (goto-char (point-max))
	  (w3-insert "<HR>Sorry, but I do not know how to handle" y
		     " authentication.  If you'd like to write it,"
		     " send it to wmperry@indiana.edu.<HR>")))))
     ((= status 402) nil)		; Payment required, retry w/Chargeto:
     ((= status 403) nil)		; Access is forbidden
     ((= status 404) nil)		; Not found...
     ((or (= status 301)		; Moved - retry with Location: header
	  (= status 302)		; Found - retry with Location: header
	  (= status 303))		; Method - retry with location/method
      (let ((x (w3-view-url t))
	    (redir (or (cdr (assoc "uri" result))
		       (cdr (assoc "location" result))))
	    (redirmeth (or (cdr (assoc "method" result)) "GET")))
	(if (not (equal x redir))
	    (let ((w3-request-method redirmeth))
	      (w3-maybe-relative redir))
	  (progn
	    (goto-char (point-max))
	    (w3-insert "<HR>Error!  This URL tried to redirect me to itself!<P>"
		       "Please notify the server maintainer.")))))
     ((= status 204)			; No response - leave old document
      (kill-buffer w3-working-buffer))
     (t nil))				; All others indicate success
    (widen)
    result))

(defun w3-lf-to-crlf (str)
  "Convert all linefeeds to carriage-return-line-feed pairs in string STR"
  (mapconcat (function
	      (lambda (x)
		(if (= x 10) "\r\n" (char-to-string x)))) str ""))	     

(defun w3-mime-response-p ()
  "Determine if the current buffer is a MIME response"
  (set-buffer w3-working-buffer)
  (if (equal w3-current-type "http")
      (progn
	(goto-char (point-min))
	(if (re-search-forward
	     (regexp-quote (w3-lf-to-crlf
			    (w3-create-mime-request w3-current-file ".*")))
	     nil t)
	    (replace-match ""))))
  (goto-char (point-min))
  (skip-chars-forward " \\\t\\\n")
  (and (looking-at "^HTTP/.+")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for compatibility with XMosaic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for global history file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-global-history (&optional fname)
  "Parse out the mosaic global history file for completions, etc."
  (or fname (setq fname (expand-file-name w3-global-history-file)))
  (if (not (file-exists-p fname))
      (message "%s does not exist." fname)
    (save-excursion
      (set-buffer (get-buffer-create " *w3-tmp*"))
      (erase-buffer)
      (insert-file-contents fname)
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (while (re-search-forward "^\\([^ \\\t]+\\)[ \\\t]+\\(.*\\)" nil t)
	(setq w3-global-history-completion-list
	      (cons (cons (buffer-substring (match-beginning 1)
					    (match-end 1))
			  (buffer-substring (match-beginning 2)
					    (match-end 2)))
		    w3-global-history-completion-list))))))

(defun w3-write-global-history (&optional fname)
  "Write the global history file into w3-global-history-file"
  (interactive)
  (if (not fname) (setq fname w3-global-history-file))
  (if (not (file-exists-p w3-global-history-file))
      (progn
	(message "Creating history file %s." w3-global-history-file)
	(set-buffer (get-buffer-create " *W3HIST*"))
	(erase-buffer)
	(w3-insert "ncsa-mosaic-history-format-1\nGlobal\n"))
    (progn
      (set-buffer (get-buffer-create " *W3HIST*"))
      (erase-buffer)
      (insert-file-contents w3-global-history-file)))
  (let (url)
    (mapcar
     (function
      (lambda (x)
	(setq url (car x))
	(goto-char (point-min))
	(if (not (re-search-forward (regexp-quote url) nil t))
	    (progn
	      (goto-char (point-min))
	      (w3-insert (concat url " " (current-time-string) "\n"))))))
     w3-history-list))
  (write-file w3-global-history-file)
  (kill-buffer (current-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse out the Mosaic documents-menu file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-docs-menu ()
  "Parse the Mosaic documents menu"
  (let ((tmp-menu (append '((separator)) w3-starting-documents
			  '((separator))))
	real-menu x y name url)
    (if (or (not (file-exists-p w3-documents-menu-file))
	    (not (file-readable-p w3-documents-menu-file)))
	(message "No documents menu found... continuing.")
      (save-excursion
	(set-buffer (get-buffer-create " *w3-temp*"))
	(erase-buffer)
	(insert-file-contents w3-documents-menu-file)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (not (looking-at "-+$"))
	      (setq x (progn (beginning-of-line) (point))
		    y (progn (end-of-line) (point))
		    name (prog1
			     (buffer-substring x y)
			   (delete-region x (min (1+ y) (point-max))))
		    x (progn (beginning-of-line) (point))
		    y (progn (end-of-line) (point))
		    url (prog1
			    (buffer-substring x y)
			  (delete-region x (min (1+ y) (point-max))))
		    tmp-menu (if (w3-rassoc url tmp-menu) tmp-menu
			       (cons (cons name url) tmp-menu)))
	    (setq tmp-menu (cons '(separator) tmp-menu))
	    (delete-region (point-min) (min (1+ (progn (end-of-line)
						       (point)))
					    (point-max)))))
	(kill-buffer (current-buffer))))
    (if (equal (car (car tmp-menu)) "") (setq tmp-menu (cdr tmp-menu)))
    (while tmp-menu
      (setq real-menu (cons (if (equal 'separator (car (car tmp-menu)))
				"--------"
			      (vector (car (car tmp-menu))
				      (list 'w3-fetch
					    (if (listp (cdr (car tmp-menu)))
						(car (cdr (car tmp-menu)))
					      (cdr (car tmp-menu)))) t))
			    real-menu)
	    tmp-menu (cdr tmp-menu)))
    (setq w3-navigate-menu (append w3-navigate-menu real-menu
				   (list "-----")))))
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hotlist Handling Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-hotlist-refresh ()
  "Reload the default hotlist file into memory"
  (interactive)
  (w3-parse-hotlist))  

(defun w3-hotlist-delete ()
  "Deletes a document from your hotlist file"
  (interactive)
  (save-excursion
    (if (not w3-hotlist) (message "No hotlist in memory!")
      (if (not (file-exists-p w3-hotlist-file))
	  (message "Hotlist file %s does not exist." w3-hotlist-file)
	(let* ((title (car (assoc (completing-read "Delete Document: "
						   w3-hotlist nil t)
				  w3-hotlist)))
	       (buffer (get-buffer-create " *HOTW3*")))
	  (set-buffer buffer)
	  (erase-buffer)
	  (insert-file-contents w3-hotlist-file)
	  (if (re-search-forward (regexp-quote title) nil t)
	      (progn
		(previous-line 1)
		(beginning-of-line)
		(delete-region (point) (progn (forward-line 2) (point)))
		(write-file w3-hotlist-file)
		(setq w3-hotlist (w3-delete-from-alist title w3-hotlist))
		(kill-buffer (current-buffer)))
	    (message "%s was not found in %s" title w3-hotlist-file))))))
  (if (and w3-running-FSF19 (eq window-system 'x))
      (progn
	(delete-menu-item '("Navigate"))
	(w3-build-FSF19-menu))))

(defun w3-hotlist-rename-entry (title)
  "Rename a personal annotation"
  (interactive (list (completing-read "Rename entry: " w3-hotlist nil t)))
  (cond					; Do the error handling first
   ((not w3-hotlist) (error "No hotlist in memory!"))
   ((not (file-exists-p (expand-file-name w3-hotlist-file)))
    (error "Hotlist file %s does not exist." w3-hotlist-file))
   ((not (file-readable-p (expand-file-name w3-hotlist-file)))
    (error "Hotlist file %s exists, but is unreadable." w3-hotlist-file)))
  (save-excursion
    (let ((obj (assoc title w3-hotlist))
	  (used (mapcar 'car w3-hotlist))
	  (buff (get-buffer-create " *HOTW3*"))
	  (new nil)
	  )
      (while (or (null new) (w3-member new used))
	(setq new (read-string "New name: ")))
      (set-buffer buff)
      (erase-buffer)
      (insert-file-contents (expand-file-name w3-hotlist-file))
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote title) nil t)
	  (progn
	    (previous-line 1)
	    (beginning-of-line)
	    (delete-region (point) (progn (forward-line 2) (point)))
	    (w3-insert (format "%s %s\n%s\n" (nth 1 obj) (current-time-string)
			    new))
	    (setq w3-hotlist (cons (list new (nth 1 obj))
				   (w3-delete-from-alist title w3-hotlist)))
	    (write-file w3-hotlist-file)
	    (kill-buffer (current-buffer))
	    (if (and w3-running-FSF19 window-system)
		(progn
		  (delete-menu-item '("Navigate"))
		  (w3-build-FSF19-menu))))
	(message "%s was not found in %s" title w3-hotlist-file)))))    
       
(defun w3-hotlist-append (fname)
  "Append a hotlist to the one in memory"
  (interactive "fAppend hotlist file: ")
  (let ((x w3-hotlist))
    (w3-parse-hotlist fname)
    (setq w3-hotlist (nconc x w3-hotlist))))

(defun w3-parse-hotlist (&optional fname)
  "Read in the hotlist specified by FNAME"
  (if (not fname) (setq fname w3-hotlist-file))
  (setq w3-hotlist nil)
  (if (not (file-exists-p fname))
      (message "%s does not exist!" fname)
    (let* ((old-buffer (current-buffer)) 
	   (buffer (get-buffer-create " *HOTW3*"))
	   cur-link
	   cur-alias)
      (set-buffer buffer)
      (erase-buffer)
      (insert-file-contents fname)
      (goto-char (point-min))
      (while (re-search-forward "^\n" nil t) (replace-match ""))
      (goto-line 3)
      (while (not (equal (point) (point-max)))
	(re-search-forward "^[^ ]*" nil t)
	(setq cur-link (buffer-substring (match-beginning 0) (match-end 0)))
	(setq cur-alias (buffer-substring (progn
					    (forward-line 1)
					    (beginning-of-line)
					    (point))
					  (progn
					    (end-of-line)
					    (point))))
	(if (not (equal cur-alias ""))
	    (setq w3-hotlist (cons (list cur-alias cur-link) w3-hotlist))))
      (kill-buffer buffer)
      (set-buffer old-buffer))))

;;;###autoload
(defun w3-use-hotlist ()
  "Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist) (message "No hotlist in memory!")
    (let* ((url (car (cdr (assoc
			   (completing-read "Goto Document: " w3-hotlist nil t)
			   w3-hotlist)))))
      (w3-fetch url))))

(defun w3-hotlist-add-document-at-point (pref-arg)
  "Add the document pointed to by the hyperlink under point to the hotlist."
  (interactive "P")
  (let ((url (w3-view-this-url t))
	(title "nil"))
    (or url (error "No link under point."))
    (setq title (nth 3 (w3-zone-data (w3-zone-at (point)))))
    (w3-hotlist-add-document pref-arg title url)))

(defun w3-hotlist-add-document (pref-arg &optional the-title the-url)
  "Add this documents url to the hotlist"
  (interactive "P")
  (save-excursion
    (let* ((buffer (get-buffer-create " *HOTW3*"))
	   (title (or the-title
		      (and pref-arg (read-string "Title: "))
		      (buffer-name)))
	   (url (or the-url (w3-view-url t))))
      (if (w3-rassoc (list url) w3-hotlist)
	  (error "That item already in hotlist, use w3-hotlist-rename-entry."))
      (set-buffer buffer)
      (erase-buffer)
      (setq w3-hotlist (cons (list title url) w3-hotlist))
      (if (not (file-exists-p w3-hotlist-file))
	  (progn
	    (message "Creating hotlist file %s" w3-hotlist-file)
	    (w3-insert "ncsa-xmosaic-hotlist-format-1\nDefault\n\n")
	    (backward-char 1))
	(progn
	  (insert-file-contents w3-hotlist-file)
	  (goto-char (point-max))
	  (backward-char 1)))
      (w3-insert "\n" url " " (current-time-string) "\n" title)
      (write-file w3-hotlist-file)
      (kill-buffer (current-buffer))))
  (if (and w3-running-FSF19 (eq window-system 'x))
      (progn
	(delete-menu-item '("Navigate" ))
	(w3-build-FSF19-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotation server handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-fetch-annotations ()
  "Fetch the annotations for the current document"
  (if (not w3-group-annotation-server)
      (error "No group annotation server defined!")
    (let ((cmd (format "ANN_GET /url=\"%s\";=\n" (w3-view-url t)))
	  (proc nil)
	  (tmp 0)
	  (tmp2 0))
      (save-excursion
	(set-buffer (get-buffer-create w3-working-buffer))
	(goto-char (point-max))
	(w3-insert "\n"))
      (setq proc (w3-open-stream "*anno*" (get-buffer-create w3-working-buffer)
				 w3-group-annotation-server
				 w3-group-annotation-port))
      (message "Fetching annotations...")
      (if (processp proc)
	  (progn
	    (process-send-string proc cmd)
	    (while (memq (w3-process-status proc) '(run open))
	      (if (= 0 (setq tmp2 (% (1+ tmp2) 200)))
		  (message "Fetching annotations..%s" (make-string
						       (setq tmp (% (1+ tmp) 50))
						       ?.)))
	      (w3-accept-process-output proc)))
	(message proc))
      (condition-case ()
	  (w3-kill-process proc);; make sure its dead
	(error nil))
      (w3-replace-regexp (regexp-quote (substring cmd 0 -1)) "")
      (w3-replace-regexp
       "Process .*anno.* \\(exit\\|kill\\|finish\\).*" ""))))

(defun w3-is-annotation ()
  "Is this a group annotation?"
  (and (equal w3-current-server w3-group-annotation-server)
       (= (string-to-int w3-current-port) w3-group-annotation-port)))

(defun w3-delete-group-annotation ()
  "Delete this group annotation"
  (interactive)
  (if (not w3-group-annotation-server)
      (error "No group annotation server defined!")
    (if (w3-is-annotation)
	(let ((cmd (format "ANN_DELETE /url=\"%s\";=\n" (w3-view-url t)))
	      (proc nil)
	      (parseit nil)
	      (buff (current-buffer)))
	  (save-excursion
	    (set-buffer (get-buffer-create w3-working-buffer))
	    (erase-buffer)
	    (setq proc (w3-open-stream "*anno*" (get-buffer-create w3-working-buffer)
				       w3-group-annotation-server
				       w3-group-annotation-port))
	    (if (processp proc)
		(process-send-string proc cmd))
	    (while (memq (w3-process-status proc) '(run open))
	      (w3-accept-process-output proc))
	    (condition-case ()
		(w3-kill-process proc);; make sure its dead
	      (error nil))
	    (w3-replace-regexp (regexp-quote cmd) "")
	    (w3-replace-regexp "Process .*anno.* exit.*" "")
	    (goto-char (point-min))
	    (setq w3-current-type nil
		  w3-current-file "historylist"
		  w3-current-last-buffer buff)
	    (if (and (re-search-forward "[^ \t\n]+" nil t)
		     (not
		      (progn
			(goto-char (point-min))
			(re-search-forward "success!" nil t))))
		(setq parseit t)
	      (setq parseit nil)))
	  (if parseit
	      (w3-sentinel nil nil)
	    (message "Deleted...")))
      (message "This is not an annotation."))))

(defun w3-add-group-annotation ()
  "Add an annotation to the current url"
  (interactive)
  (let ((url (w3-view-url t))
	(buf (get-buffer-create "*Annotation*")))
    (if w3-mutable-windows (pop-to-buffer buf) (switch-to-buffer buf))
    (set-buffer buf)
    (erase-buffer)
    (w3-insert "</PRE>\n\n")
    (html-mode)
    (setq w3-current-annotation url)
    (define-key html-mode-map "\C-c\C-c" 'w3-do-group-annotation)
    (message "Hit C-cC-c to send this annotation.")))	

(defun w3-do-group-annotation ()
  "Finish adding an annotation"
  (interactive)
  (let* ((bufsize (buffer-size))
	 (bufstr  (buffer-string))
	 (cmd (format "ANN_%s /url=\"%s\";title=\"%s\";user=\"%s\";date=%s;length=%d;=%s\n"
		      (if w3-editing-annotation "CHANGE" "SET")
		      w3-current-annotation
		      (read-string "Title: "
				   (format "Annotation by %s (%s@%s)"
					   (user-full-name)
					   (user-real-login-name)
					   (system-name)))
		      (format "%s@%s" (user-real-login-name)
			      (system-name))
		      (current-time-string)
		      bufsize bufstr))
	 (proc nil)
	 (parseit nil))
    (save-excursion
      (not-modified)
      (kill-buffer (current-buffer))
      (set-buffer (get-buffer-create w3-working-buffer))
      (erase-buffer)
      (setq proc (w3-open-stream "*anno*" (get-buffer-create w3-working-buffer)
				 w3-group-annotation-server
				 w3-group-annotation-port))
      (process-send-string proc cmd)
      (while (memq (w3-process-status proc) '(run open))
	(w3-accept-process-output proc))
      (condition-case ()
	  (w3-kill-process proc);; make sure its dead
	(error nil))
      (w3-replace-regexp (regexp-quote cmd) "")
      (w3-replace-regexp "Process .*anno.* exit.*" "")
      (goto-char (point-min))
      (setq w3-current-type nil
	    w3-current-file "historylist")
      (if (re-search-forward "[^ \t\n]+" nil t)
	  (setq parseit t)
	(setq parseit nil)))
    (if parseit
	(w3-sentinel nil nil)
      (message "Annotation delivered..."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private annotation support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-personal-annotations ()
  "Read in personal annotation file"
  (if (and 
       (file-exists-p (format "%s/LOG" w3-personal-annotation-directory))
       (file-readable-p (format "%s/LOG" w3-personal-annotation-directory)))
      (save-excursion
	(setq w3-personal-annotations nil);; nuke the old list
	(let ((start nil)
	      (end nil)
	      (txt nil)
	      (url nil)
	      (num nil))
	  (set-buffer (get-buffer-create " *panno*"))
	  (erase-buffer)
	  (insert-file-contents
	   (format "%s/LOG" w3-personal-annotation-directory))
	  (goto-char (point-min))
	  (w3-replace-regexp "\\\n+" "\\\n")
	  (goto-char (point-min))
	  ;; nuke the header lines
	  (delete-region (point-min) (progn (forward-line 2) (point)))
	  (cond
	   ((eobp) nil)			; Empty LOG file
	   (t
	    (if (/= (char-after (1- (point-max))) ?\n)
		(save-excursion
		  (goto-char (point-max))
		  (w3-insert "\n")))
	    (while (/= (point-max) (point))
	      (setq start (point)
		    end (prog2 (end-of-line) (point) (forward-char 1))
		    txt (buffer-substring start end)
		    url (substring txt 0 (string-match " " txt))
		    num (w3-split
			 (substring txt (1+ (string-match " " txt)) nil)
			 "[ \\\t]"))
	      (while num
		(setq w3-personal-annotations 
		      (cons
		       (list url
			     (list (car (car num))
				   (w3-grok-annotation-format
				    (car (car num)))))
		       w3-personal-annotations)
		      num (cdr num))))))
	  (kill-buffer " *panno*")))))

(defun w3-grok-annotation-format (anno)
  "Grab the title from an annotation"
  (save-excursion
    (set-buffer (get-buffer-create " *annotmp*"))
    (erase-buffer)
    (if (file-exists-p (format "%s/PAN-%s.html"
			       w3-personal-annotation-directory anno))
	(insert-file-contents (format "%s/PAN-%s.html"
				      w3-personal-annotation-directory anno)))
    (goto-char (point-min))
    (prog1
	(if (re-search-forward "<title>\\(.*\\)</title>" nil t)
	    (buffer-substring (match-beginning 1) (match-end 1))
	  "No title")
      (kill-buffer " *annotmp*"))))    

(defun w3-fetch-personal-annotations ()
  "Grab any personal annotations for the current url"
  (let ((url  (w3-view-url t))
	(anno w3-personal-annotations)
	(annolist nil))
    (if (assoc url anno)
	(while anno
	  (if (equal (car (car anno)) url)
	      (setq annolist
		    (cons
		     (format "<A HREF=\"file:%s%s/PAN-%s.html\">%s</A>"
			     (if (= ?/ (string-to-char
					w3-personal-annotation-directory)) ""
			       "/")
			     w3-personal-annotation-directory
			     (car (car (cdr (car anno))))
			     (car (cdr (car (cdr (car anno))))))
		     annolist)))
	  (setq anno (cdr anno))))
    annolist))

(defun w3-is-personal-annotation (url)
  "Is URL a personal annotation?"
  (string-match "file:/.*/PAN-.*\\.html" url))

(defun w3-delete-personal-annotation ()
  "Delete a personal annotation."
  (interactive)
  (if (w3-is-personal-annotation (w3-view-url t))
      (let ((num nil)
	    (annotated-url nil)
	    (anno w3-personal-annotations))
	(string-match "file:/.*/PAN-\\(.*\\)\\.html" (w3-view-url t))
	(setq num (substring (w3-view-url t) (match-beginning 1)
			     (match-end 1)))
	(while anno
	  (if (equal num (car (car (cdr (car anno)))))
	      (setq annotated-url (car (car anno))))
	  (setq anno (cdr anno)))
	(if annotated-url
	    (save-excursion
	      (set-buffer (get-buffer-create " *annotmp*"))
	      (erase-buffer)
	      (insert-file-contents (format "%s/LOG"
					    w3-personal-annotation-directory))
	      (replace-regexp (format "[ \\\t]+\\b%s\\b[ \\\t]*" num) " ")
	      (goto-char (point-min))
	      (delete-matching-lines (format "^%s +$" annotated-url))
	      (let ((make-backup-files nil)
		    (version-control nil)
		    (require-final-newline t))
		(write-region (point-min) (point-max)
			      (format "%s/LOG"
				      w3-personal-annotation-directory)))
	      (kill-buffer " *annotmp*")
	      (setq anno w3-personal-annotations
		    w3-personal-annotations nil)
	      (while anno
		(if (not (string= num (car (car (cdr (car anno))))))
		    (setq w3-personal-annotations
			  (cons (car anno) w3-personal-annotations)))
		(setq anno (cdr anno)))
	      (delete-file (format "%s/PAN-%s.html"
				   w3-personal-annotation-directory num)))
	  (message "Couldn't find url that this is annotating!")))
    (message "This isn't a personal annotation.")))  

(defun w3-add-personal-annotation ()
  "Add an annotation to this document."
  (interactive)
  (let ((url (w3-view-url t))
	(buf (get-buffer-create "*Personal Annotation*")))
    (set-buffer buf)
    (if w3-mutable-windows (pop-to-buffer buf) (switch-to-buffer buf))
    (erase-buffer)
    (w3-insert "</PRE>\n\n")
    (html-mode)
    (setq w3-current-annotation url)
    (define-key html-mode-map "\C-c\C-c" 'w3-do-personal-annotation)
    (message "Hit C-cC-c to send this annotation.")))

(defun w3-find-highest-annotation-number ()
  "Find the highest annotation number in this buffer"
  (let (x)
    (goto-char (point-min))
    (while (re-search-forward "[^ \\\t\\\n]*[ \\\t]\\(.*\\)" nil t)
      (setq x (nconc (mapcar (function (lambda (x) (string-to-int (car x))))
			     (w3-split (buffer-substring (match-beginning 1)
							 (match-end 1))
				       "[ \\\t]")) x)))
    (if (not x) (setq x '(0)))
    (1+ (car (sort x '>)))))

(defun w3-do-personal-annotation ()
  "Finish doing a personal annotation."
  (interactive)
  (if (or (not w3-personal-annotation-directory)
	  (not (file-exists-p w3-personal-annotation-directory))
	  (not (file-directory-p w3-personal-annotation-directory)))
      (error "No personal annotation directory!")
    (let ((url w3-current-annotation)
	  (txt (buffer-string))
	  (title (read-string "Title: "
			      (format "Annotation by %s on %s"
				      (user-real-login-name)
				      (current-time-string))))
	  (fname nil)
	  (num nil))
      (save-excursion
	(not-modified)
	(kill-buffer (current-buffer))
	(set-buffer (get-buffer-create " *annotmp*"))
	(erase-buffer)
	(if (file-exists-p		; Insert current LOG file if
					; it exists.
	     (format "%s/LOG" w3-personal-annotation-directory))
	    (insert-file-contents
	     (format "%s/LOG" w3-personal-annotation-directory))
	  (progn			; Otherwise, create a file
	    (goto-char (point-min))	; that conforms to first
					; annotation format from NCSA
	    (w3-insert "ncsa-mosaic-personal-annotation-log-format-1\n")
	    (w3-insert "Personal\n")))
	(goto-char (point-min))
	(setq num (int-to-string (w3-find-highest-annotation-number))
	      fname (format "%s/PAN-%s.html"
			    w3-personal-annotation-directory num))
	(goto-char (point-min))
	(if (re-search-forward (regexp-quote url) nil t)
	    (progn
	      (end-of-line)
	      (w3-insert " "))
	  (goto-char (point-max))
	  (w3-insert "\n" url " "))
	(w3-insert num)
	(let ((make-backup-files nil)
	      (version-control nil)
	      (require-final-newline t))
	  (write-region (point-min) (point-max)
			(format "%s/LOG" w3-personal-annotation-directory))
	  (erase-buffer)
	  (w3-insert (format "%s\n<title>%s</title>\n<h1>%s</h1>\n"
			  w3-annotation-marker
			  title title))
	  (w3-insert
	   (format "<address>%s (%s@%s)</address>\n<address>%s</address>\n"
		   (user-full-name)
		   (user-real-login-name)
		   (system-name)
		   (current-time-string)))
	  (w3-insert "<HR>\n<pre>" txt)
	  (write-region (point-min) (point-max) fname))
	(setq w3-personal-annotations
	      (cons (list url (list num title)) w3-personal-annotations))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WAIS support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-create-wais-source (server port dbase)
  "Create a temporary wais source description file.
Returns the file name the description is in."
  (let ((x (w3-generate-unique-filename))
	(y (get-buffer-create " *waisq-tmp*")))
    (save-excursion
      (set-buffer y)
      (erase-buffer)
      (insert 
       (format "(:source\n:version 3\n:ip-name \"%s\"\n:tcp-port %s\n:database-name \"%s\"\n)" server port dbase))
      (write-region (point-min) (point-max) x nil nil)
      (kill-buffer y))
    x))

(defun w3-wais-stringtoany (str)
  "Return a wais subelement that specifies STR in any database"
  (concat "(:any :size " (length str) " :bytes #( "
	  (mapconcat 'identity str " ")
	  " ) )"))

;(defun w3-retrieve-wais-docid (server port dbase local-id)
;  (call-process "waisretrieve" nil w3-working-buffer nil
;		(format "%s:%s@%s:%s" (w3-unhex-string local-id)
;			dbase server port)))

;(w3-retrieve-wais-docid "quake.think.com" "210" "directory-of-servers"
;			"0 2608 /proj/wais/wais-sources/vpiej-l.src")
(defun w3-retrieve-wais-docid (server port dbase local-id)
  "Retrieve a wais document.
SERVER is the server the database is on (:ip-name in source description)
PORT is the port number to contact (:tcp-port in the source description)
DBASE is the database name (:database-name in the source description)
LOCAL-ID is the document (:original-local-id in the question description)"
  (let* ((dbf (w3-create-wais-source server port dbase))
	 (qstr (format
		(concat "(:question :version 2\n"
			"           :result-documents\n"
			"           ( (:document-id\n"
			"              :document\n"
			"              (:document\n"
			"               :doc-id\n"
			"               (:doc-id :original-database %s\n"
			"                :original-local-id %s )\n"
			"               :number-of-bytes -1\n"
			"               :type \"\"\n"
			"               :source\n"
			"               (:source-id :filename \"%s\") ) ) ) )")
		(w3-wais-stringtoany dbase)
		(w3-wais-stringtoany (w3-unhex-string local-id))
		dbf))
	 (qf (w3-generate-unique-filename)))
    (set-buffer (get-buffer-create w3-working-buffer))
    (insert qstr)
    (write-region (point-min) (point-max) qf nil nil)
    (erase-buffer)
    (call-process w3-waisq-prog nil w3-working-buffer nil "-f" qf "-v" "1")
    (condition-case ()
	(delete-file dbf)
      (error nil))
    (condition-case ()
	(delete-file qf)
      (error nil))))

;(w3-perform-wais-query "quake.think.com" "210" "directory-of-servers" "SGML")
(defun w3-perform-wais-query (server port dbase search)
  "Perform a wais query.
SERVER is the server the database is on (:ip-name in source description)
PORT is the port number to contact (:tcp-port in the source description)
DBASE is the database name (:database-name in the source description)
SEARCH is the search term (:seed-words in the question description)"
  (let ((dbfname (w3-create-wais-source server port dbase))
	(qfname (w3-generate-unique-filename))
	(results 'w3-none-gotten))
    (save-excursion
      (w3-clear-tmp-buffer)
      (insert
       (format
	(concat "(:question\n"
		" :version 2\n"
		" :seed-words \"%s\"\n"
		" :sourcepath \"" w3-temporary-directory "\"\n"
		" :sources\n"
		" (  (:source-id\n"
		"     :filename \"%s\"\n"
		"    )\n"
		" )\n"
		" :maximum-results 100)\n")
	search dbfname))
      (write-region (point-min) (point-max) qfname nil nil)
      (erase-buffer)
      (call-process w3-waisq-prog nil w3-working-buffer nil "-g" "-f" qfname)
      (set-buffer w3-working-buffer)
      (erase-buffer)
      (setq w3-current-server server
	    w3-current-port port
	    w3-current-file dbase)
      (insert-file-contents qfname)
      (goto-char (point-min))
      (if (re-search-forward "(:question" nil t)
	  (delete-region (point-min) (match-beginning 0)))
      (w3-replace-regexp "Process.*finished.*" "")
      (w3-replace-regexp "#" "")
      (goto-char (point-min))
      (message "Done reading info - parsing results...")
      (if (re-search-forward ":result-documents[^(]+" nil t)
	  (progn
	    (goto-char (match-end 0))
	    (while (eq results 'w3-none-gotten)
	      (condition-case ()
		  (setq results (read (current-buffer)))
		(error (progn
			 (setq results 'w3-none-gotten)
			 (goto-char (match-end 0))))))
	    (erase-buffer)
	    (w3-insert "<title>Results of WAIS search</title>\n"
		       "<h1>Searched " dbase " for " search "</h1>\n"
		       "<hr>\n"
		       "Found <b>" (int-to-string (length results))
		       "</b> matches.\n"
		       "<ol>\n<li>"
		       (mapconcat 'w3-parse-wais-doc-id results "\n<li>")
		       "\n</ol>\n<hr>\n"))
	(message "No results"))
      (setq w3-current-mime-type "text/html")
      (condition-case ()
	  (delete-file qfname)
	(error nil))
      (condition-case ()
	  (delete-file dbfname)
	(error nil)))))

(defun w3-wais-anytostring (x)
  "Convert a (:any ....) wais construct back into a string."
  (mapconcat 'char-to-string (car (cdr (memq ':bytes x))) ""))

(defun w3-parse-wais-doc-id (x)
  "Return a list item that points at the doc-id specified by X"
  (let* ((document (car (cdr (memq ':document x))))
	 (doc-id (car (cdr (memq ':doc-id document))))
	 (score (car (cdr (memq ':score x)))) 
	 (title (car (cdr (memq ':headline document))))
	 (type (car (cdr (memq ':type document))))
	 (size (car (cdr (memq ':number-of-bytes document))))
	 (server (car (cdr (memq ':original-server doc-id))))
	 (dbase (car (cdr (memq ':original-database doc-id))))
	 (localid (car (cdr (memq ':original-local-id doc-id))))
	 (dist-server (car (cdr (memq ':distributor-server doc-id))))
	 (dist-dbase (car (cdr (memq ':distributor-database doc-id))))
	 (dist-id (car (cdr (memq ':distributor-local-id doc-id))))
	 (copyright (or (car (cdr (memq ':copyright-disposition doc-id))) 0)))
    (format "<a href=\"wais://%s:%s/%s/%s/%d/1=%s;2=%s;3=%s;4=%s;5=%s;6=%s;7=%d;\">%s (Score = %s)</a>"
	    w3-current-server w3-current-port w3-current-file
	    type size
	    (w3-hexify-string (w3-wais-anytostring server))
	    (w3-hexify-string (w3-wais-anytostring dbase))
	    (w3-hexify-string (w3-wais-anytostring localid))
	    (w3-hexify-string (w3-wais-anytostring dist-server))
	    (w3-hexify-string (w3-wais-anytostring dist-dbase))
	    (w3-hexify-string (w3-wais-anytostring dist-id))
	    copyright title score)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-print-this-url (&optional url)
  "Print out the current document (in LaTeX format)"
  (interactive)
  (if (not url) (setq url (w3-view-url t)))
  (let ((format (completing-read
		 "Format: "
		 '(("HTML Source") ("Formatted Text") ("LaTeX'd"))
		 nil t)))
    (save-excursion
      (cond
       ((equal "HTML Source" format)
	(if w3-current-source
	    (let ((x w3-current-source))
	      (set-buffer (get-buffer-create w3-working-buffer))
	      (erase-buffer)
	      (insert x))
	  (w3-retrieve url))
	(lpr-buffer))
       ((equal "Formatted Text" format)
	(lpr-buffer))
       ((equal "LaTeX'd" format)
	(if w3-current-source
	    (let ((x w3-current-source))
	      (set-buffer (get-buffer-create w3-working-buffer))
	      (erase-buffer)
	      (insert x))
	  (w3-retrieve url))
	(w3-convert-html-to-latex)
	(save-window-excursion
	  (write-region (point-min) (point-max)
			(expand-file-name "w3-tmp.latex"
					  w3-temporary-directory) nil 5)
	  (shell-command
	   (format
	    "cd %s ; latex w3-tmp.latex ; %s w3-tmp.dvi ; rm -f w3-tmp*"
	    w3-temporary-directory
	    w3-print-command))
	  (kill-buffer "*Shell Command Output*")))))))

(defun w3-print-url-under-point ()
  "Print out the url under point (in LaTeX format)"
  (interactive)
  (w3-print-this-url (w3-view-this-url t)))

(defun w3-convert-html-to-latex ()
  "Convert an html document into LaTeX - this is pretty much the same as the
sed scripts from info.cern.ch"
  (interactive)
  (set-buffer w3-working-buffer)
  (if w3-use-html2latex
      (shell-command-on-region (point-min) (point-max)
			       (format "%s %s" w3-html2latex-prog
				       w3-html2latex-args) t)
    (progn
      (goto-char (point-min))
      (w3-replace-regexp "\\\\" "\\\\backslash ")
      (w3-replace-regexp "{" "\\\\{")
      (w3-replace-regexp "}" "\\\\}")
      (goto-char (point-min))
      (w3-insert (concat "\\documentstyle" w3-latex-docstyle "\n"))
      (w3-insert "\\begin{document}\n")
      (goto-char (point-max))
      (w3-insert "\\end{document}")
      (w3-replace-regexp "<\\(XMP\\|LISTING\\)>" "\\\\begin{verbatim}")
      (w3-replace-regexp "</\\(XMP\\|LISTING\\)>" "\\\\end{verbatim}")
      (w3-replace-regexp "<\\(ISINDEX\\|NEXTID\\)[^>]*>" "")
      (w3-replace-regexp (regexp-quote "$") "\\\\$")
      (w3-replace-regexp (regexp-quote "&gt;") "$>$")
      (w3-replace-regexp "%" "\\\\%")
      (w3-replace-regexp "#" "\\\\#")
      (w3-replace-regexp "_" "\\\\_")
      (w3-replace-regexp "~" "\\\\~")
      (w3-replace-regexp "<LI> *" "\\\\item ")
      (w3-replace-regexp (regexp-quote "^") "\\\\^")
      (w3-replace-regexp "<P>" "\\\\par")
      (w3-replace-regexp "<TITLE>\\([^<]*\\)</TITLE>" "\\\\section{\\1}")
      (w3-replace-regexp "<IMG *SRC=\"\\([^\"]*.ps\\)\">"
			 "\\\\psfig{figure=\\1,width=\\\\columnwidth}")
      (w3-replace-regexp "<H1>" "\\\\section{")
      (w3-replace-regexp "<H2>" "\\\\subsection{")
      (w3-replace-regexp "<H3>" "\\\\subsubsection{")
      (w3-replace-regexp "<H4>" "\\\\subsubsection{")
      (w3-replace-regexp "<H5>" "\\\\paragraph{")
      (w3-replace-regexp "<H6>" "\\\\subparagraph{")
      (w3-replace-regexp "</H[0-9]*>" "}")
      (w3-replace-regexp "<\\(UL\\|DIR\\|MENU\\)>" "\\\\begin{itemize}")
      (w3-replace-regexp "</\\(UL\\|DIR\\|MENU\\)>" "\\\\end{itemize}")
      (w3-replace-regexp "<OL>" "\\\\begin{enumerate}")
      (w3-replace-regexp "</OL>" "\\\\end{enumerate}")
      (w3-replace-regexp "<DL>" "\\\\begin{description}")
      (w3-replace-regexp "</DL>" "\\\\end{description}")
      (w3-replace-regexp "<DT>\\([^<]*$\\)" "\\\\item[\\1]")
      (w3-replace-regexp "<DD>" "")
      (w3-replace-regexp "<A[ \t\n]+[^>]*>" "")   ;; get rid of anchors
      (w3-replace-regexp "</A>" "")
      (w3-replace-regexp
       "<\\(EM\\|B\\|STRONG\\|DFN\\)>\\([^<]*\\)</\\(EM\\|B\\|STRONG\\|DFN\\)>"
       "{\\\\bf \\2}")
      (w3-replace-regexp
       "<\\(CODE\\|SAMP\\|TT\\|KBD\\|VAR\\)>\\([^<]*\\)</\\(CODE\\|SAMP\\|TT\\|KBD\\|VAR\\)>"
       "{\\\\tt \\2}")
      (w3-replace-regexp
       "<\\(CITE\\|U\\)>\\([^<]*\\)</\\(CITE\\|U\\)>" "{\\\\underline \\2}")
      (w3-replace-regexp
       "<\\(I\\|ADDRESS\\)>\\([^<]*\\)</\\(I\\|ADDRESS\\)>" "{\\\\it \\2}")
      (w3-replace-regexp "<IMG[^>]*>" "")
      (w3-replace-regexp (regexp-quote "&lt;") "$<$")
      (w3-replace-regexp (regexp-quote "&amp;") " and ")
      (w3-replace-regexp "<[^>]*>" ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to pass files off to external viewers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-start-viewer (fname cmd &optional view)
  "Start a subprocess, named FNAME, executing CMD
If third arg VIEW is non-nil, show the output in a buffer when
the subprocess exits."
  (if view (save-excursion
	     (set-buffer (get-buffer-create view))
	     (erase-buffer)))
  (let ((proc
	 (start-process fname view (or (getenv "ESHELL")
				       (getenv "SHELL")
				       "/bin/sh") "-c" cmd)))
    proc))

(defun w3-pass-to-viewer ()
  "Pass a w3 buffer to a viewer based on file extension."
  (set-buffer w3-working-buffer)
  (let* ((view w3-current-mime-viewer))
    (if (null view)
	(setq view 'indented-text-mode))
    (cond
     ((symbolp view)
      (if (not (memq view '(w3-prepare-buffer w3-print w3-source)))
	  (progn
	    (rename-buffer (w3-generate-new-buffer-name
			    (file-name-nondirectory w3-current-file)))
	    (set-buffer-modified-p nil)
	    (if w3-mutable-windows
		(pop-to-buffer (file-name-nondirectory w3-current-file))
	      (switch-to-buffer (file-name-nondirectory w3-current-file)))
	    (buffer-enable-undo)
	    (funcall view))
	(funcall view)))
     ((stringp view)
      (let ((fname (w3-generate-unique-filename))
	    (show (cond
		   ((null w3-always-show-output) nil)
		   ((eq w3-always-show-output t) t)
		   (t (funcall w3-confirmation-func
			       "View process output?")))))
	(if (w3-file-directly-accessible-p (w3-view-url t))
	    (make-symbolic-link w3-current-file fname t)
	  (if (boundp 'MULE)
	      (write-region (point-min) (point-max) fname nil nil *noconv*)
	    (write-region (point-min) (point-max) fname)))
	(kill-buffer w3-working-buffer)
	(message (concat "Passing to viewer " view) fname)
	(set-process-sentinel
	 (w3-start-viewer fname (format view fname)
			  (if show (w3-generate-new-buffer-name
				    (prin1-to-string (read view)))
			    nil))
	 'w3-viewer-sentinel)))
     ((listp view)
      (set-buffer-modified-p nil)
      (buffer-enable-undo)
      (eval view))
     (t
      (message "Unknown viewer specified: %S" view)
      (switch-to-buffer w3-working-buffer)))))

(defun w3-save-binary-file ()
  (interactive)
  (let ((x (read-file-name "Filename to save as: "
			   (expand-file-name "~/") "")))
    (save-excursion
      (if (boundp 'MULE)
	  (write-region (point-min) (point-max) x nil nil *noconv*)
	(write-region (point-min) (point-max) x))
      (kill-buffer (current-buffer)))))

(defun w3-viewer-sentinel (proc string)
  "Delete any temp files left from a viewer process."
  (let ((fname (process-name proc))
	(buffr (process-buffer proc)))
    (if (and (file-exists-p fname)
	     (file-writable-p fname))
	(delete-file fname))
    (if buffr
	(if w3-mutable-windows
	    (pop-to-buffer buffr)
	  (switch-to-buffer buffr)))))

(defun w3-generate-new-buffer-name (start)
  "Create a new buffer name based on START."
  (let ((x 1)
	name)
    (if (not (get-buffer start))
	start
      (progn
	(setq name (format "%s<%d>" start x))
	(while (get-buffer name)
	  (setq x (1+ x)
		name (format "%s<%d>" start x)))
	name))))

(defun w3-generate-unique-filename ()
  "Generate a unique filename in w3-temporary-directory"
  (let ((base (format "w3-tmp.%d" (user-real-uid)))
	(fname "")
	(x 0))
    (setq fname (format "%s%d" base x))
    (while (file-exists-p (expand-file-name fname w3-temporary-directory))
      (setq x (1+ x)
	    fname (format "%s%d" base x)))
    (expand-file-name fname w3-temporary-directory)))

(defun w3-extension-to-mime (extn)
  "This will convert a file extensions (EXTN) to a mime-type, using
the variable w3-mime-extensions"
  (cdr (assoc (downcase extn) w3-mime-extensions)))


(defvar w3-lazy-message-time 0)

(defun w3-lazy-message-1 (&rest args)
  "Just like `message', but is a no-op if called more than once a second.
Will not do anything if w3-show-status is nil."
  (if (or (null w3-show-status)
	  (= w3-lazy-message-time
	     (setq w3-lazy-message-time (nth 1 (current-time)))))
      nil
    (apply 'message args)))

(defun w3-lazy-message-2 (&rest args)
  "Just like `message', but will not do anything if w3-show-transfer-status
is nil."
  (if w3-show-status
      (apply 'message args)
    nil))

(if (fboundp 'current-time)
    (fset 'w3-lazy-message 'w3-lazy-message-1)
  (fset 'w3-lazy-message 'w3-lazy-message-2))

(defun w3-build-url (protocol)
  "Build a url for PROTOCOL, return it as a string"
  (interactive (list (cdr (assoc (completing-read
				  "Protocol: "
				  w3-acceptable-protocols-alist nil t)
				 w3-acceptable-protocols-alist))))
  (let (user host port file)
    (cond
     ((null protocol) (error "Protocol is unknown to me!"))
     ((string= protocol "news")
      (setq host (read-string "Enter news server name, or blank for default: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Newgroup name or Message-ID: ")))
     ((string= protocol "mailto") (setq file (read-string "E-mail address: ")))
     ((string= protocol "http")
      (setq host (read-string "Enter server name: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Remote file: "))
      (and (string= "" port) (setq port nil))
      (and (string= "" host) (error "Must specify a remote machine!")))
     ((string= protocol "file")
      (if (funcall w3-confirmation-func "Local file?")
	  (setq file (read-file-name "Local File: " nil nil t))
	(setq user (read-string "Login as user (blank=anonymous): ")
	      host (read-string "Remote machine name: "))
	(and (string= user "") (setq user "anonymous"))
	(and (string= host "") (error "Must specify a remote machine!"))
	(setq file (read-file-name "File: " (format "/%s@%s:" user host)
				   nil t)
	      file (substring file (length (format "/%s@%s:" user host))))))
     ((or (string= protocol "telnet")
	  (string= protocol "tn3270"))
      (setq user (read-string "Login as user (blank=none): ")
	    host (read-string "Remote machine name: ")
	    port (read-string "Port number (blank=23): "))
      (and (string= "" port) (setq port nil))
      (and (string= "" user) (setq user nil))
      (and (string= "" host) (error "Must specify a host machine!")))
     ((string= protocol "gopher")
      (setq host (read-string "Enter server name: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Remote file: "))
      (and (string= "" port) (setq port nil))
      (and (string= "" host) (error "Must specify a remote machine!"))))
    (message "%s:%s%s"
	     protocol
	     (if (null host) "" (concat "//" host
					(if (null port) "" (concat ":" port))))
	     (if (= ?/ (string-to-char file)) file (concat "/" file)))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for parsing different types of HREF's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-grok-wais-href (url)
  "Return a list of server, port, database, search-term, doc-id"
  (string-match "wais:/+\\([^/:]+\\):*\\([^/]*\\)/+\\(.*\\)" url)
  (let ((host (w3-match url 1))
	(port (w3-match url 2))
	(data (w3-match url 3)))
    (list host port data)))

(defun w3-grok-http-href (url)
  "Return a list of server, port, file, dest from URL"
  (let ((x w3-current-server)
	(y w3-current-port))
    (if (string-match "http:/\\([^/].*\\)" url)	; Weird URL
	(setq url (format "http://%s:%s/%s"
			  x y (substring url
					 (match-beginning 1)
					 (match-end 1)))))
    (if (string-match "http:\\([^/].*\\)" url)	; Another weird URL
	(setq url (w3-parse-relative-link (w3-match url 1))))
    (string-match "http:+/*\\([^:/]*\\):*\\([^/]*\\)/*\\(/.*\\)" url)
    (let* ((server (w3-match url 1))
	   (port   (w3-match url 2))
	   (file   (w3-match url 3))
	   (dest   (if (string-match "#.+$" file)
		       (prog1
			   (substring file (1+ (match-beginning 0))
				      (match-end 0))
			 (setq file (substring file 0 (match-beginning 0))))
		     nil)))
      (if (and (string= server "")
	       (string= port ""))
	  (progn
	    (string-match "/*\\([^:]+\\):*\\([0-9]*\\)" file)
	    (setq server (w3-match file 1)
		  port (w3-match file 2)
		  file "/")))
      (if (string= port "")
	  (setq port "80"))
      (and w3-using-proxy
	   (= ?/ (string-to-char file))
	   (setq file (substring file 1 nil)))
      (list server port file dest))))

(defun w3-grok-file-href (url)
  "Return a list of username, server, file, destination out of URL"
  (let (user server file dest pswd)
    (cond
     ((string-match "//" url)		; Remote file
      (string-match "^\\(file\\|ftp\\):/*\\([^/]*\\)/*\\(/.*\\)" url)
      (setq server (w3-match url 2)
	    file (w3-match url 3)
	    user "anonymous"
	    dest (if (string-match "#.+$" file)
		     (prog1
			 (substring file (1+ (match-beginning 0))
				    (match-end 0))
		       (setq file (substring file 0 (match-beginning 0))))
		   nil))
      (if (string-match "@" server)
	  (setq user (substring server 0 (match-beginning 0))
		server (substring server (1+ (match-beginning 0)) nil)))
      (if (string-match ":" server)
	  (setq server (substring server 0 (match-beginning 0))))
      (if (equal server "localhost")
	  (setq server nil))
      (if (string-match "\\(.*\\):\\(.*\\)" user)
	  (setq user (w3-match user 1)
		pswd (w3-match user 2)))
      (cond
       ((null pswd) nil)
       ((fboundp 'ange-ftp-set-passwd)
	(ange-ftp-set-passwd server user pswd))
       ((fboundp 'efs-set-passwd)
	(efs-set-passwd server user pswd))))
     (t
      (setq dest (if (string-match "#\\(.+\\)$" url)
		     (prog1
			 (w3-match url 1)
		       (setq url (substring url 0 (match-beginning 0))))
		   nil)
	    file url)
      (if (string-match "file:\\(.*\\)" file)
	  (setq file (w3-match file 1)))))
    (setq file (expand-file-name file (w3-basepath w3-current-file)))
    (list user server file dest)))

(defun w3-grok-news-href (url)
  "Parse out a news url"
  (string-match "news:/*\\([^/:]*\\):*\\([0-9]*\\)/*\\(.*\\)" url)
  (let (
	(host (substring url (match-beginning 1) (match-end 1)))
	(port (substring url (match-beginning 2) (match-end 2)))
	(art  (substring url (match-beginning 3) (match-end 3))))
    (if (equal port "") (setq port "119"))
    (if (equal host "") (setq host w3-news-server))
    (if (equal art "") (setq art host
			     host w3-news-server))
    (list host port art)))

(defun w3-grok-gopher-href (url)
  "Return a list of attributes from a gopher url.  List is of the
type: host port selector-string MIME-type extra-info"
  (let (host				; host name
	port				; Port #
	selector			; String to send to gopher host
	type				; MIME type
	extra				; Extra information
	x				; Temporary storage for host/port
	y				; Temporary storage for selector
	)
    (or (string-match "gopher:/*\\([^/]+\\)/*\\(.*\\)" url)
	(error "Can't understand url %s" url))
    (setq x (w3-match url 1)		; The host (and possible port #)
	  y (w3-unhex-string
	     (w3-match url 2)))		; The selector (and possible type)

    ;First take care of the host/port/gopher+ information from the url
    ;A + after the port # (host:70+) specifies a gopher+ link
    ;A ? after the port # (host:70?) specifies a gopher+ ask block
    (if (string-match "^\\([^:]+\\):\\([0-9]+\\)\\([?+]*\\)" x)
	(setq host (w3-match x 1)
	      port (w3-match x 2)
	      extra (w3-match x 3))
      (setq host x
	    port "70"
	    extra nil))
    (cond
     ((equal extra "")  (setq extra nil))
     ((equal extra "?") (setq extra 'ask-block))
     ((equal extra "+") (setq extra 'gopher+)))

    ; Next, get the type/get rid of the Mosaic double-typing. Argh.
    (setq x (string-to-char y)		; Get gopher type
	  selector (if (or w3-use-hypertext-gopher
			   (< 3 (length y)))
		       y		; Get the selector string
		     (substring y 1 nil))
	  type (cdr (assoc x w3-gopher-to-mime)))
    
    (list host port (or selector "") type extra)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous file-type operations for URLs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-buffer-is-hypertext (&optional buff)
  "Return t if a buffer contains HTML, as near as we can guess."
  (setq buff (or buff (current-buffer)))
  (save-excursion
    (set-buffer buff)
    (goto-char (point-min))
    (re-search-forward
     "<\\(TITLE\\|HEAD\\|BASE\\|H[0-9]\\|ISINDEX\\|P\\)>" nil t)))

(defun w3-have-visited-url (url &rest args)
  "Return non-nil iff the user has visited URL before.
The return value is a cons of the url and the date last accessed as a string"
  (assoc url w3-global-history-completion-list))

(defun w3-directory-files (url &rest args)
  "Return a list of files on a server."
  nil)

(defun w3-file-writable-p (url &rest args)
  "Return t iff a url is writable by this user"
  nil)

(defun w3-copy-file (url &rest args)
  "Copy a url to the specified filename."
  nil)

(defun w3-file-directly-accessible-p (url)
  "Returns t iff the specified URL is directly accessible
on your filesystem.  (nfs, local file, etc)."
  (let ((type (and (string-match w3-nonrelative-link url)
		   (w3-match url 1))))
    (cond
     ((null type) nil)
     ((or (equal type "file")
	  (equal type "ftp"))
      (setq type (w3-grok-file-href url))
      (if (nth 1 type) nil t))
     (t nil))))

(defun w3-file-attributes (url &rest args)
  "Return a list of attributes of URL.
Value is nil if specified file cannot be opened.
Otherwise, list elements are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid.
 3. File gid.
 4. Last access time, as a list of two integers.
  First integer has high-order 16 bits of time, second has low 16 bits.
 5. Last modification time, likewise.
 6. Last status change time, likewise.
 7. Size in bytes. (-1, if number is out of range).
 8. File modes, as a string of ten letters or dashes as in ls -l.
    If URL is on an http server, this will return the content-type if possible.
 9. t iff file's gid would change if file were deleted and recreated.
10. inode number.
11. Device number.

If file does not exist, returns nil."
  (and url
       (let ((type (and (string-match "^\\([^:]+\\):/" url)
			(downcase (w3-match url 1))))
	     (data nil) (exists nil))
	 (cond
	  ((equal type "http")
	   (setq data (w3-grok-http-href url))
	   (cond
	    ((or (not w3-be-anal-about-file-attributes)
		 (w3-member (nth 0 data) w3-bad-server-list))
	     (setq data (list
			 (w3-file-directory-p url) ; Directory
			 1		; number of links to it
			 0		; UID
			 0		; GID
			 (cons 0 0)	; Last access time
			 (cons 0 0)	; Last mod. time
			 (cons 0 0)	; Last status time
			 -1		; file size
			 (w3-extension-to-mime
			  (w3-file-extension (nth 2 data)))
			 nil		; gid would change
			 0		; inode number
			 0		; device number
			 )))
	    (t				; HTTP/1.0, use HEAD
	     (let ((w3-request-method "HEAD")
		   (w3-request-data nil)
		   (w3-working-buffer " *w3-temp*"))
	       (save-excursion
		 (w3-retrieve url)
		 (setq data (and (setq exists (cdr (assoc "status"
							  w3-current-mime-headers)))
				 (>= exists 200)
				 (< exists 300)
				 (list
				  (w3-file-directory-p url) ; Directory
				  1	; links to
				  0	; UID
				  0	; GID
				  (cons 0 0) ; Last access time
				  (cons 0 0) ; Last mod. time
				  (cons 0 0) ; Last status time
				  (or	; Size in bytes
				   (cdr (assoc "content-length"
					       w3-current-mime-headers))
				   -1)
				  (or
				   (cdr (assoc "content-type"
					       w3-current-mime-headers))
				   nil)	; content-type
				  nil	; gid would change
				  0	; inode number
				  0	; device number
				  )))
		 (and (not data)
		      (setq data (list (w3-file-directory-p url)
				       1 0 0 (cons 0 0) (cons 0 0) (cons 0 0)
				       -1 (w3-extension-to-mime w3-current-file)
				       nil 0 0)))
		 (kill-buffer " *w3-temp*"))))))
	  ((or (equal type "ftp")	; file-attributes
	       (equal type "file"))
	   (let ((href (w3-grok-file-href url)))
	     (if (nth 1 href)		; remote file
		 (setq data (file-attributes (concat (nth 0 href) "@"
						     (nth 1 href) ":"
						     (nth 2 href))))
	       (setq data (file-attributes (nth 2 href))))
	     (setq data (or data (make-list 12 nil)))
	     (setcar (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr data))))))))
		     (w3-extension-to-mime
		      (w3-file-extension (nth 2 href))))))
	  (t nil))
	 data)))

(defun w3-file-name-all-completions (file dirname &rest args)
  "Return a list of all completions of file name FILE in directory DIR.
These are all file names in directory DIR which begin with FILE."
  nil)

(defun w3-file-name-completion (file dirname &rest args)
  "Complete file name FILE in directory DIR.
Returns the longest string
common to all filenames in DIR that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIR contains no name starting with FILE."
  nil)

(defun w3-file-local-copy (file &rest args)
  "Copy the file FILE into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  nil)

(defun w3-insert-file-contents (url &rest args)
  "Insert the contents of the URL in this buffer."
  (save-excursion
    (w3-retrieve url))
  (insert-buffer w3-working-buffer)
  (kill-buffer w3-working-buffer))

(defun w3-file-directory-p (url &rest args)
  "Return t iff a url points to a directory"
  (equal (substring url -1 nil) "/"))

(defun w3-file-exists (url &rest args)
  "Return t iff a file exists."
  (string-match "^\\([^:]+\\):/" url)
  (let ((type (downcase (w3-match url 1)))
	(exists nil))
    (cond
     ((equal type "http")		; use head
      (let ((w3-request-method "HEAD")
	    (w3-request-data nil)
	    (w3-working-buffer " *w3-temp*"))
	(save-excursion
	  (w3-retrieve url)
	  (setq exists (or (cdr (assoc "status" w3-current-mime-headers)) 500))
	  (kill-buffer " *w3-temp*")
	  (setq exists (and (>= exists 200) (< exists 300))))))
     ((or (equal type "ftp")		; file-attributes
	  (equal type "file"))
      (setq exists (w3-grok-file-href url))
      (if (nth 1 exists)			; remote file
	  (setq exists (file-exists-p (concat (nth 0 exists) "@"
					      (nth 1 exists) ":"
					      (nth 2 exists))))
	(setq exists (file-exists-p (nth 2 exists)))))
     (t nil))
    exists))

(defun w3-buffer-visiting (url)
  "Return the name of a buffer (if any) that is visiting URL."
  (let ((bufs (buffer-list))
	(found nil))
    (while (and bufs (not found))
      (save-excursion
	(set-buffer (car bufs))
	(setq found (if (and
			 (not (equal (buffer-name (car bufs))
				     w3-working-buffer))
			 (eq major-mode 'w3-mode)
			 (equal (w3-view-url t) url)) (car bufs) nil)
	      bufs (cdr bufs))))
    found))

(defun w3-file-size (url &rest args)
  "Return the size of a file in bytes, or -1 if can't be determined."
  (string-match "^\\([^:]+\\):/" url)
  (let ((type (downcase (w3-match url 1)))
	(size -1)
	(data nil))
    (cond
     ((equal type "http")		; use head
      (let ((w3-request-method "HEAD")
	    (w3-request-data nil)
	    (w3-working-buffer " *w3-temp*"))
	(save-excursion
	  (w3-retrieve url)
	  (setq size (or (cdr (assoc "content-length" w3-current-mime-headers))
			 -1))
	  (kill-buffer " *w3-temp*"))))
     ((or (equal type "ftp")		; file-attributes
	  (equal type "file"))
      (setq data (w3-grok-file-href url))
      (if (nth 1 data)			; remote file
	  (setq data (file-attributes (concat (nth 0 data) "@"
					      (nth 1 data) ":"
					      (nth 2 data))))
	(setq data (file-attributes (nth 2 data))))
      (setq size (nth 7 data)))
     (t nil))
    (cond
     ((stringp size) (string-to-int size))
     ((integerp size) size)
     ((null size) -1)
     (t -1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for parsing/updating the user's .newsrc file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-newsrc (&optional newsrc-file)
  "Parse out a newsrc.  This was largely yanked out of gnus"
  (save-excursion
    (setq newsrc-file (or newsrc-file (expand-file-name
				       (concat "~/.newsrc" w3-news-server))))
    (if (and (file-exists-p newsrc-file)
	     (file-readable-p newsrc-file))
	(message "Using newsrc file %s... " newsrc-file)
      (setq newsrc-file (expand-file-name "~/.newsrc")))
    (or (file-exists-p newsrc-file)
	(file-readable-p newsrc-file)
	(error "%s could not be read." newsrc-file))
    (set-buffer (get-buffer-create " *newsrc*"))
    (erase-buffer)
    (insert-file-contents newsrc-file)
    (w3-replace-regexp "^[ \\\t]options.*\\\n" "")
    (let ((subscribe nil)
	  (read-list nil)
	  newsgroup
	  p p2)
      (save-restriction
	(while (not (eobp))
	  (cond
	   ((= (following-char) ?\n)
	    ;; skip blank lines
	    nil)
	   (t
	    (setq p (point))
	    (skip-chars-forward "^:!\n")
	    (if (= (following-char) ?\n)
		(error "unparsable line in %s" (buffer-name)))
	    (setq p2 (point))
	    (skip-chars-backward " \t")

	    ;; #### note: we could avoid consing a string here by
	    ;; binding obarray and reading the newsgroup directly into
	    ;; the gnus-newsrc-hashtb, then setq'ing newsgroup to
	    ;; symbol-name of that, like we do in
	    ;; gnus-active-to-gnus-format.

	    (setq newsgroup (read (buffer-substring p (point))))
	    (goto-char p2)

	    (setq subscribe (= (following-char) ?:))
	    (setq read-list nil)

	    (forward-char 1)		; after : or !
	    (skip-chars-forward " \t")
	    (while (not (= (following-char) ?\n))
	      (skip-chars-forward " \t")
	      (or
	       (and (cond
		     ((looking-at "\\([0-9]+\\)-\\([0-9]+\\)") ; a range
		      (setq read-list
			    (cons
			     (cons
			      (progn
				;; faster that buffer-substring/string-to-int
				(narrow-to-region (point-min) (match-end 1))
				(read (current-buffer)))
			      (progn
				(narrow-to-region (point-min) (match-end 2))
				(forward-char) ; skip over "-"
				(prog1
				    (read (current-buffer))
				  (widen))))
			     read-list))
		      t)
		     ((looking-at "[0-9]+")
		      ;; faster that buffer-substring/string-to-int
		      (narrow-to-region (point-min) (match-end 0))
		      (setq p (read (current-buffer)))
		      (widen)
		      (setq read-list (cons (cons p p) read-list))
		      t)
		     (t
		      ;; bogus chars in ranges
		      nil))
		    (progn
		      (goto-char (match-end 0))
		      (skip-chars-forward " \t")
		      (cond ((= (following-char) ?,)
			     (forward-char 1)
			     t)
			    ((= (following-char) ?\n)
			     t)
			    (t
			     ;; bogus char after range
			     nil))))
	       ;; if we get here, the parse failed
	       (progn
		 (end-of-line)		; give up on this line
		 (ding)
		 (message "Ignoring bogus line for %s in %s"
			  newsgroup (buffer-name))
		 (sleep-for 1)
		 )))
	    (put 'w3-newsrc newsgroup (cons subscribe (nreverse read-list)))))
	  (forward-line 1))))
    (kill-buffer (current-buffer))
    (put 'w3-newsrc 'parsed t)))

(defun w3-save-newsrc (&optional fname)
  "Save the newsrc of the user"
  (set-buffer (get-buffer-create " *newsrc*"))
  (erase-buffer)
  (insert-file-contents (or fname (expand-file-name "~/.newsrc")))
  (goto-char (point-min))
  (delete-non-matching-lines "^[ \\\t]options")	; preserve option lines
  (goto-char (point-max))
  (let ((grps (symbol-plist 'w3-newsrc)) grp info)
    (while grps
      (setq grp (car grps)
	    info (car (cdr grps))
	    grps (cdr (cdr grps)))
      (if (eq grp 'parsed)
	  nil
	(insert (symbol-name grp) (if (car info) ": " "! ")
		(mapconcat
		 (function
		  (lambda (range)
		    (cond
		     ((consp range) (concat (car range) "-" (cdr range)))
		     ((numberp range) range)))) (cdr info) ",") "\n")))))
		     
(defun w3-retrieve-newsgroup (group &optional show-all howmany)
  "Select newsgroup NEWSGROUP and return a list of headers of the remaining
articles"
  (or (get 'w3-newsrc 'parsed) (w3-parse-newsrc))
  (if (symbolp group) (setq group (symbol-name group)))
  (let ((stat
	 (cond
	  ((string-match "flee" nntp-version)
	   (nntp/command "GROUP" group)
	   (save-excursion
	     (set-buffer nntp-server-buffer)
	     (while (progn
		      (goto-char (point-min))
		      (not (re-search-forward
			    "[0-9]+[ \\\t]+[0-9]+[ \\\t]+\\([0-9]+\\)[ \\\t]+\\([0-9]+\\)" nil t)))
	       (w3-accept-process-output nntp/connection))
	     (cons (string-to-int
		    (buffer-substring (match-beginning 1) (match-end 1)))
		   (string-to-int
		    (buffer-substring (match-beginning 2) (match-end 2))))))
	  (t
	   (nntp-request-group group)
	   (let ((msg (nntp-status-message)))
	     (string-match "[0-9]+[ \\\t]+\\([0-9]+\\)[ \\\t]+\\([0-9]+\\)"
			   msg)
	     (cons (string-to-int (w3-match msg 1))
		   (string-to-int (w3-match msg 2)))))))
	(info (cdr (get 'w3-newsrc (read group))))
	(seqs '())
	(temp nil)
	(last nil)			; last unread article
	)
    (setq last (car stat))
    (w3-lazy-message "Finding unread articles...")
    (if show-all
	(setq seqs (w3-make-sequence (car stat) (cdr stat)))
      (while info
	(setq temp (car info)
	      info (cdr info))
	(cond
	 ((consp temp)			; a range of articles
	  (setq seqs (nconc seqs (w3-make-sequence last (1- (car temp))))
		last (1+ (cdr temp))))
	 ((numberp temp)
	  (setq seqs (nconc seqs (w3-make-sequence last (1- temp)))
		last (1+ temp))))))
    (setq seqs (nconc seqs (w3-make-sequence last (cdr stat))))
    (if howmany (length seqs)
      (nntp-retrieve-headers seqs))))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the different types of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-wais (url)
  "Retrieve a document via WAIS"
  (if (and w3-wais-gateway-server w3-wais-gateway-port)
      (w3-fetch
       (format "http://%s:%s/%s"
	       w3-wais-gateway-server
	       w3-wais-gateway-port
	       (substring url (match-end 0) nil)))
    (let ((href (w3-grok-wais-href url))
	  (lb (current-buffer)))
      (w3-clear-tmp-buffer)
      (setq w3-current-type "wais"
	    w3-current-server (nth 0 href)
	    w3-current-port (nth 1 href)
	    w3-current-file (nth 2 href)
	    w3-current-last-buffer lb)
      (cond
       ((string-match "\\([^/]+\\)/.*3=\\([^ ;]+\\)" (nth 2 href)); full link
	(w3-retrieve-wais-docid (nth 0 href) (nth 1 href)
				(w3-match (nth 2 href) 1)
				(w3-match (nth 2 href) 2)))
       ((string-match "\\([^\\?]+\\)\\?\\(.*\\)" (nth 2 href)) ; stored query
	(w3-perform-wais-query (nth 0 href) (nth 1 href)
			       (w3-match (nth 2 href) 1)
			       (w3-match (nth 2 href) 2)))
       (t
	(insert "<title>WAIS search</title>\n"
		"<h1>WAIS search of " (nth 2 href) "</h1>"
		"<hr>\n"
		"<form>\n"
		"Enter search term: <input name=\"internal-wais\">\n"
		"</form>\n"
		"<hr>\n"))))))
	  
(and (boundp 'after-change-functions)
     (make-variable-buffer-local 'after-change-functions))

(defun w3-after-change-function (&rest args)
  "The nitty gritty details of messaging the HTTP/1.0 status messages
in the minibuffer."
  (save-excursion
    (set-buffer w3-working-buffer)
    (let (status-message)
      (if w3-current-content-length
	  nil
	(goto-char (point-min))
	(skip-chars-forward " \\\t\\\n")
	(if (not (looking-at "HTTP/[0-9]\.[0-9]"))
	    (setq w3-current-content-length 0)
	  (setq w3-current-isindex
		(and (re-search-forward "$\r*$" nil t) (point)))
	  (if (re-search-forward
	       "^content-type:[ \\\t]*\\([^\\\r\\\n]+\\)\\\r*$"
	       w3-current-isindex t)
	      (setq w3-current-mime-type (downcase
					  (w3-eat-trailing-space
					   (buffer-substring
					    (match-beginning 1)
					    (match-end 1))))))
	  (if (re-search-forward "^content-length:\\([^\\\r\\\n]+\\)\\\r*$"
				 w3-current-isindex t)
	      (setq w3-current-content-length
		    (string-to-int (buffer-substring (match-beginning 1)
						     (match-end 1))))
	    (setq w3-current-content-length nil))))
      (goto-char (point-min))
      (if (re-search-forward "^status:\\([^\\\r]*\\)" w3-current-isindex t)
	  (progn
	    (setq status-message (buffer-substring (match-beginning 1)
						   (match-end 1)))
	    (replace-match (concat "btatus:" status-message))))
      (goto-char (point-max))
      (cond
       (status-message (w3-lazy-message (w3-quotify-percents status-message)))
       ((and w3-current-content-length (> w3-current-content-length 1)
	     w3-current-mime-type)
	(w3-lazy-message "Read %d of %d bytes (%d%%) [%s]"
			 (point-max) w3-current-content-length
			 (/ (* (point-max) 100) w3-current-content-length)
			 w3-current-mime-type))
       ((and w3-current-content-length (> w3-current-content-length 1))
	(w3-lazy-message "Read %d of %d bytes (%d%%)"
			 (point-max) w3-current-content-length
			 (/ (* (point-max) 100) w3-current-content-length)))
       ((and (/= 1 (point-max)) w3-current-mime-type)
	(w3-lazy-message "Read %d bytes. [%s]" (point-max)
			 w3-current-mime-type))
       ((/= 1 (point-max))
	(w3-lazy-message "Read %d bytes." (point-max)))
       (t (w3-lazy-message "Waiting for response."))))))

(defun w3-http (url)
  "Retrieve URL via http.  If SOURCE is non-nil, then don't parse the buffer."
  (let ((lb (current-buffer))
	(href (w3-grok-http-href url)))
    (w3-clear-tmp-buffer)
    (setq w3-current-type "http"
	  w3-current-last-buffer lb)
    (let* ((server (nth 0 href))
	   (port   (nth 1 href))
	   (file   (nth 2 href))
	   (dest   (nth 3 href)))
      (if (or (not (w3-member port w3-bad-port-list))
	      (funcall w3-confirmation-func
		       (format
			"Warning!  Trying to connect to port %s - continue? "
			port)))
	  (progn
	    (if (equal port "") (setq port "80"))
	    (if (equal file "") (setq file "/") )
	    (setq w3-current-server server
		  w3-current-port port
		  w3-current-file file
		  w3-find-this-link dest)
	    (w3-lazy-message "Fetching: %s %s %s" server port file)
	    (let ((process
		   (w3-open-stream "WWW" w3-working-buffer server
				   (string-to-int port))))
	      (if (stringp process)
		  (progn
		    (set-buffer w3-working-buffer)
		    (erase-buffer)
		    (setq w3-current-mime-type "text/html"
			  w3-current-mime-viewer (w3-mime-viewer "text/html"))
		    (w3-insert "<TITLE>ERROR</TITLE>\n"
			    "<H1>ERROR - Could not establish connection</H1>"
			    "<P>"
			    "The browser could not establish a connection "
			    (format "to %s:%s.<P>" server port)
			    "The server is either down, or the URL"
			    (format "(%s) is malformed.<P>" (w3-view-url t)))
		    (message process))
		(progn
		  (process-kill-without-query process)
		  (process-send-string
		   process
		   (w3-create-mime-request file (w3-view-url t)))
		  (if (and w3-show-http2-transfer
			   (boundp 'after-change-functions))
		      (add-hook 'after-change-functions
				'w3-after-change-function))
		  (if w3-be-asynchronous
		      (progn
			(set-process-sentinel process 'w3-sentinel)
			(if (eq w3-gateway-method 'host)
			    (set-process-filter process 'w3-filter)))
		    (save-excursion
		      (set-buffer w3-working-buffer)
		      (while (memq (w3-process-status process) '(run open))
			(if (boundp 'after-change-functions)
			    nil
			  (w3-after-change-function nil))
			(w3-accept-process-output process))
		      (condition-case ()
			  (w3-kill-process process)
			(error nil))))
		  (if (boundp 'after-change-functions)
		      (remove-hook 'after-change-functions
				   'w3-after-change-function))))))
	(progn
	  (ding)
	  (message "Aborting connection to bad port..."))))))

(defun w3-file (url)
  "Find a link to an ftp site - simple transformation to ange-ftp format"
  (let* ((href (w3-grok-file-href url))
	 (user (nth 0 href))
	 (site (nth 1 href))
	 (file (nth 2 href))
	 (dest (nth 3 href))
	 (obuf (current-buffer)))
    (w3-clear-tmp-buffer)
    (setq w3-current-last-buffer obuf)
    (cond
     (site
      (let ((filename (concat "/" user "@" site ":" file)))
	(cond
	 ((file-directory-p filename)
	  (if (eq w3-directory-format 'hypertext)
	      (progn
		(setq w3-current-type "ftp"
		      w3-find-this-link dest
		      w3-current-last-buffer obuf
		      w3-current-user user
		      w3-current-server site
		      w3-current-file (format
				       "%s%s" file
				       (if (equal "/"
						  (substring file -1 nil))
					   "" "/")))
		(w3-format-directory filename))
	    (progn
	      (if (get-buffer w3-working-buffer)
		  (kill-buffer w3-working-buffer))
	      (find-file filename))))
	 (t
	  (set-buffer (get-buffer-create w3-working-buffer))
	  (setq w3-current-type "ftp"
		w3-current-user user
		w3-current-server site
		w3-current-file file)
	  (condition-case ()
	      (insert-file-contents filename nil)
	    (error (w3-retrieve (concat "www://error/nofile/" filename))))))))
     (t
      (setq file (expand-file-name (if (string-match "file:" file)
				       (substring file (match-end 0) nil)
				     file))
	    w3-current-type nil
	    w3-current-last-buffer obuf
	    w3-find-this-link dest
	    w3-current-file file)
      (cond
       ((file-directory-p file)
	(cond
	 ((eq w3-directory-format 'hypertext)
	  (w3-format-directory file)
	  (setq w3-current-file (format
				 "%s%s" file
				 (if (equal "/"
					    (substring file -1 nil))
				     "" "/"))))
	 (t
	  (if (get-buffer w3-working-buffer)
	      (kill-buffer w3-working-buffer))
	  (find-file file))))
       (t
	(let ((viewer (w3-mime-viewer
		       (w3-extension-to-mime (w3-file-extension file)))))
	  (cond
	   ((or w3-source		; Need it in a buffer
		(symbolp viewer)
		(listp viewer))
	    (condition-case ()
		(insert-file-contents file)
	      (error (w3-retrieve (concat "www://error/nofile/" file)))))
	   (t
	    nil)))))))))

(defun w3-get-new-newsgroups (&optional tm)
  "Get a string suitable for an NTTP server to get a list of new newsgroups.
Optional argument TM is a list of three integers. The first has the
most significant 16 bits of the seconds, while the second has the
least significant 16 bits.  The third integer gives the microsecond
count.  (The format returned either by (current-time) or file-attributes
mod-time, etc.)"
  (let* ((x (current-time-string tm))
	 (y (cdr (assoc (substring x 4 7) monthabbrev-alist)))
	 (z (substring x 9 10)))
    (concat "NEWGROUPS "
	    (substring x -2 nil)
	    (if (< y 10) "0" "")
	    y
	    (if (= (length z) 2) "" "0")
	    z " "
	    (substring x 11 13)
	    (substring x 14 16)
	    (substring x 17 19))))
	  
(defun w3-news-get-header (header)
  "Get header information HEADER out of news article in nntp buffer"
  (save-excursion
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (if (re-search-forward (concat "^" header ": +\\(.*\\)") nil t)
	(buffer-substring (match-beginning 1) (match-end 1))
      "")))

(defun w3-news-get-body ()
  "Get body of article from the nntp buffer"
  (save-excursion
    (set-buffer nntp-server-buffer)
    (goto-char (point-min))
    (re-search-forward "\\\n\\\n")
    (buffer-substring (match-end 0) (point-max))))

(defun w3-format-news ()
  "Format a news buffer in html"
  (let ((from  (w3-fix-entities-in-string (w3-news-get-header "From")))
	(subj  (w3-fix-entities-in-string (w3-news-get-header "Subject")))
	(org   (w3-fix-entities-in-string (w3-news-get-header "Organization")))
	(grps  (mapcar 'car (w3-split (w3-news-get-header "Newsgroups")
				      "[ \\\t\\\n]+")))
	(refs  (mapcar 'car (w3-split (w3-news-get-header "References")
				      "[ \\\t\\\n<>]+")))
	(date  (w3-fix-entities-in-string (w3-news-get-header "Date")))
	(body  (w3-news-get-body)))
    (w3-clear-tmp-buffer)
    (setq w3-current-file ""
	  w3-current-type "")
    (insert "<htmlplus>\n"
	    " <head>\n"
	    "  <title>" subj "</title>\n"
	    "  <link rev=\"made\" href=\"mailto:" from "\">\n"
	    " </head>\n"
	    " <body>\n"
	    "  <div1>\n"
	    "   <h1>" subj "</h1>\n"
	    "   <p role=\"headers\">\n"
	    "    <b>From</b>: <address> " from "</address><br>\n"
	    "    <b>Newsgroups</b>: "
	    (mapconcat
	     (function
	      (lambda (grp)
		(concat "<a href=\"" grp "\"> " grp "</a>"))) grps ", ")
	    "<br>\n"
	    "    <b>Organization</b>: <i> " org "</i> <br>\n"
	    "    <b>Date</b>: <date> " date "</date> <br>\n"
	    "   </p> <hr>\n"
	    "   <p align=\"center\">References\n"
	    "    <ol>\n"
	    (mapconcat
	     (function
	      (lambda (ref)
		(concat "     <li> <a href=\"" ref "\"> " ref "</a></li>\n")))
	     refs "")
	    "    </ol>\n"
	    "   <hr>\n"
	    "   <ul plain>\n"
	    "    <li><a href=\"newspost:disfunctional\"> "
	    "Post to this group </a></li>\n"
	    "    <li><a href=\"mailto:" from "\"> Reply to " from "</a></li>\n"
	    "   <hr>"
	    "   <pre>\n"
	    body
	    "   </pre>\n"
	    "  </div1>\n"
	    " </body>\n"
	    "</htmlplus>\n"
	    "<!-- Automatically generated by Emacs-W3/" w3-version-number
	    "-->")))

(defun w3-format-whole-newsgroup (newsgroup header-list)
  (w3-clear-tmp-buffer)
  (insert "<htmlplus>\n"
	  " <head>\n"
	  "  <title>" newsgroup "</title>\n"
	  " </head>\n"
	  " <body>\n"
	  "  <div1>\n"
	  "   <h1>" newsgroup "</h1>\n"
	  "   <ol>\n"
	  (mapconcat
	   (function
	    (lambda (artcl)
	      (let ((id (nntp-header-id artcl))
		    (subj (w3-fix-entities-in-string (nntp-header-subject artcl)))
		    (from (w3-fix-entities-in-string (nntp-header-from artcl))))
		(if (string-match "<\\(.*\\)>" id)
		    (setq id (w3-match id 1)))
		(concat "    <li> <a href=\"" id "\"> " subj "</a> <br>\n"
			"         " from " </li>\n")))) header-list "")
	  "   </ol>\n"
	  "  </div1>\n"
	  " </body>\n"
	  "</htmlplus>\n"
	  "<!-- Automatically generated by Emacs-W3/" w3-version-number
	  "-->"))

(defun w3-show-all-newsgroups ()
  "Show a hypertext list of all newsgroups."
  (or (get 'w3-newsrc 'parsed) (w3-parse-newsrc))
  (let ((grps (symbol-plist 'w3-newsrc))
	grp info)
    (insert "<htmlplus>\n"
	    " <head>\n"
	    "  <title> Newsgroups </title>\n"
	    " </head>\n"
	    " <body>\n"
	    "  <div1>\n"
	    "   <h1> Newsgroup listing </h1>\n"
	    "   <pre>\n")
    (while grps
      (setq grp (symbol-name (car grps))
	    info (car (cdr grps))
	    grps (cdr (cdr grps)))
      (if (eq grp 'parsed)
	  nil
	(insert (format "    <a href=\"%s\">%7d%s %s" grp
			(w3-retrieve-newsgroup grp nil t)
			(if (car info) ": " "! ") grp))))
    (insert "   </pre>\n"
	    "  </div1>\n"
	    " </body>\n"
	    "</htmlplus>\n"
	    "<!-- Automatically generated by Emacs-W3/" w3-version-number
	    "-->")))    

(defun w3-news (article)
  "Find a news reference"
  (or noninteractive (require 'nntp))
  (let ((buff (current-buffer)))
    (set-buffer (get-buffer-create w3-working-buffer))
    (setq w3-current-last-buffer buff)
    (set-buffer buff))
  (let* ((info (w3-grok-news-href article))
	 (host (nth 0 info))
	 (port (nth 1 info))
	 (article (nth 2 info)))
    (if (not (equal w3-current-nntp-server host))
	(nntp-close-server))
    (or (nntp-server-opened) (nntp-open-server host (string-to-int port)))
    (cond
     ((string-match "@" article)	; Its a specific article
      (if (not (equal ">" (substring article -1 nil)));; get in correct
	  (setq article (format "<%s>" article)));; format
      (if (not (nntp-server-opened))
	  (nntp-open-server host (string-to-int port)))
      (if (nntp-request-article article);; did we get it?
	  (w3-format-news);; yes
	(progn
	  (set-buffer w3-working-buffer)
	  (w3-insert "<TITLE>ERROR</TITLE>\n"
		  "<H1> Error requesting article... </H1>"
		  "The status message returned by the NNTP server was:<BR>"
		  (format "<PRE>%s</PRE><P>" (nntp-status-message))
		  "If you feel this is an error, <A HREF=\""
		  "mailto:wmperry@indiana.edu>send me mail</A>."))))
     ((string= article "")		; List all newsgroups
      (w3-show-all-newsgroups))
     (t					; Whole newsgroup
      (w3-format-whole-newsgroup article (w3-retrieve-newsgroup article))))
    (setq w3-current-type "news"
	  w3-current-server host
	  w3-current-port port
	  w3-current-file article)))

(defun w3-news-generate-reply-form (to newsgroups body &rest refs)
  "Generate an HTML reply form."
  (set-buffer (get-buffer-create w3-working-buffer))
  (erase-buffer)
  (insert "<htmlplus>\n"
	  " <head>\n"
	  "  <title>News Post/Reply Form</title>\n"
	  "  <!-- Automatically generated by emacs-w3 -->\n"
	  " </head>\n"
	  " <body>\n"
	  "  <div1>\n"
	  "   <h1>News Post/Reply Form</h1>\n"
	  "   <hr>\n"
	  "   <form method=\"GET\" action=\"news-internal://\">\n"
	  "    <ul>\n"
	  "     <li> Reply by:"
	  "<select name=\"replyby\"><option>Mail<option>News</select></li>\n"
	  "     <li> Email: <input name=\"addr\" default=\"" to "\"></li>\n"
	  "     <li> Newsgroups: <input name=\"newsg\" default=\""
	  newsgroups "\"></li>\n"
	  "     <li> <input type=\"checkbox\" name=\"include\">"
	  "Include/quote article in followup</li>\n"
	  "    </ul>\n"
	  "    <hr>\n"
	  "    <textarea \"name=body\">\n" body "\n</textarea>\n"
	  "    <hr>\n"
	  "    <input type=\"submit\" value=\"Send it\">\n"
	  "    <br>\n"
	  "    <input type=\"reset\"  value=\"Reset to default values\">\n"
	  "   </form>\n"
	  "  </div1>\n"
	  " </body>\n"
	  "</htmlplus>\n"))	    

;;;###autoload
(defun w3-open-local (fname)
  "Find a local file, and interpret it as a hypertext document.
This is part of the emacs World Wide Web browser.  It will prompt for
an existing file or directory, and retrieve it as a hypertext document.
If it is a directory, and w3-directory-format is 'hypertext, then an
HTML directory listing is created on the fly.  Otherwise, dired-mode is
used to visit the buffer."
  (interactive "FLocal file: ")
  (if (not w3-setup-done) (w3-do-setup))
  (w3-fetch (concat "file:" fname)))

(defun w3-format-directory (dir)
  "Format the files in DIR into hypertext"
  (let ((files (directory-files dir)) file
	div attr mod-time size)
    (save-excursion
      (setq div (1- (length files)))
      (set-buffer w3-working-buffer)
      (erase-buffer)
      (w3-insert (format "<TITLE>Index of %s</TITLE>\n" dir))
      (w3-insert (format "<H1> Directory listing of %s</H1>\n<P>" dir))
      (w3-insert "<DL>\n")
      (while files
	(w3-lazy-message "Building directory list... (%d%%)"
		 (/ (* 100 (- div (length files))) div))
	(setq file (if (equal "/" (substring dir -1 nil))
		       (format "%s%s" dir (car files))
		     (format "%s/%s" dir (car files))))
	(setq attr (file-attributes file)
	      mod-time (nth 5 attr)
	      size (nth 7 attr))
	(if (or (equal '(0 0) mod-time) ; Set to null if unknown or
                                        ; untranslateable
		(not (or w3-running-lemacs w3-running-old-lemacs
			 w3-running-FSF19)))
	    (setq mod-time nil)
	  (setq mod-time (current-time-string mod-time)))
	(if (or (equal size 0)
		(equal size -1)
		(null size))
	    (setq size nil) (setq size (int-to-string size)))
	(cond
	 ((equal "." (car files)) nil)
	 ((equal ".." (car files))
	  (w3-insert
	   (format
	    "<DT> &folder;<A HREF=\"%s\">Parent Directory</A>" (car files))))
	 ((stringp (nth 0 attr))	; Symbolic link handling
	  (w3-insert
	   (format
	    "<DT> &symlink;<A HREF=\"%s\">%s -&gt; %s</A>"
	    (car files) (car files) (nth 0 attr))))
	 ((nth 0 attr)			; Directory handling
	  (w3-insert
	   (format
	    "<DT> &folder;<A HREF=\"%s\">%s/</A>"
	    (car files) (car files))))
	 (t				; Normal file handlnig
	  (w3-insert
	   (format
	    "<DT> &unknown.document;<A HREF=\"%s\">%s</A>"
	    (car files) (car files)))))
	(cond
	 ((equal "." (car files)) nil)
	 (t 
	  (if (not (or mod-time size)) nil (w3-insert "\n<DD>"))
	  (if mod-time (w3-insert "Last Mod: " mod-time))
	  (if (and mod-time size) (w3-insert ", "))
	  (if size (w3-insert "Size: " size " bytes"))
	  (w3-insert "\n")))
	(setq files (cdr files)))
      (w3-insert "\n</DL>"))))

(defun w3-telnet (url)
  "Open up a telnet connection"
  (string-match "telnet:/*\\(.*@\\)*\\([^/]*\\)/*" url)
  (let* ((server (substring url (match-beginning 2) (match-end 2)))
	 (name (if (match-beginning 1)
		   (substring url (match-beginning 1) (1- (match-end 1)))
		 nil))
	 (title (format "%s%s" (if name (concat name "@") "") server))
	 (thebuf (string-match ":" server))
	 (port (if thebuf
		   (prog1
		       (substring server (1+ thebuf) nil)
		     (setq server (substring server 0 thebuf))) "23")))
    (cond
     (window-system
      (start-process "htmlsub" nil w3-xterm-command
		     "-title" title
		     "-ut" "-e"
		            (if (and w3-gateway-local-host-regexp
				     (string-match w3-gateway-local-host-regexp
						   server))
				w3-local-telnet-prog
			      w3-remote-telnet-prog) server port)
      (if name (message "Please log in as %s" name)))
     (w3-use-transparent
      (require 'transparent)
      (if name (message "Please log in as %s" name))
      (sit-for 1)
      (transparent-window (get-buffer-create
			   (format "%s%s:%s" (if name (concat name "@") "")
				   server port))
			  (if (and w3-gateway-local-host-regexp
				   (string-match w3-gateway-local-host-regexp
						 server))
			      w3-local-telnet-prog
			    w3-remote-telnet-prog)
			  (list server port) nil
			  "Press any key to return to emacs"))
     (t
      (terminal-emulator
       (get-buffer-create (format "%s%s:%s" (if name (concat name "@") "")
				  server port))
       (if (and w3-gateway-local-host-regexp
		(string-match w3-gateway-local-host-regexp
			      server))
	   w3-local-telnet-prog
	 w3-remote-telnet-prog)
       (list server port))
      (if name (message "Please log in as %s" name))))))

(defun w3-tn3270 (url)
  "Open up a tn3270 connection"
  (string-match "tn3270:/*\\(.*@\\)*\\([^/]*\\)/*" url)
  (let* ((server (substring url (match-beginning 2) (match-end 2)))
	 (name (if (match-beginning 1)
		   (substring url (match-beginning 1) (1- (match-end 1)))
		 nil))
	 (thebuf (string-match ":" server))
	 (title (format "%s%s" (if name (concat name "@") "") server))
	 (port (if thebuf
		   (prog1
		       (substring server (1+ thebuf) nil)
		     (setq server (substring server 0 thebuf))) "23")))
    (cond
     (window-system
      (start-process "htmlsub" nil w3-xterm-command
		     "-title" title
		     "-ut" "-e" w3-tn3270-emulator server port)
      (if name (message "Please log in as %s" name)))
     (w3-use-transparent
      (require 'transparent)
      (if name (message "Please log in as %s" name))
      (sit-for 1)
      (transparent-window (get-buffer-create
			   (format "%s%s:%s" (if name (concat name "@") "")
				   server port))
			  w3-tn3270-emulator
			  (list server port) nil
			  "Press any key to return to emacs"))
     (t
      (terminal-emulator
       (get-buffer-create (format "%s%s:%s" (if name (concat name "@") "")
				  server port))
       w3-tn3270-emulator
       (list server port))
      (if name (message "Please log in as %s" name))))))

(defun w3-mailto (url)
  "Send mail to someone"
  (string-match "mailto:/*\\(.*\\)" url)
  (let ((to (substring url (match-beginning 1) (match-end 1)))
	(url (w3-view-url t)))
    (cond
     ((and w3-mutable-windows (fboundp w3-mail-other-window-command))
      (funcall w3-mail-other-window-command))
     ((fboundp w3-mail-command)
      (funcall w3-mail-command))
     (w3-mutable-windows (mail-other-window))
     (t (mail)))
    (mail-to)
    (w3-insert (format "%s\nX-URL-From: %s" to url))
    (mail-subject)))

(defun w3-gopher (url)
  "Handle gopher URLs"
  (let ((descr (w3-grok-gopher-href url)))
    (cond
     ((or (not (w3-member (nth 1 descr) w3-bad-port-list))
	  (funcall
	   w3-confirmation-func
	   (format "Warning!  Trying to connect to port %s - continue? "
		   (nth 1 descr))))
      (if w3-use-hypertext-gopher
	  (w3-do-gopher descr)
	(gopher-dispatch-object (vector (if (= 0
					       (string-to-char (nth 2 descr)))
					    ?1
					  (string-to-char (nth 2 descr)))
					(nth 2 descr) (nth 2 descr)
					(nth 0 descr)
					(string-to-int (nth 1 descr)))
				(current-buffer))))
     (t (ding) (message "Aborting connection to bad port...")))))

(defun w3-generate-error (type data)
  "Generate an HTML error buffer for error TYPE with data DATA."
  (cond
   ((equal type "nofile")
    (insert "<title>Error</title>\n"
	    "<h1>No file " data " found</h1>\n"
	    "<hr>\n"
	    "The file " data " could not be found.  Either it does not"
	    "exist, or is unreadable.\n"))
   ((equal type "nobuf")
    (insert "<TITLE>Error</TITLE>\n"
	    "<H1>No buffer " data " found</h1>\n"
	    "<HR>\n"
	    "The buffer " data " could not be found.  It has either\n"
	    "been killed or renamed.\n"))
   ((equal type "nohist")
    (insert "<TITLE>Error</TITLE>\n"
	    "<H1>No history items found.</H1>\n"
	    "<HR>\n"
	    "There is no history list available at this time.  Either\n"
	    "you have not visited any nodes, or the variabe <i>\n"
	    "w3-keep-history</i> is nil.\n"))
   )
  (insert "<HR>\n"
	  "If you feel this is a bug, <A HREF=\"mailto:wmperry@indiana.edu\">"
	  "send mail to wmperry@indiana.edu</A>\n<HR>"))

(defun w3-generate-auto-html (type)
  "Generate one of several automatic html pages"
  (cond
   ((equal type "hotlist")
    (let ((tmp (reverse w3-hotlist)))
      (insert "<htmlplus>\n\t<head>\n\t\t"
	      "<title> Hotlist </title>\n\t</head>\n"
	      "\t<body>\n\t\t<div1>\n\t\t\t<h1>Hotlist from " w3-hotlist-file
	      "</h1>\n\t\t\t<ol>\n")
      (while tmp
	(insert  "\t\t\t\t<li> <a href=\"" (car (cdr (car tmp)))
		 "\">" (car (car tmp)) "</a></li>\n")
	(setq tmp (cdr tmp)))
      (insert "\n\t\t\t</ol>\n\t\t</div1>\n\t</body>\n</htmlplus>")))
   ((equal type "starting-points")
    (let ((tmp w3-starting-documents))
      (insert "<htmlplus>\n\t<head>\n\t\t"
	      "<title> Starting Points </title>\n\t</head>\n"
	      "\t<body>\n\t\t<div1>\n\t\t\t<h1>Starting Point on the Web"
	      "</h1>\n\t\t\t<ol>\n")
      (while tmp
	(insert (format "\t\t\t\t<li> <a href=\"%s\">%s</a></li>\n"
			(car (cdr (car tmp)))
			(car (car tmp))))
	(setq tmp (cdr tmp)))
      (insert "\n\t\t\t</ol>\n\t\t</div1>\n\t</body>\n</htmlplus>")))
   ((equal type "history")
    (if (not w3-history-list)
	(w3-retrieve "www://error/nohist")
      (let ((urls w3-history-list))
	(insert "<htmlplus>\n\t<head>\n\t\t"
		"<title> History List For This Session of W3</title>"
		"\n\t</head>\n\t<body>\n\t\t<div1>\n\t\t\t<h1>"
		"History List For This Session of W3</h1>\n\t\t\t<ol>\n")
	(while urls
	  (insert (format "\t\t\t\t<li> <a href=\"%s\">%s</A>\n"
			  (car (car urls)) (cdr (car urls))))
	  (setq urls (cdr urls)))
	(insert "\n\t\t\t</ol>\n\t\t</div1>\n\t</body>\n</htmlplus>"))))
   ((equal type "help")
    (let* ((funcs w3-doc-functions)
	   (funcstr "")
	   (vars w3-doc-variables)
	   (varstr "")
	   (keys nil))
      (while funcs
	(if (fboundp (car funcs))
	    (setq keys (where-is-internal (car funcs) w3-mode-map t)
		  funcstr (format "%s\n<LI>%5s: %s" funcstr
				  (if (null keys)
				      (format "M-x %s" (car funcs))
				    (if w3-running-FSF19
					(key-description (car keys))
				      (key-description keys)))
				  (documentation (car funcs)))))
	(setq funcs (cdr funcs)))
      (while vars
	(if (boundp (car vars))
	    (let* ((thevar (prin1-to-string (car vars)))
		   (doc (documentation-property (car vars)
						'variable-documentation)))
	      (setq varstr
		    (format "%s\n<LI>%20s: %s\n" varstr thevar
			    (if (> (+ (length thevar) (length doc)) 80)
				(concat "\n" doc)
			      doc)))))
	(setq vars (cdr vars)))
      (w3-insert (format "<TITLE>Help For W3 V%s</TITLE>\n" w3-version-number))
      (w3-insert "<H1>Current keybindings:</H1>\n<HR><P>\n"
		 "<UL>" funcstr "</UL>")
      (w3-insert "<HR><H1>Modifiable variables:</H1>\n<HR><P>\n"
		 "<UL>" varstr "</UL>")))))

(defun w3-internal-url (url)
  "Handle internal urls (previewed buffers, etc"
  (string-match "www:/+\\([^/]+\\)/\\(.*\\)" url)
  (let ((type (w3-match url 1))
	(data (w3-match url 2)))
    (set-buffer (get-buffer-create w3-working-buffer))
    (setq w3-current-type "www"
	  w3-current-server type
	  w3-current-file data)
    (cond
     ((equal type "preview")		; Previewing a document
      (if (get-buffer data)		; Buffer still exists
	  (insert-buffer data)		; Insert the document
	(w3-retrieve (concat "www://error/nobuf/" data))))
     ((equal type "error")		; Error message
      (string-match "\\([^/]+\\)/\\(.*\\)" data)
      (w3-generate-error (w3-match data 1) (w3-match data 2)))
     ((equal type "auto")		; Hotlist or help stuff
      (w3-generate-auto-html data)))))

(fset 'w3-www 'w3-internal-url)
(fset 'w3-ftp 'w3-file)

(defun w3-x-exec (url)
  "Handle local execution of scripts."
  (set-buffer (get-buffer-create w3-working-buffer))
  (erase-buffer)
  (string-match "x-exec:/+\\([^/]+\\)\\(/.*\\)" url)
  (let ((process-environment process-environment)
	(executable (w3-match url 1))
	(path-info (w3-match url 2))
	(query-string nil)
	(safe-paths w3-local-exec-path)
	(found nil)
	(y nil)
	)
    (setq w3-current-server executable
	  w3-current-file path-info)
    (if (string-match "\\(.*\\)\\?\\(.*\\)" path-info)
	(setq query-string (w3-match path-info 2)
	      path-info (w3-match path-info 1)))
    (setenv "SERVER_SOFTWARE" "X-exec/1.0")
    (setenv "SERVER_NAME" (system-name))
    (setenv "GATEWAY_INTERFACE" "CGI/1.1")
    (setenv "SERVER_PROTOCOL" "HTTP/1.0")
    (setenv "SERVER_PORT" "")
    (setenv "REQUEST_METHOD" w3-request-method)
    (setenv "HTTP_ACCEPT" (mapconcat
			   (function
			    (lambda (x)
			      (cond
			       ((= x ?\n) (setq y t) "")
			       ((= x ?:) (setq y nil) ",")
			       (t (char-to-string x))))) w3-mime-accept-string
			       ""))
    (setenv "PATH_INFO" (w3-unhex-string path-info))
    (setenv "PATH_TRANSLATED" (w3-unhex-string path-info))
    (setenv "SCRIPT_NAME" executable)
    (setenv "QUERY_STRING" (w3-unhex-string query-string))
    (setenv "REMOTE_HOST" (system-name))
    (if (assoc "content-type" w3-request-extra-headers)
	(setenv "CONTENT_TYPE" (cdr
				(assoc "content-type"
				       w3-request-extra-headers))))
    (if w3-request-data
	(setenv "CONTENT_LENGTH" (length w3-request-data)))
    (while (and safe-paths (not found))
      (setq y (expand-file-name executable (car safe-paths))
	    found (and (file-exists-p y) (file-executable-p y) y)
	    safe-paths (cdr safe-paths)))
    (if (not found)
	(w3-retrieve (concat "www://error/nofile/" executable))
      (and w3-request-data (insert w3-request-data))
      (setq y (call-process-region (point-min) (point-max) found t t))
      (goto-char (point-min))
      (while (looking-at " \\\t\\\n") (delete-char 1))
      (cond
       ((w3-mime-response-p) nil)	; Its already got an HTTP/1.0 header
       ((null y)			; Weird exit status, whassup?
	(insert "HTTP/1.0 404 Not Found\n"
		"Server: Emacs-W3/x-exec\n"))	
       ((= 0 y)				; The shell command was successful
	(insert "HTTP/1.0 200 Document follows\n"
		"Server: Emacs-W3/x-exec\n"))	
       (t				; Non-zero exit status is bad bad bad
	(insert "HTTP/1.0 404 Not Found\n"
		"Server: Emacs-W3/x-exec\n"))))))

(defun w3-fix-proxy-url ()
  "Fix a proxy url so that it doesn't get appended twice."
  (string-match w3-nonrelative-link w3-current-file)
  (let* ((type (w3-match w3-current-file 1))
	 (prsr (read (concat "w3-grok-" type "-href")))
	 (info (and prsr (funcall prsr w3-current-file))))
    (setq w3-current-type type)
    (cond
     ((string= type "news")
      (setq w3-current-server (nth 0 info)
	    w3-current-port (nth 1 info)
	    w3-current-file (nth 2 info)))
     ((string= type "http")
      (setq w3-current-server (nth 0 info)
	    w3-current-port (nth 1 info)
	    w3-current-file (nth 2 info)))
     ((or (string= type "ftp") (string= type "file"))
      (setq w3-current-user (nth 0 info)
	    w3-current-server (nth 1 info)
	    w3-current-file (nth 2 info)))
     ((string= type "gopher")
      (setq w3-current-server (nth 0 info)
	    w3-current-port (nth 1 info)
	    w3-current-file (nth 2 info))))))
     
;;;###autoload
(defun w3-retrieve (url)
  "Retrieve a document over the World Wide Web.
The document should be specified by its fully specified
Uniform Resource Locator.  No parsing is done, just return the
document as the server sent it.  The document is left in the
buffer specified by w3-working-buffer.  w3-working-buffer is killed
immediately before starting the transfer, so that no buffer-local
variables interfere with the retrieval.  HTTP/1.0 redirection will
be honored before this function exits."
  (if (get-buffer w3-working-buffer)
      (kill-buffer w3-working-buffer))
  (string-match "\\([^:]*\\):/*" url)
  (let* ((type (substring url (match-beginning 1) (match-end 1)))
	 (w3-using-proxy (and
			  (if (assoc "no_proxy" w3-proxy-services)
			      (not (string-match
				    (cdr (assoc "no_proxy" w3-proxy-services))
				    url))
			    t)
			  (cdr (assoc type w3-proxy-services))))
	 (handler nil)
	 (tmp w3-current-file))
    (if w3-using-proxy
	(setq url (concat w3-using-proxy
			  (if (equal (substring w3-using-proxy -1 nil) "/")
			      "" "/") url)
	      type (and (string-match "\\([^:]*\\):/*" w3-using-proxy)
			(w3-match w3-using-proxy 1))))
    (setq handler (intern (downcase (concat "w3-" type))))
    (if (fboundp handler)
	(funcall handler url)
      (set-buffer (get-buffer-create w3-working-buffer))
      (setq w3-current-file tmp)
      (erase-buffer)
      (w3-insert "<TITLE> Link Error! </TITLE>\n"
		 "<H1> An error has occurred... </H1>\n"
		 (format "The link type <CODE>%s</CODE>" type)
		 " is unrecognized or unsupported at this time.<P>\n"
		 "If you feel this is an error, please"
		 "<A HREF=\"mailto://wmperry@indiana.edu\">send me mail.</A>"
		 "<P><ADDRESS>William Perry</ADDRESS>"
		 "<ADDRESS>wmperry@indiana.edu</ADDRESS>")
      (setq w3-current-file "error.html"))
    (if (boundp 'MULE) (w3-convert-code-for-mule type url))
    (if (and
	 (not w3-be-asynchronous)
	 (get-buffer w3-working-buffer))
	(w3-clean-text))
    (cond
     ((equal type "wais") nil)
     ((and w3-be-asynchronous (equal type "http")) nil)
     ((not (get-buffer w3-working-buffer)) nil)
     ((w3-mime-response-p) (w3-parse-mime-headers))
     ((w3-member w3-current-server w3-bad-server-list) nil)
     ((equal type "http")
      (setq w3-bad-server-list (cons w3-current-server w3-bad-server-list))))
    (if (and (not w3-be-asynchronous)
	     (not w3-current-mime-type))
	(if (w3-buffer-is-hypertext)
	    (setq w3-current-mime-type "text/html")
	  (setq w3-current-mime-type (w3-extension-to-mime
				      (w3-file-extension
				       w3-current-file)))))
    (and w3-using-proxy (w3-fix-proxy-url))
    (and (not (w3-have-visited-url url))
	 (setq w3-global-history-completion-list
	       (cons (cons url (current-time-string)) 
		     w3-global-history-completion-list)))))

;;;###autoload
(defun w3-fetch (&optional url)
  "Retrieve a document over the World Wide Web.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The document should be specified by its fully specified
Uniform Resource Locator.  The document will be parsed, printed, or
passed to an external viewer as appropriate.  See the variable
w3-mime-viewers for how to specify a viewer for a file type."
  (interactive (list
		(progn
		  (if (not w3-setup-done) (w3-do-setup))
		  (completing-read "URL: "
				   w3-global-history-completion-list
				   nil nil 
				   (if (eq major-mode 'w3-mode)
				       (w3-view-url t) "")))))
  (let ((x (w3-view-url t))
	(buf (w3-buffer-visiting url)))
    (if (not w3-setup-done) (w3-do-setup))
    (if (string= "file:nil" x) (setq x nil))
    (if (or (not buf)
	    (cond
	     ((eq w3-reuse-buffers 'no) t)
	     ((eq w3-reuse-buffers 'yes) nil)
	     (t
	      (if w3-reuse-buffers
		  (progn
		    (ding)
		    (message
		     "Warning: Invalid value for variable w3-reuse-buffers: %s"
		     (prin1-to-string w3-reuse-buffers))
		    (sit-for 2)))
	      (not (funcall w3-confirmation-func
			    (format "URL found in buffer %10s, reuse "
				    (buffer-name buf)))))))
	(progn
	  (w3-retrieve url)
	  (w3-add-urls-to-history x url)
	  (if (get-buffer w3-working-buffer)
	      (cond
	       ((and w3-be-asynchronous (string-match "^http:" url)) nil)
	       (t (w3-sentinel nil nil)))))
      (switch-to-buffer buf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History for forward/back buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-forward-in-history ()
  "Go forward in the history from this page"
  (interactive)
  (let* ((x (intern (w3-view-url t)))
	 (y (get 'w3-history x))
	 (w3-reuse-buffers 'yes)
	 (url (car y))
	 (buf (car (cdr y)))
	 (pnt (car (cdr (cdr y)))))
    (if (or (null y) (eq y 'none)) (error "No forward found for %s" x))
    (if (and buf (buffer-name buf))
	(progn
	  (switch-to-buffer buf)
	  (goto-char pnt))
      (w3-fetch url))))

(defun w3-backward-in-history ()
  "Go backward in the history from this page"
  (interactive)
  (let* ((x (intern (w3-view-url t)))
	 (y (get 'w3-history x))
	 (w3-reuse-buffers 'yes)
	 (url (car y))
	 (buf (car (cdr y)))
	 (pnt (car (cdr (cdr y)))))
    (if (or (null y) (eq y 'none)) (error "No backward found for %s" x))
    (if (and buf (buffer-name buf))
	(progn
	  (switch-to-buffer buf)
	  (goto-char pnt))
      (w3-fetch url))))

(defun w3-add-urls-to-history (referer url)
  "REFERER is the url we followed this link from.  URL is the link we got to."
  (let ((x (and referer (intern referer)))
	(y (and url (intern url))))
    (setq referer
	  (cond
	   (referer (list referer w3-current-last-buffer (point)))
	   ((get 'w3-history x) (get 'w3-history x))
	   (t 'none)))
    (setq url
	  (cond
	   (url (list url (current-buffer) (point)))
	   ((get 'w3-history y) (get 'w3-history y))
	   (t 'none)))
    (put 'w3-history x url)
    (put 'w3-history y referer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-quotify-percents (str)
  "Convert all '%'s in STR to be '%%' so it can be passed to format."
  (if str
      (let ((x ""))
	(while (string-match "%" str)
	  (setq x (concat (substring str 0 (match-beginning 0)) "%%")
		str (substring str (match-end 0) nil)))
	(concat x str))
    str))

(defun w3-use-starting-documents ()
  "Use the list of predefined starting documents from w3-starting-documents"
  (interactive)
  (let ((w3-hotlist w3-starting-documents))
    (w3-use-hotlist)))

(defun w3-show-starting-documents ()
  "Show the list of predefined starting documents from w3-starting-documents"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (w3-fetch "www://auto/starting-points"))

(defun w3-insert-formatted-url (p)
  "Insert a formatted url into a buffer.  With prefix arg, insert the url
under point."
  (interactive "P")
  (let (buff str)
    (cond
     (p
      (setq p (w3-view-this-url t))
      (or p (error "No url under point"))
      (setq str (format "<A HREF=\"%s\">%s</A>" p
			(read-string "Link text: "
				     (nth 3 (w3-zone-data
					     (w3-zone-at (point))))))))
     (t
      (setq str (format "<A HREF=\"%s\">%s</A>" (w3-view-url t)
			(read-string "Link text: " (buffer-name))))))
    (setq buff (read-buffer "Insert into buffer: " nil t))
    (if buff
	(save-excursion
	  (set-buffer buff)
	  (w3-insert str))
      (message "Cancelled."))))

(defun w3-length-after-parsing (str)
  "Returns the length of a string after removing all text between
<>, and resolving all HTML entity references"
  (let ((tmp ""))
    (while (string-match "\\([^<]*\\)<[^>]+>" str)
      (setq tmp (concat tmp (w3-match str 1))
	    str (substring str (match-end 0) nil)))
    (setq tmp (concat tmp str))
    (setq tmp (w3-fix-entities-in-string tmp))
    (length tmp)))

(defun w3-first-n-items (l n)
  "Return the first N items from list L"
  (let ((x 0)
	y)
    (if (> n (length l))
	(setq y l)
      (while (< x n)
	(setq y (nconc y (list (nth x l)))
	      x (1+ x))))
    y))
	       
(defun w3-breakup-menu (menu-desc max-len)
  (if (> (length menu-desc) max-len)
      (cons (cons "More..." (w3-first-n-items menu-desc max-len))
	    (w3-breakup-menu (nthcdr max-len menu-desc) max-len))
    menu-desc))

(defun w3-get-url-at-point (&optional pt)
  "Get the URL closest to point, but don't change your
position. Has a preference for looking backward when not
directly on a symbol."
  ;; Not at all perfect - point must be right in the name.
  (save-excursion
    (if pt (goto-char pt))
    (let ((filename-chars ".a-zA-Z0-9---_/:~") start)
      (save-excursion
	;; first see if you're just past a filename
	(if (not (eobp))
	    (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
		(progn
		  (skip-chars-backward " \n\t\r({[]})")
		  (if (not (bobp))
		      (backward-char 1)))))
	(if (string-match (concat "[" filename-chars "]")
			  (char-to-string (following-char)))
	    (progn
	      (skip-chars-backward filename-chars)
	      (setq start (point))
	      (skip-chars-forward filename-chars))
	  (message "No URL found around point!")
	  (setq start (point)))
	(buffer-substring start (point))))))

(defun w3-follow-url-at-point (&optional pt)
  "Follow the URL under PT, defaults to link under (point)"
  (interactive "d")
  (w3-fetch (w3-get-url-at-point pt)))			     

;;;###autoload
(defun w3-batch-fetch ()
  "Fetch all the URLs on the command line and save them to files in
the current directory.  The first argument after the -f w3-batch-fetch
on the command line should be a string specifying how to save the
information retrieved.  If it is \"html\", then the page will be
unformatted when it is written to disk.  If it is \"text\", then the
page will be formatted before it is written to disk.  If it is
\"binary\" it will not mess with the file extensions, and just save
the data in raw binary format.  If none of those, the default is
\"text\", and the first argument is treated as a normal URL."  
  (if (not noninteractive)
      (error "`w3-batch-fetch' is to be used only with -batch"))
  (let ((fname "")
        (curname "")
	(x 0)
	(args (symbol-value 'command-line-args-left))
	(w3-strict-width 80)
	(w3-delimit-emphasis nil)
	(w3-delimit-links nil)
	(retrieval-function 'w3-fetch)
	(file-format "text")
	(header "")
	(file-extn ".txt"))
    (setq file-format (downcase (car args)))
    (cond
     ((string= file-format "html")
      (message "Saving all text as raw HTML...")
      (setq retrieval-function 'w3-retrieve
	    file-extn ".html"
	    header "<BASE HREF=\"%s\">"
	    args (cdr args)))
     ((string= file-format "binary")
      (message "Saving as raw binary...")
      (setq retrieval-function 'w3-retrieve
	    file-extn ""
	    args (cdr args)))
     ((string= file-format "text")
      (setq header "Text from: %s\n---------------\n")
      (message "Saving all text as formatted...")
      (setq args (cdr args)))
     (t
      (setq header "Text from: %s\n---------------\n")
      (message "Going with default, saving all text as formatted...")))
    (while args
      (funcall retrieval-function (car args))
      (goto-char (point-min))
      (if buffer-read-only (toggle-read-only))
      (insert (format header (car args)))
      (if (string= file-extn "") nil
	(setq fname (w3-file-extension (w3-basepath w3-current-file t) t)))
      (if (string= (w3-strip-leading-spaces fname) "")
	  (setq fname "root"))
      (setq curname fname)
      (while (file-exists-p (concat curname file-extn))
	(setq curname (concat fname x)
	      x (1+ x)))
      (setq fname (concat curname file-extn))
      (write-region (point-min) (point-max) fname)
      (setq args (cdr args)))))

(defun w3-eat-trailing-space (x)
  "Remove spaces/tabs at the end of a string"
  (let ((y (1- (length x)))
	(skip-chars (list ?  ?\t ?\n)))
    (while (and (>= y 0) (memq (aref x y) skip-chars))
      (setq y (1- y)))
    (substring x 0 (1+ y))))

(defun w3-strip-leading-spaces (x)
  "Remove spaces at the front of a string"
  (let ((y (1- (length x)))
	(z 0)
	(skip-chars (list ?  ?\t ?\n)))
    (while (and (<= z y) (memq (aref x z) skip-chars))
      (setq z (1+ z)))
    (substring x z nil)))

(defun w3-reload-all-files ()
  "Reload all w3 files"
  (interactive)
  (setq w3-setup-done nil
	w3-hotlist nil
	w3-mime-accept-string nil
	w3-style-regexp nil)
  (let ((x '(w3 w3-mule w3-emacs w3-emacs19 w3-epoch w3-lucid
		w3-new-lucid w3-next)))
    (while x
      (setq features (delq (car x) features)
	    x (cdr x)))
    (require 'w3))
  (w3-do-setup))

(defun w3-source-document-at-point ()
  "View source to the document pointed at by link under point"
  (interactive)
  (w3-source-document t))

(defun w3-source-document (under)
  "View this documents source"
  (interactive "P")
  (let* ((url (if under (w3-view-this-url) (w3-view-url t)))
	 (src
	  (cond
	   ((and under (null url)) (message "No link at point!"))
	   ((and (not under) w3-current-source) w3-current-source)
	   (t 
	    (prog2
		(w3-retrieve url)
		(buffer-string)
	      (kill-buffer (current-buffer))))))
	 (tmp (w3-generate-new-buffer-name url)))
    (if (not url) nil
      (set-buffer (get-buffer-create tmp))
      (insert src)
      (goto-char (point-min))
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (if w3-mutable-windows (pop-to-buffer tmp) (switch-to-buffer tmp)))))

(defun w3-mail-document-under-point ()
  "Mail the document pointed to by the hyperlink under point."
  (interactive)
  (w3-mail-current-document t))

(defun w3-mail-current-document (under)
  "Mail the current-document to someone"
  (interactive "P")
  (let* ((format (completing-read
		  "Format: "
		  '(("HTML Source") ("Formatted Text") ("LaTeX Source"))
		  nil t))
	 (url (if under (w3-view-this-url t) (w3-view-url t)))
	 (str 
	  (save-excursion
	    (cond
	     ((and (equal "HTML Source" format) under)
	      (w3-retrieve url))
	     ((equal "HTML Source" format)
	      (if w3-current-source
		  (let ((x w3-current-source))
		    (set-buffer (get-buffer-create w3-working-buffer))
		    (erase-buffer)
		    (insert x))
		(w3-retrieve url)))
	     ((and under (equal "Formatted Text" format))
	      (w3-fetch url))
	     ((equal "Formatted Text" format) nil)
	     ((and under (equal "LaTeX Source" format))
	      (w3-retrieve url)
	      (w3-convert-html-to-latex))
	     ((equal "LaTeX Source" format)
	      (if w3-current-source
		  (let ((x w3-current-source))
		    (set-buffer (get-buffer-create w3-working-buffer))
		    (erase-buffer)
		    (insert x))
		(w3-retrieve url))
	      (w3-convert-html-to-latex)))
	    (buffer-string))))
    (cond
     ((and w3-mutable-windows (fboundp w3-mail-other-window-command))
      (funcall w3-mail-other-window-command))
     ((fboundp w3-mail-command)
      (funcall w3-mail-command))
     (w3-mutable-windows (mail-other-window))
     (t (mail)))
    (mail-subject)
    (w3-insert (format "%s from URL %s" format url))
    (re-search-forward mail-header-separator nil)
    (forward-char 1)
    (while (< (current-column) 79) (w3-insert "-"))
    (w3-insert "\n" (if (equal "HTML Source" format)
			(format "<BASE HREF=\"%s\">" url) "") str "\n")
    (while (< (current-column) 79) (w3-insert "-"))
    (mail-to)))

(defun w3-parse-relative-link (url)
  "Try to resolve a link like \"library/io.html\""
  (let* ((w3-current-file w3-current-file)
	 (resolved (cond ((equal w3-current-type "http")
			  (concat "http://" w3-current-server
				  (if (equal w3-current-port "80") ""
				    (concat ":" w3-current-port))))
			 ((equal w3-current-type "gopher")
			  (concat "gopher://" w3-current-server
				  (if (equal w3-current-port "70") ""
				    (concat ":" w3-current-port)) "/"))
			 ((equal w3-current-type "news")
			  (concat "news:"
				  (if (equal w3-current-server w3-news-server)
				      "" (concat "//" w3-news-server
						 (if (equal w3-current-port
							    "119") ""
						   (concat ":"
							   w3-current-port))
						 "/"))))
			 ((equal w3-current-type "ftp")
			  (concat "file://"
				  (if w3-current-user (concat
						       w3-current-user "@") "")
				  w3-current-server))
			 ((equal w3-current-type "www")
			  (setq w3-current-file
				(buffer-file-name
				 (get-buffer w3-current-file)))
			  "file:")
			 (t "file:"))))
    (cond
     ((equal "news" w3-current-type)
      (setq resolved (w3-remove-relative-links (concat resolved url))))
     ((= ?# (string-to-char url))
      (setq resolved url))
     ((equal url "") nil)
     ((equal (aref url 0) ?/) (setq resolved (concat resolved url)))
     (t (setq resolved (concat resolved
			       (w3-remove-relative-links
				(concat (w3-basepath w3-current-file) url))))))
    resolved))

(defun w3-internal-use-history (hist-item)
  "Go to the link in the history"
  (let ((url (nth 0 hist-item))
	(buf (nth 1 hist-item))
	(pnt (nth 2 hist-item)))
    (cond
     ((null buf)			; Find a buffer with same url
      (let ((x (buffer-list))
	    (found nil))
	(while (and x (not found))
	  (save-excursion
	    (set-buffer (car x))
	    (setq found (string= (w3-view-url t) url))
	    (if (not found) (setq x (cdr x)))))
	(cond
	 (found
	  (switch-to-buffer (car x))
	  (if (number-or-marker-p pnt) (goto-char pnt)))
	 (t
	  (w3-fetch url)))))
     ((buffer-name buf)			; Reuse the old buffer if possible
      (switch-to-buffer buf)
      (if (number-or-marker-p pnt) (goto-char pnt))
      (if (and url (= ?# (string-to-char url)))	; Destination link
	  (progn
	    (goto-char (point-min))
	    (w3-find-specific-link (substring url 1 nil)))))
     (url (w3-maybe-relative url))		; Get the link
     (t (message "Couldn't understand whats in the history.")))))

(defun w3-relative-link (url)
  (if (equal "#" (substring url 0 1))
      (progn
	(push-mark (point) t)
	(goto-char (point-min))
	(w3-find-specific-link (substring url 1 nil)))
    (w3-fetch (w3-parse-relative-link url))))
  
(defun w3-maybe-eval ()
  "Maybe evaluate a buffer of emacs lisp code"
  (if (funcall w3-confirmation-func "This is emacs-lisp code, evaluate it?")
      (eval-current-buffer)
    (emacs-lisp-mode)))

(defun w3-build-continuation ()
  "Build a series of functions to be run on this file"
  (save-excursion
    (set-buffer w3-working-buffer)
    (let ((cont w3-default-continuation)
	  (extn (w3-file-extension w3-current-file)))
      (if (assoc extn w3-uncompressor-alist)
	  (setq extn (w3-file-extension
		      (substring w3-current-file 0 (- (length extn))))))
      (if w3-source (setq w3-current-mime-viewer
			  (w3-mime-viewer "www/source")))
      (if (not w3-current-mime-viewer)
	  (setq w3-current-mime-viewer
		(w3-mime-viewer (or w3-current-mime-type
				    (w3-extension-to-mime extn)))))
      (if w3-current-mime-viewer
	  (setq cont (append cont '(w3-pass-to-viewer)))
	(setq cont (append cont (list w3-default-action))))
      cont)))

(defun w3-link-info (&optional url no-show)
  "Get more information on a link."
  (interactive)
  (if (not url) (setq url (w3-view-this-url t)))
  (if (not url) (setq url (w3-view-url t)))
  (let (ctype result serv fil type)
    (string-match "^\\([^:]+\\):/*\\([^/]+\\)/*\\(.*\\)" url)
    (setq type (w3-match url 1)
	  serv (w3-match url 2)
	  fil (w3-match url 3))
    (cond
     ((string= type "news")
      (setq ctype (if (string-match "@" serv) "news article"
		    (concat "newsgroup (" serv ")"))
	    type "NNTP site at %s"
	    serv w3-news-server))
     ((and (string= type "file")
	   (not (string-match "^file://" url)))
      (setq serv "local file system."
	    ctype (concat (cdr (assoc (w3-extension-to-mime
				       (w3-file-extension fil))
				      w3-mime-descriptions)) " file")
	    type "%s"))
     ((string= "gopher" type)
      (setq type (concat type " site at %s."))
      (if (= ?0 (string-to-char fil))
	  (setq ctype "gopher document")
	(setq ctype "gopher directory")))
     (t
      (if (string= type "file") (setq type "ftp"))
      (setq ctype (concat (cdr
			   (assoc (w3-extension-to-mime
				   (w3-file-extension fil))
				  w3-mime-descriptions)) " file")
	    type (concat type " site at %s."))))
    (setq result
	  (format "A %s on the %s" ctype (format type serv)))
    (if (not no-show) (message result) result)))

(defun w3-use-links ()
  "Select one of the <LINK> tags from this document and fetch it."
  (interactive)
  (and (not w3-current-links)
       (error "No <LINK> tags in this document."))
  (let* ((completion-ignore-case t)
	 (type (cdr (assoc
		     (completing-read "Type of relation: "
				      '(("Reverse" . "rev") ("Normal" . "rel"))
				      nil t "normal")
		     '(("Reverse" . "rev") ("Normal" . "rel")))))
	 (table nil)
	 (x w3-current-links)
	 y)
    (while x
      (setq y (car x)
	    x (cdr x))
      (if (assoc type y)
	  (setq table (cons
		       (cons (cdr (assoc type y)) (cdr (assoc "href" y)))
		       table))))
    (w3-fetch (cdr (assoc (completing-read "Link: " table nil t) table)))))

(defun w3-fix-fake-urls (st nd)
  "Fix fake urls into real \"<A HREF=...></A>\" type links"
  (interactive "r")
  (save-restriction
    (narrow-to-region st nd)
    (goto-char (point-min))
    (w3-replace-regexp
     (concat w3-nonrelative-link "\\(:[^ >]+\\)") " \\1\\2")
    (w3-replace-regexp
     (concat "[^=\"]+" w3-nonrelative-link "\\(:[^ >\\\n]+\\)")
     "<A HREF=\"\\1\\2\"> \\1\\2 </A>")))

(defun w3-update-source-files (w3-source-directory)
  "Retrieve the latest version of the w3 browser and copy it to`
w3-source-directory"
  (interactive "DWhere to store w3.tar.z? ")
  (let ((newfile (format "%s%sw3.tar.z" w3-source-directory
			 (if (equal "/" (substring w3-source-directory -1 nil))
			     "" "/")))
	(oldfile "/anonymous@cs.indiana.edu:/pub/elisp/w3/w3.tar.z"))
    (if (file-newer-than-file-p oldfile newfile)
	(copy-file oldfile newfile 3))
    (shell-command (format "gunzip %s" newfile))
    (shell-command (format "cd %s ; tar xf %s"
			   w3-source-directory
			   newfile))))

(defun w3-hexify-string (str)
  "Escape characters in a string"
  (if (boundp 'MULE)
      (setq str (code-convert-string 
		 str *internal* w3-mule-retrieval-coding-system)))
  (setq str (mapconcat
	     (function
	      (lambda (char)
		(if (or (> char ?z)
			(< char ?.)
			(and (< char ?a)
			     (> char ?Z))
			(and (< char ?@)
			     (> char ?9)))
		    (if (< char 16)
			(upcase (format "%%0%x" char))
		      (upcase (format "%%%x" char)))
		  (char-to-string char))))
	     (w3-fix-entities-in-string str) "")))

(defun w3-find-this-file ()
  "Do a find-file on the currently viewed html document if it is a file: or
ftp: reference"
  (interactive)
  (cond
   ((and (null w3-current-type)
	 (eq major-mode 'w3-mode))
    (if w3-mutable-windows
	(find-file-other-window w3-current-file)
      (find-file w3-current-file)))
   ((equal w3-current-type "ftp")
    (if w3-mutable-windows
	(find-file-other-window
	 (format "/anonymous@%s:%s" w3-current-server w3-current-file))
      (find-file 
       (format "/anonymous@%s:%s" w3-current-server w3-current-file))))
   (t (message "Sorry, I can't get that file so you can alter it."))))

(defun w3-delete-from-alist (x alist)
  "Remove X from ALIST, return new alist"
  (if (eq (assoc x alist) (car alist)) (cdr alist)
    (delq (assoc x alist) alist)))

(defun w3-count-occurences (regexp)
  "Count # of matches for REGEXP after point. Modified from the how-many
function of emacs19"
  (let ((n 0) opoint)
    (save-excursion
      (while (and (not (eobp))
		  (progn (setq opoint (point))
			 (re-search-forward regexp nil t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq n (1+ n)))))
    n))      

(defun w3-insert-this-url (pref-arg)
  "Insert the current url in another buffer, with prefix ARG, insert URL under point"
  (interactive "P")
  (let ((thebuf (get-buffer (read-buffer "Insert into buffer: ")))
	(oldbuf (current-buffer))
	(url (if pref-arg (w3-view-this-url t) (w3-view-url t))))
    (if (not (equal "Not on a link!" url))
	(progn
	  (set-buffer thebuf)
	  (w3-insert url)
	  (set-buffer oldbuf)))))      

(defun w3-show-hotlist ()
  "View the hotlist in hypertext form"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist)
      (error "Sorry, no hotlist is in memory.")
    (w3-fetch "www://auto/hotlist")))

(defun w3-lookup-style (type)
  "Return the physical style of logical style <TYPE>"
  (let ((x (cdr (assoc type w3-style-assoc))))
    (if (symbolp x) (symbol-value x) x)))	

(defun w3-make-sequence (start end)
  "Make a sequence (list) of numbers from START to END"
  (cond
   ((= start end) '())
   ((> start end) '())
   (t
    (let ((sqnc '()))
      (while (<= start end)
	(setq sqnc (cons end sqnc)
	      end (1- end)))
      sqnc))))

(defun w3-maybe-relative (url)
  "Take a url and either fetch it, or resolve relative refs, then fetch it"
  (cond
   ((not
     (string-match w3-nonrelative-link url))
    (w3-relative-link url))
   (t (w3-fetch url))))
 
(defun w3-in-assoc (elt list)
  "Check to see if ELT matches any of the regexps in the car elements of LIST"
  (let (rslt)
    (while (and list (not rslt))
      (and (car (car list))
	   (string-match (car (car list)) elt)
	   (setq rslt (car list)))
      (setq list (cdr list)))
    rslt))

(defun w3-member (elt list)
  "Function defined so that we are sure member will always use equal, like
its supposed to.  This was pulled from Jamie Zawinskies byte compiler "
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

(defun w3-goto-last-buffer ()
  "Go to last WWW buffer visited"
  (interactive)
  (if w3-current-last-buffer
      (if w3-mutable-windows
	  (pop-to-buffer w3-current-last-buffer)
	(switch-to-buffer w3-current-last-buffer))
    (message "No previous buffer found.")))

(defun w3-file-extension (fname &optional x)
  "Return the filename extension of FNAME.  If optional variable X is t,
then return the basename of the file with the extension stripped off."
  (if (string-match "\\.[^\\.]+$" fname)
      (if x (substring fname 0 (match-beginning 0))
	(substring fname (match-beginning 0) nil))
    ""))

(defun w3-basepath (file &optional x)
  "Return the base pathname of FILE, or the actual filename if X is true"
  (cond
   ((null file) "")
   (x (file-name-nondirectory file))
   (t (file-name-directory file))))

(defun w3-replace-regexp (regexp to-string)
  "Quiet replace-regexp."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defun w3-find-highest-link-num ()
  "Find highest NAMEd link, so we can number on from there."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let ((lnkctr 0))
    (while (re-search-forward "<A[ \t\n]+NAME=" (point-max) t)
      (let* ((start (match-end 0))
	     (end (save-excursion
		    (re-search-forward "[ \t\n>]" (point-max) t)
		    (match-beginning 0)))
	     (subst (buffer-substring start end)))
	(and subst
	     (> (string-to-int subst) lnkctr)
	     (setq lnkctr (string-to-int subst)))))
    lnkctr))

(defun w3-clear-tmp-buffer ()
  "Clear the temporary W3 buffer"
  (set-buffer (get-buffer-create w3-working-buffer))
  (if buffer-read-only (toggle-read-only))
  (erase-buffer))  

;;;###autoload
(defun w3-preview-this-buffer ()
  "See what this buffer will look like when its formatted as HTML.
HTML is the HyperText Markup Language used by the World Wide Web to
specify formatting for text.  More information on HTML can be found at
info.cern.ch:/pub/www/doc."
  (interactive)
  (w3-fetch (concat "www://preview/" (buffer-name))))

(defun w3-gateway-initialize-host-process (host user pass)
  "Start up the remote host for use as a telnet gateway"
  (condition-case ()
      (delete-process w3-gateway-host-process)
    (error nil))
  (condition-case ()
      (kill-process w3-gateway-host-process)
    (error nil))
  (save-excursion
    (set-buffer (get-buffer-create w3-working-buffer))
    (erase-buffer)
    (let ((x (start-process "GATEWAY"
			    (get-buffer-create w3-working-buffer)
			    w3-gateway-host-program
			    host)))
      (if (not w3-gateway-program-interactive)
	  nil
	(while (not (progn
		      (goto-char (point-min))
		      (re-search-forward
		       w3-gateway-handholding-login-regexp nil t)))
	  (w3-accept-process-output w3-gateway-host-process)
	  (w3-lazy-message "Waiting for login prompt..."))
	(process-send-string x (concat user "\n"))
	(while (not (progn
		      (goto-char (point-min))
		      (re-search-forward
		       w3-gateway-handholding-password-regexp nil t)))
	  (w3-accept-process-output w3-gateway-host-process)
	  (w3-lazy-message "Waiting for password prompt..."))
	(process-send-string x (concat pass "\n")))
      (while (not (progn
		    (goto-char (point-min))
		    (re-search-forward
		     w3-gateway-host-prompt-pattern nil t)))
	(w3-accept-process-output w3-gateway-host-process)
	(w3-lazy-message "Waiting for shell prompt..."))
      (setq w3-gateway-host-process x))))

(defun w3-kill-process (proc)
  "Kill the process PROC"
  (cond
   ((eq w3-gateway-method 'native) (delete-process proc))
   ((eq w3-gateway-method 'program) (kill-process proc))
   ((eq w3-gateway-method 'host)
    (save-excursion
      (set-buffer (process-buffer proc))
      (interrupt-process proc)
      (erase-buffer)))
   (t (error "Unknown w3-gateway-method %S" w3-gateway-method))))

(defun w3-accept-process-output (proc)
  "Allow any pending output from subprocesses to be read by Emacs.
It is read into the process' buffers or given to their filter functions.
Where possible, this will not exit until some output is received from PROC,
or 1 second has elapsed."
  (if (or w3-running-FSF19 w3-running-old-lemacs
	  w3-running-lemacs w3-running-epoch)
      (accept-process-output proc 1)
    (accept-process-output)))

(defun w3-process-status (proc)
  "Return the process status of a w3 buffer"
  (cond
   ((memq w3-gateway-method '(native program)) (process-status proc))
   ((eq w3-gateway-method 'host)
    (if (memq (process-status proc) '(stop exit signal closed))
	'exit
      (save-excursion
	(set-buffer (process-buffer proc))
	(goto-char (point-min))
	(if (re-search-forward w3-gateway-host-prompt-pattern nil t)
	    'exit
	  'open))))
   (t (error "Unkown w3-gateway-method %S" w3-gateway-method))))  

(defun w3-open-stream (name buffer host service)
  "Open a stream to a host"
  (let ((w3-gateway-method (if (and w3-gateway-local-host-regexp
				    (string-match w3-gateway-local-host-regexp
						  host))
			       'native
			     w3-gateway-method)))
    (and (eq w3-gateway-method 'tcp)
	 (require 'tcp)
	 (setq w3-gateway-method 'native))
    (cond
     ((eq w3-gateway-method 'native)
      (let ((proc (open-network-stream name buffer host service)))
	(if (boundp 'MULE) (w3-inhibit-code-conversion proc buffer))
	proc))
     ((eq w3-gateway-method 'host)
      (if (or (null w3-gateway-host-process)
	      (not (processp w3-gateway-host-process))
	      (not (memq (w3-process-status w3-gateway-host-process)
			 '(run open))))
	  (w3-gateway-initialize-host-process w3-gateway-host
					      w3-gateway-host-username
					      w3-gateway-host-password))
      (save-excursion
	(set-process-buffer w3-gateway-host-process
			    (get-buffer-create w3-working-buffer))
	(set-buffer (get-buffer-create w3-working-buffer))
	(erase-buffer)
	(process-send-string w3-gateway-host-process
			     (concat w3-gateway-host-program " "
				     host " " service "\n"))
	(while (not
		(progn
		  (goto-char (point-min))
		  (re-search-forward
		   w3-gateway-host-program-ready-regexp nil t)))
	  (w3-accept-process-output w3-gateway-host-process)
	  (w3-lazy-message "Waiting for remote process to initialize..."))
	(delete-region (point-min) (match-end 0))
	w3-gateway-host-process))
     ((eq w3-gateway-method 'program)
      (let ((proc (start-process name buffer w3-gateway-telnet-program host
				 (int-to-string service)))
	    (tmp nil))
	(if (boundp 'MULE) (w3-inhibit-code-conversion proc buffer))
	(save-excursion
	  (set-buffer buffer)
	  (setq tmp (point))
	  (while (not (progn
			(goto-char (point-min))
			(re-search-forward 
			 w3-gateway-telnet-ready-regexp nil t)))
	    (w3-accept-process-output proc))
	  (delete-region tmp (point))
	  (goto-char (point-min))
	  (if (re-search-forward "connect:" nil t)
	      (progn
		(condition-case ()
		    (delete-process proc)
		  (error nil))
		(w3-replace-regexp ".*connect:.*" "")
		nil)
	    proc))))
     (t (error "Unknown w3-gateway-method %S" w3-gateway-method)))))

(defun w3-unhex (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

(defun w3-unhex-string (str)
  "Remove %XXX embedded spaces, etc in a url"
  (setq str (or str ""))
  (let ((tmp ""))
    (while (string-match "%[0-9a-f][0-9a-f]" str)
      (let* ((start (match-beginning 0))
	     (ch1 (w3-unhex (elt str (+ start 1))))
	     (code (+ (* 16 ch1)
		      (w3-unhex (elt str (+ start 2))))))
	(setq tmp
	      (concat 
	       tmp (substring str 0 start)
	       (char-to-string code)))
	(setq str (substring str (match-end 0)))))
    (setq tmp (concat tmp str))
    tmp))

(defun w3-rassoc (key list)
  "An 'rassoc' function - don't want to bother with loading cl just
for this function"
  (let ((found nil))
    (while (and list (not found))
      (if (equal (cdr (car list)) key) (setq found (car list)))
      (setq list (cdr list)))
    found))

(defun w3-insert-entities-in-string (str)
  "Remove special characters in STR and replace them with HTML[+] entities"
  (mapconcat
   (function
    (lambda (x)
      (cond
       ((= x ?<) "&lt;")
       ((= x ?>) "&gt;")
       ((= x ?&) "&amp;")
       ((= x ? ) "&ensp;")
       (t (char-to-string x))))) str ""))

(defun w3-fix-entities-in-string (str)
  "Remove &xxx; entities in string STR"
  (let ((tmp "")
	(regexp (concat "\\(" (mapconcat (function (lambda (x) (car x)))
					 w3-html-entities "\\|") "\\)"))
	(x nil))
    (while (string-match regexp str)
      (setq x (cdr (assoc (w3-match str 1) w3-html-entities))
	    tmp (format "%s%s%s" tmp (substring str 0 (match-beginning 0)) x)
	    str (substring str (match-end 0))))
    (setq tmp (concat tmp str))
    tmp))    
       
(defun w3-edit-source ()
  "Edit the html document just retrieved"
  (set-buffer w3-working-buffer)
  (let ((ttl (format "Editing %s Annotation: %s"
		     (cond
		      ((eq w3-editing-annotation 'group) "Group")
		      ((eq w3-editing-annotation 'personal) "Personal")
		      (t "Unknown"))
		     (w3-basepath w3-current-file t)))
	(str (buffer-string)))
    (set-buffer (get-buffer-create ttl))
    (w3-insert str)
    (kill-buffer w3-working-buffer)))

(defun w3-clean-text ()
  "Clean up a buffer after telnet (trash at beginning, connection closed)"
  (set-buffer w3-working-buffer)
  (w3-replace-regexp "Connection closed by.*" "")
  (w3-replace-regexp "Process WWW.*" ""))

(defun w3-source ()
  "Show the source of a file"
  (let ((tmp (buffer-name (generate-new-buffer "Document Source"))))
    (set-buffer w3-working-buffer)
    (kill-buffer tmp)
    (rename-buffer tmp)
    (set-buffer-modified-p nil)
    (buffer-enable-undo)
    (if w3-mutable-windows (pop-to-buffer tmp) (switch-to-buffer tmp))))

(defun w3-uncompress ()
  "Uncompress a file"
  (set-buffer w3-working-buffer)
  (let ((extn (w3-file-extension w3-current-file)))
    (if (assoc extn w3-uncompressor-alist)
	(progn
	  (message "Uncompressing")
	  (shell-command-on-region (point-min) (point-max)
				   (cdr (assoc extn w3-uncompressor-alist))
				   t)))))

(defun w3-filter (proc string)
  (save-excursion
    (set-buffer w3-working-buffer)
    (insert string)
    (if (string-match "\nConnection closed by" string)
	(progn (set-process-filter proc nil)
	       (w3-sentinel proc string))))
  string)


(defun w3-sentinel (proc string)
  (set-buffer w3-working-buffer)
  (if (boundp 'after-change-functions)
      (remove-hook 'after-change-functions 'w3-after-change-function))
  (if w3-be-asynchronous
      (progn
	(w3-clean-text)
	(if (boundp 'MULE) (w3-convert-code-for-mule w3-current-type
						     (w3-view-url t)))
	(cond
	 ((not (get-buffer w3-working-buffer)) nil)
	 ((w3-mime-response-p) (w3-parse-mime-headers))
	 ((w3-member w3-current-server w3-bad-server-list) nil)
	 (t
	  (setq w3-bad-server-list
		(cons w3-current-server w3-bad-server-list))))
	(if (not w3-current-mime-type)
	    (setq w3-current-mime-type (w3-extension-to-mime
					(w3-file-extension
					 w3-current-file))))))
  (let ((x (w3-build-continuation)))
    (while x
      (funcall (car x))
      (setq x (cdr x)))))

(defun w3-show-history-list ()
  "Format the w3-history-list prettily and show it to the user"
  (interactive)
  (w3-fetch "www://auto/history"))

(defun w3-save-as ()
  "Save a document to the local disk"
  (interactive)
  (let ((format (completing-read
		 "Format: "
		 '(("HTML Source") ("Formatted Text") ("LaTeX Source")
		   ("Binary"))
		 nil t))
	(fname (expand-file-name
		(read-file-name "File name: " default-directory)))
	(url (w3-view-url t)))
    (cond
     ((equal "Binary" format)
      (if (not w3-current-source)
	  (let ((w3-be-asynchronous nil))
	    (w3-retrieve url))))
     ((equal "HTML Source" format)
      (if (not w3-current-source)
	  (let ((w3-be-asynchronous nil))
	    (w3-retrieve url))		; Get the document if necessary
	(let ((txt w3-current-source))
	  (set-buffer (get-buffer-create w3-working-buffer))
	  (insert txt)))
      (goto-char (point-min))
      (insert (format "<BASE HREF=\"%s\">\n" url)))
     ((equal "Formatted Text" format)
      nil)				; Do nothing - we have the text already
     ((equal "LaTeX Source" format)
      (if (not w3-current-source)
	  (let ((w3-be-asynchronous nil))
	    (w3-retrieve url))		; Get the file
	(let ((txt w3-current-source))
	  (set-buffer (get-buffer-create w3-working-buffer))
	  (insert txt)))
      (w3-convert-html-to-latex)))	; Convert to LaTeX
    (write-region (point-min) (point-max) fname)))

(defun w3-upcase-region (st nd)
  "Uppercase a region of text, ignoring text within < and >"
  (save-excursion
    (goto-char st)
    (while (re-search-forward "\\(<[^>]+>\\)\\|\\(&[^;]+;\\)" nd t)
      (upcase-region st (match-beginning 0))
      (setq st (match-end 0)))
    (upcase-region st nd)))
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to parse out <A> tags and replace it with a hyperlink zone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-match (s x)
  "Return regexp match x in s."
  (substring s (match-beginning x) (match-end x)))

(defun w3-build-links-list ()
  "Build links out of url specs in the temporary buffer.  This function
looks in the buffer pointed to by w3-working-buffer.  The links will be
fully usable by w3-follow-link, etc."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (if buffer-read-only (toggle-read-only))
  (let* ((cur-id "")
	 (cur-txt "")
	 (cur-urn "")
	 (cur-rel "")
	 (cur-href "")
	 (cur-rev "")
	 (cur-title "")
	 (cur-meth "")
	 (been-visited nil)
	 (st-del "")
	 (nd-del "")
	 )
    (goto-char (point-min))
    (while (re-search-forward w3-link-begin-regexp nil t)
      (let* ((start (match-beginning 0))
	     (cur-url (prog1
			  (w3-parse-args (match-beginning 1) (match-end 1))
			(replace-match " ")))
	     (end   (if (re-search-forward w3-link-end-regexp nil t)
			(prog1
			    (match-beginning 0)
			  (replace-match ""))
		      (progn
			(end-of-line)
			(point)))))
	(save-excursion
	  (goto-char start)
	  (skip-chars-forward " \t")
	  (setq start (point))
	  (goto-char end)
	  (skip-chars-backward " \t")
	  (setq end (point)))
	(if (< end start) (setq cur-txt end
				end start
				start cur-txt))
	(setq cur-id (or (cdr (assoc "name" cur-url)) "")
	      cur-href (cdr (assoc "href" cur-url))
	      cur-rel (cdr (assoc "ref" cur-url))
	      cur-rev (cdr (assoc "rev" cur-url))
	      cur-urn (cdr (assoc "urn" cur-url))
	      cur-title (cdr (assoc "title" cur-url))
	      cur-meth (cdr (assoc "methods" cur-url))
	      cur-txt (w3-fix-entities-in-string
		       (buffer-substring start end)))
	(if (and cur-href
		 (not (string-match w3-nonrelative-link cur-href)))
	    (setq cur-href (w3-parse-relative-link cur-href)))
	(setq been-visited (w3-have-visited-url cur-href))
	(cond
	 ((and (eq w3-delimit-links 'linkname) cur-href)
	  (goto-char end)
	  (w3-insert (concat (if been-visited "{" "[") cur-id
			     (if been-visited "}" "{"))))
	 ((and (not (null w3-delimit-links)) cur-href)
	  (setq st-del (if been-visited (cdr w3-link-start-delimiter)
			 (car w3-link-start-delimiter))
		nd-del (if been-visited (cdr w3-link-end-delimiter)
			 (car w3-link-end-delimiter)))
	  (goto-char start)
	  (skip-chars-forward " \\\t\\\n")
	  (w3-insert st-del)
	  (goto-char (+ end (length st-del)))
	  (skip-chars-backward " \\\t\\\n")
	  (w3-insert nd-del)
	  (setq end (+ end (length st-del) (length nd-del)))))
	(and w3-link-delimiter-info
	     (w3-insert (funcall w3-link-delimiter-info cur-href)))
	(if cur-href
	    (w3-add-zone start end (or (and been-visited w3-visited-node-style)
				       w3-node-style)
			 (list 'w3 cur-id cur-href cur-txt
			       cur-urn cur-rel cur-rev
			       cur-meth cur-title) t)
	  (w3-add-zone start end w3-default-style
		       (list 'w3 cur-id cur-href cur-txt
			     cur-urn cur-rel cur-rev
			     cur-meth cur-title)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to handle LINK attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-links ()
  "Parse out the LINK attributes.
This will take the <LINK> attributes out of the current w3-working-buffer
and return an assoc list of the form (Rel or rev tag . url)"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (if buffer-read-only (toggle-read-only))
  (let* ((cur-href "")
	 (result nil))
    (goto-char (point-min))
    (while (re-search-forward "<LINK[ \\\t]+" nil t)
      (let* ((start (prog1 (match-beginning 0) (replace-match "")))
	     (end (prog2 (re-search-forward ">" nil t) (point)
		    (replace-match "")))
	     (cur-lnk (prog1
			  (w3-parse-args start (1- end))
			(delete-region start (1- end)))))
	(setq cur-href (cdr (assoc "href" cur-lnk)))
	(if (and cur-href (not (string-match w3-nonrelative-link cur-href)))
	    (setq cur-href (w3-parse-relative-link cur-href)))
	(setq result (cons cur-lnk result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Embedded document/image handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-embed-text (data type)
  "Use DATA as extra text for this document."
   (if (equal type "text/html") (insert data)
     (insert "<pre>" data "</pre>")))

(defun w3-embed-postscript (data type)
  "Embed a LaTeX document"
  (let ((fname (w3-generate-unique-filename)))
    (save-excursion
      (set-buffer (get-buffer-create " *w3-temp*"))
      (erase-buffer)
      (insert data)
      (write-region (point-min) (point-max) fname 5)
      (call-process "pstoxbm" nil nil nil fname fname)
      (erase-buffer)
      (insert-file-contents fname)
      (condition-case ()
	  (delete-file fname)
	(error nil))
      (setq fname (buffer-string))
      (kill-buffer (current-buffer)))
    (w3-embed-image fname "image/xbm")))

(defun w3-embed-mpeg (data type)
  "Embed an mpeg movie in the buffer"
  (let ((fnam "w3-img-")
	(x 0))
    (while (file-exists-p (expand-file-name
			   (concat fnam x ".mpg") w3-temporary-directory))
      (setq x (1+ x)))
    (setq fnam (expand-file-name (concat fnam x ".mpg")
				 w3-temporary-directory))
    (save-excursion
      (set-buffer (get-buffer-create " *w3-temp*"))
      (erase-buffer)
      (insert data)
      (write-region (point-min) (point-max) fnam nil 5)
      (set-buffer w3-working-buffer)
      (w3-add-delayed-mpeg fnam (point)))))

(defun w3-embed-eqn (data type)
  "Embed an equation in the buffer"
  (let ((fname (w3-generate-unique-filename)))
    (save-excursion
      (set-buffer (get-buffer-create " *w3-temp*"))
      (erase-buffer)
      (insert ".EQ" data ".EN")
      (call-process-region (point-min) (point-max) "eqn" t nil nil "|"
			   "groff" ">" fname)
      (erase-buffer)
      (call-process "pstoxbm" fname fname)
      (insert-file-contents fname)
      (condition-case () (delete-file fname) (error nil))
      (setq fname (buffer-string))
      (kill-buffer (current-buffer)))
    (w3-embed-image fname "image/xbm")))

(defun w3-embed-image (data type)
  "Use DATA as an image of content-type TYPE and insert it in the buffer."
  (let ((fnam "w3-img-")
	(extn (car (w3-rassoc type w3-mime-extensions)))
	(x 0))
    (while (file-exists-p
	    (expand-file-name (concat fnam x extn) w3-temporary-directory))
      (setq x (1+ x)))
    (setq fnam (expand-file-name (concat fnam x extn)
				 w3-temporary-directory))
    (save-excursion
      (set-buffer (get-buffer-create " *w3temp*"))
      (erase-buffer)
      (insert data)
      (write-region (point-min) (point-max) fnam 5)
      (kill-buffer (current-buffer)))
    (insert "<img src=\"file:" fnam "\" alt=\"embedded data\">")))

(defun w3-handle-embeds ()
  "Handle <EMBED>....</EMBED> tags."
  (goto-char (point-min))
  (let ((args nil)			; Attributes for current embed
	(type nil)			; Content-type for current embed
	(cvtr nil)			; Converter to xbm for current embed
	(data nil)			; Data between <embed> and </embed>
	(src nil)			; Optional SRC attribute
	(st nil)			; Start of embed tag
	)
  (while (re-search-forward "<EMBED\\([^>]*\\)>" nil t)
    (setq args (prog1
		   (w3-parse-args (match-beginning 1) (match-end 1))
		 (replace-match ""))
	  type (or (cdr (assoc "type" args)) "text/plain")
	  src (cdr (assoc "src" args))
	  cvtr (cdr (w3-in-assoc type w3-embedded-data-converters))
	  st (point))
    (cond
     (src
      (save-excursion
	(let ((w3-working-buffer " *w3-temp*")
	      (w3-source t))
	  (if (string-match w3-nonrelative-link src)
	      nil
	    (setq src (w3-parse-relative-link src)))
	  (w3-retrieve src)
	  (setq data (buffer-string))
	  (kill-buffer (current-buffer)))))
     ((re-search-forward "</embed>" nil t)
      (setq data (buffer-substring st (match-beginning 0)))
      (delete-region st (match-end 0)))
     (t
      (message "Nonterminated <embed> tag, trying to cope.")
      (setq cvtr nil)))
    (and (fboundp cvtr) (funcall cvtr data type)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to handle formatting an html buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-personal-annotations ()
  "Take care of personal annotations"
  (w3-lazy-message "Finding personal annotations...")
  (let ((annos (w3-fetch-personal-annotations)))
    (if annos
	(progn
	  (goto-char (cond
		      ((eq w3-annotation-position 'bottom) (point-max))
		      ((eq w3-annotation-position 'top) (point-min))
		      (t (message "Bad value for w3-annotation-position")
			 (point-max))))
	  (w3-insert"<P><HR>\n<H1>Personal Annotations</H1><P><UL>")
	  (while annos
	    (w3-insert "\n<LI> " (car annos))
	    (setq annos (cdr annos)))
	  (w3-insert "</UL><HR>"))))
  (w3-lazy-message "Finding personal annotations... done."))

(defun w3-insert-headers ()
  "Insert some HTTP/1.0 headers if necessary"
  (w3-lazy-message "Inserting HTTP/1.0 headers...")
  (let ((hdrs w3-show-headers)
	x y)
    (goto-char (setq y (point-max)))
    (while hdrs
      (if (setq x (assoc (car hdrs) w3-current-mime-headers))
	  (w3-insert "<LI> <B>" (car x) "</B> :" (w3-insert-entities-in-string
						  (cdr x))))
      (setq hdrs (cdr hdrs)))
    (if (= y (point-max))
	nil
      (w3-insert "</UL>")
      (goto-char y)
      (w3-lazy-message "Inserting HTTP/1.0 headers... done.")
      (w3-insert "<HR><UL>"))))

(defun w3-fixup-bad-html ()
  "Fix lots of bad html markup"
  (message "Checking for bad HTML...")
  (w3-replace-regexp "<\\(TBL[^>]*\\)>" "<\\1>\n<PRE>")
  (w3-replace-regexp "</TBL>" "</TBL>\n</PRE>")
  (w3-replace-regexp "<TEXTAREA" "<PRE><TEXTAREA")
  (w3-replace-regexp "</TEXTAREA>" "</TEXTAREA></PRE>")
  (w3-replace-regexp "<LISTING>" "<PRE>")
  (w3-replace-regexp "</LISTING>" "</PRE>")
  (w3-replace-regexp "<DL[^>]*>" "<DL>")
  (w3-replace-regexp "\\(</H[0-9][^>]*>\\)" "\\1\n")
  (goto-char (point-min))
  (let (st)
    (while (re-search-forward
	    (concat "</\\(H[0-9]\\|PRE\\|DL\\|OL\\|UL\\|DIR\\|MENU\\)[^>]*>"
		    "[ \\\t\\\n]*\\([^ \\\t\\\n]\\)") nil t)
      (setq st (match-beginning 2))
      (cond 
       ((eq (char-after st) ?<)		; Its a markup tag
	(goto-char st)
	(cond
	 ((looking-at "<[Pp][ >]")	; Good, they have a paragraph
	  nil)
	 ((looking-at "<[DdOoUu][Ll]")
	  nil)
	 ((looking-at "<[dD][iI][rR]")
	  nil)
	 ((looking-at "<[Mm][Ee][Nn][Uu]")
	  nil)
	 (t
	  (goto-char (1- st))
	  (insert "<P>"))))
       (t				; No markup immediately before header
	(goto-char (1- st))
	(insert "<P>")))))
  (message "Checking for bad HTML... done."))

(defun w3-kill-comments ()
  "Take care of SGML comments in a buffer."
  (goto-char (point-min))
  (w3-lazy-message "Removing SGML comments...")
  (let (st nd)
    (while (re-search-forward "[ \\\t\\\n]*<!--" nil t)
      (setq st (match-beginning 0)
	    nd (if (re-search-forward "-->[ \\\t\\\n]*" nil t)
		   (match-end 0)
		 (end-of-line)
		 (point)))
      (delete-region st nd)
      (if (or (memq (or (char-after (point)) 0) '(?  ?\t ?\n))
	      (memq (or (char-after (1- (point))) 0) '(?  ?\t ?\n)))
	  nil
	(insert " "))))
  (w3-lazy-message "Removing SGML comments... done."))
    
(defun w3-prepare-buffer (&optional no-display)
  "Function to prepare w-buffer for processing.  This will completely
reformat a buffer - if you just want to parse out links, see the documentation
for w3-build-links-list."
  (w3-lazy-message "Parsing...")
  (set-buffer w3-working-buffer)
  (set-syntax-table w3-parse-args-syntax-table)
  (w3-handle-personal-annotations)
  (w3-insert-headers)
  (setq w3-current-source (buffer-string))
  (run-hooks 'w3-file-prepare-hooks)
  (setq fill-column (- (or w3-strict-width (window-width)) w3-right-border))
  (w3-replace-regexp (format "[%c%c%c]" ?\r ? ?) "")
  (let ((case-fold-search t)
	(ttl "")
	(pltxt nil))
    (goto-char (point-min))
    (w3-kill-comments)
    (goto-char (point-min))
    (if (re-search-forward "<PLAINTEXT>" nil t)
	(progn
	  (replace-match "")
	  (setq pltxt (buffer-substring (point) (point-max)))
	  (delete-region (point) (point-max))))
    (w3-handle-embeds)
    (w3-fixup-bad-html)
    (w3-balance-pre)
    (w3-balance-xmp)
    (w3-handle-arbitrary-tags)
    (w3-check-index)
    (w3-replace-regexp "<LIT>" "<PRE>")
    (w3-replace-regexp "</LIT>" "</PRE>")
    (w3-fix-xmp)
    (w3-fix-pre)
    (w3-fix-render-hints)
    (w3-handle-footnotes)
    (w3-handle-notes)
    (setq w3-current-links (w3-handle-links))
    (w3-fix-extras)
    (w3-handle-generic-emphasis)
    (goto-char (point-min))
    (w3-replace-regexp "[ \\\t]*<SP>[ \\\t]*" "<SP>")
    (w3-replace-regexp "[ \\\t]*&nbsp;[ \\\t]*" "&nbsp;")
    (w3-handle-whitespace)
    (w3-handle-headers)
    (w3-restore-pre)
    (goto-char (point-min))
    (let* ((x (w3-handle-base))
	   (w3-delay-image-loads t)
	   (w3-current-file (and (not (car x)) w3-current-file))
	   (w3-current-server (and (not (car x)) w3-current-server))
	   (w3-current-type (and (not (car x)) w3-current-type))
	   (w3-current-user (and (not (car x)) w3-current-user))
	   (w3-current-port (and (not (car x)) w3-current-port)))
      (cond
       ((car x)				; there was a <base> tag
	(cond
	 ((eq (car x) 'http)
	  (setq x (w3-grok-http-href (cdr x))
		w3-current-type "http"
		w3-current-server (nth 0 x)
		w3-current-port (nth 1 x)
		w3-current-file (nth 2 x)))
	 ((or (eq (car x) 'file)
	      (eq (car x) 'ftp))
	  (setq x (w3-grok-file-href (cdr x))
		w3-current-type (and (nth 0 x) "ftp")
		w3-current-user (nth 0 x)
		w3-current-server (nth 1 x)
		w3-current-file (nth 2 x)))
	 ((eq (car x) 'gopher)
	  (setq x (w3-grok-gopher-href (cdr x))
		w3-current-type "gopher"
		w3-current-server (nth 0 x)
		w3-current-port (nth 1 x)
		w3-current-file (nth 2 x)))))
       (t nil))
      (w3-build-links-list)
      (w3-handle-graphics))
    (w3-handle-forms)
    (w3-do-lists)
    (w3-replace-regexp "<LI>" "\n\t*")
    (w3-replace-regexp "<DT>" "\n<DT>")
    (w3-replace-regexp "<DD>" "\n\t*")
    (goto-char (point-min))
    (let ((st (if (re-search-forward "<title>" nil t)
		  (prog1
		      (match-beginning 0)
		    (replace-match "")) nil))
	  (nd (if (re-search-forward "</title[ \\\t\\\n]*>" nil t)
		  (prog1
		      (match-beginning 0)
		    (replace-match ""))
		nil)))
      (if st
	  (progn
	    (setq ttl (w3-fix-entities-in-string
		       (w3-strip-leading-spaces
			(w3-eat-trailing-space
			 (buffer-substring st nd)))))
	    (delete-region st nd))
	(setq ttl (w3-basepath w3-current-file t)))
      (if (> (length ttl) 50) (setq ttl (substring ttl 0 50)))
      (setq ttl (w3-generate-new-buffer-name ttl)))
    (w3-fix-paragraphs)
    (w3-replace-regexp "<X>\\\n+\\(\\\t*\\)<W3BR>" "\n\\1")
    (w3-replace-regexp "<SP>" " ")
    (w3-fix-unknown-tags)
    (w3-fix-entities)
    (w3-restore-xmp)
    (goto-char (point-min))
    (set-buffer w3-working-buffer)
    (if pltxt (progn (goto-char (point-max)) (w3-insert "\n" pltxt)))
    (goto-char (point-min))
    (while (looking-at "\\\n")
      (delete-char 1))
    (if (boundp 'MULE)
	(w3-mule-attribute-zones w3-zones-list w3-mule-attribute))
    (w3-fix-extent-endpoints)
    (run-hooks 'w3-file-done-hooks)
    (if (not no-display)
	(progn
	  (w3-mode)
	  (rename-buffer ttl)
	  (if w3-mutable-windows
	      (pop-to-buffer ttl)
	    (switch-to-buffer ttl))
	  (goto-char (point-min))
	  (if (and (bufferp w3-current-last-buffer)
		   (save-excursion
		     (set-buffer w3-current-last-buffer)
		     (eq major-mode 'w3-mode))
		   (not w3-keep-old-buffers))
	      (kill-buffer w3-current-last-buffer))
	  (if w3-keep-history
	      (let ((url (w3-view-url t)))
		(if (and (not (assoc url w3-history-list))
			 (not (equal url "file:historylist")))
		    (setq w3-history-list
			  (cons (cons url ttl) w3-history-list)))))
	  (w3-lazy-message "Done.")
	  (cond
	   ((not (fboundp 'w3-insert-graphic)) nil)	; No graphics abilities
	   ((and w3-delay-image-loads w3-delay-mpeg-loads)
	    nil)
	   (t
	    (message "Processing images...")		; Grab the images
	    (w3-load-delayed-images)
	    (if (not w3-delay-mpeg-loads) (w3-load-delayed-mpegs))))
	  (if w3-find-this-link
	      (w3-find-specific-link w3-find-this-link)
	    (goto-char (point-min)))
	  (if (not buffer-read-only) (toggle-read-only))
	  (set-buffer-modified-p nil)
	  (if (get-buffer "Conversion errors")
	      (switch-to-buffer-other-window "Conversion errors"))
	  (if w3-running-epoch (set-variable 'buffer-style w3-default-style))
	  (if w3-running-FSF19 (setq w3-zones-list (w3-only-links)))
	  (sit-for 0)))
    (message "")
    ttl))

(defun w3-handle-base ()
  "Handle BASE tag"
  (let (base url)
    (goto-char (point-min))
    (if (re-search-forward "<BASE\\([^>]+\\)>" nil t)
	(progn
	  (setq base (prog1
			 (w3-parse-args (match-beginning 1) (match-end 1))
		       (replace-match "")))
	  (setq url (cdr (assoc "href" base)))
	  (and (not url) (message "Malformed 'BASE' tag."))))
    (if (stringp url)
	(if (string-match "^\\([^:]+\\):/+" url)
	    (setq base (intern (downcase (w3-match url 1))))))
    (cons base url)))

(defun w3-handle-notes ()
  "Handle NOTE tags, as per the HTML+ 'Notes and Admonishments' section."
  (w3-lazy-message "Handling notices...")
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let (role img x)
    (while (re-search-forward "<NOTE\\([^>]*\\)>" nil t)
      (setq x (prog1
		  (w3-parse-args (match-beginning 1) (match-end 1))
		(replace-match "<HR><BR><B>"))
	    role (or (cdr (assoc "role" x)) "NOTE")
	    img (cdr (assoc "src" x)))
      (if img
	  (w3-insert (format "<IMG SRC=\"%s\" ALIGN=\"CENTER\">" img)))
      (w3-insert (format "<B>%s:</B>" role))))
  (w3-replace-regexp "</NOTE>" "<BR><HR>")
  (w3-lazy-message "Handling notices... done."))

(defun w3-handle-footnotes ()
  "Handle footnotes, margin notes, etc, from the HTML+ spec"
  (w3-lazy-message "Handling footnotes....")
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (if (re-search-forward "<FOOTNOTE>" nil t)
      (progn
	(goto-char (point-max))
	(w3-insert "<P>Footnotes<BR><HR>")
	(goto-char (point-min))))
  (let ((fcounter 1) st nd txt)
    (while (re-search-forward "<FOOTNOTE>" nil t)
      (setq st (prog1 (match-beginning 0) (replace-match ""))
	    nd (if (re-search-forward "</FOOTNOTE>" nil t)
		   (prog1
		       (match-beginning 0)
		     (replace-match ""))
		 (progn (end-of-line) (point)))
	    txt (buffer-substring st nd))
      (delete-region st nd)
      (goto-char st)
      (w3-insert (format "<A HREF=\"#w3-internal-footnote%d\">%d</A>"
		      fcounter fcounter))
      (goto-char (point-max))
      (w3-insert (format "<P ID=\"w3-internal-footnote%d\">%d. "
		      fcounter fcounter) txt)
      (setq fcounter (1+ fcounter))
      (goto-char (point-min)))
    (while (re-search-forward "<MARGIN>" nil t)
      (setq st (prog1 (match-beginning 0) (replace-match ""))
	    nd (if (re-search-forward "</MARGIN>" nil t)
		   (prog1
		       (match-beginning 0)
		     (replace-match ""))
		 (progn (end-of-line) (point)))
	    txt (buffer-substring st nd))
      (delete-region st nd)
      (goto-char st)
      (w3-insert (format "<A HREF=\"#w3-internal-footnote%d\">%d</A>"
		      fcounter fcounter))
      (goto-char (point-max))
      (w3-insert (format "<P ID=\"w3-internal-footnote%d\">%d. "
		      fcounter fcounter) txt)
      (setq fcounter (1+ fcounter))
      (goto-char (point-min))))
  (w3-lazy-message "Handling footnotes... done."))
       
(defun w3-fix-render-hints ()
  "Parse out the RENDER hints ala the HTML+ specification."
  (w3-lazy-message "Fixing custom render attributes...")
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let (x tag sty)
    (while (re-search-forward "<RENDER\\([^>]+\\)>" nil t)
      (setq x (prog1
		  (w3-parse-args (match-beginning 1) (match-end 1))
		(replace-match ""))
	    tag (cdr (assoc "tag" x))
	    sty (cdr (assoc "style" x)))
      (w3-replace-regexp (format "<%s>" tag) (format "<%s>" sty))
      (w3-replace-regexp (format "</%s>" tag) (format "</%s>" sty))))
  (w3-lazy-message "Fixing custom render attributes... done."))

(defun w3-handle-arbitrary-tags ()
  "Find occurences of <!ENTITY ...> and replace them correctly."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (while (re-search-forward
	  "<!ENTITY[ \\\t]+\\([^ ]*\\)[ \\\t]+\"\\([^\"]*\\)\">" nil t)
    (let ((entity (buffer-substring (match-beginning 1) (match-end 1)))
	  (defn   (buffer-substring (match-beginning 2) (match-end 2))))
      (replace-match "")
      (w3-replace-regexp (regexp-quote (format "&%s;" entity)) defn))))

(defun w3-balance-xmp ()
  "This function will attempt to balance embedded plaintext elements
<XMP> tags.  This is necessary or the parser will fail some critical
regular expression matches."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let* ((st (w3-count-occurences "<XMP>"))
	 (nd (w3-count-occurences "</XMP>"))
	 (df (- st nd)))
    (goto-char (point-max))
    (while (> df 0)
      (setq df (1- df))
      (w3-insert "</XMP>\n"))))

(defun w3-balance-pre ()
  "This function will attempt to balance embedded plaintext elements
(<PRE> tags).  This is necessary or the parser will fail some critical
regular expression matches."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let* ((st (w3-count-occurences "<PRE[^>]*>"))
	 (nd (w3-count-occurences "</PRE>"))
	 (df (- st nd)))
    (goto-char (point-max))
    (while (> df 0)
      (setq df (1- df))
      (w3-insert "</PRE>\n"))))

(defun w3-fix-extras ()
  "Replace <B>, <I>, etc tags in the buffer.  Appropriate zones will be
created, and highlighting will be added when possible."
  (w3-lazy-message "Doing textual highlighting...")
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (while (re-search-forward w3-style-regexp nil t)
    (let* ((st (match-beginning 0))
	   (dastyle (upcase (buffer-substring (match-beginning 1)
					      (match-end 1))))
	   (nd (progn
		 (replace-match "")
		 (if (re-search-forward (concat "</" dastyle ">") nil t)
		     (prog1
			 (match-beginning 0)
		       (replace-match ""))
		   (point))))
	   (sty (w3-lookup-style dastyle))
	   (ltrs (cdr (assoc dastyle w3-style-chars-assoc))))
      (w3-add-zone st nd sty '(w3style))
      (if (and ltrs w3-delimit-emphasis)
	  (progn
	    (goto-char nd)
	    (w3-insert (cdr ltrs))
	    (goto-char st)
	    (w3-insert (car ltrs))))))
  (w3-lazy-message "Doing textual highlighting... done."))

(defun w3-find-graphic-entity (entity)
  "Return where we found the bitmap for entity... this searches through
w3-icon-directory-list and tries to find the bitmap corresponding to entity."
  (let* ((retval (cdr (assoc entity w3-icon-path-cache)))
	 (done nil))
    (if retval nil
      (while (and w3-icon-directory-list (not done))
	(if (w3-file-exists (setq retval
				  (concat (car w3-icon-directory-list) 
					  entity)))
	    (setq done t)))
      (setq w3-icon-path-cache (cons (cons entity retval) w3-icon-path-cache)))
    retval))

(defun w3-fix-entities ()
  "Replace &#XXX with ASCII character XXX."
  (w3-lazy-message "Finding HTML+ entities...")
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (cond
   ((and (fboundp 'w3-insert-graphic) w3-delay-image-loads
	 (not w3-graphics-always-show-entities))
    (mapcar (function
	     (lambda (entry)
	       (goto-char (point-min))
	       (while (re-search-forward (car entry) nil t)
		 (replace-match "")
		 (w3-add-delayed-graphic
		  (w3-find-graphic-entity (car (cdr entry)))
		  (set-marker (make-marker) (point)) 'center
		  (or (cdr (cdr entry)) "")))))
	    w3-graphics-entities-alist))
   ((fboundp 'w3-insert-graphic)
    (mapcar (function
	     (lambda (entry)
	       (goto-char (point-min))
	       (while (re-search-forward (car entry) nil t)
		 (replace-match "")
		 (w3-insert-graphic (list
				     (w3-find-graphic-entity
				      (car (cdr entry))))
				    (point) 'center
				    (or (cdr (cdr entry)) "")))))
	    w3-graphics-entities-alist))
   (t
    (mapcar (function
	     (lambda (entry)
	       (goto-char (point-min))
	       (w3-replace-regexp (car entry)
				  (or (cdr (cdr entry)) ""))))
	    w3-graphics-entities-alist)))
  (let ((case-fold-search nil))
    (while (re-search-forward "&#\\([0-9]+\\);*" nil t)
      (replace-match (char-to-string 
		      (string-to-int (buffer-substring (match-beginning 1)
						       (match-end 1))))))
    (mapcar (function (lambda (x) (w3-replace-regexp (car x) (cdr x))))
	    w3-html-entities))
  (goto-char (point-min))
  (w3-lazy-message "Finding HTML+ entities..."))

(defun w3-fix-pre ()
  "Extract <PRE> fields, and put them back in later."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (setq w3-pre-data nil
	w3-pre-data-count 0)
  (while (re-search-forward "<PRE[^>]*>" nil t)
    (let* ((start (prog1 (match-beginning 0) (replace-match "")))
	   (end (prog2
		    (re-search-forward "</PRE>" nil t)
		    (match-beginning 0)
		  (replace-match "")))
	   (repl (not (string= "" (w3-eat-trailing-space
				   (buffer-substring start end))))))
      (cond
       (repl 
	(setq w3-pre-data-count (1+ w3-pre-data-count)
	      w3-pre-data (cons (list w3-pre-data-count
				      (buffer-substring start end))
				w3-pre-data))
	(delete-region start end)
	(goto-char start)
	(w3-insert "***PREDATA" (int-to-string w3-pre-data-count)))
       (t (delete-region start end))))))

(defun w3-restore-pre (&optional done)
  "Restore the <PRE> fields"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (while (> w3-pre-data-count 0)
    (re-search-forward (concat "***PREDATA" (int-to-string w3-pre-data-count))
		       nil t)
    (replace-match (concat (if (not done) "<PRE>" "\n")
			   (car (cdr (assoc w3-pre-data-count w3-pre-data)))
			   (if (not done) "</PRE>" "\n")) t t)
    (goto-char (point-min))
    (setq w3-pre-data-count (1- w3-pre-data-count))))

(defun w3-fix-xmp ()
  "Extract <XMP> fields, and put them back in later."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (setq w3-xmp-data nil
	w3-xmp-data-count 0)
  (while (re-search-forward "<XMP>" nil t)
    (let* ((start (match-beginning 0))
	   (end (progn (re-search-forward "</XMP>" nil t)
		       (point))))
      (setq w3-xmp-data-count (1+ w3-xmp-data-count)
	    w3-xmp-data (cons (list w3-xmp-data-count
				    (buffer-substring start end)) w3-xmp-data))
      (delete-region start end)
      (goto-char start)
      (w3-insert "***XMPDATA" (int-to-string w3-xmp-data-count)))))

(defun w3-restore-xmp ()
  "Restore the <XMP> fields"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (while (> w3-xmp-data-count 0)
    (goto-char (point-min))
    (re-search-forward (concat "***XMPDATA" (int-to-string w3-xmp-data-count))
		       nil t)
    (replace-match (concat "\n"
			   (substring
			    (car (cdr (assoc w3-xmp-data-count w3-xmp-data)))
			    5 -6) "\n") t t)
    (setq w3-xmp-data-count (1- w3-xmp-data-count))))

(defun w3-check-index ()
  "Check to see if this is an indexed file.  If it is, update the mode line"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (if (re-search-forward "\\\n*<ISINDEX>\\\n*" nil t)
      (progn
	(setq w3-current-isindex t)
	(replace-match
	 (if (and w3-use-forms-index
		  (equal w3-current-type "http"))
	     (concat
	      "<FORM>\nThis is a searchable index.  Search for:"
	      " <INPUT NAME=\"isindex\"><P></FORM>")
	   "") t))
    (setq w3-current-isindex nil)))

(defun w3-handle-whitespace ()
  "Fix newlines, tabs, and spaces"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (w3-replace-regexp "<P>\\\n*" "<P>")
  (w3-replace-regexp "\\\n" " ")
  (w3-replace-regexp "\t" " ")
  (w3-replace-regexp "  +" " ")
  (w3-replace-regexp "\\\. +" ".  "))

(defun w3-handle-headers ()
  "Do the headers"
  (w3-lazy-message "Parsing headers...")
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (while (re-search-forward
	  "[ \\\t\\\n]*\\(<P[^>]*>\\)*[ \\\t\\\n]*<H\\([0-9]+\\)\\([^>]*\\)>"
	  nil t)
    (let* ((siz (buffer-substring (match-beginning 2) (match-end 2)))
	   (tags (buffer-substring (match-beginning 3) (match-end 3)))
	   x y
	   (st (set-marker
		(make-marker)
		(prog1
		    (match-beginning 0)
		  (setq x 
			(if (match-beginning 1)
			    (w3-eat-trailing-space
			     (w3-strip-leading-spaces
			      (buffer-substring (match-beginning 1)
						(match-end 1)))) "<P>"))
		  (replace-match ""))))
	   (end (set-marker
		 (make-marker)
		 (progn
		  (if (re-search-forward
		       (concat 
		       (format "</H%s>" siz)
		       "[ \\\t\\\n]*\\(<P[^>]*>\\)*") nil t)
		      (prog1
			  (match-beginning 0)
			(setq y
			      (if (match-beginning 1)
				  (w3-eat-trailing-space
				   (w3-strip-leading-spaces
				    (buffer-substring
				     (match-beginning 1) (match-end 1))))
				"<P>"))
			(replace-match ""))
		    (progn (end-of-line) (setq y "<P>") (point))))))
	   (forms (cdr (assoc siz w3-header-chars-assoc))))
      (w3-add-zone st end w3-header-style
		   (cons 'w3header
			 (list
			  (if (string-match "ID=\"\\([^\"]+\\)\"" tags)
			      (substring tags (match-beginning 1)
					 (match-end 1))
			    nil) nil nil nil nil nil nil nil)))
      (if (and forms w3-delimit-emphasis)
	  (let ((len (w3-length-after-parsing (buffer-substring st end))))
	    (setq len (if (> len (- (or (window-width) w3-strict-width)
				    w3-right-border))
			  (- (or (window-width) w3-strict-width)
			     w3-right-border)
			len))
	    (and (nth 2 forms) (funcall (nth 2 forms) st end))
	    (goto-char end)
	    (and (nth 0 forms) (w3-insert "<BR>"
					  (make-string len (nth 0 forms))
					  "<BR>"))
	    (w3-insert y)
	    (goto-char st)
	    (w3-insert x)
	    (and (nth 1 forms) (w3-insert "<BR>"
					  (make-string len (nth 1 forms))
					  "<BR>")))
	(progn
	  (goto-char end) (w3-insert y)
	  (goto-char st) (w3-insert x)))))
  (goto-char (point-min))
  (w3-lazy-message "Parsing headers... done."))

(defun w3-fix-horizontal-rules ()
  "Replace all the <HR> tags"
  (goto-char (point-min))
  (while (re-search-forward "<[Hh][rR]>" nil t)
    (replace-match (format "<p>%s<p>"
			   (make-string (- (or w3-strict-width
					       (window-width))
					   w3-right-border)
					w3-horizontal-rule-char)))))

(defun w3-fix-unknown-tags (&optional pt recur)
  "Remove unknown tags in a buffer"
  (w3-lazy-message "Removing unknown tags...")
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (if (re-search-forward "<\\(PRE\\|XMP\\)>" nil t)
      (let ((st (set-marker (make-marker) (if pt pt (point-min))))
	    (nd (set-marker (make-marker) (match-beginning 1)))
	    (tp (buffer-substring (match-beginning 1) (match-end 1))))
	(replace-match "")
	(save-restriction
	  (narrow-to-region st nd)
	  (w3-replace-regexp "<[^>]*>" ""))
	(re-search-forward (format "</%s>" tp) nil t)
	(replace-match "")
	(w3-fix-unknown-tags (point) t))
    (narrow-to-region (point) (point-max))
    (w3-replace-regexp "<[^>]*>" "")
    (widen))
  (w3-lazy-message "Removing unknown tags... done."))

(defun w3-fix-paragraphs-in-region ()
  "Fill paragraphs in the visible part of the buffer"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (w3-replace-regexp "<[bB][Rr]> *" (concat fill-prefix "<PW3>"))
  (w3-replace-regexp "\\\n\\\n+\\\t" "\n\t")
  (w3-replace-regexp "^ +" "")
  (goto-char (point-min))
  (let (ptag st align args eol next-p next-t nd)
    (while (re-search-forward "<P\\([^>]*\\)>[ \\\n]*" nil t)
      (setq st (set-marker (make-marker) (match-beginning 0))
	    args (buffer-substring (match-beginning 1) (match-end 1))
	    args (w3-parse-args-string
		  (prog1
		      (if (equal "W3" args) (concat "ALIGN="align) args)
		    (replace-match (if (equal "W3" args) ""
				     (concat "\n\n" fill-prefix)))))
	    ptag (cdr (assoc "id" args))
	    align (or (cdr (assoc "align" args)) "left")
	    eol (save-excursion (end-of-line) (point))
	    next-t (save-excursion
		     (if (re-search-forward "\\\t" nil t) (match-beginning 0)
		       eol))
	    next-p (save-excursion
		     (if (re-search-forward "<P" nil t) (match-beginning 0)
		       eol))
	    nd (set-marker (make-marker) (min eol next-p next-t)))
      (if ptag
	  (w3-add-zone st nd w3-default-style (cons 'w3par (list ptag))))
      (cond
       ((equal "left" align)		; Normal left justification
	(fill-region st nd))
       ((equal "justify" align)		; Fully justified text
	(fill-region st nd t))
       ((equal "center" align)		; Center each line
	(let ((fill-column (- fill-column 7)))
	  (fill-region st nd)
	  (center-region st nd)
	  (goto-char st)
	  (while (re-search-forward "^" nd t)
	    (replace-match "    "))))
       ((equal "right" align)		; Right justified
	(let ((fill-column (- fill-column 7)))
	  (fill-region st nd t)))
       ((equal "indent" align)		; Indent extra
	(let ((fill-prefix (concat "\t" fill-prefix)))
	  (goto-char st)
	  (skip-chars-forward " \t\n")
	  (insert "\t")
	  (goto-char nd)
	  (fill-region st nd))))))
  (goto-char (point-min))
  (w3-replace-regexp "\\\n\\\n+" "\n\n")
  (w3-replace-regexp "<[sS][Pp]>" " "))

(defun w3-fix-paragraphs (&optional pt recur)
  "Fix filling of paragraphs in a new buffer"
  (w3-lazy-message "Filling paragraphs...")
  (set-buffer w3-working-buffer)
  (goto-char (if pt pt (point-min))) 
  (w3-fix-horizontal-rules)
  (goto-char (if pt pt (point-min)))
  (if (re-search-forward "<\\(PRE\\|XMP\\)>" nil t)
      (let ((st (if pt pt (point-min)))
	    (nd (- (point) 5))
	    (tp (buffer-substring (match-beginning 1) (match-end 1))))
	(save-restriction
	  (narrow-to-region st nd)
	  (w3-fix-paragraphs-in-region))
	(re-search-forward (format "</%s>" tp) nil t)
	(w3-fix-paragraphs (point) t))
    (narrow-to-region (point) (point-max))
    (w3-fix-paragraphs-in-region)
    (widen))
  (w3-lazy-message "Filling paragraphs..."))

(defun w3-add-delayed-mpeg (src st)
  "Add a delayed mpeg for the current buffer."
  (setq w3-delayed-movies (cons (cons src (set-marker (make-marker) st))
				w3-delayed-movies))
  (w3-insert (concat "[MPEG(" (w3-basepath src t) ")]"))
  (w3-add-zone st (point) nil (list 'w3mpeg src st)))

(defun w3-add-delayed-graphic (src st align alt)
  "Add a delayed image for the current buffer."
  (setq st (set-marker (make-marker) st)
	w3-delayed-images (cons (list src st align alt) w3-delayed-images))
  (insert alt)
  (if (string= alt "") nil
    (w3-add-zone st (point) nil (list 'w3delayed src st align alt))))

(defun w3-handle-graphics ()
  "A function to parse out IMG tags.  In epoch, this will actually
insert the picture into the buffer.  The ALT attribute is displayed
when not in epoch (or when epoch fails to read in the graphic
correctly."
  (set-buffer w3-working-buffer)
  (if (get-buffer "Conversion errors") (kill-buffer "Conversion errors"))
  (goto-char (point-min))
  (if (fboundp 'w3-insert-graphic)
      (while (re-search-forward "<IMG[ \\\t]+\\([^>]+\\)>" nil t)
	(let ((st (match-beginning 0))
	      (lnk (and (w3-zone-at (match-beginning 1))
			(w3-zone-data (w3-zone-at (match-beginning 1)))))
	      (img (prog1
		    (w3-parse-args (match-beginning 1) (match-end 1))
		    (replace-match "")))
	      (src "")
	      (align 'center)
	      (alt nil))
	  (setq src (or (cdr (assoc "src" img)) "")
		alt (or (cdr (assoc "alt" img))
			(concat "[IMAGE(" (w3-basepath src t) ")] "))
		align (intern (downcase (or (cdr (assoc "align" img))
					    "center"))))
	  (if (not (string-match w3-nonrelative-link src))
	      (setq src (w3-parse-relative-link src)))
	  (if (assoc "ismap" img)
	      (setq lnk (cons 'ismap (cdr lnk))))
	  (setq src (cons src lnk))
	  (if w3-delay-image-loads
	      (w3-add-delayed-graphic src st align alt)
	    (w3-insert-graphic src st align alt))))
    (progn
      (goto-char (point-min))
      (let ((alt "") (img "") (src "") st)
	(while (re-search-forward "<IMG[ \\\t\\\n]*\\([^>]+\\)>" nil t)
	  (setq img (prog1
			(w3-parse-args (match-beginning 1) (match-end 1))
		      (replace-match ""))
		src (or (cdr (assoc "src" img)) "")
		alt (or (cdr (assoc "alt" img))
			(concat "[IMAGE(" (w3-basepath src t) ")] ")))
	  (if (not (string-match w3-nonrelative-link src))
	      (setq src (w3-parse-relative-link src)))
	  (setq st (point))
	  (w3-insert alt)
	  (w3-add-zone st (point) nil (list 'w3graphic src) t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the <EM> tag.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-find-emphasis-face (attributes)
  "Return a face from the various attributes of an <em> tag."
  (cond
   ((and (assoc "b" attributes) (assoc "i" attributes)) 'bold-italic)
   ((assoc "sup" attributes) (or (cdr (assoc "SUP" w3-style-assoc)) 'bold))
   ((assoc "sub" attributes) (or (cdr (assoc "SUB" w3-style-assoc)) 'italic))
   ((or (assoc "tt" attributes)
	(assoc "hv" attributes)
	(assoc "tr" attributes)) (or (cdr (assoc "TT" w3-style-assoc))
				     'w3-tt-style))
   ((assoc "b" attributes) 'w3-bold-style)
   ((assoc "i" attributes) 'w3-italic-style)
   ((assoc "u" attributes) 'w3-underline-style)
   (t (message "Error in an <em> tag - unknown emphasis.") nil)))

(defun w3-handle-generic-emphasis-1 ()
  (let ((args nil)			; Arguments to the <em> tag
	(face nil)			; Face to use
	(strt nil)			; Start of the <em> tag
	(end  nil)			; End of the <em> tag
	)
    (if (not (re-search-forward "<em\\([^>]*\\)>" nil t))
	(message "Something is wrong with an <em> tag")
      (setq strt (match-beginning 0)
	    args (prog1
		     (w3-parse-args (match-beginning 1) (match-end 1))
		   (replace-match ""))
	    end (save-excursion
		  (or (re-search-forward "</em[^>]*>" nil t) (end-of-line))
		  (prog1 (point) (replace-match "")))
	    face (w3-find-emphasis-face args))
      (w3-add-zone strt (min end (point-max)) face '(w3style)))))  

(defun w3-handle-generic-emphasis ()
  "Handle the <em> tag."
  (goto-char (point-min))
  (let ((pos  nil)			; Temporary position marker
	(opos nil)
	(st nil))
    (while (re-search-forward "<em" nil t)
      (setq st (match-beginning 0))
      (while (setq opos pos
		   pos (w3-subemphasis-exists))
	(goto-char (cdr pos)))
      (goto-char (or (car opos) st))
      (w3-handle-generic-emphasis-1)
      (goto-char st))))


(defun w3-subemphasis-exists ()
  "Return t iff there is a nested <em> tag"
  (let* ((end-tag (save-excursion
		    (and (re-search-forward "</em[^>]*>" nil t)
			 (match-beginning 0))))
	 (search-limit (or end-tag (save-excursion (end-of-line) (point)))))
    (if (re-search-forward "<em" search-limit t)
	(cons (match-beginning 0) (match-end 0))
      nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shared graphics routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-convert-graphic-to-useable-format (buf fname xbm)
  "Convert the image data in buffer BUF into a format useable by
lemacs or epoch.  Second arg FNAME is the filename to redirect output
into.  If third arg XBM is t, convert it to an Xbitmap, otherwise
convert it to an XPM (recommended, as they can do color).  Returns a
filename containing the bitmap specification"
  (save-excursion
    (set-buffer buf)
    (let (converter)
      (if (not w3-current-mime-type)
	  (setq w3-current-mime-type (w3-extension-to-mime
				      (w3-file-extension w3-current-file))))
      (setq converter (assoc w3-current-mime-type w3-graphic-converter-alist))
      (if (not converter)
	  (message "Cannot convert %s to www/present!" w3-current-mime-type)
	(progn
	  (message "Converting image %s (%s)..." w3-current-file
		   w3-current-mime-type)
	  (shell-command-on-region
	   (point-min) (point-max)
	   (concat (format (cdr converter) w3-max-colors
			   (if xbm w3-ppmtoxbm-command w3-ppmtoxpm-command))
		   "> " fname) t))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cacheing for documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ring)
(defvar w3-document-cache (make-ring w3-cache-size) "Internal doc cache")

(defun w3-find-in-cache (url)
  "Return the document referenced by URL if it is in the document cache."
  (let* ((ln (car (cdr w3-document-cache)))
	 (x 0) y retval)
    (while (<= x ln)
      (setq y (ring-ref w3-document-cache x))
      (if (equal (car y) url)
	  (setq retval (cdr y)))
      (setq x (1+ x)))
    retval))

(defun w3-store-in-cache ()
  "Store the current document in the cache."
  (let ((prolog (concat (format "(let ((doc \"%s\"))\n"
				(or (buffer-string) ""))
			"(w3-clear-tmp-buffer)\n"
			"(insert doc)\n"
			(mapconcat
			 (function
			  (lambda (x)
			    (format "(setq %S %s%S)\n" x
				    (if (or (eq x 'tab-stop-list)
					    (eq x 'w3-current-mime-viewer))
					"'" "")
				    (if (eq x 'w3-current-last-buffer)
					(buffer-name (symbol-value x))
				      (symbol-value x)))))
			 w3-persistent-variables "\n")))
	(buf (current-buffer))
	(fnam (w3-generate-unique-filename))
	(st (point-min))
	(url (w3-view-url t))
	(nd (point-max)))
  (set-buffer (get-buffer-create " *w3-cache*"))
  (erase-buffer)
  (insert prolog)
  (w3-write-zones st nd buf)
  (ring-insert w3-document-cache (cons url fnam))
  (write-region (point-min) (point-max) fnam nil 5)
  (kill-buffer (current-buffer))))
  

(defun w3-load-flavors ()
  "Load the correct zone/font info for each flavor of emacs"
  (cond
   (w3-running-lemacs (require 'w3-lemacs))
   (w3-running-old-lemacs (require 'w3-old-lemacs))
   (w3-running-epoch  (require 'w3-epoch))
   (w3-running-FSF19  (require 'w3-emacs19))
   (t                 (require 'w3-emacs)))
  (condition-case ()
      (require 'w3-site-init)
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic bug submission.                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-submit-bug ()
  "Function to submit a bug to the programs maintainer"
  (interactive)
  (let ((url (w3-view-url t)))
    (if (equal "file:nil" url) (setq url nil))
    (cond
     ((and w3-mutable-windows (fboundp w3-mail-other-window-command))
      (funcall w3-mail-other-window-command))
     ((fboundp w3-mail-command)
      (funcall w3-mail-command))
     (w3-mutable-windows (mail-other-window))
     (t (mail)))
    (mail-to)
    (insert "wmperry@indiana.edu")
    (mail-subject)
    (insert "Bug found in w3-mode")
    (re-search-forward mail-header-separator nil t)
    (next-line 1)
    (while (< (current-column) 29) (w3-insert "-"))
    (insert "Description of System:")
    (while (< (current-column) 75) (w3-insert "-"))
    (insert "\n")
    (string-match "WWW \\([^ ]*\\) \\(.*\\)" w3-version)
    (insert "WWW Browser Version: "
	    (substring w3-version (match-beginning 1) (match-end 1))
	    ", of "
	    (substring w3-version (match-beginning 2) (match-end 2))
	    "\n"
	    "      Emacs Version: "
	    (substring (emacs-version) 0 (string-match " of" (emacs-version)))
	    (if w3-running-epoch "(Epoch)" "")
	    (if (boundp 'MULE) "(MULE)" "")
	    "\n"
	    (if window-system
		(concat "      Window System: " (symbol-name window-system)
			"-" window-system-version "\n") "")
	    "        System Type: "
	    (prin1-to-string system-type) "\n"
	    (if url (concat "                URL: " url "\n") "")
	    (if (featurep 'ange-ftp)
		(concat "           Ange-FTP: " ange-ftp-version "\n") "")
	    (if (featurep 'efs)
		(concat "                EFS: " efs-version "\n") ""))
    (while (< (current-column) 29) (w3-insert "-"))
    (w3-insert "Description of Problem:")
    (while (< (current-column) 75) (w3-insert "-"))
    (w3-insert "\n\n")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for searching						    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-nuke-spaces-in-search (x)
  "Remove spaces from search strings . . ."
  (let ((new ""))
    (while (not (equal x ""))
      (setq new (concat new (if (= (string-to-char x) 32) "+" 
			      (substring x 0 1)))
	    x (substring x 1 nil)))
    new))

(defun w3-parse-args (st nd)
  "Return an assoc list of attribute/value pairs from an SGML-type string"
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	)
    (save-restriction
      (narrow-to-region st nd)
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \\\n\\\t")
	(setq name-pos (point))
	(skip-chars-forward "^ \\\n\\\t=")
	(downcase-region name-pos (point))
	(setq name (buffer-substring name-pos (point)))
	(skip-chars-forward " \\\t\\\n")
	(if (/= (or (char-after (point)) 0)  ?=) ; There is no value
	    (setq value nil)
	  (skip-chars-forward " \\\t\\\n=")
	  (setq val-pos (point)
		value
		(cond
		 ((or (= (or (char-after val-pos) 0) ?\")
		      (= (or (char-after val-pos) 0) ?'))
		  (buffer-substring (1+ val-pos)
				    (condition-case ()
					(prog2
					    (forward-sexp 1)
					    (1- (point))
					  (skip-chars-forward "\""))
				      (error
				       (skip-chars-forward "^ \\\t\\\n")
				       (point)))))
		 (t
		  (buffer-substring val-pos
				    (progn
				      (skip-chars-forward "^ \\\t\\\n")
				      (point)))))))
	(setq results (cons (cons name value) results)))
      results)))

(defun w3-parse-args-string (str)
  "Return an assoc list of attribute/value pairs from an SGML-type string"
  (let ((buff (get-buffer-create " *w3-tmp*")))
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (set-syntax-table w3-parse-args-syntax-table)
      (insert str) 
      (w3-parse-args (point-min) (point-max)))))

(defun w3-search ()
  "Perform a search, if this is a searchable index."
  (interactive)
  (cond
   ((not w3-current-isindex) (message "Not a searchable index!"))
   ((not (equal w3-current-type "http"))
    (message "Sorry, searching is not implemented on local files yet."))
   (t
    (let ((querystring (w3-nuke-spaces-in-search
			(read-string "Search on (+ separates keywords): ")))
	  (url (w3-view-url t)))
      (if (string-match "\\(.*\\)\\?.*" url)
	  (setq url (w3-match url 1)))
      (w3-fetch (concat url
			(if (= ?? (string-to-char (substring w3-current-file
							     -1 nil))) "" "?")
			querystring))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto documentation, etc                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-help ()
  "Print documentation on w3 mode."
  (interactive)
  (w3-fetch "www://auto/help"))

(defun w3-remove-relative-links-helper (name)
  (cond
   ((string-match "\\\.\\\./" name)
    (let ((tmp (substring name (match-end 0) nil)))
      (if (= 0 (match-beginning 0))
 	  (concat (w3-basepath (w3-basepath w3-current-file)) tmp)
 	(concat (w3-basepath (substring name 0
 					(1- (match-beginning 0)))) tmp))))
   ((string-match "\\\./" name)
    (if (= 0 (match-beginning 0))
 	(substring name 2 nil)
      (concat
       (substring name 0 (match-beginning 0))
       (substring name (match-end 0) nil))))))

(defun w3-remove-relative-links (name)
  "Strip . and .. from pathnames"
  (while (string-match "\\\.+/" name)
    (setq name (w3-remove-relative-links-helper name)))
  name)

(defun w3-version ()
  "Show the version # of W3 in the minibuffer"
  (interactive)
  (message w3-version))

;;;###autoload
(defun w3 ()
  "Retrieve the default World Wide Web home page.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The home page is specified by the variable w3-default-homepage.  The
document should be specified by its fully specified Uniform Resource
Locator.  The document will be parsed as HTML (if appropriate) and
displayed in a new buffer."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not (string-match ".*:.*" w3-default-homepage))
      (w3-fetch (concat "file:" w3-default-homepage))
    (w3-fetch w3-default-homepage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition							    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-reload-document ()
  "Reload the current document"
  (interactive)
  (let ((tmp (w3-view-url t))
	(pnt (point))
	(w3-request-extra-headers '(("Pragma" . "no-cache"))))
    (kill-buffer (current-buffer))
    (w3-fetch tmp)
    (goto-char pnt)))

(defun w3-leave-buffer ()
  "Bury this buffer,but don't kill it"
  (interactive)
  (let ((x w3-current-last-buffer))
    (if (and w3-running-FSF19
	     (or (eq window-system 'x)
		 (eq window-system 'pm)))
	(set-variable 'lucid-menu-bar-dirty-flag t))
    (bury-buffer (current-buffer))
    (if (and (bufferp x) (buffer-name x))
	(if w3-mutable-windows (pop-to-buffer x) (switch-to-buffer x)))))  

(defun w3-quit ()
  "Quit WWW mode"
  (interactive)
  (let ((x w3-current-last-buffer))
    (if (and w3-running-FSF19
	     (or (eq window-system 'x)
		 (eq window-system 'pm)))
	(set-variable 'lucid-menu-bar-dirty-flag t))
    (and (boundp 'w3-mpeg-kill-processes) (w3-mpeg-kill-processes))
    (kill-buffer (current-buffer))
    (if (and (bufferp x) (buffer-name x))
	(if w3-mutable-windows (pop-to-buffer x) (switch-to-buffer x)))))

(defun w3-view-this-url (&optional no-show)
  "View the URL of the link under point"
  (interactive)
  (let* ((ext (w3-zone-at (point)))
	 (data (and ext (w3-zone-data ext))))
    (cond
     ((eq (car data) 'w3)
      (if (not no-show) (message "%s"
				 (w3-quotify-percents (nth 2 data)))
	(nth 2 data)))
     ((eq (car data) 'w3form)
      (if (not no-show)
	  (message "Form entry (name=%s, type=%s)" (w3-quotify-percents
						    (nth 3 data))
		   (w3-quotify-percents
		    (if (equal "" (nth 2 data)) "TEXT" (nth 2 data))) nil)))
     ((eq (car data) 'w3graphic)
      (if (not no-show) (message "Inlined image (%s)" (w3-quotify-percents
						       (nth 1 data))) nil))
     (t (if (not no-show) (message "No link at point.")
	  nil)))))

(defun w3-load-delayed-images ()
    "Load inlined images that were delayed, if necessary.
This function searches through w3-delayed-images and fetches the
appropriate picture for each point in the buffer and inserts it."
  (interactive)
  (and (fboundp 'w3-insert-graphic)
       (let ((buffer-read-only nil))
	 (mapcar (function (lambda (data) (apply 'w3-insert-graphic data)))
		 (nreverse w3-delayed-images))))
  (setq w3-delayed-images nil))

(defun w3-view-url (&optional no-show)
  "View the current document's URL"
  (interactive)
  (let ((url ""))
    (cond
     ((equal w3-current-type "gopher")
      (setq url (format "%s://%s%s/%s"
			w3-current-type w3-current-server
			(if (string= "70" w3-current-port) ""
			  (concat ":" w3-current-port))
			(w3-quotify-percents w3-current-file))))
     ((equal w3-current-type "news")
      (setq url (concat "news://" w3-current-server
			(if (string= "119" w3-current-port) ""
			  (concat ":" w3-current-port)) "/"
			w3-current-file)))
     ((equal w3-current-type "http")
      (setq url (format  "%s://%s%s/%s" w3-current-type w3-current-server
			 (if (string= "80" w3-current-port) ""
			   (concat ":" w3-current-port))
			 (w3-quotify-percents
			  (if (= ?/ (string-to-char w3-current-file))
			      (substring w3-current-file 1 nil)
			    w3-current-file)))))
     ((equal w3-current-type "ftp")
      (setq url (format "%s://%s%s/%s" w3-current-type
			(if w3-current-user (concat w3-current-user "@") "")
			w3-current-server
			(w3-quotify-percents
			 (if (= ?/ (string-to-char w3-current-file))
			     (substring w3-current-file 1 nil)
			   w3-current-file)))))
     ((equal w3-current-type nil)
      (setq url (format "file:%s" (w3-quotify-percents w3-current-file))))
     ((equal w3-current-type "www")
      (setq url (format "www:/%s/%s" w3-current-server w3-current-file))))
    (if (not no-show) (message url) url)))

(defun w3-save-this-url ()
  "Save url under point in the kill ring"
  (interactive)
  (w3-save-url t))

(defun w3-save-url (under-pt)
  "Save current url in the kill ring"
  (interactive "P")
  (let ((x (if under-pt (w3-view-this-url t) (w3-view-url t))))
    (setq kill-ring (cons x kill-ring))
    (setq kill-ring-yank-pointer kill-ring)
    (if (fboundp 'w3-store-in-x-clipboard) (w3-store-in-x-clipboard x))))

(defun w3-end-of-document ()
  "Go to end of document"
  (interactive)
  (goto-char (point-max)))

(defun w3-start-of-document ()
  "Go to start of document"
  (interactive)
  (goto-char (point-min)))

(defun w3-mail-to-author ()
  "Send mail to the author of this document, if possible."
  (interactive)
  (let ((x w3-current-links)
	(y nil)
	(found nil))
    (while (and x (not found))
      (setq y (car x)
	    x (cdr x)
	    found (equal (or (cdr (assoc "rel" y)) (cdr (assoc "rev" y)))
			 "made"))
      (if found
	  (setq found (cdr (assoc "href" y)))))
    (if found (w3-fetch found)
      (error "Cannot find the 'made' link for this document, sorry."))))

(defun w3-kill-emacs-func ()
  "Routine called when exiting emacs.  Do miscellaneous clean up."
  (and w3-keep-history w3-history-list (w3-write-global-history))
  (message "Cleaning up w3 storage...")
  (let ((x (directory-files w3-temporary-directory t "w3.*")))
    (while x
      (condition-case ()
	  (delete-file (car x))
	(error nil))
      (setq x (cdr x))))
  (message "Cleaning up w3 storage... done.")
  (and w3-old-kill-emacs-hook (funcall w3-old-kill-emacs-hook)))

(defun w3-do-setup ()
  "Do setup - this is to avoid conflict with user settings when W3 is
dumped with emacs."
  (w3-load-flavors)
  (w3-setup-version-specifics)
  ; Create the fonts, etc in windowing systems
  (w3-create-faces)

  ; Parse the global history file if it exists, so that it can be used
  ; for URL completion, etc.
  (if (file-exists-p w3-global-history-file) (w3-parse-global-history))

  ; Add the local etc directory to the icon search path
  (if (boundp 'data-directory)
      (let ((maybe-dir (file-name-as-directory
			(expand-file-name "w3" data-directory))))
	(if (file-directory-p maybe-dir)
	    (setq w3-icon-directory-list (cons (concat "file:" maybe-dir)
					       w3-icon-directory-list)))))
    
  ; Read in proxy gateways
  (setq w3-proxy-services
	(mapcar
	 (function
	  (lambda (x)
	    (let ((y (getenv (concat x "_proxy"))))
	      (and y (cons x y)))))
	 (mapcar 'car
		 (w3-split (substring
			    (substring w3-nonrelative-link 0 -3) 3 nil)
			   (regexp-quote "\\|")))))
  (if (getenv "no_proxy")
      (setq w3-proxy-services
	    (cons "no_proxy"
		  (concat "\\("
			  (mapconcat
			   (function
			    (lambda (x)
			      (cond
			       ((= x ?,) "\\|")
			       ((= x ? ) "")
			       ((= x ?.) (regexp-quote "."))
			       ((= x ?*) ".*")
			       ((= x ??) ".")
			       (t (char-to-string x)))))
			   (getenv "no_proxy") "") "\\)"))))

  ; Set up delimiting based on window-system and value of
  ; w3-emacs19-hack-faces-p
  (if (eq w3-delimit-emphasis 'guess)
      (setq w3-delimit-emphasis
	    (and (not w3-running-lemacs)
		 (not w3-running-old-lemacs)
		 (not w3-running-epoch)
		 (not (boundp 'MULE))
		 (not (and w3-running-FSF19
			   (or (eq window-system 'x)
			       (eq window-system 'ns)
			       (eq window-system 'pm)
			       w3-emacs19-hack-faces-p))))))

  (if (eq w3-delimit-links 'guess)
      (setq w3-delimit-links
	    (and (not w3-running-lemacs)
		 (not w3-running-old-lemacs)
		 (not w3-running-epoch)
		 (not (boundp 'MULE))
		 (not (and w3-running-FSF19
			   (or (eq window-system 'x)
			       (eq window-system 'ns)
			       (eq window-system 'pm)
			       w3-emacs19-hack-faces-p))))))
  
  ; Set up a hook that will save the history list when
  ; exiting emacs
  (if (or w3-running-lemacs w3-running-FSF19 w3-running-old-lemacs)
      (add-hook 'kill-emacs-hook 'w3-kill-emacs-func)
    (setq w3-old-kill-emacs-hook kill-emacs-hook
	  kill-emacs-hook 'w3-kill-emacs-func))

  ; Read in the ~/.w3 file if it exists - could set up some of these
  ; defaults.  This file is where I will store configuration information
  ; once I write the auto-editing of variables/info, etc.
  (if (file-exists-p (expand-file-name "~/.w3"))
      (load-file (expand-file-name "~/.w3")))

  ; Set the w3-use-transparent with decent defaults
  (if (or w3-running-lemacs
	  w3-running-old-lemacs
	  w3-running-epoch
	  window-system)
      (setq w3-use-transparent nil))
  (and w3-use-transparent (require 'transparent))
  
  ; Set the password entry funtion based on user defaults or guess
  ; based on which remote-file-access package they are using.
  (cond
   (w3-passwd-entry-func nil)		; Already been set
   ((or (featurep 'efs)			; Using EFS
	(featurep 'efs-auto))		; or autoloading efs
    (setq w3-passwd-entry-func 'efs-read-passwd))
   ((featurep 'ange-ftp)		; Using ange-ftp
    (setq w3-passwd-entry-func 'ange-ftp-read-passwd))
   (t (error "You must use either ange-ftp or efs!")))

  (setq w3-mime-default-mailcap
	(or w3-mime-default-mailcap (expand-file-name "~/.mailcap")))
  
  ; Read in extra mime types that may not be defined in w3-mime-viewers
  (if (file-exists-p (expand-file-name "~/.mime-types")) (w3-parse-mime-types))
  
  ; Load in the hotlist if they haven't set it already
  (or w3-hotlist (w3-parse-hotlist))
  
  ; Load in their personal annotations if they haven't set them already
  (or w3-personal-annotations (w3-parse-personal-annotations))
  
  ; Set up the news service if they haven't done so
  (setq w3-news-server
	(cond
	 (w3-news-server w3-news-server)
	 ((and (boundp 'gnus-default-nntp-server)
	       (not (equal "" gnus-default-nntp-server)))
	  gnus-default-nntp-server)
	 ((and (boundp 'gnus-nntp-server)
	       (not (null gnus-nntp-server))
	       (not (equal "" gnus-nntp-server)))
	  gnus-nntp-server)
	 ((and (boundp 'nntp-server-name)
	       (not (null nntp-server-name))
	       (not (equal "" nntp-server-name)))
	  nntp-server-name)
	 ((getenv "NNTPSERVER") (getenv "NNTPSERVER"))
	 (t "news")))
  
  ; Set the default home page, honoring their defaults, then
  ; the standard WWW_HOME, then default to the documentation @ IU
  (or w3-default-homepage
      (setq w3-default-homepage
	    (or (getenv "WWW_HOME")
		"http://cs.indiana.edu/elisp/w3/docs.html")))
  
  ; Set up the MIME accept string if they haven't got it hardcoded yet
  (or w3-mime-accept-string
      (setq w3-mime-accept-string (w3-parse-viewer-types)))
  
  ; This isn't used yet, but just in case we ever need it for the
  ; graphics parsing routines - perhaps use this value to determine
  ; value for w3-max-colors?
  (or w3-color-planes (setq w3-color-planes
			    (cond
			     (w3-running-lemacs (x-display-planes))
			     (w3-running-old-lemacs (x-display-planes))
			     ((and w3-running-FSF19
				   (or (eq window-system 'x)
				       (eq window-system 'pm)))
			      (x-display-planes))
			     (w3-running-epoch 8)
			     (t nil))))
  
  ; This isn't used yet, but just in case we ever need it for the
  ; graphics parsing routines - perhaps use this value to determine
  ; value for w3-max-colors?
  (or w3-color-display (setq w3-color-display
			     (cond
			      (w3-running-lemacs (x-color-display-p))
			      (w3-running-old-lemacs (x-color-display-p))
			      ((and w3-running-FSF19
				    (or (eq window-system 'x)
					(eq window-system 'pm)))
			       (x-display-color-p))
			      ((and w3-running-FSF19
				    (eq window-system 'ns))
			       (ns-display-color-p))
			      (w3-running-epoch t)
			      (t nil))))

  (if (and (fboundp 'w3-insert-graphic)
	   (not w3-color-display)
	   (string-match "ppmtoxpm" w3-ppmtoxpm-command))
      (setq w3-ppmtoxpm-command w3-ppmtoxbm-command))

  ; Set up the documents menu
  (w3-parse-docs-menu)
  ; Set up the regular expression used to find styles.
  (setq w3-style-regexp (or w3-style-regexp
			    (concat "<\\("
				    (mapconcat 'car w3-style-assoc "\\|")
				    "\\)>")))
  ; Set up the entity definition for PGP and PEM authentication
  (setq w3-pgp/pem-entity (or w3-pgp/pem-entity
			      (format "%s@%s"  (user-real-login-name)
				      (system-name))))
  (setq w3-personal-mail-address (or w3-personal-mail-address
				     w3-pgp/pem-entity))
  (run-hooks 'w3-load-hooks)
  (setq w3-setup-done t))

(defun w3-mark-link-as-followed (ext dat)
  "Mark a link as followed, by removing the old extent EXT, and replacing
it with a new extent with the w3-visited-node-style face."
  (let ((st (w3-zone-start ext))
	(nd (w3-zone-end ext)))
    (w3-delete-zone ext)
    (w3-add-zone st nd w3-visited-node-style dat t)
    (cond
     (w3-delimit-links
;      (goto-char nd)
;      (delete-region nd (- nd (length (car w3-link-end-delimiter))))
;      (insert (cdr w3-link-end-delimiter))
;      (goto-char st)
;      (delete-region st (+ st (length (car w3-link-start-delimiter))))
;      (insert (cdr w3-link-start-delimiter))
      )
     (t nil))))	

;;;###autoload
(defun w3-follow-link ()
  "Attempt to follow the hypertext reference under point."
  (interactive)
  (let* ((ext (w3-zone-at (point)))
	 (dat (and ext (w3-zone-data ext))))
    (cond
     ((null dat) (message "No link, form entry, or image at point."))
     ((eq (car dat) 'w3)
      (let ((buffer-read-only nil))
	(w3-mark-link-as-followed ext dat))
      (if (stringp (nth 2 dat))
	  (w3-maybe-relative (nth 2 dat))
	(message "No link.")))
     ((eq (car dat) 'w3form) (w3-do-form-entry dat ext))
     ((eq (car dat) 'w3graphic) (w3-maybe-relative (nth 1 dat)))
     ((eq (car dat) 'w3expandlist) (w3-expand-list dat))
     ((eq (car dat) 'w3delayed)
      (apply 'w3-load-single-delayed-graphic
	     (w3-zone-start ext) (w3-zone-end ext) (cdr dat))
      (w3-delete-zone ext))
     ((eq (car dat) 'w3mpeg)
      (apply 'w3-load-single-delayed-mpeg
	     (w3-zone-start ext) (w3-zone-end ext) (cdr dat)))
     (t (message "Confused about what type of link is at point: %S" (car dat)))
     )))

(defun w3-complete-link ()
  "Choose a link from the current buffer and follow it"
  (interactive)
  (let (links-alist
	choice
	(completion-ignore-case t))
    (w3-map-links (function
		   (lambda (data arg)
		     (setq links-alist (cons
					(cons (nth 3 data)
					      (nth 2 data)) links-alist)))))
    (if (not links-alist) (error "No links in current document."))
    (setq links-alist (sort links-alist (function
					 (lambda (x y)
					   (string< (car x) (car y))))))
      (setq choice (completing-read "Link: " links-alist nil t))
      (w3-fetch (cdr (assoc choice links-alist)))))

(defun w3-mode ()
  "Mode for viewing HTML documents.  Will try to bring up the document
specified by w3-default-homepage.
Current keymap is:
\\{w3-mode-map}"
  (interactive)
  (or w3-setup-done (w3-do-setup))
  (let ((tmp (mapcar (function (lambda (x) (cons x (symbol-value x))))
		     w3-persistent-variables)))
    (kill-all-local-variables)
    (use-local-map w3-mode-map)
    (setq major-mode 'w3-mode)
    (setq mode-name "WWW")
    (run-hooks 'w3-mode-hooks)
    (mapcar (function (lambda (x) (set-variable (car x) (cdr x)))) tmp)
    (w3-mode-version-specifics)
    (if (and w3-current-isindex (equal w3-current-type "http"))
	(setq mode-line-process "-Searchable"))))

(provide 'w3)
