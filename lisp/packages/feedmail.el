;;; From: William.J.Carpenter@hos1cad.att.com (Bill C)
;;; Subject: feedmail.el, patchlevel 2 [repost]
;;; Date: 8 Jun 91 22:23:00 GMT
;;; Organization: AT&T Bell Laboratories
;;;
;;; 5-may-92  jwz	Conditionalized calling expand-mail-aliases, since that
;;;			function doesn't exist in Lucid GNU Emacs or when using
;;;			mail-abbrevs.el.
;;; 
;;; Here's the latest version of feedmail.el, a replacement for parts of
;;; GNUemacs' sendmail.el (specifically, it's what handles your outgoing
;;; mail after you type C-c C-c in mail mode).   (Sorry if you're seeing
;;; this a second time.  Looks like my earlier attempt to post it didn't
;;; get off the local machine.)
;;; 
;;; This version contains the following new things:
;;; 
;;;    * fix for handling default-case-fold-search
;;;    * involve user-full-name in default from line
;;;    * fix for my improper use of mail-strip-quoted-names when
;;;      addresses contain a mix of "<>" and "()" styles
;;;    * new feature allowing optional generation of Message-ID

;;; feedmail.el
;;; LCD record:
;;; feedmail|Bill Carpenter|william.j.carpenter@att.com|Outbound mail handling|91-05-24|2|feedmail.el
;;;
;;; Written by Bill Carpenter <william.j.carpenter@att.com>
;;; original,      31 March 1991
;;; patchlevel 1,   5 April 1991
;;; patchlevel 2,  24 May   1991
;;;
;;; As far as I'm concerned, anyone can do anything they want with
;;; this specific piece of code.  No warranty or promise of support is
;;; offered.
;;;
;;; This stuff does in elisp the stuff that used to be done
;;; by the separate program "fakemail" for processing outbound email.
;;; In other words, it takes over after you hit "C-c C-c" in mail mode.
;;; By appropriate setting of options, you can still use "fakemail",
;;; or you can even revert to sendmail (which is not too popular
;;; locally).  See the variables at the top of the elisp for how to
;;; achieve these effects:
;;;
;;;    --- you can get one last look at the prepped outbound message and
;;;        be prompted for confirmation
;;;
;;;    --- removes BCC: headers after getting address info
;;;
;;;    --- does smart filling of TO: and CC: headers
;;;
;;;    --- processes FCC: lines and removes them
;;;
;;;    --- empty headers are removed
;;;
;;;    --- can force FROM: or SENDER: line
;;;
;;;    --- can generate a Message-ID line
;;;
;;;    --- strips comments from address info (both "()" and "<>" are
;;;        handled via a call to mail-strip-quoted-names); the
;;;        comments are stripped in the simplified address list given
;;;        to a subprocess, not in the headers in the mail itself
;;;        (they are left unchanged, modulo smart filling)
;;;
;;;    --- error info is pumped into a normal buffer instead of the
;;;        minibuffer
;;;
;;;    --- just before the optional prompt for confirmation, lets you
;;;        run a hook on the prepped message and simplified address
;;;        list
;;;
;;;    --- you can specify something other than /bin/mail for the
;;;        subprocess
;;;
;;; After a few options below, you will find the function
;;; feedmail-send-it.  Everything after that function is just local
;;; stuff for this file.  There are two ways you can use the stuff in
;;; this file:
;;;
;;; (1)  Put the contents of this file into sendmail.el and change the
;;; name of feedmail-send-it to sendmail-send-it, replacing that
;;; function in sendmail.el.
;;;
;;;                              or
;;;
;;; (2)  Save this file as feedmail.el somewhere on your elisp
;;; loadpath; byte-compile it.  Put the following lines somewhere in
;;; your ~/.emacs stuff:
;;;
;;;        (setq send-mail-function 'feedmail-send-it)
;;;        (autoload 'feedmail-send-it "feedmail")
;;;


(defvar feedmail-confirm-outgoing nil
  "*If non-nil, gives a y-or-n confirmation prompt after prepping,
before sending mail.")


(defvar feedmail-nuke-bcc t
  "*Non-nil means get rid of the BCC: lines from the message header
text before sending the mail.  In any case, the BCC: lines do
participate in the composed address list.  You probably want to keep
them if you're using sendmail (see feedmail-buffer-eating-function).")


(defvar feedmail-fill-to-cc t
  "*Non-nil means do smart filling (line-wrapping) of TO: and CC: header
lines.  If nil, the lines are left as-is.  The filling is done after
mail address alias expansion.")


(defvar feedmail-fill-to-cc-fill-column default-fill-column
  "*Fill column used when wrapping mail TO: and CC: lines.")


(defvar feedmail-nuke-empty-headers t
  "*If non-nil, headers with no contents are removed from the outgoing
email.  A completely empty SUBJECT: header is always removed,
regardless of the setting of this variable.  The only time you would
want them left in would be if you used some headers whose presence
indicated something rather than their contents.")

;;; wjc sez:  I think the use of the SENDER: line is pretty pointless,
;;; but I left it in to be compatible with sendmail.el and because
;;; maybe some distant mail system needs it.  Really, though, if you
;;; want a sender line in your mail, just put one in there and don't
;;; wait for feedmail to do it for you.

(defvar feedmail-sender-line nil
  "*If nil, no SENDER: header is forced.  If non-nil and the email
already has a FROM: header, a SENDER: header is forced with this as
its contents.  You can probably leave this nil, but if you feel like
using it, a good value would be a fully-qualified domain name form of
your address.  For example, william.j.carpenter@att.com.  Don't
include a trailing newline or the keyword SENDER:.  They're
automatically provided.")


;; user-full-name suggested by kpc@ptolemy.arc.nasa.gov (=Kimball Collins)
(defvar feedmail-from-line
  (concat (user-login-name) "@" (system-name) " (" (user-full-name) ")")
  "*If non-nil and the email has no FROM: header, one will be forced
with this as its contents. A good value would be a fully-qualified
domain name form of your address.  For example, william.j.carpenter@att.com.
(The default value of this variable is probably not very good, since
it doesn't have a domain part.)  Don't include a trailing newline or
the keyword FROM:.  They're automatically provided.")


;;; Here's how I use the GNUS Message-ID generator for mail but not
;;; for news postings:
;;;
;;;   (setq feedmail-message-id-generator 'wjc:gnusish-message-id)
;;;   (setq gnus-your-domain "hos1cad.ATT.COM")
;;;   
;;;   (defun wjc:gnusish-message-id ()
;;;     (require 'gnuspost)
;;;     (if (fboundp 'wjc:gnus-inews-message-id)
;;;   	  (wjc:gnus-inews-message-id)
;;;   	(gnus-inews-message-id)))
;;;   
;;;   (setq news-inews-hook
;;;   	  '(lambda () 
;;;   		 (defun gnus-inews-date () nil)
;;;   		 (fset 'wjc:gnus-inews-message-id (symbol-function 'gnus-inews-message-id))
;;;   		 (defun gnus-inews-message-id () nil)
;;;   		 ))
;;;   
(defvar feedmail-message-id-generator nil
  "*If non-nil, should be a function (called with no arguments) which
will generate a unique message ID which will be inserted on a
Message-ID: header.  The message ID should be the return value of the
function.  Don't include trailing newline, leading space, or the
keyword MESSAGE-ID.  They're automatically provided.  Do include
surrounding <> brackets.  For an example of a message ID generating
function, you could look at the GNUS function gnus-inews-message-id.
When called, the current buffer is the prepped outgoing mail buffer
(the function may inspect it, but shouldn't modify it).  If the returned
value doesn't contain any non-whitespace characters, no message ID
header is generated, so you could generate them conditionally,
based on the contents of the mail.")


(defun feedmail-confirm-addresses-hook-example ()
  "An example of a last chance hook that shows the simple addresses
and gets a confirmation.  Use as (setq feedmail-last-chance-hook
'feedmail-confirm-addresses-hook-example)."
  (save-window-excursion 
	(display-buffer feedmail-address-buffer)
	(if (not (y-or-n-p "How do you like them apples? "))
		(error "Sending...gave up in last chance hook"))))


(defvar feedmail-last-chance-hook nil
  "*User's last opportunity to modify the message on its way out.  It
has already had all the header prepping from the standard package.
The next step after running the hook will be to push the buffer into a
subprocess that mails the mail.  The hook might be interested in these
buffers:  (1) feedmail-prepped-text-buffer contains the header and body
of the message, ready to go;  (2) feedmail-address-buffer contains the
space-separated, simplified list of addresses which is to be given to
the subprocess (the hook may change them).  feedmail-error-buffer is
an empty buffer intended to soak up errors for display to the user.
If the hook allows interactive activity, the user should not send more
mail while in the hook since some of the internal buffers will be reused.")


(defvar feedmail-buffer-eating-function 'feedmail-buffer-to-binmail
  "*Function used to send the prepped buffer to a subprocess.  The
function's three (mandatory) arguments are: (1) the buffer containing
the prepped message; (2) a buffer where errors should be directed; and
(3) a string containing the space-separated list of simplified
addresses.  Two popular choices for this are 'feedmail-buffer-to-binmail
and 'feedmail-buffer-to-sendmail.  If you use the sendmail form, you
probably want to set feedmail-nuke-bcc to nil.  If you use the binmail
form, check the value of feedmail-binmail-template.")


(defvar feedmail-binmail-template (if mail-interactive "/bin/mail %s" "/bin/rmail %s")
  "*Command template for the subprocess which will get rid of the
mail.  It can result in any command understandable by /bin/sh.  The
single '%s', if present, gets replaced by the space-separated,
simplified list of addressees.  Used in feedmail-buffer-to-binmail to
form the shell command which will receive the contents of the prepped
buffer as stdin.  If you'd like your errors to come back as mail
instead of immediately in a buffer, try /bin/rmail instead of
/bin/mail (this can be accomplished by keeping the default nil setting
of mail-interactive).  You might also like to consult local mail
experts for any other interesting command line possibilities.")


;; feedmail-buffer-to-binmail and feedmail-buffer-to-sendmail are the
;; only things provided for values for the variable
;; feedmail-buffer-eating-function.  It's pretty easy to write your
;; own, though.

(defun feedmail-buffer-to-binmail (prepped-mail-buffer mail-error-buffer simple-address-list)
  "Function which actually calls /bin/mail as a subprocess and feeds the buffer to it."
  (save-excursion
	(set-buffer prepped-mail-buffer)
	(apply 'call-process-region
		   (append (list (point-min) (point-max)
						 "/bin/sh" nil mail-error-buffer nil "-c"
						 (format feedmail-binmail-template simple-address-list ))))
	) ;; save-excursion
  )


(defun feedmail-buffer-to-sendmail (prepped-mail-buffer feedmail-error-buffer simple-address-list)
  "Function which actually calls sendmail as a subprocess and feeds the buffer to it."
  (save-excursion
	(set-buffer prepped-mail-buffer)
	(apply 'call-process-region
		   (append (list (point-min) (point-max)
					   (if (boundp 'sendmail-program)
						   sendmail-program
						 "/usr/lib/sendmail")
					   nil feedmail-error-buffer nil
					   "-oi" "-t")
				 ;; Don't say "from root" if running under su.
				 (and (equal (user-real-login-name) "root")
					  (list "-f" (user-login-name)))
				 ;; These mean "report errors by mail"
				 ;; and "deliver in background".
				 (if (null mail-interactive) '("-oem" "-odb"))))
))


;; feedmail-send-it is the only "public" function is this file.
;; All of the others are just little helpers.
;;;###autoload
(defun feedmail-send-it ()
  (let* ((default-case-fold-search t)
		 (feedmail-error-buffer (get-buffer-create " *Outgoing Email Errors*"))
		 (feedmail-prepped-text-buffer (get-buffer-create " *Outgoing Email Text*"))
		 (feedmail-address-buffer (get-buffer-create " *Outgoing Email Address List*"))
		 (feedmail-raw-text-buffer (current-buffer))
		 (case-fold-search nil)
		 end-of-headers-marker)

    (unwind-protect (save-excursion
		(set-buffer feedmail-prepped-text-buffer) (erase-buffer)

		;; jam contents of user-supplied mail buffer into our scratch buffer
		(insert-buffer-substring feedmail-raw-text-buffer)

		;; require one newline at the end.
		(goto-char (point-max))
		(or (= (preceding-char) ?\n) (insert ?\n))

		;; Change header-delimiter to be what mailers expect (empty line).
		(goto-char (point-min))
		(re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n"))
		(replace-match "\n")
		;; why was this backward-char here?
		;;(backward-char 1)
		(setq end-of-headers-marker (point-marker))

		(if (and (fboundp 'expand-mail-aliases) ; nil = mail-abbrevs.el
			 mail-aliases)
		    (expand-mail-aliases (point-min) end-of-headers-marker))

		;; make it pretty
		(if feedmail-fill-to-cc (feedmail-fill-to-cc-function end-of-headers-marker))
		;; ignore any blank lines in the header
		(goto-char (point-min))
		(while (and (re-search-forward "\n\n\n*" end-of-headers-marker t) (< (point) end-of-headers-marker))
		  (replace-match "\n"))
	  
		(let ((case-fold-search t))
		  (feedmail-deduce-address-list feedmail-prepped-text-buffer (point-min) end-of-headers-marker)
		  (save-excursion (set-buffer feedmail-address-buffer)
						  (goto-char (point-min))
						  (if (not (re-search-forward "\\S-" (point-max) t))
							  (error "Sending...abandoned, no addressees!")))

		  ;; Find and handle any BCC fields.
		  (if feedmail-nuke-bcc (feedmail-do-bcc end-of-headers-marker))

		  ;; Find and handle any FCC fields.
		  (goto-char (point-min))
		  (if (re-search-forward "^FCC:" end-of-headers-marker t)
			  (mail-do-fcc end-of-headers-marker))

		  (goto-char (point-min))
		  (if (re-search-forward "^FROM:" end-of-headers-marker t)
			  
			  ;; If there is a FROM: and no SENDER:, put in a SENDER:
			  ;; if requested by user
			  (if (and feedmail-sender-line
					   (not (save-excursion (goto-char (point-min))
						   (re-search-forward "^SENDER:" end-of-headers-marker t))))
				  (progn (forward-line 1) (insert "Sender: " feedmail-sender-line "\n")))

			;; no FROM: ... force one?
			(if feedmail-from-line
				(progn (goto-char (point-min)) (insert "From: " feedmail-from-line "\n")))
			)

		  ;; don't send out a blank subject line
		  (goto-char (point-min))
		  (if (re-search-forward "^Subject:[ \t]*\n" end-of-headers-marker t)
			  (replace-match ""))

		  ;; don't send out a blank headers of various sorts
		  (goto-char (point-min))
		  (and feedmail-nuke-empty-headers  ;; hey, who's an empty-header? 
			   (while (re-search-forward "^[A-Za-z0-9-]+:[ \t]*\n" end-of-headers-marker t)
				 (replace-match ""))))

		;; message ID generation
		(if feedmail-message-id-generator
			(progn
			  (goto-char (point-min))
			  (if (re-search-forward "^MESSAGE-ID:[ \t]*\n" end-of-headers-marker t)
				  (replace-match ""))
			  (setq feedmail-msgid-part (funcall feedmail-message-id-generator))
			  (goto-char (point-min))
			  (and feedmail-msgid-part (string-match "[^ \t]" feedmail-msgid-part)
				  (insert "Message-ID: " feedmail-msgid-part "\n"))))


		(save-excursion (set-buffer feedmail-error-buffer) (erase-buffer))

		(run-hooks 'feedmail-last-chance-hook)

		(if (or (not feedmail-confirm-outgoing) (feedmail-one-last-look feedmail-prepped-text-buffer))
			(funcall feedmail-buffer-eating-function feedmail-prepped-text-buffer feedmail-error-buffer
					 (save-excursion (set-buffer feedmail-address-buffer) (buffer-string)))
		  (error "Sending...abandoned")
		  )
		)  ;; unwind-protect body (save-excursion)

	  ;; unwind-protect cleanup forms
	  (kill-buffer feedmail-prepped-text-buffer)
	  (kill-buffer feedmail-address-buffer)
	  (set-buffer feedmail-error-buffer)
	  (if (zerop (buffer-size))
		  (kill-buffer feedmail-error-buffer)
		(progn (display-buffer feedmail-error-buffer)
			   (error "Sending...failed")))
	  (set-buffer feedmail-raw-text-buffer))
	) ;; let
  )


(defun feedmail-do-bcc (header-end)
  "Delete BCC: and their continuation lines from the header area.
There may be multiple BCC: lines, and each may have arbitrarily
many continuation lines."
  (let ((case-fold-search t))
	(save-excursion (goto-char (point-min))
	  ;; iterate over all BCC: lines
	  (while (re-search-forward "^BCC:" header-end t)
		(delete-region (match-beginning 0) (progn (forward-line 1) (point)))
		;; get rid of any continuation lines
		(while (and (looking-at "^[ \t].*\n") (< (point) header-end))
		  (replace-match ""))
		)
	  ) ;; save-excursion
	) ;; let
  )

(defun feedmail-fill-to-cc-function (header-end)
  "Smart filling of TO: and CC: headers.  The filling tries to avoid
splitting lines except at commas.  This avoids, in particular,
splitting within parenthesized comments in addresses."
  (let ((case-fold-search t)
		(fill-prefix "\t")
		(fill-column feedmail-fill-to-cc-fill-column)
		this-line
		this-line-end)
	(save-excursion (goto-char (point-min))
	  ;; iterate over all TO:/CC: lines
	  (while (re-search-forward "^\\(TO:\\|CC:\\)" header-end t)
		(setq this-line (match-beginning 0))
		(forward-line 1)
		;; get any continuation lines
		(while (and (looking-at "^[ \t]+") (< (point) header-end))
		  (replace-match " ")
		  (forward-line 1))
		(setq this-line-end (point-marker))

		;; The general idea is to break only on commas.  Change
		;; all the blanks to something unprintable; change the
		;; commas to blanks; fill the region; change it back.
		(subst-char-in-region this-line this-line-end ?   2 t) ;; blank --> C-b
		(subst-char-in-region this-line this-line-end ?, ?  t) ;; comma --> blank
		(fill-region-as-paragraph this-line this-line-end)

		(subst-char-in-region this-line this-line-end ?  ?, t) ;; comma <-- blank
		(subst-char-in-region this-line this-line-end  2 ?  t) ;; blank <-- C-b

		;; look out for missing commas before continuation lines
		(save-excursion
		  (goto-char this-line)
		  (while (re-search-forward "\\([^,]\\)\n\t[ ]*" this-line-end t)
			(replace-match "\\1,\n\t")))
		)
	  ) ;; while
	) ;; save-excursion
  )


(defun feedmail-deduce-address-list (feedmail-text-buffer header-start header-end)
  "Get address list suitable for command line use on simple /bin/mail."
  (require 'mail-utils)  ;; pick up mail-strip-quoted-names
  (let
	  ((case-fold-search t)
	   (simple-address-list "")
	   this-line
	   this-line-end)
	(unwind-protect
		(save-excursion
		  (set-buffer feedmail-address-buffer) (erase-buffer)
		  (insert-buffer-substring feedmail-text-buffer header-start header-end)
		  (goto-char (point-min))
		  (while (re-search-forward "^\\(TO:\\|CC:\\|BCC:\\)" header-end t)
			(replace-match "")
			(setq this-line (match-beginning 0))
			(forward-line 1)
			;; get any continuation lines
			(while (and (looking-at "^[ \t]+") (< (point) header-end))
			  (forward-line 1))
			(setq this-line-end (point-marker))
			(setq simple-address-list
				  (concat simple-address-list " "
						  (mail-strip-quoted-names (buffer-substring this-line this-line-end))))
			)
		  (erase-buffer)
		  (insert-string simple-address-list)
		  (subst-char-in-region (point-min) (point-max) 10 ?  t)  ;; newline --> blank
		  (subst-char-in-region (point-min) (point-max) ?, ?  t)  ;; comma   --> blank
		  (subst-char-in-region (point-min) (point-max)  9 ?  t)  ;; tab     --> blank

		  (goto-char (point-min))
		  ;; tidyness in case hook is not robust when it looks at this
		  (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))

		  )
	  )
	)
  )


(defun feedmail-one-last-look (feedmail-prepped-text-buffer)
  "Offer the user one last chance to give it up."
  (save-excursion (save-window-excursion
	(switch-to-buffer feedmail-prepped-text-buffer)
	(y-or-n-p "Send this email? "))))


(provide 'feedmail)
