;;; -*- Mode:Emacs-Lisp -*-

;;; gnus-hide.el	hide quotes and/or signatures
;;; v1.02	
;;;
;;; improve readability and reduce screen output time 
;;; (for slow baud rates) by hiding quotes and/or signatures.
;;;
;;; (Note: this file can be viewed/modified with Awefold 1.0)
;;;
;:: 		QUOTE HIDING
;;;
;;; Don't you hate it when an article has pages of quoted text that you've
;;; already read?  Load this file and you can just type 'h' to get rid of
;;; it and 'H' to bring it back.
;;;
;;; If someone uses something other than ">" to mark the quoted text, it
;;; notices that; it can even cope with the kind of stuff that SuperCite
;;; inserts, and simple indentation (though it tries that as a last resort).
;;; If it can't figure out what the attribution string is, it prompts for it.
;;; 'C-uh' will make it prompt anyway, with it's guess as a default.  If a
;;; SuperCited article has multiple sections like
;;;
;;;       FOO> some text, some text
;;;       FOO> blah blah blah
;;;       oh yeah?
;;;       BAR> some text, some text
;;;       BAR> blah blah blah
;;;
;;; you can generally make both sections go away just by typing 'h' twice.
;;; Also, if two blocks of text to be elided are seperated only by blank
;;; lines, the blank lines are hidden as well.
;;;
;;;	To hide a quote while in subject mode: type "h".
;;;     To show everything including quotes and signature: type "H"
;:|
;;;
;:: 		SIGNATURE HIDING
;;;
;;;	o Hide signatures when they can be identified
;;;	o Useful for reading articles for people with long signatures
;;;	  especially at low baud rates.
;;;	o Useful even at high baud rates to avoid the need for
;;;	  extra scrolling and to aid readability.
;;;     o Will hide most signatures
;;;
;;;	To hide a signature while in subject mode: type "S".
;;;     To show everything including quotes and signature: type "H"
;;;
;:|
;;;
;:: 		LIMITATIONS
;;;
;;;	o sometimes quote hiding and signature hiding will be overzealous
;;;	  and hide stuff you really didn't want to hide. The user must
;;;	  type "H" then, and put up with the quoting and signature 
;;;	  for that article. 
;;;
;;;	o Signature hiding will not catch signatures with unorthodox styles 
;;;	  or signatures (non gnus-generated) that have no "dividing line".
;;;
;:|
;;;	
;:: 		.emacs startup
;;;
;;; To automatically load gnus-hide when starting gnus, 
;;; put this into your .emacs::
;;; (setq gnus-Startup-hook 
;;;   '(lambda ()
;;;	(require 'gnus-hide)))
;;;
;;;
;;; If you want gnus to do quote hiding automatically when you select
;;; an article:
;;; (setq  gnus-Article-prepare-hook 
;;;	'(lambda () (gnus-Article-hide-quote)))	
;;;
;;; If you want gnus to do signature hiding automatically when you select
;;; an article:
;;; (setq  gnus-Article-prepare-hook 
;;;	'(lambda () (gnus-Article-hide-sig)))
;;;
;;; To use both quote hiding and signature hiding use:
;;; (setq gnus-Article-prepare-hook 
;;;	'(lambda ()
;;;		(gnus-Article-hide-quote)
;;;		(gnus-Article-hide-sig)))
;;;
;;; To use aggressive quote prefixes in gnus-Article-hide-quote:
;;; (setq gnus-hide-hookified-be-aggressive t)
;;;
;;; If you want to save "hidden articles", 
;;; (setq gnus-save-article-prepare-hook nil)
;;; By default, it does unhiding, and most people won't want to
;;; change this behavior.
;;;
;;; To YANK Unhidden while followup posting or replying,
;;; (setq mail-yank-hooks 'gnus-hide-yank-original-unhide)
;;; I use supercite as well, so I use:
;;; (setq mail-yank-hooks '(gnus-hide-yank-original-unhide sc-cite-original))
;;;
;;; In order for this to work, sendmail.el must be modified to add 
;;; the mail-yank-hook. 
;;; This will be the case if you've already installed Supercite.
;;; If not, you may use the sendmail.el patches from the supercite
;;; distribution. (These will be posted together with gnus-hide.el).
;;;
;:|
;;;
;:: 		AUTHORS and HISTORY
;;;
;;;  14-dec-90	Tim Lambert <lambert@spectrum.cs.unsw.oz.au>
;;;		Created gnus-hide-quote.el
;;;  27-jan-91	Jamie Zawinski <jwz@lucid.com>
;;;		Made it automatic.
;;;   1-jun-91  Brent J. Krawchuk <krawchuk@cpsc.ucalgary.ca>
;;;		o added gnus-Article-hide-quote
;;;		  for use with gnus-Article-prepare-hook
;;;             o added signature hiding functions
;;;             o renamed to gnus-hide.el
;;;		o made into awefold 1.0 file
;;;		Tim Lambert and Dave Brennan also made numerous style 
;;;		improvement changes and bug fixes.
;;;		Thanx also to J. Zawinski.
;;;
;;;  Feel free to contact the authors to make suggestions, or bug fixes.
;;;
;:|
;;;	
;;;--------------------------------------------------------------------
(provide 'gnus-hide)
(require 'gnus)

(define-key gnus-Subject-mode-map "S" 'gnus-Subject-hide-sig)
(define-key gnus-Subject-mode-map "h" 'gnus-Subject-hide-quote)
(define-key gnus-Subject-mode-map "H" 'gnus-Subject-unhide)

;:: 		QUOTE HIDING FUNCTIONS

;:: Quote Prefixes

(defvar gnus-possible-quote-prefixes
    '("^[^ \t\n\(A-Z#%;]"	;; first, search for ">", "}", etc.
      "^[ \t]+[^ \t\n\(A-Z#%;]"	;; then that with leading whitespace.
				;; these don't use #%; because of shar files
      				;; and postscript and lisp code...
      "^[ \t]*[A-Z]+[]}>[{<-]"  ;; then, SuperCite: "FOO> ", "  Yow>", etc.
      )
  "Regexps to search for to identify quoted-text attributions.
These regexps should match the initial subsequence of the line that is the
attribution prefix.  They are ordered; regexps which are less ambiguous and 
less likely to produce mismatches should come first.  The entire buffer will 
be searched for two or more consecutive lines which match the first element 
of this list, then the second, and so on.  The initial subsequence of the 
two lines which first match is returned. Regular quote hiding also
uses gnus-aggressive-quote-prefixes, unlike hookified quote hiding 
which, by default, does not.")

(defvar gnus-hide-hookified-be-aggressive nil
  "Variable to determine if hooked calling of gnus-hide-Article-quote
should use aggressive quote prefixes. If set to t, aggressive 
prefixes will be used.
Default: nil")

	
(defvar gnus-aggressive-quote-prefixes
    '("^[ \t]+"			;;  simple indentation
      "^[\(#%;]"		;; "comment" chars...
     )
 "Regexps for last-resort hiding. By default, these are not 
used in hookified calling (gnus-Article-hide-{quote/sig}).
See gnus-hide-hookified-be-conservative and gnus-possible-quote-prefixes.")

(defun gnus-identify-quote-prefix (use-aggressive)
  "Figure out what the current message uses for attribution.  See the
documentation for gnus-possible-quote-prefixes."
  (save-excursion
   (save-restriction
    (gnus-find-sig-position)
    (if (not (= (point) (point-min)))
	(narrow-to-region (point-min) (point)))
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (let ((match nil)
	  (start (point))
	  (rest (if use-aggressive 
		    (append gnus-possible-quote-prefixes
			    gnus-aggressive-quote-prefixes)
		    gnus-possible-quote-prefixes)))
      (while (and rest (not match))
	(goto-char start)
	(let ((regexp (car rest)))
	  (while (not (or match (eobp)))
	    (if (re-search-forward regexp nil 0)
		(save-excursion
		  (beginning-of-line)
		  (let ((prefix (buffer-substring (point) (match-end 0))))
		    (forward-line 1)
		    (if (looking-at (regexp-quote prefix))
			(setq match prefix)))))
	    (forward-line 1)))
	(setq rest (cdr rest)))
      match))))

;:|
;:: Hide Quote Routines

(defun gnus-Article-hide-quote (&optional prefix-string)
  "Hide quotations in current article.
For use with gnus-Article-prepare-hook."
  (setq prefix-string (or prefix-string 
			  (gnus-identify-quote-prefix 
					gnus-hide-hookified-be-aggressive)))
    (if prefix-string
	(progn
	  (message "Hiding text beginning with \"%s\"..." prefix-string)
	  (save-excursion
	    (goto-char (point-min))
	    (let ((buffer-read-only nil)
		  (quote-regexp (concat "\n*" (regexp-quote prefix-string))))
	      (gnus-hide-quote-internal quote-regexp)
	      (set-buffer-modified-p nil))
	    (setq selective-display t)
	    )
        (message "Hiding text beginning with \"%s\"... done." 
		 prefix-string))))

(defun gnus-Subject-hide-quote (&optional prefix-string)
  "Hide quotations in current article."
  (interactive (list
		 (let* ((default (gnus-eval-in-buffer-window 
				   gnus-Article-buffer
				   (gnus-identify-quote-prefix t)))
			(string (if (or current-prefix-arg (not default))
				    (read-from-minibuffer
				      (concat
					"String that starts quotation lines"
					(if default
					    (concat " \(default \"" 
							default "\"\)"))
					": "))
				    default)))
		   (if (string= "" string)
		       (or default (error "You tell me, buckaroo."))
		       string))))
  (if (string= prefix-string "") (error "empty string"))
  (gnus-eval-in-buffer-window gnus-Article-buffer
			      (gnus-Article-hide-quote prefix-string)))
     
(defun gnus-hide-quote-internal (prefix)
  (let ((search-pattern (concat "\n+" prefix))
	(looking-at-pattern (concat "^" prefix)))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(and (search-forward "\n\n" nil t)
	     (forward-char -1))
	(while (re-search-forward search-pattern nil t)
	  ;; go forward one line, so that exactly one line of each
	  ;; elided block is visible, to give a little bit of context.
	  (forward-line 1)
	  (while (looking-at looking-at-pattern)
	    (delete-char -1)
	    (insert "\^M")
	    (forward-line 1)))))))
;:|
;:|
;:: 		SIGNATURE HIDING FUNCTIONS

;:: Signature Identification

(defvar gnus-possible-signature-prefixes
  '(
	  "^--[ \t]*$" 		         ;; gnus signature type
	  "^[---=_~\*]+[ \t]*$"	         ;; line type
   )
  "Regexps to search for beginning of a signature.
   They are ordered; regexps which are less ambiguous and 
   less likely to produce mismatches should come first. 
   Replace [\n\C-m] for ^ if you wish the sig indicator
   to be shown.")

(defvar gnus-sig-start-search-point 0
  "Point from end of file to start searching for signature.
  Set in gnus-get-sig-start-search-point.")

(defun gnus-get-sig-start-search-point ()
  "Determine point at which searches for signatures should begin."
 (goto-char (point-max))
 (if (re-search-backward "\n\n" nil 'move)
	 (goto-char (match-beginning 0)))
 (setq gnus-sig-start-search-point 
	(max (point-min) 
	  (min (point) (- (point-max) 500)))))

(defun gnus-search-for-sig-start (regexp-list)
   "Loop through gnus-possible-signature-prefixes until 
    a regexp matches or the end of list is found."
        (goto-char gnus-sig-start-search-point)
	(if regexp-list 
		 (if (re-search-forward (car regexp-list) nil 'move)
		        (goto-char (match-beginning 0))
		     (gnus-search-for-sig-start (cdr regexp-list)))
	))

(defun gnus-find-sig-position ()
  "Move point to start of signature."
   	(gnus-get-sig-start-search-point)
	(gnus-search-for-sig-start 
		gnus-possible-signature-prefixes)
	(if (= (point) gnus-sig-start-search-point)
		(goto-char (point-min)))
	(point))
;:|

(defun gnus-Article-hide-sig ()
  "Signature hiding for use with gnus-Article-prepare-hook."
  (save-excursion
    (let ((buffer-read-only nil))
      (if (not (= 1 (gnus-find-sig-position)))
	  (gnus-hide-to-eob))
      (set-buffer-modified-p nil))
    (setq selective-display t)))

(defun gnus-Subject-hide-sig ()
  "Hide signature."
  (interactive)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (save-excursion
      (let ((buffer-read-only nil))
	(if (not (= 1 (gnus-find-sig-position)))
	    (gnus-hide-to-eob))
        (set-buffer-modified-p nil))
      (setq selective-display t))))

(defun gnus-hide-to-eob ()
  "Hide all lines to end of buffer."
  (subst-char-in-region (point) (point-max) ?\n ?\C-M))

;:|
;::		ARTICLE SAVING

(defvar gnus-save-article-prepare-hook '(lambda () (gnus-Subject-unhide))
 "Hook to prepare article buffer for saving, (o,C-o)
  eg. undoing things that are done by gnus-article-prepare-hook.")

;;; The only difference between these and the gnus 3.13 functions
;;; is the addition of 'gnus-save-article-prepare-hook.

(defun gnus-Subject-save-in-rmail (&optional filename)
  "Append this article to Rmail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (run-hooks 'gnus-save-article-prepare-hook)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(let ((default-name
		(funcall gnus-rmail-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-rmail
			 )))
	  (or filename
	      (setq filename
		    (read-file-name
		     (concat "Save article in Rmail file: (default "
			     (file-name-nondirectory default-name)
			     ") ")
		     (file-name-directory default-name)
		     default-name)))
	  (gnus-make-directory (file-name-directory filename))
	  (gnus-output-to-rmail filename)
	  ;; Remember the directory name to save articles.
	  (setq gnus-newsgroup-last-rmail filename)
	  )))
    ))

(defun gnus-Subject-save-in-mail (&optional filename)
  "Append this article to Unix mail file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (run-hooks 'gnus-save-article-prepare-hook)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(let ((default-name
		(funcall gnus-mail-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-mail
			 )))
	  (or filename
	      (setq filename
		    (read-file-name
		     (concat "Save article in Unix mail file: (default "
			     (file-name-nondirectory default-name)
			     ") ")
		     (file-name-directory default-name)
		     default-name)))
	  (gnus-make-directory (file-name-directory filename))
	  (rmail-output filename)
	  ;; Remember the directory name to save articles.
	  (setq gnus-newsgroup-last-mail filename)
	  )))
    ))

(defun gnus-Subject-save-in-file (&optional filename)
  "Append this article to file.
Optional argument FILENAME specifies file name.
Directory to save to is default to `gnus-article-save-directory' which
is initialized from the SAVEDIR environment variable."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (run-hooks 'gnus-save-article-prepare-hook)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (save-excursion
      (save-restriction
	(widen)
	(let ((default-name
		(funcall gnus-file-save-name
			 gnus-newsgroup-name
			 gnus-current-headers
			 gnus-newsgroup-last-file
			 )))
	  (or filename
	      (setq filename
		    (read-file-name
		     (concat "Save article in file: (default "
			     (file-name-nondirectory default-name)
			     ") ")
		     (file-name-directory default-name)
		     default-name)))
	  (gnus-make-directory (file-name-directory filename))
	  (gnus-output-to-file filename)
	  ;; Remember the directory name to save articles.
	  (setq gnus-newsgroup-last-file filename)
	  )))
    ))

(defun gnus-Subject-save-in-folder (&optional folder)
  "Save this article to MH folder (using `rcvstore' in MH library).
Optional argument FOLDER specifies folder name."
  (interactive)
  (gnus-Subject-select-article
   (not (null gnus-save-all-headers)) gnus-save-all-headers)
  (run-hooks 'gnus-save-article-prepare-hook)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (save-restriction
      (widen)
      ;; Thanks to yuki@flab.Fujitsu.JUNET and ohm@kaba.junet.
      (mh-find-path)
      (let ((folder
	     (or folder
		 (mh-prompt-for-folder "Save article in"
				       (funcall gnus-folder-save-name
						gnus-newsgroup-name
						gnus-current-headers
						gnus-newsgroup-last-folder
						)
				       t
				       )))
	    (errbuf (get-buffer-create " *GNUS rcvstore*")))
	(unwind-protect
	    (call-process-region (point-min) (point-max)
				 (expand-file-name "rcvstore" mh-lib)
				 nil errbuf nil folder)
	  (set-buffer errbuf)
	  (if (zerop (buffer-size))
	      (message "Article saved in folder: %s" folder)
	    (message "%s" (buffer-string)))
	  (kill-buffer errbuf)
	  (setq gnus-newsgroup-last-folder folder))
	))
    ))

;:|
;:: 		UNHIDE

(defun gnus-Subject-unhide ()
  "Show signature and quotations in current article."
  (interactive)
  (gnus-eval-in-buffer-window gnus-Article-buffer
      (let ((buffer-read-only nil))
	(subst-char-in-region (point-min) (point-max) ?\C-M ?\n)
	(set-buffer-modified-p nil))))

(defun gnus-hide-yank-original-unhide ()
 "Unhiding function for use in mail-yank-hooks."
  (let ((buffer-read-only nil))
	(subst-char-in-region (point-min) (point-max) ?\C-M ?\n)
	(set-buffer-modified-p nil)))

;:|
;:|000123;
