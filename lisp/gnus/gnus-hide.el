;;;--------------------------------------------------------------------
;;;
;;; gnus-hide.el	hide quotes and/or signatures
;;; v1.04		simplify references
;;;
;;; improve readability and reduce screen output time 
;;; (for slow baud rates) by hiding quotes and/or signatures.
;;;
;;;
;;; (Note: this file can be viewed/modified with Awefold 1.0)
;;;
;:: 		QUOTE HIDING
;;;
;;; Don't you hate it when an article has pages of quoted text that you've
;;; already read?  Load this file and you can just type 'h' or '['
;;; to get rid of it and 'H' or ']' to bring it back.
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
;;;	To hide a quote while in subject mode: type "h" or "[".
;;;     To show everything including quotes and signature: type "H" or "]"
;;;
;;; Quote hiding can also be done automatically on selecting an article.
;;; 	(see .emacs setup section below)
;;;     You may temporarily turn autohiding off by typing "'" or "C-cC-r"
;:|
;;;
;:: 		SIGNATURE HIDING
;;;
;;;	Can't stand long and ugly signatures? Hate seeing the 
;;;	same signatures over and over again? Hate scrolling 
;;;	the screen with <SPC> only to find a signature on the 
;;;	next page? TRY SIGNATURE HIDING!
;;;
;;;	o Hide signatures when they can be identified
;;;	o Useful for reading articles for people with long signatures
;;;	  especially at low baud rates.
;;;	o Useful even at high baud rates to avoid the need for
;;;	  extra scrolling and to aid readability.
;;;     o Will hide most signatures
;;;
;;;	To hide a signature while in subject mode: type "S" or "{"
;;;     To show everything including quotes and signature: type "H" or "]"
;;;
;;; Signature hiding can also be done automatically on selecting an article.
;;; 	(see .emacs setup section below)
;:|
;;;
;::		REFERENCE SIMPLIFICATION
;;;
;;; Can't stand those ugly reference lines? Don't understand what 
;;; they mean anyway? Use reference simplification.
;;;
;;; Simply type "C-cC-r" or "}" in subject mode, and all those long 
;;; reference lines will be simplified.
;;; For example, 
;;;	In article < ... > foo@goo.edu (Jawn Dough) writes:
;;; will be simplified to:
;;;	(Jawn Dough) writes:
;;; 
;;; Reference hiding can also be done automatically on selecting an article.
;;; 	(see .emacs setup section below)
;:|
;;;
;:: 		LIMITATIONS
;;;
;;;	o sometimes quote hiding and signature hiding will be overzealous
;;;	  and hide stuff you really didn't want to hide. The user must
;;;	  type "H" then, and put up with the quoting and signature 
;;;	  for that article. This is particularly the case when 
;;;	  gnus-hide-hookified-be-aggressive is set to t.
;;;
;;;	o Signature hiding may occasionally not recognize some things
;;;	  that look like signatures. (Paragraphs with street addresses
;;;	  without email addresses, for example).
;;;
;;;	o Reference simplification is not reversible. Reselecting the 
;;;	  article will restore the original reference lines. (If using
;;;	  autohiding -- hookified simplification -- first you will have to
;;;	  toggle the autohiding by pressing "'", the apostrophe key.
;;;
;:|
;;;	
;:: 		.emacs startup
;;;
;:: Hooks
;;;
;;; To automatically load gnus-hide when starting gnus, 
;;; put this into your .emacs::
;;; (setq gnus-Startup-hook 
;;;   '(lambda ()
;;;	(require 'gnus-hide)))
;;;
;;; If you want gnus to do quote hiding automatically when you select
;;; an article:
;;; (setq  gnus-Article-prepare-hook 'gnus-Article-hide-quote)
;;;
;;; If you want gnus to do signature hiding automatically when you select
;;; an article:
;;; (setq  gnus-Article-prepare-hook 'gnus-Article-hide-sig)
;;;
;;; If you want gnus to do reference hiding automatically when you select
;;; an article:
;;; (setq  gnus-Article-prepare-hook 'gnus-Article-simplify-references)
;;;
;;; To use a couple or all three of these, put the names in a list like this:
;;;
;;; (setq gnus-Article-prepare-hook 
;;;	'(gnus-Article-hide-quote 
;;;	  gnus-Article-hide-sig
;;;	  gnus-Article-simplify-references))
;;;
;:|
;:: Variables
;;;
;;; -- To use aggressive quote prefixes in gnus-Article-hide-quote
;;; 	(setq gnus-hide-hookified-be-aggressive t)
;;;
;;; -- To turn off aggressive signature hiding:
;;; 	(setq gnus-hide-sig-aggressively nil)
;;;
;;; -- If you want to save "hidden articles", 
;;; 	(setq gnus-save-article-prepare-hook nil)
;;;
;;;    (By default, it does unhiding, and most people won't want to
;;;     change this behavior.)
;;;
;;; -- To YANK Unhidden while followup posting or replying,
;;; 	(setq mail-yank-hooks 'gnus-hide-yank-original-unhide)
;;;     (autoload 'gnus-hide-yank-original-unhide "gnus-hide" "" t)
;;;
;;;    I use supercite as well, so I use:
;;; 	(setq mail-yank-hooks '(gnus-hide-yank-original-unhide 
;;;				sc-cite-original))
;;;
;;; -- To do automatic quote hiding only on followups:
;;;	(setq gnus-autohide-only-on-followup t)
;;;
;;; -- To not show the first line of a hidden quote
;;;	(setq gnus-hide-show-first-line nil)
;;;
;;; -- To not place the ellipsis on a newline (when gnus-hide-show-first-line
;;;	is set to nil), ie. place it on the same line as the reference line.
;;;	(setq gnus-hide-place-ellipsis-on-newline nil)
;;; 
;;; Other customizable variables are available for more experienced users...
;;;
;:|
;:|
;;;
;:: 		AUTHORS and HISTORY
;;;
;;;  14-dec-90	Tim Lambert <lambert@spectrum.cs.unsw.oz.au>
;;;		o Created gnus-hide-quote.el
;;;  27-jan-91	Jamie Zawinski <jwz@lucid.com>
;;;		o Made it automatic.
;;;   1-jun-91  Brent J. Krawchuk <krawchuk@cpsc.ucalgary.ca>
;;;             o renamed to gnus-hide.el
;;;		o autohiding (use of article prepare hook)
;;;             o signature hiding functions
;;;		o reference hiding functions
;;;		o made into awefold 1.0 file
;;;		Tim Lambert, J. Zawinski, Dave Brennan and Dan Jacobson
;;;		o added/improved code, suggestions, bug fixes
;;;
;;;  Feel free to contact the authors to make suggestions, or bug fixes.
;;;
;:|
;;;	
;;;--------------------------------------------------------------------
(require 'gnus)

;::		KEY DEFINITIONS

(define-key gnus-Subject-mode-map "S"    'gnus-Subject-hide-sig)
(define-key gnus-Subject-mode-map "h"    'gnus-Subject-hide-quote)
(define-key gnus-Subject-mode-map "H"    'gnus-Subject-unhide)
(define-key gnus-Subject-mode-map "\C-c\C-r" 'gnus-Subject-simplify-references)
(define-key gnus-Subject-mode-map "{"    'gnus-Subject-hide-sig)
(define-key gnus-Subject-mode-map "}"    'gnus-Subject-simplify-references)
(define-key gnus-Subject-mode-map "["    'gnus-Subject-hide-quote)
(define-key gnus-Subject-mode-map "]"    'gnus-Subject-unhide)
(define-key gnus-Subject-mode-map "'" 	 'gnus-hide-autohide-toggle)

;:|

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
See gnus-hide-hookified-be-aggressive and gnus-possible-quote-prefixes.")


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

(defvar gnus-autohide-only-on-followup nil
  "When set to t, the first articles in threads will not be hidden.")


(defvar gnus-hide-show-first-line t
  "When set to t (default), the first line of a quote is not
hidden, to give some context.")

(defvar gnus-hide-place-ellipsis-on-newline t
  "If t, put ellipsis on new line when gnus-hide-show-first-line is nil")


(defun gnus-Article-is-followupp ()
  "Is current article a followup?"
  (string-match "^[Rr][Ee][:\^] "
		(gnus-fetch-field "Subject")))

(defun gnus-Article-hide-quote (&optional prefix-string)
  "Hide quotations in current article.
For use with gnus-Article-prepare-hook."
  (if 	gnus-hide-autohide-toggle
  (progn
  (setq prefix-string (or prefix-string 
			  (and (or (not gnus-autohide-only-on-followup)
				   (gnus-Article-is-followup))
			  (gnus-identify-quote-prefix 
					gnus-hide-hookified-be-aggressive))))
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
		 prefix-string))))))


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
  (let ((gnus-hide-autohide-toggle t))
  (gnus-eval-in-buffer-window gnus-Article-buffer
			      (gnus-Article-hide-quote prefix-string)))
)

      
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
	  (if gnus-hide-show-first-line
	    (forward-line 1)
	    (progn 
		(goto-char (match-beginning 0))
		(if gnus-hide-place-ellipsis-on-newline
		   (progn (forward-char 1)	; skip first-newline
		     (if (looking-at prefix) ; already
			(insert "\n"))))	; add a new newline

		; eat up leading newlines
		(while (looking-at "\n")
			 (delete-char 1)
			 (insert "\^M"))))


	  (while (looking-at prefix)
	    (delete-char -1)
	    (insert "\^M")
	    (forward-line 1)))))))




;:|

;:|

;:: 		SIGNATURE HIDING FUNCTIONS

;:: Signature Identification

(defvar gnus-possible-signature-prefixes
  '(
   "[\n\C-m]--[ \t]*$" 		 ;; gnus signature type
				 ;; line type (at least 2 fancy chars)
   "[\n\C-m][---=_~\*\$\+\|\^:;\\\/\<]+[---=_~\*\$\+\|\^:;\\\/\<]+[ \t]*$" 
   "[\n\C-m]---"		 ;; --- type
   "[\n\C-m]--[A-Za-z ]"	 ;; --Name ... type
   "[\n\C-m]-[A-Za-z ]"		 ;; -Name ... type
   )
  "Regexps to search for beginning of a signature.
   They are ordered; regexps which are less ambiguous and 
   less likely to produce mismatches should come first. 
   Replace [\n\C-m] for ^ if you wish the sig indicator
   to be shown."
)


(defvar gnus-hide-sig-aggressively t
  "When set, the last paragraph will be searched 
for an email address. If one is found, assume it is a signature,
and hide it.")

(defvar gnus-hide-largest-signature 650
  "The largest size of signature to hide. The larger this number,
the greater the chance that non-signatures will be mistakenly hidden")

(defun gnus-find-sig-position ()
  "Move point to start of signature. Moves to point-min if none found."
  (let ((start 	(max (progn (goto-char (point-min))
			    (re-search-forward "\n\n" nil t)
			    (point))
		     (- (point-max) gnus-hide-largest-signature))))
	(goto-char start)
	(gnus-search-for-sig-start gnus-possible-signature-prefixes start)
 	(if (= (point) start) 			; no divider
	  (if (not (gnus-last-paragraph-sigp))  ; no addr in last para
	      (goto-char (point-min))))
	(point)))

(defun gnus-search-for-sig-start (regexp-list start)
   "Loop through gnus-possible-signature-prefixes until 
    a regexp matches or the end of list is found."
	(if regexp-list 
		 (if (re-search-forward (car regexp-list) nil t)
		        (goto-char (match-beginning 0))
		     (progn (goto-char start)
			     (gnus-search-for-sig-start 
				  (cdr regexp-list) start)))))


(defun gnus-last-paragraph-point ()
  "Point at start of last paragraph in buffer.
Return nil if a \n\n is not found."
  (save-excursion 
	(goto-char (point-max))
	(re-search-backward "[a-zA-Z]" nil t)	; skip trailing whitespace
	(if (re-search-backward "\n[\n\t ]*\n" nil t)
	    (match-beginning 0)
	    nil)))

(defun gnus-start-of-article-point ()
 "Point at which article begins."
  (save-excursion 
	(goto-char (point-min))
	(if (re-search-forward "\n\n" nil t)
	    (match-beginning 0)
	    nil)))


(defun gnus-last-paragraph-sigp ()
  "Is last paragraph a signature? If so, move point there.
The last paragraph is not considered to be a signature if 
it is the only paragraph in the article."
   (if gnus-hide-sig-aggressively
   (let ((lpp (gnus-last-paragraph-point)))
	(if (not (= lpp (gnus-start-of-article-point)))  ; not only para
	(if lpp  (progn (goto-char lpp) (gnus-address-belowp)))))))


(defun gnus-address-belowp ()
  "non-nil if there is what looks like an email address below this
point in the buffer (handles internet and uucp addresses)."
  (or	(string-match "[a-zA-Z0-9]+@[---a-zA-Z0-9\.]+[\"\|\)>\n\t ]" ;internet 
	   (buffer-substring (point) (point-max)))
	(string-match "\\([a-zA-Z0-9]+\!\\)+[a-zA-Z0-9]+[\n\t ]" ;uucp
	   (buffer-substring (point) (point-max)))))
	


;:|
;:: The Signature Hiding Functions


(defun gnus-Article-hide-sig ()
  "Signature hiding for use with gnus-Article-prepare-hook."
  (if 	gnus-hide-autohide-toggle
  (save-excursion
    (let ((buffer-read-only nil))
      (if (not (= 1 (gnus-find-sig-position)))
	  (gnus-hide-to-eob))
      (set-buffer-modified-p nil))
    (setq selective-display t))))


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

;:|

;:: 		REFERENCE CLEANING FUNCTIONS

(defvar gnus-reference-regexps 
 '(
	"In article \<.*\>.*\("
	"In \<.*\>.*\("
	"On .*T, .*\@.*\("
	"In article \<.*\>.*\n[^>]*\(" 		; across 2 lines
	"In \<.*\>.*\n[^>]*\("	      		; "" (no quote > before ()
	"In article \<.*\>[ ,\t\n]*"	 	; empty name
	"\>\>\>\>\>.*\n*.*\("  	      		; Supercite verbose
	"^[A-Za-z]*\>+[ ]*In.*\("    		; Sc simple	
	"^.*\@.*\(.*\) writes:"			; no In article.
	"^.*\@.*\(.*\) \/.*\/.* writes:"	; " In article with date
     )
 "Regexps to match reference lines."
)


(defun orify-regexp-list (regexps)
 "Convert list of reg expressions to or form."
 (if regexps
 (if (cdr regexps)
	(concat (car regexps) "\\|" (orify-regexp-list (cdr regexps)))
        (car regexps))))

(defun gnus-simplify-references ()
  "Create one big or'ed together regexp from a list of regexps."
  (let ((Ref-Regexp (orify-regexp-list gnus-reference-regexps)))
  (goto-char (point-min))
  (while  (re-search-forward Ref-Regexp nil t)
	(gnus-ref-simplify)
	(forward-line 1) 
	(beginning-of-line))))


(defun gnus-ref-simplify ()
 "Simplify found reference"
 (let	((MB (match-beginning 0))
	 (ME (match-end 0)))
  (goto-char MB)
  (bjk-replace  (if (re-search-forward "^[A-Za-z]*[=:#>]+" ME t)
		    (match-end 0) 	; end of quote marker
		    MB)
		(if (re-search-forward "(" ME t) 
		    (match-beginning 0) 
		    ME)			; can't find a name	
		"")
  ;; hack to get rid of date strings still leftover
  (goto-char MB)
  (if (re-search-forward "\/.*\/" ME t)
	  (bjk-replace (match-beginning 0) (match-end 0) ""))
  ;; hack to get rid of newlines in middle of namestring
  (goto-char MB)
  (if (looking-at "([a-zA-Z ]+\n[a-zA-Z ]+)")
	(progn (end-of-line) (delete-char 1) (insert " "))) ))
  


(defun bjk-replace (Start End String)
 "replace text between Start and End with String"
 (save-excursion
	(delete-region Start End)
	(goto-char  Start)
	(insert String)))

(defun gnus-Article-simplify-references ()
 "Simplify all references in current buffer."
 (if gnus-hide-autohide-toggle
 (save-excursion 
  (let ((buffer-read-only nil))
	(gnus-simplify-references)
 	(set-buffer-modified-p nil)))))


(defun gnus-Subject-simplify-references ()
 "Simplify all references in current article."
  (interactive)
  (let ((gnus-hide-autohide-toggle t))	
  (gnus-eval-in-buffer-window gnus-Article-buffer
	(gnus-Article-simplify-references))))


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

;::		TOGGLE
(defvar gnus-hide-autohide-toggle t
 "Only use autohiding functions if t (not nil). This toggle
is changed by the function gnus-hide-autohide-toggle.")

(defun gnus-hide-autohide-toggle ()
 "Toggle the autohiding feature. May be useful for 
saving articles that use auto reference simplification."
 (interactive)
 (setq gnus-hide-autohide-toggle (not gnus-hide-autohide-toggle))
 (message (if gnus-hide-autohide-toggle "Autohiding on." "Autohiding off.")))


;:|

;::		OVERLOAD FUNCTIONS 
;;;
;::	Overload Code 
;; Code from Barry Warsaw, Supercite 2.2 
;; (with minor renaming mods for gnus-hide)
;; ======================================================================
;; functions which do the overloading
;; based on code supplied by umerin@tc.nagasaki.go.jp

(defvar gnus-hide-overload-functions
  '((mail-yank-original       	 sc-mail-yank-original) 
    (gnus-Subject-save-in-rmail  gnus-hide-Subject-save-in-rmail)
    (gnus-Subject-save-in-mail 	 gnus-hide-Subject-save-in-mail)
    (gnus-Subject-save-in-file 	 gnus-hide-Subject-save-in-file)
    (gnus-Subject-save-in-folder gnus-hide-Subject-save-in-folder)
    )
  "*Functions to be overloaded by gnus-hide.
It is a list of '(original overload)', where original is the original
function symbol, overload is the supercite equivalent function.")


(defun gnus-hide-overload-functions ()
  "Overload functions defined by the variable gnus-hide-overload-functions.
If the original symbol is not yet bound, it will not be overloaded.
Also, if the symbol has already been overloaded, it will not be
overloaded again."
  (let ((binding nil)
	(overloads gnus-hide-overload-functions))
    (while overloads
      (setq binding (car overloads)
	    overloads (cdr overloads))
      (and (fboundp (car binding))
	   (not (get (car binding) 'gnus-hide-overloaded))
	   (progn
	     (fset (car binding) (symbol-function (car (cdr binding))))
	     (put (car binding) 'gnus-hide-overloaded 'gnus-hide-overloaded))
	   )
      )))



;:|

;::	Sendmail Overloads 
;; Code from Barry Warsaw's Supercite 2.2 (with minor mods)
;; ======================================================================
;; sendmail.el overload functions. This is the heart of supercite
;; conformance by packages which rely on distribution emacs elisp. You
;; should almost always overload this function.

(defun sc-mail-yank-original (arg)
  "Supercite version of mail-yank-original.
This function is the workhorse which many packages rely upon to do
citing. It inserts the message being replied to in the reply buffer.
Puts point before the mail headers and mark after body of text.

Citation is accomplished by running the hook mail-yank-hooks and is
thus user configurable. Default is to indent each nonblank line ARG
spaces (default 3). Just \\[universal-argument] as argument means
don't indent and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  ;; mod 28-Jul-1989 bwarsaw@cen.com
	  ;; generalized, hookified citations
	  (run-hooks 'mail-yank-hooks))
	(exchange-point-and-mark)
	(if (not (eolp)) (insert ?\n)))))

(defvar mail-yank-hooks nil 
  "*Hook to run citation function.
Expects point and mark to be set to the region to cite.")



;:|

;::	Article Saving Overloads



(defvar gnus-save-article-prepare-hook '(lambda () (gnus-Subject-unhide))
 "Hook to prepare article buffer for saving, (o,C-o)
  eg. undoing things that are done by gnus-article-prepare-hook."
)

;;; The only difference between these and the gnus 3.13 functions
;;; is the addition of 'gnus-save-article-prepare-hook


;:: Rmail Save 
(defun gnus-hide-Subject-save-in-rmail (&optional filename)
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

;:|
;:: Unix Mail Save

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


;:|
;:: Gnus File Save

(defun gnus-hide-Subject-save-in-file (&optional filename)
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

;:|
;:: MH Folder Save

(defun gnus-hide-Subject-save-in-folder (&optional folder)
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
;:|
;:|

(provide 'gnus-hide)

;:|
