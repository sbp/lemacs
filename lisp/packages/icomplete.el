;;;_* Icomplete.el - minibuffer completion with incremental feedback

;;; Loading this package implements a more finely-grained completion
;;; feedback scheme, indicating prospective minibuffer completion
;;; candidates as you type.  See the documentation string for
;;; 'icomplete-prompt' for a specific description of icompletion.

;;; This will not work on Emacs 18 versions - there may be a version
;;; for v18 in the elisp archives, at archive.cis.ohio-state.edu, in
;;; /pub/gnu/emacs/elisp-archive.

;;; It should run on most version of Emacs 19 (including Lucid emacs
;;; 19 - thanks to the efforts of Colin Rafferty (craffert@lehman.com)
;;; - thanks, Colin!)

;;;_ + Provide
(provide 'icomplete)

;;;_ + Package Identification Stuff
;;;_  - Author: Ken Manheimer <klm@nist.gov>
;;;_  - Maintainer: Ken Manheimer <klm@nist.gov>
;;;_  - Version: <<<cvs ate me>>>
;;;_  - Created: Mar 1993 klm@nist.gov - first release to usenet
;;;_  - Keywords: extensions
;;;_  - LCD Archive Entry and RCS ID
;;; icomplete|Ken Manheimer|klm@nist.gov
;;; |Completion enhancement, with incremental feedback in the minibuffer.
;;;

;;;_  - GNU Copyright
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Developed by Ken Manheimer, klm@nist.gov.

;;;_ + User Customization variables
;;;_  = icomplete-inhibit
(defvar icomplete-inhibit nil
  "*Set this variable to t at any time to inhibit icomplete.")

;;;_ + Setup
;;;_  - Internal Variables
;;;_   = icomplete-eoinput 1
(defvar icomplete-eoinput 1
  "Point where minibuffer input ends and completion info begins.")
(make-variable-buffer-local 'icomplete-eoinput)
;;;_  > icomplete-prime-session ()
(defun icomplete-prime-session ()

  "Prep emacs v 19 for icompletion.  For emacs v19.18 and later revs,
icomplete is installed in 'minibuffer-setup-hook'.  Global pre- and
post-command-hook functions are used in v19.17 and earlier revs."

  (cond ((string-match "^19\\.\\([0-9]+\\)\\." emacs-version)
	 (let* ((vers (string-to-int (substring emacs-version
						(match-beginning 1)
						(match-end 1)))))
	   (if (> vers 17)
	       ;; Post v19rev17, use minibuffer-setup-hook:
	       (add-hook 'minibuffer-setup-hook 'icomplete-prime-minibuffer)
	     ;; v19rev18 and prior, use global pre- and -post-commands:
	     (add-hook 'pre-command-hook 'icomplete-pre-command-hook 'append)
	     (add-hook 'post-command-hook
		       'icomplete-post-command-hook 'append))))
	((string-match "Lucid" emacs-version)
	 (add-hook 'pre-command-hook 'icomplete-pre-command-hook 'append)
	 (add-hook 'post-command-hook
		   'icomplete-post-command-hook 'append))
	(format "icomplete.el: non v19 emacs / Lucid version %s - %s"
		emacs-version "try prior icomplete code")))
;;;_  > icomplete-prime-minibuffer ()
(defun icomplete-prime-minibuffer ()

  "Prep emacs, v 19.18 or later, for icomplete.  \(icomplete-prime-
session establishes global hooks, instead, in emacs 19 versions 19.17
and prior.\)  Run via minibuffer-setup-hook \(emacs 19.18 or later\),
adds icomplete pre- and post-command hooks to do icomplete display
management."

  ;; We append the hooks because preliminary version of blink-paren
  ;; post-command-hook i have interferes with proper operation of
  ;; minibuffer quit.
  (make-local-variable 'pre-command-hook)
  (make-local-variable 'post-command-hook)
  (add-hook 'pre-command-hook 'icomplete-pre-command-hook)
  (add-hook 'post-command-hook 'icomplete-post-command-hook))

;;;_  > icomplete-window-minibuffer-p ()
(defun icomplete-window-minibuffer-p ()

  "Returns non-nil if current window is a minibuffer window."

  (if (boundp 'lucid-logo)
      (window-minibuffer-p nil)
    (window-minibuffer-p)
    ))

;;;_ + Completion
;;;_  - Completion feedback hooks
;;;_   > icomplete-pre-command-hook ()
(defun icomplete-pre-command-hook ()
  "Cleanup completions exhibit before user's new input (or whatever) is dealt
with."
  (if (and (icomplete-window-minibuffer-p)
	   (not (symbolp minibuffer-completion-table))
	   (not icomplete-inhibit))
      (if (and (boundp 'icomplete-eoinput)
	       icomplete-eoinput)
	  (if (> icomplete-eoinput (point-max))
	      ;; Oops, got rug pulled out from under us - reinit:
	      (setq icomplete-eoinput (point-max))
	    (let ((buffer-undo-list buffer-undo-list ))	; prevent entry
	      (delete-region icomplete-eoinput (point-max))))
	(make-local-variable 'icomplete-eoinput)
	(setq icomplete-eoinput 1))))
;;;_   > icomplete-post-command-hook ()
(defun icomplete-post-command-hook ()
  "Exhibit completions, leaving icomplete-eoinput with position where user
input leaves off and exhibit begins, so icomplete-pre-command-hook can
subsequently cleanup."
  (if (and (icomplete-window-minibuffer-p)	; ... in a minibuffer.
	   (not icomplete-inhibit)	; ... not specifically inhibited.
	   ;(sit-for 0)			; ... redisplay and if there's input
					; waiting, then don't icomplete
					; (stigs suggestion)
	   ;; Inhibit for file-name and other custom-func completions:
	   (not (symbolp minibuffer-completion-table))
	   )
      (let ((buffer-undo-list buffer-undo-list ))	; prevent entry
	(icomplete-exhibit))))
;;;_   > icomplete-window-setup-hook ()
(defun icomplete-window-setup-hook ()
  "Exhibit completions, leaving icomplete-eoinput with position where user
input leaves off and exhibit begins, so icomplete-pre-command-hook can
subsequently cleanup."
  (if (and (icomplete-window-minibuffer-p)	; ... in a minibuffer.
	   )
      (message "ic ws doing")(sit-for 1)))
;;;_   > icomplete-exhibit ()
(defun icomplete-exhibit ()
  "Exhibit completions, leaving icomplete-eoinput with position where user
input leaves off and exhibit begins, so icomplete-pre-command-hook can
subsequently cleanup."
  (if (not (symbolp minibuffer-completion-table))
      (let ((contents (buffer-substring (point-min)(point-max)))
	    (buffer-undo-list t))
	(save-excursion
	  (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
	  (if (not (boundp 'icomplete-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'icomplete-eoinput))
	  (setq icomplete-eoinput (point))
                                        ; Insert the match-status information:
	  (if (> (point-max) 1)
	      (insert-string
		(icomplete-prompt contents
				  minibuffer-completion-table
				  minibuffer-completion-predicate
				  (not
				   minibuffer-completion-confirm))))))))

;;;_  - Completion feedback producer
;;;_   > icomplete-prompt (name candidates predicate require-match)
(defun icomplete-prompt (name candidates predicate require-match)
  "Identify prospective candidates for minibuffer completion.

The display is updated with each minibuffer keystroke when icomplete
is enabled \(by loading the 'icomplete' elisp package\) and doing
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
one of \(), \[], or \{} pairs.  The choice of brackets is as follows:

  \(...) - a single prospect is identified and matching is enforced,
  \[...] - a single prospect is identified but matching is optional, or
  \{...} - multiple prospects, separated by commas, are indicated, and
          further input is required to distingish a single one.

The displays for disambiguous matches have \" [Matched]\" appended
\(whether complete or not), or \" \[No matches]\", if no eligible
matches exist."

  (let ((comps (all-completions name candidates predicate))
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]"))
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
        )
    (cond ((null comps) (format " %sNo matches%s"
                                open-bracket-determined
                                close-bracket-determined))
          ((null (cdr comps))           ;one match
           (concat (if (and (> (length (car comps))
                               (length name)))
                       (concat open-bracket-determined
                               (substring (car comps) (length name))
                               close-bracket-determined)
                     "")
                   " [Matched]"))
          (t                            ;multiple matches
           (let* ((most (try-completion name candidates predicate))
                  (most-len (length most))
                  most-is-exact
                  (alternatives
                   (apply
                    'concat
                    (cdr (apply 'append
                                (mapcar '(lambda (com)
                                           (if (= (length com) most-len)
                                               ;; Most is one exact match,
                                               ;; note that and leave out
                                               ;; for later indication:
                                               (progn
                                                 (setq most-is-exact t)
                                                 ())
                                             (list ","
                                                   (substring com
                                                              most-len))))
                                        comps))))))
             (concat (and (> most-len (length name))
                          (concat open-bracket-determined
                                  (substring most (length name))
                                  close-bracket-determined))
                     open-bracket-prospects
                     (if most-is-exact
                         (concat "," alternatives)
                       alternatives)
                     close-bracket-prospects))))))

;;;_ + Initialization
(icomplete-prime-session)

;;;_* Local emacs vars.
'(
Local variables:
eval: (save-excursion
        (if (not (condition-case err (outline-mode t)
                   (wrong-number-of-arguments nil)))
            (progn
              (message
               "Allout outline-mode not loaded, not adjusting buffer exposure")
              (sit-for 1))
          (message "Adjusting '%s' visibility" (buffer-name))
          (outline-lead-with-comment-string ";;;_")
          (goto-char 0)
          (outline-exposure -1 0)))
End:)
