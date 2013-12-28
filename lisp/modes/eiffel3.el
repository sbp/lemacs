;;; Id: eiffel3.el,v 1.35 1993/10/27 21:38:11 tynor Exp 
;;;--------------------------------------------------------------------------
;;; TowerEiffel 1.0
;;; Tower Technology Corporation
;;; Copyright (c) 1993 - All rights reserved.
;;;
;;; This file is made available for use and distribution under the same terms 
;;; as GNU Emacs. Such availability of this elisp file should not be construed 
;;; as granting such availability to the rest of TowerEiffel.
;;;--------------------------------------------------------------------------
;;; Portions of the file, as indicated below, were derived from "eiffel.el"
;;; and "eif-mult-fmt.el
;;; Copyright (C) 1989, 1990 Free Software Foundation, Inc. and Bob Weiner
;;; Available for use and distribution under the same terms as GNU Emacs.
;;;--------------------------------------------------------------------------
;;;
;;;  EIFFEL3  : GNU Emacs mode for Eiffel Version 3
;;;
;;;  INSTALLATION
;;;    To install, simply copy this file into a directory in your
;;;    load-path and add the following two commands in your .emacs file:
;;;
;;;        (setq auto-mode-alist (cons '("\\.e$" . eiffel-mode) 
;;;   			               auto-mode-alist)))))
;;;        (autoload 'eiffel-mode "eiffel3" "Mode for Eiffel programs" t)
;;;
;;;    TowerEiffel users should do the following instead: See the file
;;;    dot-emacs that comes with the TowerEiffel distribution for a sample
;;;    ".emacs" file. If all Tower elisp files are already in your
;;;    load-path, then simply add the following line to your .emacs file: 
;;;
;;;        (load "tinstall")
;;;
;;;  SUPPORT
;;;    Please send bug reports, fixes or enhancements to:
;;;	   fred.hart@atlanta.twr.com
;;;
;;;  COMPATIBILITY:
;;;    This file has been tested with Epoch 4, Emacs 18, Lemacs
;;;    19.6 and 19.8. It has not yet been tested with Gnu Emacs 19.
;;;    Syntax highlighting is currently supported only under Lemacs
;;;    with lhilit.el.
;;;
;;;    (Editorial from jwz: lhilit.el is slow and otherwise obsolete.
;;;	  Someone please convert this to use font-lock-mode instead.)
;;;
;;;  COMMANDS
;;;    eif-backward-sexp
;;;    eif-feature-quote
;;;    eif-forward-sexp
;;;    eif-goto-matching-line
;;;    eif-indent-region
;;;    eif-indent-construct
;;;    eif-indent-line
;;;    eif-newline
;;;    eiffel-mode
;;;
;;;  PUBLIC VARIABLES
;;;    eif-body-comment-indent
;;;    eif-check-keyword-indent
;;;    eif-class-level-comment-indent
;;;    eif-class-level-kw-indent
;;;    eif-extra-body-comment-indent
;;;    eif-extra-check-keyword-indent
;;;    eif-extra-class-level-comment-indent
;;;    eif-extra-class-level-kw-indent
;;;    eif-extra-feature-level-comment-indent
;;;    eif-extra-feature-level-indent
;;;    eif-extra-feature-level-kw-indent
;;;    eif-extra-inherit-level-kw-indent
;;;    eif-extra-then-indent
;;;    eif-feature-level-comment-indent
;;;    eif-feature-level-indent
;;;    eif-feature-level-kw-indent
;;;    eif-indent-increment
;;;    eif-inherit-level-kw-indent
;;;    eif-rescue-keyword-indent
;;;    eif-then-indent
;;;    eiffel-mode-abbrev-table
;;;    eiffel-mode-hook
;;;    eiffel-mode-map
;;;    eiffel-mode-syntax-table
;;;
;;;  PUBLIC FUNCTIONS
;;;    None.
;;;
;;;  HISTORY
;;;    Fred Hart    - Jul 31, 1992: Created.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Detection of lemacs and epoch.                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar running-lemacs  (string-match "Lucid" emacs-version))
(defvar running-lemacs19-6 (and running-lemacs (string-match "19.6" emacs-version)))
(defvar running-lemacs19-8 (and running-lemacs (string-match "19.8" emacs-version)))
(defvar running-epoch  (and (boundp 'epoch::version) epoch::version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Indentation Amount Variables.                   ;;;
;;;                                                              ;;;
;;; The default values correspond to style used in ``Eiffel: The ;;;
;;; Language''.  Note: for TowerEiffel users the values below    ;;;
;;; will be superceded by the values in either tcustom.el or     ;;;
;;; ~/.tcustom.el if  it is present.                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eif-indent-increment                   3 
  "Default indentation interval (in spaces)")

(defvar eif-class-level-kw-indent        0 
  "Indentation amount for Class level keywords (in number of
eif-indent-increments) (see eif-class-level-keywords variable).")
(defvar eif-extra-class-level-kw-indent        0 
  "Number of SPACES to add to eif-class-level-kw-indent to get the
actual indentation of a class level keyword. Can be negative.")

(defvar eif-class-level-comment-indent   0 
  "Indentation of comments at the beginning of the class (in number of
eif-indent-increments)")
(defvar eif-extra-class-level-comment-indent   0 
  "Number of SPACES to add to eif-class-level-comment-indent to get the
actual indentation of a class level comment. Can be negative.")

(defvar eif-inherit-level-kw-indent      2 
  "Indentation of keywords falling under the Inherit clause (in number of
eif-indent-increments) (see eif-inherit-level-keywords variable.")
(defvar eif-extra-inherit-level-kw-indent      0 
  "Number of SPACES to add to eif-inherit-level-kw-indent to get the
actual indentation of an inherit level keyword. Can be negative.")

(defvar eif-feature-level-indent         1 
  "Indentation amount of features. (in number of eif-indent-increments)")
(defvar eif-extra-feature-level-indent         0 
  "Number of SPACES to add to eif-feature-level-indent to get the
actual indentation of a feature. Can be negative.")

(defvar eif-feature-level-kw-indent      2 
  "Indentation of keywords belonging to individual features. (in number of
eif-indent-increments) (see eif-feature-level-keywords variable)")
(defvar eif-extra-feature-level-kw-indent      0 
  "Number of SPACES to add to eif-feature-level-kw-indent to get the
actual indentation of a feature level keyword. Can be negative.")

(defvar eif-feature-level-comment-indent 3 
  "Indentation of comments at the beginning of a feature. (in number of
eif-indent-increments)")
(defvar eif-extra-feature-level-comment-indent 0 
  "Number of SPACES to add to eif-feature-level-comment-indent to get the
actual indentation of a feature level comment. Can be negative.")

(defvar eif-body-comment-indent 0 
  "Indentation of comments in the body of a routine. (in number of
eif-indent-increments)")
(defvar eif-extra-body-comment-indent 0 
  "Number of SPACES to add to eif-body-comment-indent to get the
actual indentation of a routine body comment. Can be negative.")

(defvar eif-check-keyword-indent         0
  "Extra indentation for the check clause as described in ETL. (in number of
eif-indent-increments). Default is 0, which is different than in ETL's 1.")
(defvar eif-extra-check-keyword-indent         0
  "Number of SPACES to add to eif-check-keyword-indent to get the
actual indentation of a check keyword. Can be negative.")

(defvar eif-rescue-keyword-indent         -1
  "Extra indentation for the rescue clause as described in ETL. (in number of
eif-indent-increments). Default is -1.")
(defvar eif-extra-rescue-keyword-indent         0
  "Number of SPACES to add to eif-rescue-keyword-indent to get the
actual indentation of a rescue keyword. Can be negative.")

(defvar eif-then-indent                  0
  "Indentation for a `then' appearing on a line by itself rather 
than on the same line as an `if'. (in number of eif-indent-increments)")
(defvar eif-extra-then-indent                  1
  "Number of SPACES to add to eif-then-indent to get the
actual indentation of a `then' appearing on a line by itself. Can be 
negative.")

(defvar eif-continuation-indent                1
  "Extra indentation for a continued statement line. (in number of eif-indent-increments)")
(defvar eif-extra-continuation-indent          0
  "Number of SPACES to add to eif-continuation-indent to get the
actual indentation of a continued statement line. Can be 
negative.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  Lucid Emacs hilit support                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (featurep 'lhilit)
    (progn
      ;; The value for a font variable must either be a string
      ;; specifying a valid font, the symbol 'default meaning the
      ;; default font, or the symbol 'context meaning the font of the
      ;; surrounding text. 
      ;;
      ;; Simlarly, the value for a color variable must either be a string
      ;; specifying a valid color, the symbol 'default meaning the
      ;; default foreground color, or the symbol 'context meaning the
      ;; foregound color of the  surrounding text. 
      (defvar comment-font 'default
	"The font in which to display comments in Eiffel and Ace files (either a font name string or 'default or 'context)")
      (defvar comment-color "red3" 
	"Color of comments in Eiffel and Ace files (either a color name string or 'default or 'context)")

      (defvar hidden-comment-font 'default
	"The font in which to display hidden comments in Eiffel and Ace files (either a font name string or 'default or 'context)")
      (defvar hidden-comment-color "forestgreen" 
	"Color of hidden comments in Eiffel and Ace files (either a color name string or 'default or 'context)")

      (defvar major-keyword-font "-*-fixed-bold-*-*-*-*-120-*-*-*-*-*-*"
	"The font in which to display major keywords in Eiffel and Ace files (either a font name string or 'default or 'context)")
      (defvar major-keyword-color 'default
	"Color of major keywords in Eiffel and Ace files (either a color name string or 'default or 'context)")

      (defvar assertion-keyword-font "-*-fixed-bold-*-*-*-*-120-*-*-*-*-*-*"
	"The font in which to display assertion keywords in Eiffel and Ace files (either a font name string or 'default or 'context)")
      (defvar assertion-keyword-color "slate blue"
	"Color of assertion keywords in Eiffel and Ace files (either a color name string or 'default or 'context)")

      (defvar minor-keyword-font "-*-fixed-bold-*-*-*-*-120-*-*-*-*-*-*"
	"The font in which to display minor keywords in Eiffel and Ace files (either a font name string or 'default or 'context)")
      (defvar minor-keyword-color 'default
	"Color of minor-keywords in Eiffel and Ace files (either a color name string or 'default or 'context)")

      (defvar string-font 'default
	"The font in which to display literal strings in Eiffel and Ace files (either a font name string or 'default or 'context)")
      (defvar string-color "brown"
	"Color of literal strings in Eiffel and Ace files (either a color name string or 'default or 'context)")

      (defvar quoted-feature-font "-*-times-bold-i-*-*-*-120-*-*-*-*-*-*"
	"The font in which to display features names enclosed in `'s in Eiffel and Ace file comments (either a font name string or 'default or 'context)")
      (defvar quoted-feature-color 'context 
	"Color of features names enclosed in `'s in Eiffel and Ace file comments (either a color name string or 'default or 'context)")

      (defvar default-foreground-color 'default
	"Default text color in Eiffel and Ace files (either a color name string or 'default or 'context)")

      (defvar disable-color nil "Should hilighting not use colors")


      (defun eif-set-foreground (face color)
	"Set the FACE's foreground color to COLOR if COLOR is a string, to the default foreground color if COLOR is 'default, or to the color of the surrounding text if COLOR is 'context"
	(cond ((stringp color)
	       (cond (running-lemacs (set-face-foreground face color))
		     (running-epoch nil)
		     )
	       )
	      ((eq color 'context)
	       (cond (running-lemacs (set-face-foreground face nil))
		     (running-epoch nil)
		     )
	       )
	      ((eq color 'default)
	       (cond (running-lemacs 
		      (set-face-foreground face (face-foreground 'default))
		      )
		     (running-epoch nil)
		     )
	       )
	      )
	)

      (defun eif-set-font (face font)
	"Set the FACE's font to FONT if FONT is a string, to the default font if FONT is 'default, or to the font of the surrounding text if FONT is 'context"
	(cond ((stringp font)
	       (cond (running-lemacs (set-face-font face font))
		     (runing-epoch nil)
		     )
	       )
	      ((eq font 'context)
	       (cond (running-lemacs (set-face-font face nil))
		     (runing-epoch nil)
		     )
	       )
	      ((eq font 'default)
	       (cond (running-lemacs (set-face-font face (face-font 'default)))
		     (runing-epoch nil)
		     )
	       )
	  )
	)

      (defun eif-init-color ()
	"Reset the Eiffel fonts and faces from the values of their repective variables"
	(hilit::create-face-if-needed 'comment nil)
	(hilit::create-face-if-needed 'hidden-comment nil)
	(hilit::create-face-if-needed 'major-keyword nil)
	(hilit::create-face-if-needed 'minor-keyword nil)
	(hilit::create-face-if-needed 'quoted-feature nil)
	(hilit::create-face-if-needed 'assertion nil)
	(hilit::create-face-if-needed 'string nil)

	(if (and (x-color-display-p) (not disable-color))
	    (progn
	      (eif-set-foreground 'comment         comment-color)
	      (eif-set-font       'comment         comment-font)
	      (eif-set-foreground 'hidden-comment  hidden-comment-color)
	      (eif-set-font       'hidden-comment  hidden-comment-font)
	      (eif-set-foreground 'quoted-feature  quoted-feature-color)
	      (eif-set-font       'quoted-feature  quoted-feature-font)
	      (eif-set-foreground 'major-keyword   major-keyword-color)
	      (eif-set-font       'major-keyword   major-keyword-font)
	      (eif-set-foreground 'minor-keyword   minor-keyword-color)
	      (eif-set-font       'minor-keyword   minor-keyword-font)
	      (eif-set-foreground 'assertion       assertion-keyword-color)
	      (eif-set-font       'assertion       assertion-keyword-font)
	      (eif-set-foreground 'string          string-color)
	      (eif-set-font       'string          string-font)
	      )
	  (eif-set-foreground 'comment         default-foreground-color)
	  (eif-set-font       'comment         comment-font)
	  (eif-set-foreground 'hidden-comment  default-foreground-color)
	  (eif-set-font       'hidden-comment  hidden-comment-font)
	  (eif-set-foreground 'quoted-feature  default-foreground-color)
	  (eif-set-font       'quoted-feature  quoted-feature-font)
	  (eif-set-foreground 'major-keyword   default-foreground-color)
	  (eif-set-font       'major-keyword   major-keyword-font)
	  (eif-set-foreground 'minor-keyword   default-foreground-color)
	  (eif-set-font       'minor-keyword   minor-keyword-font)
	  (eif-set-foreground 'assertion       default-foreground-color)
	  (eif-set-font       'assertion       assertion-keyword-font)
	  (eif-set-foreground 'string          default-foreground-color)
	  (eif-set-font       'string          string-font)
	  )
	)

      (eif-init-color)

      ;; ---- Eiffel mode -----	
      ;; NOTE: The order of keywords below is generally alphabetical except 
      ;; when one keyword is the prefix of another (e.g. "and" & "and then")
      ;; In such cases, the prefix keyword MUST be the last one.
      (defvar eiffel-mode-hilit
	    '(
	      ("--|.*" nil hidden-comment 4)	;; hidden comments
	      ("--[^\n|].*\\|--$" nil comment 3);; comments
	      ("`[^`']*'" nil quoted-feature 5)	;; quoted expr's in comments
	      ("^creation\\|^deferred[ \t]*class\\|^expanded[ \t]*class\\|^class\\|^feature\\|^indexing\\|^inherit\\|^obsolete" nil major-keyword 1) ;; major keywords
	      ("[ \t\n)(,;]\\(alias\\|all\\|and then\\|and\\|as\\|debug\\|deferred\\|do\\|else\\|elseif\\|end\\|export\\|external\\|from\\|frozen\\|if not\\|if\\|implies not\\|implies\\|infix\\|inspect\\|is\\|like\\|local\\|loop\\|not\\|obsolete\\|old\\|once\\|or else\\|or\\|prefix\\|redefine\\|rename\\|rescue\\|retry\\|select\\|strip\\|then\\|undefine\\|until\\|when\\|xor\\)[ \t\n)(,;]" nil minor-keyword 0) ;; minor keywords
	      ("[ \t\n)(,;]\\(check\\|ensure then\\|ensure\\|invariant\\|require else\\|require\\|variant\\)[ \t\n)(,;]" nil assertion 2) ;; assertions
	      ("\\(\"\"\\)\\|\\(\"\\([^\"%]\\|%.\\|%\n\\)+\"\\)" nil string 2) ;; strings
	      ))
      (hilit::mode-list-update "Eiffel" eiffel-mode-hilit)
    ;; ---- Ace mode -----	
      (defvar ace-mode-hilit
	    '(
	      ("--|.*"    nil hidden-comment 2)	;; hidden comments
	      ("--[^\n|].*\\|--$" nil comment 1);; comments
	      ("`[^`']*'" nil quoted-feature)	;; quoted expr's in comments
	      ("^system\\|^default\\|^root\\|^cluster" nil major-keyword);; major keywords

	      ))
      (hilit::mode-list-update "Ace" ace-mode-hilit)
      )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     No user-customizable definitions below this point.       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro eif-class-level-kw-indent-m () 
  "Indentation amount for Class level keywords (in number of spaces)."
  '(+ (* eif-class-level-kw-indent eif-indent-increment) 
     eif-extra-class-level-kw-indent)
)

(defmacro eif-class-level-comment-indent-m () 
  "Indentation amount for Class level comments (in number of spaces)."
  '(+ (* eif-class-level-comment-indent eif-indent-increment) 
     eif-extra-class-level-comment-indent)
)

(defmacro eif-inherit-level-kw-indent-m () 
  "Indentation amount for Inherit level keywords (in number of spaces)."
  '(+ (* eif-inherit-level-kw-indent eif-indent-increment) 
     eif-extra-inherit-level-kw-indent)
)

(defmacro eif-feature-level-indent-m () 
  "Indentation amount for features (in number of spaces)."
  '(+ (* eif-feature-level-indent eif-indent-increment) 
     eif-extra-feature-level-indent)
)

(defmacro eif-feature-level-kw-indent-m () 
  "Indentation amount for Feature level keywords (in number of spaces)."
  '(+ (* eif-feature-level-kw-indent eif-indent-increment) 
     eif-extra-feature-level-kw-indent)
)

(defmacro eif-body-comment-indent-m () 
  "Indentation amount for comments in routine bodies (in number of spaces)."
  '(+ (* eif-body-comment-indent eif-indent-increment) 
     eif-extra-body-comment-indent)
)

(defmacro eif-feature-level-comment-indent-m () 
  "Indentation amount for Feature level comments (in number of spaces)."
  '(+ (* eif-feature-level-comment-indent eif-indent-increment) 
     eif-extra-feature-level-comment-indent)
)

(defmacro eif-check-keyword-indent-m ()
  "Indentation amount for Check keyword (in number of spaces)."
  '(+ (* eif-check-keyword-indent eif-indent-increment) 
     eif-extra-check-keyword-indent)
)

(defmacro eif-rescue-keyword-indent-m ()
  "Indentation amount for Rescue keyword (in number of spaces)."
  '(+ (* eif-rescue-keyword-indent eif-indent-increment) 
     eif-extra-rescue-keyword-indent)
)

(defmacro eif-then-indent-m ()
  "Indentation amount for `then' appearing on a line by itself (in number of spaces)."
  '(+ (* eif-then-indent eif-indent-increment) 
     eif-extra-then-indent)
)

(defmacro eif-continuation-indent-m ()
  "Indentation amount for a statement continuation line (in number of spaces)."
  '(+ (* eif-continuation-indent eif-indent-increment) 
     eif-extra-continuation-indent)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Keyword Regular Expression Variables.               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eif-all-keywords-regexp 
  "\\(indexing\\|class\\|inherit\\|creation\\|feature\\|invariant\\|rename\
\\|redefine\\|undefine\\|select\\|export\\|require\\|local\\|deferred\
\\|do\\|once\\|ensure\\|alias\\|external\\|check\\|rescue\\|debug\\|if\
\\|inspect\\|from\\|else\\|elseif\\|when\\|until\\|variant\\|loop\\|then\
\\|obsolete\\|end\\)[^a-z0-9_]"
  "Regular Expression to identify the presence of any eiffel keyword in a line.
Does not include `is'."
  )

;; Note invariant is handled as a special case since it is both a 
;; class-level and a from-level keyword
;; Note obsolete is handled as a special case since it is both a 
;; class-level and a feature-level keyword
(defvar eif-class-level-keywords 
  "\\(indexing\\|class\\|deferred[ \t]*class\\|expanded[ \t]*class\\|inherit\\|creation\\|feature\\)[^a-z0-9_]" 
  "Those keywords introducing class-level clauses. Note that `invariant'
and `obsolete' are not included here since can function as more than one type of keyword. "
  )

(defvar eif-inherit-level-keywords 
  "\\(rename\\|redefine\\|undefine\\|select\\|export\\)" 
  "Those keywords which introduce subclauses of the inherit clause."
  )

(defvar eif-feature-level-keywords 
  "\\(require\\|local\\|deferred\\|do\\|once\\|ensure\\|alias\\|external\\)[^a-z0-9_]"
  "Those keywords which are internal to features (in particular, routines)."
  )

(defvar eif-end-keyword "end" "The `end' keyword.")

(defvar eif-end-keyword-regexp "[^a-z0-9_]end[^a-z0-9_]" 
  "The `end' keyword with context.")

(defvar eif-end-matching-keywords
  "\\(check\\|class\\|debug\\|feature\\|rename\\|redefine\\|undefine\\|select\\|export\\|do\\|once\\|deferred\\|external\\|alias\\|if\\|inspect\\|from\\|debug\\)[^a-z0-9_]"
  "Those keywords whose clause is terminated by an `end' keyword."
  )

(defvar eif-control-flow-keywords 
  "\\(if\\|inspect\\|from\\|debug\\)"
  "Keywords which introduce control-flow constructs."
  )

(defvar eif-control-flow-matching-keywords
  "\\(deferred\\|do\\|once\\|if\\|inspect\\|from\\|debug\\)[^a-z0-9_]" 
  "Keywords whose occurance prior to a control-flow-keyword causes the
indentation of the control-flow-keyword. Note that technically,
`end' is part of this list but it is handled separately in the
functions: eif-matching-indent and eif-matching-kw."
  )

(defvar eif-check-keyword "check"  "The `check' keyword.")

(defvar eif-check-keywords   "\\(check\\)[^a-z0-9_]"  
  "The `check' keyword (with trailing context).")

(defvar eif-check-matching-keywords 
  "\\(deferred\\|do\\|once\\|if\\|inspect\\|from\\|debug\\)[^a-z0-9_]"
  "Keywords whose occurance prior to a check-keyword causes the
indentation of the check-keyword. Note that technically, `end' is
part of this list but it is handled separately in the functions:
eif-matching-indent and eif-matching-kw. (see also
eif-control-flow-matching-keywords)"
  )

(defvar eif-rescue-keyword "rescue"  "The `rescue' keyword.")

(defvar eif-obsolete-keyword "obsolete"  "The `obsolete' keyword.")

(defvar eif-rescue-keywords   "\\(rescue\\)[^a-z0-9_]"  
  "The `rescue' keyword (with trailing context).")

(defvar eif-rescue-matching-keywords 
  "\\(deferred\\|do\\|once\\)[^a-z0-9_]"
  "Keywords whose occurance prior to a rescue-keyword causes the
indentation of the rescue-keyword. Note that technically, `end' is
part of this list but it is handled separately in the functions:
eif-matching-indent and eif-matching-kw. (see also
eif-control-flow-matching-keywords)"
  )

(defvar eif-from-level-keywords 
  "\\(until\\|variant\\|loop\\)[^a-z0-9_]"
  "Keywords occuring inside of a from clause."
  )

(defvar eif-from-keyword  "from" "The keyword `from'.")

(defvar eif-if-or-inspect-level-keywords "\\(elseif\\|else\\|when\\)[^a-z0-9_]"
  "Keywords occuring inside of an if or inspect clause."
  )

(defvar eif-if-or-inspect-keyword "\\(if\\|inspect\\)[^a-z0-9_]"
  "The `if' or `inspect' keywords."
  )

(defvar eif-solitary-then-keyword "then" "The keyword `then'.")

(defvar eif-then-matching-keywords "\\(if\\|elseif\\|when\\)"
  "Keywords whose occurance prior to a then-keyword sets the
indentation of the then-keyword. Note that technically, `end' is
part of this list but it is handled separately in the functions:
eif-matching-indent and eif-matching-kw. (see also
eif-control-flow-matching-keywords)"
  )

(defvar eif-invariant-keyword "invariant" "The `invariant' keyword.")

(defvar eif-invariant-matching-keywords 
  "\\(from\\|feature\\)"
  "Keywords whose occurance prior to an invariant-keyword causes the
indentation of the invariant-keyword. Note that technically, `end'
is part of this list but it is handled separately in the functions:
eif-matching-indent and eif-matching-kw. (see also
eif-control-flow-matching-keywords)"
  )

(defvar eif-obsolete-matching-keywords 
  "\\(is\\|class\\)"
  "Keywords whose occurance prior to an obsolete-keyword causes the
indentation of the obsolete-keyword."
  )

(defvar eif-white-space-regexp       "[ 	]*"
  "RE to locate whitespace.")

(defvar eif-comment-line-regexp      "[ 	]*\\(--.*\\)$" 
  "RE to match a line with a comment on it.")

(defvar eif-non-source-line          "[ 	]*\\(--.*\\)?$" 
  "RE to match a line with a only a comment or whitespace.")

(defvar eif-variable-or-const-regexp "[^()]*:[^=].*" 
  "RE to match a variable or constant declaration.")

(defvar eif-indentation-keywords-regexp 
  "\\(indexing\\|class\\|check\\|rescue\\|inherit\\|creation\\|feature\\|invariant\\|rename\\|redefine\\|undefine\\|select\\|export\\|require\\|local\\|deferred\\|do\\|once\\|ensure\\|alias\\|external\\|if\\|inspect\\|from\\|debug\\|else\\|elseif\\|when\\|until\\|variant\\|invariant\\|loop\\|obsolete\\)[^a-z0-9_]"
  "RE to identify the presence of any eiffel keyword triggering indentation"
  )

(defvar eif-feature-indentation-keywords-regexp 
  "\\(creation\\|feature\\)[^a-z0-9_]"
  "Keywords which denote the presence of features following them."
  )

(defvar eif-is-keyword-regexp "\\(.*[ 	)]\\)?is[ 	]*\\(--.*\\)?$"
  "The `is' keyword (with some context)."
  )

(defvar eif-multiline-routine-is-keyword-regexp
  ".*([^)]*)\\([ \t\n]*\\|[ \t\n]*:[][ \t\nA-Za-x0-9_]*\\)is[ 	]*\\(--.*\\)?$"
  "The `is' keyword (with some context)."
  )

(defvar eif-operator-regexp
  "[ 	]*\\([@*/+]\\|-[^-]\\|and[ 	(]\\|or[ 	(]\\)"
  "Eiffel operators - used to identify continuation lines"
  )

(defvar eif-operator-eol-regexp
  ".**\\([@*/+-]\\|and\\|or\\|:=\\)[ 	]*$"
  "Eiffel operators - used to identify continuation lines"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eif-matching-indent -1 
  "The indentation of the keyword found on the last call to eif-matching-kw. 
-1 if no match was found."
  )

(defvar eif-matching-kw-for-end nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Indentation Functions.                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eif-calc-indent ()
  "Calculate the indentation the current line of eiffel code. This
function generally assumes that the preceding line of code is
indented properly although lines containing certain class-level
constructs do not require correct indentation of the preceding line."
  (let ((indent   0)
	(line-end 0)
	(originally-looking-at-comment nil)
	(kw-match nil)
	(continuation)
	)
    
    (save-excursion
      
      ;; Save location of line-end and skip past leading white space.
      (end-of-line)
      (setq line-end   (point))
      (beginning-of-line)
      (re-search-forward eif-white-space-regexp line-end t)
      
      ;; Is the line we are trying to indent a comment line?
      (setq originally-looking-at-comment (looking-at eif-comment-line-regexp))
      
      ;; Look for a keyword on the current line
      (if (looking-at eif-all-keywords-regexp)
	  
	  ;; Then we are looking at a keyword
	  (cond ((looking-at eif-class-level-keywords)
		 ;; File level keywords (indent defaults to 0)
		 (setq indent (eif-class-level-kw-indent-m))
		 )
		((looking-at eif-inherit-level-keywords)
		 ;; Inherit level keywords (indent defaults to 
		 ;; 2*eif-indent-increment)
		 (setq indent (eif-inherit-level-kw-indent-m))
		 )
		((looking-at eif-feature-level-keywords)
		 ;; Feature level keywords (indent defaults to 
		 ;; (eif-feature-level-indent-m) + eif-indent-increment)
		 (setq indent (eif-feature-level-kw-indent-m))
		 )
		((looking-at eif-end-keyword)
		 ;; End keyword (indent to level of matching keyword)
		 (if (string-match "end" 
				   (eif-matching-kw eif-end-matching-keywords))
		     ;; Then 
		     (if (= eif-matching-indent 
			    (eif-feature-level-kw-indent-m))
			 ;; Then
			 (setq indent (eif-class-level-kw-indent-m))
		       ;; Else
		       (setq indent 
			     (- eif-matching-indent eif-indent-increment))
		       )
		   ;; Else
		   (setq indent eif-matching-indent)
		   )
		 (if (<= indent (eif-feature-level-indent-m))
		     (save-excursion
		       (end-of-line)
		       (while (and (< (point) (point-max))
				   (or (forward-char 1) t)
				   (looking-at eif-non-source-line)
				   )
			 (end-of-line)
			 )
		       (if (not (looking-at eif-non-source-line))
			   (setq indent (eif-inherit-level-kw-indent-m))
			 (setq indent (eif-class-level-kw-indent-m))
			 )
		       )
		   )
		 )
		((looking-at eif-control-flow-keywords)
		 ;; Control flow keywords 
		 ;;  Indent to same level as a preceding "end" or
		 ;;  if no preceding "end" is found, indent to the level
		 ;;  of the preceding "do" plus the value of 
		 ;;  eif-indent-increment
		 (setq kw-match 
		       (eif-matching-kw eif-control-flow-matching-keywords)) 
		 (cond ((string-match "end" kw-match)
			(setq indent eif-matching-indent)
			)
		       (t
			(setq indent 
			      (+ eif-matching-indent eif-indent-increment)
			      )
			)
		       )
		 )
		((looking-at eif-check-keywords)
		 ;; Check keyword
		 ;;  Indent to level of preceding "end"+eif-indent-increment or
		 ;;  if no preceding "end" is found, indent to the level
		 ;;  of the preceding eif-check-matching-keywords plus the 
		 ;;  value (eif-indent-increment + eif-check-keyword-indent).
		 (setq kw-match (eif-matching-kw eif-check-matching-keywords)) 
		 (cond ((string-match "end" kw-match)
			(setq indent (+ eif-matching-indent 
					(eif-check-keyword-indent-m)
					)
			      )
			)
		       (t
			(setq indent 
			      (+ eif-matching-indent 
				 (+ eif-indent-increment 
				    (eif-check-keyword-indent-m)
				    )
				 )
			      )
			)
		       )
		 )
		((looking-at eif-rescue-keywords)
		 ;; Rescue keyword
		 ;;  Indent to level of preceding "end"+eif-indent-increment or
		 ;;  if no preceding "end" is found, indent to the level
		 ;;  of the preceding eif-rescue-matching-keywords plus the 
		 ;;  value (eif-indent-increment + eif-rescue-keyword-indent).
		 (setq kw-match (eif-matching-kw eif-rescue-matching-keywords)) 
		 (cond ((string-match "end" kw-match)
			(setq indent (+ eif-matching-indent 
					(eif-rescue-keyword-indent-m)
					)
			      )
			)
		       (t
			(setq indent eif-matching-indent)
			)
		       )
		 )
		((looking-at eif-from-level-keywords)
		 ;; From level keywords (indent to level of matching "From")
		 (if (string-match "end" (eif-matching-kw eif-from-keyword))
		     ;; Closest matching KW is `end'.
		     (setq indent (- eif-matching-indent eif-indent-increment))
		   ;; Closest matching KW is one of `eif-from-keyword'.
		   (setq indent eif-matching-indent)
		   )
		 )
		((looking-at eif-if-or-inspect-level-keywords)
		 ;; If level keywords (indent to level of matching 
		 ;; "If" or "Inspect")
		 (if (string-match "end" 
				   (eif-matching-kw eif-if-or-inspect-keyword)
				   )
		     ;; Closest matching KW is `end'.
		     (setq indent (- eif-matching-indent eif-indent-increment))
		   ;; Closest matching KW is one of `eif-if-or-inspect-keyword'.
		   (setq indent eif-matching-indent)
		   )
		 )
		((looking-at eif-solitary-then-keyword)
		 ;; Handles case where "then" appears on a line by itself
		 ;;   (Indented to the level of the matching if, elseif or when)
		 (setq indent (+ (eif-matching-indent eif-then-matching-keywords) 
				 (eif-then-indent-m)
				 )
		       )
		 )
		((looking-at eif-invariant-keyword)
		 ;; Invariant keyword
		 ;;   (Indented to the level of the matching from or feature)
		 (if (string-match "from" 
				   (eif-matching-kw eif-invariant-matching-keywords)
				   )
		     ;; Then - loop invariant
		     (setq indent eif-matching-indent)
		   ;; Else - class invariant
		   (setq indent (eif-class-level-kw-indent-m))
		   )
		 )
		((looking-at eif-obsolete-keyword)
		 ;; Obsolete keyword
		 ;;   (Indented to the level of the matching from or feature)
		 (if (string-match "is" 
				   (eif-matching-kw eif-obsolete-matching-keywords)
				   )
		     ;; Then - feature obsolete
		     (setq indent (eif-feature-level-kw-indent-m))
		   ;; Else - class obsolete
		   (setq indent (eif-class-level-kw-indent-m))
		   )
		 )		;; end of cond
		)
	
	;; Else no keyword on current line, 
	;;   are we in a multi-line parenthesis expression
	
	(if (and (> (eif-in-paren-expression) 0) 
		 (> (setq indent (eif-indent-multi-line)) -1))
	    
	    ;; multi-line parenthesis expression
	    indent
	  
	  ;; Else Find the first preceding line with non-comment source on it
	  ;; that is not a continuation line of a multi-line parnethesized
	  ;; expression.

	  ;; Record whether this line begins with an operator. We assume 
	  ;; that the line is a continuation line if it begins with an operator
	  (beginning-of-line)
	  (if (looking-at eif-operator-regexp)
	      (setq continuation t)
	    (setq continuation nil)
	    )
	  
	  (previous-line 1)
	  (beginning-of-line)
	  (while (and (looking-at eif-non-source-line) (not (= (point) 1)))
	    (previous-line 1)
	    (beginning-of-line)
	    )
	  (if (eif-line-contains-close-paren)
	      (backward-sexp)
	    )
	  (end-of-line)
	  (setq line-end (point))
	  (beginning-of-line)
	  (re-search-forward eif-white-space-regexp line-end t)
	  
	  (cond ((and (= (point) 1)
		      originally-looking-at-comment
		      (setq indent (eif-class-level-comment-indent-m))
		      )
		 )
		;; 'eif-is-keyword-regexp' case must precede 
		;; '(not eif-all-keywords-regexp)' case since "is" is not 
		;; part of 'eif-all-keywords-regexp'
		((or (looking-at eif-is-keyword-regexp)
		     (looking-at eif-multiline-routine-is-keyword-regexp)
		     (looking-at eif-obsolete-keyword) 
		     )
		 ;;;; Indent to the level of a feature level comment
		 ;;(setq indent (eif-feature-level-comment-indent-m))
		 (if originally-looking-at-comment
		     ;; Then  the line we are trying to indent is a comment
		     (setq indent (eif-feature-level-comment-indent-m))
		   ;; Else  the line being indented is not a comment
		   (setq indent (eif-feature-level-kw-indent-m))
		   )
		 )
		((looking-at eif-feature-indentation-keywords-regexp)
		 (setq indent (eif-feature-level-indent-m))
		 )
		((looking-at eif-indentation-keywords-regexp)
		 (setq indent (+ (eif-current-line-indent) eif-indent-increment))
		 )
		((looking-at eif-solitary-then-keyword)
		 (setq indent (- (+ (eif-current-line-indent) eif-indent-increment)
				 (eif-then-indent-m)
				 )
		       )
		 )
		((looking-at eif-end-keyword)
		 (if (= (setq indent (eif-current-line-indent)) 
			(eif-feature-level-kw-indent-m)
			)
		     (setq indent (eif-feature-level-indent-m))
		   (eif-matching-line)
		   (if (string-match eif-check-keyword eif-matching-kw-for-end)
		       (setq indent (- indent (eif-check-keyword-indent-m)))
		     )
		   )
		 )
		((looking-at eif-variable-or-const-regexp)
		 (if originally-looking-at-comment
		     ;; Then  the line we are trying to indent is a comment
		     (if (= (setq indent (eif-current-line-indent)) 
			    (eif-feature-level-indent-m)
			    )
			 ;; Then - a feature level comment
			 (setq indent (eif-feature-level-comment-indent-m))
		       ;; Else - some other kind of comment
		       (setq indent (+ indent (eif-body-comment-indent-m)))
		       )
		   ;; Else  the line being indented is not a comment
		   (setq indent (eif-current-line-indent))
		   )
		 )
		((not (looking-at eif-all-keywords-regexp))
		 (if originally-looking-at-comment
		     ;; Then  the line we are trying to indent is a comment
		     (setq indent (+ (if (eif-continuation-line)
					 (- (eif-current-line-indent) 
					    eif-indent-increment
					    )
				       (eif-current-line-indent)
				       )
				     (eif-body-comment-indent-m)
				     )
			   )
		   ;; Else line being indented is not a comment

		   ;; The line the point is on is the one above the line being
		   ;; indented
		   (beginning-of-line)
		   (if (or continuation (looking-at eif-operator-eol-regexp))
		       ;; Then the line being indented is a continuation line
		       (if  (eif-continuation-line)
			   ;; The line preceding the line being indented is 
			   ;; also a continuation line. Indent to the current
			   ;; line indentation.
			   (setq indent (eif-current-line-indent))
			 ;; Else The line preceding the line being indented is 
			 ;; not a continuation line. Indent an extra 
			 ;; eif-continuation-indent
			 (setq indent (+ (eif-current-line-indent)
					 (eif-continuation-indent-m)))
			 )
		     ;; Else the line being indented is not a continuation line.
		     (if (eif-continuation-line)
			 ;; Then the line preceding the one being indented is 
			 ;; a continuation line. Un-indent by an 
			 ;; eif-continuation-indent.
			(setq indent (- (eif-current-line-indent) 
					eif-indent-increment
					)
			      )
		       ;; Else the line preceding the line being indented is
		       ;; also not a continuation line. Use the current indent.
		       (setq indent (eif-current-line-indent))
		       )
		     )
		   )
		 )
		) ;; cond
	  ) ;; if
	) ;; if
      ) ;; save-excursion
    indent
    ) ;; let
  )

(defun eif-continuation-line ()
  (or (looking-at eif-operator-regexp)
      (save-excursion 
	(previous-line 1)
	(beginning-of-line)
	(looking-at eif-operator-eol-regexp)
	)
      )
  )

(defun eif-matching-indent (matching-keyword-regexp)
  "Search backward from the point looking for one of the keywords
in the MATCHING-KEYWORD-REGEXP. Return the indentation of the
keyword found. If an `end' keyword occurs prior to finding one of
the keywords in MATCHING-KEYWORD-REGEXP and it terminates a check
clause, return the indentation of the `end' minus the value of
eif-check-keyword-indent."
  (let ((search-regexp (concat "[^a-z0-9A-Z_]"
			       eif-end-keyword 
			       "[^a-z0-9A-Z_]\\|[^a-z0-9A-Z_]" 
			       matching-keyword-regexp
			       )
		       )
	(indent 0)
	(continue t)
	)
    (save-excursion
      (while (and (re-search-backward search-regexp 1 t)
		  (or (eif-in-quoted-string-p)
		      (eif-in-comment-p)
		      )
		  )
	)
      (if (looking-at search-regexp)
	  ;; Then
	  (if (and (looking-at eif-end-keyword-regexp)
		   (eif-matching-line)
		   (string-match eif-check-keyword eif-matching-kw-for-end)
		   )
	      ;; The keyword "end" was found that terminated a "check" clause
	      (setq indent (- (eif-current-line-indent) (eif-check-keyword-indent-m)))
	    ;; Else a keyword in "matching-keyword-regexp" or a normal 
	    ;; "end"was found
	    (setq indent (eif-current-line-indent))
	    )
	;; Else
	(message "No matching indent keyword was found")
	)
      indent
    
      )
    )
  )

(defun eif-matching-kw (matching-keyword-regexp)
  "Search backward from the point looking for one of the keywords in
the MATCHING-KEYWORD-REGEXP. Return the keyword found. Also set the
value of eif-matching-indent to the indentation of the keyword
found.  If an `end' keyword occurs prior to finding one of the
keywords in MATCHING-KEYWORD-REGEXP and it terminates a check
clause, set the value of eif-matching-indent to the indentation of
the `end' minus the value of eif-check-keyword-indent."
  (let ((search-regexp (concat "[^a-z0-9A-Z_]" 
			       eif-end-keyword 
			       "[^a-z0-9A-Z_]\\|[^a-z0-9A-Z_]" 
			       matching-keyword-regexp
			       )
		       )
	(keyword "")
	)
    (save-excursion
      ;; Search backward for a matching keyword.
      (while (and (re-search-backward search-regexp 1 t)
		  (or (eif-in-quoted-string-p)
		      (eif-in-comment-p)
		      )
		  )
	)
      (if (looking-at search-regexp)
	  ;; Then - a keyword was found
	  (progn
	    (setq keyword 
		  (buffer-substring (match-beginning 0) (match-end 0))
		  )
	    (if (and (looking-at eif-end-keyword-regexp)
		     (eif-matching-line)
		     (string-match eif-check-keyword eif-matching-kw-for-end)
		     )
		;; Then
		(setq eif-matching-indent 
		      (- (eif-current-line-indent) (eif-check-keyword-indent-m))
		      )
	      ;; Else
	      (setq eif-matching-indent (eif-current-line-indent))
	      )
	    )
	;; Else no keyword was found. I think this is an error
	(setq eif-matching-indent 0)
	(message "No matching indent keyword was found")
	)
      keyword
      )
    )
  )

(defun eif-current-line-indent ()
  "Return the indentation of the line containing the point."
  (save-excursion
    (let ((line-end 0)
	  (indent   0)
	  )
      (end-of-line)
      (setq line-end (point))
      (beginning-of-line)
      (re-search-forward eif-white-space-regexp line-end t)
      (setq indent (current-column))
      )
    )
  )

(defun eif-line-contains-close-paren ()
  "This function returns t if the current line contains a close paren and
nil otherwise. If a close paren is found, the point is placed immediately
after the last close paren on the line. If no paren is found, the point is
placed at the beginning of the line."
  (let ((search-min 0))
    (beginning-of-line)
    (setq search-min (point))
    (end-of-line)
    (if (search-backward ")" search-min t)
	;; Then
	(progn
	  (forward-char 1)
	  t
	  )
      ;; Else
      (beginning-of-line)
      nil
      )
    )
  )

;;;; Not Currently Used
;;;(defun eif-quoted-string-on-line-p ()
;;;  "t if an Eiffel quoted string begins, ends, or is continued 
;;;   on current line."
;;;  (save-excursion
;;;    (beginning-of-line)
;;;    ;; Line must either start with optional whitespace immediately followed
;;;    ;; by a '%' or include a '\"'.  It must either end with a '%' character
;;;    ;; or must include a second '\"' character.
;;;    (looking-at "^\\([ \t]*%\\|[^\"\n]*\"\\)[^\"\n]*\\(%$\\|\"\\)")
;;;  )
;;;)

(defun eif-in-quoted-string-p (&optional non-strict-p)
  "t if point is in a quoted string. Optional argument NON-STRICT-P if true
causes the function to return true even if the point is located in leading
white space on a continuation line. Normally leading white space is not
considered part of the string."
  (let ((pt (point))
	(initial-regexp nil)
	(search-limit nil)
	front
	)
    (if non-strict-p
	;; Then
	(save-excursion
	  (setq initial-regexp "\\(%\n[ \t]*\\|\"\\)")
	  (beginning-of-line)
	  (backward-char 2)
	  (setq search-limit (point))
	  )
      ;; Else        
      (save-excursion 
	(setq initial-regexp "\\(^[ \t]*%\\|\"\\)")
	(beginning-of-line) 
	(setq search-limit (point))
	)
      )
    (save-excursion
      ;; Line must either start with optional whitespace immediately followed
      ;; by a '%' or include a '\"'.
      (if (re-search-backward initial-regexp search-limit t)
	  (progn (setq front (point))
		 (if non-strict-p
		     ;; Then
		     (forward-char 3)
		   ;; Else
		   (forward-char 1)
		   )
		 ;; Line must either end with a '%' character or must
		 ;; include a second '\"' character.
		 (and (re-search-forward 
		       "\\(%$\\|\"\\)"
		       (save-excursion (end-of-line) (point)) 
		       t
		       )
		      (>= (point) pt)
		      (<= front pt)
		      )
		 );; progn
	);; if
      );; save-excursion
    );; let
  );; e-in-quoted-string

(defvar eif-opening-regexp 
  "[^a-z_0-9]\\(check\\|deferred\\|do\\|once\\|from\\|if\\|inspect\\)[^a-z_0-9]"
  "Keywords that open eiffel nesting constructs."
  )
(defvar eif-closing-regexp "[^a-z_0-9]\\(end\\)[^a-z_0-9]"
  "Keywords that close eiffel nesting constructs."
  )
(defvar eif-do-regexp "[^a-z_0-9]\\(do\\)[^a-z_0-9]"
  "Keyword that opens eiffel routine body."
  )
(defvar eif-opening-or-closing-regexp 
  (concat "\\(" eif-opening-regexp "\\|" eif-closing-regexp "\\)") 
  "Keywords that open or close eiffel nesting constructs."
  )

;;;
;;; Code to allow indenting whole eiffel blocks
;;;

(defun eif-matching-line (&optional return-line-break direction)
  "Return the character position of the keyword matching the eiffel
keyword on the current line. (e.g. a line containing the keyword
'do' is matched by a line containing the keyword 'end' and a line
containing 'end' may be matched by a number of opening keywords.
If the optional parameter 'return-line-break' is not nil, the
character position returned is the beginning (or end) of the line
containing the matching keyword instead of the position of the
keyword itself. If the second optional parameter, direction, is 
non-null, the current line is not searched for a keyword. Instead, 
if the value of direction is 'forward, the function acts as if
an eif-opening-regexp is on the current line. If the value of direction
is 'backward, the function acts as if a eif-closing-regexp is on the 
current line. The effect of using the direction parameter is to 
locate either the opening or closing keyword of the syntactic 
construct containing the point."
  (let ((nesting-level 0)
	(matching-point nil)
	(search-end 0)
	(opening-keyword nil)
	(match-start nil)
	(match-end nil)
	(success   nil)
	(start-point nil)
	)
    (save-excursion
      (setq eif-matching-kw-for-end "") ;; public variable set by this function
      (setq start-point (point))
      (end-of-line)
      (setq search-end (1+ (point)))
      (beginning-of-line)
      ;; Set starting state: If direction was specified use it.
      ;; If direction is nil, search for a keyword on the current line
      ;; If the keyword in in eif-opening-regexp, set the search 
      ;; direction to 'forward, if the keyword on the current line is `end' 
      ;; set the search direction to 'backward.
      (cond ((eq direction 'forward)
	     (end-of-line) ;; So we wont see any keywords on the current line
	     (setq nesting-level 1)
	     )
	    ((eq direction 'backward)
	     (beginning-of-line) ;; So we wont see any keywords on the current line
	     (setq nesting-level -1)
	     )
	    ((and (re-search-forward eif-opening-regexp search-end t)
		  (not (or (eif-in-quoted-string-p)
			   (eif-in-comment-p)
			   )
		       )
		  )
	     (setq match-start (1+ (match-beginning 0)))
	     (goto-char match-start) 
	     (if (not (or (eif-in-quoted-string-p) (eif-in-comment-p)))
		 (setq nesting-level 1)
	       )
	     (setq opening-keyword 
		   (cons (buffer-substring match-start (- (match-end 0) 1))
			 opening-keyword
			 )
		   )
	     )
	    ((and (progn (beginning-of-line) t)
	          (re-search-forward eif-closing-regexp search-end t)
		  (not (or (eif-in-quoted-string-p)
			   (eif-in-comment-p)
			   )
		       )
		  )
	     (goto-char (1+ (match-beginning 0)))
	     (if (not (or (eif-in-quoted-string-p) (eif-in-comment-p)))
		 (setq nesting-level -1)
	       )
	     )
	    )
      ;; Perform the search
      (while (not (= nesting-level 0))
	(if (> nesting-level 0)
	    ;; Then search forward for the next keyword not in a comment
	    (while (and (re-search-forward eif-opening-or-closing-regexp nil t)
			(goto-char (setq match-start (match-beginning 0)))
			(setq match-end   (match-end 0))
			(setq success t)
			(or (eif-in-quoted-string-p) (eif-in-comment-p))
			)
	      (goto-char match-end)
	      (setq success nil)
	      )
	  ;; Else search backward for the next keyword not in a comment
	  (while (and (re-search-backward eif-opening-or-closing-regexp nil t)
		      (goto-char (setq match-start (match-beginning 0)))
		      (setq success t)
		      (or (eif-in-quoted-string-p) (eif-in-comment-p))
		      )
	    (setq success nil)
	    )
	  )
	(cond ((and (looking-at eif-opening-regexp) success)
	       ;; Found an opening keyword
	       (if (> nesting-level 0)
		   ;; Then
		   (if (looking-at eif-do-regexp)
		       ;; Then
		       (setq nesting-level -1)
		     ;; Else
		     (setq opening-keyword 
			   (cons (buffer-substring (1+ match-start)
						   (1- (match-end 0))
						   )
				 opening-keyword
				 )
			   )
		     (goto-char (1- (match-end 0)))
		     )
		 ;; Else
		 (if (= nesting-level -1)
		     ;; Then
		     (progn
		       (setq eif-matching-kw-for-end
			     (buffer-substring (1+ match-start)
					       (1- (match-end 0))
					       )
			     )
		       (if (looking-at "\n") (forward-char 1))
		       )
		   ;; Else
		   (if (looking-at eif-do-regexp)
		       ;; Then
		       (progn
			 (goto-char (eif-matching-line nil 'forward))
			 (setq nesting-level -1)
			 )
		     )
		   )
		 (setq opening-keyword (cdr opening-keyword))
		 (if return-line-break
		     (beginning-of-line)
		   )
		 )
	       (setq nesting-level (1+ nesting-level))
	      )
	      ((and (looking-at eif-closing-regexp) success)
	       ;; Found an opening keyword
	       (if (> nesting-level 0)
		   ;; Then
		   (progn
		     (setq opening-keyword (cdr opening-keyword))
		     (if return-line-break
			 (end-of-line)
		       )
		     (goto-char (1- (match-end 0)))
		     )
		 ;; Else
		 (setq opening-keyword 
		       (cons (buffer-substring (match-beginning 0) 
					       (1- (match-end 0))
					       )
			     opening-keyword
			     )
		       )
		 )
	       (setq nesting-level (1- nesting-level))
	       )
	      (t (message (concat "Could not find match"
				  (if (car opening-keyword) 
				      (concat " for: " (car opening-keyword))
				    )
				  )
			  )
		 (goto-char start-point)
		 (setq nesting-level 0)
		 )
	      );; cond
	);; while
      (setq matching-point (point))
      );; save-excursion
    (set-mark matching-point)
    );; let
  );; eif-matching-line 

;;; ENHANCEME: Make this function correctly indent more than just routine 
;;;            bodies and their sub-constructs. At the least it should 
;;;            handle whole routines also.
(defun eif-indent-construct ()
  "Indents an entire eiffel syntactic construct. It is assumed that
the point is within a nesting construct ('do', `once', 'check', 'if',
'from', or 'inspect'). The whole construct is indented up to the
matching end. If the point is not within such a construct, then
only that line is indented"
  (interactive)
  (let ((end-point 0))
    (save-excursion
      (end-of-line)
      (if (not (= (point) (point-max))) (forward-char 1))
      (goto-char (eif-matching-line t 'backward))
      (setq end-point (eif-matching-line t 'forward))
      (while (< (point) end-point)
	(eif-indent-line)
	(next-line 1)
	(beginning-of-line)
	)
      )
    )
  )

(defun eif-indent-region ()
  "Indents the lines in the current region"
  (interactive)
  (let ((start-point (region-beginning)) (end-point (region-end)))
    (save-excursion
      (goto-char start-point)
      (while (< (point) end-point)
	(eif-indent-line)
	(next-line 1)
	(beginning-of-line)
	)
      )
    )
  )

;;(defun eif-goto-matching-line (&optional direction)
;;  "Place the cursor on the line which closes(opens) the current
;;opening(closing) syntactic construct. For example if the point
;;is on `from', executing goto-matching-line places the point
;;on the matching `end' and vice-versa."
;;  (interactive)
;;  (if (not direction)
;;      (progn
;;	(cond ((save-excursion (beginning-of-line) (looking-at "[ 	]*end.*$"))
;;	       (goto-char (eif-matching-line nil 'backward))
;;	       )
;;	      ((looking-at "(")
;;	       (forward-sexp)
;;	       )
;;	      ((save-excursion (backward-char 1) (looking-at ")"))
;;	       (backward-sexp)
;;	       )
;;	      (t
;;	       (goto-char (eif-matching-line nil 'forward))
;;	       )
;;	      )
;;	)
;;    )
;;  )

(defun eif-forward-sexp ()
  "Place the cursor on the line which closes the current opening syntactic construct. For example if the point is  on `from', executing eif-forward-sexp places the point on the matching `end'. This also does matching of parens ala forward-sexp."
  (interactive)
  (cond ((looking-at "[[(]")
	 (forward-sexp)
	 )
	(t
	 (goto-char (eif-matching-line nil 'forward))
	 )
	)
  )

(defun eif-backward-sexp ()
  "Place the cursor on the line which opens  the current closing syntactic construct. For example if the point is  on the terminating `end' of an `if' statement, executing  eif-backward-sexp places the point on the opening `if'.  This also does matching of parens ala backward-sexp."
  (interactive)
  (cond ((save-excursion (backward-char 1) (looking-at "[])]"))
	 (backward-sexp)
	 )
	(t
	 (goto-char (eif-matching-line nil 'backward))
	 )
	)
  )

(defun eif-local-indent (amount)
  "Set the value of eif-indent-increment to amount and make the change local to this buffer."
  (interactive "NNumber of spaces for eif-indent-increment: ")
  (make-local-variable 'eif-indent-increment)
  (setq eif-indent-increment amount)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Utility Functions.                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eif-feature-quote ()
  "Put a `' around the current feature name"
  (interactive)
  (save-excursion
    (backward-sexp)
    (insert "`")
    (forward-sexp)
    (insert "'")
    )
  (if (looking-at "'")
      (forward-char 1))
  )

(defvar eiffel-mode-abbrev-table nil)
(define-abbrev-table 'eiffel-mode-abbrev-table ())

;;; ----------------------------------------------------------------------
;;; This next portion of the file is derived from "eiffel.el"
;;; Copyright (C) 1989, 1990 Free Software Foundation, Inc. and Bob Weiner
;;; Available for use and distribution under the same terms as GNU Emacs.
;;; ----------------------------------------------------------------------

(defvar eiffel-mode-map nil 
  "Keymap for Eiffel mode.")

(defvar eiffel-mode-syntax-table nil
  "Syntax table in use in Eiffel-mode buffers.")

(if eiffel-mode-syntax-table
    nil
  (let ((table (make-syntax-table))
	(i 0))
    (while (< i ?0)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (modify-syntax-entry ?  "    " table)
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?_  "_  " table)
    (modify-syntax-entry ?\t "    " table)
    (modify-syntax-entry ?\n ">   " table)
    (modify-syntax-entry ?\f ">   " table)
    (modify-syntax-entry ?\" "\"    " table)
    (modify-syntax-entry ?\\ "\\   " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "/" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?; "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?. "." table)
    (setq eiffel-mode-syntax-table table))
  )

  
(if eiffel-mode-map
    nil  
  (setq eiffel-mode-map (make-sparse-keymap))
  (define-key eiffel-mode-map "\t" 'eif-indent-line)
  (define-key eiffel-mode-map "\C-m" 'eif-newline)
  (define-key eiffel-mode-map "\177" 'backward-delete-char-untabify)
  (define-key eiffel-mode-map "\M-\C-q" 'eif-indent-construct)
  (define-key eiffel-mode-map "\M-'" 'eif-feature-quote)
  )

(defun eiffel-mode ()
  "Major mode for editing Eiffel programs."
  (interactive)
  (setq major-mode 'eiffel-mode)
  (setq mode-name "Eiffel")
  (use-local-map eiffel-mode-map)
  (run-hooks 'eiffel-mode-hook)
  (setq local-abbrev-table eiffel-mode-abbrev-table)
  (set-syntax-table eiffel-mode-syntax-table)
  )

(defun eif-in-comment-p ()
  "t if point is in a comment."
  (save-excursion
    (and (/= (point) (point-max)) (forward-char 1))
    (search-backward "--" (save-excursion (beginning-of-line) (point)) t)))


(defun eif-newline ()
  "Indent the current line, insert a newline, and place the point at the correctly
indented position on the next line."
  (interactive)
  (eif-indent-line)
  (insert "\n")
  (eif-indent-line)
  )

(defun eif-indent-line ()
  "Indent the current line as Eiffel code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (eif-calc-indent)))
  (skip-chars-forward " \t"))

;; ENHANCEME: Currently eif-beginning-of-feature only works for routines. 
;;            It should be made more general.
;;

(defun eif-beginning-of-feature (&optional arg)
  "Move backward to next feature beginning. With argument, do this that many 
times. Returns t unless search stops due to beginning of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (if (or (re-search-backward eif-multiline-routine-is-keyword-regexp 
			      nil t (or arg 1))
	  (re-search-backward eif-is-keyword-regexp 
			      nil 'move (or arg 1))	  
	  )
      (progn
	(backward-sexp 1)
	(if (looking-at "(")
	    (backward-word 1)
	  )
	(beginning-of-line)
	)
    nil
    )
  )

(defun eif-move-to-prev-non-blank ()
  "Moves point to previous line excluding blank lines. 
Returns t if successful, nil if not."
  (beginning-of-line)
  (re-search-backward "^[ \t]*[^ \t\n]" nil t))

(defun eif-in-paren-expression ()
  "Determine if we are inside of a parenthesized expression"
  (interactive)
  (let ((paren-count 0))
    (save-excursion
      (while (re-search-backward "[()]" nil t)
	(if (looking-at "(")
	    (setq paren-count (1+ paren-count))
	  (setq paren-count (1- paren-count))
	  )
	)
      )
    paren-count
    )
)

;;; ----------------------------------------------------------------------
;;; The portion of the file below this point is derived from "eif-mult-fmt.el"
;;; Copyright (C) 1985 Free Software Foundation, Inc.
;;; Copyright (C) 1990 Bob Weiner, Motorola Inc.
;;; Available for use and distribution under the same terms as GNU Emacs.
;;; ----------------------------------------------------------------------

(defun eif-indent-multi-line (&optional parse-start)
  "Return integer giving appropriate indentation for current Eiffel code
line between parentheses or double quotes, otherwise -1.  Optional
PARSE-START is buffer position at which to begin parsing, default is to begin
at the feature enclosing or preceding point."
  (let ((eif-opoint (point))
	(indent-point (progn (beginning-of-line) (point)))
	(eif-ind-val -1)
	(eif-in-str nil)
	(eif-paren-depth 0)
	(retry t)
	state
	;; setting this to a number inhibits calling hook
	last-sexp containing-sexp)
    (if parse-start
	(goto-char parse-start)
      (eif-beginning-of-feature))
    ;; Find outermost containing sexp
    (while (< (point) indent-point)
      (setq state (parse-partial-sexp (point) indent-point 0)))
    ;; Find innermost containing sexp
    (while (and retry
		state
		(> (setq eif-paren-depth (elt state 0)) 0))
      (setq retry nil)
      (setq last-sexp (elt state 2))
      (setq containing-sexp (elt state 1))
      ;; Position following last unclosed open.
      (goto-char (1+ containing-sexp))
      ;; Is there a complete sexp since then?
      (if (and last-sexp (> last-sexp (point)))
	  ;; Yes, but is there a containing sexp after that?
	  (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
	    (if (setq retry (car (cdr peek))) (setq state peek)))))
    (if retry
	nil
      ;; Innermost containing sexp found
      (goto-char (1+ containing-sexp))
      (if (not last-sexp)
	  ;; indent-point immediately follows open paren.
	  nil
	;; Find the start of first element of containing sexp.
	(parse-partial-sexp (point) last-sexp 0 t)
	(cond ((looking-at "\\s(")
	       ;; First element of containing sexp is a list.
	       ;; Indent under that list.
	       )
	      ((> (save-excursion (forward-line 1) (point))
		  last-sexp)
	       ;; This is the first line to start within the containing sexp.
	       (backward-prefix-chars))
	      (t
	       ;; Indent beneath first sexp on same line as last-sexp.
	       ;; Again, it's almost certainly a routine call.
	       (goto-char last-sexp)
	       (beginning-of-line)
	       (parse-partial-sexp (point) last-sexp 0 t)
	       (backward-prefix-chars))))
      (setq eif-ind-val (current-column))
      )
    ;; Point is at the point to indent under unless we are inside a string.
    (setq eif-in-str (elt state 3))
    (goto-char eif-opoint)
    (if (not eif-in-str)
	nil
      ;; Inside a string, indent 1 past string start
      (setq eif-paren-depth 1);; To account for being inside string
      (save-excursion
	(if (re-search-backward "\"" nil t)
	    (setq eif-ind-val (1+ (current-column)))
	  (goto-char indent-point)
	  (if (looking-at "^[ \t]*[^ \t\n]")
	      (eif-move-to-prev-non-blank))
	  (skip-chars-forward " \t")
	  (setq eif-ind-val (current-column)))))
    (if (> eif-paren-depth 0) eif-ind-val -1)
    ))

(provide 'eiffel3)
