;;; func-menu.el         --- Jump to a function within a buffer.
;;;
;;; David Hughes <djh@harston.cv.com>
;;; Last modified: 25th April 1994
;;; Version: 2.16
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
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
;;;
;;; Acknowledgements:
;;; =================
;;;
;;; Made fume-match-find-next-function-name iterative, not recursive, to avoid
;;; blowing out the emacs stack on big files with lots of prototypes.
;;; Joe Marshall <jrm@odi.com>
;;;
;;; Verilog support
;;; Matt Sale <mdsale@icdc.delcoelect.com>
;;;
;;; Minibuffer interface & Pascal support
;;; Espen Skoglund <espensk@stud.cs.uit.no>
;;;
;;; Python support
;;; Shuichi Koga <skoga@virginia.edu>
;;;
;;; Maple support
;;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
;;;
;;; Combined Tcl and C++ function finder
;;; Andy Piper <ajp@eng.cam.ac.uk>
;;;
;;; Perl Support
;;; Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Michael Lamoureux <lamour@engin.umich.edu>
;;;
;;; Suggested mouse interface
;;; Raymond L. Toy <toy@soho.crd.ge.com>
;;;
;;; Dired support
;;; Improved modula support
;;; Numerous code cleanups
;;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
;;;
;;; Makefile support
;;; Suggested multi-choice sublisting
;;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
;;;
;;; Suggestions for menubar entry
;;; Andy Piper <ajp@eng.cam.ac.uk>
;;;
;;; Ada support
;;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
;;;
;;; Scheme, BibTeX, Ehdm & PVS support
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;;
;;; Modula support
;;; Geoffrey Wyant <gwyant@cloyd.east.sun.com>
;;;
;;; SGML support; submenu indexing
;;; Thomas Plass <thomas.plass@mid-heidelberg.de>
;;;
;;; Extensions to fume-function-name-regexp-lisp
;;; Kari Heinola <kph@dpe.fi>
;;; Milo A. Chan <chan@jpmorgan.com>
;;; Cedric Beust <Cedric.Beust@sophia.inria.fr>
;;; Joachim Krumnow <krumnow@srsir02.ext.sap-ag.de>
;;;
;;; ksh support
;;; Philippe Bondono <bondono@vnet.ibm.com>
;;;
;;; FORTRAN support
;;; Paul Emsley <paule@chem.gla.ac.uk>
;;; Raymond L. Toy <toy@soho.crd.ge.com>
;;; Richard Cognot <cognot@elfgrc.co.uk>
;;; Greg Sjaardema <gdsjaar@sandia.gov>
;;;
;;; Latex support
;;; Wolfgang Mettbach <wolle@uni-paderborn.de>
;;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;;;
;;; Removal of cl dependencies
;;; Russell Ritchie <russell@gssec.bt.co.uk>
;;;
;;; C++ mode enhancemencements for func-menu
;;; Andy Piper      <ajp@eng.cam.ac.uk>
;;; Kevin R. Powell <powell@csl.ncsa.uiuc.edu>
;;; Mike Battaglia  <mbattagl@spd.dsccc.com>
;;; Oliver Schittko <schittko@fokus.gmd.de>
;;; Russell Ritchie <russell@gssec.bt.co.uk>
;;;
;;; Tcl mode additions for func-menu
;;; Andy Piper <ajp@eng.cam.ac.uk>
;;; Jean-Michel Augusto <augusto@eurecom.fr>
;;; Dr P.G. Sjoerdsma <pgs1002@esc.cam.ac.uk>
;;;
;;; Postscript mode additions for func-menu
;;; Leigh Klotz <klotz@adoc.xerox.com>
;;;
;;; Suggestions for popup menu positioning
;;; Marc Gemis <makke@wins.uia.ac.be>
;;;
;;; Original FSF package
;;; Ake Stenhoff <etxaksf@aom.ericsson.se>
;;;
;;; Description:
;;; ============
;;; Suppose you have a file with a lot of functions in it. Well, this package
;;; makes it easy to jump to any of those functions. The names of the functions
;;; in the current buffer are automatically put into a popup menu, you select
;;; one of the function-names and the point is moved to that very function. The
;;; mark is pushed on the mark-ring, so you can easily go back to where you
;;; were. Alternatively, you can use enter the name of the desired function via
;;; the minibuffer which offers completing read input.
;;;
;;; Modes supported:
;;; ================
;;; Bacis2, BibTex, C++, C, Dired, Ehdm, ELisp, FORTRAN, Ksh, Latex, Lelisp,
;;; Makefile, Maple, Modula2, Modula3, Pascal, Perl, Postscript, PVS, Python,
;;; SGML, Scheme, Tcl, Verilog
;;;
;;; Installation:
;;; =============
;;; (require 'func-menu)
;;; (define-key global-map 'f8 'function-menu)
;;; (add-hook 'find-file-hooks 'fume-add-menubar-entry)
;;; (define-key global-map "\C-cg" 'fume-prompt-function-goto)
;;; (define-key global-map '(shift button3) 'mouse-function-menu)

;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;    Customizable Variables    ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fume-auto-position-popup t
  "*Set this to nil if you don't want the popup menu to always appear in the
corner of the window, in which case it will track the mouse position instead.")

(defvar fume-menubar-menu-name "Functions"
  "*Set this to the string you want to appear in the menubar")
(make-variable-buffer-local 'fume-menubar-menu-name)

(defvar fume-menubar-menu-location "File"
  "*Set this nil if you want the menu to appear last on the menubar.
Otherwise set this to the menu you want \"Functions\" to appear in front of.")

(defvar fume-max-items 25
  "*Maximum number of elements in a function (sub)menu.")

(defvar fume-index-method 3
  "*Set this to the method number you want used.

Methods currently supported:
0 = if you want submenu names to be numbered
1 = if you want submenu range indicated by first character
2 = if you want submenu range indicated by first 10 characters
3 = if you want submenu range indicated by as many characters as needed")

(defvar fume-scanning-message "Scanning buffer... (%3d%%)"
  "*Set this to nil if you don't want any progress messages during the
scanning of the buffer.")

(defvar fume-rescan-buffer-hook nil
  "*Buffer local hook to call at the end of each buffer rescan")
(make-variable-buffer-local 'fume-rescan-buffer-hook)

(defvar fume-sort-function 'fume-sort-by-name
  "*The function to use for sorting the function menu.

Set this to nil if you don't want any sorting (faster).
The items in the menu are then presented in the order they were found
in the buffer.

The function should take two arguments and return T if the first
element should come before the second.  The arguments are cons cells;
(NAME . POSITION).  Look at 'fume-sort-by-name' for an example.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;     Buffer local variables     ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The latest list of function names in the buffer.
(defvar fume-funclist nil)
(make-variable-buffer-local 'fume-funclist)

(defvar fume-function-name-regexp nil
  "The keywords to show in a menu")
(make-variable-buffer-local 'fume-function-name-regexp)

(defvar fume-find-next-function-name-method nil
  "The function to use to find the next function name in the buffer")
(make-variable-buffer-local 'fume-find-next-function-name-method)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;    Mode specific regexp's and hooks    ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Every fume-function-name-regexp-<language> should uniquely identify a
;;; function for that certain language.

;;; LISP
;;;
;;; Cedric Beust <Cedric.Beust@sophia.inria.fr>
(defconst fume-function-name-regexp-lisp
  (concat
   "\\(^(defun+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   "\\|"
   "\\(^(defmacro+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   "\\|"
   "\\(^(de+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   "\\|"
   "\\(^(dmd+\\s-*[#:?A-Za-z0-9_+->]+\\s-*(\\)"
   )
  "Expression to get lisp function names")

;;; C
;;;
;;; Danny Bar-Dov <danny@acet02.amil.co.il>
(defconst fume-function-name-regexp-c
  (concat
   "^[a-zA-Z0-9]+\\s-?"                ; type specs; there can be no
   "\\([a-zA-Z0-9_*]+\\s-+\\)?"        ; more than 3 tokens, right?
   "\\([a-zA-Z0-9_*]+\\s-+\\)?"
   "\\([*&]+\\s-*\\)?"                 ; pointer
   "\\([a-zA-Z0-9_*]+\\)[ \t\n]*("     ; name
   )
  "Expression to get C function names")

;;; C++
;;;
;;; Andy Piper      <ajp@eng.cam.ac.uk>
;;; Kevin R. Powell <powell@csl.ncsa.uiuc.edu>
;;; Mike Battaglia  <mbattagl@spd.dsccc.com>
;;; Oliver Schittko <schittko@fokus.gmd.de>
(defconst fume-function-name-regexp-c++
  (cons
   (concat
    "^\\(template\\\s +<[^>]+>\\\s +\\)?"        ; template formals
    "\\([a-zA-Z0-9_*&<,>:]+\\s-+\\)?"            ; type specs; there can be no
    "\\([a-zA-Z0-9_*&<,>]+\\s-+\\)?"             ; more than 3 tokens, right?
    "\\([a-zA-Z0-9_*&<,>]+\\s-+\\)?"
    "\\(\\([a-zA-Z0-9_~:<,>*]\\|\\(\\s +::\\s +\\)\\)+\\)"
    "\\(o?perator\\s *.[^(]*\\)?\\s-*("          ; name
    ) 5)
  "Expression to get C++ function names")

;;; FORTRAN
;;;
;;; Paul Emsley <paule@chem.gla.ac.uk>
;;; Raymond L. Toy <toy@soho.crd.ge.com>
;;; Richard Cognot <cognot@elfgrc.co.uk>
;;; Greg Sjaardema <gdsjaar@sandia.gov>
(defconst fume-function-name-regexp-fortran
  (concat
   ;; >= six spaces
   "^      \\s-*"
   ;; type specs
   "+[a-zA-Z0-9*]*\\s-*"
   ;; continuation lines
   "\\(\n     [^ 0]\\s-*\\)*"
   ;; function or subroutine
   "\\(entry\\|ENTRY\\|function\\|FUNCTION\\|subroutine\\|SUBROUTINE\\)\\s-*"
   ;; continuation lines
   "\\(\n     [^ 0]\\s-*\\)*"
   )
  "Expression to get fortran function and subroutine names")

;;; Modula
(defconst fume-function-name-regexp-modula
  "^\\s-*PROCEDURE\\s-+[A-Za-z0-9_-]+"
  "Expression to get Modula function names")

;;; Bacis2
;;;
;;; Computervision in-house language
(defconst fume-function-name-regexp-bacis
  "module_define(!\\|define_constant(!\\|sys_sysdefine(!\\|<<dbgid +\\s-*"
  "Expression to get Bacis2 function names")

;;; Maple
;;;
;;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
(defvar fume-function-name-regexp-maple
  "^[ \t]*[a-zA-Z0-9_]+[ \t]*:=[ \t]*proc[ \t]*("
  "Expression to get maple function/procedure names")

;;; Tcl
;;;
;;; Andy Piper <ajp@eng.cam.ac.uk>
;;; Jean-Michel Augusto <augusto@eureecom.fr>
;;; Dr P.G. Sjoerdsma <pgs1002@esc.cam.ac.uk>
(defconst fume-function-name-regexp-tcl
  (cons "^\\s *proc\\s +\\(\\S-+\\)\\s *{" 1)
  "Expression to get Tcl function Names")

;;; Perl
;;;
;;; Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Michael Lamoureux <lamour@engin.umich.edu>
(defconst fume-function-name-regexp-perl "^sub[ \t]+\\([A-Za-z0-9_]+\\)"
  "Expression to get Perl function Names")

;;; Python support
;;; Shuichi Koga <skoga@virginia.edu>
;;;
(defconst fume-function-name-regexp-python
  "^\\s-*\\(class\\|def\\)+\\s-*\\([A-Za-z0-9_]+\\)\\s-*[(:]"
  "Expression to get Python class and function names")

;;; Postscript
;;;
;;; Leigh L. Klotz <klotz@adoc.xerox.com>
(defconst fume-function-name-regexp-postscript
  "^/[^][ \t{}<>]*"
  "Expression to get postscript function names")

;;; Ehdm
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defconst fume-function-name-regexp-ehdm
  (concat
   "[A-Za-z0-9_]*:[ ]*"
   "\\([Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\|"
   "[Ll][Ee][Mm][Mm][Aa]\\|"
   "[Aa][Xx][Ii][Oo][Mm]\\|"
   "[Pp][Rr][Oo][Vv][Ee]\\|"
   "[Tt][Hh][Ee][Oo][Rr][Ee][Mm]"
   "\\)"
   )
  "*Expression to get Ehdm function, theorems, axioms, lemmas, and proofs.")

;;; PVS
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defconst fume-function-name-regexp-pvs
  (concat
   "\\([A-Za-z0-9_]*:[ ]*"
   "\\([Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\|"
   "[Ll][Ee][Mm][Mm][Aa]\\|"
   "[Aa][Xx][Ii][Oo][Mm]\\|"
   "[Tt][Hh][Ee][Oo][Rr][Ee][Mm]\\|"
   "[Ff][Or][Rr][Mm][Uu][La][Aa]"
   "\\|"
   "\\[.*\\]"
   "\\)\\)\\|"
   "[A-Za-z0-9_]*(.*)[ ]*:"
   )
  "*Expression to get PVS functions, theorems, axioms, lemmas")

;;; Tex, LaTex
;;;
;;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
(defvar fume-tex-chapter 0)
(make-variable-buffer-local 'fume-tex-chapter)
(defvar fume-tex-section 0)
(make-variable-buffer-local 'fume-tex-section)
(defvar fume-tex-subsection 0)
(make-variable-buffer-local 'fume-tex-subsection)
(defvar fume-tex-subsubsection 0)
(make-variable-buffer-local 'fume-tex-subsubsection)

(defun fume-tex-rescan-buffer-hook ()
  (setq fume-tex-chapter 0
        fume-tex-section 0
        fume-tex-subsection 0
        fume-tex-subsubsection 0))

(defun fume-tweak-tex-mode ()
  (make-variable-buffer-local 'fume-sort-function)
  (setq fume-sort-function nil)
  (add-hook 'fume-rescan-buffer-hook 'fume-tex-rescan-buffer-hook))

(add-hook 'tex-mode-hook 'fume-tweak-tex-mode)
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(add-hook 'TeX-mode-hook 'fume-tweak-tex-mode)
;;; Wolfgang Mettbach <wolle@uni-paderborn.de>
(add-hook 'latex-mode-hook 'fume-tweak-tex-mode)
(add-hook 'LaTeX-mode-hook 'fume-tweak-tex-mode)

;;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
(defconst fume-section-name-regexp-latex
  (concat
   "^\\s-*\\\\\\("
   "\\(sub\\)*section\\|chapter\\)"
   "\\*?\\(\\[[^]]*\\]\\)?{\\([^}]*\\)}"
   )
  "Expression to get latex section names")

;;; ksh
;;;
;;; Philippe Bondono <bondono@vnet.ibm.com>
(defconst fume-function-name-regexp-ksh
  (concat
   "\\(^\\s-*function\\s-+[A-Za-z_][A-Za-z_0-9]*\\)"
   "\\|"
   "\\(^\\s-*[A-Za-z_][A-Za-z_0-9]*\\s-*()\\)")
  "Expression to get ksh function names")

;;; Scheme
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defconst fume-function-name-regexp-scheme
  "^(define [ ]*"
  "Expression to get Scheme function names")

;;; BibTeX
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defconst fume-function-name-regexp-bibtex
  "^@[A-Za-z]*[({]\\([A-Za-z0-9:;&-]*\\),"
  "Expression to get bibtex citation headers.")

;;; SGML
;;;
;;; Thomas Plass <thomas.plass@mid-heidelberg.de>
(defconst fume-function-name-regexp-sgml
  "<!\\(element\\|entity\\)[ \t\n]+%?[ \t\n]*\\([A-Za-z][-A-Za-z.0-9]*\\)"
  "Expression to find declaration of SGML element or entity")

;;; Ada
;;;
;;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
(defconst fume-function-name-regexp-ada
  (cons "^[ \t]*\\(procedure\\|PROCEDURE\\|function\\|FUNCTION\\)[ \n\t]+\\([a-zA-Z0-9_]+\\|\"[^\"]\"\\)" 2)
  "Expression to find declaration of Ada function")

;;; ignore prototypes, 'renames', 'is new' to eliminate clutter
;;;
(defconst fume-function-name-regexp-ada-ignore
  "[ \n\t]*\\(([^()]+)[ \n\t]*\\)?\\(return[ \t\n]+[^ \t\n;]+[ \n\t]*\\)?\\(;\\|is[ \n\t]+new\\|renames\\)"
  "ignore if ada function name matches this string")

;;; Makefiles
;;;
;;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
(defconst fume-function-name-regexp-make
  "^\\(\\(\\$\\s(\\)?\\(\\w\\|\\.\\)+\\(:sh\\)?\\(\\s)\\)?\\)\\s *\\(::?\\|\\+?=\\)"
  "Expression to get makefile target names")
(add-hook 'makefile-mode-hook 'fume-add-menubar-entry)

;;; Directory Listings
;;;
;;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
;;; regexp stolen from font-lock-mode
(defconst fume-function-name-regexp-dired
  "^. +d.*\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) +[0-9]+ +[0-9:]+ \\(.*\\)$"
  "Expression to get directory names")

;;; Pascal
;;;
;;; Espen Skoglund <espensk@stud.cs.uit.no>
(defconst fume-function-name-regexp-pascal
  "^\\(function\\|procedure\\)[ \t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\)"
  "Expression to get function/procedure names in pascal.")


;;; Verilog support
;;;
;;; Matt Sale <mdsale@icdc.delcoelect.com>
(defconst fume-function-name-regexp-verilog
  "^\\(task\\|function\\|module\\|primitive\\)[ \t]+\\([A-Za-z0-9_+-]*\\)[ \t]*(?"
  "Expression to get verilog module names")


;;; This where the mode specific regexp's are hooked in
;;;
(defconst fume-function-name-regexp-alist
  '(;; Lisp
    (lisp-mode . fume-function-name-regexp-lisp)
    (emacs-lisp-mode . fume-function-name-regexp-lisp)
    (lisp-interaction-mode . fume-function-name-regexp-lisp)

    ;; C
    (c-mode . fume-function-name-regexp-c)
    (elec-c-mode . fume-function-name-regexp-c)
    ;; Bob Weiner <weiner@pts.mot.com>
    (c++-c-mode . fume-function-name-regexp-c)

    ;; C++
    (c++-mode . fume-function-name-regexp-c++)

    ;; FORTRAN
    ;; Paul Emsley <paule@chem.gla.ac.uk>
    (fortran-mode . fume-function-name-regexp-fortran)

    ;; Modula
    (modula-2-mode . fume-function-name-regexp-modula)
    (modula-3-mode . fume-function-name-regexp-modula)

    ;; Bacis2
    (bacis-mode . fume-function-name-regexp-bacis)

    ;; Maple
    ;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
    (maple-mode . fume-function-name-regexp-maple)

    ;; Perl
    (perl-mode . fume-function-name-regexp-perl)

    ;; Python
    ;; Shuichi Koga <skoga@virginia.edu>
    (alice-mode  . fume-function-name-regexp-python)
    (python-mode . fume-function-name-regexp-python)

    ;; Postscript
    ;; Leigh L. Klotz <klotz@adoc.xerox.com>
    (postscript-mode . fume-function-name-regexp-postscript)

    ;; Tcl
    ;; Jean-Michel Augusto <augusto@eurecom.fr>
    (tcl-mode . fume-function-name-regexp-tcl)

    ;; ksh
    ;; Philippe Bondono <bondono@vnet.ibm.com>
    (ksh-mode . fume-function-name-regexp-ksh)

    ;; LaTeX
    ;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
    (latex-mode . fume-section-name-regexp-latex)
    (LaTeX-mode . fume-section-name-regexp-latex)

    ;; Scheme
    ;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
    (scheme-mode . fume-function-name-regexp-scheme)

    ;; BibTeX
    ;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
    (bibtex-mode . fume-function-name-regexp-bibtex)

    ;; Ehdm & PVS
    ;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
    (ehdm-mode . fume-function-name-regexp-ehdm)
    (pvs-mode  . fume-function-name-regexp-pvs)

    ;; SGML
    ;; Thomas Plass <thomas.plass@mid-heidelberg.de>
    (sgml-mode . fume-function-name-regexp-sgml)

    ;; Ada
    ;; Mike Polo <mikep@cfsmo.honeywell.com>, <mikep@polo.mn.org>
    (ada-mode . fume-function-name-regexp-ada)

    ;; Makefiles
    ;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
    (makefile-mode . fume-function-name-regexp-make)

    ;; Dired
    ;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
    (dired-mode . fume-function-name-regexp-dired)

    ;; Pascal
    ;; Espen Skoglund <espensk@stud.cs.uit.no>
    (pascal-mode . fume-function-name-regexp-pascal)

    ;; Verilog support
    ;; Matt Sale <mdsale@icdc.delcoelect.com>
    (verilog-mode . fume-function-name-regexp-verilog)
    )

  "The connection between a mode and the regexp that matches function names.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;    Mode specific finding functions    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Default routine : Note, most modes will need a specialised routine
;;;
(defun fume-find-next-function-name (buffer)
  "Searches for the next function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((char (progn
                    (backward-up-list 1)
                    (save-excursion
                      (goto-char (scan-sexps (point) 1))
                      (following-char)))))
        ;; Skip this function name if it is a prototype declaration.
        (if (eq char ?\;)
            (fume-find-next-function-name buffer)
          ;; Get the function name and position
          (let (beg)
            (forward-sexp -1)
            (setq beg (point))
            (forward-sexp)
            (cons (buffer-substring beg (point)) beg))))))

;;; General purpose sexp find function
;;;
(defun fume-find-next-sexp (buffer)
  "Searches for the next sexp type function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (save-excursion (forward-sexp -1) (point))))
        (cons (buffer-substring beg (point)) beg))))

;;; Specialised routine to get the next ehdm entity in the buffer.
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defun fume-find-next-ehdm-entity (buffer)
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to get the next PVS entity in the buffer.
;;;
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defun fume-find-next-pvs-entity (buffer)
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (goto-char (1- end))
        (if (looking-at ":")
            (setq end (1- end)))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to get the next C function name in the buffer.
;;;
(defun fume-find-next-c-function-name (buffer)
  "Searches for the next C function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((char (progn
                    (backward-up-list 1)
                    (save-excursion
                      (goto-char (scan-sexps (point) 1))
                      (following-char)))))
        ;; Skip this function name if it is a prototype declaration.
        (if (eq char ?\;)
            (fume-find-next-function-name buffer)
          (let (beg
                name)
            ;; Get the function name and position
            (forward-sexp -1)
            (setq beg (point))
            (forward-sexp)
            (setq name (buffer-substring beg (point)))
            ;; ghastly crock for DEFUN declarations
            (cond ((string-match "^DEFUN\\s-*" name)
                   (forward-word 1)
                   (forward-word -1)
                   (setq beg (point))
                   (cond ((re-search-forward "\"," nil t)
                          (re-search-backward "\"," nil t)
                          (setq name
                                (format "%s %s"
                                        name
                                        (buffer-substring beg (point))))))))
            ;; kludge to avoid 'void' in menu
            (if (string-match "^void\\s-*" name)
                (fume-find-next-function-name buffer)
              (cons name beg)))))))

;;; <jrm@odi.com>
;;; <ajp@eng.cam.ac.uk>
;;; <schittko@fokus.gmd.de>
(defun fume-match-find-next-function-name (buffer)
  "General next function name in BUFFER finder using match.
The regexp is assumed to be a two item list the car of which is the regexp to
use, and the cdr of which is the match position of the function name."
  (set-buffer buffer)
  (let ((result nil)
        (continue t))
    (while continue
      ;; Search for the function
      (if (re-search-forward (car fume-function-name-regexp) nil t)
          (let ((char (progn
                        (backward-up-list 1)
                        (save-excursion
                          (goto-char (scan-sexps (point) 1))
                          (following-char)))))
            ;; Skip this function name if it is a prototype declaration.
            (if (eq char ?\;)
                nil
              (setq result
                    ;; Get the function name and position including scope
                    (cons (buffer-substring
                           (match-beginning (cdr fume-function-name-regexp))
                           (point))
                          (match-beginning (cdr fume-function-name-regexp)))
                    continue nil)))
        (setq continue nil)))
    result))

;;; Specialised routine to find the next Perl function
;;;
(defun fume-find-next-perl-function-name (buffer)
  "Searches for the next Perl function in BUFFER."
  (fume-find-next-sexp buffer))

;;; Specialised routine to find the next Python function
;;; Shuichi Koga <skoga@virginia.edu>
;;;
(defun fume-find-next-python-function-name (buffer)
  "Searches for the next python function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (save-excursion
        (let* ((retpnt (match-beginning 2))
               (retname (buffer-substring retpnt (match-end 2))))
          (goto-char (match-beginning 0))
          (cond ((looking-at "\\s-+def")
                 (re-search-backward
                  "^class\\s-*\\([A-Za-z0-9_]+\\)\\s-*[(:]" nil t)
                 (setq retname
                       (concat
                        (buffer-substring (match-beginning 1) (match-end 1))
                        "."
                        retname))))
          (cons retname retpnt)))))

;;; Specialised routine to find the next Modula function or subroutine.
;;;
(defun fume-find-next-modula-function-name (buffer)
  "Searches for the next modula function in BUFFER."
  (fume-find-next-sexp buffer))

;;; Specialised routine to find the next directory.
;;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
(defun fume-find-next-directory-name (buffer)
  "Searches for the next directory in dired BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to find the next FORTRAN function or subroutine
;;;
(defun fume-find-next-fortran-function-name (buffer)
  "Searches for the next fortran function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((pos (point))
            ;; name may have "_" but must start with a letter
            (name-regexp "\\s-+[a-zA-Z]+[_a-zA-Z0-9*]*")
            (eol (save-excursion (end-of-line 1) (point))))
        (skip-chars-backward " \t")
        (if (re-search-forward name-regexp eol t)
            ;; name is ok; so return it
            (cons (buffer-substring pos (point)) pos)
          ;; rubbish found; skip to next function
          (fume-find-next-fortran-function-name buffer)))))

;;; Specialised routine to get the next postscript function name in the buffer
;;; Leigh L. Klotz <klotz@adoc.xerox.com>
;;;
(defun fume-find-next-postscript-function-name (buffer)
  "Searches for the next postscript function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (beginning-of-line 1) (point))))
        (forward-sexp)
        ;; keep including sexps as long as they
        ;; start with / or [.
        (if (looking-at "\\s-+\\(/\\|\\[\\)")
            (forward-sexp))
        (cons (buffer-substring beg (point)) beg))))

;;; Specialised routine to get the next bacis2 procedure name in the buffer
;;;
(defun fume-find-next-bacis-function-name (buffer)
  "Searches for the next Bacis2 function in BUFFER"
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((pos (point))
            (name (condition-case ()
                      (funcall
                       (symbol-function (intern "focus-get-function-name")))
                    (error nil))))
        (if (null name)
            (fume-find-next-bacis-function-name buffer)
          ;; jump past possible function dbgid
          (re-search-forward
           (format "<<dbgid +\\s-*%s%s" name "\\s-*>>") nil t)
          (cons name pos)))))

;;; Specialized routine to get the next Maple function name in the buffer
;;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
;;;
(defun fume-find-next-maple-function-name (buffer)
  "Searches for the next maple function in BUFFER"
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (backward-up-list 1) (forward-sexp -2) (point))))
        (forward-sexp)
        (cons (buffer-substring beg (point)) beg))))

;;; Specialised routine to get the next latex section name in the buffer
;;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
;;;
(defun fume-find-next-latex-section-name (buffer)
  "Searches for the next latex section in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let* ((secname (buffer-substring (match-beginning 1) (match-end 1)))
             (beg (match-beginning 4))
             (name (buffer-substring beg (match-end 4))))
        (cond ((string= secname "chapter")
               (setq fume-tex-chapter (1+ fume-tex-chapter)
                     fume-tex-section 0
                     fume-tex-subsection 0
                     fume-tex-subsubsection 0
                     name (concat fume-tex-chapter " " (upcase name))))
              ((string= secname "section")
               (setq fume-tex-section (1+ fume-tex-section)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat fume-tex-chapter ".") "")
                           fume-tex-section " " name)
                     fume-tex-subsection 0
                     fume-tex-subsubsection 0))
              ((string= secname "subsection")
               (setq fume-tex-subsection (1+ fume-tex-subsection)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat fume-tex-chapter ".") "")
                           fume-tex-section "."
                           fume-tex-subsection " " name)
                     fume-tex-subsubsection 0))
              ((string= secname "subsubsection")
               (setq fume-tex-subsubsection (1+ fume-tex-subsubsection)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat fume-tex-chapter ".") "")
                           fume-tex-section "."
                           fume-tex-subsection "."
                           fume-tex-subsubsection " " name)))
              ((string= secname "subsubsection")
               (setq name (concat "   " name))))
        (cons name beg))))

;;; Specialised routine to get the next ksh function in the buffer
;;; Philippe Bondono <bondono@vnet.ibm.com>
;;;
(defun fume-find-next-ksh-function-name (buffer)
  "Searches for the ksh type function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let (name
            (beg (match-beginning 0)))
        (cond ((re-search-backward "\\(^\\|\\s-\\)function\\s-" beg t)
               (re-search-forward
                "\\(function\\s-+\\)\\([A-Za-z_][A-Za-z_0-9]*\\)" nil t)
               (setq beg (match-beginning 2)
                     name (buffer-substring beg (match-end 2))))
              (t
               (re-search-backward
                "\\(^\\|\\s-\\)\\([A-Za-z_][A-Za-z_0-9]*\\)" beg t)
               (setq beg (match-beginning 2)
                     name (buffer-substring beg (match-end 2)))))
        (if (null name)
            (fume-find-next-ksh-function-name buffer)
          (end-of-line)
          (cons name beg)))))

;;; Specialised routine to get the next Scheme function in the buffer
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;;
(defun fume-find-next-scheme-function (buffer)
  "Searches for the next Scheme function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (if (looking-at "(") (forward-char 1)) (point)))
            (end (save-excursion (forward-sexp) (point))))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to get the next BibTeX citation in the buffer
;;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;;
(defun fume-find-next-bibtex-citation (buffer)
  "Searches for the next BibTeX citation in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (cons (buffer-substring beg end) beg))))

;;; Specialised routine to get the next SGML declaration in the buffer
;;; Thomas Plass <thomas.plass@mid-heidelberg.de>
;;;
(defun fume-find-next-sgml-element-name (buffer)
  "Searches for the next SGML declaration in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((type (buffer-substring (match-beginning 1) (match-end 1)))
            (beg (match-beginning 2))
            (name (buffer-substring (match-beginning 2) (match-end 2))))
        (if (string= (downcase type) "element")
            (setq name (format "%-17s%3s" name "EL"))
          (setq name (format "%-17s%3s" name "ENT")))
        (cons name beg))))

;;; Specialised routine to get the next ada function in the buffer
;;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
;;;
(defun fume-find-next-ada-function-name (buffer)
  "Searches for the next ada function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward (car fume-function-name-regexp-ada) nil t)
      (let ((beg (match-beginning (cdr fume-function-name-regexp-ada)))
            (end (match-end (cdr fume-function-name-regexp-ada))))

        (if (looking-at fume-function-name-regexp-ada-ignore)
            (fume-find-next-ada-function-name buffer)
          (cons (buffer-substring beg end) beg)))))

;; Makefiles
(defun fume-find-next-function-name-make (buffer)
  "Searches for the next make item in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (cons (buffer-substring beg end) beg))))

;;; Find next pascal function in the buffer
;;; Espen Skoglund <espensk@stud.cs.uit.no>
(defun fume-find-next-pascal-function-name (buffer)
  "Searches for the next pascal procedure in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;;; Verilog support
;;; Matt Sale <mdsale@icdc.delcoelect.com>
(defun fume-find-next-verilog-function-name (buffer)
  "Searches for the next verilog module in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;;; This is where you can hook in other languages which may need a different
;;; method to scan for function names. Otherwise, the default defun used is
;;; fume-find-next-function-name which is suitable sexp-based for languages
;;; such as C, C++ and elisp.
;;;
(defconst fume-find-function-name-method-alist
  '((ada-mode        . fume-find-next-ada-function-name)
    (alice-mode      . fume-find-next-python-function-name)
    (bacis-mode      . fume-find-next-bacis-function-name)
    (bibtex-mode     . fume-find-next-bibtex-citation)
    (c++-mode        . fume-match-find-next-function-name)
    (c-mode          . fume-find-next-c-function-name)
    (dired-mode      . fume-find-next-directory-name)
    (ehdm-mode       . fume-find-next-ehdm-entity)
    (pvs-mode        . fume-find-next-pvs-entity)
    (fortran-mode    . fume-find-next-fortran-function-name)
    (ksh-mode        . fume-find-next-ksh-function-name)
    (latex-mode      . fume-find-next-latex-section-name)
    (LaTeX-mode      . fume-find-next-latex-section-name)
    (makefile-mode   . fume-find-next-function-name-make)
    (maple-mode      . fume-find-next-maple-function-name)
    (modula-2-mode   . fume-find-next-modula-function-name)
    (modula-3-mode   . fume-find-next-modula-function-name)
    (pascal-mode     . fume-find-next-pascal-function-name)
    (perl-mode       . fume-find-next-perl-function-name)
    (postscript-mode . fume-find-next-postscript-function-name)
    (python-mode     . fume-find-next-python-function-name)
    (scheme-mode     . fume-find-next-scheme-function)
    (sgml-mode       . fume-find-next-sgml-element-name)
    (tcl-mode        . fume-match-find-next-function-name)
    (verilog-mode    . fume-find-next-verilog-function-name)
    )

  "The connection between a mode and the defun that finds function names.
If no connection is in this alist for a given mode, a default method is used.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;   General utility functions    ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sets 'fume-function-name-regexp' to something appropriate for the current
;;; mode for this buffer.
;;;
(defun fume-set-defaults ()
  "Returns nil if unsuccessful in setting up buffer-local defaults.
Otherwise returns fume-function-name-regexp"
  (setq fume-function-name-regexp
        (symbol-value
         (cdr-safe (assoc major-mode fume-function-name-regexp-alist))))
  (if fume-function-name-regexp
      (setq fume-find-next-function-name-method
            (or (cdr-safe (assoc major-mode
                                 fume-find-function-name-method-alist))
                'fume-find-next-function-name)))
  fume-function-name-regexp)


;;; Sort function to sort items depending on their function-name
;;; An item looks like (NAME . POSITION).
;;;
(defun fume-sort-by-name (item1 item2)
  (or (string-lessp (car item1) (car item2))
      (string-equal (car item1) (car item2))))


;;; Support function to calculate relative position in buffer
;;;
(defun fume-relative-position ()
  (let ((pos (point))
        (total (buffer-size)))
    (if (> total 50000)
        ;; Avoid overflow from multiplying by 100!
        (/ (1- pos) (max (/ total 100) 1))
      (/ (* 100 (1- pos))
         (max total 1)))))

;;; Split LIST into sublists of max length N.
;;; Example (fume-split '(1 2 3 4 5 6 7 8) 3)-> '((1 2 3) (4 5 6) (7 8))
;;;
(defun fume-split (list n)
  (let ((remain list)
        (result '())
        (sublist '())
        (i 0))
    (while remain
      (setq sublist (cons (car remain) sublist))
      (setq remain (cdr remain))
      (setq i (1+ i))
      (and (= i n)
           ;; We have finished a sublist
           (progn (setq result (cons (nreverse sublist) result))
                  (setq i 0)
                  (setq sublist '()))))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (and sublist
         (setq result (cons (nreverse sublist) result)))
    (nreverse result)))

;;; Routines to create indexes for submenus
;;;

;;; Method 0
(defun fume-index-sublist-method-0 (sublist count)
  (concat "Function sublist #" count))

;;; Method 1
;;; Thomas Plass <thomas.plass@mid-heidelberg.de>
(defun fume-index-sublist-method-1 (sublist &rest count)
  (interactive)
  (let ((s (substring (car (car sublist)) 0 1))
        (e (substring (car (nth (1- (length sublist)) sublist)) 0 1)))
    (format "Function sublist (%s%s)"
            s (if (string-equal s e) "<>" (format "<>-%s<>" e)))))

;;; Method 2
;;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
(defun fume-index-sublist-method-2 (sublist &rest count)
  (let ((s (substring (car (car sublist))
                      0 (min (length (car (car sublist))) 10)))
        (e (substring (car (nth (1- (length sublist)) sublist))
                      0 (min (length (car (nth (1- (length sublist)) sublist)))
                             10))))
    (format "%s%s" s (if (string-equal s e) "<>" (format "<> ... %s<>" e)))))

;;; Method 3
;;;
(defun fume-index-sublist-method-3-1 (sublist ix limit)
  (let ((s1 (substring (car (car sublist)) 0 (min limit ix)))
        (s2 (substring
             (car (nth (1- (length sublist)) sublist))
             0 (min (length (car (nth (1- (length sublist)) sublist))) ix))))
    (cons s1 s2)))

(defun fume-index-sublist-method-3 (sublist &rest count)
  (let* ((cmplength 10)
         (limit (length (car (car sublist))))
         (result (fume-index-sublist-method-3-1 sublist cmplength limit))
         (str1 (car result))
         (str2 (cdr result)))
    (while (and (string-equal str1 str2) (< cmplength limit))
      (setq cmplength (1+ cmplength)
            result (fume-index-sublist-method-3-1 sublist cmplength limit)
            str1 (car result)
            str2 (cdr result)))
    (cond ((not (string-equal str1 str2))
           (format "%s<> ... %s<>" str1 str2))
          ((< cmplength limit)
           (format "%s<>" str1))
          (t
           (format "%s ..." str1)))))

;;; Routine to rescan the buffer
;;;
(defun fume-rescan-buffer (&optional popmenu)
  "Rescans the buffer for function names.
If optional arg POPMENU is non-nil, brings up the function-menu."
  (interactive)
  (let ((funcfinder (symbol-value 'fume-find-next-function-name-method))
        (funcname)
        (funclist '())
        (buffer-to-scan (current-buffer)))
    (save-excursion
      (goto-char (point-min))
      (and fume-scanning-message
           (message fume-scanning-message 0))
      (while (setq funcname
                   (condition-case ()
                       (funcall funcfinder buffer-to-scan)
                     (error
                      ;; test for more possible fns after this error trap
                      (save-excursion
                        (re-search-forward fume-function-name-regexp nil t)))))
        (if (listp funcname)
            (setq funclist (cons funcname funclist)))
        (if fume-scanning-message
            (message fume-scanning-message (fume-relative-position))))
      (if fume-scanning-message
          (message "%s done" (format fume-scanning-message 100)))
      (if fume-sort-function
          (setq fume-funclist
                (sort funclist fume-sort-function))
        (setq fume-funclist (nreverse funclist)))
      (run-hooks 'fume-rescan-buffer-hook)))
  (if popmenu
      (function-menu)
    (fume-update-menubar-entry)))

;;; Routine to position cursor
;;;
(defun fume-goto-function (fn pos)
  "Thoughtfully position cursor at location of function probed on popup menu"
  (let ((orig-pos (point))
        (case-fold-search nil)
        (match-fn (cond ((string-match "DEFUN " fn) ; DEFUN decl in Emacs C
                         (substring fn (match-end 0)))
                        ((string-match "^ *" fn)    ; strip leading spaces
                         (substring fn (match-end 0)))
                        (t
                         fn))))

    (save-excursion
      (goto-char pos)
      (or (looking-at match-fn)
          (let ((fume-scanning-message nil))
            (fume-rescan-buffer)
            (setq pos (cdr-safe (assoc fn fume-funclist))))))

    (if pos
        (or (prog1
                (pos-visible-in-window-p pos)
              (goto-char pos)
              ;; possibly set mark
              (or (= orig-pos (point))
                  (push-mark orig-pos (null fume-scanning-message))))
            (set-window-start (selected-window)
                              (save-excursion
                                (beginning-of-line -2) (point))))
      (ding)
      (message "%s not found" fn)
      (function-menu))))

;;; Routines to add/remove/update function menu from menubar
;;;
(defun fume-add-menubar-entry ()
  (interactive)
  (and (string-match "Lucid" emacs-version)
       current-menubar
       (save-window-excursion (function-menu t))))

(defun fume-remove-menubar-entry ()
  (interactive)
  (delete-menu-item (list fume-menubar-menu-name))
  ;; force update of the menubar
  (set-buffer-modified-p (buffer-modified-p)))

(defun fume-update-menubar-entry ()
  "Returns t if menubar was updated. Nil otherwise"
  (interactive)
  (if (not (assoc fume-menubar-menu-name current-menubar))
      nil
    (fume-add-menubar-entry)
    t))

(defun fume-event-window (event)
  "Similar to event-window but more robust!"
  (or (event-window event)
      (locate-window-from-coordinates
       (selected-screen) (list (event-x event) (event-y event)))
      (locate-window-from-coordinates
       (selected-screen) (list (event-x event) (1- (event-y event))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;    The main entry points for this package    ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Interface to function-menu for mouse bindings only
;;;
(defun mouse-function-menu (event)
  "Wrapper for mouse button bindings for function-menu"
  (interactive "e")
  (let ((currwin (selected-window)))
    (condition-case err
        (progn
          (select-window (fume-event-window event))
          (let ((fume-auto-position-popup nil)) (function-menu)))
      (error (select-window currwin) (eval err)))))

;;; Interface for Key bindings
;;;
(defun function-menu (&optional use-menubar)
  "Pop up a menu of functions for selection with the mouse.

With a prefix arg adds the menu to the current menubar.
Jumps to the selected function.  A mark is set at the old position,
so you can easily go back with C-u \\[set-mark-command]."
  (interactive "P")

  (catch 'no-functions
    (or (fume-set-defaults)
        (if use-menubar
            (throw 'no-functions t)
          (error "The mode \"%s\" is not implemented in 'function-menu'."
                 mode-name)))

    ;; Create a list for this buffer only if there isn't any.
    (or fume-funclist
        (fume-rescan-buffer))
    (or fume-funclist
        (if use-menubar
            (throw 'no-functions t)
          (error "No functions found in this buffer.")))

    ;; Create the menu
    (let* ((count 0)
           (index-method
            (intern (format "fume-index-sublist-method-%d" fume-index-method)))
           (function-menu
            (mapcar
             (function
              (lambda (sublist)
                (setq count (1+ count))
                (cons (format "%s" (funcall index-method sublist count))
                      (mapcar
                       (function
                        (lambda (menu)
                          (vector (format "%s" (car menu))
                                  (list 'fume-goto-function
                                        (car menu) (cdr menu))
                                  t)))
                       sublist))))
             (fume-split fume-funclist fume-max-items))))

      (or (> count 1)
          (setq function-menu (cdr (car function-menu))))

      (setq function-menu (cons
                           (vector
                            (concat "Rescan buffer :  " (buffer-name))
                            (list 'fume-rescan-buffer (null use-menubar))
                            t)
                           (cons "----" function-menu)))

      (cond (use-menubar
             (fume-remove-menubar-entry)
             (set-buffer-menubar (copy-sequence current-menubar))
             (add-menu nil
                       fume-menubar-menu-name
                       (append
                        function-menu
                        (list
                         "----"
                         (vector "Remove Function Menu from menubar"
                                 'fume-remove-menubar-entry t)))
                       fume-menubar-menu-location))
            ((not (popup-menu-up-p))
             (or (fume-update-menubar-entry)
                 (setq function-menu
                       (cons
                        (vector "Put Function Menu into menubar"
                                (list 'function-menu t) t)
                        (cons "----" function-menu))))

             (if fume-auto-position-popup
                 (set-mouse-position
                  (selected-screen)
                  (nth 0 (window-edges)) (nth 1 (window-edges))))

             (popup-menu
              (cons (concat "Function menu" (if (> count 1) "s"))
                    function-menu)))))))

;;; Espen Skoglund <espensk@stud.cs.uit.no>
(defun fume-prompt-function-goto ()
  "Goto function prompted for in minibuffer (with completion)."
  (interactive)
  ;; Create funclist and set defaults
  (if fume-funclist
      ()
    (fume-set-defaults)
    (fume-rescan-buffer))
  (let* (;; Get default name from current position
         (default-name
           (save-excursion
             (while (looking-at "\\sw\\|\\s_") (forward-char 1))
             (if (re-search-backward "\\sw\\|\\s_" nil t)
                 (let ((beg (progn (forward-char 1) (point))))
                   (forward-sexp -1)
                   (while (looking-at "\\s'") (forward-char 1))
                   (buffer-substring beg (point))))))
         ;; verify default-name is a valid function name
         (default-exists-p (assoc default-name fume-funclist))
         ;; Prompt for function name in minibuffer
         (function-name
          (completing-read
           (if default-exists-p
               (concat "Function name (" default-name "): ")
             "Function name: ")
           fume-funclist nil t))
         ;; Use default function name if just RET was pressed
         (function-name (if (and default-exists-p (string= "" function-name))
                            default-name
                          function-name)))
    ;; Beat redisplay glitch if *Completions* window was used
    (sit-for 0)
    ;; Goto function or just return if function name is empty string
    (or (string= "" function-name)
        (fume-goto-function
         function-name (cdr (assoc function-name fume-funclist))))))

(provide 'func-menu)
