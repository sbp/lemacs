;; big-menubar.el --- an alternate menubar

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Dror Caspi <dcaspi@qualcomm.com>
;; Modified by: jwz and allender

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

(defconst big-menubar
  (purecopy-menubar
   (list
    (assoc "File" default-menubar)
    (append
     (assoc "Edit" default-menubar)
     '("-----"
       ["Copy to Register..."		copy-to-register		(mark)]
       ["Paste Register..."		insert-register			t]
       "-----"
       ("Mark"
	["Here"				set-mark-command		t]
	["Word"				mark-word			t]
	["Sentence"			mark-end-of-sentence		t]
	["Paragraph"			mark-paragraph			t]
	["Page"				mark-page			t]
	["Balanced Expression"		mark-sexp			t]
	["Lisp Function"		mark-defun			t]
	["C Function"			mark-c-function			t]
	["Whole Buffer"			mark-whole-buffer		t]
	)
       "-----"
       ("Search"
	["Forward..."			isearch-forward			t]
	["Backward..."			isearch-backward		t]
	"-----"
	["Regexp Forward..."		isearch-forward-regexp		t]
	["Regexp Backward..."		isearch-backward-regexp		t]
	"-----"
	["Words Forward..."		word-search-forward		t]
	["Words Backward..."		word-search-backward		t]
	)
       ("Replace"
	["Query..."			query-replace			t]
	["Regexp Query..."		query-replace-regexp		t]
	"-----"
	["All..."			replace-string			t]
	["Regexp All..."		replace-regexp			t]
	)
       "-----"
       ("Transpose"
	["Characters"			transpose-chars			t]
	["Words"			transpose-words			t]
	["Lines"			transpose-lines			t]
	["Sentences"			transpose-sentences		t]
	["Paragraphs"			transpose-paragraphs		t]
	["Balanced Expressions"		transpose-sexps			t]
	)
       ("Sort"
	["Lines"			sort-lines			(mark)]
	["Paragraphs"			sort-paragraphs			(mark)]
	["Pages"			sort-pages			(mark)]
	["Columns"			sort-columns			(mark)]
	["Regexp..."			sort-regexp-fields		(mark)]
	)
       ("Center"
	["Line"				center-line			t]
	["Paragraph"			center-paragraph		t]
	["Region"			center-region			(mark)]
	)
       ("Indent"
	["As Previous Line"		indent-relative			t]
	["To Column..."			indent-to-column		t]
	"-----"
	["Region"			indent-region			(mark)]
	["Balanced Expression"		indent-sexp			t]
	["C Expression"			indent-c-exp			t]
	)
       "-----"
       ("Narrow"
	["To Region"			narrow-to-region		(mark)]
	["To Page"			narrow-to-page			t]
	"-----"
	["Cancel"			widen
	 (not (and (= (point-min) 1) (= (point-max) (1+ (buffer-size)))))]
	)
       ))
    (assoc "Options" default-menubar)
    '("Motion"
      ["Goto Mark"			exchange-point-and-mark	     (mark t)]
      ["Goto Line..."			goto-line			t]
      "-----"
      ["End of Balanced Parentheses ( )"	forward-list		t]
      ["Beginning of Balanced Parentheses ( )"	backward-list		t]
      ["Next Opening Parenthesis ("		down-list		t]
      ["Previous Opening Parenthesis ("		backward-up-list	t]
      ["Next Closing Parenthesis )"		up-list			t]
      "-----"
      ["End of Balanced Expression"		forward-sexp		t]
      ["Beginning of Balanced Expression"	backward-sexp		t]
      "-----"
      ["End of Function"		end-of-defun		t]
      ["Beginning of Function"		beginning-of-defun		t]
      "-----"
      ["Next Page"			forward-page			t]
      ["Previous Page"			backward-page			t]
      "-----"
      ["End of Buffer"			end-of-buffer			t]
      ["Beginning of Buffer"		beginning-of-buffer		t]
      "-----"
      ["Save Current Position..."	point-to-register		t]
      ["Goto Saved Position..."		register-to-point		t]
      "-----"
      ["Set Marker..."			set-user-marker			t]
      ["Goto Marker..."			goto-user-marker		t]
      ["List Markers"			list-markers			t]
      "-----"
      ["Set Goal Column"		set-goal-column			t]
      ["Cancel Goal Column"		(set-goal-column t)	goal-column]
      )
    '("Run"
      ["Compile..."			compile				t]
      ["Kill Compilation"		kill-compilation		t]
      "-----"
      ["Next Error"			next-error			t]
      ["Previous Error"			previous-error			t]
      ["Goto Error"			compile-goto-error		t]
      "-----"
      ["GDB Debugger"			gdb				t]
      )
    '("Utilities"
      ["Shell"				shell				t]
      "-----"
      ("Mail"
       ["Send"				mail				t]
       ["Read"				rmail				t]
       ["Read Folder..."		rmail-input			t]
       )
      ["Dired..."			dired				t]
      "-----"
      ["Grep..."			grep				t]
      ("Tags"
       ["Set Tags Table File..."	visit-tags-table		t]
       "-----"
       ["Show Occurrence..."		find-tag			t]
       ["Show Occurrence (Other)..."	find-tag-other-window		t]
       ["Next Occurrence"		(find-tag nil)			t]
       ["Next Occurrence (Other)"	(find-tag-other-window nil)	t]
       "-----"
       ["Search by Tags..."		tags-search			t]
       ["Query Replace by Tags..."	tags-query-replace		t]
       ["Continue Search/Replace"	tags-loop-continue		t]
       "-----"
       ["Next File"			next-file			t]
       "-----"
       ["List Tags in File..."		list-tags			t]
       ["List Tags by Regexp..."	tags-apropos			t]
       )
      "-----"
      ("Spell Check"
       ["Word"				ispell-word			t]
       ["Region"			ispell-region			t]
       ["Whole Buffer"			ispell-buffer			t]
       )
      "-----"
      ("Compare Windows"
       ["Exact Match"			compare-windows			t]
       ["Ignore White Space"		(compare-windows t)		t]
       )
      "-----"
      ["Hex Edit File..."		hexl-find-file			t]
      )
    '("Macro"
      ["Start Macro Recording"		start-kbd-macro
					(not defining-kbd-macro)]
      ["End Macro Recording"		end-kbd-macro	    defining-kbd-macro]
      ["Name Last Macro..."		name-last-kbd-macro	last-kbd-macro]
      ["Insert Macro in Buffer..."	insert-kbd-macro		     t]
      ["Execute Last Macro"		call-last-kbd-macro	last-kbd-macro]
      )
    ;(assoc "Buffers" default-menubar)
    '("Buffers" "")
    nil
    (assoc "Help" default-menubar)
    )))

(set-menubar big-menubar)
(install-font-menus)			; install fonts in options menu
