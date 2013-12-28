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
;;; Support for printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

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
	(w3-retrieve url)
	(lpr-buffer))
       ((equal "Formatted Text" format)
	(lpr-buffer))
       ((equal "LaTeX'd" format)
	(w3-retrieve url)
	(w3-convert-html-to-latex)
	(save-window-excursion
	  (write-region (point-min) (point-max) "/tmp/w3-tmp.latex" nil 5)
	  (shell-command
	   (format
	    "cd /tmp/ ; latex w3-tmp.latex ; %s w3-tmp.dvi ; rm -f w3-tmp*"
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
      (insert "\\documentstyle{article}\n\\begin{document}\n")
      (goto-char (point-max))
      (insert "\\end{document}")
      (w3-replace-regexp "<\\(XMP\\|LISTING\\)>" "\\\\begin{verbatim}")
      (w3-replace-regexp "</\\(XMP\\|LISTING\\)>" "\\\\end{verbatim}")
      (w3-replace-regexp "<\\(ISINDEX\\|NEXTID\\)[^>]*>" "")
      (w3-replace-regexp (regexp-quote "$") "\\\\$")
      (w3-replace-regexp (regexp-quote "&gt;") "$>$")
      (w3-replace-regexp (regexp-quote "&lt;") "$<$")
      (w3-replace-regexp (regexp-quote "&amp;") " and ")
      (w3-replace-regexp "%" "\\\\%")
      (w3-replace-regexp "#" "\\\\#")
      (w3-replace-regexp "_" "\\\\_")
      (w3-replace-regexp "~" "\\\\~")
      (w3-replace-regexp "<LI> *" "\\\\item ")
      (w3-replace-regexp (regexp-quote "^") "\\\\^")
      (w3-replace-regexp "<P>" "\\\\par")
      (w3-replace-regexp "<TITLE>\\([^<]*\\)</TITLE>" "\\\\section{\\1}")
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
      (w3-replace-regexp "<IMG[^>]*>" ""))))

(provide 'w3-print)
