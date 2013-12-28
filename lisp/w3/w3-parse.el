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
;;; Functions to parse out a url and replace it in the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

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
  (let* ((next-id (int-to-string (1+ (w3-find-highest-link-num))))
	 (cur-id "")
	 (cur-txt "")
	 (cur-urn "")
	 (cur-rel "")
	 (cur-href "")
	 (cur-rev "")
	 (cur-title "")
	 (cur-meth "")
	 )
    (goto-char (point-min))
    (while (re-search-forward w3-link-begin-regexp nil t)
      (let* ((start (match-beginning 0))
	     (cur-url (prog1
			  (buffer-substring (match-beginning 0)
					    (match-end 0))
			(replace-match "")))
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
	(if (string-match "NAME *= *\"*\\([^\" >]*\\)\"*" cur-url)
	    (setq cur-id (w3-match cur-url 1))
	  (setq cur-id next-id
		next-id (int-to-string (1+ (string-to-int next-id)))))
	(if (string-match "HREF *= *\"*[ \\\t]*\\([^>\" ]*\\)\"*" cur-url)
	    (setq cur-href (w3-match cur-url 1))
	  (setq cur-href nil))
	(if (string-match "[^H]REF *= *\"*\\([^\" ]*\\)\"*" cur-url)
	    (setq cur-rel (w3-match cur-url 1)))
	(if (string-match "REV *= *\"*\\([^\" ]*\\)\"*" cur-url)
	    (setq cur-rev (w3-match cur-url 1))
	  (setq cur-rev nil))
	(if (string-match "URN *= *\"*\\([^\" ]*\\)\"*" cur-url)
	    (setq cur-rev (w3-match cur-url 1))
	  (setq cur-rev nil))
	(if (string-match "TITLE *= *\"\\([^\" ]*\\)\"" cur-url)
	    (setq cur-title (w3-match cur-url 1))
	  (setq cur-title nil))
	(if (string-match "METHODS *= *\"*\\([^\" ]*\\)\"*" cur-url)
	    (setq cur-meth (w3-match cur-url 1))
	  (setq cur-meth nil))
	(setq cur-txt (w3-fix-entities-in-string (buffer-substring start end)))
	(if (and cur-href
		 (not (string-match w3-nonrelative-link cur-href)))
	    (setq cur-href (w3-parse-relative-link cur-href)))
	(cond
	 ((and (eq w3-delimit-links 'linkname) cur-href)
	  (goto-char end)
	  (insert (format "[%s]" cur-id)))
	 ((and (not (null w3-delimit-links)) cur-href)
	  (goto-char start)
	  (insert w3-link-start-delimiter)
	  (goto-char (+ end (length w3-link-start-delimiter)))
	  (insert w3-link-end-delimiter)
	  (setq end (+ end (length w3-link-start-delimiter)
		       (length w3-link-end-delimiter)))))
	(if cur-href
	    (w3-add-zone start end w3-node-style
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
	 (cur-rev "")
	 (result nil))
    (goto-char (point-min))
    (while (re-search-forward "<LINK[ \\\t]+" nil t)
      (let* ((start (prog1 (match-beginning 0) (replace-match "")))
	     (end (prog2 (re-search-forward ">" nil t) (point)
		    (replace-match "")))
	     (cur-lnk (prog1 (buffer-substring start end)
			(delete-region start end))))
	(if (string-match "RE[LV][ \\\t]*=\"*\\([^\"]*\\)\"*" cur-lnk)
	    (setq cur-rev (downcase (w3-match cur-lnk 1))))
	(if (string-match "HREF *= *\"*[ \\\t]*\\([^>\"]*\\)\"*" cur-lnk)
	    (setq cur-href (w3-match cur-lnk 1))
	  (setq cur-href nil))
	(if (and cur-href (not (string-match w3-nonrelative-link cur-href)))
	    (setq cur-href (w3-parse-relative-link cur-href)))
	(setq result (cons (cons cur-rev cur-href) result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to handle formatting an html buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-prepare-buffer ()
  "Function to prepare w3-buffer for processing.  This will completely
reformat a buffer - if you just want to parse out links, see the documentation
for w3-build-links-list."
  (message "Parsing...")
  (set-buffer w3-working-buffer)
  (let ((annos (w3-fetch-personal-annotations)))
    (if annos
	(progn
	  (goto-char (point-max))
	  (insert
	   "<P>--------------\n<H1>Personal Annotations</H1><P><UL>")
	  (while annos
	    (goto-char (point-max))
	    (insert "\n<LI> " (car annos))
	    (setq annos (cdr annos)))
	  (insert "</UL>"))))
  (let ((hdrs w3-show-headers)
	x y)
    (goto-char (setq y (point-max)))
    (while hdrs
      (if (setq x (assoc (car hdrs) w3-current-mime-headers))
	  (insert "<LI> <B>" (car x) "</B> :" (w3-insert-entities-in-string
					       (cdr x))))
      (setq hdrs (cdr hdrs)))
    (if (= y (point-max))
	nil
      (insert "</UL>")
      (goto-char y)
      (insert "<HR><UL>")))    
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (run-hooks 'w3-file-prepare-hooks)
  (if (and w3-running-lemacs
	   (face-differs-from-default-p w3-default-style))
      (set-extent-face (make-extent (point-min) (point-max)) w3-default-style))
  (setq fill-column (- (or w3-strict-width (window-width)) w3-right-border))
  (w3-replace-regexp (format "[%c%c%c]" ?\r ? ?) "")
  (let ((case-fold-search t)
	(ttl "")
	(pltxt nil))
    (goto-char (point-min))
    (w3-replace-regexp "<!--[.\\\n]--[\\\t\\\n]>" "")
    (goto-char (point-min))
    (if (re-search-forward "<PLAINTEXT>" nil t)
	(progn
	  (replace-match "")
	  (setq pltxt (buffer-substring (point) (point-max)))
	  (delete-region (point) (point-max))))
    (w3-replace-regexp "<\\(TBL[^>]*\\)>" "<\\1>\n<PRE>")
    (w3-replace-regexp "</TBL>" "</TBL>\n</PRE>")
    (w3-replace-regexp "<TEXTAREA" "<PRE><TEXTAREA")
    (w3-replace-regexp "</TEXTAREA>" "</TEXTAREA></PRE>")
    (goto-char (point-min))
    (w3-balance-pre)
    (w3-balance-xmp)
    (message "Parsing...")
    (w3-handle-arbitrary-tags)
    (w3-ignore)
    (w3-check-index)
    (w3-kill-obsolete)
    (w3-replace-regexp "<LIT>" "<PRE>")
    (w3-replace-regexp "</LIT>" "</PRE>")
    (w3-fix-xmp)
    (w3-fix-pre)
    (w3-fix-render-hints)
    (w3-handle-footnotes)
    (w3-handle-notes)
    (setq w3-current-links (w3-handle-links))
    (w3-fix-extras)
    (w3-fix-nonames)
    (goto-char (point-min))
    (message "Parsing...")
    (w3-replace-regexp "[ \\\t]*<SP>[ \\\t]*" "<SP>")
    (w3-replace-regexp "[ \\\t]*&nbsp;[ \\\t]*" "&nbsp;")
    (w3-handle-whitespace)
    (w3-handle-headers)
    (message "Parsing...")
    (w3-handle-address)
    (message "Parsing...")
    (w3-handle-graphics)
    (w3-restore-pre)
    (w3-build-links-list)
    (w3-handle-forms)
    (w3-do-lists)
    (w3-replace-regexp "<LI>" "\n\t*")
    (w3-replace-regexp "<DT>" "\n<DT>")
    (w3-replace-regexp "<DD>" "\n\t*")
    (w3-handle-tables)
    (goto-char (point-min))
    (let ((st (if (re-search-forward "<TITLE>[ \\\t]*" nil t)
		  (prog1
		      (match-beginning 0)
		    (replace-match "")) nil))
	  (nd (if (re-search-forward "[ \\\t]*</TITLE>" nil t)
		  (prog1
		      (match-beginning 0)
		    (replace-match ""))
		nil)))
      (if st
	  (progn
	    (setq ttl (buffer-substring st nd))
	    (delete-region st nd))
	(setq ttl (w3-basepath w3-current-file t)))
      (if (> (length ttl) 50) (setq ttl (substring ttl 0 50)))
      (setq ttl (w3-generate-new-buffer-name ttl)))
    (w3-fix-paragraphs)
    (w3-replace-regexp "<SP>" " ")
    (w3-replace-regexp "<[^>]*>" "")
    (w3-fix-ampersands)
    (w3-restore-xmp)
    (goto-char (point-min))
    (set-buffer w3-working-buffer)
    (w3-mode)
    (rename-buffer ttl)
    (if w3-mutable-windows
	(pop-to-buffer ttl)
      (switch-to-buffer ttl))
    (if pltxt (progn (goto-char (point-max)) (insert "\n" pltxt)))
    (goto-char (point-min))
    (while (looking-at "[ \\\n]")
      (delete-char 1))
    (if (boundp 'MULE)
	(w3-mule-attribute-zones w3-zones-list w3-mule-attribute))
    (if (not buffer-read-only) (toggle-read-only))
    (message "Done.")
    (w3-fix-extent-endpoints)
    (goto-char (point-min))
    (if w3-find-this-link
	(w3-find-specific-link w3-find-this-link))
    (run-hooks 'w3-file-done-hooks)
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
    (message "Done.")
    (if w3-running-epoch (set-variable 'buffer-style w3-default-style))
    (if w3-running-FSF19 (setq w3-zones-list (w3-only-links)))
    (sit-for 0)))

(defun w3-handle-notes ()
  "Handle NOTE tags, as per the HTML+ 'Notes and Admonishments' section."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let (role img x)
    (while (re-search-forward "<NOTE\\([^>]*\\)>" nil t)
      (setq x (buffer-substring (match-beginning 1) (match-end 1)))
      (replace-match "<HR><BR><B>")
      (if (string-match "ROLE=\"\\([^\"]+\\)\"" x)
	  (setq role (substring x (match-beginning 1) (match-end 1)))
	(setq role "NOTE"))
      (if (string-match "SRC=\"\\([^\"]+\\)\"" x)
	  (setq img (substring x (match-beginning 1) (match-end 1)))
	(setq img nil))
      (if img
	  (insert (format "<IMG SRC=\"%s\" ALIGN=\"CENTER\">" img)))
      (insert (format "<B>%s:</B>" role))))
  (w3-replace-regexp "</NOTE>" "<BR><HR>"))

(defun w3-handle-footnotes ()
  "Handle footnotes, margin notes, etc, from the HTML+ spec"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (if (re-search-forward "<FOOTNOTE>" nil t)
      (progn
	(goto-char (point-max))
	(insert "<P>Footnotes<BR><HR>")
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
      (insert (format "<A HREF=\"#w3-internal-footnote%d\">%d</A>"
		      fcounter fcounter))
      (goto-char (point-max))
      (insert (format "<P ID=\"w3-internal-footnote%d\">%d. "
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
      (insert (format "<A HREF=\"#w3-internal-footnote%d\">%d</A>"
		      fcounter fcounter))
      (goto-char (point-max))
      (insert (format "<P ID=\"w3-internal-footnote%d\">%d. "
		      fcounter fcounter) txt)
      (setq fcounter (1+ fcounter))
      (goto-char (point-min)))))    
       
(defun w3-fix-render-hints ()
  "Parse out the RENDER hints ala the HTML+ specification."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let (x tag sty)
    (while (re-search-forward "<RENDER\\([^>]+\\)>" nil t)
      (setq x (buffer-substring (match-beginning 1) (match-end 1)))
      (replace-match "")
      (if (string-match "TAG=\"\\([^\"]+\\)\"" x)
	  (setq tag (substring x (match-beginning 1) (match-end 1))))
      (if (string-match "STYLE=\"\\([^\"]+\\)\"" x)
	  (setq sty (substring x (match-beginning 1) (match-end 1))))
      (w3-replace-regexp (format "<%s>" tag) (format "<%s>" sty))
      (w3-replace-regexp (format "</%s>" tag) (format "</%s>" sty)))))

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
      (insert "</XMP>\n"))))

(defun w3-balance-pre ()
  "This function will attempt to balance embedded plaintext elements
(<PRE> tags).  This is necessary or the parser will fail some critical
regular expression matches."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let* ((st (w3-count-occurences "<PRE>"))
	 (nd (w3-count-occurences "</PRE>"))
	 (df (- st nd)))
    (goto-char (point-max))
    (while (> df 0)
      (setq df (1- df))
      (insert "</PRE>\n"))))

(defun w3-fix-extras ()
  "Replace <B>, <I>, etc tags in the buffer.  Appropriate zones will be
created, and highlighting will be added when possible."
  (if (not w3-setup-done) (w3-do-setup))
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
	    (insert (cdr ltrs))
	    (goto-char st)
	    (insert (car ltrs)))))))

(defun w3-fix-ampersands ()
  "Replace &#XXX with ASCII character XXX."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let ((tmp w3-html-entities)
	(case-fold-search nil))
    (while (re-search-forward "&#\\([0-9]+\\);*" nil t)
      (replace-match (char-to-string 
		      (string-to-int (buffer-substring (match-beginning 1)
						       (match-end 1))))))
    (while tmp
      (w3-replace-regexp (car (car tmp)) (cdr (car tmp)))
      (setq tmp (cdr tmp))))
  (goto-char (point-min)))

(defun w3-fix-pre ()
  "Extract <PRE> fields, and put them back in later."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (setq w3-pre-data nil
	w3-pre-data-count 0)
  (while (re-search-forward "<PRE>" nil t)
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
	(insert "***PREDATA" (int-to-string w3-pre-data-count)))
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
      (insert "***XMPDATA" (int-to-string w3-xmp-data-count)))))

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

(defun w3-fix-nonames ()
  "Replace links with no name fields with bogus #s.
This probably isn't necessary any more."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (let ((bogus-num (1+ (w3-find-highest-link-num))))
    (while (re-search-forward "<A[ \t\n]+HREF" nil t)
      (replace-match (concat "<A NAME=" (int-to-string bogus-num) " HREF"))
      (setq bogus-num (1+ bogus-num)))))

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

(defun w3-ignore ()
  "Ignore certain fields - (NEXTID, etc).
This can probably be ignored, and let the general <...> replacement code
take care of it."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (w3-replace-regexp "<NEXTID *[A-Z]* *=* *\"*[a-zA-z0-9]*\"*>" ""))

(defun w3-kill-obsolete ()
  "Delete old/obsolete html headers/references"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (w3-replace-regexp "</*HEAD\\(ER\\)*>" "")
  (w3-replace-regexp "</*BODY>" "")
  (w3-replace-regexp "<LISTING>" "<PRE>")
  (w3-replace-regexp "</LISTING>" "</PRE>"))

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
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (while (re-search-forward
	  "[ \\\t\\\n]*\\(<P[^>]*>\\)*[ \\\t\\\n]*<H\\([0-9]+\\)\\([^>]*\\)>"
	  nil t)
    (let* ((siz (buffer-substring (match-beginning 2) (match-end 2)))
	   (tags (buffer-substring (match-beginning 3) (match-end 3)))
	   x y
	   (st (prog1
		   (match-beginning 0)
		 (setq x 
		       (if (match-beginning 1)
			   (w3-eat-trailing-space
			    (w3-strip-leading-spaces
			     (buffer-substring (match-beginning 1)
					       (match-end 1)))) "<P>"))
		 (replace-match "")))
	   (end (progn
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
		    (progn (end-of-line) (setq y "<P>") (point)))))
	   (forms (cdr (assoc siz w3-header-chars-assoc))))
      (w3-add-zone st end w3-header-style
		   (cons 'w3header
			 (list
			  (if (string-match "ID=\"\\([^\"]+\\)\"" tags)
			      (substring tags (match-beginning 1)
					 (match-end 1))
			    nil) nil nil nil nil nil nil nil)))
      (if (and forms w3-delimit-emphasis)
	  (progn
	    (if (nth 2 forms) (funcall (nth 2 forms) st end))
	    (goto-char end)
	    (insert (nth 1 forms) y)
	    (goto-char st)
	    (insert x (nth 0 forms)))
	(progn
	  (goto-char end) (insert y)
	  (goto-char st) (insert x)))))
  (goto-char (point-min)))

(defun w3-fix-horizontal-rules ()
  "Replace all the <HR> tags"
  (goto-char (point-min))
  (if (and (not w3-hrule-pixmap) w3-running-epoch)
      (setq w3-hrule-pixmap (w3-create-hrule)))
  (while (re-search-forward "<[Hh][rR]>" nil t)
    (if (fboundp 'add-graphic-zone)
	(progn
	  (replace-match "-\n")
	  (add-graphic-zone
	   w3-hrule-pixmap (match-beginning 0)
	   (1+ (match-beginning 0)) 50 nil))
      (replace-match (format "<P>%s<P>" (make-string (- (or w3-strict-width
							    (window-width))
							w3-right-border)
						     ?-))))))

(defun w3-fix-paragraphs-in-region ()
  "Fill paragraphs in the visible part of the buffer"
  (set-buffer w3-working-buffer)
  (w3-fix-horizontal-rules)
  (goto-char (point-min))
  (w3-replace-regexp "<[bB][Rr]> *" "<X>\n<BR>")
  (w3-replace-regexp "\\\n\\\n\\\t" "\n\t")
  (goto-char (point-min))
  (let (ptag st)
    (while (re-search-forward "<P\\([^>]*\\)>" nil t)
      (setq ptag (buffer-substring (match-beginning 1) (match-end 1)))
      (setq st (match-beginning 0))
      (replace-match (concat "\n\n" fill-prefix))
      (if (string-match "ID=\"\\([^\"]+\\)\"" ptag)
	  (w3-add-zone st (progn (end-of-line) (point))
		       w3-default-style
		       (cons 'w3par
			     (list (substring ptag (match-beginning 1)
					      (match-end 1))
				   nil nil nil nil nil nil nil))))))
  (w3-replace-regexp "^ +" "")
  (goto-char (point-min))
  (while (re-search-forward "^[^\\\n\\\t]" nil t)
    (fill-region (progn (beginning-of-line) (point))
		 (progn (end-of-line) (point))))
  (w3-replace-regexp "<X>\\\n+<BR>" "\n")
  (w3-replace-regexp "\\\n\\\n+" "\n\n")
  (w3-replace-regexp "<[sS][Pp]>" " "))

(defun w3-fix-paragraphs (&optional pt recur)
  "Fix filling of paragraphs in a new buffer"
  (set-buffer w3-working-buffer)
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
    (widen)))

(defun w3-handle-address ()
  "Handle the <ADDRESS> field"
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (while (re-search-forward "\\\n*<ADDRESS>\\\n*" nil t)
    (let* ((st (prog1
		   (match-beginning 0)
		 (replace-match "<BR>")))
	   (nd (progn
		 (if (re-search-forward "\\\n*</ADDRESS>\\\n*" nil t)
		     (prog1
			 (match-beginning 0)
		       (replace-match "<BR>"))
		   (point)))))
      (w3-add-zone st nd w3-address-style '(w3address)))))

(defun w3-handle-graphics ()
  "A function to parse out IMG tags.  In epoch, this will actually
insert the picture into the buffer.  The ALT attribute is displayed
when not in epoch (or when epoch fails to read in the graphic
correctly."
  (set-buffer w3-working-buffer)
  (goto-char (point-min))
  (if (and w3-running-epoch (fboundp 'add-graphic-zone))
      (while (re-search-forward "<IMG[ \\\t]+\\([^>]+\\)>" nil t)
	(let ((img (buffer-substring (match-beginning 1) (match-end 1)))
	      (src "")
	      (align 'center)
	      (alt nil)
	      (st (match-beginning 0)))
	  (replace-match "^")
	  (if (string-match "ALT[ \\\t\\\n]*=[ \\\t\\\n]*\"\\([^\"]*\\)\"" img)
	      (setq alt (substring img (match-beginning 1) (match-end 1)))
	    (setq alt "&lt;IMAGE&gt;"))
	  (if (string-match "ALIGN[ \\\t]*=[ \\\t]*\"*\\([^\"]+\\)\"" img)
	      (setq align (read (downcase (substring img
						     (match-beginning 1)
						     (match-end 1))))))
	  (if (string-match "SRC[ \\\t]*=[ \\\t]*\"*\\([^\"]+\\)\"" img)
	      (setq src (substring img (match-beginning 1) (match-end 1))))
	  (if w3-delay-image-loads
	      (progn
		(setq w3-delayed-images
		      (cons (list src st align alt) w3-delayed-images)))
	    (w3-insert-graphic src st align alt))))
    (progn
      (goto-char (point-min))
      (let ((alt "") (img ""))
	(while (re-search-forward "<IMG[ \\\t\\\n]*\\([^>]+\\)>" nil t)
	  (setq img (buffer-substring (match-beginning 1) (match-end 1)))
	  (replace-match "")
	  (if (string-match "ALT[ \\\t\\\n]*=[ \\\t\\\n]*\"\\([^\"]*\\)\"" img)
	      (setq alt (substring img (match-beginning 1) (match-end 1)))
	    (setq alt "&lt;IMAGE&gt;"))
	  (insert alt))))))

(provide 'w3-parse)
