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
;;; Gopher and Gopher+ support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

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

(defun w3-grok-gopher-link (str)
  "Return a list of link attributes from a gopher string.  Order is:
title, type, selector string, server, port, gopher-plus?"
  (if (string-match "^\\([^\\\t\\\n]*\\)\\\t\\([^\\\t\\\n]*\\)\\\t\\([^\\\t\\\n]*\\)\\\t\\([^\\\t\\\n]*\\)\\\t*\\([^\\\r]\\)\\\r*$" str)
      (list (w3-match str 1)
	    (concat (char-to-string (aref (w3-match str 1) 0))
		    (w3-match str 2))
	    (w3-match str 3) (w3-match str 4) (w3-match str 5))
    (make-list 5 nil)))

(defun w3-is-gopher-directory-listing (&optional buffer)
  "Return t iff buffer BUFFER contains a gopher directory listing.
BUFFER defaults to w3-working-buffer."
  (save-excursion
    (set-buffer (or buffer w3-working-buffer))
    (goto-char (point-min))
    (while (looking-at "\\\n") (delete-char 1))
    (re-search-forward "\\`\\([^\\\t\\\n]*\\\t[^\\\t\\\n]*\\\t[^\\\t\\\n]*\\\t[^\\\t\\\n]*\\\t*[^\\\r]\\\r*\\\n*\\)+\\'" nil t)))

(defun w3-format-gopher-link (gophobj)
  "Insert a gopher link as an <A> tag"
  (let ((title (nth 0 gophobj))
	(ref   (nth 1 gophobj))
	(type  (if (> (length (nth 1 gophobj)) 0)
		   (substring (nth 1 gophobj) 0 1) ""))
	(serv  (nth 2 gophobj))
	(port  (nth 3 gophobj))
	(plus  (nth 4 gophobj))
	(desc  nil))
    (if (and (equal type "")
	     (> (length title) 0))
	(setq type (substring title 0 1)))
    (setq title (and title (substring title 1 nil)))
    (setq desc (or (cdr (assoc type w3-gopher-labels)) "(UNK)"))
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

(defun w3-parse-gopher (&optional buffer)
  "Parse out a gopher response"
  (save-excursion
    (set-buffer (or buffer w3-working-buffer))
    (w3-replace-regexp (regexp-quote "&") "&amp;")
    (w3-replace-regexp (regexp-quote ">") "&gt;")
    (w3-replace-regexp (regexp-quote "<") "&lt;")
    (w3-replace-regexp "\\\n*\\.\\\n*\\'" "\n")
    (goto-char (point-max)) (insert "\n")
    (goto-char (point-min))
    (let ((objs nil))
        (goto-char (point-min))
	(while (re-search-forward "^\\(.*\\)\\\r*\\\n" nil t)
	  (if (string= "" (buffer-substring (match-beginning 1) (match-end 1)))
	      nil
	    (setq objs (cons
			(w3-grok-gopher-link
			 (prog1
			     (buffer-substring (match-beginning 1)
					       (match-end 1))
			   (replace-match ""))) objs))))
	(setq objs (nreverse objs))
	(goto-char (point-min))
	(insert "<TITLE>"
		(cond
		 ((string= "" w3-current-file) "Gopher root")
		 ((string= "1/" w3-current-file) "Gopher root")
		 ((string= "1" w3-current-file) "Gopher root")
		 ((string-match (format "^[%s]+/" w3-gopher-types)
				w3-current-file)
		  (substring w3-current-file 2 nil))
		 (t w3-current-file))
		"</TITLE><OL>")
	(while objs
	  (insert (w3-format-gopher-link (car objs)))
	  (setq objs (cdr objs)))
	(insert "</OL>"))))	

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
		    (memq (process-status proc) '(run open)))
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
	  (accept-process-output))
	(condition-case ()
	    (delete-process proc)
	  (error nil))
	(w3-replace-regexp (regexp-quote "\r") "")
	(w3-replace-regexp "^\\\.\\\n" "")
	(w3-replace-regexp "^\\\.\r*$\\\n*" "")
	(w3-replace-regexp "\\\n*Connection closed.*\\\n*" "")
	(w3-replace-regexp "\\\n*Process .*gopher.*\\\n*" "")))))

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
    (insert "<TITLE>Results of CSO search</TITLE>\n"
	    "<H1>" search-type " = " search-term "</H1>\n")
    (goto-char (point-max))
    (insert "</PRE>")))
	    

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
      (insert "<TITLE> CSO SEARCH </TITLE>\n"
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
     ((equal type "www/gopher-search")	; Ack!  Mosaic-style search href
      (setq type "text/html"
	    parse-gopher t)
      (w3-clear-tmp-buffer)
      (insert "<TITLE>Gopher Server</TITLE>\n")
      (insert "<H1>Searchable Gopher Index</H1>")
      (insert "<HR>Enter the search keywords below<P>")
      (insert "<FORM><INPUT NAME=\"internal-gopher\">&ensp;</FORM><HR>"))
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
	  (insert (w3-convert-ask-to-form x))
	  (setq type "text/html" parse-gopher t)))
       (t (setq parse-gopher t)))))
    (if (and parse-gopher (or (equal type "www/gopher")
			      (w3-is-gopher-directory-listing)))
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

(defun w3-read-gopher-icons (&optional directory)
  (let ((x (directory-files (or directory w3-default-icon-directory) t
			    "w3-gopher-..xbm"))
	type bit)
    (while x
      (string-match "w3-gopher-\\(.\\).xbm" (car x))
      (setq bit (make-pixmap (car x))
	    type (w3-match (car x) 1)
	    w3-gopher-icons (cons (cons type bit) w3-gopher-icons)
	    x (cdr x)))))

(provide 'w3-gopher)
