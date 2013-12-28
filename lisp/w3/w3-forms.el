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
;;; FORMS processing for html+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

(defun w3-split (str del)
  "Split the string STR, with DEL (a regular expression) as the delimiter.
Returns an assoc list that you can use with completing-read."
  (let (x y)
    (while (string-match del str)
      (setq y (substring str 0 (match-beginning 0))
	    str (substring str (match-end 0) nil))
      (if (not (string-match "^[ \\\t]+$" y))
	  (setq x (cons (list y y) x))))
    (if (not (equal str ""))
	(setq x (cons (list str str) x)))
    x))

(defun w3-handle-form (num)
  "Parse a form, expecting the region to be narrowed between the <FORM>
and </FORM> tags."
  (goto-char (point-min))
  (let ((x (if (re-search-forward "<FORM\\([^>]*\\)>" nil t)
	       (buffer-substring (match-beginning 1) (match-end 1)) ""))
	action method ;enctype
	st nd input type name default value checked size maxlength prompt
	options formatfun)
    (if (string-match "METHOD *=+ *\"*[ \\\t]*\\([^>\" ]*\\)\"*" x)
	(setq method (w3-match x 1)) (setq method "GET"))
    (if (string-match "ACTION *=+ *\"*[ \\\t]*\\([^>\" ]*\\)\"*" x)
	(setq action (w3-match x 1)) (setq action (w3-view-url t)))
    (setq action (cons method action))
    (while (re-search-forward "<INPUT" nil t)
      (setq st (match-beginning 0)
	    nd (progn
		 (goto-char st)
		 (if (re-search-forward ">" nil t)
		     (match-end 0)
		   (progn (end-of-line) (point))))
	    input (buffer-substring st nd))
      (delete-region st nd)
      (if (string-match "TYPE[ \\\t]*=+[ \\\t]*\"*\\([^ \"]+\\)\"*" input)
	  (setq type (upcase
		      (substring input (match-beginning 1) (match-end 1))))
	(setq type ""))
      (if (string-match "NAME[ \\\t]*=+[ \\\t]*\"+\\([^ \"]*\\)\"+" input)
	  (setq name (substring input (match-beginning 1) (match-end 1)))
	(setq name ""))
      (if (string-match "VALUE[ \\\t]*=+[ \\\t]*\"\\([^\"]*\\)\"" input)
	  (setq value (substring input (match-beginning 1) (match-end 1)))
	(setq value ""))
      (if (string-match "SIZE[ \\\t]*=+[ \\\t]*\"*\\([^ \"]+\\)\"*" input)
	  (setq size (string-to-int
		      (substring input (match-beginning 1) (match-end 1))))
	(setq size 20))
      (if (string-match "MAXLENGTH[ \\\t]*=+[ \\\t]*\"*\\([^ \"]+\\)\"*" input)
	  (setq maxlength (string-to-int
			   (substring input (match-beginning 1)
				      (match-end 1))))
	(setq maxlength 10000))
      (setq default value)
      (if (string-match "CHECKED" input)
	  (setq checked t)
	(setq checked nil))
      (if (equal type "CHECKBOX")
	  (setq default checked))
      (if (and (equal type "CHECKBOX")
	       (equal "" value))
	  (setq value "on"))
      (setq formatfun (intern (concat "w3-form-format-" (downcase type))))
      (if (not (fboundp formatfun))
	  (setq formatfun 'w3-form-format-unknown))
      (setq prompt (funcall formatfun value size checked))
      (goto-char st)
      (insert prompt)
      (w3-add-zone st (point) w3-node-style
		   (list 'w3form
			 action type name default value
			 checked size maxlength num options) t)
      (insert (if (w3-member type '("CHECKBOX" "RADIO")) "" " ")))))

(defun w3-form-format-int (&rest args)
  "Format an integer entry field"
  (let ((value (or (nth 0 args) ""))
	(size (nth 1 args)))
    (format "%s%s"
	    (if (> (length value) size) (substring value 0 size) value)
	    (if (> (length value) size)
		""
	      (make-string (- size (length value)) ?_)))))

(fset 'w3-form-format-url 'w3-form-format-int)
(fset 'w3-form-format-float 'w3-form-format-int)
(fset 'w3-form-format-date 'w3-form-format-int)

(defun w3-form-format-reset (&rest args)
  "Format a reset button"
  (if (string= (nth 0 args) "") "Reset fields" (nth 0 args)))

(defun w3-form-format-password (&rest args)
  "Format a password entry field"
  (let ((value (or (nth 0 args) ""))
	(size (nth 1 args)))
    (format "%s%s"
	    (if (>= (length value) size) (make-string size ?*)
	      (make-string (length value) ?*))
	    (if (>= (length value) size) ""
	      (make-string (- size (length value)) ?.)))))

(defun w3-form-format-checkbox (&rest args)
  "Format a checkbox entry"
  (let ((checked (nth 2 args)))
    (format "[%s]" (if checked "X" " "))))

(fset 'w3-form-format-radio 'w3-form-format-checkbox)

(defun w3-form-format-submit (&rest args)
  "Format a form submit button"
  (if (string= (nth 0 args) "") "Submit this form" (nth 0 args)))

(defun w3-form-format-text (&rest args)
  "Format a text field"
  (let ((value (nth 0 args))
	(size (nth 1 args)))
    (format "%s%s"
	    (if (> (length value) size) (substring value 0 size) value)
	    (if (> (length value) size)
		""
	      (make-string (- size (length value)) ?_)))))

(defun w3-form-format-textarea (&rest args)
  "Format a multiline text box"
  "Multiline text entry")

(fset 'w3-form-format- 'w3-form-format-text)
(fset 'w3-form-format-unknown 'w3-form-format-text)

(defun w3-handle-textareas (num action)
  "Handle &lt;SELECT&gt; tags in a form"
  (let (
	(type "TEXTAREA")
	name
	default
	value
	checked
	size
	(maxlength 100)
	options
	st
	nd
	input)
    (goto-char (point-min))
    (while (re-search-forward "<TEXTAREA\\([^>]*\\)>\\\n*" nil t)
      (setq input (buffer-substring (match-beginning 1) (match-end 1))
	    st (prog1 (match-beginning 0)
		 (replace-match ""))
	    nd (if (re-search-forward "\\n*</TEXTAREA[^>]*>\\\n*" nil t)
		   (progn
		     (replace-match "")
		     (match-beginning 0))
		 (progn
		   (end-of-line)
		   (point)))
	    value (buffer-substring st nd)
	    options value
	    default value)
      (if (string-match "NAME[ \\\t]*=+[ \\\t]*\"*\\([^ \"]+\\)\"*" input)
	  (setq name (substring input (match-beginning 1) (match-end 1)))
	(setq name ""))
      (delete-region st nd)
      (insert "Multiline Text Entry Area")
      (w3-add-zone st (point) w3-node-style
		   (list 'w3form
			 action type name default value checked size
			 maxlength num options) t))))

(defun w3-handle-selections (num action)
  "Handle &lt;SELECT&gt; tags in a form"
  (let (
	(type "OPTION")
	name
	default
	value
	checked
	size
	(maxlength 100)
	options
	parm
	st
	nd
	input
	mult				; Multiple input?
	(longest 0)			; Longest selection?
	sel)
    (goto-char (point-min))
    (while (re-search-forward "<SELECT\\([^>]*\\)>\\\n*" nil t)
      (setq input (buffer-substring (match-beginning 1) (match-end 1)))
      (setq options nil
	    mult nil
	    value nil
	    default nil)
      (replace-match "")
      (setq st (match-beginning 0))
      (setq nd
	    (if (re-search-forward "\\n*</SELECT[^>]*>\\\n*" nil t)
		(progn
		  (replace-match "")
		  (match-beginning 0))
	      (progn
		(end-of-line)
		(point))))
      (goto-char st)
      (while (re-search-forward "[\\\n ]*<OPTION\\([^>]*\\)> *\\([^<]*\\)"
				nd t)
	(setq parm (buffer-substring (match-beginning 1) (match-end 1))
	      sel (w3-eat-trailing-space
		   (buffer-substring (match-beginning 2) (match-end 2)))
	      options (cons (cons sel sel) options))
	(if (> (length sel) longest) (setq longest (length sel)))
	(if (string-match "selected" parm) (setq default sel)))
      (setq longest (+ 5 longest))
      (delete-region st nd)
      (if (string-match "NAME[ \\\t]*=+[ \\\t]*\"*\\([^ \"]+\\)\"*" input)
	  (setq name (substring input (match-beginning 1) (match-end 1)))
	(setq name ""))
      (if (string-match "SIZE[ \\\t]*=+[ \\\t]*\"*\\([^ \"]+\\)\"*" input)
	  (setq size (string-to-int (substring input (match-beginning 1)
					       (match-end 1))))
	(setq size 20))
      (setq mult (string-match "SEVERAL\\|MULTIPLE" input))
      (cond
       (mult
;	(let ((x 1) (z (/ (window-width) longest)))
	  (goto-char st)
	  (insert "<UL>\n")
	  (while options
	    (insert
	     (format
	      "<LI><INPUT TYPE=\"CHECKBOX\" NAME=\"%s\" VALUE=\"%s\" %s>%s\n"
	      name (car (car options))
	      (if (equal default (car (car options)))
		  "CHECKED" "") (car (car options))))
	    (setq options (cdr options)))
	  (insert "</UL>\n"));)
       (t
	(if (not default)
	    (setq value (car (nth (1- (length options)) options)))
	  (setq value default))
	(setq default value)
	(goto-char st)
	(if (> (length value) maxlength)
	    (setq value (substring value 0 maxlength)))
	(insert (w3-form-format-text value size nil))
	(w3-add-zone st (point) w3-node-style
		     (list 'w3form
			   action type name default value checked size
			   maxlength num options) t))))))

(defun w3-handle-forms ()
  "Take care of parsing an entire buffer for <FORM> tags."
  (set-buffer w3-working-buffer)
  (let ((num 1)
	x y z)
    (goto-char (point-min))
    (while (re-search-forward "<FORM\\([^>]*\\)>" nil t)
      (setq x (buffer-substring (match-beginning 1) (match-end 1)))
      (narrow-to-region (match-beginning 0)
			(if (re-search-forward "</FORM>" nil t) (match-end 0)
			  (point-max)))
      (if (string-match "METHOD *=+ *\"*[ \\\t]*\\([^>\" ]*\\)\"*" x)
	  (setq y (w3-match x 1)) (setq y "GET"))
      (if (string-match "ACTION *=+ *\"*[ \\\t]*\\([^>\" ]*\\)\"*" x)
	  (setq z (w3-match x 1)) (setq z (w3-view-url t)))
      (w3-handle-selections num (cons y z))
      (w3-handle-textareas num (cons y z))
      (w3-handle-form num)
      (setq num (1+ num))
      (w3-replace-regexp "</*FORM[^>]*>" "")
      (widen))))

(defun w3-do-text-entry (formobj zone)
  "Read in a multiline text entry area."
  (let ((data (list formobj zone (current-buffer)))
	(buff (get-buffer-create (format "%d:%s" (nth 9 formobj)
					 (nth 3 formobj)))))
    (switch-to-buffer-other-window buff)
    (indented-text-mode)
    (erase-buffer)
    (insert (nth 5 formobj))
    (setq w3-current-last-buffer data)
    (message "Press C-c C-c when finished with text entry.")
    (local-set-key "\C-c\C-c" 'w3-finish-text-entry)))

(defun w3-finish-text-entry ()
  "Finish a text entry area"
  (interactive)
  (if w3-current-last-buffer
      (let* ((formobj (nth 0 w3-current-last-buffer))
	     (zone (nth 1 w3-current-last-buffer))
	     (buff (nth 2 w3-current-last-buffer))
	     (actn (nth 1 formobj))
	     (type (nth 2 formobj))
	     (name (nth 3 formobj))
	     (deft (nth 4 formobj))
	     (valu (buffer-string))
	     (chkd (nth 6 formobj))
	     (size (nth 7 formobj))
	     (maxl (nth 8 formobj))
	     (ident (nth 9 formobj))
	     (options (nth 10 formobj))
	     (st nil)
	     (nd nil))
	(local-set-key "\C-c\C-c" 'undefined)
	(kill-buffer (current-buffer))
	(delete-window)
	(if (not (and buff (bufferp buff) (buffer-name buff)))
	    (message "Could not find the form buffer for this text!")
	  (switch-to-buffer buff)
	  (setq st (w3-zone-start zone)
		nd (w3-zone-end zone))
	  (w3-delete-zone zone)
	  (w3-add-zone st nd w3-node-style
		       (list 'w3form actn type name deft valu chkd
			     size maxl ident options) t)))
    nil))

(defun w3-do-form-entry (formobj zone)
  "Read in a data entry field defined by FORMOBJ, covered by zone ZONE."
  (let* ((actn (nth 1 formobj))
	 (type (nth 2 formobj))
	 (name (nth 3 formobj))
	 (deft (nth 4 formobj))
	 (valu (nth 5 formobj))
	 (chkd (nth 6 formobj))
	 (size (nth 7 formobj))
	 (maxl (nth 8 formobj))
	 (ident (nth 9 formobj))
	 (options (nth 10 formobj))
	 (st (w3-zone-start zone))
	 (nd (w3-zone-end zone))
	 (submit-it nil)
	 (formatfun (intern (concat "w3-form-format-" (downcase type)))))
    (if (not (equal "SUBMIT" type))
	(save-excursion
	  (if (not (fboundp formatfun))
	      (setq formatfun 'w3-form-format-unknown))
	  (if buffer-read-only (toggle-read-only))
	  (cond
	   ((equal "CHECKBOX" type) (setq chkd (not chkd)))
	   ((equal "RADIO" type) nil)
	   ((equal "TEXTAREA" type) nil)
	   ((equal "RESET" type) (w3-revert-form ident))
	   (t (setq valu
		    (w3-read-correct-format type name options ident valu))))
	  (cond
	   ((equal "RESET" type) nil)
	   ((equal "RADIO" type) (if (not chkd) (w3-set-radio-button zone)))
	   ((equal "TEXTAREA" type)
	    (if (not buffer-read-only) (toggle-read-only))
	    (w3-do-text-entry formobj zone))
	   (t
	    (w3-delete-zone zone)
	    (delete-region st nd)
	    (goto-char st)
	    (insert (funcall formatfun valu size chkd))
	    (w3-add-zone st nd w3-node-style
			 (list 'w3form actn type name deft valu chkd
			       size maxl ident options) t)
	    (if (not buffer-read-only) (toggle-read-only))
	    (if w3-running-FSF19 (setq w3-zones-list (w3-only-links)))))
	  (cond
	   ((string-match "^isindex$" name) (setq submit-it t))
	   ((string-match "^internal-gopher$" name) (setq submit-it 'gopher))))
      (w3-submit-form ident nil))
    (if submit-it (w3-submit-form ident submit-it))))

(defun w3-zones-matching (actn)
  "Return a list of data entry zones in form ACTN"
  (let* ((big (w3-all-zones))
	 (data nil)
	 (result nil)
	 (cur nil))
    (while big
      (setq data (w3-zone-data (car big))
	    cur (car big)
	    big (cdr big))
      (if (and
	   (eq (nth 0 data) 'w3form)
	   (equal (nth 9 data) actn))
	  (setq result (cons cur result))))
    result))

(defun w3-revert-form (actn)
  "Revert all values for form ACTN to their defaults"
  (save-excursion
    (let* ((zones (w3-zones-matching actn))
	   actn data type name deft valu chkd size maxl idnt strt end cur
	   options formatfun
	   )
      (if buffer-read-only (toggle-read-only))
      (while zones
	(setq data (w3-zone-data (car zones))
	      actn (nth 1 data)
	      type (nth 2 data)
	      name (nth 3 data)
	      deft (nth 4 data)
	      valu (nth 5 data)
	      chkd (nth 6 data)
	      size (nth 7 data)
	      maxl (nth 8 data)
	      idnt (nth 9 data)
	      options (nth 10 data)
	      cur (car zones)
	      strt (w3-zone-start cur)
	      end  (w3-zone-end cur)
	      zones (cdr zones)
	      formatfun (intern (concat "w3-form-format-" (downcase type))))
	(if (not (fboundp formatfun)) (setq formatfun 'w3-form-format-unknown))
	(cond
	 ((or (w3-member type '("SUBMIT" "TEXTAREA" "RESET"))) nil)
	 (t
	  (if (w3-member type '("RADIO" "CHECKBOX"))
	      (setq chkd deft)
	    (setq valu deft))
	  (if w3-running-FSF19 (goto-char strt)
	    (w3-delete-zone cur))
	  (delete-region strt end)
	  (goto-char strt)
	  (insert (funcall formatfun valu size chkd))
	  (w3-add-zone strt end w3-node-style
		       (list 'w3form actn type name deft valu chkd
			     size maxl idnt options) t))))
      (if (not buffer-read-only) (toggle-read-only)))
    (if w3-running-FSF19
	(setq w3-zones-list (w3-only-links)))))

(defun w3-form-encode (result &optional isindex-query)
  "Create a string suitably enocoded for a URL request from the form
data in RESULT."
  (let ((query ""))
    (cond
     ((eq isindex-query 'gopher)	; Gopher searching by hypertext
      (concat "\t" (nth 5 (car result))))
     (isindex-query			; Isindex handling by hypertext
      (w3-hexify-string (nth 5 (car result))))
     (t					; Normal submission of form
      (while result			; This is a little convoluted, but
					; gets only checkboxes that are
					; and ignores submit & reset buttons
	(if (and (not
		  (and (w3-member (nth 2 (car result)) '("CHECKBOX" "RADIO"))
		       (not (nth 6 (car result)))))
		 (not (w3-member (nth 2 (car result)) '("SUBMIT" "RESET"))))
	    (setq query (format "%s%s=%s&" query
				(nth 3 (car result))
				(w3-hexify-string (nth 5 (car result))))))
	(setq result (cdr result)))
      (substring query 0 -1)))))

(defun w3-form-encode-ask-block (result)
  "Submit a gopher ask block to the server."
  (let ((query ""))
    ;;; This is different than the w3-form-encode function, because
    ;;; gopher+ will expect all the checkboxes/etc, even if they are
    ;;; not turned on.  Should still ignore RADIO boxes that are not
    ;;; active though.
  (while result	      
    (if (and (not (and (string= (nth 2 (car result)) "RADIO")
		       (not (nth 6 (car result)))))
	     (not (w3-member (nth 2 (car result)) '("SUBMIT" "RESET"))))
	(setq query (format "%s\r\n%s" query (nth 5 (car result)))))
    (setq result (cdr result)))
  (concat query "\r\n.\r\n")))

(defun w3-submit-form (actn isindex)
  "Submit form entry fields matching ACTN as their action identifier."
  (let* ((result (mapcar 'w3-zone-data (w3-zones-matching actn)))
	 (query (w3-form-encode result isindex))
	 (themeth (upcase (or (car (nth 1 (car result))) "GET")))
	 (theurl (cdr (nth 1 (car result)))))
    (cond
     ((eq isindex 'gopher) (w3-fetch (concat theurl query)))
     ((string= "GOPHER-ASK" themeth)
      (setq query (w3-form-encode-ask-block result))
      (w3-fetch (concat theurl (w3-hexify-string (concat "\t+\t1\n+-1\r\n"
							 query)))))
     ((string= "POST" themeth) (w3-maybe-relative theurl "POST" query))
     ((string= "GET" themeth) (w3-maybe-relative (format "%s?%s" theurl query)
						 "GET" ""))
     (t (message "Unknown submit method: %s" themeth)))))

(defun w3-matching-radios (ext)
  "Return a list of all zones containing radio buttons with the same name
as that in EXT."
  (let* ((big (w3-all-zones))
	 (idnt (nth 9 (w3-zone-data ext)))
	 (name (nth 3 (w3-zone-data ext)))
	 data cur result)
    (while big
      (setq data (w3-zone-data (car big))
	    cur (car big)
	    big (cdr big))
      (if (and
	   (eq (nth 0 data) 'w3form)
	   (equal (nth 9 data) idnt)
	   (equal (nth 3 data) name))
	  (setq result (cons cur result))))
    result))
      
(defun w3-set-radio-button (ext)
  "Set the radio button at EXT to be on.  Will automatically
toggle other radio butons with the same name to be off."
  (save-excursion
    (let* ((result (w3-matching-radios ext))
	   (idnt (nth 9 (w3-zone-data ext)))
	   (name (nth 3 (w3-zone-data ext)))
	   actn type deft valu chkd size maxl strt end data options)
      (while result
	(setq data (w3-zone-data (car result))
	      actn (nth 1 data)
	      type (nth 2 data)
	      name (nth 3 data)
	      deft (nth 4 data)
	      valu (nth 5 data)
	      chkd (nth 6 data)
	      size (nth 7 data)
	      maxl (nth 8 data)
	      idnt (nth 9 data)
	      options (nth 10 data)
	      strt (w3-zone-start (car result))
	      end (w3-zone-end (car result)))
	(cond
	 ((and chkd (not (w3-zone-eq
			  ext (car result)))) ; Not supposed to be chkd
	  (w3-delete-zone (car result))	      ; but is.
	  (goto-char strt)
	  (delete-region strt end)
	  (setq chkd nil)
	  (insert (funcall 'w3-form-format-radio valu size chkd))
	  (w3-add-zone strt end w3-node-style
		       (list 'w3form actn type name deft valu chkd size maxl
			     idnt options) t))
	 ((and (not chkd) (w3-zone-eq
			   ext (car result))) ; Supposed to be chkd
	  (w3-delete-zone (car result))       ; but isn't.
	  (goto-char strt)
	  (delete-region strt end)
	  (setq chkd t)
	  (insert (funcall 'w3-form-format-radio valu size chkd))
	  (w3-add-zone strt end w3-node-style
		       (list 'w3form actn type name deft valu chkd size maxl
			     idnt options) t))
	 (t nil)) ; not supposed to be checked, and isn't
	(setq result (cdr result))))
    (if (not buffer-read-only) (toggle-read-only))
    (if w3-running-FSF19 (setq w3-zones-list (w3-only-links)))))
			
(provide 'w3-forms)
