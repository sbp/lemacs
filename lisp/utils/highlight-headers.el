;;; Highlighting message headers.
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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

;; This code is shared by RMAIL, VM, and GNUS.
;;
;; Faces:
;;
;; message-headers			the part before the colon
;; message-header-contents		the part after the colon
;; message-highlighted-header-contents	contents of "special" headers
;; message-cited-text			quoted text from other messages
;;
;; Variables:
;;
;; highlight-headers-regexp			what makes a "special" header
;; highlight-headers-citation-regexp		matches lines of quoted text
;; highlight-headers-citation-header-regexp	matches headers for quoted text


(make-face 'message-headers)
(make-face 'message-header-contents)
(make-face 'message-highlighted-header-contents)
(make-face 'message-cited-text)
;;(make-face 'message-addresses)

(or (face-differs-from-default-p 'message-headers)
    (copy-face 'bold 'message-headers))

(or (face-differs-from-default-p 'message-header-contents)
    (copy-face 'italic 'message-header-contents))

(or (face-differs-from-default-p 'message-highlighted-header-contents)
    (progn
      (copy-face 'message-header-contents
		 'message-highlighted-header-contents)
      ;; Most people seem not to like underlining, so change the font instead.
;;      (set-face-underline-p 'message-highlighted-header-contents t)
      (or (make-face-bold 'message-highlighted-header-contents)
	  (make-face-unbold 'message-highlighted-header-contents)
	  (make-face-italic 'message-highlighted-header-contents)
	  (make-face-unitalic 'message-highlighted-header-contents))
      ))

(or (face-differs-from-default-p 'message-cited-text)
    (copy-face 'italic 'message-cited-text))

;;(or (face-differs-from-default-p 'message-addresses)
;;    (progn
;;      (copy-face 'bold-italic 'message-addresses)
;;      (set-face-underline-p 'message-addresses
;;       (face-underline-p 'message-highlighted-header-contents))))


(defvar highlight-headers-regexp "Subject[ \t]*:"
  "*The headers whose contents should be emphasized more.
The contents of these headers will be displayed in the face 
`message-highlighted-header-contents' instead of `message-header-contents'.")

(defvar highlight-headers-citation-regexp "^[ \t]*[A-Z]*[]}<>|][ \t]*"
  "*The pattern to match cited text.
Text in the body of a message which matches this will be displayed in
the face `message-cited-text'.")

(defvar highlight-headers-highlight-citation-too nil
  "*Whether the whole citation line should go in the `mesage-cited-text' face.
If nil, the text matched by `highlight-headers-citation-regexp' is in the
default face, and the remainder of the line is in the message-cited-text face.")

(defvar highlight-headers-citation-header-regexp
  (concat "^In article\\|^In message\\|"
	  "^[^ \t].*\\(writes\\|wrote\\|said\\):\n[ \t]*[A-Z]*[]}<>|]")
  "*The pattern to match the prolog of a cited block.
Text in the body of a message which matches this will be displayed in
the `message-headers' face.")

(defvar highlight-headers-max-message-size 10000
  "*If the message body is larger than this many chars, don't highlight it.
This is to prevent us from wasting time trying to fontify things like
uuencoded files and large digests.  If this is nil, all messages will
be highlighted.")

(defvar highlight-headers-hack-x-face-p nil
  "*If true, then the bitmap in an X-Face header will be displayed
in the buffer.  This assumes you have the `uncompface' and `icontopbm'
programs on your path.")


;;;###autoload
(defun highlight-headers (start end hack-sig)
  "Highlight message headers between start and end.
Faces used:
  message-headers			the part before the colon
  message-header-contents		the part after the colon
  message-highlighted-header-contents	contents of \"special\" headers
  message-cited-text			quoted text from other messages

Variables used:

  highlight-headers-regexp			what makes a \"special\" header
  highlight-headers-citation-regexp		matches lines of quoted text
  highlight-headers-citation-header-regexp	matches headers for quoted text

If HACK-SIG is true,then we search backward from END for something that
looks like the beginning of a signature block, and don't consider that a
part of the message (this is because signatures are often incorrectly
interpreted as cited text.)"
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let* ((current 'message-header-contents)
	 (too-big (and highlight-headers-max-message-size
		       (> (- end start)
			  highlight-headers-max-message-size)))
	 e p)
    ;; delete previous highlighting
    (map-extents (function (lambda (extent ignore)
			     (if (extent-property extent 'headers)
				 (delete-extent extent))
			     nil))
		 (current-buffer) start end)
    (save-excursion
      (save-restriction
	(widen)
	;; take off signature
	(if (and hack-sig (not too-big))
	    (save-excursion
	      (goto-char end)
	      (if (re-search-backward "\n--+ *\n" start t)
		  (setq end (point)))))

	(narrow-to-region start end)

	(goto-char start)
	(while (and (not (eobp))
		    (not (= (following-char) ?\n)))
	  (cond
	   ((looking-at "^[^ \t\n]+[ \t]*:")
	    (setq e (make-extent (match-beginning 0) (match-end 0)))
	    (set-extent-face e 'message-headers)
	    (set-extent-property e 'headers t)
	    (setq p (match-end 0))
	    (cond
	     ((and highlight-headers-regexp
		   (looking-at highlight-headers-regexp))
	      (setq current 'message-highlighted-header-contents)
	      (end-of-line)
	      (setq e (make-extent p (point)))
	      (set-extent-face e current)
	      (set-extent-property e 'headers t)
	      )
;; I don't think this is worth the effort
;;	     ((looking-at "\\(From\\|Resent-From\\)[ \t]*:")
;;	      (setq current 'message-highlighted-header-contents)
;;	      (goto-char (match-end 0))
;;	      (or (looking-at ".*(\\(.*\\))")
;;		  (looking-at "\\(.*\\)<")
;;		  (looking-at "\\(.*\\)[@%]")
;;		  (looking-at "\\(.*\\)"))
;;	      (end-of-line)
;;	      (setq e (make-extent p (match-beginning 1)))
;;	      (set-extent-face e current)
;;	      (set-extent-property e 'headers t)
;;	      (setq e (make-extent (match-beginning 1) (match-end 1)))
;;	      (set-extent-face e 'message-addresses)
;;	      (set-extent-property e 'headers t)
;;	      (setq e (make-extent (match-end 1) (point)))
;;	      (set-extent-face e current)
;;	      (set-extent-property e 'headers t)
;;	      )
	     (t
	      (setq current 'message-header-contents)
	      (let ((x-face-p (and highlight-headers-hack-x-face-p
				   (looking-at "^X-Face: *"))))
		(end-of-line)
		(setq e (make-extent p (point)))
		(set-extent-face e current)
		(set-extent-property e 'headers t)
		(if x-face-p (highlight-headers-x-face p e))
		))))
	   (t
	    (setq p (point))
	    (end-of-line)
	    (setq e (make-extent p (point)))
	    (set-extent-face e current)
	    (set-extent-property e 'headers t)
	    ))
	  (forward-line 1))

	(if too-big
	    nil
	  (while (not (eobp))
	    (cond ((null highlight-headers-citation-regexp)
		   nil)
		  ((looking-at highlight-headers-citation-regexp)
		   (or highlight-headers-highlight-citation-too
		       (goto-char (match-end 0)))
		   (or (save-excursion
			 (beginning-of-line)
			 (let ((case-fold-search nil)) ; aaaaah, unix...
			   (looking-at "^>From ")))
		       (setq current 'message-cited-text)))
;;		  ((or (looking-at "^In article\\|^In message")
;;		       (looking-at
;;	      "^[^ \t].*\\(writes\\|wrote\\|said\\):\n[ \t]+[A-Z]*[]}<>|]"))
;;		   (setq current 'message-headers))
		  ((null highlight-headers-citation-header-regexp)
		   nil)
		  ((looking-at highlight-headers-citation-header-regexp)
		   (setq current 'message-headers))
		  (t (setq current nil)))
	    (cond (current
		   (setq p (point))
		   (forward-line 1) ; this is to put the \n in the face too
		   (setq e (make-extent p (point)))
		   (forward-char -1)
		   (set-extent-face e current)
		   (set-extent-property e 'headers t)
		   ))
	    (forward-line 1)))))
    ))


;;; Kludge kludge kludge for displaying the bitmap in the X-Face header.

;;; This depends on the following programs: icontopbm, from the pbmplus
;;; toolkit (available everywhere) and uncompface, which comes with
;;; several faces-related packages, and can also be had at ftp.clark.net
;;; in /pub/liebman/compface.tar.Z.  See also xfaces 3.*.  Not needed
;;; for this, but a very nice xbiff replacment.

(defconst highlight-headers-x-face-bitrev
  (purecopy
   (eval-when-compile
     (let* ((v (make-string 256 0))
	    (i (1- (length v))))
       (while (>= i 0)
	 (let ((j 7)
	       (k 0))
	   (while (>= j 0)
	     (if (/= 0 (logand i (lsh 1 (- 7 j))))
		 (setq k (logior k (lsh 1 j))))
	     (setq j (1- j)))
	   (aset v i k))
	 (setq i (1- i)))
       v))))

(defun highlight-headers-parse-x-face-data (start end)
  (save-excursion
    (let ((b (current-buffer))
	  (lines 0)
	  p)
      (set-buffer (get-buffer-create " *x-face-tmp*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (if (stringp start)
	  (insert start)
	(insert-buffer-substring b start end))
      (while (search-forward "\n" nil t)
	(skip-chars-backward " \t\n")
	(setq p (point))
	(skip-chars-forward " \t\n")
	(delete-region p (point)))
      (call-process-region (point-min) (point-max) "uncompface" t t nil)
      (goto-char (point-min))
      (while (not (eobp))
	(or (looking-at "0x....,0x....,0x...., *$")
	    (error "unexpected uncompface output"))
	(forward-line 1)
	(setq lines (1+ lines))
	(delete-char -1))
      (goto-char (point-min))
      (insert (format "/* Format_version=1, Width=%d, Height=%d" lines lines))
      (insert ", Depth=1, Valid_bits_per_item=16\n */\n")
      (while (not (eobp))
	(insert ?\t)
	(forward-char 56) ; 7 groups per line
	(insert ?\n))
      (forward-char -1)
      (delete-char -1)  ; take off last comma
      ;;
      ;; Ok, now we've got the format that "icontopbm" knows about.
      (call-process-region (point-min) (point-max) "icontopbm" t t nil)
      ;;
      ;; If PBM is using binary mode, we're winning.
      (goto-char (point-min))
      (cond ((looking-at "P4\n")
	     (forward-line 2)
	     (delete-region (point-min) (point))
	     (while (not (eobp))
	       (insert (aref highlight-headers-x-face-bitrev (following-char)))
	       (delete-char 1))
	     (make-pixmap (list lines lines
				(prog1 (buffer-string) (erase-buffer)))))
	    (t ; fix me
	     (error "I only understand binary-format PBM..."))))))

(defvar highlight-headers-x-face-to-pixmap-cache nil)

(defun highlight-headers-x-face-to-pixmap (start end)
  (let* ((string (if (stringp start)
		     start
		   (buffer-substring start end)))
	 (data (assoc string highlight-headers-x-face-to-pixmap-cache)))
    (if data
	(cdr data)
      (setq data (cons string
		       (condition-case c
			   (highlight-headers-parse-x-face-data start end)
			 (error
			  (display-error c nil)
			  (sit-for 2)
			  nil))))
      (setq highlight-headers-x-face-to-pixmap-cache
	    (cons data highlight-headers-x-face-to-pixmap-cache))
      (cdr data))))

(defun highlight-headers-x-face (start extent)
  (let* ((end (if (stringp start)
		  nil
		(save-excursion
		  (goto-char start)
		  (forward-line 1)
		  (while (looking-at "[ \t]")
		    (forward-line 1))
		  (1- (point)))))
	 (p (highlight-headers-x-face-to-pixmap start end)))
    ;; Once the `invisible' attribute works, using that to hide
    ;; the original contents of the header would be nice.
    (if p (set-extent-begin-glyph extent p))))


(provide 'highlight-headers)
