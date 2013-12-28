;;; Highlighting message headers.
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

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
      (set-face-underline-p 'message-highlighted-header-contents t)))

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

(defvar highlight-headers-citation-header-regexp
  (concat "^In article\\|^In message\\|"
	  "^[^ \t].*\\(writes\\|wrote\\|said\\):\n[ \t]*[A-Z]*[]}<>|]")
  "*The pattern to match the prolog of a cited block.
Text in the body of a message which matches this will be displayed in
the `message-headers' face.")

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
  (let* ((current 'message-header-contents)
	 e p)
    ;; delete previous highlighting
    (map-extents (function (lambda (extent ignore)
			     (if (eq (extent-data extent) 'headers)
				 (delete-extent extent))))
		 (current-buffer) start end)
    (save-excursion
      (save-restriction
	(widen)
	;; take off signature
	(if hack-sig
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
	    (set-extent-data e 'headers)
	    (setq p (match-end 0))
	    (cond
	     ((and highlight-headers-regexp
		   (looking-at highlight-headers-regexp))
	      (setq current 'message-highlighted-header-contents)
	      (end-of-line)
	      (setq e (make-extent p (point)))
	      (set-extent-face e current)
	      (set-extent-data e 'headers)
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
;;	      (set-extent-data e 'headers)
;;	      (setq e (make-extent (match-beginning 1) (match-end 1)))
;;	      (set-extent-face e 'message-addresses)
;;	      (set-extent-data e 'headers)
;;	      (setq e (make-extent (match-end 1) (point)))
;;	      (set-extent-face e current)
;;	      (set-extent-data e 'headers)
;;	      )
	     (t
	      (setq current 'message-header-contents)
	      (end-of-line)
	      (setq e (make-extent p (point)))
	      (set-extent-face e current)
	      (set-extent-data e 'headers)
	      )))
	   (t
	    (setq p (point))
	    (end-of-line)
	    (setq e (make-extent p (point)))
	    (set-extent-face e current)
	    (set-extent-data e 'headers)
	    ))
	  (forward-line 1))

	(while (not (eobp))
	  (cond ((null highlight-headers-citation-regexp)
		 nil)
		((looking-at highlight-headers-citation-regexp)
		 (goto-char (match-end 0))
		 (or (save-excursion
		       (beginning-of-line)
		       (let ((case-fold-search nil)) ; aaaaah, unix...
			 (looking-at "^>From ")))
		     (setq current 'message-cited-text)))
;;		((or (looking-at "^In article\\|^In message")
;;		     (looking-at
;;	      "^[^ \t].*\\(writes\\|wrote\\|said\\):\n[ \t]+[A-Z]*[]}<>|]"))
;;		 (setq current 'message-headers))
		((null highlight-headers-citation-header-regexp)
		 nil)
		((looking-at highlight-headers-citation-header-regexp)
		 (setq current 'message-headers))
		(t (setq current nil)))
	  (cond (current
		 (setq p (point))
		 (end-of-line)
		 (setq e (make-extent p (point)))
		 (set-extent-face e current)
		 (set-extent-data e 'headers)
		 ))
	  (forward-line 1))))
    ))

(provide 'highlight-headers)
