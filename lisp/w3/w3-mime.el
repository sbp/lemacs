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
;;; Support for HTTP/1.0 MIME messages                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

(defun w3-parse-mailcap (&optional fname)
  "Parse out the ~/.mailcap file"
  (let (x y z mjr)
    (if (not fname)
	(if (and (file-exists-p  (expand-file-name "~/.mailcap"))
		 (file-readable-p (expand-file-name "~/.mailcap")))
	    (setq fname (expand-file-name "~/.mailcap"))
	  (error "%s is non-existant or unreadable"
		 (expand-file-name "~/.mailcap"0))))
    (save-excursion
      (set-buffer (get-buffer-create " *mailcap*"))
      (erase-buffer)
      (insert-file-contents fname)
      (w3-replace-regexp "#.*" "")
      (w3-replace-regexp "\\\n+" "\\\n")
      (w3-replace-regexp "^$\\\n" "")
      (w3-replace-regexp "\\\\[ \\\t\\\n]+" " ")
      (sort-lines nil (point-min) (point-max))
      (while (re-search-forward
	      "\\(^[^\\\n/]+\\)/\\([^;]+\\);[ \\\t]*\\([^;\\\n]+\\).*"
	      nil t)
	(setq x (buffer-substring (match-beginning 1) (match-end 1))
	      y (buffer-substring (match-beginning 2) (match-end 2))
	      z (buffer-substring (match-beginning 3) (match-end 3))
	      mjr (assoc x w3-mime-viewers))
	(if (= ?' (string-to-char z))
	    (setq z (read (substring z 1 nil))))
	(if (null mjr)			; new major area
	    (progn
	      (setq w3-mime-viewers
		    (cons (cons x (list (cons y z))) w3-mime-viewers)))
	  (if (not (w3-in-assoc y (cdr mjr))) ; not in minor area either
	      (setcdr mjr (cons (cons y z) (cdr mjr)))))))))

(defun w3-mime-viewer (encoding)
  "Get the mime viewer command for ENCODING, return nil if none found.
Expects an argument like text/html, or application/dvi"
  (if (not encoding) (setq encoding "/"))
  (string-match "/" encoding)
  (let* ((major (substring encoding 0 (match-beginning 0)))
	 (minor (substring encoding (1+ (match-beginning 0)) nil))
	 (alist (cdr-safe (assoc major w3-mime-viewers)))
	 (viewr nil))
    (if alist
	(setq viewr (cdr-safe (w3-in-assoc minor alist))))
    viewr))

(defun w3-parse-viewer-types ()
  "Create a string usable for an Accept: header from w3-mime-viewere"
  (let ((tmp w3-mime-viewers)
	mjr mnr (str ""))
    (while tmp
      (setq mnr (cdr (car tmp))
	    mjr (car (car tmp))
	    tmp (cdr tmp))
      (while mnr
	(if (> (+ (% (length str) 60)
		  (length (concat ", " mjr "/" (car (car mnr))))) 60)
	    (setq str (format "%s\nAccept: %s/%s" str mjr
			      (if (string= ".*" (car (car mnr))) "*"
				(car (car mnr)))))
	  (setq str (format "%s, %s/%s" str mjr
			    (if (string= ".*" (car (car mnr))) "*"
			      (car (car mnr))))))
	(setq mnr (cdr mnr))))
    (substring str 2 nil)))
              
(defun w3-create-mime-request (url ref-url &optional meth data)
  "Create a MIME request for URL, referred to by REF-URL, using METH as the
method.  DATA is the body o the request."
  (if (and w3-use-http2
	   (not (w3-member w3-current-server w3-bad-server-list)))
      (let* ((tmp (w3-view-url t))
	     (request
	      (format
	       (concat
		"%s %s HTTP/1.0\n"			; The request
		"From: %s@%s\n"				; Who its from
		"Accept-encoding: x-compress; x-gzip\n"	; Encoding
		"%s%s"					; Authentication
		"Accept: %s\n"				; Accept-string
		"User-Agent: Emacs-W3/%s\n"		; User agent
		"%s"					; Any data
		"\n\n")					; End request
	       (if meth meth "GET")
	       url
	       (user-real-login-name)
	       (system-name)
	      (if (w3-basic-auth tmp)
		  (format "Authorization: Basic %s\n" (w3-basic-auth tmp))
		"")
	      (if (w3-pubkey-auth tmp)
		  (format "Authorization: PubKey %s\n" (w3-pubkey-auth tmp))
		"")
	      w3-mime-accept-string
	      w3-version-number
	      (if data
		  (format "Content-length: %d\n\n%s" (length data) data) "")
	      ref-url)))
	request)
    (format "GET %s\n" url)))

(defun w3-parse-mime-headers ()
  "Parse mime headers and remove them from the html"
  (set-buffer w3-working-buffer)
  (let* ((st (point-min))
	 (nd (progn
	       (goto-char (point-min))
	       (re-search-forward "^\r*$" nil t)
	       (1+ (point))))
	 (hdrs (buffer-substring st nd)))
    (delete-region st nd)
    (let (
	  status			; status field (200-500)
	  result			; Assoc-List of MIME headers
	  )
      (if (string-match "^HTTP.+ \\([0-9]+\\) *\\([^\\\n\\\r]*\\)\\\r*$" hdrs)
	  (setq status (string-to-int
			(substring hdrs (match-beginning 1) (match-end 1)))))
      (while (string-match "^\\([^\\\n\\\r:]+\\): *\\([^\\\n\\\r]*\\)\\\r*\\\n"
			   hdrs)
	(setq result (cons
		      (cons (downcase
			     (substring hdrs (match-beginning 1)
					(match-end 1)))
			    (substring hdrs (match-beginning 2) (match-end 2)))
		      result)
	      hdrs (substring hdrs (match-end 0) nil)))
      (setq w3-current-mime-type (cdr (assoc "content-type" result))
	    w3-current-mime-encoding (cdr (assoc "content-encoding" result))
	    w3-current-mime-viewer (w3-mime-viewer w3-current-mime-type)
	    w3-current-mime-headers result)
      (cond
       ((= status 500) nil)		; Internal server error
       ((= status 501) nil)		; Facility not supported
       ((= status 400) nil)		; Bad request - syntax
       ((= status 401)			; Unauthorized access, retry w/auth.
	(let ((x (intern
		  (format "w3-%s-auth"
			  (downcase
			   (cdr (assoc "WWW-Authenticate" result)))))))
	  (funcall x (w3-view-url t) t (funcall x (w3-view-url t)))
	  (w3-retrieve (w3-view-url t))))
       ((= status 402) nil)		; Payment required, retry w/Chargeto:
       ((= status 403) nil)		; Access is forbidden
       ((= status 404) nil)		; Not found...
       ((or (= status 301)		; Moved - retry with Location: header
	    (= status 302)		; Found - retry with Location: header
	    (= status 303))		; Method - retry with location/method
	(let ((x (w3-view-url t))
	      (redir (cdr (assoc "location" result)))
	      (redirmeth (or (cdr (assoc "method" result)) "GET")))
	  (if (not (equal x redir))
	      (w3-maybe-relative redir redirmeth)
	    (progn
	      (goto-char (point-max))
	      (insert "<HR>Error!  This URL tried to redirect me to itself!<P>"
		      "Please notify the server maintainer.")))))
       ((= status 204)			; No response - leave old document
	(kill-buffer w3-working-buffer))
       (t nil))				; All others indicate success
      hdrs)))

(defun w3-is-mime-response ()
  "Determine if the current buffer is a MIME response"
  (set-buffer w3-working-buffer)
  (if (and w3-use-telnet (equal w3-current-type "http"))
      (progn
	(w3-replace-regexp "^\\\r" "")
	(goto-char (point-min))
	(if (re-search-forward
	     (regexp-quote (w3-create-mime-request w3-current-file "")) nil t)
	    (progn
	      (replace-match "")
	      (goto-char (point-min))
	      (kill-line 1)))))
  (let* ((st (point-min))
	 (nd (progn
	       (goto-char (point-min))
	       (re-search-forward "^\r*$" nil t)
	       (if (not (eobp)) (1+ (point)) (point))))
	 (hdrs (buffer-substring st nd)))
    (and (string-match "^HTTP/.+" hdrs)
	 (equal w3-current-type "http")
	 (string-match "^MIME-version:.*" hdrs))))    

(provide 'w3-mime)
