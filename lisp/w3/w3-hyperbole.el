
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
;;; This file was written by Jin S. Choi (jsc@monolith.mit.edu) for w3.el
;;; Since I really don't have a clue about hyperbole yet, I will just
;;; forward all questions on this file to him. :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(autoload 'w3-follow-link "w3" "Follow a WWW link." t)
(autoload 'w3-fetch "w3" "Fetch the given URL." t)
(autoload 'gopher-directory-choose "gopher" "Choose a gopher item." t)


;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defib hwww:link-follow ()
  "When in a www buffer, follows a link with the primary button."
  (cond ((eq 'w3-mode major-mode)
	 (hact 'w3-follow-link))
	((eq 'gopher-directory-mode major-mode)
	 (beginning-of-line)
	 (if (search-forward-regexp "\\s-*\\([0-9]+\\)\\. "
				    (save-excursion (end-of-line) (point))
				    t)
	     (let ((dir (string-to-int (buffer-substring (match-beginning 1)
							 (match-end 1)))))
	       (hact 'gopher-directory-choose dir))))))

(defib hwww:url ()
  "Attempts to follow a URL."
  (w3-follow-url-at-point))


(defact hwww:start (url)
  "Starts a www session, and tries to load up the given URL."
  (interactive "sURL:")
  (or (stringp url)
      (error "(hwww:start): URL argument is not a string."))
  (if (string-match "none" url)
      (w3)
    (w3-fetch url)))


(provide 'hwww)
