;; Load this file to increment the recorded Emacs version number.
;; Copyright (C) 1985, 1986, 1992 Free Software Foundation, Inc.

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


(load "startup")
(set-default-load-path)

(insert-file-contents "../lisp/version.el")

(re-search-forward 
  "emacs-version \"[^\"0-9]*[0-9]+\\.[0-9]+\\(\\.\\([0-9]+\\)\\|\\)\\( (.*)\\)?\"")
(let ((version (if (= (match-beginning 1) (match-end 1))
		   (forward-char -1)
		 (goto-char (match-beginning 2))
		 (read (current-buffer)))))
  (widen)
  (if (null version)
      (insert ".1")
    (delete-region (match-beginning 2) (match-end 2))
    (prin1 (1+ version) (current-buffer))))
(skip-chars-backward "^\"")
(message "New Emacs version will be %s"
	 (buffer-substring (point)
			   (progn (skip-chars-forward "^\"") (point))))


(write-region (point-min) (point-max) "../lisp/version.el" nil 'nomsg)
(erase-buffer)
(set-buffer-modified-p nil)

(kill-emacs)
