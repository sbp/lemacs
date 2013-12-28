;; interactive adjust font size and face
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

(defconst energize-x-modify-font-regexp
  "-\\([^-]+-[^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)"
  "Regexpr to extract or modify font entries")

(defconst energize-font-families-parameters
  '(("courier" . ("adobe-courier" "o"))
    ("lucida" . ("b&h-lucida" "i"))
    ("openwindows" . energize-make-openwindow-font)
    ("helvetica" . ("adobe-helvetica" "o"))
    ("times" . ("adobe-times" "i"))
    ("clean" . ("shumacher-clean" "i"))))

(defun energize-x-set-font-name-entry (font-name entry value)
  (if (and font-name (string-match energize-x-modify-font-regexp font-name))
      (let ((match (substring font-name (match-beginning entry)  (match-end entry))))
	(concat (substring font-name 0 (match-beginning entry))
		value
		(substring font-name (match-end entry))))
    font-name))

(defun energize-x-set-font-entry (font entry value)
  (if font
      (let* ((font-name (if (stringp font) font (font-name font)))
	     (new-name (energize-x-set-font-name-entry font-name entry value)))
	(if (stringp font)
	    new-name
	  (make-font new-name)))))

(defun energize-x-set-face-font-entry (face entry value)
  "Sets the face font to be of the specified point size"
  (let* ((font (face-font face))
	 (new-font (energize-x-set-font-entry font entry value)))
    (and new-font
	 (condition-case a
	     (set-face-font face new-font)
	   (error (message (format "%S" a)) (sit-for 0))))))

(defun energize-set-font-size (size)
  (interactive "sSet new font size to: ")
  (mapcar '(lambda (face) (energize-x-set-face-font-entry face 7 size))
	  (list-faces)))

(defun energize-make-openwindow-font (font-name)
  (string-match energize-x-modify-font-regexp f-name)
  (let ((slant (substring f-name (match-beginning 3) (match-end 3))))
    (if (member slant '("i" "o"))
	(concat
	 (substring f-name 0 (match-beginning 1))
	 "b&h-lucida"
	 (substring f-name (match-end 1) (match-beginning 3))
	 "i"
	 (substring f-name (match-end 3)))
      (let ((new-name
	     (concat
	      (substring f-name 0 (match-beginning 1))
	      "b&h-lucidatypewriter"
	      (substring f-name (match-end 1) (match-beginning 3))
	      slant
	      (substring f-name (match-end 3)))))
	;; tries the R5 name first and the openwindows name second
	(if (x-list-fonts new-name)
	    new-name
	     (concat
	      (substring f-name 0 (match-beginning 1))
	      "b&h-lucida sans typewriter"
	      (substring f-name (match-end 1) (match-beginning 3))
	      slant
	      (substring f-name (match-end 3))))))))
    
(defun energize-set-font-family (family)
  (interactive "sSet new font family to: ")
  (let ((font-desc (cdr (assoc family energize-font-families-parameters)))
	(faces (list-faces)))
    (if (null font-desc)
	(error (format "Unknown font family %s, use one of %s" family
		       (mapcar 'car energize-font-families-parameters))))
    (while faces
      (let* ((face (car faces))
	     (font (face-font face))
	     (f-name (and font (font-name font))))
	(if f-name
	    (progn
	      (if (symbolp font-desc)
		  (setq f-name (funcall font-desc f-name))
		(string-match energize-x-modify-font-regexp f-name)
		(let ((slant (substring f-name
					(match-beginning 3) (match-end 3))))
		  (if (member slant '("i" "o"))
		      (setq slant (nth 1 font-desc)))
		  (setq f-name
			(concat
			 (substring f-name 0 (match-beginning 1))
			 (car font-desc)
			 (substring f-name (match-end 1) (match-beginning 3))
			 slant
			 (substring f-name (match-end 3))))))))
	(set-face-font face f-name))
      (setq faces (cdr faces)))))

(defun energize-set-font-boldness (bold)
  (interactive "sEnter boldness:")
  (let* ((default-name (font-name (face-font 'default)))
	 (default-boldness
	   (and (string-match energize-x-modify-font-regexp default-name)
		(substring default-name (match-beginning 2) (match-end 2)))))
    (if (not (equal bold default-boldness))
	(let ((faces (list-faces)))
	  (while faces
	    (let* ((face (car faces))
		   (font (face-font face))
		   (f-name (and font (font-name font))))
	      (if f-name
		  (progn
	      (string-match energize-x-modify-font-regexp f-name)
	      (let ((font-boldness
		     (substring f-name (match-beginning 2) (match-end 2))))
		(setq new-boldness
		      (if (equal font-boldness "bold")
			  "medium"
			"bold"))
		(setq f-name
		      (concat
		       (substring f-name 0 (match-beginning 2))
		       new-boldness
		       (substring f-name (match-end 2)))))
	      (set-face-font face f-name))))
	    (setq faces (cdr faces)))))))
