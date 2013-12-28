;; interactive turn on and off of the font-lock-mode
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

(defun energize-reset-face-to-resources (face)
  "Reset FACE to the parameters defined in the resources.
Return T if FACE is not different than the default face"
  (set-face-font face ())
  (set-face-foreground face ())
  (set-face-underline-p face ())
  (set-face-background face ())
  (set-face-background-pixmap face ())
  (x-resource-face face)
  (face-differs-from-default-p face))

(defun energize-font-lock-copy-face (from to)
  (energize-reset-face-to-resources to)
  (if (face-differs-from-default-p to)
      (message (format "Warning: X resources override default for %s" to))
    (copy-face from to)))

(defun energize-font-lock-set-foreground (color to)
  (energize-reset-face-to-resources to)
  (if (face-differs-from-default-p to)
      (message (format "Warning: X resources override default for %s" to))
    (set-face-foreground to color)))

(defun energize-font-lock-use-fonts ()
  "Make the font-lock-faces be using fonts"
  (energize-font-lock-copy-face 'italic 'font-lock-comment-face)
  (energize-font-lock-copy-face 'font-lock-comment-face
				'font-lock-string-face)
  (energize-font-lock-copy-face 'font-lock-string-face
				'font-lock-doc-string-face)
  (energize-font-lock-copy-face 'bold-italic 'font-lock-function-name-face)
  (energize-font-lock-copy-face 'bold 'font-lock-keyword-face)
  (energize-font-lock-copy-face 'italic 'font-lock-type-face)
  (remove-hook 'font-lock-mode-hook 'energize-font-lock-use-fonts))
  
(defun energize-font-lock-use-colors ()
  "Make the font-lock-faces be using colors"
  (energize-font-lock-set-foreground "#6920ac" 'font-lock-comment-face)
  (energize-font-lock-set-foreground "green4" 'font-lock-string-face)
  (energize-font-lock-set-foreground "green4" 'font-lock-doc-string-face)
  (energize-font-lock-set-foreground "red3" 'font-lock-function-name-face)
  (energize-font-lock-set-foreground "blue3" 'font-lock-keyword-face)
  (energize-font-lock-set-foreground "blue3" 'font-lock-type-face)
  (remove-hook 'font-lock-mode-hook 'energize-font-lock-use-colors))

(defun energize-font-lock-decorate-less ()
  "Decorate only with the basic keywords"
  (setq lisp-font-lock-keywords lisp-font-lock-keywords-1)
  (setq c-font-lock-keywords c-font-lock-keywords-1)
  (setq c++-font-lock-keywords c-font-lock-keywords-1)
  (remove-hook 'font-lock-mode-hook 'energize-font-lock-decorate-less))

(defun energize-font-lock-decorate-more ()
  "Decorate only with the extended keywords"
  (setq lisp-font-lock-keywords lisp-font-lock-keywords-2)
  (setq c-font-lock-keywords c-font-lock-keywords-2)
  (setq c++-font-lock-keywords c-font-lock-keywords-2)
  (remove-hook 'font-lock-mode-hook 'energize-font-lock-decorate-more))

(defun energize-font-lock-mode-on ()
  (font-lock-mode 1))

(defun energize-font-lock-mode-off ()
  (font-lock-mode -1))

(defvar energize-font-lock-on-p t)
(defvar energize-font-lock-font-p t)
(defvar energize-font-lock-less-p t)

(defun energize-configure-font-lock-mode (on-p font-p less-p)
  "Configure the font-lock-mode.  This is to be called in the .emacs file"
  (setq energize-font-lock-on-p on-p)
  (setq energize-font-lock-font-p font-p)
  (setq energize-font-lock-less-p less-p)
  (cond (on-p
	 (add-hook 'c++-mode-hook 'energize-font-lock-mode-on)
	 (add-hook 'c-mode-hook 'energize-font-lock-mode-on)
	 (add-hook 'emacs-lisp-mode-hook 'energize-font-lock-mode-on)
	 (add-hook 'lisp-mode-hook 'energize-font-lock-mode-on))
	(t
	 (remove-hook 'c++-mode-hook 'energize-font-lock-mode-on)
	 (remove-hook 'c-mode-hook 'energize-font-lock-mode-on)
	 (remove-hook 'emacs-lisp-mode-hook 'energize-font-lock-mode-on)
	 (remove-hook 'lisp-mode-hook 'energize-font-lock-mode-on)))
  (cond (font-p
	 (remove-hook 'font-lock-mode-hook 'energize-font-lock-use-colors)
	 (add-hook 'font-lock-mode-hook 'energize-font-lock-use-fonts))
	(t
	 (remove-hook 'font-lock-mode-hook 'energize-font-lock-use-fonts)
	 (add-hook 'font-lock-mode-hook 'energize-font-lock-use-colors)))
  (cond (less-p
	 (remove-hook 'font-lock-mode-hook 'energize-font-lock-decorate-more)
	 (add-hook 'font-lock-mode-hook 'energize-font-lock-decorate-less))
	(t
	 (remove-hook 'font-lock-mode-hook 'energize-font-lock-decorate-less)
	 (add-hook 'font-lock-mode-hook 'energize-font-lock-decorate-more))))

(defun energize-memorize-font-lock-configuration (on-p font-p less-p)
  "Store in the .emacs the command to configure font-lock-mode"
  (energize-configure-font-lock-mode on-p font-p less-p)
  (let ((pattern "(and (fboundp 'energize-configure-font-lock-mode)\n     (energize-configure-font-lock-mode %s %s %s))"))
    (save-excursion
      (set-buffer (find-file-noselect (substitute-in-file-name "~/.emacs")))
      (goto-char (point-min))
      (if (re-search-forward (format pattern "[a-z]*" "[a-z]*" "[a-z]*") nil t)
	  (delete-region (match-beginning 0 ) (match-end 0))
	;; Must have been disabled by default.
	(goto-char (point-max)))
      (insert
       (format (format pattern on-p font-p less-p)))
      (save-buffer))))
	 

;;; Menubar interface
(defun energize-menu-no-decorations ()
  ""
  (interactive)
  (setq energize-font-lock-on-p ())
  (energize-memorize-font-lock-configuration ()
					     energize-font-lock-font-p
					     energize-font-lock-less-p)
  (font-lock-mode -1))

(defun energize-menu-font-decorations ()
  ""
  (interactive)
  (setq energize-font-lock-on-p ())
  (energize-memorize-font-lock-configuration t
					     t
					     energize-font-lock-less-p)
  (energize-font-lock-use-fonts)
  (font-lock-mode 1))

(defun energize-menu-color-decorations ()
  ""
  (interactive)
  (energize-memorize-font-lock-configuration t
					     ()
					     energize-font-lock-less-p)
  (energize-font-lock-use-colors)
  (font-lock-mode 1))

(defun energize-menu-less-decorations ()
  ""
  (interactive)
  (energize-memorize-font-lock-configuration t
					     energize-font-lock-font-p
					     t)
  (energize-font-lock-decorate-less)
  (font-lock-mode -1)
  (font-lock-mode 1))

(defun energize-menu-more-decorations ()
  ""
  (interactive)
  (energize-memorize-font-lock-configuration t
					     energize-font-lock-font-p
					     ())
  (energize-font-lock-decorate-more)
  (font-lock-mode -1)
  (font-lock-mode 1))

(delete-menu-item '("Browse" "Syntax Highlighting"))
(add-menu '("Browse") "Syntax Highlighting"
	  '(["None" energize-menu-no-decorations t]
	    ["Fonts" energize-menu-font-decorations t]
	    ["Colors" energize-menu-color-decorations t]
	    "----"
	    ["Less" energize-menu-less-decorations t]
	    ["More" energize-menu-more-decorations t]))

(defun energize-font-lock-sensitize-menu ()
  (let* ((rest (cdr (car
		     (find-menu-item current-menubar
				     '("Browse" "Syntax Highlighting")))))
	 (case-fold-search t)
	 item
	 command)
    (while rest
      (setq item (car rest))
      (if (not (vectorp item))
	  nil
	(setq command (aref item 1))
	(cond ((eq command 'energize-menu-no-decorations)
	       (if (null energize-font-lock-on-p)
		   (aset item 0 "None     ×")
		 (aset item 0 "None")))
	      ((eq command 'energize-menu-font-decorations)
	       (if (and energize-font-lock-on-p energize-font-lock-font-p)
		   (aset item 0 "Fonts     ×")
		 (aset item 0 "Fonts")))
	      ((eq command 'energize-menu-color-decorations)
	       (if (and energize-font-lock-on-p
			(null energize-font-lock-font-p))
		   (aset item 0 "Colors     ×")
		 (aset item 0 "Colors")))
	      ((eq command 'energize-menu-less-decorations)
	       (if energize-font-lock-less-p
		   (aset item 0 "Less     ×")
		 (aset item 0 "Less")))
	      ((eq command 'energize-menu-more-decorations)
	       (if (not energize-font-lock-less-p)
		   (aset item 0 "More     ×")
		 (aset item 0 "More")))))
      (setq rest (cdr rest)))
    nil))

(add-hook 'activate-menubar-hook 'energize-font-lock-sensitize-menu)

;; turn the default set-up on
(energize-configure-font-lock-mode t t t)
