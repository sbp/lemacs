;;;; dired-cwd.el - Fix a command's current working directory in Tree Dired.

(defconst dired-cwd-version (substring "!Revision: 1.2 !" 11 -2)
  "!Id: dired-cwd.el,v 1.2 1991/10/08 15:31:28 sk RelBeta !")

;; Copyright (C) 1991 by Sebastian Kremer <sk@thp.uni-koeln.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    dired-cwd|Sebastian Kremer|sk@thp.uni-koeln.de
;;    |Fix a command's current working directory in Tree Dired. 
;;    |Date: 1991/10/08 15:31:28 |Revision: 1.2 |

;; INSTALLATION ======================================================
;; 
;; Put this file into your load-path and the following in your ~/.emacs:
;; 
;;   (autoload 'dired-cwd-make-magic "dired-cwd")
;;
;; You have to load dired-x.el in your dired-load-hook to define
;; function default-directory, or you will not benefit from this
;; package: as long as function default-directory is not defined, the
;; functions wrapped by dired-cwd-make-magic will behave as before.

;; EXAMPLE USAGE ======================================================
;;
;; How to fix M-x compile (and grep) to know about Tree Dired's multiple
;; working directories by putting the following lines into your ~/.emacs:
;; 
;;    (require 'compile)
;;    (dired-cwd-make-magic 'compile1)
;;
;; After that, a compilation or grep started in a subdirectory in a
;; Dired buffer will have that subdirectory as working directory.
;;
;; Note you must require 'compile as function compile1 is redefined.
;; You could use a load hook instead by adding the line
;;
;;     (run-hooks 'compile-load-hook)
;;     
;; at the end of compile.el and setting
;;
;;    (setq compile-load-hook '(lambda () (dired-cwd-make-magic 'compile1)))
;;
;; in your ~/.emacs.


;;;###autoload
(defun dired-cwd-make-magic (function)
  "Modify COMMAND so that it's working directory is the current dired directory.
This works by binding `default-directory' to `(default-directory)'s value.
See also function `default-directory'."
  (interactive "aMake work with tree dired (function): ")
  (if (commandp function)
      (error "Cannot make interactive functions work for tree dired"))
  (let ((save-name (intern (concat "dired-cwd-wrap-real-" (symbol-name
							   function))))
	new-function)
    (setq new-function
	  (` (lambda (&rest dired-cwd-args)
	       ;; Name our formal args unique to avoid shadowing
	       ;; through dynamic scope.
	       (let ((default-directory
		       (if (fboundp 'default-directory)
			   ;; This is defined in dired-x.el, but dired
			   ;; may not yet be loaded.
			   (default-directory)
			 default-directory)))
		 (apply 'funcall (quote (, save-name)) dired-cwd-args)))))
    (or (fboundp save-name)
	(fset save-name (symbol-function function)))
    (fset function new-function)))
