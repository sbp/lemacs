;;; -*-Emacs-Lisp-*-
;;; Rcs_Info: ilisp-aut.el,v 1.21 1993/09/03 02:05:07 ivan Rel $
;;;%Header
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;
;;;


;;;
;;; ILISP autoloads
;;;
(autoload 'lisp-directory "ilisp-src" 
	  "Select directories to search." t)
(autoload 'next-definition-lisp "ilisp-src"
	  "Edit the next definition." t)
(autoload 'edit-definitions-lisp "ilisp-src" 
	  "Edit definitions." t)
(autoload 'search-lisp "ilisp-src" 
	  "Search for pattern in source files." t)
(autoload 'replace-lisp "ilisp-src" 
	  "Relace pattern in source files." t)
(autoload 'who-calls-lisp "ilisp-src"
	  "Show callers of a function." t)
(autoload 'next-caller-lisp "ilisp-src" 
	  "Edit the next caller of a function." t)
(autoload 'edit-callers-lisp "ilisp-src" 
	  "Edit the callers of a function." t)

(autoload 'ilisp-bug "ilisp-bug"
	  "Send a mail message about a bug." t)

;;;%%Changed definitions
(autoload 'mark-change-lisp "ilisp-bat" 
	  "Mark the current defun as changed." t)
(autoload 'list-changes-lisp "ilisp-bat"
	  "List the current LISP changes." t)
(autoload 'clear-changes-lisp "ilisp-bat"
	  "Clear the list of LISP changes." t)
(autoload 'eval-changes-lisp "ilisp-bat"
	  "Evaluate the list of LISP changes." t)
(autoload 'compile-changes-lisp "ilisp-bat"
	  "Compile the list of LISP changes." t)


(provide 'ilisp-aut)
