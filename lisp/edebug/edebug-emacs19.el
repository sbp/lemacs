;; edebug-emacs19.el  -*- Syntax: Emacs-Lisp; Mode: emacs-lisp -*- ;; 
;; Menus for using Edebug with Emacs version 19.

;; Use this by adding the following to your .emacs file:
;; (add-hook 'edebug-setup-hook 
;;	  (function (lambda () (load-library "edebug-emacs19"))))

;; define-menus may be a good function for defining menus in emacs 19.  
;; Please send me any improvements.

(provide 'edebug-emacs19)

(defun define-menus (keymap menu menus)
  "Define menus for a KEYMAP starting at MENU adding all the MENUS.
Each of the MENUS is of the form (NAME TITLE MENU-ITEMS).
The NAME is vconcatted to the MENU.
The MENUS and MENU-ITEMS are all given in the order they should appear."
  (mapcar 
   (function
    (lambda (menu-spec)
      (apply 
       (function
	(lambda (name title menu-items)
	  (edebug-define-menu keymap
			      (vconcat menu (list name))
			      title menu-items)))
       menu-spec)))
   (nreverse menus)))


(defun edebug-define-menu (keymap menu title menu-items)
  "Define KEYMAPs MENU, titled TITLE, with MENU-ITEMS under it.
Append the name of the new menu in MENU after the parent menu.  TITLE
is used for both the menu item in MENU and as the heading for the
MENU-ITEMS.  MENU-ITEMS is a list of items in the order they should
appear.  Each menu item is a list with the title, function, and
optional item name."
  (let ((submenu (make-sparse-keymap title))
	(menus (nreverse menu-items)))
    (define-key keymap menu (cons title submenu))
    (mapcar (function
	     (lambda (item)
	       (apply 
		(function 
		 (lambda (title function &optional name)
		   (define-key submenu (vector (or name function))
		     (cons title function))
		   ))
		item)))
	    menus)))


(defun edebug-toggle (variable)
  (set variable (not (eval variable)))
  (message "%s: %s" variable (eval variable)))

(define-menus edebug-mode-map [menu-bar]
  '((edebug "Edebug"
	    (("Help" edebug-help)
	     ("Stop" edebug-stop)
	     ("Step" edebug-step-mode)
	     ("Next" edebug-next-mode)
	     ("Trace" edebug-trace-mode)
	     ("Trace Fast" edebug-Trace-fast-mode)
	     ("Continue" edebug-continue-mode)
	     ("Continue Fast" edebug-Continue-fast-mode)
	     ("Go" edebug-go-mode)
	     ("Go Nonstop" edebug-Go-nonstop-mode)
	     ("Abort" abort-recursive-edit)
	     ("Quit to Top Level"  top-level)
	     ("Quit Nonstop" edebug-top-level-nonstop)
	     ))

    (jumps "Jumps"
	   (("Forward Sexp" edebug-forward-sexp)
	    ("Step In" edebug-step-in)
	    ("Step Out" edebug-step-out)
	    ("Goto Here" edebug-goto-here)))

    (breaks "Breaks"
	    (("Set Breakpoint" edebug-set-breakpoint)
	     ("Unset Breakpoint" edebug-unset-breakpoint)
	     ("Set Conditional Breakpoint" edebug-set-conditional-breakpoint)
	     ("Set Global Break Condition" edebug-set-global-break-condition)
	     ("Show Next Breakpoint" edebug-next-breakpoint)))

    (views "Views"
	   (("Where am I?" edebug-where)
	    ("Bounce to Current Point" edebug-bounce-point)
	    ("View Outside Windows" edebug-view-outside)
	    ("Previous Result" edebug-previous-result)
	    ("Show Backtrace" edebug-backtrace)
	    ("Display Freq Count" edebug-display-freq-count)))

    (eval "Eval"
	  (("Expression" edebug-eval-expression)
	   ("Last Sexp" edebug-eval-last-sexp)
	   ("Visit Eval List" edebug-visit-eval-list)))

    (options "Options"
	     (("Edebug All Defs" edebug-all-defs)
	      ("Edebug All Forms" edebug-all-forms)

	      ("Trace Enter and Exit" 
	       (function 
		(lambda () 
		  (interactive)
		  (edebug-toggle 'edebug-trace))) tracing)

	      ("Test Coverage" 
	       (function 
		(lambda () 
		  (interactive) 
		  (edebug-toggle 'edebug-test-coverage))) coverage)

	      ("Toggle Window Saving" edebug-toggle-save-windows)

	      ("Save Displayed Buffer Points" 
	       (lambda () (interactive) 
		 (edebug-toggle 'edebug-save-displayed-buffer-points))
	       save-displayed-buffer-points)
	      ))
    ))

