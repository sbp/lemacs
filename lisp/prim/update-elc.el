;; Byte compile the .EL files necessary to dump out xemacs.
;; Use this file like this:
;;
;; temacs -batch -l ../lisp/prim/update-elc.el $lisp
;;
;; where $lisp comes from the Makefile.  .elc files listed in $lisp will
;; cause the corresponding .el file to be compiled.  .el files listed in
;; $lisp will be ignored.
;;
;; (the idea here is that you can bootstrap if your .ELC files
;; are missing or badly out-of-date)

(setq update-elc-files-to-compile
      (delq nil
	    (mapcar (function
		     (lambda (x)
		       (if (string-match "\.elc$" x)
			   (let ((src (substring x 0 -1)))
			     (if (file-newer-than-file-p src x)
				 src)))))
		    ; -batch gets filtered out.
		    (nthcdr 3 command-line-args))))

(if update-elc-files-to-compile
    (progn
      (setq command-line-args
	    (cons (car command-line-args)
		  (append '("-l" "loadup-el.el" "run-temacs"
			    "-batch" "-q" "-no-site-file" "-f"
			    "batch-byte-compile")
			  update-elc-files-to-compile)))
      (load "loadup-el.el")))

(kill-emacs)
