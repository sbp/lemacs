;; dired-trns.el - file transformers for dired shell commands.

;; Id: dired-trns.el,v 1.6 1991/07/05 13:36:01 sk RelBeta 

;; Code contributed by Hans Chalupsky <hans@cs.Buffalo.EDU>.
;; Integrated with my dired.el sk@sparc0 11-Jan-1991 14:38.
;; And hacked up a bit.

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    dired-trns|Hans Chalupsky|hans@cs.Buffalo.EDU
;;    |Filename Transformation for Tree Dired Shell Commands 
;;    |Date: 1991/07/05 13:36:01 |Revision: 1.6 |

;; INSTALLATION ======================================================
;; Put this file into your load-path and add (load "dired-trns") to
;; your dired-load-hook, e.g.
;;
;; (setq dired-load-hook '(lambda ()
;; 			  ;; possibly more statements here
;;			  (load "dired-trns")))

;; Transformers are functions that take a file (a string) as an argument
;; and transform it into some other string (e.g., a filename without an
;; extension).
;;
;; Each transformer is associated with a dispatch character. The associations
;; are stored in a keymap for fast and easy lookup. The dispatch character
;; is used to activate the associated transformer function at a particular
;; position in a shell command issued in dired.
;;
;; Transformers can be used to construct complicated shell commands that
;; operate on a large number of files, for example, they allow to create
;; functionality such as "mv *.lsp *.lisp" where each .lsp file is
;; renamed into a a file with same name but new extension .lisp.

(defvar dired-trans-map (make-keymap)
  "Array that associates keys with file transformer functions")

(defmacro dired-trans-define (char &rest body)
  "Macro that assigns the transformer function (lambda (file) BODY) to 
CHAR (a character or string). BODY must return a string (the transformed
file or whatever. This macro allows easy definition of user specific
transformation functions."
  (if (not (stringp char)) (setq char (char-to-string char)))
  (list 'define-key 'dired-trans-map char
	(list 'function (append '(lambda (file)) body))))

(defun dired-trans-run (transformers file)
  "Applies each transformer supplied in the string TRANSFORMERS in sequence
to FILE and returns the concatenation of the results."
  (mapconcat (function
	      (lambda (transformer)
		(setq transformer (char-to-string transformer))
		(funcall (or (lookup-key dired-trans-map transformer)
			     (error "Undefined transfomer: %s" transformer))
			 file)))
	     transformers nil))

(defvar dired-trans-re-ext "\\.[^.]*\\(\\.\\(\\(g?z\\)\\|Z\\)\\)?$"
  "The part of a filename matching this regexp will be viewed as extension")

(defun dired-trans-init ()
  "Defines a basic set of useful transformers.

*  is a noop that returns the unmodified filename (equivalent to [dbe]).
n  returns the Name component of a filename without directory information
d  returns the Directory component of a filename
b  returns the Basename of a filename, i.e., the name of the file without
   directory and extension (see dired-trans-re-ext)
   A basename with directory component can be obtained by [db].
e  returns the Extension of a filename (i.e., whatever
   dired-trans-re-ext splits off)
v  returns a file without directory and without ,v suffixes if any.
z  returns a file without directory and without .Z .z .gz suffixes if any."
  (dired-trans-define
   "*" file)
  (dired-trans-define
   "n" (or (file-name-nondirectory file) ""))
  (dired-trans-define
   "d" (or (file-name-directory file) ""))
  (dired-trans-define
   "b" (setq file (dired-trans-run "n" file))
       (substring file 0 (string-match dired-trans-re-ext file)))
  (dired-trans-define
   "e" (let ((e (string-match dired-trans-re-ext file)))
	 (if e
	     (substring file e)
	   "")))
  (dired-trans-define
   "v" (setq file (dired-trans-run "n" file))
       (substring file 0 (string-match ",v$" file)))
  (dired-trans-define
   "z" (setq file (dired-trans-run "n" file))
       (substring file 0 (string-match "\\.\\(\\(g?z\\)\\|Z\\)$" file)))
  )

(dired-trans-init)

(defun dired-trans-mklist (files &optional transformers)
  "Takes a list of FILES and applies the sequence of TRANSFORMERS to each
of them. The transformed results are concatenated, separated by 
dired-mark-separator, prefixed by dired-mark-prefix and postfixed by
dired-mark-postfix to generate a file list suitable for a particular shell."
  (if (not (consp files))(setq files (list files)))
  (if (null transformers) (setq transformers "*"))
  (let ((file-list
	 (mapconcat (function
		     (lambda (file)
		       (shell-quote
			(dired-trans-run transformers file))))
		    files dired-mark-separator)))
    (if (> (length files) 1)
	(concat dired-mark-prefix file-list dired-mark-postfix)
      file-list)))

;; By default, transformations can be specified like this:
;; [db] or [dv] or #z# or #dbe# or #dbe  (blank at the end).
    
(defvar dired-trans-starters "[#[]"
  "User definable set of characters to be used to indicate the start of a
transformer sequence")

(defvar dired-trans-enders "[]# ]"
  "User definable set of characters to be used to indicate the end of a
transformer sequence")

(defun dired-trans-expand (command files)
  "Takes a shell COMMAND and a list of FILES and substitutes each occurance
of a transformer sequence by an accordingly transformed file list. Special
characters such as [,] or * can be quoted with a backslash."
  (let ((quoted nil)
	(collect-transformers nil)
	(transformers ""))
    (mapconcat (function
		(lambda (char)
		  (setq char (char-to-string char))
		  (cond (quoted (setq quoted nil) char)
			((equal char "\\") (setq quoted t) nil)
			(collect-transformers
			 (cond ((string-match dired-trans-enders char)
				(setq collect-transformers nil)
				(prog1 (dired-trans-mklist
					files transformers)
				  (setq transformers "")))
			       (t (setq transformers
					(concat transformers char))
				  nil)))
			((string-match dired-trans-starters char)
                          (setq collect-transformers t) nil)
			;; for compatibility and as a special case that should
			;; not be redefinable by the user (used below)
			((equal char "*")
			 (dired-trans-mklist files "*"))
			(t char))))
	       command nil)))

(defun dired-trans-make (command files &optional all-at-once)
  "Takes a shell COMMAND and a list of FILES and returns a command operating
on the list of files (transformed if COMMAND contains transformers). If
ALL-AT-ONCE is t the resulting command will be of the form
  cmd file1 file2 ... fileN
otherwise it will be
  cmd file1; cmd file2; ... cmd fileN;
Both examples assume a single reference to the file list."
  (let (fns expanded-command)
    (cond (all-at-once
	   (setq expanded-command (dired-trans-expand command files))
	   (if (equal command expanded-command)
	       (concat command (dired-trans-expand " *" files))
	       expanded-command))
	  (t (mapconcat
	      (function
	       (lambda (file)
		 (dired-trans-make command file t)))
	      files ";")))))

;; Redefine this function from dired.el:

(defun dired-shell-stuff-it (command file-list on-each &optional raw-arg)
"Make up a shell command line from COMMAND and FILE-LIST.
If ON-EACH is t, COMMAND should be applied to each file, else
  simply concat all files.
The list of marked files is appended to the command string unless asterisks
  `*' or transformer sequences enclosed in `[]' indicate the place(s) where 
  the (transformed) list should go.  See documentation of function
  dired-trans-init for a list of transformers.
With a zero argument the resulting command will be of the form
  cmd file1; cmd file2; ... cmd fileN assuming only one reference to the
  file list. E.g., to rename all .lsp files into .lisp files mark all the
  .lsp files and issue the command `mv * [db].lisp' ."
  (dired-trans-make command file-list (not on-each)))
