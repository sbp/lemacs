;;; -*- Mode:Emacs-Lisp; Blat:Foop -*-

;;; conx.el: Yet Another Dissociator.
;;; Original design by Skef Wholey <skef@cs.cmu.edu>;
;;; ported to Emacs-Lisp by Jamie Zawinski <jwz@lucid.com>, 5-mar-91.
;;;
(defconst conx-version "1.3, 10-may-92.")
;;;
;;; Run this compiled.  It will be an order of magnitude faster.
;;;
;;; Select a buffer with a lot of text in it.  Say Meta-X conx-buffer
;;; or Meta-X conx-region.  Repeat on as many other bodies of text as
;;; you like.
;;;
;;; Meta-X conx will use the word-frequency tree the above generated
;;; to produce random sentences in a popped-up buffer.  It will pause
;;; at the end of each paragraph for two seconds; type ^G to stop it.
;;;
;;; Meta-X conx-init will clear the data structures so you can start
;;; over.  Note that if you run it twice consecutively on the same
;;; body of text, word sequences in that buffer will be twice as
;;; likely to be generated.
;;;
;;; Once you have sucked in a lot of text and like the kinds of
;;; sentences conx is giving you, you can save the internal data
;;; structures to a file with the Meta-x conx-save command.  Loading
;;; this file with Meta-x conx-load will be a lot faster and easier
;;; than re-absorbing all of the text files.  Beware that loading a
;;; saved conx-file clears the conx database in memory.
;;;
;;; Ideas for future improvement:
;;;
;;;  o  It would be nice if we could load in more than one saved
;;;     file at a time.
;;;
;;;  o  use it to collect statistics on newsgroup conversations by
;;;     examining the tree for the most common words and phrases
;;;
;;;  o  when replying to mail, insert an X-CONX: header field which
;;;     contains a sentence randomly generated from the body of the
;;;     message being replied to.
;;;
;;;  o  It could stand to be faster...

(defvar conx-bounce 10) ; 1/x
(defvar conx-hashtable-size 9923)  ; 9923 is prime
(defconst conx-words-hashtable nil)
(defconst conx-words-vector nil)
(defconst conx-words-vector-fp 0)

(defconst conx-last-word nil)

(defvar conx-files nil "FYI")

(defun conx-init ()
  "Forget the current word-frequency tree."
  (interactive)
  (if (and conx-words-hashtable
	   (>= (length conx-words-hashtable) conx-hashtable-size))
      (fillarray conx-words-hashtable 0)
      (setq conx-words-hashtable (make-vector conx-hashtable-size 0)))
  (if conx-words-vector
      (fillarray conx-words-vector nil)
      (setq conx-words-vector (make-vector 1000 nil))) ; this grows
  (setq conx-words-vector-fp 0)
  (setq conx-last-word nil
	conx-files nil))

(defun conx-rehash ()
  ;; misnomer; this just grows the linear vector, growing the hash table
  ;; is too hard.
  (message "Rehashing...")
  (let* ((L (length conx-words-vector))
	 (v2 (make-vector (+ L L) nil)))
    (while (< 0 L)
      (aset v2 (1- L) (aref conx-words-vector (setq L (1- L)))))
    (setq conx-words-vector v2)
    )
  (message "Rehashing...done"))

(defmacro conx-count  (word) (list 'aref word 0))
(defmacro conx-cap    (word) (list 'aref word 1))
(defmacro conx-comma  (word) (list 'aref word 2))
(defmacro conx-period (word) (list 'aref word 3))
(defmacro conx-quem   (word) (list 'aref word 4))
(defmacro conx-bang   (word) (list 'aref word 5))
(defmacro conx-succ   (word) (list 'aref word 6))
(defmacro conx-pred   (word) (list 'aref word 7))
(defmacro conx-succ-c (word) (list 'aref word 8))
(defmacro conx-pred-c (word) (list 'aref word 9))
(defconst conx-length 10)

(defmacro conx-make-word ()
  '(copy-sequence '[1 0 0 0 0 0 nil nil 0 0]))

(defmacro conx-setf (form val)  ; mind-numbingly simple
  (setq form (macroexpand form (and (boundp 'byte-compile-macro-environment)
				    byte-compile-macro-environment)))
  (cond ((symbolp form) (list 'setq form val))
	((eq (car form) 'aref) (cons 'aset (append (cdr form) (list val))))
	((eq (car form) 'cdr) (list 'setcdr (nth 1 form) val))
	((eq (car form) 'car) (list 'setcar (nth 1 form) val))
	(t (error "can't setf %s" form))))

(defmacro conx-push (thing list)
  (list 'conx-setf list (list 'cons thing list)))

(defconst conx-most-positive-fixnum (lsh -1 -1)
  "The largest positive integer that can be represented in this emacs.")

(defmacro conx-rand (n)
  (list '% (list 'logand 'conx-most-positive-fixnum '(random)) n))

(defmacro conx-relate-succ (word related)
  (` (let ((vec (symbol-value (, word))))
       (conx-setf (conx-succ-c vec) (1+ (conx-succ-c vec)))
       (let ((rel (assq (, related) (conx-succ vec))))
	 (if rel
	     (setcdr rel (1+ (cdr rel)))
	     (conx-push (cons (, related) 1) (conx-succ vec)))))))

(defmacro conx-relate-pred (word related)
  (` (let ((vec (symbol-value (, word))))
       (conx-setf (conx-pred-c vec) (1+ (conx-pred-c vec)))
       (let ((rel (assq (, related) (conx-pred vec))))
	 (if rel
	     (setcdr rel (1+ (cdr rel)))
	     (conx-push (cons (, related) 1) (conx-pred vec)))))))

(defmacro conx-add-word (word)
  (` (let* ((word (, word))
	    (fc (aref word 0)))
       (setq word (intern (downcase word) conx-words-hashtable))
       (let ((vec (and (boundp word) (symbol-value word))))
	 (if vec
	     (conx-setf (conx-count vec) (1+ (conx-count vec)))
	   (if (= conx-words-vector-fp (length conx-words-vector))
	       (conx-rehash))
	   (set word (setq vec (conx-make-word)))
	   (aset conx-words-vector conx-words-vector-fp word)
	   (setq conx-words-vector-fp (1+ conx-words-vector-fp)))
	 (or (< fc ?A) (> fc ?Z)
	     (conx-setf (conx-cap vec) (1+ (conx-cap vec)))))
       (if conx-last-word
	   (progn
	     (conx-relate-succ conx-last-word word)
	     (conx-relate-pred word conx-last-word)))
       (setq conx-last-word word))))

(defmacro conx-punx (char)
  (` (if conx-last-word
	 (let ((char (, char))
	       (vec (symbol-value conx-last-word)))
	   (cond ((eq char ?\,)
		  (conx-setf (conx-comma vec) (1+ (conx-comma vec))))
		 ((or (eq char ?\.)
		      (eq char ?\;))
		  (conx-setf (conx-period vec) (1+ (conx-period vec)))
		  (setq conx-last-word nil))
		 ((eq char ?\?)
		  (conx-setf (conx-quem vec) (1+ (conx-quem vec)))
		  (setq conx-last-word nil))
		 ((eq char ?\!)
		  (conx-setf (conx-bang vec) (1+ (conx-bang vec)))
		  (setq conx-last-word nil)))))))

(defun conxify-internal ()
  (let (p w)
    (while (not (eobp))
      (skip-chars-forward "^A-Za-z0-9'")
      (while (memq (following-char) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\'))
	;; ignore words beginning with digits
	(skip-chars-forward "A-Za-z0-9'")
	(skip-chars-forward "^A-Za-z0-9'"))
      (setq p (point))
      (skip-chars-forward "A-Za-z0-9'")
      (if (= ?\' (preceding-char)) (forward-char -1))
      (if (eq p (point))
	  nil
	(setq w (buffer-substring p (point)))
	(if (equal "nil" w)  ; hey, nil is totally magic, this doesn't work!
	    nil
	  (conx-add-word w)
	  (setq n (1+ n))
	  (skip-chars-forward " \t\n\r")
	  (if (memq (setq p (following-char)) '(?\, ?\. ?\! ?\? ?\;))
	      (conx-punx p)))))))

(defun conx-buffer ()
  "Absorb the text in the current buffer into the tree."
  (interactive)
  (or conx-words-vector (conx-init))
  (let ((i conx-words-vector-fp)
	(n 0)
	(pm (point-max)))
    (save-excursion
      (goto-char (point-min))
      (save-restriction
	(widen)
	(while (< (setq p (point)) pm)
	  (search-forward "\n\n" pm 0)
	  (narrow-to-region p (point))
	  (goto-char (prog1 p (setq p (point))))
	  (conxify-internal)
	  (widen)
	  (message "%d%%..." (/ (* p 100) (point-max))))))
    (if buffer-file-name
	(setq conx-files (nconc conx-files (list buffer-file-name))))
    (message "%s words, %d unique" n (- conx-words-vector-fp i))))

(defun conx-region (p m)
  "Absorb the text in the current region into the tree."
  (interactive "r")
  (save-restriction
    (widen)
    (narrow-to-region p m)
    (conx-buffer)))

(defun conx-mail-buffer ()
  "Conxify a buffer in /bin/mail format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward "\n \t")
    (let ((case-fold-search nil)
	  (buffer-file-name nil)
	  p p2 p3)
      (or (looking-at "^From ") (error "not in /bin/mail format"))
      (while (not (eobp))
	(search-forward "\n\n" nil 0)
	(setq p (point))
	(search-forward "\nFrom " nil 0)
	(setq p3 (setq p2 (point)))
	;; don't count ".signature" sections.
	(and (re-search-backward "\n--+\n" nil t)
	     (< (count-lines (point) p2) 9)
	     (setq p2 (point)))
	(conx-region p (point))
	(goto-char p3)))
    (if buffer-file-name
	(setq conx-files (nconc conx-files (list buffer-file-name))))
    ))

;;; output

(defun conx-random-related (count list)
  (let ((foll (if (= 0 count) 0 (conx-rand count)))
	ans)
    (while list
      (if (<= foll (cdr (car list)))
	  (setq ans (car (car list))
		list nil)
	  (setq foll (- foll (cdr (car list)))
		list (cdr list))))
    ans))

(defun conx-random-succ (word)
  (if (= 0 (conx-succ-c (symbol-value word)))
      word
      (let ((next (conx-random-related
		    (conx-succ-c (symbol-value word))
		    (conx-succ (symbol-value word)))))
	(if (= 0 (conx-rand conx-bounce))
	    (conx-random-succ
	      (conx-random-related
		(conx-pred-c (symbol-value next))
		(conx-pred (symbol-value next))))
	    next))))


(defun conx-sentence ()
  (let* ((word (aref conx-words-vector (conx-rand conx-words-vector-fp)))
	 (first-p t)
	 (p (point))
	 vec punc str)
    (while word
      (setq punc (conx-rand (conx-count (setq vec (symbol-value word)))))
      (if (or first-p
	      ;; (< (conx-rand (conx-count vec)) (conx-cap vec))
	      (= (conx-count vec) (conx-cap vec))
	      )
	  (progn
	    (setq first-p nil)
	    (setq str (symbol-name word))
	    (insert (+ (- ?A ?a) (aref str 0)))
	    (insert (substring str 1)))
	  (insert (symbol-name word)))
      (cond ((< punc (conx-comma vec))
	     (insert ", "))
	    ((< (setq punc (- punc (conx-comma vec))) (conx-period vec))
	     (setq word nil)
	     (if (= 0 (conx-rand 5))
		 (if (= 0 (conx-rand 4))
		     (insert ": ")
		     (insert "; "))
		 (insert ".  ")))
	    ((< (setq punc (- punc (conx-period vec))) (conx-quem vec))
	     (setq word nil)
	     (insert "?  "))
	    ((< (setq punc (- punc (conx-quem vec))) (conx-bang vec))
	     (setq word nil)
	     (insert "!  "))
	    (t
	     (insert " ")
	     (if (= 0 (conx-succ-c vec)) (setq word nil))))
      (if word
	  (setq word (conx-random-succ word))))
    (fill-region-as-paragraph (save-excursion
				(goto-char p)
				(beginning-of-line)
				(point))
			      (point))
    (if (= (preceding-char) ?\n)
	(if (= 0 (conx-rand 4))
	    (insert "\n")
	  (delete-char -1)
	  (insert "  "))))
  nil)

(defun conx ()
  "Generate some random sentences in the *conx* buffer."
  (interactive)
  (display-buffer (set-buffer (get-buffer-create "*conx*")))
  (select-window (get-buffer-window "*conx*"))
  (message "type ^G to stop.")
  (while t
    (goto-char (point-max))
    (sit-for (if (= (preceding-char) ?\n) 2 0))
    (conx-sentence)))


;;; GNUS interface; grab words from the current message.

(defun conx-gnus-snarf ()
  "For use as a gnus-Select-article-hook."
  (set-buffer gnus-Article-buffer)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (conx-region (point) (point-max)))))

(if nil ;## (featurep 'gnus)
    (setq gnus-Select-article-hook
	  (cons 'conx-gnus-snarf
		(delq 'conx-gnus-snarf
		      (if (and (consp gnus-Select-article-hook)
			       (not (eq 'lambda (car gnus-Select-article-hook))))
			  gnus-Select-article-hook
			  (cons gnus-Select-article-hook nil))))))


;;; Saving the database

(defun conx-save (file)
  "Save the current CONX database to a file for future retrieval.
You can re-load this database with the \\[conx-load] command."
  (interactive "FSave CONX corpus to file: ")
  (save-excursion
   (let (b)
    (unwind-protect
      (progn
	(set-buffer (setq b (get-buffer-create "*conx-save-tmp*")))
	(delete-region (point-min) (point-max))
	(insert ";;; -*- Mode:Emacs-Lisp -*-\n")
	(insert ";;; This is a CONX database file.  Load it with `conx-load'.\n")
	(if conx-files
	    (insert ";;; Corpus: " (mapconcat 'identity conx-files ", ") "\n"))
	(insert ";;; Date: " (current-time-string) "\n\n")
	;; The file format used here is such a cute hack that I'm going to
	;; leave it as an excercise to the reader to figure it out.
	(let ((p (point))
	      (fill-column 78)
	      (fill-prefix "\t")
	      (i 0))
	  (insert "(!! [\t")
	  (while (< i conx-words-vector-fp)
	    (prin1 (aref conx-words-vector i) (current-buffer))
	    (insert " ")
	    (setq i (1+ i)))
	  (insert "])\n")
	  (fill-region-as-paragraph p (point))
	  (insert "\n"))
	(mapatoms (function (lambda (sym)
		    (if (not (boundp sym))
			nil
		      (insert "\(! ")
		      (prin1 sym (current-buffer))
		      (insert " ")
		      (prin1 (symbol-value sym) (current-buffer))
		      (insert "\)\n"))))
		  conx-words-hashtable)
	(goto-char (point-min))
	(while (re-search-forward "\\bnil\\b" nil t)
	  (replace-match "()"))
	(set-visited-file-name file)
	(save-buffer)))
    (and b (kill-buffer b)))))

(defun conx-load (file)
  "Load in a CONX database written by the \\[conx-save] command.
This clears the database currently in memory."
  (interactive "fLoad CONX corpus from file: ")
  (conx-init)
  (fset (intern "!!" conx-words-hashtable)
	(function (lambda (vec)
	  (setq conx-words-vector vec
		conx-words-vector-fp (length vec)))))
  (fset (intern "!" conx-words-hashtable)
	(function setq))
  (let ((obarray conx-words-hashtable))
    (load file)))


;;; Reporting stats

(defun conx-stats ()
  (set-buffer (get-buffer-create "*conx-stats*"))
  (delete-region (point-min) (point-max))
  (mapatoms (function (lambda (x)
	      (or (not (boundp x))
		  (progn
		    (insert (format "%s" (conx-count (symbol-value x))))
		    (insert "\t\t")
		    (insert (symbol-name x))
		    (insert "\n")))))
	    conx-words-hashtable)
  (sort-numeric-fields -1 (point-min) (point-max)))
