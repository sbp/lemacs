;;; about.el --- the About The Authors page (shameless self promotion.)
;;;
;;; This is kind of a kludge.  We were going to use W3 to do this, but
;;; it's too slow to load, and HTML gives you too little control over
;;; the layout (especially indentation and inter-paragraph spacing).
;;; Maybe the text should have been written in limited HTML anyway,
;;; and I should have hacked up a simple and fast parser for it, but
;;; it's done now...
;;;
;;; Code: Jamie Zawinski <jwz@lucid.com>
;;; Text: Ben Wing <wing@netcom.com> and Jamie Zawinski <jwz@lucid.com>
;;; Hard: Amiga 1000, Progressive Peripherals Frame Grabber.
;;; Soft: FG 2.0, DigiPaint 3.0, pbmplus (dec 91), xv 3.0.

(defvar about-xref-map (let ((map (make-sparse-keymap)))
			 (define-key map 'button1 'about-lucid-emacs-xref)
			 (define-key map 'button2 'about-lucid-emacs-xref)
			 map))

(defconst what-are-we-calling-it-today nil) ; historically significant var name

;;;###autoload
(defun about-lucid-emacs ()
  (interactive)
  (or what-are-we-calling-it-today
      (setq what-are-we-calling-it-today
	    (if (featurep 'sunpro) "XEmacs" "Lucid Emacs")))
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create
		     (concat "About " what-are-we-calling-it-today)))
;  (pop-to-buffer (get-buffer-create
;		     (concat "About " what-are-we-calling-it-today)))
  (buffer-disable-undo (current-buffer))
  (widen)
  (set (make-local-variable 'tab-width) 8)
  (setq buffer-read-only t)
  (view-mode)
  (set (make-local-variable 'view-kill-on-exit) t)	     ; for view-less.el
  (set (make-local-variable 'view-exit-action) 'kill-buffer) ; for view.el
  (let* ((buffer-read-only nil)
	 (indent-tabs-mode t)
	 (face #'(lambda (text face)
		   (let ((p (point))
			 e)
		     (insert text)
		     (setq e (make-extent p (point)))
		     ;;(set-extent-property e 'start-open t)
		     (set-extent-face e face)
		     e)))
	 (xref #'(lambda (text xref)
		   (let ((e (funcall face text 'bold)))
		     (set-extent-property e 'keymap about-xref-map)
		     (set-extent-property e 'highlight t)
		     (set-extent-property e 'xref xref)
		     e)))
	 )
    (erase-buffer)
    (insert "\n")
    (cond ((featurep 'sunpro)
	   (indent-to 32)
	   (insert-xemacs-logo)
	   (insert "\n")
	   (indent-to 24)
	   (insert "(also known as Lucid Emacs)"))
	  (t
	   (insert "\t")
	   (set-extent-begin-glyph (make-extent (point) (point)) lucid-logo)
	   (insert "\n")
	   (indent-to 26)
	   (insert "(also known as XEmacs)")))
    (insert "\n\n")
    (indent-to 26) (funcall xref "version 19.10; May 1994" 'version)
    (insert "\n\n")
    (indent-to 29) (insert "brought to you by\n")
    (insert "\n")
    (indent-to 20) (insert "* ") (funcall xref "Jamie Zawinski" 'jwz)
    (insert " <jwz@lucid.com>\n\n")
    (indent-to 34) (insert "with\n\n")
    (indent-to 20) (insert "* ") (funcall xref "Chuck Thompson" 'cthomp)
    (insert " <cthomp@cs.uiuc.edu>\n")
    (indent-to 20) (insert "* ") (funcall xref "Ben Wing" 'wing)
    (insert " <wing@netcom.com>\n")
    (indent-to 20) (insert "* ") (funcall xref "Richard Mlynarik" 'mly)
    (insert " <mly@adoc.xerox.com>\n")
    (indent-to 34) (insert "and\n")
    (indent-to 20) (insert "* ") (funcall xref "many others!" 'others)
    (insert "\n\n")
    (insert "\tClick on the highlighted words for more information.\n")
    (insert "\n\t")
    (funcall face what-are-we-calling-it-today 'italic)
    (insert " is a powerful, extensible ")
    (insert "text editor with full\n\tGUI support, upwardly compatible with ")
    (insert " industry-standard\n\t") (funcall face "GNU Emacs." 'italic)
    (insert "  It stems from a collaboration of Lucid, Inc.\n\twith ")
    (insert "Sun Microsystems, Inc. and the University of Illinois.\n")

    (goto-char (point-min))
    (sit-for 0)
    (message "One moment please...")

    (goto-char (point-max))
    (insert "\n   ")
    (let ((rest '(jwz cthomp wing mly))
	  (got-error nil))
      (while rest
	(let ((who (car rest)))
	  (make-local-variable who)
	  (or (and (boundp who)
		   (pixmapp (symbol-value who)))
	      (condition-case c
		  (save-restriction
		    (set who nil)
		    (narrow-to-region (point) (point))
		    (insert-file-contents
		     (expand-file-name (concat (symbol-name who)
					       (if (or (x-color-display-p)
						       (x-grayscale-display-p))
						   ".xpm.Z"
						 "m.xpm.Z"))
				       data-directory))
		    (call-process-region (point-min) (point-max)
					 "zcat" t t nil)
		    (set who (make-pixmap
			      (prog1 (buffer-string)
				(delete-region (point-min) (point-max)))))
		    )
		(error
		 (setq got-error t)
		 (message nil)
		 (display-error c nil)
		 (sit-for 2))))
	  (and (symbol-value who)
	       (let ((e (make-extent (point) (point))))
		 (set-extent-begin-glyph e (symbol-value who))
		 (set-extent-property e 'keymap about-xref-map)
		 (set-extent-property e 'xref who))))
	(insert " ")
	(sit-for 0)
	(setq rest (cdr rest)))
      (insert "\n")
      (goto-char (point-min))
      (or got-error (message nil)))
    ))

(defun about-lucid-emacs-xref (e)
  (interactive "@e")
  (let* ((extent (extent-at (event-point e) (window-buffer (event-window e))
			    'xref))
	 (xref (extent-property extent 'xref)))
    (or (equal (buffer-name) (concat "About " what-are-we-calling-it-today))
	;; Kill the sub-buffers when going back to the top, so that we
	;; don't hold pointers to the bitmaps longer than necessary.
	(kill-buffer (current-buffer)))
    (cond
     ((eq xref 'about)
      (about-lucid-emacs))
     ((eq xref 'news)
      (view-emacs-news)
      (view-mode)
      (set (make-local-variable 'view-kill-on-exit) t)	     ; for view-less.el
      (set (make-local-variable 'view-exit-action) 'kill-buffer) ; for view.el
      (save-excursion
	(goto-char (point-min))
	(let ((buffer-read-only nil)
	      p e)
	  (insert "\nClick ")
	  (setq p (point))
	  (insert "here")
	  (setq e (make-extent p (point)))
	  (set-extent-face e 'bold)
	  (set-extent-property e 'keymap about-xref-map)
	  (set-extent-property e 'highlight t)
	  (set-extent-property e 'xref 'version)
	  (insert " to go back to the previous page.\n\n")
	  (set-buffer-modified-p nil)
	  )))
     (t
      (switch-to-buffer (get-buffer-create
			 (cond ((eq xref 'jwz) "About Jamie Zawinski")
			       ((eq xref 'cthomp) "About Chuck Thompson")
			       ((eq xref 'wing) "About Ben Wing")
			       ((eq xref 'mly) "About Richard Mlynarik")
			       ((eq xref 'others) "About Everyone Else")
			       ((eq xref 'version)
				(concat "More About "
					what-are-we-calling-it-today))
			       ((eq xref 'history)
				(concat what-are-we-calling-it-today
					" History"))
			       )))
      (delete-other-windows)
      (buffer-disable-undo (current-buffer))
      (widen)
      (setq buffer-read-only t)
      (view-mode)
      (set (make-local-variable 'view-kill-on-exit) t)	     ; for view-less.el
      (set (make-local-variable 'view-exit-action) 'kill-buffer) ; for view.el
      (let ((buffer-read-only nil)
	    (case-fold-search nil)
	    p)
	(erase-buffer)
	(let* ((b (get-buffer (concat "About " what-are-we-calling-it-today)))
	       (p (and b (symbol-value-in-buffer xref b nil))))
	  (or (pixmapp p) (setq p nil))
	  (cond (p
		 (insert "\n\t")
		 (set-extent-begin-glyph (make-extent (point) (point)) p)
		 (insert "\n\t"))
		(t
		 (insert "\n\n\n\t"))))
	(cond
	 ((eq xref 'version)
	  (setq p (point))
	  (insert "About " what-are-we-calling-it-today)
	  (set-extent-face (make-extent p (point)) 'bold)
	  (insert "\n\n\t" what-are-we-calling-it-today
		  " is based on an early version of GNU Emacs 19 from the
	Free Software Foundation.  It provides all of the functionality
	of GNU Emacs 18, and nearly all the functionality of the latest
	version of GNU Emacs 19 (the major piece of functionality not
	currently provided, TTY support, is due out at the end of the
	summer of 1994).

	In addition, ")
	  (insert what-are-we-calling-it-today
		  " provides a great number of features not
	found in GNU Emacs 19 or any other version, including support for
	arbitrary pixmaps in a buffer; proper integration with Xt and
	Motif (including Motif menubars and scrollbars); support for
	overlapping regions (or ")
	  (setq p (point))
	  (insert "extents")
	  (set-extent-face (make-extent p (point)) 'italic)
	  (insert ") and efficient handling of a
	large number of such extents in a single buffer; support for
	multiple and variable-width fonts; and a clean interface to the
	menubar, window-system events, and key combinations. 

	(Although GNU Emacs 19 provides some support for these things,
	it is done in a complicated, non-general, hacked-up way that is
	still very much intertwined with the old character- and
	ASCII-based ways of approaching the world, which are sufficient
	when dealing with a terminal but not when dealing with a window
	system.)\n\n\t"
		  what-are-we-calling-it-today
		  " is upwardly compatible with GNU Emacs versions 18
	and 19, and in almost all circumstances, Emacs-Lisp code written
	for either of the latter two products will run under "
		  what-are-we-calling-it-today "
	without requiring any modifications, or at most will require
	small changes to accommodate an improved functional interface.
	All the packages available in GNU Emacs 18 and 19 are provided
	in " what-are-we-calling-it-today
	", and many more come standard only in "
	what-are-we-calling-it-today ".\n\n\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'history))
	  (insert " to find out more about the history of "
		  what-are-we-calling-it-today ".")

	  (insert "\n\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'news))
	  (insert " to find out more specifics about this version.")

	  (insert "\n\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'about))
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'history)
	  (setq p (point))
	  (insert what-are-we-calling-it-today " History")
	  (set-extent-face (make-extent p (point)) 'bold)
	  (insert "\n\n\n\t")
	  (setq p (point))
	  (insert "The Lucid, Inc. Point of View")
	  (set-extent-face (make-extent p (point)) 'italic)
	  (insert "

	Lucid's latest product, Energize, is a C/C++ development
	environment.  Rather than invent (and force our users to learn)
	a new user interface, we chose to build part of our environment
	on top of the world's best editor, GNU Emacs.  (Though our
	product is commercial, the work we did on GNU Emacs is free
	software, and is useful without having to purchase our product.
	For information about Energize, other Lucid products, or
	support contracts, send mail to ")
	  (setq p (point))
	  (insert "lucid-info@lucid.com")
	  (set-extent-face (make-extent p (point)) 'italic)
	  (insert " or call\n\tus at (800) 223-9322.)

	We needed a version of Emacs with mouse-sensitive regions,
	multiple fonts, the ability to mark sections of a buffer as
	read-only, the ability to detect which parts of a buffer has
	been modified, and many other features.

	For our purposes, the existing version of Epoch was not
	sufficient; it did not allow us to put arbitrary pixmaps/icons
	in buffers, `undo' did not restore changes to regions, regions
	did not overlap and merge their attributes in the way we needed,
	and several other things.

	We could have devoted our time to making Epoch do what we needed
	(and, in fact, we spent some time doing that in 1990) but, since
	the FSF planned to include Epoch-like features in their version
	19, we decided that our efforts would be better spent improving
	Emacs 19 instead of Epoch.

	Our original hope was that our changes to Emacs would be
	incorporated into the \"official\" v19.  However, scheduling
	conflicts arose, and we found that, given the amount of work
	still remaining to be done, we didn't have the time or manpower
	to do the level of coordination that would be necessary to get
	our changes accepted by the FSF.  Consequently, we released our
	work as a forked branch of Emacs, instead of delaying any
	longer.

	Roughly a year after Lucid Emacs 19.0 was released, a beta
	version of the FSF branch of Emacs 19 was released.  The FSF
	version is better in some areas, and worse in others, as
	reflects the differing focus of our development efforts.

	We plan to continue developing and supporting Lucid Emacs, and
	merging in bug fixes and new features from the FSF branch as
	appropriate; we do not plan to discard any of the functionality
	that we implemented which RMS has chosen not to include in his
	version.

	Certain elements of Lucid Emacs, or derivatives of them, have
	been ported to the FSF version.  We have not been doing work in
	this direction, because we feel that Lucid Emacs has a cleaner
	and more extensible substrate, and that any kind of merger
	between the two branches would be far easier by merging the FSF
	changes into our version than the other way around.

	We have been working closely with the Epoch developers to merge
	in the remaining Epoch functionality which Lucid Emacs does not
	yet have.  Epoch and Lucid Emacs will soon be one and the same
	thing.  Work is being done on a compatibility package which will
	allow Epoch 4 code to run in Lemacs with little or no change.
	(Lucid Emacs is now running a descendant of the Epoch redisplay
	engine.)")

	  (insert "\n\n\n\t")
	  (setq p (point))
	  (insert "The SunPro Point of View")
	  (set-extent-face (make-extent p (point)) 'italic)
	  (insert "

	Emacs 18 has been around for a long, long time.  Version 19 was
	supposed to be the successor to v18 with X support.  It was
	going to be available \"real soon\" for a long time (some
	people remember hearing about v19 as early as 1984!), but it
	never came out.  v19 development was going very, very slowly,
	and from the outside it seemed that it was not moving at all.
	In the meantime other people gave up waiting for v19 and
	decided to build their own X-aware Emacsen.  The most important
	of these was probably Epoch, which came from University of
	Illinois (\"UofI\") and was based on v18.

	Around two years ago SunPro (a division of Sun Microsystems,
	Inc.) decided that it wanted an integrated editor.  They contracted
	with UofI to provide a number of basic enhancements to the
	functionality in Epoch.  UofI initially was planning to deliver
	this on top of Epoch code.

	In the meantime (actually some time before they talked with UofI)
	Lucid had decided that it also wanted to provide an integrated
	environment with an integrated editor.  Lucid decided that the
	v19 basis was a better one than v18 and thus decided not to use
	Epoch but instead work with Richard Stallman, the head of the
	Free Software Foundation and principle author of Emacs, on
	getting v19 out.  At some point Stallman and Lucid parted ways.
	Lucid kept working and got a v19 out that they called Lucid
	Emacs 19.

	After Lucid's v19 came out it became clear to UofI and SunPro
	that the right thing to do was to push for an integration of
	both Lucid Emacs and Epoch, and to get the deliverables that
	SunPro was asking from UofI on top of this integrated platform.
	Through the last two years, SunPro has been actively supporting
	this product and has been investing a comparable amount of
	effort into it, as Lucid has.  Substantial portions of the
	current code have originated under the support of SunPro, either
	directly in SunPro, or in UofI but paid for by SunPro.  This
	code was kept away from Lucid for a while, but later was made
	available to them.  Initially Lucid didn't know that SunPro was
	supporting UofI, but later they were open about it.

	Around 6 months ago the SunPro-related code started showing up
	in Lucid Emacs.  This started with the infusion of the Epoch
	redisplay code.

	At this moment there is basically no difference in the source
	trees between what is at UofI, SunPro, or Lucid.  All the
	development sites are in sync.

	SunPro originally called the integrated product ERA, for
	\"Emacs Rewritten Again\".  SunPro and Lucid recently came to
	an agreement to find a name for the product that was not
	specific to either company.  An additional constraint that
	Lucid placed on the name was that it must contain the word
	\"Emacs\" in it -- thus \"ERA\" was not acceptable.  The
	tentatively agreed-upon name is \"XEmacs\", and, if all goes
	according to plan, this is what the product will be called
	after the release of 19.10.  (SunPro is already calling the
	product XEmacs, but Lucid is still calling it Lucid Emacs.)\n\n")

	  (insert "\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'version))
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'jwz)
	  (setq p (point))
	  (insert "Jamie Zawinski")
	  (set-extent-face (make-extent p (point)) 'bold)
	  (insert "\t\t\"")
	  (setq p (point))
	  (insert "So much to do, so little time.")
	  (set-extent-face (make-extent p (point)) 'italic)
	  (insert "\"\n\n")
	  (insert "\tYou can FTP other stuff I've written from LUCID.COM
	(or just click here and I'll do it for you!)

	Here's my resume, in case you feel like making me an
	offer I can't refuse.\n\n")
	  (insert-file-contents (expand-file-name "jwz-resume.txt"
						  data-directory))
	  (skip-chars-forward " \t\n")
	  (if (looking-at "This is Info file ")
	      (while (not (looking-at "^---"))
		(delete-region (point) (progn (forward-line 1) (point)))))
	  (goto-char (point-min))
	  (search-forward "click here")
	  (let ((e (make-extent (match-beginning 0) (match-end 0)))
		(m (make-sparse-keymap))
		(f #'(lambda ()
		       (interactive)
		       (require 'ange-ftp)
		       (let ((ange-ftp-default-user "anonymous")
			     (ange-ftp-generate-anonymous-password
			      (format "%s@" (user-login-name))))
			 (dired "/anonymous@lucid.com:/pub/hacks/")))))
	    (define-key m 'button1 f)
	    (define-key m 'button2 f)
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap m)
	    (set-extent-property e 'highlight t))
	  (re-search-forward "^-+\n")
	  (while (looking-at "^[ \t]+\\(.*\\)[ \t]*$")
	    (set-extent-face (make-extent (match-beginning 1) (match-end 1))
			     'bold)
	    (forward-line 1))
	  (while (re-search-forward "^[A-Z][A-Z].*$" nil t)
	    (set-extent-face (make-extent (match-beginning 0) (match-end 0))
			     'bold))
	  (goto-char (point-min))
	  (while (re-search-forward "^ *\\*" nil t)
	    (let ((p (point)))
	      (forward-line 2)
	      (search-backward "," nil nil 2)
	      (set-extent-face (make-extent p (point)) 'bold)
	      (skip-chars-forward ", \n")
	      (setq p (point))
	      (end-of-line)
	      (set-extent-face (make-extent p (point)) 'italic)))
	  (goto-char (point-min))
	  (let ((words '("Lucid Emacs" "Energize Programming System"
			 "xkeycaps" "xdaliclock" "fnord" "xscreensaver"
			 "Insidious Big Brother Database" "audio-tape.ps"
			 "Scribe" "Hemlock")))
	    (while words
	      (if (search-forward (car words) nil t)
		  (set-extent-face
		   (make-extent (match-beginning 0) (match-end 0))
		   'italic))
	      (setq words (cdr words))))
	  (goto-char (point-max))
	  (skip-chars-backward " \t\n")
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (set-extent-face (make-extent (point) (progn (end-of-line) (point)))
			   'italic)

	  (goto-char (point-max))
	  (insert "------------------------------------"
		  "-----------------------------------")
	  (insert "\n\n\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'about))
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'cthomp)
	  (setq p (point))
	  (insert "Chuck Thompson")
	  (set-extent-face (make-extent p (point)) 'bold)
	  (insert " <cthomp@cs.uiuc.edu>

	Chuck works on " what-are-we-calling-it-today
	" under the direction of Simon Kaplan
	(originator of Epoch) with funding from Sun Microsystems.
	He is responsible for the last year's redisplay improvements
	and the scrollbars, among many other things.  Through the
	magic of the Internet, he manages to work closely with the
	other " what-are-we-calling-it-today
	" developers while living in a cold place
	2,000 miles away from everyone else.")

	  (insert "\n\n\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'about))
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'wing)
	  (setq p (point))
	  (insert "Ben Wing")
	  (set-extent-face (make-extent p (point)) 'bold)
	  (insert " <wing@netcom.com>

	I'm not a thug -- I just play one on video.
	I'm a San Francisco \"Mission Critter\" and a hacker.\n\n\t")
	  (setq p (point))
	  (insert "Gory stuff follows:")
	  (set-extent-face (make-extent p (point)) 'italic)
	  (insert "

	In 1992 I left a stuffy university that shall remain nameless
	(but which is sometimes known as Princeton), set out into the
	real world, and ended up a co-founder of Pearl Software.  As
	part of this company, I am the principal architect of
	Win-Emacs, a port of Lucid Emacs to Microsoft Windows and
	Windows NT (for more info, e-mail to ")
	  (setq p (point))
	  (insert "info@pearlsoft.com")
	  (set-extent-face (make-extent p (point)) 'italic)
	  (insert ").

	Since April 1993, I've worked on " what-are-we-calling-it-today
	" as a contractor
	for SunPro, a division of Sun Microsystems.  My main
	contributions to the product include more robust cooperation
	between Emacs and Xt/Motif, user interface improvements,
	improved documentation (especially the Emacs Lisp manual),
	and the ability to run an Emacs screen as a widget inside of
	another Motif or Xt client.")
	  (insert "\n\n\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'about))
	  (insert " to go back to the previous page.\n")
	  )
	 ((eq xref 'mly)
	  (setq p (point))
	  (insert "Richard Mlynarik")
	  (set-extent-face (make-extent p (point)) 'bold)
	  (insert " <mly@adoc.xerox.com>

	Richard Mlynarik has many clever things to say.")

	  (insert "\n\n\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'about))
	  (insert " to go back to the previous page.\n")
	  )

	 ((eq xref 'others)
	  (setq p (point))
	  (insert "Other Contributors to " what-are-we-calling-it-today)
	  (set-extent-face (make-extent p (point)) 'bold)
	  (insert "

	Like most free software, " what-are-we-calling-it-today
	" would be much less than
	it is today without the time and effort volunteered by its
	users.  These are a few of them; we have no doubt forgotten
	someone; we apologize!

	Matthieu Devin <devin@rs.com>
	  Part of the original (pre-19.0) Lucid Emacs development team.
	  Matthieu wrote the initial Energize interface, designed the
	  toolkit-independent Lucid Widget library, and fixed enough
	  redisplay bugs to last a lifetime.  The features in Lucid
	  Emacs were largely inspired by Matthieu's initial prototype
	  of an Energize interface using Epoch.

	Harlan Sexton <hbs@odi.com>
	  Part of the original (pre-19.0) Lucid Emacs development team.
	  Harlan designed and implemented many of the low level data
	  structures which are original to the Lucid version of Emacs,
	  including extents and hash tables.

	Eric Benson <eb@kaleida.com>
	  Part of the original (pre-19.0) Lucid Emacs development team.
	  Eric played a big part in the design of many aspects of the
	  system, including the new command loop and keymaps, fixed
	  numerous bugs, and has been a reliable beta tester ever
	  since.

	Vladimir Ivanovic <Vladimir.Ivanovic@Eng.Sun.COM>
	  Technical lead for XEmacs at SunPro.  Responsible for
	  overseeing SunPro's development effort and getting XEmacs
	  onto the SunPro Friends CD, thus greatly increasing the
	  distribution on the product.  Also provided a great deal
	  of testing, quality assurance, and user interface
	  improvements.

	Eduardo Pelegri-Llopart <pelegri@eng.sun.com>
	  Author of EOS, a package included in the standard XEmacs
	  distribution that integrates XEmacs with the SPARCworks
	  development environment from SunPro.  Past lead for XEmacs
	  at SunPro; advocated the validity of using Epoch, and later
	  Lemacs, at SunPro through several early prototypes.

	John Rose <john.rose@sun.com>
	  Author of many extensions to the `extents' code, including
	  the initial implementation of `duplicable' properties.

	Hans Muller <hmuller@eng.sun.com>
	 Author of the code used to connect " what-are-we-calling-it-today
	  " with ToolTalk,
	 and of an early client of the external Emacs widget.

	William Perry <wmperry@indiana.edu>
	  Author of W3, a package for browsing the World Wide Web
	  which is included in the standard " what-are-we-calling-it-today
	  " distribution.
	  Although W3 runs on all versions of Emacs, Bill has been
	  quick to take advantage of the unique features of "
	  what-are-we-calling-it-today "
	  (such as embedded images and windows).  Thus, the "
	  what-are-we-calling-it-today "
	  version of W3 is significantly more powerful than versions
	  running in other Emacs variants.

	Kyle Jones <kyle@crystal.wonderworks.com>
	  Author of VM (View Mail), a mail-reading package that is
	  included in the standard " what-are-we-calling-it-today
	  " distribution, and
	  contributor of many improvements and bug fixes.  Unlike most
	  other mail-reading packages, VM uses the standard Unix-mail
	  format for its mailboxes; thus, you can use VM concurrently
	  with standard mail readers such as Unix mail and ELM.

	Barry Warsaw <warsaw@anthem.nlm.nih.gov>
	  Author of C++ mode, cc-mode, and numerous other Emacs utilities.

	In addition to those just mentioned, the following people have
	spent a great deal of effort providing feedback, testing beta
	versions of " what-are-we-calling-it-today
	", providing patches to the source code,
	or doing all of the above.  We couldn't have done it without
	them.

	  Mark Allender <allender@vnet.IBM.COM>
	  Butch Anton <butch@taligent.com>
	  Tor Arntsen <tor@spacetec.no>
	  Neal Becker <neal@ctd.comsat.com>
	  Tim Bradshaw <tfb@edinburgh.ac.uk>
	  Matthew J. Brown <mjb@doc.ic.ac.uk>
	  Rick Busdiecker <rfb@lehman.com>
	  <jsc@slayer.mit.edu>
	  Richard Caley <rjc@cstr.edinburgh.ac.uk>
	  Philippe Charton <charton@lmd.ens.fr>
	  Richard Cognot <cognot@ensg.u-nancy.fr>
	  Andy Cohen <cohen@andy.bu.edu>
	  Christopher Davis <ckd@kei.com>
	  Samuel J. Eaton <samuele@cogs.susx.ac.uk>
	  Eric Eide <eeide@asylum.cs.utah.edu>
	  David Fletcher <frodo@tsunami.com>
	  Paul Flinders <ptf@delcam.co.uk>
	  Barry Friedman <friedman@bnr.ca>
	  Dave Gillespie <daveg@synaptics.com>
	  James Grinter <jrg@doc.ic.ac.uk>
	  Dirk Grunwald <grunwald@foobar.cs.Colorado.EDU>
	  Dipankar Gupta <dg@grubb.hpl.hp.com>
	  Magnus Hammerin <magnush@isy.liu.se>
	  Derek Harding <dharding@lssec.bt.co.uk>
	  John Haxby <J.Haxby@isode.com>
	  David Hughes <djh@harston.cv.com>
	  Robin Jeffries <robin.jeffries@sun.com>
	  Doug Keller <dkeller@vnet.ibm.com>
	  Gregor Kennedy <gregork@dadd.ti.com>
	  Darrell Kindred <Darrell.Kindred@cmu.edu>
	  Simon Leinen <simon@lia.di.epfl.ch>
	  Hamish Macdonald <hamish@bnr.ca>
	  Steve March <smarch@quaver.urbana.mcd.mot.com>
	  Dave Mason <dmason@plg.uwaterloo.ca>
	  Jaye Mathisen <osyjm@schizo.coe.montana.edu>
	  Michael Meissner <meissner@osf.org>
	  David M. Meyer <meyer@ns.uoregon.edu>
	  Rob Mori <rob.mori@sun.com>
	  John Morey <jmorey@crl.com>
	  Heiko Muenkel <muenkel@daedalus.tnt.uni-hannover.de>
	  Arup Mukherjee <arup+@cmu.edu>
	  Georg Nikodym <Georg.Nikodym@canada.sun.com>
	  Andy Norman <ange@hplb.hpl.hp.com>
	  Marc Paquette <paquette@crim.ca>
	  Thomas A. Peterson <tap@src.honeywell.com>
	  Andy Piper <ajp@eng.cam.ac.uk>
	  Tibor Polgar <tlp00@eng.amdahl.com>
	  Daniel Rich <drich@corp.sgi.com>
	  Roland Rieke <rol@darmstadt.gmd.de>
	  Russell Ritchie <Russell.Ritchie@gssec.bt.co.uk>
	  Mike Russell <mjruss@rchland.vnet.ibm.com>
	  Jan Sandquist <etxquist@iqa.ericsson.se>
	  Marty Sasaki <sasaki@netop3.harvard.edu>
	  Darrel Schneider <darrel@slc.com>
	  Hayden Schultz <haydens@ll.mit.edu>
	  Cotton Seed <cottons@mcrc.mcrc.mot.com>
	  John Shen <zfs60@cas.org>
	  Jeffrey Sparkes <jsparkes@bnr.ca>
	  Michael Sperber <sperber@informatik.uni-tuebingen.de>
	  Jonathan Stigelman <stig@lucid.com>
	  Raymond L. Toy <toy@alydar.crd.ge.com>
	  Bob Weiner <bob_weiner@pts.mot.com>
	  La Monte Yarroll <piggy@hilbert.maths.utas.edu.au>
	  Blair Zajac <blair@olympia.gps.caltech.edu>
	  and the makers of Jolt Cola (tm)")
	  (goto-char (point-min))
	  (while (re-search-forward "^[ \t]*\\([^<>\n]+\\) <[^>\n]+>$"
				    nil t)
	    (set-extent-face (make-extent (match-beginning 1) (match-end 1))
			     'bold))
	  (goto-char (point-min))
	  (while (re-search-forward "^[ \t]*<\\([^>\n]+\\)>$" nil t)
	    (set-extent-face (make-extent (match-beginning 1) (match-end 1))
			     'bold))

	  (goto-char (point-max))
	  (insert "\n\n\tClick ")
	  (setq p (point))
	  (insert "here")
	  (let ((e (make-extent p (point))))
	    (set-extent-face e 'bold)
	    (set-extent-property e 'keymap about-xref-map)
	    (set-extent-property e 'highlight t)
	    (set-extent-property e 'xref 'about))
	  (insert " to go back to the previous page.\n")
	  )))
      (goto-char (point-min))
      ))))
