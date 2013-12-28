;; Run gdb under Emacs
;; Author: W. Schelter, University of Texas
;;     wfs@rascal.ics.utexas.edu
;; Rewritten by rms.

;; Some ideas are due to  Masanobu. 

;; This file is part of GNU Emacs.
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.

;; Description of GDB interface:

;; A facility is provided for the simultaneous display of the source code
;; in one window, while using gdb to step through a function in the
;; other.  A small arrow in the source window, indicates the current
;; line.

;; Starting up:

;; In order to use this facility, invoke the command GDB to obtain a
;; shell window with the appropriate command bindings.  You will be asked
;; for the name of a file to run.  Gdb will be invoked on this file, in a
;; window named *gdb-foo* if the file is foo.

;; M-s steps by one line, and redisplays the source file and line.

;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the gdb command next on \M-n
;; (def-gdb next "\M-n")

;; This causes the emacs command gdb-next to be defined, and runs
;; gdb-display-frame after the command.

;; gdb-display-frame is the basic display function.  It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the gdb window.  For example after a gdb-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the gdb buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; gdb-display-frame is invoked automatically when a filename-and-line-number
;; appears in the output.


(require 'comint)
(require 'shell)

(defvar gdb-last-frame)
(defvar gdb-delete-prompt-marker)
(defvar gdb-filter-accumulator)
(defvar gdb-last-frame-displayed-p)

(defvar gdb-prompt-pattern "^(.*gdb[+]?) *"
  "A regexp to recognize the prompt for gdb or gdb+.") 

(defvar gdb-mode-map nil
  "Keymap for gdb-mode.")

(if gdb-mode-map
   nil
  (setq gdb-mode-map (copy-keymap comint-mode-map))
  (define-key gdb-mode-map "\C-l" 'gdb-refresh)
  (define-key gdb-mode-map "\C-c\C-c" 'gdb-control-c-subjob)
  (define-key gdb-mode-map "\t" 'comint-dynamic-complete)
  (define-key gdb-mode-map "\M-?" 'comint-dynamic-list-completions))

(define-key ctl-x-map " " 'gdb-break)
(define-key ctl-x-map "&" 'send-gdb-command)

;;Of course you may use `def-gdb' with any other gdb command, including
;;user defined ones.   

(defmacro def-gdb (name key &optional doc)
  (let* ((fun (intern (format "gdb-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'gdb-call cstr))
	  (if key
	      (list 'define-key 'gdb-mode-map key  (list 'quote fun))
	    nil))))

(def-gdb "step"   "\M-s" "Step one source line with display")
(def-gdb "stepi"  "\M-i" "Step one instruction with display")
(def-gdb "finish" "\C-c\C-f" "Finish executing current function")

;;"next" and "cont" were bound to M-n and M-c in Emacs 18, but these are
;;poor choices, since M-n is used for history navigation and M-c is
;;capitalize-word.  These are defined without key bindings so that users
;;may choose their own bindings.
(def-gdb "next"   nil "Step one source line (skip functions)")
(def-gdb "cont"   nil "Proceed with the program")

(def-gdb "up"     "\C-c<" "Go up N stack frames (numeric arg) with display")
(def-gdb "down"   "\C-c>" "Go down N stack frames (numeric arg) with display")


(defun gdb-mode ()
  "Major mode for interacting with an inferior Gdb process.
The following commands are available:

\\{gdb-mode-map}

\\[gdb-display-frame] displays in the other window
the last line referred to in the gdb buffer.

\\[gdb-step],\\[gdb-next], and \\[gdb-nexti] in the gdb window,
call gdb to step,next or nexti and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[gdb-break].

Commands:
Many commands are inherited from comint mode. 
Additionally we have:

\\[gdb-display-frame] display frames file in other window
\\[gdb-step] advance one line in program
\\[send-gdb-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line."
  (interactive)
  (comint-mode)
  (use-local-map gdb-mode-map)
  (set-syntax-table c-mode-syntax-table)
  (mapcar 'make-local-variable
	  '(gdb-last-frame-displayed-p  gdb-last-frame
	    gdb-delete-prompt-marker    gdb-filter-accumulator))
  (setq
   gdb-last-frame nil
   gdb-delete-prompt-marker nil
   gdb-filter-accumulator nil
   major-mode 'gdb-mode
   mode-name "Inferior GDB"
   comint-prompt-regexp gdb-prompt-pattern
   gdb-last-frame-displayed-p t)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (setq comint-input-sentinel 'shell-directory-tracker)
  (run-hooks 'gdb-mode-hook))

(defvar current-gdb-buffer nil)

(defvar gdb-command-name "gdb"
  "Pathname for executing gdb.")

(defun gdb (path)
  "Run gdb on program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'."
  (interactive "fRun gdb on file: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*gdb-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (make-comint (concat "gdb-" file) (substitute-in-file-name gdb-command-name)
		 nil "-fullname"
		 "-cd" default-directory file)
    (gdb-mode)
    (set-process-filter (get-buffer-process (current-buffer)) 'gdb-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'gdb-sentinel)
    (gdb-set-buffer)))

(defun gdb-set-buffer ()
  (cond ((eq major-mode 'gdb-mode)
	(setq current-gdb-buffer (current-buffer)))))

;; This function is responsible for inserting output from GDB
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that GDB prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun gdb-filter (proc string)
  (let ((inhibit-quit t))
    (if gdb-filter-accumulator
	(gdb-filter-accumulate-marker proc
				      (concat gdb-filter-accumulator string))
	(gdb-filter-scan-input proc string))))

(defun gdb-filter-accumulate-marker (proc string)
  (setq gdb-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq gdb-last-frame
			  (cons (substring string 2 first-colon)
				(string-to-int
				 (substring string (1+ first-colon)
					    second-colon)))))
		  (setq gdb-last-frame-displayed-p nil)
		  (gdb-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq gdb-filter-accumulator string)))
	(gdb-filter-insert proc "\032")
	(gdb-filter-scan-input proc (substring string 1)))
    (setq gdb-filter-accumulator string)))

(defun gdb-filter-scan-input (proc string)
  (if (equal string "")
      (setq gdb-filter-accumulator nil)
      (let ((start (string-match "\032" string)))
	(if start
	    (progn (gdb-filter-insert proc (substring string 0 start))
		   (gdb-filter-accumulate-marker proc
						 (substring string start)))
	    (gdb-filter-insert proc string)))))

(defun gdb-filter-insert (proc string)
  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc)))
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert-before-markers string)
	  (set-marker (process-mark proc) (point))
	  (gdb-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (gdb-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t))
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))

(defun gdb-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gdb buffer.
	     (set-buffer obuf))))))


(defun gdb-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive "P")
  (recenter arg)
  (gdb-display-frame))

(defun gdb-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from GDB.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (gdb-set-buffer)
  (and gdb-last-frame (not nodisplay)
       (or (not gdb-last-frame-displayed-p) (not noauto))
       (progn (gdb-display-line (car gdb-last-frame) (cdr gdb-last-frame))
	      (setq gdb-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun gdb-display-line (true-file line)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defun gdb-call (command)
  "Invoke gdb COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  ;; Record info on the last prompt in the buffer and its position.
  ;; This is used in  gdb-maybe-delete-prompt
  ;; to prevent multiple prompts from accumulating.
  (save-excursion
    (goto-char (process-mark (get-buffer-process current-gdb-buffer)))
    (let ((pt (point)))
      (beginning-of-line)
      (setq gdb-delete-prompt-marker
	    (list (point-marker) (- pt (point))
		  (buffer-substring (point) pt)))))
  (gdb-set-buffer)
  (send-string (get-buffer-process current-gdb-buffer)
	       (concat command "\n")))

(defun gdb-maybe-delete-prompt ()
  (if gdb-delete-prompt-marker
      ;; Get the string that we used as the prompt before.
      (let ((prompt (nth 2 gdb-delete-prompt-marker))
	    (length (nth 1 gdb-delete-prompt-marker)))
	;; Position after it.
	(goto-char (+ (car gdb-delete-prompt-marker) length))
	;; Delete any duplicates of it which follow right after.
	(while (and (<= (+ (point) length) (point-max))
		    (string= prompt
			     (buffer-substring (point) (+ (point) length))))
	  (delete-region (point) (+ (point) length)))
	;; If that didn't take us to where output is arriving,
	;; we have encountered something other than a prompt,
	;; so stop trying to delete any more prompts.
	(if (not (= (point)
		    (process-mark (get-buffer-process current-gdb-buffer))))
	    (progn
	      (set-marker (car gdb-delete-prompt-marker) nil)
	      (setq gdb-delete-prompt-marker nil))))))

(defun gdb-break (temp)
  "Set GDB breakpoint at this source line.  With ARG set temporary breakpoint."
  (interactive "P")
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(beginning-of-line)
		(1+ (count-lines 1 (point))))))
    (send-string (get-buffer-process current-gdb-buffer)
		 (concat (if temp "tbreak " "break ")
			 file-name ":" line "\n"))))

(defun gdb-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (point)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(point)))
     (cond (found (forward-char 2)
		  (buffer-substring found
				    (progn (re-search-forward "[^0-9a-f]")
					   (forward-char -1)
					   (point))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (point)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (point)))))))


(defvar gdb-commands nil
  "List of strings or functions used by send-gdb-command.
It is for customization by you.")

(defun send-gdb-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the gdb buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list gdb-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of gdb-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg gdb-commands)))
    (setq addr (gdb-read-address))
    (if (eq (current-buffer) current-gdb-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-gdb-buffer)
    (goto-char (point-max))
    (insert-string comm)))

(defun gdb-control-c-subjob ()
  "Send a Control-C to the subprocess."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer))
		       "\C-c"))

(provide 'gdb)
