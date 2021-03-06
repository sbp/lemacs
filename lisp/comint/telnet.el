;;; telnet.el --- run a telnet session from within an Emacs buffer

;;; Copyright (C) 1985, 1988, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: William F. Schelter
;; Maintainer: FSF

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

;;; Commentary:

;; This mode is intended to be used for telnet or rsh to a remode host;
;; `telnet' and `rsh' are the two entry points.  Multiple telnet or rsh
;; sessions are supported.
;;
;; Normally, input is sent to the remote telnet/rsh line-by-line, as you
;; type RET or LFD.  C-c C-c sends a C-c to the remote immediately; 
;; C-c C-z sends C-z immediately.  C-c C-q followed by any character
;; sends that character immediately.
;;
;; All RET characters are filtered out of the output coming back from the
;; remote system.  The mode tries to do other useful translations based
;; on what it sees coming back from the other system before the password
;; query.  It knows about UNIX, ITS, TOPS-20 and Explorer systems.

;;; Code:

;; to do fix software types for lispm:
;; to eval current expression.  Also to try to send escape keys correctly.
;; essentially we'll want the rubout-handler off.

;; filter is simplistic but should be okay for typical shell usage.
;; needs hacking if it is going to deal with asynchronous output in a sane
;; manner

(require 'comint)

(defvar telnet-mode-map nil)
(defvar telnet-new-line "\r")
(make-variable-buffer-local 'telnet-new-line)
(defvar telnet-default-prompt-pattern "^[^#$%>]*[#$%>] *")
(defvar telnet-prompt-pattern telnet-default-prompt-pattern)
(defvar telnet-replace-c-g nil)
(make-variable-buffer-local 'telnet-replace-c-g)
(defvar telnet-remote-echoes t
   "True if the telnet process will echo input.")
(make-variable-buffer-local 'telnet-remote-echoes)
(defvar telnet-interrupt-string "\C-c"
  "String sent by C-c.")
(make-variable-buffer-local 'telnet-interrupt-string)

(defvar telnet-count 0
  "Number of output strings read from the telnet process
while looking for the initial password.")
(make-variable-buffer-local 'telnet-count)

(defvar telnet-initial-count -50
  "Initial value of `telnet-count'.  Should be set to the negative of the
number of terminal writes telnet will make setting up the host connection.")

(defvar telnet-maximum-count 4
  "Maximum value `telnet-count' can have.
After this many passes, we stop looking for initial setup data.
Should be set to the number of terminal writes telnet will make
rejecting one login and prompting for the again for a username and password.")

(defun telnet-interrupt-subjob ()
  (interactive)
  "Interrupt the program running through telnet on the remote host."
  (process-send-string nil telnet-interrupt-string))

(defun telnet-c-z ()
  (interactive)
  (process-send-string nil "\C-z"))

(defun telnet-send-process-next-char ()
  (interactive)
  (process-send-string nil
	       (char-to-string
		(let ((inhibit-quit t))
		  (prog1 (read-char)
		    (setq quit-flag nil))))))

; initialization on first load.
(if telnet-mode-map
    nil
  (progn
    (setq telnet-mode-map (make-sparse-keymap))
    (set-keymap-name telnet-mode-map 'telnet-mode-map)
    (set-keymap-parent telnet-mode-map comint-mode-map)
    (define-key telnet-mode-map "\C-m" 'telnet-send-input)
    ;;(define-key telnet-mode-map "\C-j" 'telnet-send-input)
    (define-key telnet-mode-map "\C-c\C-q" 'telnet-send-process-next-char)
    (define-key telnet-mode-map "\C-c\C-c" 'telnet-interrupt-subjob) 
    (define-key telnet-mode-map "\C-c\C-z" 'telnet-c-z)))

;;maybe should have a flag for when have found type
(defun telnet-check-software-type-initialize (string)
  "Tries to put correct initializations in.  Needs work."
  (let ((case-fold-search t))
    (cond ((string-match "unix" string)
	   (setq telnet-prompt-pattern shell-prompt-pattern)
	   (setq telnet-new-line "\n"))
	  ((string-match "tops-20" string) ;;maybe add telnet-replace-c-g
	   (setq telnet-prompt-pattern  "[@>] *"))
	  ((string-match "its" string)
	   (setq telnet-prompt-pattern  "^[^*>]*[*>] *"))
	  ((string-match "explorer" string) ;;explorer telnet needs work
	   (setq telnet-replace-c-g ?\n))
	  (t
	   (setq telnet-prompt-pattern telnet-default-prompt-pattern))))
  (setq comint-prompt-regexp telnet-prompt-pattern))

(defun telnet-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (cond ((string-match "No such host" string)
	 (kill-buffer (process-buffer proc))
	 (error "No such host."))
	((string-match "passw" string)
	 (telnet-filter proc string)
	 (let ((password (comint-read-noecho "Password: ")))
	   (setq telnet-count 0)
	   (process-send-string proc (concat password telnet-new-line))))
	(t (telnet-check-software-type-initialize string)
	   (telnet-filter proc string)
	   (cond ((> telnet-count telnet-maximum-count)
		  ;; (set-process-filter proc 'telnet-filter)
		  ;; Kludge for shell-fonts -- this is the only mode that
		  ;; actually changes what its process filter is at run time,
		  ;; which confuses shell-font.  So we special-case that here.
		  ;; #### Danger, knows an internal shell-font variable name.
		  (let ((old-filter (process-filter proc)))
		    (if (eq old-filter 'shell-font-process-filter)
			(set (make-local-variable 'shell-font-process-filter)
			     'telnet-filter)
		      (set-process-filter proc 'telnet-filter))))
		 (t (setq telnet-count (1+ telnet-count)))))))

;; Identical to comint-simple-send, except that it sends telnet-new-line
;; instead of "\n".
(defun telnet-simple-send (proc string)
  (comint-send-string proc string)
  (comint-send-string proc telnet-new-line))

(defun telnet-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (save-match-data
     (let* ((last-insertion (marker-position (process-mark proc)))
	    (delta (- last-insertion (point)))
	    (ie (and comint-last-input-end
		     (marker-position comint-last-input-end)))
	    (w (get-buffer-window (current-buffer)))
	    (ws (and w (window-start w))))
       (goto-char last-insertion)
	;; Insert STRING, omitting all C-m characters.
       (insert-before-markers string)
       (set-marker (process-mark proc) (point))
       ;; the insert-before-markers may have screwed window-start
       ;; and likely moved comint-last-input-end.  This is why the
       ;; insertion-reaction should be a property of markers, not
       ;; of the function which does the inserting.
       (if ws (set-window-start w ws t))
       (if ie (set-marker comint-last-input-end ie))
       (while (search-backward "\C-m" last-insertion t)
	 (delete-char 1))
       (goto-char (process-mark proc))
       (and telnet-replace-c-g
	    (subst-char-in-region last-insertion (point) ?\C-g
				  telnet-replace-c-g t))
       (goto-char (+ (process-mark proc) delta))
       ))))

(defun telnet-send-input ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	p1 p2)
    (if (and telnet-remote-echoes
	     (>= (point) (process-mark proc)))
	(save-excursion
	  (if comint-eol-on-send (end-of-line))
	  (setq p1 (marker-position (process-mark proc))
		p2 (point))))
    (prog1
	(comint-send-input)
      ;; at this point, comint-send-input has moved the process mark, inserted
      ;; a newline, and possibly inserted the (echoed) output.  If the host is
      ;; in remote-echo mode, then delete our local copy of the command, and
      ;; the newline that comint-send-input sent.
      (if p1
	  (delete-region p1 (1+ p2))))))

;;;###autoload
(defun telnet (host &optional port)
  "Open a network login connection to host named HOST (a string).
With a prefix argument, prompts for the port name or number as well.
Communication with HOST is recorded in a buffer *HOST-telnet*.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[rsh]'."
  (interactive (list (read-string "Open telnet connection to host: ")
		     (if current-prefix-arg
			 (read-string "Port name or number: ")
		       nil)))
  (let* ((name (concat (if port (concat host "/" port) host) "-telnet"))
         (buffer (get-buffer (concat "*" name "*"))))
    (if (and buffer (get-buffer-process buffer))
	(switch-to-buffer buffer)
      (progn
	(switch-to-buffer (make-comint name "telnet"))
	(set-process-filter (get-process name) 'telnet-initial-filter)
	;; Don't send the `open' cmd till telnet is ready for it.
	(accept-process-output (get-process name))
	(erase-buffer)
	(process-send-string name (concat "open " host
					  (if port (concat " " port) "")
					  "\n"))
	(setq comint-input-sender 'telnet-simple-send)
	(setq telnet-count telnet-initial-count)
	;; run last so that hooks can change things.
	(telnet-mode)))))

(defun telnet-mode ()
  "This mode is for using telnet (or rsh) from a buffer to another host.
It has most of the same commands as comint-mode.
There is a variable ``telnet-interrupt-string'' which is the character
sent to try to stop execution of a job on the remote host.
Data is sent to the remote host when RET is typed.

\\{telnet-mode-map}
"
  (interactive)
  (comint-mode)
  (setq major-mode 'telnet-mode
        mode-name "Telnet"
        comint-prompt-regexp telnet-prompt-pattern)
  (use-local-map telnet-mode-map)
  (run-hooks 'telnet-mode-hook))

;; Berkeley spawn of hell
;;;###autoload
(defun rsh (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer *HOST-rsh*.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[telnet]'."
  (interactive "sOpen rsh connection to host: ")
  (require 'shell)
  (let ((name (concat host "-rsh")))
    (switch-to-buffer (make-comint name "rsh" nil host))
    (setq telnet-count telnet-initial-count)
    ;;
    ;; SunOS doesn't print "unix" in its rsh login banner, so let's get a
    ;; reasonable default here.  There do exist non-Unix machines which
    ;; speak the rsh protocol, but let's hope they print their OS name
    ;; when one connects.
    ;;
    (telnet-check-software-type-initialize "unix")
    ;;
    ;; I think we should use telnet-filter here instead of -initial-filter,
    ;; because rsh generally doesn't prompt for a password, and gobbling the
    ;; first line that contains "passw" is extremely antisocial.  More
    ;; antisocial than echoing a password, and more likely than connecting
    ;; to a non-Unix rsh host these days...
    ;;
    ;; (set-process-filter (get-process name) 'telnet-initial-filter)
    (set-process-filter (get-process name) 'telnet-filter)
    ;; run last so that hooks can change things.
    (telnet-mode)))

(provide 'telnet)
