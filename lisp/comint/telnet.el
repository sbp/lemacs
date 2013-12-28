;; Copyright (C) 1985, 1988, 1992 Free Software Foundation, Inc.

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

;; Author William F. Schelter

;;to do fix software types for lispm:
;;to eval current expression.  Also to try to send escape keys correctly.
;;essentially we'll want the rubout-handler off.

;; filter is simplistic but should be okay for typical shell usage.
;; needs hacking if it is going to deal with asynchronous output in a sane
;; manner

(require 'comint)
(provide 'telnet)

(make-variable-buffer-local
 (defvar telnet-new-line "\r"))
(defvar telnet-mode-map nil)
(defvar telnet-prompt-pattern "^[^#$%>]*[#$%>] *")
(make-variable-buffer-local
 (defvar telnet-replace-c-g nil))
(make-variable-buffer-local
 (defvar telnet-remote-echoes t
   "True if the telnet process will echo input."))
(make-variable-buffer-local
 (defvar telnet-interrupt-string "\C-c" "String sent by C-c."))

(defvar telnet-count 0
  "Number of output strings read from the telnet process
while looking for the initial password.")

(defvar telnet-initial-count -50
  "Initial value of telnet-count.  Should be set to the negative of the
number of terminal writes telnet will make setting up the host connection.")

(defvar telnet-maximum-count 4
  "Maximum value telnet-count can have.
After this many passes, we stop looking for initial setup data.
Should be set to the number of terminal writes telnet will make
rejecting one login and prompting for the again for a username and password.")

(defun telnet-interrupt-subjob ()
  (interactive)
  "Interrupt the program running through telnet on the remote host."
  (send-string nil telnet-interrupt-string))

(defun telnet-c-z ()
  (interactive)
  (send-string nil "\C-z"))

(defun send-process-next-char ()
  (interactive)
  (send-string nil
	       (char-to-string
		(let ((inhibit-quit t))
		  (prog1 (read-char)
		    (setq quit-flag nil))))))

; initialization on first load.
(if telnet-mode-map
    nil
  (setq telnet-mode-map (copy-keymap comint-mode-map))
  (define-key telnet-mode-map "\C-m" 'telnet-send-input)
;  (define-key telnet-mode-map "\C-j" 'telnet-send-input)
  (define-key telnet-mode-map "\C-c\C-q" 'send-process-next-char)
  (define-key telnet-mode-map "\C-c\C-c" 'telnet-interrupt-subjob) 
  (define-key telnet-mode-map "\C-c\C-z" 'telnet-c-z))

;;maybe should have a flag for when have found type
(defun telnet-check-software-type-initialize (string)
  "Tries to put correct initializations in.  Needs work."
  (let ((case-fold-search t))
    (cond ((string-match "unix" string)
	 (setq telnet-prompt-pattern comint-prompt-regexp)
	 (setq telnet-new-line "\n"))
	((string-match "tops-20" string) ;;maybe add telnet-replace-c-g
	 (setq telnet-prompt-pattern  "[@>]*"))
	((string-match "its" string)
	 (setq telnet-prompt-pattern  "^[^*>]*[*>] *"))
	((string-match "explorer" string)  ;;explorer telnet needs work
	 (setq telnet-replace-c-g ?\n))))
  (setq comint-prompt-regexp telnet-prompt-pattern))

(defun telnet-initial-filter (proc string)
  ;For reading up to and including password; also will get machine type.
  (cond ((string-match "No such host" string)
	 (kill-buffer (process-buffer proc))
	 (error "No such host."))
	((string-match "passw" string)
	 (telnet-filter proc string)
	 (let* ((echo-keystrokes 0)
		(password (read-password)))
	   (setq telnet-count 0)
	   (send-string proc (concat password telnet-new-line))))
	(t (telnet-check-software-type-initialize string)
	   (telnet-filter proc string)
	   (cond ((> telnet-count telnet-maximum-count)
		  (set-process-filter proc 'telnet-filter))
		 (t (setq telnet-count (1+ telnet-count)))))))

(defun telnet-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let* ((last-insertion (marker-position (process-mark proc)))
	   (delta (- last-insertion (point))))
      (goto-char last-insertion)
      (insert-before-markers string)
      (set-marker (process-mark proc) (point))
      (while (search-backward "\C-m" last-insertion t)
	(delete-char 1))
      (goto-char (process-mark proc))
      (and telnet-replace-c-g
	   (subst-char-in-region last-insertion (point) ?\C-g
				 telnet-replace-c-g t))
      (goto-char (+ (process-mark proc) delta))
      )))

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
      (if p1 (delete-region p1 (1+ p2))))))

(defun telnet-input-sender (proc string)
  (comint-send-string proc string)
  (comint-send-string proc telnet-new-line))

(defun telnet (arg)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer *HOST-telnet*.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen telnet connection to host: ")
  (let ((name (concat arg "-telnet" )))
    (switch-to-buffer (make-comint name "telnet"))
    (set-process-filter (get-process name) 'telnet-initial-filter)
    (erase-buffer)
    (send-string  name (concat "open " arg "\n"))
    (telnet-mode)
    (setq telnet-count telnet-initial-count)))

(defun telnet-mode ()
  "This mode is for use during telnet from a buffer to another
host. It has most of the same commands as comint-mode.
There is a variable ``telnet-interrupt-string'' which is the character
sent to try to stop execution of a job on the remote host.
Data is sent to the remote host when RET is typed.

\\{telnet-mode-map}

Bugs:
--Replaces \C-m by a space, really should remove."
  (interactive)
  (comint-mode)
  (setq major-mode 'telnet-mode
	mode-name "Telnet"
	comint-prompt-regexp telnet-prompt-pattern
	comint-input-sender 'telnet-input-sender)
  (use-local-map telnet-mode-map)
  (run-hooks 'telnet-mode-hook))

(defun read-password ()
  (let ((answ "") tem)
    (message "Reading password...")
    (while (not (or (= (setq tem (read-char)) ?\^m)
		    (= tem ?\n)))
      (setq answ (concat answ (char-to-string tem))))
    (message "")
    answ))
