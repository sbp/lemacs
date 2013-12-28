;;; User Contributed Software for GNUS newsreader
;; Copyright (C) 1989 Masanobu UMEDA

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; The program in this file is contributed by Sakaeda
;; <saka@mickey.trad.pf.fujitsu.junet>, and is not part of the
;; standard distribution of GNUS.  This may be included in the future
;; releases of GNUS.  Please do not send me any flame on it.

;;Return-Path: <saka@mickey.trad.pfu.fujitsu.junet>
;;To: umerin@photon.stars.flab.fujitsu.junet (Masanobu UMEDA)
;;Subject: Re: GNUS 3.11 on SX/A EMACS. 
;;In-Reply-To: Your message of Mon, 03 Apr 89 12:43:11 +0900.
;;             <8904030343.AA00729@photon.stars.flab.fujitsu.junet> 
;;Date: Thu, 06 Apr 89 11:32:41 JST
;;From: Sakaeda <saka@mickey.trad.pf.fujitsu.junet>

;; This file contains patches for very old version of GNU Emacs such
;; as SX/A Emacs (17.64!).

(provide 'gnus-user-sxa)

;; To load SX/A specific patches, use the following hooks:
;;(setq gnus-Group-mode-hook
;;      '(lambda ()
;;	 (require 'gnus-user-sxa)))
;;
;; To show NNTP server name in mode line, use the following hook:
;;(setq gnus-Startup-hook
;;      '(lambda ()
;;	 (gnus-user-sxa-show-server-name)))

(defun gnus-user-sxa-show-server-name ()
  "Show NNTP server name in mode line."
  (setq mode-name (concat mode-name mode-line-process)))

(defun gnus-Subject-set-mode-line ()
  "Set Subject mode line string."
  ;; For ooold GNU Emacs such as SX/A Emacs. by Yas.Itoh at PFU '88.11.24
  (setq mode-line-format 
	(format "--- GNUS: %17s %%[(%%m)%%]----%%3p-%%-"
		(if gnus-current-headers
		    (nntp-header-subject gnus-current-headers)
		  gnus-newsgroup-name)))
  (set-buffer-modified-p t))

(defun gnus-Article-set-mode-line ()
  "Set Article mode line string."
  (let ((unmarked
	 (- (length gnus-newsgroup-unreads) (length gnus-newsgroup-marked))))
    (setq mode-line-format 
 	  (concat "--- GNUS:"
 		  (format "%17s"
			  (format " %s{%d} %s"
				  gnus-newsgroup-name
				  gnus-current-article
				  ;; This is proposed by tale@pawl.rpi.edu.
				  (if (zerop unmarked)
				      "      "
				    (format "%d more" unmarked))
				  ))
		  " %[(%m)%]----%3p-%-")))
  (set-buffer-modified-p t))


;; The following definitions are only for compatibility with *OOOOOLD*
;; Emacs, especially SX/A Emacs (a variant of GNU Emacs).
;; By Yasunari,Itoh and Sakaeda at PFU limited.

(defvar news-inews-program "inews"
  "Function to post news.")
(defvar news-path "/usr/spool/news/"
  "The root directory below which all news files are stored.")

(fset 'load-library (symbol-function 'load))
(fset 'process-send-string (symbol-function 'send-string))
(fset 'process-send-region (symbol-function 'send-region))

;; Save original funcitons.
(or (fboundp 'load-org)
    (fset 'load-org (symbol-function 'load)))
(or (fboundp 'bury-buffer-org)
    (fset 'bury-buffer-org (symbol-function 'bury-buffer)))
(or (fboundp 'apply-org)
    (fset 'apply-org (symbol-function 'apply)))

(load "backquote")

(defun one-window-p (&optional win)
  (if (or (not win)(eq win t)) (setq win (selected-window)))
  (eq (selected-window)(next-window win)))

(defun bury-buffer (&optional buffer)
  (let ((buf (or buffer (current-buffer))))
    (bury-buffer-org buf)
    ;; By Sakaeda and Itoh at PFU '89.02.28
    (or buffer (switch-to-buffer (other-buffer)))
    nil
    ))

(defun apply (func &rest args)
  (let* ((last (car (reverse args)))
	 (before (reverse (cdr (reverse args))))
	 (org-arg (append before last)))
    (apply-org func org-arg)))

(defun file-name-as-directory (filename &optional expand-filename)
  (let* ((expanded 
	  (if expand-filename (expand-file-name filename)
	    filename))
	 (tail-ix (1- (length expanded))))
    (if (and (> tail-ix 0)
	     (= (aref expanded tail-ix) ?/)) expanded
      (concat expanded "/"))))

(defun load (file &optional missing-ok nomessage dum)
  "Load FILE."
  (interactive "sLoad file: ")
  (load-org file missing-ok nomessage))

(defun insert-char (chr count)
  "Insert COUNT (second arg) copies of CHAR (first arg).
Both arguments are required."
  (while (> count 0)
    (insert chr)
    (setq count (1- count))))

;; Fixed by H.Tsujimura(PFU) for GNUS 3.13.

(defun current-window-configuration ()
  (current-buffer))

(defun set-window-configuration (window-config)
  (switch-to-buffer window-config))
