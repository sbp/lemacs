;; Display time and load in mode line of Emacs.
;; This uses the Lucid GNU Emacs timeout-event mechanism, via a 
;; version of Kyle Jones' timer package.
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'timer)

(defvar display-time-interval 60
  "*Seconds between updates of time in the mode line.")

(defvar display-time-echo-area nil
  "*If non-nil, display-time will use the echo area instead of the mode line.")

(defvar display-time-string nil)

(defun display-time ()
  "Display current time and load level in mode line of each buffer.
Updates automatically every minute.
If display-time-day-and-date is non-nil, the current day and date
are displayed as well.
If display-time-echo-area is non-nil, the time is displayed in the
echo area instead of in the mode-line."
  (interactive)
  ;; if the "display-time" timer already exists, nuke it first.
  (let ((old (get-timer "display-time")))
    (if old (delete-timer old)))
  ;; If we're not displaying the time in the echo area
  ;; and the global mode string does not have a non-nil value
  ;; then initialize the global mode string's value.
  (or display-time-echo-area
      global-mode-string
      (setq global-mode-string '("")))
  ;; If we're not displaying the time in the echo area
  ;; and our display variable is not part of the global-mode-string list
  ;; the we add our variable to the list.  This will make the time
  ;; appear on the modeline.
  (or display-time-echo-area
      (memq 'display-time-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(display-time-string))))
  ;; Display the time initially...
  (display-time-function)
  ;; ... and start a timer to do it automatically thereafter.
  ;;
  ;; If we wanted to be really clever about this, we could have the timer
  ;; not be automatically restarted, but have it re-add itself each time.
  ;; Then we could look at (current-time-seconds) and arrange for the timer
  ;; to wake up exactly at the minute boundary.  But that's just a little
  ;; more work than it's worth...
  (start-timer "display-time" 'display-time-function
	       display-time-interval display-time-interval))


(defun display-time-function ()
  (let ((time (current-time-string))
	(load (format "%03d" (car (load-average))))
	(mail-spool-file (concat rmail-spool-directory
				 (or (getenv "LOGNAME")
				     (getenv "USER")
				     (user-login-name))))
	hour pm string)
    (setq hour (read (substring time 11 13)))
    (setq pm (>= hour 12))
    (if (> hour 12)
	(setq hour (- hour 12))
      (if (= hour 0)
	  (setq hour 12)))
    (setq string
	  (concat (format "%d" hour) (substring time 13 16)
		  (if pm "pm " "am ")
		  (substring load 0 -2) "." (substring load -2)
		  (if (and (file-exists-p mail-spool-file)
			   ;; file not empty?
			   (> (nth 7 (file-attributes mail-spool-file)) 0))
		      " Mail"
		    "")))
    ;; Append the date if desired.
    (if display-time-day-and-date
	(setq string (concat (substring time 0 11) string)))
    (if display-time-echo-area
	(or (eq (selected-window) (minibuffer-window))
	    ;; don't stomp echo-area-buffer if reading from minibuffer now.
	    (save-excursion
	      (save-window-excursion
		(select-window (minibuffer-window))
		(erase-buffer)
		(indent-to (- (screen-width) (length string) 1))
		(insert string)
		(message (buffer-string)))))
      (setq display-time-string string)
      ;; Force redisplay of all buffers' mode lines to be considered.
      (save-excursion (set-buffer (other-buffer)))
      (set-buffer-modified-p (buffer-modified-p)))))
