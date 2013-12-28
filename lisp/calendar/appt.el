;;; -*- Mode:Emacs-Lisp -*-
;; Appointment notification functions.
;; Copyright (C) 1989, 1990, 1992, 1993 Free Software Foundation, Inc.

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

;;; 29-nov-89	created by Neil Mager <neilm@juliet.ll.mit.edu>.
;;; 23-feb-91	hacked upon by Jamie Zawinski <jwz@lucid.com>.
;;;  1-apr-91	some more.
;;;
;; appt.el - visible and/or audible notification of
;;           appointments from ~/diary file generated from
;;           Edward M. Reingold's calendar.el.
;;
;; Version 2.1
;;
;; Comments, corrections, and improvements should be sent to
;; Neil M. Mager
;; Net                     <neilm@juliet.ll.mit.edu>
;; Voice                   (617) 981-4803
;;;
;;; Thanks to  Edward M. Reingold for much help and many suggestions, 
;;; And to many others for bug fixes and suggestions.
;;;
;;;
;;; This functions in this file will alert the user of a 
;;; pending appointment based on their diary file.
;;;
;;; ******* It is necessary to invoke 'display-time' and ********
;;; ******* 'appt-initialize' for this to work properly. ********
;;; 
;;; A message will be displayed in the mode line of the emacs buffer and (if
;;; the user desires) the terminal will beep and display a message from the
;;; diary in the mini-buffer, or the user may select to have a message
;;; displayed in a new buffer.
;;;
;;; Variables of note:
;;;
;;; appt-issue-message		If this variable is nil, then the code in this
;;;				file does nothing.
;;; appt-msg-countdown-list	Specifies how much warning you want before 
;;;				appointments.
;;; appt-audible		Whether to beep when it's notification-time.
;;; appt-display-mode-line	Whether to display a countdown to the next 
;;;				appointment in the mode-line.
;;; appt-announce-method	The function used to do the notifications.
;;;	'appt-window-announce		   do it in a pop-up window.
;;;     'appt-screen-announce		   do it in a pop-up screen (v19 only)
;;;	'appt-message-announce		   do it in the echo area.
;;;	'appt-persistent-message-announce  do it in the echo area, but make the
;;;				    messages not go away at the next keystroke.
;;; appt-display-duration	If appt-announce-method is set to the function
;;;				'appt-window-announce, this specifies how many
;;;				seconds the pop-up window should stick around.
;;;
;;; In order to use this, create a diary file, and add the following to your
;;; .emacs file:
;;;
;;;    (require 'appt)
;;;    (display-time)
;;;    (appt-initialize)
;;;
;;; If you wish to see a list of appointments, or a full calendar, when emacs
;;; starts up, you can add a call to (diary) or (calendar) after this.
;;;
;;;  This is an example of what can be in your diary file:
;;;	 Monday
;;;	   9:30am Coffee break
;;;	  12:00pm Lunch
;;; 
;;; Based upon the above lines in your .emacs and diary files, the calendar
;;; and/or diary will be displayed when you enter emacs and your appointments
;;; list will automatically be created.  You will then be reminded at 9:20am
;;; about your coffee break and at 11:50am to go to lunch.
;;;
;;; In order to interactively add or delete items from today's list, use 
;;; Meta-x appt-add and Meta-x appt-delete.  (This does not modify your 
;;; diary file, so these will be forgotten when you exit emacs.)
;;;
;;; Additionally, the appointments list is recreated automatically at 12:01am 
;;; for those who do not logout every day or are programming late.
;;;
;;; You can have special appointments which execute arbitrary code rather than
;;; simply notifying you -- sort of like the unix "cron" facility.  The syntax
;;; for this is borrowed from the Calendar's special-date format.  If you have
;;; a diary entry like
;;;
;;;  Monday
;;;    3:00am	%%(save-all-modified-buffers)
;;;
;;; then on monday at 3AM, the function `save-all-modified-buffers' will be
;;; invoked.  (Presumably this function is defined in your .emacs file.)
;;; There will be no notification that these "special" appointments are being
;;; triggered, unless the form evaluated produces a notification.
;;;
;;; It is necessary for the entire list after the "%%" to be on one line in 
;;; your .diary file -- there may not be embedded newlines in it.  This is a
;;; bit of a misfeature.
;;;
;;; This also interacts correctly with Benjamin Pierce's reportmail.el package.
;;;
;;; Brief internal description - Skip this if your not interested!
;;;
;;; The function appt-initialize invokes 'diary' to get a list of today's
;;; appointments, and parses the lines beginning with date descriptions.
;;; This list is cached away.  'diary' is invoked in such a way so as to
;;; not pop up a window displaying the diary buffer.
;;;
;;; The function appt-check is run from the 'loadst' process (or the 'wakeup'
;;; process in emacs 18.57 or newer) which is started by invoking display-time.
;;; It checks this cached list, and announces as appropriate.  At midnight,
;;; appt-initialize is called again to rebuild this list.
;;;
;;; display-time-filter is modified to invoke appt-check.
;;;
;;; TO DO:
;;;
;;;  o  multiple adjascent appointments are not handled gracefully.  If there 
;;;     is an appointment at 3:30 and another at 3:35, and you have set things
;;;     up so that you get a notification twenty minutes before each appt,
;;;     then a notification should come at 3:10 for the first appt, and at
;;;     3:15 for the second.  Currently, no notifications are generated for an
;;;     appointment until all preceeding appointments have completely expired.
;;;
;;;  o  If there are two appointments at the same time, all but the first are
;;;     ignored (not announced.)
;;;
;;;  o  Appointments which are early enough in the morning that their 
;;;     announcements should begin before midnight are not announced until
;;;     midnight.
;;;
;;;  o  There should be some way to mark certain appointments as "important,"
;;;     so that you will be harassed about them even after they have expired.


(require 'calendar)
(require 'diary)

(defvar appt-issue-message t
  "*If T, the diary buffer is checked for appointments.  For an
 appointment warning to be made, the time must be the first thing on
 the line.")

(defvar appt-msg-countdown-list '(20 15 10 5 3 1)
  "*A list of the intervals in minutes before the appointment when
 the warnings will be given.  That is, if this were the list '(5 3 1),
 then a notification would be given five minutes, three minutes, and
 one minute before the appointment.")

(defvar appt-check-time-syntax nil
  "*Whether all diary entries are intended to beging with time specifications.
Appt will beep and issue a warning message when encountering unparsable 
lines.")

(defvar appt-audible t
  "*Controls whether appointment announcements should beep.")

(defvar appt-display-mode-line t
  "*Controls if minutes-to-appointment should be displayed on the mode line.")

(defvar appt-announce-method 'appt-window-announce
  "*The name of the function used to notify the user of an impending 
appointment.  This is called with two arguments, the number of minutes
until the appointment, and the appointment description list.

Reasonable values for this variable are 'appt-window-announce,
'appt-message-announce, or 'appt-persistent-message-announce.")


(defvar appt-time-msg-list nil
  "The list of appointments for today.  Use appt-add and appt-delete
 to add and delete appointments from list.  The original list is generated
 from the today's diary-entries-list. The number before each time/message
 is the time in minutes after midnight.")

(defconst max-time 1439
  "11:59pm in minutes - number of minutes in a day minus 1.")

(defconst appt-check-tick -1)


;;; Announcement methods

(defun appt-message-announce (min-to-app appt)
  "Set appt-announce-method to the name of this function to cause appointment
notifications to be given via messages in the minibuffer."
  (message (if (eq min-to-app 0) "App't NOW."
	       (format "App't in %d minute%s -- %s"
		       min-to-app
		       (if (eq 1 min-to-app) "" "s")
		       (car (cdr appt))))))


(defun appt-persistent-message-announce (min-to-app appt)
  "Set appt-announce-method to the name of this function to cause appointment
notifications to be given via messages in the minibuffer, but have those 
messages stay around even if you type something (unlike normal messages)."
  (let ((str (if (eq min-to-app 0)
		 (format "App't NOW -- %s" (car (cdr appt)))
		 (format "App't in %d minute%s -- %s"
			 min-to-app
			 (if (eq 1 min-to-app) "" "s")
			 (car (cdr appt)))))
	(in-echo-area-already (eq (selected-window) (minibuffer-window))))
    (if (not in-echo-area-already)
	;; don't stomp the echo-area-buffer if reading from the minibuffer now.
	(save-excursion
	  (save-window-excursion
	    (select-window (minibuffer-window))
	    (delete-region (point-min) (point-max))
	    (insert str))))
    ;; if we're reading from the echo-area, and all we were going to do is
    ;; clear the thing, like, don't bother, that's annoying.
    (if (and in-echo-area-already (string= "" str))
	nil
      (message "%s" str))
    ))


(defvar appt-display-duration 5
  "*The number of seconds an appointment message is displayed in its own 
 window if appt-announce-method is 'appt-window-announce.")

(defun appt-window-announce (min-to-app appt)
  "Set appt-announce-method to the name of this function to cause appointment 
notifications to be given via messages in a pop-up window.  The variable
appt-display-duration controls how long this window should be left up."
  (require 'electric)
  (save-excursion
   (save-window-excursion
    ;; Make sure we're not in the minibuffer
    ;; before splitting the window.
    (if (= (screen-height)
           (nth 3 (window-edges (selected-window))))
        nil
      (appt-select-lowest-window)
      (split-window))
    (let ((this-buffer (current-buffer))
	  appt-disp-buf)
      (unwind-protect
	   (progn
	     (setq appt-disp-buf (set-buffer (get-buffer-create "*appt-buf*")))
	     ;; set the mode-line of the pop-up window
	     (setq mode-line-format 
	       (concat "-------------------- Appointment "
		 (if (eq min-to-app 0)
		     "NOW"
		   (concat "in " min-to-app
		     (if (eq min-to-app 1) " minute" " minutes")))
		 ". ("
		 (let ((h (string-to-int
			    (substring (current-time-string) 11 13))))
		   (concat (if (> h 12) (- h 12) h) ":"
			   (substring (current-time-string) 14 16)
			   (if (< h 12) "am" "pm")))
		 ") %-"))
	     (pop-to-buffer appt-disp-buf)
	     (insert (car (cdr appt)))
	     (shrink-window-if-larger-than-buffer
	       (get-buffer-window appt-disp-buf))
	     (set-buffer-modified-p nil)
	     (sit-for appt-display-duration))
	(and appt-disp-buf (kill-buffer appt-disp-buf)))))))

(defvar appt-screen-defaults nil)

(defun appt-screen-announce (min-to-app appt)
  "Set appt-announce-method to the name of this function to cause appointment 
notifications to be given via messages in a pop-up screen."
  (save-excursion
    (setq appt-disp-buf (set-buffer (get-buffer-create "*appt-buf*")))
    (erase-buffer)
    ;; set the mode-line of the pop-up window
    (setq mode-line-format 
	  (concat "-------------------- Appointment "
		  (if (eq min-to-app 0)
		      "NOW"
		    (concat "in " min-to-app
			    (if (eq min-to-app 1) " minute" " minutes")))
		  ". ("
		  (let ((h (string-to-int
			    (substring (current-time-string) 11 13))))
		    (concat (if (> h 12) (- h 12) h) ":"
			    (substring (current-time-string) 14 16)
			    (if (< h 12) "am" "pm")))
		  ") %-"))
    (insert (car (cdr appt)))
    (let ((height (max 10 (min 20 (+ 2 (count-lines (point-min)
						    (point-max)))))))
      (if (and (boundp 'appt-disp-screen) appt-disp-screen)
	  (let ((s (selected-screen)))
	    (select-screen appt-disp-screen)
	    (make-screen-visible appt-disp-screen)
	    (set-screen-height height)
	    (sit-for 0)
	    (select-screen s))
	(setq appt-disp-screen
	      (x-create-screen
	       (append appt-screen-defaults
		       (list '(width . 80) (cons 'height height)))))))
    ))

;;; used by appt-window-announce
(defun appt-select-lowest-window ()
  " Determines which window is the lowest one being displayed and 
selects that one."
  (setq lowest-window (selected-window))
  (let* ((bottom-edge (car (cdr (cdr (cdr (window-edges))))))
         (last-window (previous-window))
         (window-search t))
    (while window-search
      (let* ((this-window (next-window))
             (next-bottom-edge (nth 3 (window-edges this-window))))
        (if (< bottom-edge next-bottom-edge)
            (progn
              (setq bottom-edge next-bottom-edge)
              (setq lowest-window this-window)))
        (select-window this-window)
        (if (eq last-window this-window)
            (progn
              (select-window lowest-window)
              (setq window-search nil)))))))


;;; To display stuff in the mode line, we use a new variable instead of
;;; just adding stuff to the display-time-string -- this causes less
;;; flicker.

(defvar appt-mode-line-string ""
  "*The string displayed in the mode line by the appointment package.")

(defun appt-display-mode-line (min-to-app)
  "Add an appointment annotation to the mode line."
  (setq appt-mode-line-string
	(if (and appt-display-mode-line min-to-app)
	    (if (eq 0 min-to-app)
		"App't NOW "
		(concat "App't in " min-to-app
			(if (eq 1 min-to-app) " minute  " " minutes ")))
	    ""))
  ;; make sure our variable is visible in global-mode-string.
  (cond ((not appt-display-mode-line) nil)
	((null global-mode-string)
	 (setq global-mode-string (list "" 'appt-mode-line-string)))
	((stringp global-mode-string)
	 (setq global-mode-string
	       (list global-mode-string 'appt-mode-line-string)))
	((not (memq 'appt-mode-line-string global-mode-string))
	 (setq global-mode-string
	       (append global-mode-string (list 'appt-mode-line-string)))))
  ;; force mode line updates - from time.el
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))


;;; Internal stuff

(defun appt-convert-time (time2conv)
  " Convert hour:min[am/pm] format to minutes from midnight."
  (cond ((string-match "^[ \t]*midni\\(ght\\|te\\)[ \t]*\\'" time2conv)
	 0)
	((string-match "^[ \t]*noon[ \t]*\\'" time2conv)
	 (* 12 60))
	(t
	 (let ((hr 0)
	       (min 0))
	   (or (string-match
		 "\\`[ \t]*\\([0-9][0-9]?\\)[ \t]*\\(:[ \t]*\\([0-9][0-9]\\)\\)?[ \t]*\\(am\\|pm\\)?"
		 time2conv)
	       (error "unparsable time \"%s\"" time2conv))
	   (setq hr (string-to-int
		      (substring time2conv
				 (match-beginning 1) (match-end 1))))
	   (if (match-beginning 3)
	       (setq min (string-to-int 
			   (substring time2conv 
				      (match-beginning 3) (match-end 3)))))
	   ;; convert the time appointment time into 24 hour time
	   (if (match-beginning 4)
	       (progn
		 (if (or (= hr 0) (> hr 12))
		     (error "mixing 12hr and 24 hr time!  %s" time2conv))
		 (if (string-match "am"
				   (substring time2conv (match-beginning 4)))
		     (if (= hr 12) (setq hr 0))
		   (if (< hr 12) (setq hr (+ 12 hr))))))
	   (if (> min 59) (error "minutes outa bounds - %s" time2conv))
	   (+ (* hr 60) min)))))


(defun appt-current-time-in-seconds ()
  "returns the current time in seconds since midnight."
  (let* ((str (current-time-string))
	 (hour (string-to-int (substring str 11 13)))
	 (min  (string-to-int (substring str 14 16))))
    (+ (* hour 60) min)))


(defun appt-sort-list (appt-list)
  (sort (copy-sequence appt-list)
	(function (lambda (x y)
	  (< (car (car x)) (car (car y)))))))

(defun appt-diary-entries ()
  (let ((list-diary-entries-hook '(appt-make-list))
	(diary-display-hook nil)
	(diary-list-include-blanks nil))
    ;; this will set appt-time-msg-list.
    (diary 1)
    appt-time-msg-list))

(defun appt-initialize ()
  " Read your `diary-file' and remember today's appointments.  Call this from 
 your .emacs file, or any time you want your .diary file re-read (this happens 
 automatically at midnight to move to notice the next day's appointments).
 
 The time must be at the beginning of a line for it to be put in the 
 appointments list.
               02/23/89
                  12:00pm    lunch
                Wednesday
                  10:00am    group meeting"
  (install-display-time-hook)
  (let ((n (length (appt-diary-entries))))
    (cond ((= n 0) (message "no appointments today."))
	  ((= n 1) (message "1 appointment today."))
	  (t (message (format "%d appointments today." n))))))

(defun appt-make-list ()
  "Don't call this directly; call appt-initialize or appt-diary-entries."
  (setq appt-time-msg-list nil)
  (if diary-entries-list
      ;; Cycle through the entry-list (diary-entries-list) looking for
      ;; entries beginning with a time. If the entry begins with a time,
      ;; add it to the appt-time-msg-list. Then sort the list.
      ;;
      (let ((entry-list diary-entries-list)
	    (new-appts '()))
	(while (and entry-list
		    (calendar-date-equal
		      (calendar-current-date) (car (car entry-list))))
	  (let ((time-string (car (cdr (car entry-list)))))
	    (while (string-match
		    "\\`[ \t\n]*\\([0-9]?[0-9]\\(:[0-9][0-9]\\)?[ \t]*\\(am\\|pm\\)?\\|noon\\|midnight\\|midnite\\).*$"
		     time-string)
	      (let* ((eol (match-end 0))
		     (appt-time-string
		      (substring time-string (match-beginning 1)
				 (match-end 1)))
		     (appt-msg-string
		      (substring time-string (match-end 1) eol))
		     (appt-time (list (appt-convert-time appt-time-string))))
		(setq time-string (substring time-string eol)
		      new-appts (cons (cons appt-time
					    (list (concat appt-time-string ":"
							  appt-msg-string)))
				      new-appts))))
	    (if appt-check-time-syntax
		(while (string-match "\n*\\([^\n]+\\)$" time-string)
		  (beep)
		  (message "Unparsable time: %s"
			   (substring time-string (match-beginning 1)
				      (match-end 1)))
		  (sit-for 3)
		  (setq time-string (substring time-string (match-end 0)))))
					       
	    )
	  (setq entry-list (cdr entry-list)))
	(setq appt-time-msg-list ; seems we can't nconc this list...
	      (append (nreverse new-appts) appt-time-msg-list))))
  (setq appt-time-msg-list (appt-sort-list appt-time-msg-list))
  ;;
  ;; Get the current time and convert it to minutes from midnight. ie. 12:01am
  ;; = 1, midnight = 0, so that the elements in the list that are earlier than
  ;; the present time can be removed.
  ;;
  (let ((cur-comp-time (appt-current-time-in-seconds))
	(appt-comp-time (car (car (car appt-time-msg-list)))))
    (while (and appt-time-msg-list (< appt-comp-time cur-comp-time))
      (setq appt-time-msg-list (cdr appt-time-msg-list)) 
      (if appt-time-msg-list
          (setq appt-comp-time (car (car (car appt-time-msg-list)))))))
  appt-time-msg-list)


(defun appt-beep ()
  (cond ((null appt-audible) nil)
	((numberp appt-audible)
	 (let ((i appt-audible))
	   (while (> i 0) (beep) (setq i (1- i)))))
	((consp appt-audible)
	 (let ((i (car appt-audible))
	       (j (cdr appt-audible)))
	   (if (consp j) (setq j (car j)))
	   (while (> i 0)
	     (beep)
	     (if (integerp j)
		 (sleep-for-millisecs j)
	       (sleep-for j))
	     (setq i (1- i)))))
	(t (beep))))


(defun appt-check ()
  "Check for an appointment and update the mode line and minibuffer if
 desired. Note: the time must be the first thing in the line in the diary
 for a warning to be issued.
  The format of the time can be either 24 hour or am/pm.  Example: 
 
               02/23/89
                 18:00 Dinner
              Thursday
                11:45am Lunch meeting.
  
 The following variables control the action of the notification:
 
 appt-issue-message		If this variable is nil, then the code in this
				file does nothing.
 appt-msg-countdown-list	Specifies how much warning you want before 
				appointments.
 appt-audible			Whether to beep when it's notification-time.
 appt-display-mode-line		Whether to display a countdown to the next 
				appointment in the mode-line.
 appt-announce-method   	The function used to do the notifications.
				'appt-window-announce to do it in a pop-up
				window, 'appt-message-announce or 
				'appt-persistent-message-announce to do it 
				in the echo-area.
 appt-display-duration  	If appt-announce-method is set to the function
				'appt-window-announce, this specifies how many
				seconds the pop-up window should stick around.
 
 This function is run from the `loadst' or `wakeup' process for display-time.
 Therefore, you need to have (display-time) in your .emacs file."
  (if appt-issue-message
   (let ((min-to-app -1)
	 (new-time ""))
     ;; Get the current time and convert it to minutes
     ;; from midnight. ie. 12:01am = 1, midnight = 0.
     (let* ((cur-comp-time (appt-current-time-in-seconds))
	    ;; If the current time is the same as the tick, just return.
	    ;; This means that this function has been called more than once
	    ;; in the current minute, which is not useful.
	    (shut-up-this-time (= cur-comp-time appt-check-tick))
	    (turnover-p (> appt-check-tick cur-comp-time)))
       (setq appt-check-tick cur-comp-time)
       ;;
       ;; If it is now the next day (we have crossed midnight since the last
       ;; time this was called) then we should update our appointments to
       ;; today's list.
       (if turnover-p (appt-diary-entries))
       ;;
       ;; Get the first time off of the list and calculate the number
       ;; of minutes until the appointment.
       (if appt-time-msg-list
	   (let ((appt-comp-time (car (car (car appt-time-msg-list)))))
	     (setq min-to-app (- appt-comp-time cur-comp-time))
	     (while (and appt-time-msg-list (< appt-comp-time cur-comp-time))
	       (setq appt-time-msg-list (cdr appt-time-msg-list)) 
	       (if appt-time-msg-list
		   (setq appt-comp-time (car (car (car appt-time-msg-list))))))
	     ;;
	     ;; If we have an appointment between midnight and warning-time
	     ;; minutes after midnight, we must begin to issue a message
	     ;; before midnight.  Midnight is considered 0 minutes and 11:59pm
	     ;; is 1439 minutes. Therefore we must recalculate the minutes to
	     ;; appointment variable. It is equal to the number of minutes
	     ;; before midnight plus the number of minutes after midnight our
	     ;; appointment is.
	     ;;
	     ;; ## I don't think this does anything -- it would if it were
	     ;; (for example) a 12:01am appt on the list at 11:55pm, but that
	     ;; can't ever happen, because the applicable 12:01am appt is for
	     ;; tomorrow, not today, and we only have today's diary list.
	     ;; It's not simply a matter of concatenating two days together,
	     ;; either, because then tuesday's appts would be signalled on
	     ;; monday.  We have to do a real one-day lookahead -- keep a list
	     ;; of tomorrow's appts, and check it when near midnight.
	     ;;
	     (if (and (< appt-comp-time (apply 'max appt-msg-countdown-list))
		      (> (+ cur-comp-time (apply 'max appt-msg-countdown-list))
			 max-time))
		 (setq min-to-app (+ (- (1+ max-time) cur-comp-time))
		       appt-comp-time))
	     ;;
	     ;; issue warning if the appointment time is within warning-time
	     (cond
	       ;; if there should not be any notifications in the mode-line,
	       ;; clear it.
	       ((> min-to-app (apply 'max appt-msg-countdown-list))
		(appt-display-mode-line nil))
	       ;; do nothing if this is the second time this minute we've
	       ;; gotten here, of if we shouldn't be notifying right now.
	       ((or shut-up-this-time
		    (and (not (= min-to-app 0))
			 (not (memq min-to-app appt-msg-countdown-list))))
		nil)

	       ((and (= min-to-app 0)
		     (string-match "%%(" (nth 1 (car appt-time-msg-list))))
		;;
		;; If this is a magic evaluating-notification, evaluate it.
		;; these kinds of notifications aren't subject to the
		;; appt-msg-countdown-list.
		;;
		(let* ((list-string (substring (nth 1 (car appt-time-msg-list))
					       (1- (match-end 0))))
		       (form (condition-case ()
				 (read list-string)
			       (error
				 (ding)
				 (message "Appt: error reading from \"%s\""
					  (nth 1 (car appt-time-msg-list)))
				 (sit-for 2)
				 nil))))
		  (eval form)))

	       ((and (<= min-to-app (apply 'max appt-msg-countdown-list))
		     (>= min-to-app 0))
		;;
		;; produce a notification.
		(appt-beep)
		(funcall appt-announce-method min-to-app
			 (car appt-time-msg-list))
		;; update mode line and expire if necessary
		(appt-display-mode-line min-to-app)
		;; if it's expired, remove it.
		(if (= min-to-app 0)
		    (setq appt-time-msg-list (cdr appt-time-msg-list))))
	       (t
		;; else we're not near any appointment, or there are no
		;; apointments; make sure mode line is clear.
		(appt-display-mode-line nil))))
	   (appt-display-mode-line nil))))))



;;; Interactively adding and deleting appointments

(defun appt-add (new-appt-time new-appt-msg)
  "Adds an appointment to the list of appointments for the day at TIME
 and issue MESSAGE. The time should be in either 24 hour format or
 am/pm format. "
 
  (interactive "sTime (hh:mm[am/pm]): \nsMessage: ")
  (if (string-match "[0-9]?[0-9]:[0-9][0-9]\\(am\\|pm\\)?" new-appt-time)
      nil
    (error "Unacceptable time-string"))
  
  (let* ((appt-time-string (concat new-appt-time " " new-appt-msg))
         (appt-time (list (appt-convert-time new-appt-time)))
         (time-msg (cons appt-time (list appt-time-string))))
    (setq appt-time-msg-list (append appt-time-msg-list
                                     (list time-msg)))
    (setq appt-time-msg-list (appt-sort-list appt-time-msg-list)))) 

(defun appt-delete ()
  "Deletes an appointment from the list of appointments."
  (interactive)
  (let* ((tmp-msg-list appt-time-msg-list))
    (while tmp-msg-list
      (let* ((element (car tmp-msg-list))
             (prompt-string (concat "Delete " 
                                    (prin1-to-string (car (cdr element))) 
                                    " from list? "))
             (test-input (y-or-n-p prompt-string)))
        (setq tmp-msg-list (cdr tmp-msg-list))
        (if test-input
            (setq appt-time-msg-list (delq element appt-time-msg-list)))
        (setq tmp-appt-msg-list nil)))
    (message "")))


;;; Patching in to existing time code to install our hook.

(defvar display-time-hook nil
  "*List of functions to be called when the time is updated on the mode line.")

(setq display-time-hook 'appt-check)

(defvar display-time-hook-installed nil)

(defun install-display-time-hook ()
 (if display-time-hook-installed         ;; only do this stuff once!
    nil
  (let ((old-fn (if (and (fboundp 'display-time-filter-18-55) ; reportmail.el
			 (fboundp 'display-time-filter-18-57))
		    (if (and (featurep 'timer)  ; Lucid GNU Emacs reportmail.el
			     (fboundp 'display-time-timer-function))
			'display-time-timer-function
		      ;; older reportmail, or no timer.el.
		      (if (string-match "18\\.5[0-5]" (emacs-version))
			  'display-time-filter-18-55
			'display-time-filter-18-57))
		  ;; othewise, time.el
		  (if (and (featurep 'timer)
			   (fboundp 'display-time-function)) ; Lucid GNU Emacs
		      'display-time-function
		    'display-time-filter))))
    ;; we're about to redefine it...
    (fset 'old-display-time-filter (symbol-function old-fn))
    (fset old-fn
	  (function (lambda (&rest args)  ;; ...here's the revised definition
	    "Revised version of the original function: this version calls a hook."
	    (apply 'old-display-time-filter args)
	    (run-hooks 'display-time-hook)))))
  (setq display-time-hook-installed t)
  ))

(provide 'appt)
