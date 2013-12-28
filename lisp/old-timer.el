;; Run a function with args at some time in future
;; Copyright (C) 1990 Free Software Foundation, Inc.

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

(defvar timer-process nil)
(defvar timer-alist ())
(defvar timer-out "")
(defvar timer-dont-exit nil
  ;; this is useful for functions which will be doing their own erratic
  ;; rescheduling or people who otherwise expect to use the process frequently
  "If non-nil, don't exit the timer process when no more events are pending.")

(defun run-at-time (time repeat function &rest args)
  "Run a function at a time, and optionally on a regular interval.
Arguments are TIME, REPEAT, FUNCTION &rest ARGS.
TIME, a string,  can be specified absolutely or relative to now.
REPEAT, an integer number of seconds, is the interval on which to repeat
the call to the function."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")
  (cond ((or (not timer-process) 
             (memq (process-status timer-process) '(exit signal nil)))
         (if timer-process (delete-process timer-process))
         (setq timer-process (start-process "timer" nil "timer")
               timer-alist nil)
         (set-process-filter   timer-process 'timer-process-filter)
         (set-process-sentinel timer-process 'timer-process-sentinel)
         (process-kill-without-query timer-process))
        ((eq (process-status timer-process) 'stop)
         (continue-process timer-process)))
  ;; There should be a living, breathing timer process now
  (let ((token (concat (current-time-string) "-" (length timer-alist))))
    (send-string timer-process (concat time "\001" token "\n"))
    (setq timer-alist (cons (list token repeat function args) timer-alist))))

(defun timer-process-filter (proc str)
  (setq timer-out (concat timer-out str))
  (let (do token error)
    (while (string-match "\n" timer-out)
      (setq token (substring timer-out 0 (match-beginning 0))
            do (assoc token timer-alist)
            timer-out (substring timer-out (match-end 0)))
      (cond
       (do (apply (nth 2 do) (nth 3 do))   ; do it
           (if (natnump (nth 1 do))        ; reschedule it
               (send-string proc (concat (nth 1 do) " sec\001" (car do) "\n"))
             (setq timer-alist (delq do timer-alist))))
       ((string-match "timer: \\([^:]+\\): \\([^\001]*\\)\001\\(.*\\)$" token)
        (setq error (substring token (match-beginning 1) (match-end 1))
              do    (substring token (match-beginning 2) (match-end 2))
              token (assoc (substring token (match-beginning 3) (match-end 3))
                           timer-alist)
              timer-alist (delq token timer-alist))
        (ding 'no-terminate) ; using error function in process filters is rude
        (message "%s for %s; couldn't set at \"%s\"" error (nth 2 token) do))))
    (or timer-alist timer-dont-exit (process-send-eof proc))))

(defun timer-process-sentinel (proc str)
  (let ((stat (process-status proc)))
    (if (eq stat 'stop) (continue-process proc)
      ;; if it exited normally, presumably it was intentional.
      ;; if there were no pending events, who cares that it exited?
      (if (or (not timer-alist) (eq stat 'exit)) ()
        (ding 'no-terminate)
        (message "Timer exited abnormally.  All events cancelled."))
      (setq timer-process nil timer-alist nil timer-scratch ""))))

(defun cancel-timer (function)
  "Cancel all events scheduled by ``run-at-time'' which would run FUNCTION."
  (interactive "aCancel function: ")
  (let ((alist timer-alist))
    (while alist
      (if (eq (nth 2 (car alist)) function)
          (setq timer-alist (delq (car alist) timer-alist)))
      (setq alist (cdr alist))))
  (or timer-alist timer-dont-exit (process-send-eof timer-process)))

(provide 'timer)
