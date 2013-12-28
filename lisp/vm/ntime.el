;; Simple implementation of mode-line/echo-area clock, using timers.

(require 'timer)

(defvar display-time-interval 60
  "*Number of secods between update of the clock display.")

(defvar display-time-echo-area nil
  "*Non-nil value means the clock should be displayed in the message echo area,
instead of the mode line.")

(defvar display-time-day-and-date nil
  "*Non-nil value means the day and date should be displayed along with the
usual time of day.")

(defun display-time ()
  "Display time of day in the mode line or echo area."
  (interactive)
  ;; if the "display-time" timer already exists, do nothing.
  (if (get-timer "display-time")
      ()
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
    (start-timer "display-time" 'display-time-function
		 display-time-interval display-time-interval)))

(defun display-time-function ()
  (let (string)
    ;; display the day and date if the user requests it.
    (setq string (substring (current-time-string)
			    (if display-time-day-and-date 0 11) -8))
    ;; stuff the time in the echo area if specified,
    ;; otherwise put it in the modeline, via display-time-string
    ;; and global-mode-string.
    (if display-time-echo-area
	(if (zerop (minibuffer-depth))
	    (save-excursion
	      (set-buffer (window-buffer (minibuffer-window)))
	      (erase-buffer)
	      (indent-to (- (screen-width) (length string) 1))
	      (insert string)))
      (setq display-time-string string)
      ;; voodoo to fake Emacs into recalculating the mode line displays.
      (save-excursion (set-buffer (other-buffer)))
      (set-buffer-modified-p (buffer-modified-p)))))
