;; Calendar functions for adding diary entries.
;; Copyright (C) 1990 Free Software Foundation, Inc.

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


(defun insert-diary-entry ()
  "Insert a diary entry for the date indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname " " year)
            '(monthname " " day ", " year)))
         (date-string (calendar-date-string
                       (or (calendar-cursor-to-date)
                           (error "Cursor is not on a date!"))
                       t)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert date-string " ")))

(defun insert-weekly-diary-entry ()
  "Insert a weekly diary entry for the day of the week indicated by point."
  (interactive)
  (let ((weekday (calendar-day-name
                  (or (calendar-cursor-to-date)
                      (error "Cursor is not on a date!")))))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert diary-nonmarking-symbol weekday " ")))

(defun insert-monthly-diary-entry ()
  "Insert a monthly diary entry for the day of the month indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " * ")
            '("* " day )))
         (date-string (calendar-date-string
                       (or (calendar-cursor-to-date)
                           (error "Cursor is not on a date!"))
                       t)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert date-string " ")))

(defun insert-annual-diary-entry ()
  "Insert an annual diary entry for the day of the year indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname)
            '(monthname " " day)))
         (date-string (calendar-date-string
                       (or (calendar-cursor-to-date)
                           (error "Cursor is not on a date!"))
                       t)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert date-string " ")))

(defun insert-hebrew-diary-entry ()
  "Insert a diary entry for the Hebrew date corresponding to the date
indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname " " year)
            '(monthname " " day ", " year)))
         (hebrew-date (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (calendar-month-name-array calendar-hebrew-month-name-array-leap-year)
         (date-string (calendar-date-string hebrew-date)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert hebrew-diary-entry-symbol date-string " ")))

(defun insert-monthly-hebrew-diary-entry ()
  "Insert a monthly diary entry for the day of the Hebrew month corresponding
to the date indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " * ")
            '("* " day )))
         (hebrew-date (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (calendar-month-name-array calendar-hebrew-month-name-array-leap-year)
         (date-string (calendar-date-string hebrew-date)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert hebrew-diary-entry-symbol date-string " ")))

(defun insert-annual-hebrew-diary-entry ()
  "Insert an annual diary entry for the day of the Hebrew year corresponding
to the date indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname)
            '(monthname " " day)))
         (hebrew-date (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (calendar-month-name-array calendar-hebrew-month-name-array-leap-year)
         (date-string (calendar-date-string hebrew-date)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert hebrew-diary-entry-symbol date-string " ")))

(defun insert-islamic-diary-entry ()
  "Insert a diary entry for the Islamic date corresponding to the date
indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname " " year)
            '(monthname " " day ", " year)))
         (islamic-date (calendar-islamic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (calendar-month-name-array calendar-islamic-month-name-array)
         (date-string (calendar-date-string islamic-date)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert islamic-diary-entry-symbol date-string " ")))

(defun insert-monthly-islamic-diary-entry ()
  "Insert a monthly diary entry for the day of the Islamic month corresponding
to the date indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " * ")
            '("* " day )))
         (islamic-date (calendar-islamic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (calendar-month-name-array calendar-islamic-month-name-array)
         (date-string (calendar-date-string islamic-date)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert islamic-diary-entry-symbol date-string " ")))

(defun insert-annual-islamic-diary-entry ()
  "Insert an annual diary entry for the day of the Islamic year corresponding
to the date indicated by point."
  (interactive)
  (let* ((calendar-date-display-form
          (if european-calendar-style
              '(day " " monthname)
            '(monthname " " day)))
         (islamic-date (calendar-islamic-from-absolute
                       (calendar-absolute-from-gregorian
                        (or (calendar-cursor-to-date)
                            (error "Cursor is not on a date!")))))
         (calendar-month-name-array calendar-islamic-month-name-array)
         (date-string (calendar-date-string islamic-date)))
    (find-file-other-window diary-file)
    (goto-char (point-max))
    (insert islamic-diary-entry-symbol date-string " ")))

