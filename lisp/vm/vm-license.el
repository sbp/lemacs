;;; Code to show VM's warranty and copying restrictions
;;; Copyright (C) 1989 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defun vm-show-copying-restrictions (&optional warranty)
  (interactive)
  (require 'info)
  (or 
   (condition-case ()
       (progn (Info-goto-node "(vm)License") t)
     (error nil))
   (condition-case ()
       (progn (Info-goto-node "(vm.info)License") t)
     (error nil))
   (error "VM Info documentation appears not to be installed"))
  (if warranty
      (progn
	(search-forward "NO WARRANTY\n" nil t)
	(forward-line -1)
	(set-window-start (selected-window) (point)))))

(defun vm-show-no-warranty ()
  "Display \"NO WARRANTY\" section of the GNU General Public License."
  (interactive)
  (vm-show-copying-restrictions t))
