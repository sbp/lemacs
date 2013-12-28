;;; Copyright (C) 1992 Free Software Foundation, Inc.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This file provides integration between Lucid Emacs, Energize and UIM/X.
;;; It is not necessary to be running the Lucid Emacs as part of Energize.

;;; To use this, store in a file, then do M-x load-file and type in the
;;; file name. Alternatively, add (load-file "...filename...") to your
;;; .emacs file to load it every time you start Emacs.

;; First we add a menu item File->UIM/X to the menu bar (if it is not
;; already present), and set the action for this to be "start UIM/X" with
;; a couple of make flags set.

(add-menu-item '("File") "UIM/X" 'start-uimx-running t)

;; When we start UIM/X running, we start a process which runs a shell in
;; the background. This first sets the environment variable MAKEFLAGS to
;; be -e, forcing the environment variable CC to take precedence over the
;; defaults, and defining CC to be what we want. Then it starts UIM/X
;; running. When the user selects "make" the correct options should get
;; selected to do an Energize build.

(defun start-uimx-running ()
  (interactive)
  (start-process "uimx" nil "sh" "-c" "MAKEFLAGS=e CC=\"lcc -Xk -Xez\" uimx")
)

;; Issues outstanding:

;; Make will always select all the generated C files for recompilation.
;; Energize will automatically do the minimum work required when in incremental
;; compilation mode. You also gain from incremental compilation and linking.
;; Only writing changes rather than a whole project will reduce the required
;; number of compilations.

;; Incremental Compilation is not compatible with UIM/X interpreter.
;; In order to avoid this being a problem, it is best to use full compile
;; mode. To turn this on, either select "Full Compiles" from the customise
;; compilation dialog box, or use the command:
;;
;; energize_make_target -Xez "filename.o" -build-option "full compile"
;;
;; from the directory where the file is located.

;; Energize creates a project file which may nameclash with the existing
;; UIM/X project file.

;; If this happens, one workaround is to create a new project manually
;; (using Project->New Project) and to specify the project file, and then
;; build the project, rather than accepting the default while building.
;; Another workaround is to rename the UIM/X project.


