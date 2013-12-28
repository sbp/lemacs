;; Light Weight Editor Integration for Sparcworks.
;; "Era on Sparcworks" (EOS)
;;
;; Author: Eduardo Pelegri-Llopart
;;
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;; Initialize EOS
;;

(setq eos::version "1.3")

(defvar eos::left-margin-width 5
  "size of left margin")
(defvar eos::use-left-margin-per-screen nil
  "leave the left margin on whever screen is selected in eos (unimplemented)")

(defvar eos::stop-color "red"
  "foreground color for stop signs")
(defvar eos::solid-arrow-color "purple"
  "foreground color for solid arrow")
(defvar eos::hollow-arrow-color "purple"
  "foreground color for hollow arrow")
(defvar eos::sbrowse-arrow-color "blue"
  "foreground color for browser glyphs")

(defun eos::recompute-presentation ()
  (set-face-foreground 'stop-face eos::stop-color)
  (set-face-foreground 'solid-arrow-face eos::solid-arrow-color)
  (set-face-foreground 'hollow-arrow-face eos::hollow-arrow-color)
  (set-face-foreground 'sbrowse-arrow-face eos::sbrowse-arrow-color)
  )

(defvar eos::base-directory nil
  "location for .xbm and .so's")

;; This stuff needs to be done at startup time
(defun eos::start ()
  (if (not noninteractive)
      (progn
	(or eos::base-directory
	    (setq eos::base-directory
		  (concat (directory-file-name data-directory)
			  "/sparcworks")))
	(setq x-bitmap-file-path (cons eos::base-directory x-bitmap-file-path))
	
	(eos::editor-startup)
	(eos::debugger-startup)
	(eos::debugger-extra-startup)
	(eos::browser-startup)
	(eos::menubar-startup))))

(add-hook 'before-init-hook 'eos::start t) ; append to the end of hook list
