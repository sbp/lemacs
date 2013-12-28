;; Light Weight Editor Integration for Sparcworks.
;; "Era on Sparcworks" (EOS)
;;
;; Author: Eduardo Pelegri-Llopart
;;
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

(require 'eos-common)

;; ================
;; Browser Protocol
;; ================
;; 
;; three notifications
;;
;; SPRO_SBENG_START
;; SPRO_SBENG_CURRENT_ELEMENT CONTEXT_UID filename lineno center==0
;; SPRO_SBENG_QUIT

(defun eos::browser-startup ()
  "Actions to do at startup for eos-browser.el"
  (make-face 'sbrowse-arrow-face)
  
  (set-face-foreground 'sbrowse-arrow-face
		       eos::sbrowse-arrow-color)
  (set-face-background 'sbrowse-arrow-face
		       (face-background (get-face 'default)))
  
  (setq sbrowser-pattern-list		; list of browser TT patterns
	(eos::create-sbrowser-patterns))
  
  (setq sbrowse-arrow			; pixmaps
	(make-pixmap "sbrowse-arrow.xbm"))

  ;; temporarily fixes redisplay bug
  (set-pixmap-contributes-to-line-height sbrowse-arrow nil)
)

(setq current-match nil)

(defun eos::spro_sbeng_current_element (msg pat)
  ;; SPRO_SBENG_CURRENT_ELEMENT CONTEXT_UID filename lineno center==0
  (let* ((filename
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (lineno
	  (read (get-tooltalk-message-attribute msg 'arg_ival 2)))
	 )
    (eos::make-visible current-match sbrowse-arrow
			  filename lineno 'sbrowser
			  'sbrowse-arrow-face)
    (return-tooltalk-message msg)
    ))

(defun eos::spro_sbeng_start (msg pat)
    (eos::make-invisible current-match)
    (return-tooltalk-message msg)
    )

(defun eos::spro_sbeng_quit (msg pat)
    (eos::make-invisible current-match)
    (return-tooltalk-message msg)
    )

(defun eos::create-sbrowser-patterns ()
  "returns list of patterns"
  (list
   (make-an-observer "SPRO_SBENG_CURRENT_ELEMENT"
		     'eos::spro_sbeng_current_element)
   (make-an-observer "SPRO_SBENG_START"
		     'eos::spro_sbeng_start)
   (make-an-observer "SPRO_SBENG_QUIT"
		     'eos::spro_sbeng_quit)
   ))

(defun eos::register-sbrowser-patterns ()
  "register all sbrowser patterns"
  (mapcar 'register-tooltalk-pattern sbrowser-pattern-list))

(defun eos::unregister-sbrowser-patterns ()
  "unregister all sbrowser patterns"
  (mapcar 'unregister-tooltalk-pattern sbrowser-pattern-list))

