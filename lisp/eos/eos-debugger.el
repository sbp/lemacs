;; Light Weight Editor Integration for Sparcworks.
;; "Era on Sparcworks" (EOS)
;;
;; Author: Eduardo Pelegri-Llopart
;;
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

(require 'eos-common)

;;; =================
;;; debugger protocol
;;; =================

(setq current-hollow-arrow nil)
(setq current-solid-arrow nil)

(defun eos::debugger-startup ()
  "Actions to do at startup for eos-debugger.el"
  (make-face 'stop-face)
  (make-face 'solid-arrow-face)
  (make-face 'hollow-arrow-face)
  
  (set-face-foreground 'stop-face eos::stop-color)
  (set-face-background 'stop-face 
		       (face-background (get-face 'default)))
  (set-face-foreground 'solid-arrow-face eos::solid-arrow-color)
  (set-face-background 'solid-arrow-face 
		       (face-background (get-face 'default)))
  (set-face-foreground 'hollow-arrow-face eos::hollow-arrow-color)
  (set-face-background 'hollow-arrow-face 
		       (face-background (get-face 'default)))

  (setq eos::dbx-pattern-list		; list of dbx TT patterns
	(eos::create-debugger-patterns))

  (setq solid-arrow			; pixmap for solid arrow, et al.
	(make-pixmap "solid-arrow.xbm"))
  (setq hollow-arrow
	(make-pixmap "hollow-arrow.xbm"))
  (setq stop
	(make-pixmap "stop.xbm"))

  ;; temporarily fixes redisplay bug
  (set-pixmap-contributes-to-line-height stop nil)
  (set-pixmap-contributes-to-line-height solid-arrow nil)
  (set-pixmap-contributes-to-line-height hollow-arrow nil)
)

(defun eos::spro_te_eventset (msg pat)
  ;; thread_id trap_id string string filename lineno string string
  (let* ((trap-id
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (filename
	  (get-tooltalk-message-attribute msg 'arg_val 4))
	 (lineno
	  (read (get-tooltalk-message-attribute msg 'arg_ival 5))))
    (eos::add-annotation stop filename lineno 'debugger-stop 'stop-face trap-id)
    (return-tooltalk-message msg)
    ))

(defun eos::spro_te_eventdel (msg pat)
  ;; trap_id string string filename lineno string string
  (let* ((trap-id
	  (get-tooltalk-message-attribute msg 'arg_val 0))
	 (filename
	  (get-tooltalk-message-attribute msg 'arg_val 3))
	 (lineno
	  (read (get-tooltalk-message-attribute msg 'arg_ival 4))))
    (eos::delete-annotation filename lineno 'debugger-stop trap-id)
    (return-tooltalk-message msg)
    ))

(defun eos::spro_te_stopped (msg pat)
  ;; thread_id filename procname lineno filename procname lineno
  (let* ((filename-hollow
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (lineno-hollow
	  (read (get-tooltalk-message-attribute msg 'arg_ival 3)))
	 (filename-solid
	  (get-tooltalk-message-attribute msg 'arg_val 4))
	 (lineno-solid
	  (read (get-tooltalk-message-attribute msg 'arg_ival 6)))
	 )
    (eos::make-visible current-solid-arrow solid-arrow
			  filename-solid lineno-solid 'debugger-arrow
			  'solid-arrow-face)
    (if (or (not (equal filename-solid filename-hollow))
	    (not (equal lineno-solid lineno-hollow)))
	(eos::make-visible current-hollow-arrow hollow-arrow
			      filename-hollow lineno-hollow 'debugger-arrow
			      'hollow-arrow-face))
    (return-tooltalk-message msg)
    ))

;; Tracking current id's
;;

(defvar eos::current-dbx-proc-id
  nil
  "TT id for the current dbx")

(defvar eos::current-debugger-clique-id
  nil
  "Clique_ID for the current debugger/dbx")

(defun eos::update-dbx-proc-id (msg)
  (setq eos::current-dbx-proc-id
	(get-tooltalk-message-attribute msg 'sender))
  )

(defun eos::update-current-debugger-clique-id (msg)
  (setq eos::current-debugger-clique-id
	(get-tooltalk-message-attribute msg 'arg_val 0))
  )

;;
;; Updating arrows
;;

(defun eos::show_no_arrows (msg pat)
  (eos::make-invisible current-hollow-arrow)
  (eos::make-invisible current-solid-arrow)
  (return-tooltalk-message msg)
  )

(defun eos::update-and-show_no_arrows_no_stops (msg pat)
  (eos::update-dbx-proc-id msg)
  (eos::update-current-debugger-clique-id msg)
  (eos::show_no_arrows_no_stops msg pat)
  )

(defun eos::show_no_arrows_no_stops (msg pat)
  (eos::update-dbx-proc-id msg)
  (eos::make-invisible current-hollow-arrow)
  (eos::make-invisible current-solid-arrow)
  (eos::remove-all-from-annotation-list 'debugger-stop)
  (return-tooltalk-message msg)
  )

(defun eos::spro_te_location (msg pat)
  ;; thread_id filename procname lineno filename procname lineno
  (let* ((filename-hollow
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (lineno-hollow
	  (read (get-tooltalk-message-attribute msg 'arg_ival 3)))
	 (filename-solid
	  (get-tooltalk-message-attribute msg 'arg_val 4))
	 (lineno-solid
	  (read (get-tooltalk-message-attribute msg 'arg_ival 6)))
	 )
    (eos::make-visible current-solid-arrow solid-arrow
			  filename-solid lineno-solid 'debugger-arrow
			  'solid-arrow-face)
    (if (or (not (equal filename-solid filename-hollow))
	    (not (equal lineno-solid lineno-hollow)))
	(eos::make-visible current-hollow-arrow hollow-arrow
			      filename-hollow lineno-hollow 'debugger-arrow
			      'hollow-arrow-face))
    (return-tooltalk-message msg)
    ))

(defun eos::spro_te_visit (msg pat)
  ;; thread_id filename procname lineno stackpos
  (let* ((filename
	  (get-tooltalk-message-attribute msg 'arg_val 1))
	 (lineno
	  (read (get-tooltalk-message-attribute msg 'arg_ival 3)))
	 (stackpos
	  (read (get-tooltalk-message-attribute msg 'arg_ival 4)))
	 )
    (eos::make-invisible current-hollow-arrow)
    (if (not (equal stackpos 1))
	(eos::make-visible current-hollow-arrow hollow-arrow
			      filename lineno 'debugger-arrow
			      'hollow-arrow-face)
      (if (null (eos::find-line filename lineno 'debugger-arrow))
	  (error "No screen to select"))
      )
    (return-tooltalk-message msg)
    ))

;; generate a list of patterns
;; so it can be registered and unregistered.


(defun eos::create-debugger-patterns ()
  "returns a list of patterns"
  (list
   (make-an-observer "SPRO_TE_STOPPED" 'eos::spro_te_stopped)
   (make-an-observer "SPRO_SE_STARTED" 'eos::show_no_arrows)
   (make-an-observer "SPRO_TE_STEPPED" 'eos::show_no_arrows)
   (make-an-observer "SPRO_TE_CONTINUED" 'eos::show_no_arrows)
   (make-an-observer "SPRO_SE_DROPPED" 'eos::show_no_arrows_no_stops)
   (make-an-observer "SPRO_SE_DEBUGGED" 'eos::update-and-show_no_arrows_no_stops)
   (make-an-observer "SPRO_SE_REVIVED" 'eos::update-and-show_no_arrows_no_stops)
   (make-an-observer "SPRO_SE_ATTACHED" 'eos::update-and-show_no_arrows_no_stops)
   (make-an-observer "SPRO_SE_GONE" 'eos::show_no_arrows)
   (make-an-observer "SPRO_TE_LOCATION" 'eos::spro_te_location)
   (make-an-observer "SPRO_TE_VISIT" 'eos::spro_te_visit)
   (make-an-observer "SPRO_TE_EVENTSET" 'eos::spro_te_eventset)
   (make-an-observer "SPRO_TE_EVENTDEL" 'eos::spro_te_eventdel)
   ))

(defun eos::register-debugger-patterns ()
  "register all dbx patterns"
  (mapcar 'register-tooltalk-pattern eos::dbx-pattern-list)
  (eos::register-debugger-extra-patterns))

(defun eos::unregister-debugger-patterns ()
  "unregister all dbx patterns"
  (mapcar 'unregister-tooltalk-pattern eos::dbx-pattern-list)
  (eos::unregister-debugger-extra-patterns))


