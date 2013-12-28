;; Light Weight Editor Integration for Sparcworks.
;; "Era on Sparcworks" (EOS)
;;
;; Author: Eduardo Pelegri-Llopart
;;
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;; Common routines for EOS

;; screen-specific enabling
;;
;; will maintain at most one screen to debugger, one to sbrowser
;; annotations have a type, either
;;
;;	sbrowser
;;	debugger-arrow
;;	debugger-stop
;;	debugger-visit
;;
;; adding an annotation of type sbrowser will be only on screen sbrowser
;; adding an annotation of type debugger will be only on screen debugger
;;
;; turn off patterns when there is no screen.


;;;
;;; Common ToolTalk function
;;;

(defmacro make-an-observer (op callback)
   (`
    (let ((pattern-desc (` (category TT_OBSERVE
				     scope TT_SESSION
				     class TT_NOTICE
				     op (, op)
				     callback (, callback)))))
      (make-tooltalk-pattern pattern-desc))))
  

;;;
;;; Screen management
;;;

(defun eos::log (msg)
  (if (fboundp 'ut-log-text)
      (ut-log-text msg)))

(defun eos::select-sbrowser-screen (screen)
  (if (and (null eos::sbrowser-screen) screen)
      (progn
	(eos::register-sbrowser-patterns)
	(eos::log "selected screen for sbrowser"))
    (if (and (null screen) eos::sbrowser-screen)
	(progn
	  (eos::unregister-sbrowser-patterns)
	  (eos::log "unselected screen for sbrowser"))))
  (setq eos::sbrowser-screen screen)
  )

(defun eos::select-debugger-screen (screen)
  (save-excursion
    (eos::ensure-debugger-buffer)
    (bury-buffer))
  (if (and (null eos::debugger-screen) screen)
      (progn
	(eos::register-debugger-patterns)
	(eos::register-visit-file-pattern)
	(eos::log "selected screen for debugger"))
    (if (and (null screen) eos::debugger-screen)
	(progn
	  (eos::unregister-debugger-patterns)
	  (eos::unregister-visit-file-pattern)
	  (eos::log "unselected screen for debugger"))
	  ))
  (setq eos::debugger-screen screen)
  )

(setq eos::sbrowser-screen nil)
(setq eos::debugger-screen nil)

;; HERE  use file-truename

(defun eos::select-screen (type)
  "Select a screen; return nil if should skip"
  (cond ((eq type 'sbrowser) 
	 (if (live-screen-p eos::sbrowser-screen)
	     eos::sbrowser-screen
	   (message "selecting destroyed screen; will ignore")
	   (eos::select-sbrowser-screen nil)
	   nil))
	((or (eq type 'debugger-arrow)
	     (eq type 'debugger-stop)
	     (eq type 'debugger-visit))
	 (if (live-screen-p eos::debugger-screen)
	     eos::debugger-screen
	   (message "selecting destroyed screen; will ignore")
	   (eos::select-debugger-screen nil)
	   nil))
	(t (selected-screen))))

(defun eos::find-line (file line type)
  "Show FILE at LINE; returns screen or nil if inappropriate"
  ;; if type is nil
  (if (eos::null-file file)
      (selected-screen)
    (let ((sc (eos::select-screen type)))
      (if (null sc)
	  nil
	(select-screen sc)
	(find-file file)
	(goto-line line)
	sc
	))))

(defun eos::null-file (file)
  "returns t if FILE is nil or the empty string"
  (or (null file) (equal file "")))

;;;
;;; Annotation handling
;;;

(defun eos::valid-annotation (annotation)
  "returns t if ANNOTATION is an annotation and its buffer exists"
  (and (annotationp annotation)
       (bufferp (extent-buffer annotation))
       (buffer-name (extent-buffer annotation)))
  )

(defvar eos::annotation-list nil
  "list of annotations set")

(defun eos::add-to-annotation-list (ann type)
  (if (not (eq type 'debugger-stop))
      (error "not implemented"))
  (setq eos::annotation-list (cons ann
				      eos::annotation-list))
  )

(defun eos::remove-from-annotation-list (ann type)
  (if (not (eq type 'debugger-stop))
      (error "not implemented"))
  (delq ann eos::annotation-list)
  )

(defun eos::remove-all-from-annotation-list (type)
  (if (not (eq type 'debugger-stop))
      (error "not implemented"))
  (mapcar 'delete-annotation eos::annotation-list)
  (setq eos::annotation-list nil)
  )

(defun eos::add-annotation (graphics file line type face uid)
  (let ((x nil))
(save-excursion
    (if (eos::null-file file)
	nil
      (if (null (eos::find-line file line type))
	  (error "No screen to select"))
      (setq use-left-overflow t)
      (set-buffer-left-margin-width eos::left-margin-width)
      (setq x (make-annotation graphics (point) 'whitespace))
      (set-annotation-data x uid)
      (set-extent-face x face)
      (eos::add-to-annotation-list x type)
      x
)
      )))

(defun eos::compare-uid (extent uid)
  (and (annotationp extent)
       (equal (annotation-data extent) uid)
       extent))

(defun eos::delete-annotation (file line type uid)
  (let ((x nil))
    (if (eos::null-file file)
	nil
      (if (null (eos::find-line file line type))
	  (error "No screen to select"))
      (setq x (map-extents 'eos::compare-uid
			   (current-buffer)
			   (point)
			   (+ (point) 1)
			   uid))
      (if (null x)
	  nil				;  (message "Annotation not found! Ignored")
	(delete-annotation x)
	(eos::remove-from-annotation-list x type)
	)
      )))

(defmacro eos::make-visible (annotation graphics file line type face)
  (`
   (progn
     (if (eos::null-file (, file))
	 nil
       (if (null (eos::find-line (, file) (, line) (, type)))
	   (error "No screen to select"))
       (if (eos::valid-annotation (, annotation))
	   (progn
	     (save-excursion
	       (set-buffer (extent-buffer (, annotation)))
;;	       (set-buffer-left-margin-width 0)
	       )
	     (delete-annotation (, annotation))
	     )
	 (setq (, annotation) nil))
       (setq use-left-overflow t)
       (set-buffer-left-margin-width eos::left-margin-width)
       (setq (, annotation)
	     (make-annotation (, graphics) (point) 'whitespace))
       (set-annotation-data (, annotation) (, type))
       (set-extent-face (, annotation) (, face))
       ))))

(defmacro eos::make-invisible (annotation)
  (`
   (progn
     (if (eos::valid-annotation (, annotation))
	 (progn
	   (save-excursion
	     (set-buffer (extent-buffer (, annotation)))
;;	     (set-buffer-left-margin-width 0)
	     )
	   (delete-annotation (, annotation))
	   (setq (, annotation) nil)
	   )
       (setq (, annotation) nil))
     )))


(provide 'eos-common)
