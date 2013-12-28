;; Light Weight Editor Integration for Sparcworks.
;; "Era on Sparcworks" (EOS)
;;
;; Author: Eduardo Pelegri-Llopart
;;
;; Please send feedback to eduardo.pelegri-llopart@eng.sun.com

;; debugger buffer
;;

(require 'eos-common)

(defvar eos::debugger-buffer "*debugger*"
  "name of buffer where to log debugger activity; see eos::use-debugger-buffer")

(defun eos::ensure-debugger-buffer ()
  "will ensure a debugger buffer, with the proper major mode"
  (let ((buf (get-buffer eos::debugger-buffer)))
    (if buf
	(switch-to-buffer buf)
      (setq buf (get-buffer-create eos::debugger-buffer))
      (set-buffer buf)
      (eos::debugger-mode)
      (eos::insert-string-as-extent "[Debugger] " t (get-face 'bold))
      )))

(defun eos::synchronize-debugger-buffer ()
  "ensure all views of this buffer are at the end"
  (eos::ensure-debugger-buffer)
  (let ((x (point-max)))
    (goto-char x)
    (mapcar (function
	     (lambda (win)
	       (set-window-point win x)))
	    (get-buffer-window-list eos::debugger-buffer))
    ))

(defun eos::debugger-mode ()
  (interactive)
  "local mode"
  (kill-all-local-variables)    
  (setq major-mode 'eos::debugger-mode)
  (setq mode-name "eos::debugger")
  (setq truncate-lines t)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map eos::debugger-mode-map))


(defvar eos::debugger-mode-map nil)

(if eos::debugger-mode-map
    nil
  (progn
    (setq eos::debugger-mode-map (make-keymap))
    (set-keymap-name eos::debugger-mode-map 'eos::debugger-mode-map)
    (define-key eos::debugger-mode-map [(meta p)] 'eos::debugger-previous-cmd)
    (define-key eos::debugger-mode-map [(meta n)] 'eos::debugger-next-cmd)
    (define-key eos::debugger-mode-map [return] 'eos::debugger-send-cmd)
    ))


;; Handling of command lists

(defvar eos::current-command nil "Current command navigated; as an extent")
(defvar eos::last-command nil "last command sent to debugger, as an extent")

(defun eos::debugger-previous-cmd ()
  "present the previous command"
  (interactive)
  (save-excursion
    (let ((xt nil))
      (if (null eos::current-command)
	  (setq xt eos::last-command)
	(setq xt (extent-property 
		  eos::current-command
		  'previous-command)))
      (if xt
	  (progn
	    (eos::debugger-delete-last-cmd-line)
	    (goto-char (point-max))
	    (insert (buffer-substring
		     (extent-start-position xt)
		     (1- (extent-end-position xt)) ; remove <CR>
		     ))
	    (setq eos::current-command xt))
	(error "no previous command")
	))
    ))

(defun eos::debugger-next-cmd ()
  "present the next command"
  (interactive)
  (save-excursion
    (let ((xt nil))
      (if (null eos::current-command)
	  (error "no next command")
	(setq xt (extent-property 
		  eos::current-command
		  'next-command)))
      (eos::debugger-delete-last-cmd-line)
      (if xt
	  (progn
	    (goto-char (point-max))
	    (insert (buffer-substring
		     (extent-start-position xt)
		     (1- (extent-end-position xt)) ; remove <CR>
		     ))
	    (setq eos::current-command xt))
	(setq eos::current-command nil)
	))
    ))

(defun eos::debugger-delete-last-cmd-line ()
  "delete the last command line, not yet inputed, returns that cmd line"
  (goto-char (point-max))
  (let ((e (point)))
    (beginning-of-line)
    (let* ((xt (extent-at (point)))
	   (p (extent-end-position xt))
	   (str (buffer-substring p e))
	   )
      (delete-region p e)
      str
      )))

(defun eos::debugger-send-cmd ()
  "send the message in the current line"
  (interactive)
  (end-of-line)
  (let ((e (point)))
    (beginning-of-line)
    (let* ((xt (extent-at (point)))
	   (p (extent-end-position xt))
	   (str (buffer-substring p e))
	   )
      (delete-region p e)
      (eos::send-spider-current-do-msg (concat str "\n") (selected-screen))
      (goto-char (point-max))
      (setq eos::current-command nil)
      )))

;; client
;;

(defun get-buffer-window-list (buffer)
  "like get-buffer-window except that will generate a list of windows
instead of just the first one"
  (let* ((buf (get-buffer buffer))
	 (win1 (next-window nil 'foo t t))
	 (win win1)
	 (first t)
	 (ret nil)
	 )
    (if (null buf)
	nil
      (while (or
	      (and first win)
	      (not (or first (equal win win1)))
	      )
	(setq first nil)
	(if (equal
	     buf
	     (window-buffer win))
	    (setq ret (cons win ret)))
	(setq win (next-window win t t t))
	)
      ret)))


(defun eos::insert-on-debugger-buffer (msg rdonly face &optional previous-command)
  "will insert MSG at end of debugger buffer with RDONLY property and with FACE. 
If PREVIOUS-COMMAND is given, the newly created extent will be doubly linked into this one
using 'previous-command and 'next-command properties"
  (let ((buf (current-buffer))
	(xt nil))
    (eos::ensure-debugger-buffer)
    (setq xt (eos::insert-string-as-extent msg rdonly face))
    (if previous-command
	(progn
	  (set-extent-property xt 'previous-command previous-command)
	  (set-extent-property previous-command 'next-command xt)
	  ))
    (switch-to-buffer buf)
    xt
    ))

(defun eos::insert-string-as-extent (msg rdonly face)
  "insert MSG as a extent with RDONLY and FACE.  Returns the extent"
  (let ((here nil)
	(xt nil))
    (goto-char (point-max))
    (setq here (point))
    (insert msg)
    (setq xt (make-extent here (point) nil))
    (if rdonly
	(progn
	  (set-extent-property xt 'read-only t)
	  (set-extent-property xt 'duplicable nil)
	  ))
    (set-extent-face xt face)
    (eos::synchronize-debugger-buffer)
    xt
    ))


;;
;; Communication commands
;;

(defun eos::spider-do-callback (msg pat)
  "Callback after processing a spider_do request"
  (eos::insert-on-debugger-buffer
   (format "%s" (get-tooltalk-message-attribute msg 'arg_val 2))
   t
   (get-face 'bold))

  ;; this is a temporary hack - should use attribute on tt message
  (if eos::restore-screen
      (select-screen eos::restore-screen))

  (destroy-tooltalk-message msg)
  )

(defvar eos::last-command-was-print nil "(eos:: internal)")

(defun eos::spro_spider_output (msg pat)
  "For spider output"
  (eos::insert-on-debugger-buffer
   (format "%s" (get-tooltalk-message-attribute msg 'arg_val 1))
   t
   (get-face 'default))
  (let ((err (get-tooltalk-message-attribute msg 'arg_val 2)))
    (if (and err (not (string-equal err "")))
	(eos::insert-on-debugger-buffer
	 (insert (format "STDERR> %s" err))
	 t
	 (get-face 'default))
      ))
  (destroy-tooltalk-message msg))

(defun eos::spro_spider_output-common (msg pat)
  "For spider output"
  (if eos::last-command-was-print
      (eos::spro_spider_print_output msg pat)
    (eos::spro_spider_output msg pat)))

(defmacro eos::spider-tt-args (cmd spider-id clique-id)
  (` (list
      'class TT_REQUEST
      'address TT_HANDLER
      'scope TT_SESSION
      'handler (, spider-id)
      'op "SPRO_SPIDER_DO"
      'callback 'eos::spider-do-callback
      'args (list
	     (list 'TT_IN (, clique-id) "Context_ID")
	     (list 'TT_IN (, cmd) "string")
	     (list 'TT_OUT))
      )))

(defun eos::send-spider-do-msg (cmd spider-id clique-id)
  "Send CMD, a string, to SPIDER-ID, using CLIQUE-ID"
  (let ((msg (make-tooltalk-message
	      (eos::spider-tt-args cmd spider-id clique-id))))
    (setq eos::last-command
	  (eos::insert-on-debugger-buffer
	   cmd
	   t
	   (get-face 'italic)
	   eos::last-command))
    (setq eos::current-command eos::last-command)
    (send-tooltalk-message msg)
    (destroy-tooltalk-message msg)
    ))

(setq eos::restore-screen nil)		; remembers whether the cmd was typed in

(setq eos::no-connection-box
      '("Xemacs does not know the ID of a debugger to connect to.  You may need
		to reissue a debug or attach command from the debugger"
	       ["Continue" (message "continuing") t]))

(defun eos::send-spider-current-do-msg (cmd restore-screen)
  "Send CMD to the current dbx engine using the current debugger clique;
The cmd ends in a new-line. RESTORE is a screen to request reseting the selected
screen to that value "
  (setq eos::restore-screen restore-screen)
  (if (null eos::current-debugger-clique-id)
      (popup-dialog-box eos::no-connection-box)
    (eos::send-spider-do-msg cmd
			     eos::current-dbx-proc-id
			     eos::current-debugger-clique-id)))

(defun eos::dbx-cmd (arg) 
  "Send CMD to the current dbx engine using the current debugger clique;
The cmd does not end in a new-line; a new-line will be added"
  (interactive "sDbx cmd: ")
  (eos::send-spider-current-do-msg (concat arg "\n") nil))


;;
;; Extra patterns

(defun eos::debugger-extra-startup ()
  "Actions to do at startup for eos-debugger-extra.el"
    (setq eos::dbx-extra-pattern-list	; list of extra TT patterns
	  (eos::create-debugger-extra-patterns))
    (eos::ensure-available-print-screen)
    (eos::define-prefix-map)		; initialize keymap
  )

(defun eos::create-debugger-extra-patterns ()
  "returns a list of patterns"
  (list
   (make-an-observer "SPRO_SPIDER_OUTPUT" 'eos::spro_spider_output-common)
   ))

(defun eos::register-debugger-extra-patterns ()
  "register additional dbx patterns"
    (mapcar 'register-tooltalk-pattern eos::dbx-extra-pattern-list))

(defun eos::unregister-debugger-extra-patterns ()
  "unregister additional dbx patterns"
  (mapcar 'unregister-tooltalk-pattern eos::dbx-extra-pattern-list))

;;
;; Common commands
;;

(defun eos::run () (interactive) (eos::dbx-cmd "run"))
(defun eos::fix () (interactive) (eos::dbx-cmd "fix"))

(defun eos::cont () (interactive) (eos::dbx-cmd "cont"))
(defun eos::next () (interactive) (eos::dbx-cmd "next"))
(defun eos::step () (interactive) (eos::dbx-cmd "step"))
(defun eos::step-up () (interactive) (eos::dbx-cmd "step up"))

(defun eos::up () (interactive)  (eos::dbx-cmd "up" ))
(defun eos::down () (interactive) (eos::dbx-cmd "down"))
(defun eos::pop () (interactive) (eos::dbx-cmd "pop"))

(defun eos::stop-at ()
  (interactive)
  (eos::dbx-cmd
   (format "stop at \"%s\":%d"
	   (buffer-file-name)
	   (eos::line-at (point)))
   ))

(defun eos::clear-at ()
  (interactive)
  (eos::dbx-cmd
  (format "clear \"%s\":%d"
   (buffer-file-name)
   (eos::line-at (point))
   )))

(defun eos::stop-in ()
  (interactive)
  (eos::dbx-cmd
   (format "stop in %s"
	   (buffer-substring (point) (mark)))
   ))

(defun eos::func ()
  (interactive)
  (eos::dbx-cmd
   (format "func %s"
	   (buffer-substring (point) (mark)))
   ))

(defun eos::cont-to ()
  (interactive)
  (eos::dbx-cmd
   (format "stop at \"%s\":%d -temp; cont"
	(buffer-file-name)
	(eos::line-at (point))
	)))


;; these need to look around & also preserve selection

(defun eos::send-spider-print-msg (expr)
  "Print EXPR using separate screen"
  (setq eos::last-command-was-print t)
  (eos::dbx-cmd (format "print %s" expr)))

(defun eos::send-spider-print*-msg (expr)
  "Send *EXPR using separate screen"
  (setq eos::last-command-was-print t)
  (eos::dbx-cmd (format "print *(%s)" expr)))

(defun eos::print () (interactive)
 (eos::send-spider-print-msg
  (buffer-substring (point) (mark))))

(defun eos::print* () (interactive)
 (eos::send-spider-print*-msg
  (buffer-substring (point) (mark))))


;;
;;
;; Print on separate screen


(defun eos::buffer-line-size (buffer)
  (interactive)
  (or (bufferp buffer)
      (setq buffer (current-buffer)))
  (save-excursion
    (switch-to-buffer buffer)
    (eos::line-at (point-max))))

;;
;; Handling of a collection of print screens
;; (currently only one)

(defun eos::new-available-print-screen()
  "returns an available print screen"
  ;; currently just returns the one screen
  (let ((scr (selected-screen))
	(buf (current-buffer)))

    ;; create screens
    (if (live-screen-p eos::print-screen)
	(progn
	  (make-screen-visible eos::print-screen)
	  eos::print-screen)
      (setq eos::print-screen (make-screen))
      ;; no modeline visible...
      (set-face-background 'modeline 
			   (face-background (get-face 'default))
			   eos::print-screen)
      (set-face-foreground 'modeline 
			   (face-background (get-face 'default))
			   eos::print-screen)
      ;; there is redundancy below.
      (select-screen eos::print-screen)
      (switch-to-buffer eos::print-buffer)
      (set-buffer-menubar '(["Dismiss" (eos::dismiss-print-screen) t]))
      (select-screen scr)
      (switch-to-buffer buf)
      eos::print-screen
      )))

(defun eos::ensure-available-print-screen ()
  "ensures that there is at least one available print screen"
  t)

(defun eos::show-print-screen ()
  (interactive)
  (setq eos::print-screen (eos::new-available-print-screen))
  (select-screen eos::print-screen)
  (switch-to-buffer eos::print-buffer)
  (set-screen-height eos::print-screen
		     (+ 1 (eos::buffer-line-size eos::print-buffer)))
  (goto-char (point-min))
    )

(defun eos::dismiss-print-screen ()
  (interactive)
  (make-screen-invisible eos::print-screen)
  (select-screen (car (visible-screen-list))))

(defvar eos::print-screen nil "Screen for prints")
(defvar eos::print-buffer " *print output*" "Buffer for prints")

;;
;; print output
;;

(defun eos::spro_spider_print_output (msg pat)
  "For spider print output (switched with spro_spider_output"
  (let ((buf (current-buffer))
	(scr (selected-screen)))
    (save-excursion			; does not work in callbacks?
      (switch-to-buffer eos::print-buffer)
      (delete-region (point-min) (point-max))
      (goto-char (point-max))
      (insert (format "%s" (get-tooltalk-message-attribute msg
							   'arg_val 1)))
      (let ((err (get-tooltalk-message-attribute msg
						 'arg_val 2)))
	(if (and err (not (string-equal err "")))
	    (insert (format "STDERR> %s" err))))
      (eos::show-print-screen)
      (select-screen scr)
      (switch-to-buffer buf)
      )
    (destroy-tooltalk-message msg)
    (setq eos::last-command-was-print nil)
    ))


;; User interface

(defvar eos::prefix-map (make-keymap))

(defun eos::define-prefix-map ()

  (define-key eos::prefix-map "%" 'eos::dbx-cmd)
  (define-key eos::prefix-map "r" 'eos::run)
  (define-key eos::prefix-map "f" 'eos::fix)

  (define-key eos::prefix-map "p" 'eos::print)
  (define-key eos::prefix-map "\C-p" 'eos::print*)

  (define-key eos::prefix-map "c" 'eos::cont)
  (define-key eos::prefix-map "b" 'eos::stop-at)
  (define-key eos::prefix-map "\C-b" 'eos::clear-at)

  (define-key eos::prefix-map "n" 'eos::next)
  (define-key eos::prefix-map "s" 'eos::step)
  (define-key eos::prefix-map "\C-s" 'eos::step-up)

  (define-key eos::prefix-map "u" 'eos::up)
  (define-key eos::prefix-map "d" 'eos::down)

)

(defvar eos::key-mode 'none "Style of key mode interaction for Eos")

(defun eos::set-key-mode (mode)
  "Set the key MODE to either 'none, 'prefix, or 'function"
  (setq eos::key-mode mode)
  (cond
   ((eq eos::key-mode 'none)
    (define-key global-map "\C-cd" nil)
    (eos::remove-function-keys)
    (add-menu nil "SPARCworks" eos::short-menu)
    )
   ((eq eos::key-mode 'prefix)
    (define-key global-map "\C-cd" eos::prefix-map)
    (eos::remove-function-keys)
    (add-menu nil "SPARCworks" eos::long-menu)
    )
   ((eq eos::key-mode 'function)
    (define-key global-map "\C-cd" nil)
    (eos::add-function-keys)
    (add-menu nil "SPARCworks" eos::long-menu)
    )
   (t
    (error "unimplemented")
    )))

(defun eos::add-function-keys ()
  (interactive)

  ;;
  (global-set-key [f6] 'eos::dbx-cmd)
  (global-set-key [(control f6)] 'eos::run)
  (global-set-key [(shift f6)] 'eos::fix)
  ;;
  (global-set-key [f7] 'eos::print)
  (global-set-key [(control f7)] 'eos::print*)
  (global-set-key [(shift f7)] 'eos::dismiss-print-screen)
  ;;
  (global-set-key [f8] 'eos::cont)
  (global-set-key [(control f8)] 'eos::stop-at)
  (global-set-key [(shift f8)] 'eos::clear-at)
  ;;
  (global-set-key [f9] 'eos::next)
  (global-set-key [(control f9)] 'eos::step)
  (global-set-key [(shift f9)] 'eos::step-up)
  ;;
  )

(defun eos::remove-function-keys ()
  (interactive)

  ;;
  (global-set-key [f6] nil)
  (global-set-key [(control f6)] nil)
  (global-set-key [(shift f6)] nil)
  ;;
  (global-set-key [f7] nil)
  (global-set-key [(control f7)] nil)
  (global-set-key [(shift f7)] nil)
  ;;
  (global-set-key [f8] nil)
  (global-set-key [(control f8)] nil)
  (global-set-key [(shift f8)] nil)
  ;;
  (global-set-key [f9] nil)
  (global-set-key [(control f9)] nil)
  (global-set-key [(shift f9)] nil)
  ;;
  )

;; Provides popup access

(defvar eos::popup-mode nil)
(defvar eos::saved-global-popup-menu nil)

(defun eos::toggle-popup-menu ()
  "Toggle whether to use or not popup menus for SPARCworks"
  (interactive)
  (if eos::popup-mode
      (setq global-popup-menu eos::saved-global-popup-menu)
    (eos::push-popup-menu))
  (setq eos::popup-mode (null eos::popup-mode))
  )

(defun eos::push-popup-menu ()
  (setq eos::saved-global-popup-menu global-popup-menu)
  (setq global-popup-menu
	(append
	 '("SPARCworks Command"
	   ["Stop At" eos::stop-at t]
	   ["Clear At" eos::clear-at t]
	   ["Stop In" eos::stop-in t]
	   ["Cont To" eos::cont-to t]
	   ["Print" eos::print t]
	   ["Print*" eos::print* t]
	   "---"
	   ["Do" eos::dbx-cmd t]
	   "---")
	 (list
	  eos::saved-global-popup-menu))
	))
