; If you load this in your .emacs, write-file will be redefined to do
; what is described below.  If you like it please tell us so
; that we could provide it by default.
; 
; The function implements semantic 1.  Comment out the lines marked -2
; to get semantic 2 and comment out the lines marked -3 to get semantic 3.
; 
; semantic 1: If it is assumed that C-x C-w _should_ rename the
;             buffer as well as writing to the _new_ file, 
;             Energize is concerned, this should look as if I had copied the
;             buffer, reverted the original to the disk image, and killed the
;             reverted buffer, leaving only the new buffer without
;             Energize dependencies or attachments.
; 
; semantic 2: ... I would
;             either want it to simply create the new file, leaving the buffer
;             alone...
; 
; semantic 3: ... or second best creating the file and a new buffer for it,
;             so that I had two buffers, one for each named version of the
;             file.


(defun energize-write-file (filename)
  "Write the current buffer into file FILENAME.
Revert and kill the current buffer and replaces it by a new buffer 
showing FILENAME."
  (interactive
   (list (if buffer-file-name
	     (read-file-name "Energize write file: "
			     nil nil nil nil)
	   (read-file-name "Energize write file: "
			   (cdr (assq 'default-directory
				      (buffer-local-variables)))
			   nil nil (buffer-name)))))
  (if (and (file-exists-p filename)
	   (not
	    (yes-or-no-p (format "File %s already exists.  Overwrite it? "
				 filename))))
      (error "Aborted"))
  (write-region (point-min) (point-max) filename nil nil)
  (if buffer-file-name			; -2   -3
      (revert-buffer t t))		; -2   -3
  (kill-buffer nil)			; -2   -3
  (set-window-buffer			; -2
   (selected-window)			; -2 
   (find-file-noselect filename))	; -2 
  )  


; Pick just one of the following
;   This uses the new function for all buffers
(define-key ctl-x-map '(control w) 'energize-write-file)
;   This preserves old behavior for non-energize buffers
; (energize-advise-function 'write-file)

