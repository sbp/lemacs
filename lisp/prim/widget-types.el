(begin-xtfunc-conversion "../../src/xtfunc-def.h" "../../src/xtfunc-sym.h")

;;; typedefs

(typedef 'boolean)
(typedef 'callback 'data)
(typedef 'enum 'data)
(typedef 'event)
(typedef 'font)
(typedef 'function 'data)
(typedef 'int)
(typedef 'list 'slot 'stage2 'data)
(typedef 'none 'no-l2c 'no-c2l)
(typedef 'pixel 'slot 'stage2)
(typedef 'pixmap 'slot 'stage2 'data)
(typedef 'screen)
(typedef 'set 'data)
(typedef 'short)
(typedef 'string)
(typedef 'unsigned-int)
(typedef 'unsigned-short)
(typedef 'widget 'data)

;;; type-equivalences

(type-equiv 'cardinal 'unsigned-int)
(type-equiv 'dimension 'unsigned-short)
(type-equiv 'position 'short)
(type-equiv 'time 'unsigned-int)

(type-equiv 'operation
	    '(set drop-move
		  drop-copy
		  drop-link))
(type-equiv 'reason
	    '(enum cr-drag-motion
		   cr-drop-finish
		   cr-drop-site-enter
		   cr-drop-site-leave
		   cr-drop-start
		   cr-operation-changed
		   cr-top-level-enter
		   cr-top-level-leave
		   cr-drop-site-enter-message
		   cr-drop-site-leave-message
		   cr-drag-motion-message
		   cr-operation-changed-message
		   cr-drop-message))
(type-equiv 'drop-site-status
	    '(enum drop-site-valid
		   drop-site-noop
		   drop-site-invalid))
(type-equiv 'drop-action
	    '(enum drop
		   drop-cancel
		   drop-help
		   drop-interrupt))

;;; structure definitions

(typedef-struct 'reason-struct
		'((reason reason)
		  (event event)
		  (time-stamp time)))
(typedef-struct 'reason-operation-struct
		'(reason-struct
		  (operation operation)
		  (operations operation)
		  (drop-site-status drop-site-status)))
(typedef-struct 'reason-operation-action-struct
		'(reason-operation-struct
		  (drop-action drop-action)))
(typedef-struct 'position-struct
		'((x position)
		  (y position)))
(typedef-struct 'reason-operation-action-position-struct
		'(reason-operation-action-struct
		  position-struct))
(typedef-struct 'reason-operation-position-struct
		'(reason-operation-struct
		  position-struct))
(typedef-struct 'reason-window-struct
		'(reason-struct
		  (screen x-screen)
		  (window screen)))

;;; widget definitions

(defwidget 'core nil
  '((accelerators none)
    ((ancestor-sensitive Sensitive) boolean (g))
    (background pixel)
    ((background-pixmap Pixmap) pixmap)
    (border-color pixel)
    ((border-pixmap Pixmap) pixmap)
    (border-width dimension)
    (colormap none (c g))
    (depth int (c g))
    ((destroy-callback Callback) callback (c))
    (height dimension)
    (initial-resources-persistent boolean (c))
    (mapped-when-managed boolean)
    (screen none (c g))
    (sensitive boolean)
    (translations none)
    (width dimension)
    ((x Position) position)
    ((y Position) position)
    ))

(defwidget 'object nil
  '(((destroy-callback Callback) (list function) (c))
    ))

(defwidget 'xm-display nil
  '((default-virtual-bindings none (c g))
    (drag-initiator-protocol-style
     (enum drag-preregister
	   drag-dynamic
	   drag-none
	   drag-drop-only
	   drag-prefer-dynamic
	   drag-prefer-preregister
	   drag-prefer-receiver)
     (c g))
    (drag-receiver-protocol-style
     (enum drag-preregister
	   drag-dynamic
	   drag-none
	   drag-drop-only
	   drag-prefer-dynamic
	   drag-prefer-preregister)
     (c g))
    ))

(defwidget 'xm-drag-context nil
  '((blend-model (enum blend-all
		       blend-state-source
		       blend-just-source
		       blend-none))
    (client-data any)
    (convert-proc function)
    (cursor-background pixel)
    (cursor-foreground pixel)
    ((drag-drop-finish-callback Callback)
     (callback reason-struct))
    ((drag-motion-callback Callback)
     (callback reason-operation-position-struct) (c))
    (drag-operations operation (c))
    ((drop-finish-callback Callback)
     (callback (reason-operation-action-struct
		(completion-status (enum drop-success drop-failure))))
     (c))
    ((drop-site-enter-callback Callback)
     (callback reason-operation-position-struct) (c))
    ((drop-site-leave-callback Callback)
     (callback reason-struct) (c))
    ((drop-start-callback Callback)
     (callback reason-operation-action-position-struct (c)))
    (export-targets (list atom) nil "XmNnumExportTargets")
    (incremental boolean)
    ((invalid-cursor-foreground Cursor-foreground) pixel)
    ((none-cursor-foreground Cursor-foreground) pixel)
    ((operations-changed-callback Callback)
     (callback reason-operation-struct) (c))
    (operation-cursor-icon (widget xm-drag-icon))
    (source-cursor-icon (widget xm-drag-icon))
    (source-pixmap-icon (widget xm-drag-icon))
    (state-cursor-icon (widget xm-drag-icon))
    ((top-level-enter-callback Callback)
     (callback (reason-window-struct
		position-struct
		(drag-protocol-style (enum drag-drop-only
					   drag-dynamic
					   drag-none
					   drag-preregister))))
     (c))
    ((top-level-leave-callback Callback)
     (callback reason-window-struct) (c))
    ((valid-cursor-foreground Cursor-foreground) pixel)
    ) '(s))

(defwidget 'xm-drag-icon nil
  '((attachment (enum attach-north-west
		      attach-north attach-north-east
		      attach-east
		      attach-south-east
		      attach-south
		      attach-south-west
		      attach-west
		      attach-center
		      attach-hot))
    ((offset-x Offset) position)
    ((offset-y Offset) position)
    (pixmap pixmap)
    )
  '(s))

(defwidget 'xm-drop-site nil
  '((animation-mask pixmap)
    ;(animation-pixmap pixmap)
    ;(animation-pixmap-depth int)
    (animation-style (enum drag-under-highlight
			   drag-under-shadow-out
			   drag-under-shadow-in
			   drag-under-pixmap
			   drag-under-none))
    (drag-proc (function !!!!!!!!!!!!!!!))
    (drop-proc (function
	       (list (callback
		      (reason reason)
		      (event event)
		      (timeStamp time)
		      (dragContext widget)
		      (x position)
		      (y position)
		      (dropSiteStatus drop-site-status)
		      (operation operation)
		      (operations operation)
		      (dropAction (enum drop drop-help))))))
    (drop-rectangles (list rectangle) nil "&xmds0")
    (drop-site-activity (enum drop-site-active
			      drop-site-inactive))
    (drop-site-operations operation)
    (drop-site-type (enum drop-site-simple
			  drop-site-composite)
		    (c g))
    (import-targets (list atom "XmN" nil "&xmds1")
    )))

(defwidget 'xm-drop-transfer nil
  '((drop-transfers (list drop-transfer-entry-rec) (c g) "XmNnumDropTransfers")
    (incremental boolean)
    (transfer-proc (function !!!!!!!!!!!!!!!!))
    (transfer-status (enum transfer-success
			   transfer-failure))
    ))

(defwidget 'xm-screen nil
  '((dark-threshold int (c))
    (default-copy-cursor-icon (widget xm-drag-icon))
    (default-invalid-cursor-icon (widget xm-drag-icon))
    (default-link-cursor-icon (widget xm-drag-icon))
    (default-move-cursor-icon (widget xm-drag-icon))
    (default-none-cursor-icon (widget xm-drag-icon))
    (default-source-cursor-icon (widget xm-drag-icon))
    (default-valid-cursor-icon (widget xm-drag-icon))
    (font font)
    (foreground-threshold int (c))
    (horizontal-font-unit int)
    (light-threshold int (c))
    ((menu-cursor Cursor) string (c))
    (move-opaque boolean)
    (unpost-behavior (enum unpost-and-replay
			   unpost))
    (vertical-font-unit int)
    ))

(end-xtfunc-conversion)
