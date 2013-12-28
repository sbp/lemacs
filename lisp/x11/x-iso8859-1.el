;; Mapping between X keysym names and ISO 8859-1 (aka Latin1) character codes.
;; Copyright (C) 1992 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; created by jwz, 13-jun-92.

;; Under X, when the user types a character that is ISO-8859/1 but not ASCII,
;; it comes in as a symbol instead of as a character code.  This keeps things
;; nice and character-set independent.  This file takes all of those symbols
;; (the symbols that are the X names for the 8859/1 characters) and puts a
;; property on them which holds the character code that should be inserted in
;; the buffer when they are typed.  The self-insert-command function will look
;; at this.  It also binds them all to self-insert-command.

;; It puts the same property on the keypad keys, so that (read-char) will
;; think that they are the same as the digit characters.  However, those
;; keys are bound to one-character keyboard macros, so that `kp_9' will, by
;; default, do the same thing that `9' does, in whatever the current mode is.

;; The standard case and syntax tables are set in prim/iso8859-1.el, since
;; that is not X-specific.

(require 'iso8859-1)

;; (This esoteric little construct is how you do MACROLET in elisp.  It
;; generates the most efficient code for the .elc file by unwinding the
;; loop at compile-time.)

((macro
  . (lambda (&rest syms-and-iso8859/1-codes)
      (cons
       'progn
       (nconc
	;;
	;; First emit code that puts the `x-iso8859/1' property on all of
	;; the keysym symbols.
	;; 
	(mapcar '(lambda (sym-and-code)
		   (list 'put (list 'quote (car sym-and-code))
			 ''x-iso8859/1 (car (cdr sym-and-code))))
		syms-and-iso8859/1-codes)
	;;
	;; Then emit code that binds all of those keysym symbols to
	;; `self-insert-command'.
	;; 
	(mapcar '(lambda (sym-and-code)
		   (list 'global-set-key (list 'quote (car sym-and-code))
			 ''self-insert-command))
		syms-and-iso8859/1-codes)
	))))

 ;; The names and capitalization here are as per the MIT X11R4 and X11R5
 ;; distributions.  If a vendor varies from this, adjustments will need
 ;; to be made...

 (nobreakspace		?\240)
 (exclamdown		?\241)
 (cent			?\242)
 (sterling		?\243)
 (currency		?\244)
 (yen			?\245)
 (brokenbar		?\246)
 (section 		?\247)
 (diaeresis		?\250)
 (copyright		?\251)
 (ordfeminine		?\252)
 (guillemotleft		?\253)
 (notsign		?\254)
 (hyphen		?\255)
 (registered		?\256)
 (macron		?\257)
 (degree		?\260)
 (plusminus		?\261)
 (twosuperior		?\262)
 (threesuperior		?\263)
 (acute			?\264)	; Why is there an acute keysym that is 
 (mu			?\265)	; distinct from apostrophe/quote, but 
 (paragraph		?\266)	; no grave keysym that is distinct from
 (periodcentered	?\267)	; backquote?
 (cedilla		?\270)
 (onesuperior		?\271)
 (masculine		?\272)
 (guillemotright	?\273)
 (onequarter		?\274)
 (onehalf		?\275)
 (threequarters		?\276)
 (questiondown		?\277)

 (Agrave		?\300)
 (Aacute		?\301)
 (Acircumflex		?\302)
 (Atilde		?\303)
 (Adiaeresis		?\304)
 (Aring			?\305)
 (AE			?\306)
 (Ccedilla		?\307)
 (Egrave		?\310)
 (Eacute		?\311)
 (Ecircumflex		?\312)
 (Ediaeresis		?\313)
 (Igrave		?\314)
 (Iacute		?\315)
 (Icircumflex		?\316)
 (Idiaeresis		?\317)
 (ETH			?\320)
 (Ntilde		?\321)
 (Ograve		?\322)
 (Oacute		?\323)
 (Ocircumflex		?\324)
 (Otilde		?\325)
 (Odiaeresis		?\326)
 (multiply		?\327)
 (Ooblique		?\330)
 (Ugrave		?\331)
 (Uacute		?\332)
 (Ucircumflex		?\333)
 (Udiaeresis		?\334)
 (Yacute		?\335)
 (THORN			?\336)
 (ssharp		?\337)

 (agrave		?\340)
 (aacute		?\341)
 (acircumflex		?\342)
 (atilde		?\343)
 (adiaeresis		?\344)
 (aring			?\345)
 (ae			?\346)
 (ccedilla		?\347)
 (egrave		?\350)
 (eacute		?\351)
 (ecircumflex		?\352)
 (ediaeresis		?\353)
 (igrave		?\354)
 (iacute		?\355)
 (icircumflex		?\356)
 (idiaeresis		?\357)
 (eth			?\360)
 (ntilde		?\361)
 (ograve		?\362)
 (oacute		?\363)
 (ocircumflex		?\364)
 (otilde		?\365)
 (odiaeresis		?\366)
 (division		?\367)
 (oslash		?\370)
 (ugrave		?\371)
 (uacute		?\372)
 (ucircumflex		?\373)
 (udiaeresis		?\374)
 (yacute		?\375)
 (thorn			?\376)
 (ydiaeresis		?\377)

 ;; Do the same voodoo for the keypad keys.  I used to bind these to keyboard
 ;; macros (for instance, kp_0 was bound to "0") so that they would track the
 ;; bindings of the corresponding keys by default, but that made the display
 ;; of M-x describe-bindings much harder to read, so now we'll just bind them
 ;; to self-insert by default.  Not a big difference...
 
 (kp_0			?0)
 (kp_1			?1)
 (kp_2			?2)
 (kp_3			?3)
 (kp_4			?4)
 (kp_5			?5)
 (kp_6			?6)
 (kp_7			?7)
 (kp_8			?8)
 (kp_9			?9)
 (kp_space		? )
 (kp_tab		?\t)
 (kp_enter		?\r)
 (kp_equal		?=)
 (kp_multiply		?*)
 (kp_add		?+)
 (kp_separator		?,)
 (kp_subtract		?-)
 (kp_decimal		?.)
 (kp_divide		?/)
 )

((macro . (lambda (&rest syms-and-iso8859/1-codes)
	    (cons 'progn
		  (mapcar '(lambda (sym-and-code)
			     (list 'put (list 'quote (car sym-and-code))
				   ''x-iso8859/1 (car (cdr sym-and-code))))
			  syms-and-iso8859/1-codes))))
 ;;
 ;; Let's do the appropriate thing for some vendor-specific keysyms too...
 ;; Apparently nobody agrees on what the names of these keysyms are.
 ;;
 (SunFA_Acute		?\264)
 (SunXK_FA_Acute	?\264)
 (Dacute_accent		?\264)
 (DXK_acute_accent	?\264)
 (hpmute_acute		?\264)
 (hpXK_mute_acute	?\264)
 (XK_mute_acute		?\264)

 (SunFA_Grave		 ?`)
 (Dead_Grave		 ?`)
 (SunXK_FA_Grave	 ?`)
 (Dgrave_accent		 ?`)
 (DXK_grave_accent	 ?`)
 (hpmute_grave		 ?`)
 (hpXK_mute_grave	 ?`)
 (XK_mute_grave		 ?`)

 (SunFA_Cedilla		?\270)
 (SunXK_FA_Cedilla	?\270)
 (Dcedilla_accent	?\270)
 (DXK_cedilla_accent	?\270)

 (SunFA_Diaeresis	?\250)
 (SunXK_FA_Diaeresis	?\250)
 (hpmute_diaeresis	?\250)
 (hpXK_mute_diaeresis	?\250)
 (XK_mute_diaeresis	?\250)

 (SunFA_Circum		 ?^)
 (Dead_Circum		 ?^)
 (SunXK_FA_Circum	 ?^)
 (Dcircumflex_accent	 ?^)
 (DXK_circumflex_accent	 ?^)
 (hpmute_asciicircum	 ?^)
 (hpXK_mute_asciicircum	 ?^)
 (XK_mute_asciicircum	 ?^)

 (SunFA_Tilde		 ?~)
 (Dead_Tilde		 ?~)
 (SunXK_FA_Tilde	 ?~)
 (Dtilde		 ?~)
 (DXK_tilde		 ?~)
 (hpmute_asciitilde	 ?~)
 (hpXK_mute_asciitilde	 ?~)
 (XK_mute_asciitilde	 ?~)

 (Dring_accent		?\260)
 (DXK_ring_accent	?\260)
 )

;;; And do it to the builtins, just to make describe-bindings need to do 
;;; less work.
;;;
(put 'backspace 'x-iso8859/1 ?\b)
(put 'tab	'x-iso8859/1 ?\t)
(put 'linefeed	'x-iso8859/1 ?\n)
(put 'return	'x-iso8859/1 ?\r)
(put 'escape	'x-iso8859/1  27)
(put 'space	'x-iso8859/1  ? )
(put 'delete	'x-iso8859/1 ?\177)

(provide 'x-iso8859-1)
