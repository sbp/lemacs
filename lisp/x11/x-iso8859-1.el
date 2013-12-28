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

;; (This esoteric little construct is how you do MACROLET in elisp.  It
;; generates the most efficient code for the .elc file by unwinding the
;; loop at compile-time.)

((macro . (lambda (&rest syms-and-iso8859/1-codes)
	    (cons 'progn
		  (nconc 
		   (mapcar '(lambda (sym-and-code)
			      (list 'put (list 'quote (car sym-and-code))
				    ''x-iso8859/1 (car (cdr sym-and-code))))
			   syms-and-iso8859/1-codes)
		   (mapcar '(lambda (sym-and-code)
			      (list 'global-set-key
				    (list 'quote (car sym-and-code))
				    ''self-insert-command))
			   syms-and-iso8859/1-codes)))))

 ;; The names and capitalization here are as per the MIT X11R4 and X11R5
 ;; distributions.  If a vendor varies from this, adjustments will need
 ;; to be made...

 (nobreakspace		160)
 (exclamdown		161)
 (cent			162)
 (sterling		163)
 (currency		164)
 (yen			165)
 (brokenbar		166)
 (section 		167)
 (diaeresis		168)
 (copyright		169)
 (ordfeminine		170)
 (guillemotleft		171)
 (notsign		172)
 (hyphen		173)
 (registered		174)
 (macron		175)
 (degree		176)
 (plusminus		177)
 (twosuperior		178)
 (threesuperior		179)
 (acute			180)	; Why is there an acute keysym that is 
 (mu			181)	; distinct from apostrophe/quote, but 
 (paragraph		182)	; no grave keysym that is distinct from
 (periodcentered	183)	; backquote?
 (cedilla		184)
 (onesuperior		185)
 (masculine		186)
 (guillemotright	187)
 (onequarter		188)
 (onehalf		189)
 (threequarters		190)
 (questiondown		191)
 (Agrave		192)
 (Aacute		193)
 (Acircumflex		194)
 (Atilde		195)
 (Adiaeresis		196)
 (Aring			197)
 (AE			198)
 (Ccedilla		199)
 (Egrave		200)
 (Eacute		201)
 (Ecircumflex		202)
 (Ediaeresis		203)
 (Igrave		204)
 (Iacute		205)
 (Icircumflex		206)
 (Idiaeresis		207)
 (ETH			208)
 (Ntilde		209)
 (Ograve		210)
 (Oacute		211)
 (Ocircumflex		212)
 (Otilde		213)
 (Odiaeresis		214)
 (multiply		215)
 (Ooblique		216)
 (Ugrave		217)
 (Uacute		218)
 (Ucircumflex		219)
 (Udiaeresis		220)
 (Yacute		221)
 (THORN			222)
 (ssharp		223)
 (agrave		224)
 (aacute		225)
 (acircumflex		226)
 (atilde		227)
 (adiaeresis		228)
 (aring			229)
 (ae			230)
 (ccedilla		231)
 (egrave		232)
 (eacute		233)
 (ecircumflex		234)
 (ediaeresis		235)
 (igrave		236)
 (iacute		237)
 (icircumflex		238)
 (idiaeresis		239)
 (eth			240)
 (ntilde		241)
 (ograve		242)
 (oacute		243)
 (ocircumflex		244)
 (otilde		245)
 (odiaeresis		246)
 (division		247)
 (oslash		248)
 (ugrave		249)
 (uacute		250)
 (ucircumflex		251)
 (udiaeresis		252)
 (yacute		253)
 (thorn			254)
 (ydiaeresis		255)

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
 (SunFA_Acute		180)
 (SunXK_FA_Acute	180)
 (Dacute_accent		180)
 (DXK_acute_accent	180)
 (hpmute_acute		180)
 (hpXK_mute_acute	180)
 (XK_mute_acute		180)

 (SunFA_Grave		 ?`)
 (Dead_Grave		 ?`)
 (SunXK_FA_Grave	 ?`)
 (Dgrave_accent		 ?`)
 (DXK_grave_accent	 ?`)
 (hpmute_grave		 ?`)
 (hpXK_mute_grave	 ?`)
 (XK_mute_grave		 ?`)

 (SunFA_Cedilla		184)
 (SunXK_FA_Cedilla	184)
 (Dcedilla_accent	184)
 (DXK_cedilla_accent	184)

 (SunFA_Diaeresis	168)
 (SunXK_FA_Diaeresis	168)
 (hpmute_diaeresis	168)
 (hpXK_mute_diaeresis	168)
 (XK_mute_diaeresis	168)

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

 (Dring_accent		176)
 (DXK_ring_accent	176)
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
(put 'delete	'x-iso8859/1 127)

(provide 'x-iso8859-1)
