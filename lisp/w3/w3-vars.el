;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable definitions for w3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General configuration variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-always-show-output 1
  "*Controls whether to show process output or not.  If 'nil', never
show output, if 't', always show output, if other than nil or t,
prompt.")

(defvar w3-bad-port-list
  '("25" "119")
  "*List of ports to warn the user about connecting to.  Defaults to just
the mail and NNTP ports so you cannot be tricked into sending fake mail or
forging messages by a malicious HTML document.")

(defvar w3-bad-server-list
  '("iicm.tu-graz.ac.at"
    "heplibw3.slac.stanford.edu"
    "cs.indiana.edu"
    "moose.cs.indiana.edu"
    "www.cis.ohio-state.edu")
  "*Listing of servers that can be interrupted by an HTTP/1.0 request.
Usually just HTTP/0.9 servers with lots of lag from where you are.")

(defvar w3-be-asynchronous nil
  "*Should retrieval of documents over HTTP be asynchronous?")

(defvar w3-cache-size 5 "*Size of the document cache.")

(defvar w3-confirmation-func 'yes-or-no-p
  "*What function to use for asking yes or no functions.  Possible
values are 'yes-or-no-p or 'y-or-n-p, or any function that takes a
single argument (the prompt), and returns t only if a positive answer
is gotten.")

(defvar w3-connection-retries 5 "*# of times to try for a connection")

(defvar w3-default-action 'w3-prepare-buffer
  "*A lisp symbol specifying what action to take for files with
extensions that are not in the w3-mime-extensions assoc list.
This is useful in case you ever run across files with weird extensions
(.foo, .README, .READMEFIRST, etc).

Possible values: any lisp symbol.  Should be a function that takes no
arguments.  The return value does not matter, it is ignored.  Some examples
are:

Action			Value
----------------------------------------------
Parse as HTML		'w3-prepare-buffer
View as text		'indented-text-mode
")

(defvar w3-default-homepage "http://cs.indiana.edu:80/elisp/w3/docs.html"
  "*The url to open at startup. Probably want to set this in your .emacs")

(defvar w3-default-icon-directory nil
  "*Default directory where gopher icons are stored.")

(defvar w3-directory-format 'hypertext
  "*How to format directory listings.

If value is 'hypertext, use directory-files to list them out and
transform them into a hypertext document, then pass it through the
parse like any other document.

If value is 'dired, just pass the directory off to dired using find-file.")

(defvar w3-global-history-file (expand-file-name "~/.mosaic-global-history")
  "*Global history file")

(defvar w3-gopher-labels
  '(("0" . "(TXT)")
    ("1" . "(DIR)")
    ("2" . "(CSO)")
    ("3" . "(ERR)")
    ("4" . "(MAC)")
    ("5" . "(PCB)")
    ("6" . "(UUX)")
    ("7" . "(???)")
    ("8" . "(TEL)")
    ("T" . "(TN3)")
    ("9" . "(BIN)")
    ("g" . "(GIF)")
    ("I" . "(IMG)")
    ("h" . "(WWW)")
    ("s" . "(SND)"))
  "*An assoc list of gopher types and how to describe them in the gopher
menus.")

(defvar w3-group-annotation-port 8001
  "*Port for group annotation server")

(defvar w3-group-annotation-server "hoohoo.ncsa.uiuc.edu"
  "*Group annotation server")

(defvar w3-hotlist-file (expand-file-name "~/.mosaic-hotlist-default")
  "*Hotlist file
This should be the name of a file that is stored in NCSA's Xmosaic format
(ncsa-mosaic-hotlist-format-1).  It is used to keep a listing of commonly
accessed URL's without having to go through 20 levels of menus to get to
them.")

(defvar w3-html2latex-args "-s -"
  "*Args to pass w3-html2latex-prog.  This should send the LaTeX source
to standard output.")

(defvar w3-html2latex-prog "html2latex"
  "*Program to convert html to latex.")

(defvar w3-keep-history nil
  "*Controls whether to keep a list of all the URLS being visited.
If non-nil, W3 will keep track of all the URLS visited.  If you press
\\[w3-write-global-history], it will be written to the file specified
by w3-global-history-file.  This is stored in a Mosaic-compatible file
format (ncsa-mosaic-history-format-1).")

(defvar w3-keep-old-buffers t
  "*Whether to keep old buffers around when following links.")

(defvar w3-mail-command 'mail
  "*This function will be called whenever w3 needs to send mail.  It should
enter a mail-mode-like buffer in the current window.
w3-mail-other-window-command will be used if w3-mutable-windows is t.
The commands mail-to and mail-subject should still work in this
buffer, and it should use mail-header-separator if possible.")

(defvar w3-mail-other-window-command 'mail-other-window
  "*This function will be called whenever w3 needs to send mail in
another window.  It should enter a mail-mode-like buffer in a
different window.  The commands mail-to and mail-subject should still
work in this buffer, and it should use mail-header-separator if
possible.")

(defvar w3-max-menu-length 35
  "*The maximum length of a pulldown menu before it will be split into
smaller chunks, with the first part as a submenu, followed by the rest
of the menu.")

(defvar w3-mule-attribute 1
  "*How to highlight items in Mule (Multi-Linugual Emacs).  1 is underline,
2 is reverse video.")

(defvar w3-mutable-windows nil
  "*Controls how new WWW documents are displayed.  If this is set to
non-nil and pop-up-windows is non-nil, then new buffers will be shown
in another window.  If either is nil, then it will replace the document
in the current window.")

(defvar w3-news-server nil
  "*The default news server to get newsgroups/articles from if no server
is specified in the URL.  Defaults to the environment variable NNTPSERVER
or \"news\" if NNTPSERVER is undefined.")

(defvar w3-passwd-entry-func nil
  "*This is a symbol indicating which function to call to read in a
password.  It will be set up depending on whether you are running EFS
or ange-ftp at startup if it is nil.  This function should accept the
prompt string as its first argument, and the default value as its
second argument.")

(defvar w3-personal-annotation-directory
  (expand-file-name "~/.mosaic-personal-annotations")
  "*Directory where w3 looks for personal annotations.
This is a directory that should hold the personal annotations stored in
a Mosaic-compatible format. (ncsa-mosaic-personal-annotation-log-format-1)")

(defvar w3-print-command "lpr -h -d"
  "*Print command for dvi files.
This is usually lpr -h -d to send it to a postscript printer, but you can set
it up so that it is any command that takes a dvi file as its last argument.")

(defvar w3-reuse-buffers nil
  "What to do when following a link will re-fetch a document that has
already been fetched into a W3 buffer.  Possible values are: nil,
'yes, and 'no.  Nil means ask the user if we should reuse the buffer
(this is the default value).  A value of 'yes means assume the user
wants us to reuse the buffer.  A value of 'no means assume the user
wants us to re-fetch the document.")

(defvar w3-right-border 2
  "*Amount of space to leave on right margin of WWW buffers.
This amount is subtracted from (window-width) for each new WWW buffer
and used as the new fill-column.")

(defvar w3-show-headers nil
  "*This is a list of HTTP/1.0 headers to show at the end of a buffer.
All the headers should be in lowercase.  They will be inserted at the
end of the buffer in a <UL> list.")

(defvar w3-show-http2-transfer t
  "*Whether to show the total # of bytes, size of file, and percentage
transferred when retrieving a document over HTTP/1.0 and it returns a
valid content-length header.  This can mess up some people behind
gateways.")

(defvar w3-telnet-prog "telnet"
  "*Program for opening telnet connections.  This is run in a subprocess
and should expect a hostname and port as its two command arguments and
accept HTTP/0.9 or HTTP/1.0 requests on its standard input and return the
requested url.  This defaults to telnet, but can be an expect script, or
a customized script to get around your particular firewall protection.")

(defvar w3-telnet-ready-regexp "Escape character is .*"
  "*A regular expression matching the end of the telnet headers.
This should match some text printed by w3-telnet-prog that signifies
it is ready to accept an HTTP request on its standard input.")

(defvar w3-telnet-header-length 1
  "*How many lines to kill in a telnet buffer.
This tells W3 how many lines of garbage that w3-telnet-prog puts in a
buffer before the actual HTML data.")

(defvar w3-track-mouse nil
  "*Whether to track the mouse and message the url under the mouse.
This slows down the link highlighting quite a bit.")

(defvar w3-uncompressor-alist '((".z"  . "gunzip")
				(".gz" . "gunzip")
				(".Z"  . "uncompress"))
  "*An assoc list of file extensions and the appropriate uncompression
programs for each.")

(defvar w3-use-forms-index t
  "*Non-nil means translate &lt;ISINDEX&gt; tags into &lt;FORMS&gt;")

(defvar w3-use-html2latex nil "*Use html2latex program, or regexp matching?")

(defvar w3-use-http2 t
  "*Use HTTP/1.0 Support or not?  If this is set to 't', a full HTTP/1.0
request will be sent to all servers (except those in w3-bad-server-list).
If nil, the older HTTP/0.9 protocol will be used.  If HTTP/0.9 is used,
you will lose the power of the MIME server-side typing, and W3 will make
guesses as to what type of data a file is.")

(defvar w3-use-hypertext-gopher t
  "*Use my gopher interface, and preserve the hypertext interface, or
gopher-mode?  Gopher mode is more complete at this time.")

(defvar w3-use-telnet nil
  "*Use telnet or open-network-stream to establish connections to remote
servers?  If this is set to 't', then a subprocess running telnet will be
used instead of the open-network-stream primitive.  The variable
w3-telnet-prog controls the program run in the subprocess.  This can be
used to get around firewalls.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hook Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-load-hooks nil "*Hooks to be run after loading w3")
(defvar w3-mode-hooks nil "*Hooks to be run after entering w3-mode")
(defvar w3-file-prepare-hooks nil "*Hooks to be run before preparing a buffer")
(defvar w3-file-done-hooks nil "*Hooks to be run after preparing a buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Figure out what flavor of emacs we are running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-running-epoch (and (boundp 'epoch::version)
			      (symbol-value 'epoch::version))
  "*In Epoch")

(defvar w3-running-new-lucid (or (and (fboundp 'era-version)
				      (funcall 'era-version))
				  (and (string-match "Lucid" (emacs-version))
				       (string= "19.9" (substring emacs-version
								  0 4))))
  "*In new release of Lucid emacs?")

(defvar w3-running-lemacs (and (not w3-running-new-lucid)
			       (string-match "Lucid" (emacs-version)))
  "*In lucid?")

(defvar w3-running-FSF19 (and (string-match "^19" emacs-version)
			      (not w3-running-lemacs)
			      (not w3-running-new-lucid))
  "*In FSF v19 emacs?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Link delimiting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-delimit-emphasis (and (not w3-running-lemacs)
				 (not w3-running-new-lucid)
				 (not w3-running-epoch)
				 (not (boundp 'MULE))
				 (not (and w3-running-FSF19
					   (or (eq window-system 'x)
					       (eq window-system 'dps)
					       (eq window-system 'pm)))))
  "*Whether to use characters at the start and end of each bold/italic
region.  Types of characters are specified in w3-style-chars-assoc.")

(defvar w3-link-start-delimiter "[["
  "*Put this at front of link if w3-delimit-links is t")

(defvar w3-link-end-delimiter "]]"
  "*Put this at end of link if w3-delimit-links is t")

(defvar w3-delimit-links (and (not w3-running-lemacs)
			      (not w3-running-new-lucid)
			      (not w3-running-epoch)
			      (not (boundp 'MULE))
			      (not (and w3-running-FSF19
					(or (eq window-system 'x)
					    (eq window-system 'dps)
					    (eq window-system 'pm)))))
  "*Put brackets around links?  If this variable is eq to 'linkname, then
it will put the link # in brackets after the link text.  If it is nil, then
it will not put anything.  If it is non-nil and not eq to 'linkname, then
it will put [[ & ]] around the entire text of the link.  Is initially set
to be t iff in normal emacs.  Nil if in epoch or lucid emacs, since links
should be in different colors/fonts.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terminal emulators, etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-xterm-command "xterm"
  "*Command used to start an xterm window")

(defvar w3-tn3270-emulator "tn3270"
  "The client to run in a subprocess to connect to a tn3270 machine.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-documenting stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar w3-doc-functions '(
			   w3
			   w3-add-document-to-hotlist
			   w3-add-gopher-hotlist-entry
			   w3-add-group-annotation
			   w3-add-personal-annotation
			   w3-back-link
			   w3-delete-group-annotation
			   w3-delete-personal-annotation
			   w3-document-source
			   w3-fetch
			   w3-fix-fake-urls
			   w3-follow-link
			   w3-forward-link
			   w3-goto-last-buffer
			   w3-help
			   w3-insert-this-url
			   w3-mail-current-document
			   w3-mail-to-author
			   w3-open-local
			   w3-popup-menu
			   w3-preview-this-buffer
			   w3-print-this-url
			   w3-print-url-under-point
			   w3-quit
			   w3-reload-document
			   w3-remove-from-hotlist
			   w3-retrieve
			   w3-search
			   w3-show-history-list
			   w3-show-hotlist
			   w3-submit-bug
			   w3-toggle-telnet
			   w3-use-hotlist
			   w3-use-links
			   w3-view-this-url
			   w3-view-url
			   w3-write-global-history
			   )
  "*Functions to document automatically")

(defvar w3-doc-variables '(
			   w3-annotation-marker
			   w3-bad-port-list
			   w3-bad-server-list
			   w3-be-asynchronous
			   w3-default-homepage
			   w3-delimit-links
			   w3-directory-format
			   w3-file-done-hooks
			   w3-file-prepare-hooks
			   w3-global-history-file
			   w3-graphic-converter-alist
			   w3-group-annotation-port
			   w3-group-annotation-server
			   w3-history-list
			   w3-hotlist-file
			   w3-html2latex-args
			   w3-html2latex-prog
			   w3-keep-history
			   w3-link-end-delimiter
			   w3-link-start-delimiter
			   w3-load-hooks
			   w3-max-colors
			   w3-mime-encodings
			   w3-mime-viewers
			   w3-mode-hooks
			   w3-mutable-windows
			   w3-news-server
			   w3-print-command
			   w3-right-border
			   w3-running-epoch
			   w3-running-new-lucid
			   w3-running-FSF19
			   w3-running-lemacs
			   w3-telnet-header-length
			   w3-telnet-prog
			   w3-uncompressor-alist
			   w3-use-forms-index
			   w3-use-html2latex
			   w3-use-hypertext-gopher
			   w3-use-telnet
			   w3-working-buffer
			   w3-xterm-command)
  "*Variables to document automatically")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics parsing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-graphics-list nil
  "*List of graphics already read in.")

(defvar w3-delay-image-loads nil
  "*Delay loading images for w3 or not?")

(defvar w3-delayed-images nil
  "* A buffer-local variable holding positions and urls of images within
the buffer")

(defvar w3-graphic-converter-alist
  '(
    ("image/xbm" .
     "xbmtopbm  | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/gif" .
     "giftoppm  | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/x-fax" .
     "g3topbm   | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/x-raster" .
     "rasttoppm | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/windowdump" .
     "xwdtoppm  | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/x-icon" .
     "icontopbm | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/portable-graymap" .
     "pgmtoppm  | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("impage/portable-pixmap" .
     "%s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/pict" .
     "picttoppm | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/x-macpaint" .
     "macptopbm | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/x-targa" .
     "tgatoppm  | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ("image/tiff" .
     "tifftopgm | pgmtoppm | %s ppmtoxpm -rgb /usr/lib/X11/rgb.txt")
    ) "*How to convert graphics into xpixmaps")

(defvar w3-max-colors 30
  "*The maximum # of colors per inlined image - used in epoch only")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MIME-related variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-gopher-to-mime
  '((?0 . "text/plain")			; It's a file
    (?1 . "www/gopher")			; Gopher directory
    (?2 . "www/gopher-cso-search")	; CSO search
    (?3 . "text/plain")			; Error
    (?4 . "application/mac-binhex40")	; Binhexed macintosh file
    (?5 . "application/pc-binhex40")	; DOS binary archive of some sort
    (?6 . "archive/x-uuencode")		; Unix uuencoded file
    (?7 . "www/gopher-search")		; Gopher search!
    (?9 . "application/octet-stream")	; Binary file!
    (?g . "image/gif")			; Gif file
    (?I . "image/gif")			; Some sort of image
    (?h . "text/html")			; HTML source
    (?s . "audio/basic")		; Sound file
    )
  "*An assoc list of gopher types and their corresponding MIME types")

(defvar w3-mime-accept-string nil
  "String to send to the server in the Accept: field in HTTP/1.0 requests.
This is created automatically from w3-mime-viewers, after the mailcap file
has been parsed.")

(defvar w3-mime-descriptions
  '(
    ("application/emacs-lisp" . "emacs lisp")
    ("application/postscript" . "postscript")
    ("application/mac-binhex40" . "macintosh binhex")
    ("application/octet-stream" . "raw binary")
    ("application/oda" . "ODA data")
    ("application/pdf" . "Adobe PDF format")
    ("application/rtf" . "MS RTF format")
    ("application/x-bcpio" . "old binary cpio")
    ("application/x-cpio" . "IEEE CPIO")
    ("application/x-csh" . "CSH script")
    ("application/x-hdf" . "NCSA HDF data")
    ("application/x-netcdf" . "Unidata netCDF data")
    ("application/x-sh" . "sh script")
    ("application/x-tcl" . "TCL script")
    ("application/x-troff" . "Troff source")
    ("application/x-troff-man" . "Troff w/man macros")
    ("application/x-troff-me" . "Troff w/me macros")
    ("application/x-troff-ms" . "Troff w/ms macros")
    ("application/x-wais-source" . "WAIS source")
    ("application/zip" . "pkzip archive")
    ("archive/tar" . "tar archive")
    ("audio/aiff" . "Amiga iff sound")
    ("audio/ulaw" . "sun audio")
    ("audio/x-wav" . "MS Windows+ WAVE sound")
    ("image/gif" . "GIF")
    ("image/ief" . "graphics interchange format")
    ("image/jpeg" . "JPEG")
    ("image/pict" . "Macintosh pict")
    ("image/portable-graymap" . "portable graymap graphics")
    ("image/portable-pixmap" . "portable pixmap graphics")
    ("image/tiff" . "Tagged Image File Format graphics")
    ("image/windowdump" . "Xwindows window dump")
    ("image/x-fax" . "group 3 fax")
    ("image/x-icon" . "Xwindows icon")
    ("image/x-macpaint" . "macpaint data") 
    ("image/x-portable-anymap" . "portable anymap graphics")
    ("image/x-portable-bitmap" . "portable bitmap graphics")
    ("image/x-raster" . "raster graphics")
    ("image/x-rgb" . "SGI RGB graphics")
    ("image/x-targa" . "targa 24bit graphics")
    ("image/xbm" . "Xwindows bitmap")
    ("text/html" . "hypertext")
    ("text/plain" . "plain text")
    ("text/richtext" . "MIME richtext data")
    ("text/tab-separated-values" . "tab separated values data")
    ("text/x-setext" . "Structure Enhanced Text")
    ("video/mpeg" . "Motion Picture Experts Group")
    ("video/quicktime" . "quicktime movie")
    ("video/x-msvideo" . "MS Video For Windows")
    ("video/x-sgi-movie" . "SGI \"movieplayer\" movie")
    )
  "*Descriptions of MIME types") 

(defvar w3-mime-viewers
  '(
    ("application" . (
		      ("emacs-lisp" . w3-maybe-eval)
		      ("postscript" . "ghostview %s")
		      ("dvi"        . "xdvi %s")
		      ("latex"      . "latex %s ; xdvi %s")
		      ("tex"        . "tex %s ; xdvi %s")
		      ("texinfo"    . w3-save-binary-file)
		      ("x-rtf"      . w3-save-binary-file)
		      ("zip"        . w3-save-binary-file)
		      (".*"         . w3-save-binary-file)
		      ))
    ("audio"       . (
		      ("aiff"       . w3-save-binary-file)
		      ("basic"      . "play %s")
		      ("ulaw"       . "play %s")
		      (".*"         . w3-save-binary-file)
		      ))
    ("image"       . (
		      ("x11-dump"   . "xwud -in %s")
		      ("windowdump" . "xwud -in %s")
		      ("x-cave"     . w3-save-binary-file)
		      ("x-elvm"     . w3-save-binary-file)
		      ("x-rgb"      . w3-save-binary-file)
		      ("cmu-raster" . w3-save-binary-file)
		      (".*"         . "xv -perfect %s")
		      ))
    ("text"        . (
		      ("html"       . w3-prepare-buffer)
		      ("setext"     . w3-save-binary-file)
		      ("plain"      . w3-mode)
		      (".*"         . w3-save-binary-file)
		      ))
    ("video"       . (
		      ("x-movie"    . w3-save-binary-file)
		      ("mpeg"       . "mpeg_play %s")
		      ("quicktime"  . w3-save-binary-file)
		      (".*"         . w3-save-binary-file)
		      ))
    ("archive"     . (
		      ("tar"        . tar-mode)
		      ("shar"       . w3-save-binary-file)
		      (".*"         . w3-save-binary-file)
		      ))
    ("www"         . (
		      ("print"      . w3-print)
		      ("gopher"     . w3-parse-gopher)
		      ("source"     . w3-source)
		      (".*"         . w3-save-binary-file)
		      ))
    )
  "*An assoc list containing mime content headers and how to view them.
Format is: '( (\"toplevelencoding\" . ( (\"minor encoding\" . \"how\") ...)))
The \"how\" can be either a string suitable for a (format ...) command, or
a lisp symbol specifying a function to call.

The \"minor\" encoding may be a regular expression - it is passed through
string-match.")

(defvar w3-mime-encodings
  '(
    ("x-uuencode"    . "uudecode")
    ("x-hqx"         . "mcvert")
    ("x-zip"         . "gunzip")
    ("x-compress"    . "uncompress")
    )
  "*An assoc list of mime content-encoding fields and the commands to
uncompress them.")

(defvar w3-mime-extensions
  '(
    (""          . "text/html")
    (".ai"       . "application/postscript")
    (".aif"      . "audio/aiff")
    (".aifc"     . "audio/aiff")
    (".aiff"     . "audio/aiff")
    (".au"       . "audio/ulaw")
    (".avi"      . "video/x-msvideo")
    (".bcpio"    . "application/x-bcpio")
    (".bin"      . "application/octet-stream")
    (".cpio"     . "application/x-cpio")
    (".csh"      . "application/x-csh")
    (".el"       . "application/emacs-lisp")
    (".eps"      . "application/postscript")
    (".etx"      . "text/x-setext")
    (".fax"      . "image/x-fax")
    (".gif"      . "image/gif")
    (".hdf"      . "application/x-hdf")
    (".hqx"      . "application/mac-binhex40")
    (".htm"      . "text/html")
    (".html"     . "text/html")
    (".icon"     . "image/x-icon")
    (".ief"      . "image/ief")
    (".jpe"      . "image/jpeg")
    (".jpeg"     . "image/jpeg")
    (".jpg"      . "image/jpeg")
    (".macp"     . "image/x-macpaint")
    (".man"      . "application/x-troff-man")
    (".me"       . "application/x-troff-me")
    (".mov"      . "video/quicktime")
    (".mov"      . "video/quicktime")
    (".movie"    . "video/x-sgi-movie")
    (".mpeg"     . "video/mpeg")
    (".mpg"      . "video/mpeg")
    (".ms"       . "application/x-troff-ms")
    (".nc"       . "application/x-netcdf")
    (".oda"      . "application/oda")
    (".pbm"      . "image/x-portable-bitmap")
    (".pdf"      . "application/pdf")
    (".pgm"      . "image/portable-graymap")
    (".pict"     . "image/pict")
    (".pnm"      . "image/x-portable-anymap")
    (".ppm"      . "image/portable-pixmap")
    (".ps"       . "application/postscript")
    (".qt"       . "video/quicktime")
    (".ras"      . "image/x-raster")
    (".rgb"      . "image/x-rgb")
    (".rtf"      . "application/rtf")
    (".rtx"      . "text/richtext")
    (".sh"       . "application/x-sh")
    (".snd"      . "audio/basic")
    (".src"      . "application/x-wais-source")
    (".tar"      . "archive/tar")
    (".tcl"      . "application/x-tcl")
    (".tga"      . "image/x-targa")
    (".tif"      . "image/tiff")
    (".tiff"     . "image/tiff")
    (".tr"       . "application/x-troff")
    (".troff"    . "application/x-troff")
    (".tsv"      . "text/tab-separated-values")
    (".txt"      . "text/plain")
    (".wav"      . "audio/x-wav")
    (".xbm"      . "image/xbm")
    (".xpm"      . "image/x-pixmap")
    (".xwd"      . "image/windowdump")
    (".zip"      . "application/zip")
    )
  "*An assoc list of file extensions and the mime content-types they map to.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How to look up styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-style-regexp nil "*Regular expression that matches style tags")

(defvar w3-style-assoc
  '(
    ("ABBREV" . w3-tt-style)
    ("ABSTRACT" . w3-tt-style)
    ("ACRONYM" . w3-tt-style)
    ("ADDED" . w3-bold-style)
    ("ARG" . w3-italic-style)
    ("B" . w3-bold-style)
    ("BYLINE" . w3-italic-style)
    ("CITE" . w3-underline-style)
    ("CMD" . w3-bold-style)
    ("CODE" . w3-tt-style)
    ("DFN" . w3-bold-style)
    ("EM" . w3-italic-style)
    ("I" . w3-italic-style)
    ("KBD" . w3-tt-style)
    ("PERSON" . w3-default-style)
    ("Q" . w3-italic-style)
    ("REMOVED" . w3-strikethru-style)
    ("S" . w3-strikethru-style)
    ("SAMP" . w3-tt-style)
    ("STRONG" . w3-bold-style)
    ("SUB" . w3-subscript-style)
    ("SUP" . w3-superscript-style)
    ("TT" . w3-tt-style)
    ("U" . w3-underline-style)
    ("VAR" . w3-tt-style)
    )
  "*An assoc list of emphasis tags and their corresponding styles.")

(defvar w3-style-chars-assoc
  '(
    ("B"       . ("*" . "*"))
    ("BYLINE"  . ("_" . "_"))
    ("CITE"    . ("_" . "_"))
    ("CMD"     . ("*" . "*"))
    ("DFN"     . ("*" . "*"))
    ("EM"      . ("~" . "~"))
    ("I"       . ("~" . "~"))
    ("Q"       . ("\"" . "\""))
    ("REMOVED" . ("" . ""))
    ("S"       . ("" . ""))
    ("STRONG"  . ("*" . "*"))
    ("SUB"     . ("" . ""))
    ("SUP"     . ("" . ""))
    ("U"       . ("_" . "_"))
    )
  "*An assoc list of emphasis tags and their corresponding
begin and end characters.")

(defvar w3-header-chars-assoc
  '(
    ("1" . ("<BR>**********<BR>" "<BR>**********<BR>" w3-upcase-region))
    ("2" . ("<BR>==========<BR>" "<BR>==========<BR>" w3-upcase-region))
    ("3" . ("<BR>----------<BR>" "<BR>----------<BR>" w3-upcase-region))
    ("4" . (""                   "<BR>-=-=-=-=-=-" nil))
    ("5" . (""                   "<BR>/\/\/\/\/\/" nil))
    ("6" . (""                   "<BR>:::::::::::" nil)))
  "*An assoc list of header tags and a list of formatting instructions.
This list consists of 3 items - the first item is the text to insert before
the header (at the <H#> tag), the second item is the text to insert after
the header (at the </H#> tag), and the third is a function to call on the
region between the start and end of the header.  This will be called with
2 arguments, the buffer positions of the start and end of the headers.")

(defvar w3-list-chars-assoc 
  '(
    ("UL" . ("o" "*" "+" "&gt;"))
    ("OL" . ("." ")" "]" ":"))
    ("DL" . ("o" "*" "+" "&gt;")))
  "An assoc list of characters to put at the front of list items.  It is
keyed on the type of list, followed by a list of items.  Each item should
be placed in the nth position of the list, where n is the nesting level it
should be used for.  n starts at 1.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Styles for emphasis, bold, etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((or w3-running-epoch w3-running-FSF19
      w3-running-lemacs w3-running-new-lucid)
  (defvar w3-tt-style 'w3-tt-style "Face used for fixed-width text")
  (defvar w3-bold-style 'w3-bold-style "Face used for bold text")
  (defvar w3-italic-style 'w3-italic-style "Face used for italicized text")
  (defvar w3-underline-style 'w3-underline-style "Face for underlined text")
  (defvar w3-node-style 'w3-node-style "Face used for hypertext links")
  (defvar w3-header-style 'w3-header-style "Face used for all headers")
  (defvar w3-address-style 'w3-address-style "Face used for address tags")
  (defvar w3-superscript-style 'w3-superscript-style "Face for superscripts")
  (defvar w3-subscript-style 'w3-subscript-style "Face for subscripts")
  (defvar w3-strikethru-style 'w3-strikethru-style "Face for strikethru")
  (defvar w3-default-style 'w3-default-style "Face used for all text"))
 (t
  (defvar w3-node-style nil)
  (defvar w3-header-style nil)
  (defvar w3-address-style nil)
  (defvar w3-tt-style nil)
  (defvar w3-bold-style nil)
  (defvar w3-italic-style nil)
  (defvar w3-underline-style nil)
  (defvar w3-superscript-style nil)
  (defvar w3-subscript-style nil)
  (defvar w3-strikethru-style nil)
  (defvar w3-default-style nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entities table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-html-entities
  '(
    ("&AElig;" . "Æ")
    ("&Aacute;" . "Á")
    ("&Acirc;" . "Â")
    ("&Agrave;" . "À")
    ("&Aring;" . "Å")
    ("&Atilde;" . "Ã")
    ("&Auml;" . "Ä")
    ("&Auml;" . "Ä")
    ("&Ccedil;" . "Ç")
    ("&ETH;" . "Ð")
    ("&Eacute;" . "É")
    ("&Ecirc;" . "Ê")
    ("&Egrave;" . "È")
    ("&Eth;" . "ð")
    ("&Euml;" . "Ë")
    ("&Iacute;" . "Í")
    ("&Icirc;" . "Î")
    ("&Igrave;" . "Ì")
    ("&Iuml;" . "Ï")
    ("&Ntilde;" . "Ñ")
    ("&Oacute;" . "Ó")
    ("&Ocirc;" . "Ô")
    ("&Ograve;" . "Ò")
    ("&Oslash;" . "Ø")
    ("&Otilde;" . "Õ")
    ("&Ouml;" . "Ö")
    ("&Ouml;" . "Ö")
    ("&Szlig;" . "ß")
    ("&THORN;" . "Þ")
    ("&Thorn;" . "þ")
    ("&Uacute;" . "Ú")
    ("&Ucirc;" . "Û")
    ("&Ugrave;" . "Ù")
    ("&Uuml;" . "Ü")
    ("&Uuml;" . "Ü")
    ("&Yacute;" . "Ý")
    ("&aacute;" . "á")
    ("&acirc;" . "â")
    ("&aelig;" . "æ")
    ("&agrave;" . "à")
    ("&aring;" . "å")
    ("&atilde;" . "ã")
    ("&auml;" . "ä")
    ("&auml;" . "ä")
    ("&ccedil;" . "ç")
    ("&copy;" . "(C)")
    ("&eacute;" . "é")
    ("&ecirc;" . "ê")
    ("&egrave;" . "è")
    ("&emsp;" . "  ")
    ("&ensp;" . " ")
    ("&euml;" . "ë")
    ("&gt;" . ">")
    ("&iacute;" . "í")
    ("&icirc;" . "î")
    ("&igrave;" . "ì")
    ("&iuml;" . "ï")
    ("&lt;" . "<")
    ("&mdash;" . "--")
    ("&nbsp;" . " ")
    ("&ndash;" . "-")
    ("&ntilde;" . "ñ")
    ("&oacute;" . "ó")
    ("&ocirc;" . "ô")
    ("&ograve;" . "ò")
    ("&oslash;" . "ø")
    ("&otilde;" . "õ")
    ("&ouml;" . "ö")
    ("&ouml;" . "ö")
    ("&quot;" . "\"")
    ("&reg;" . "(R)")
    ("&shy;" . "")
    ("&trade;" . "(TM)")
    ("&uacute;" . "ú")
    ("&ucirc;" . "û")
    ("&ugrave;" . "ù")
    ("&uuml;" . "ü")
    ("&uuml;" . "ü")
    ("&yacute;" . "ý")
    ("&yuml;" . "?\377")
    ("&amp;" . "&")
    )
  "*An assoc list of entity names and how to actually display them.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables internal to W3, you should not change any of these
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-acceptable-protocols-alist
  '(("Gopher"                           . "gopher")
    ("TN3270 (IBM Mainframe emulation)" . "tn3270")
    ("Interactive Telnet Session"       . "telnet")
    ("Local file or file over ftp"      . "file")
    ("File on an http server"           . "http")
    ("Usenet newsgroup/article"         . "news")
    ("Mail session"                     . "mailto"))
  "An assoc list of descriptive labels and the corresponding URL stub.")
(defvar w3-annotation-marker "<ncsa-annotation-format-1>")
(defvar w3-document-cache nil "Internal document cache")
(defconst w3-color-display nil "Is this display color or not?")
(defconst w3-color-planes nil "How many bits of color can this display show?")
(defvar w3-continuation '(w3-uncompress w3-clean-text)
  "List of functions to call to process a document completely.")
(defvar w3-current-annotation nil "URL of document we are annotating...")
(defvar w3-current-file nil "Filename of current document")
(defvar w3-current-isindex nil "Is the current document a searchable index?")
(defvar w3-current-last-buffer nil "Last W3 buffer seen before this one")
(defvar w3-current-last-link nil "What link broght us here.")
(defvar w3-current-links nil "An assoc list of <LINK> tags for this doc.")
(defvar w3-current-mime-encoding nil "MIME encoding of current document")
(defvar w3-current-mime-headers nil "An alist of MIME headers")
(defvar w3-current-mime-type nil "MIME type of current document")
(defvar w3-current-mime-viewer nil "How to view the current MIME doc")
(defvar w3-current-next-link nil "What link was followed from this document") 
(defvar w3-current-nntp-server nil "What nntp server currently opened.")
(defvar w3-current-port nil "Port # of the current document")
(defvar w3-current-server nil "Server of the current document")
(defvar w3-current-type nil "We currently in http or file mode?")
(defconst w3-default-continuation '(w3-uncompress w3-clean-text) 
  "Default action to start with - cleans text and uncompresses if necessary")
(defvar w3-editing-annotation nil "Are we editing an annotation or not?")
(defvar w3-find-this-link nil "Link to go to within a document")
(defvar w3-gopher-icons nil "An assoc list of gopher types and icons.")
(defvar w3-gopher-types "0123456789+gIThws:;<"
  "A string containing character representations of all the gopher types.")
(defvar w3-hotlist nil "Default hotlist")
(defvar w3-history-list nil "List of urls visited this session")
(defvar w3-hrule-pixmap nil
  "*The pixmap to use to replace the <HR> tag in epoch.")
(defvar w3-link-begin-regexp "<A[ \t\n]+[^>]*>" "*The beginning of a url")
(defvar w3-link-end-regexp   "</A>" "*The end of a url")
(defvar w3-mode-map (make-keymap) "*Keymap to use in w3-mode")
(defvar w3-nonrelative-link
  "^\\(mailto\\|news\\|tn3270\\|ftp\\|http\\|file\\|telnet\\|gopher\\):"
  "A regular expression that will match an absolute URL.")
(defvar w3-personal-annotations nil "Assoc list of personal annotations")
(defvar w3-pre-data nil "Data extracted from <PRE> regions")
(defvar w3-pre-data-count 0 "# of items from <PRE> regions")
(defvar w3-print-next nil "Should we latex & print the next doc?")
(defvar w3-roman-characters "ivxLCDMVX" "Roman numerals")
(defvar w3-setup-done nil "Have we been through setup code yet?")
(defvar w3-source nil "Should we source the next document or not?")
(defvar w3-strict-width nil
  "*This variable will control how wide emacs thinks the current window is.
This is useful when working in batch mode, and (window-width) returns the
wrong value.  If the value is nil, it will use the value (window-width)
returns.")
(defvar w3-working-buffer " *W3*" "The buffer to do all the processing in.")
(defconst w3-version-number "1.9.9" "Version # of w3-mode")
(defconst w3-version (format "WWW %s 19:1:6, January 8, 1994" w3-version-number)
  "More descriptive version of w3-version-number.")
(defvar w3-xmp-data nil "Data extracted from <XMP> regions")
(defvar w3-xmp-data-count 0 "# of items from <XMP> regions")
(defvar w3-zones-list nil "*List of 'zones' in a dumb emacs buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-variable-buffer-local 'w3-current-annotation)
(make-variable-buffer-local 'w3-current-file)
(make-variable-buffer-local 'w3-current-isindex)
(make-variable-buffer-local 'w3-current-last-buffer)
(make-variable-buffer-local 'w3-current-links)
(make-variable-buffer-local 'w3-current-mime-encoding)
(make-variable-buffer-local 'w3-current-mime-headers)
(make-variable-buffer-local 'w3-current-mime-type)
(make-variable-buffer-local 'w3-current-mime-viewer)
(make-variable-buffer-local 'w3-current-port)
(make-variable-buffer-local 'w3-current-server)
(make-variable-buffer-local 'w3-current-type)
(make-variable-buffer-local 'w3-current-next-link)
(make-variable-buffer-local 'w3-current-last-link)
(make-variable-buffer-local 'w3-delayed-images)
(make-variable-buffer-local 'w3-zones-list)

(suppress-keymap w3-mode-map)
(define-key w3-mode-map ">"        'w3-end-of-document)
(define-key w3-mode-map "<"        'w3-start-of-document)
(define-key w3-mode-map " "	   'scroll-up)
(define-key w3-mode-map "?"        'describe-mode)
(define-key w3-mode-map "H"	   'w3-use-hotlist)
(define-key w3-mode-map "M"	   'w3-mail-current-document)
(define-key w3-mode-map "P"        'w3-print-this-url)
(define-key w3-mode-map "U"        'w3-use-links)
(define-key w3-mode-map "V"        'w3-view-this-url)
(define-key w3-mode-map "\C-?"     'scroll-down)
(define-key w3-mode-map "\C-c\C-b" 'w3-show-history-list)
(define-key w3-mode-map "\C-c\C-v" 'w3-version)
(define-key w3-mode-map "\C-k"     'w3-save-url)
(define-key w3-mode-map "\C-o"     'w3-fetch)
(define-key w3-mode-map "\C-q"     'w3-write-global-history)
(define-key w3-mode-map "\M-\C-i"  'w3-insert-this-url)
(define-key w3-mode-map "\M-s"     'w3-document-source)
(define-key w3-mode-map "\r"       'w3-follow-link)
(define-key w3-mode-map "a"	   'w3-add-document-to-hotlist)
(define-key w3-mode-map "b"	   'w3-back-link)
(define-key w3-mode-map "d"        'w3-remove-from-hotlist)
(define-key w3-mode-map "f"	   'w3-forward-link)
(define-key w3-mode-map "g"        'w3-reload-document)
(define-key w3-mode-map "h"        'w3-help)
(define-key w3-mode-map "l"        'w3-goto-last-buffer)
(define-key w3-mode-map "m"        'w3-complete-link)
(define-key w3-mode-map "n"        'w3-forward-link)
(define-key w3-mode-map "o"	   'w3-open-local)
(define-key w3-mode-map "p"        'w3-back-link)
(define-key w3-mode-map "q"	   'w3-quit)
(define-key w3-mode-map "r"        'w3-reload-document)
(define-key w3-mode-map "s"	   'w3-search)
(define-key w3-mode-map "u"        'w3-leave-buffer)
(define-key w3-mode-map "v"	   'w3-view-url)
(define-key w3-mode-map "w"        'w3-submit-bug)

(provide 'w3-vars)
