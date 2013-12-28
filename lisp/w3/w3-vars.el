;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@indiana.edu)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
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
(defconst w3-version-number "2.1.25" "Version # of w3-mode")
(defconst w3-version (format "WWW %s 16:56:8, May 12, 1994" w3-version-number)
  "More descriptive version of w3-version-number.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General configuration variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-always-show-output 'prompt
  "*Controls whether to show process output or not.  If 'nil', never
show output, if 't', always show output, if other than nil or t,
prompt.")

(defvar w3-annotation-position 'bottom
  "*A symbol specifying where personal annotations should appear in a buffer.
Can be one of the symbols 'top or 'bottom.  If the symbol is eq to 'top, then
the annotations will appear at the top of the buffer.  If 'bottom, will appear
at the end of the buffer.")

(defvar w3-bad-port-list
  '("25" "119")
  "*List of ports to warn the user about connecting to.  Defaults to just
the mail and NNTP ports so you cannot be tricked into sending fake mail or
forging messages by a malicious HTML document.")

(defvar w3-bad-server-list
  '("iicm.tu-graz.ac.at"
    "heplibw3.slac.stanford.edu")
  "*Listing of servers that can be interrupted by an HTTP/1.0 request.
Usually just HTTP/0.9 servers with lots of lag from where you are.")

(defvar w3-be-anal-about-file-attributes nil
  "*Whether to use HTTP/1.0 to figure out file attributes
or just guess based on file extension, etc.")

(defvar w3-be-asynchronous nil
  "*Controls whether document retrievals over HTTP should be done in
the background.  This allows you to keep working in other windows
while large downloads occur.")

(defvar w3-cache-size 5 "*Size of the document cache.")

(defvar w3-confirmation-func 'yes-or-no-p
  "*What function to use for asking yes or no functions.  Possible
values are 'yes-or-no-p or 'y-or-n-p, or any function that takes a
single argument (the prompt), and returns t only if a positive answer
is gotten.")

(defvar w3-connection-retries 5
  "*# of times to try for a connection before bailing.
If for some reason w3-open-stream cannot make a connection to a host
right away, it will sit for 1 second, then try again, up to this many
tries.")

(defvar w3-default-action 'w3-prepare-buffer
  "*A lisp symbol specifying what action to take for files with
extensions that are not in the w3-mime-extensions assoc list.
This is useful in case you ever run across files with weird extensions
(.foo, .README, .READMEFIRST, etc).  This should not be required
anymore.

Possible values: any lisp symbol.  Should be a function that takes no
arguments.  The return value does not matter, it is ignored.  Some examples
are:

Action			Value
----------------------------------------------
Parse as HTML		'w3-prepare-buffer
View as text		'indented-text-mode
")

(defvar w3-default-homepage nil
  "*The url to open at startup.  It can be any valid URL.  This will
default to the environment variable WWW_HOME if you do not set it in
your .emacs file. If WWW_HOME is undefined, then it will default to
the hypertext documentation for W3 at Indiana University.")

(defvar w3-directory-format 'hypertext
  "*How to format directory listings.

If value is 'hypertext, use directory-files to list them out and
transform them into a hypertext document, then pass it through the
parse like any other document.

If value is 'dired, just pass the directory off to dired using find-file.")

(defvar w3-documents-menu-file
  (expand-file-name "/usr/local/lib/mosaic/documents.menu")
  "*Where the Mosaic documents-menu file is located.  This is a file
that has extra menus for the 'Navigate' menu.  This should be in the same
format as the Mosaic extra documents.menu")

(defvar w3-emacs19-hack-faces-p nil
  "Whether emacs19 should try to emulate bold/underline faces when running
on a dumb terminal.")

(defvar w3-global-history-file (expand-file-name "~/.mosaic-global-history")
  "*The global history file used by both Mosaic/X and W3.
This file contains a list of all the URLs you have visited.  This file
is parsed at startup and used to provide URL completion.")

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
menus.  These can be any string, but HTML/HTML+ entities should be
used when necessary, or it could disrupt formatting of the document
later on.  It is also a good idea to make sure all the strings are the
same length after entity references are removed, on a strictly
stylistic level.")

(defvar w3-gopher-icons
  '(
    ("0" . "&text.document;")
    ("1" . "&folder;")
    ("2" . "&index;")
    ("3" . "&stop;")
    ("4" . "&binhex.document;")
    ("5" . "&binhex.document;")
    ("6" . "&uuencoded.document;")
    ("7" . "&index;")
    ("8" . "&telnet;")
    ("T" . "&tn3270;")
    ("9" . "&binary.document;")
    ("g" . "&image;")
    ("I" . "&image;")
    ("s" . "&audio;"))
  "*An assoc list of gopher types and the graphic entity references to
show when possible.")    

(defvar w3-group-annotation-port 8001
  "*Port for group annotation server")

(defvar w3-group-annotation-server "hoohoo.ncsa.uiuc.edu"
  "*Group annotation server")

(defvar w3-horizontal-rule-char ?-
  "*The character to use to create a horizontal rule.
Must be the character's code, not a string.  This character is
replicated across the screen to create a division.")

(defvar w3-hotlist-file (expand-file-name "~/.mosaic-hotlist-default")
  "*Hotlist filename.
This should be the name of a file that is stored in NCSA's Mosaic/X
format (ncsa-mosaic-hotlist-format-1).  It is used to keep a listing
of commonly accessed URL's without having to go through 20 levels of
menus to get to them.")

(defvar w3-html2latex-args "-s -"
  "*Args to pass w3-html2latex-prog.  This should send the LaTeX source
to standard output.")

(defvar w3-html2latex-prog "html2latex"
  "*Program to convert html to latex.")

(defvar w3-icon-directory-list
  '("http://cs.indiana.edu/elisp/w3/icons/")
  "*A list of directorys to look in for the w3 standard icons...
must end in a /!")

(defvar w3-keep-history nil
  "*Controls whether to keep a list of all the URLS being visited.
If non-nil, W3 will keep track of all the URLS visited.  If you press
\\[w3-write-global-history], it will be written to the file specified
by w3-global-history-file.  This is stored in a Mosaic-compatible file
format (ncsa-mosaic-history-format-1).")

(defvar w3-keep-old-buffers t
  "*Whether to keep old buffers around when following links.")

(defvar w3-latex-docstyle "[psfig]{article}"
  "*The documentstyle to use when printing/mailing converted HTML
files in LaTeX.  Good defaults are:
{article}, [psfig,twocolumn]{article}, etc.")

(defvar w3-link-delimiter-info nil
  "*A function to call to get extra information about a link and
include it in a buffer.  Will be placed after the link and any other
delimiters.")

(defvar w3-local-exec-path nil
  "*A list of possible locations for x-exec scripts")

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

(defvar w3-max-inlined-image-size nil
  "*The maximum byte size of a file to transfer as an inlined image.
If an image is being retrieved and exceeds this size, then it will be
cancelled.  This works best on HTTP/1.0 servers that send a
Content-length header, otherwise the image is retrieved up until the
max number of bytes is retrieved, then killed.")

(defvar w3-max-menu-length 35
  "*The maximum length of a pulldown menu before it will be split into
smaller chunks, with the first part as a submenu, followed by the rest
of the menu.")

(defvar w3-mule-attribute 1
  "*How to highlight items in Mule (Multi-Linugual Emacs).  1 is underline,
2 is reverse video.")

(defvar w3-mule-retrieval-coding-system (if (boundp 'MULE) *euc-japan*
					  nil)
  "Coding system for retrieval, used before hexified")

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

(defvar w3-personal-mail-address nil
  "*Your full email address.  This is what is sent to HTTP/1.0 servers as
the FROM field.  If not set when w3-do-setup is run, it defaults to
the value of w3-pgp/pem-entity.")

(defvar w3-pgp/pem-entity nil
  "*The users PGP/PEM id - usually their email address.")

(defvar w3-ppmtoxbm-command "ppmtopgm | pgmtopbm | pbmtoxbm"
  "*The command used to convert from the portable-pixmap graphics format
to an x bitmap.  This will only ever be used if lemacs doesn't have support
for XPM.")

(defvar w3-ppmtoxpm-command "ppmtoxpm"
  "*The command used to convert from the portable-pixmap graphics format
to XPM.  The XPM _MUST_ be in version 3 format.")

(defvar w3-print-command "lpr -h -d"
  "*Print command for dvi files.
This is usually lpr -h -d to send it to a postscript printer, but you can set
it up so that it is any command that takes a dvi file as its last argument.")

(defvar w3-proxy-services nil
  "*An assoc list of access types and servers that gateway them.
Looks like ((\"http\" . \"server.some.domain:port\") ....)  This is set up
from the ACCESS_proxy environment variables in w3-do-setup.")

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

(defvar w3-show-status t
  "*Whether to show a running total of bytes transferred.  Can cause a
large hit if using a remote X display over a slow link, or a terminal
with a slow modem.")

(defvar w3-starting-documents
  '(("Internet Starting Points"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/StartingPoints/NetworkStartingPoints.html")
    ("Internet Resources Meta-index"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/MetaIndex.html")
    ("NCSA's What's New"  "http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/whats-new.html")))

(defvar w3-temporary-directory "/tmp" "*Where temporary files go.")

(defvar w3-track-mouse t
  "*Whether to track the mouse and message the url under the mouse.
This also changes the mouse cursor to be the cursor specified by
w3-link-cursor.")

(defvar w3-uudecode-program "uudecode" "*The UUdecode executable")
(defvar w3-uuencode-program "uuencode" "*The UUencode executable")

(defvar w3-uncompressor-alist '((".z"  . "gunzip")
				(".gz" . "gunzip")
				(".Z"  . "uncompress"))
  "*An assoc list of file extensions and the appropriate uncompression
programs for each.")

(defvar w3-use-forms-index t
  "*Non-nil means translate <ISINDEX> tags into a hypertext form.
A single text entry box will be drawn where the ISINDEX tag appears.
If t, the isindex handling will be the same as Mosaic for X.")

(defvar w3-use-html2latex nil
  "*This controls how HTML is converted into LaTeX for printing or mailing.
If nil, the w3-convert-html-to-latex function is used instead of the
html2latex in a subprocess.  The lisp function gives slightly better
formatting in my biased opinion.")

(defvar w3-use-hypertext-gopher t
  "*Controls how gopher documents are retrieved.
If non-nil, the gopher pages will be converted into HTML and parsed
just like any other page.  If nil, the requests will be passed off to
the gopher.el package by Scott Snyder.  Using the gopher.el package
will lose the gopher+ support, and inlined searching.")

(defvar w3-use-transparent nil
  "*Whether to use the transparent package by Brian Tompsett instead of
the builtin telnet functions.  Using transparent allows you to have full
vt100 emulation in the telnet and tn3270 links.")

(defvar w3-wais-to-mime
  '(
    ("WSRC" . "application/x-wais-source") 	; A database description
    ("TEXT" . "text/plain")			; plain text
    )
  "An assoc list of wais doctypes and their corresponding MIME
content-types.")

(defvar w3-waisq-prog "waisq"
  "*Name of the waisq executable on this system.  This should be the
waisq program from think.com's wais8-b5.1 distribution.")

(defvar w3-wais-gateway-server "www.ncsa.uiuc.edu"
  "*The machine name where the WAIS gateway lives")

(defvar w3-wais-gateway-port "8001"
  "*The port # of the WAIS gateway.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gateway information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-gateway-method 'native
  "*The type of gateway support to use.
Should be a symbol specifying how we are to get a connection off of the
local machine.

Currently supported methods:
'program	:: Run a program in a subprocess to connect
                   (examples are itelnet, an expect script, etc)
'host     	:: You need to log into a different machine, then
		   are able to telnet out
'native		:: Use the native open-network-stream in emacs
'tcp            :: Use the excellent tcp.el package from gnus.
                   This simply does a (require 'tcp), then sets
                   w3-gateway-method to be 'native.
")

(defvar w3-gateway-program-interactive nil
  "*Whether w3 needs to hand-hold the login program on the remote machine")

(defvar w3-gateway-handholding-login-regexp "ogin:"
  "*Regexp for when to send the username to the remote process")

(defvar w3-gateway-handholding-password-regexp "ord:"
  "*Regexp for when to send the password to the remote process")

(defvar w3-gateway-host-prompt-pattern "^[^#$%>;]*[#$%>;] *"
  "*Regexp used to detect when the login is finished on the remote host.")

(defvar w3-gateway-host nil
  "*Name of your gateway host when using the w3-gateway-method 'host")

(defvar w3-gateway-host-username nil
  "*Username to use to log into the host specified by w3-gateway-host when
using w3-gateway-method 'host")

(defvar w3-gateway-host-password nil
  "*Password to use to log into the host specified by w3-gateway-host when
using w3-gateway-method 'host")

(defvar w3-gateway-host-process nil
  "The process currently communicating with the w3-gateway-host")

(defvar w3-gateway-buffer " *GATEWAY*"
  "Buffer used temporarily when using gateways.")

(defvar w3-gateway-host-program "telnet"
  "*The name of the program on the remote host that acts like telnet.")

(defvar w3-gateway-host-program-ready-regexp "Escape character is .*"
  "*A regular expression that signifies the program on the remote host is
ready to accept input and send it to the remote host.")

(defvar w3-gateway-telnet-ready-regexp "Escape character is .*"
  "*A regular expression that signifies w3-gateway-telnet-program is
ready to accept input")

(defvar w3-local-telnet-prog "telnet"
  "*Program for local telnet connections")

(defvar w3-remote-telnet-prog "itelnet"
  "*Program for remote telnet connections")  

(defvar w3-gateway-telnet-program "itelnet"
  "*Program to run in a subprocess when using gateway-method 'program")

(defvar w3-gateway-local-host-regexp nil
  "*If a host being connected to matches this regexp then the
connection is done natively, otherwise the process is started on
`w3-gateway-host' instead.")

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

(defvar w3-running-lemacs (and (string-match "Lucid" emacs-version)
			       (not (string-match "19\\.[0-8][^0-9]"
						  emacs-version)))
  "*In new release of Lucid emacs?")

(defvar w3-running-old-lemacs (and (not w3-running-lemacs)
				   (string-match "Lucid" (emacs-version)))
  "*In lucid?")

(defvar w3-running-FSF19 (and (string-match "^19" emacs-version)
			      (not w3-running-lemacs)
			      (not w3-running-old-lemacs))
  "*In FSF v19 emacs?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Link delimiting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-delimit-emphasis 'guess
  "*Whether to use characters at the start and end of each bold/italic
region.  Types of characters are specified in w3-style-chars-assoc.")

(defvar w3-link-start-delimiter '("[[" . "{{")
  "*Put this at front of link if w3-delimit-links is t")

(defvar w3-link-end-delimiter '("]]" . "}}")
  "*Put this at end of link if w3-delimit-links is t")

(defvar w3-delimit-links 'guess
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
			   w3-source-document
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
			   w3-running-lemacs
			   w3-running-old-lemacs
			   w3-running-FSF19
			   w3-running-lemacs
			   w3-uncompressor-alist
			   w3-use-forms-index
			   w3-use-html2latex
			   w3-use-hypertext-gopher
			   w3-working-buffer
			   w3-xterm-command)
  "*Variables to document automatically")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; embedded document variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-mpeg-size 150 "*The height and width of an mpeg window")
(defvar w3-mpeg-args '("-loop") "*Arguments to mpeg_play")
(defvar w3-mpeg-program "mpeg_play" "*The mpeg_play executable")
(defvar w3-delayed-movies nil "A list of mpeg movies for this buffer")
  
(defvar w3-embedded-data-converters
  '(("application/eqn" . w3-embed-eqn)
    ("application/postscript" . w3-embed-postscript)
    ("text/plain". w3-embed-text)
    ("text/html" . w3-embed-text)
    ("image/.*"  . w3-embed-image))
  "An assoc list of regular expressions to match against MIME content-types
for embedded data in HTML documents.  The cdr is a function to be passed
to 'funcall', with the embedded data and content-type as the sole arguments
passed to the function.")

(if (and w3-running-lemacs (boundp 'emacs-minor-version)
	 (>= emacs-minor-version 10))
    (progn
      (require 'annotations)
      (setq w3-embedded-data-converters
	    (cons (cons "video/mpeg" 'w3-embed-mpeg)
		  w3-embedded-data-converters))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics parsing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-graphics-always-show-entities t
  "*Set to t to always show graphic entities, regardless of the value of
w3-delay-image-loads.  Useful if you keep the entities locally and aren't
worried about the transfer time on something that small.")

(defvar w3-graphics-list nil
  "*List of graphics already read in.")

(defvar w3-delay-image-loads nil
  "*Delay loading images for w3 or not?")

(defvar w3-delayed-images nil
  "* A buffer-local variable holding positions and urls of images within
the buffer")

(defvar w3-delay-mpeg-loads t
  "*Whether to delay loading mpegs or not.")

(defvar w3-graphic-converter-alist
  '(
    ("image/x-xbitmap"        . "cat ")
    ("image/xbitmap"          . "cat ")
    ("image/xbm"              . "cat ")
    ("image/gif"              . "giftoppm  | ppmquant %d | %s")
    ("image/x-fax"            . "g3topbm   | ppmquant %d | %s")
    ("image/x-raster"         . "rasttoppm | ppmquant %d | %s")
    ("image/windowdump"       . "xwdtoppm  | ppmquant %d | %s")
    ("image/x-icon"           . "icontopbm | ppmquant %d | %s")
    ("image/portable-graymap" . "pgmtoppm  | ppmquant %d | %s")
    ("image/portable-pixmap"  . "cat ")
    ("image/x-pixmap"         . "cat ")
    ("image/x-xpixmap"        . "cat ")
    ("image/pict"             . "picttoppm | ppmquant %d | %s")
    ("image/x-macpaint"       . "macptopbm | ppmquant %d | %s")
    ("image/x-targa"          . "tgatoppm  | ppmquant %d | %s")
    ("image/tiff"             . "tifftopgm | pgmtoppm | ppmquant %d | %s")
    ) "*How to convert graphics into xpixmaps")

(defvar w3-max-colors 30
  "*The maximum # of colors per inlined image.")

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

(defvar w3-mime-default-mailcap nil
  "*The default mailcap file.  Can be either a local file, or a fully
qualified URL.")

(defvar w3-mime-viewers
  '(
    ("application" . (
		      ("x-www-pem-reply" . (w3-decode-pgp/pem "pem"))
		      ("x-www-pgp-reply" . (w3-decode-pgp/pem "pgp"))
		      (".*"         . w3-save-binary-file)
		      ))
    ("audio"       . (
		      (".*"         . w3-save-binary-file)
		      ))
    ("image"       . (
		      (".*"         . "xv -perfect %s")
		      ))
    ("text"        . (
		      ("html"       . w3-prepare-buffer)
		      (".*"         . w3-mode)
		      ))
    ("video"       . (
		      (".*"         . w3-save-binary-file)
		      ))
    ("archive"     . (
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

(defvar w3-mime-editors nil
  "*An assoc list containing MIME types and how to edit a file of that
type.  A %s in the cdr is replaced with a temporary file name that the
program should put the new data into, otherwise data is assumed to be
written to standard output.")

(defvar w3-mime-bitmaps nil
  "*An assoc list containing MIME types and bitmaps that represent
that type.  A %s in the cdr is replaced with a temporary file name
that the program should put the new data into, otherwise data is
assumed to be written to standard output.")

(defvar w3-mime-printers nil
  "*An assoc list containing MIME types and how to print a file of
that type.  A %s in the cdr is replaced with a temporary file name
that the program should put the new data into, otherwise data is
assumed to be written to standard output.")

(defvar w3-mime-composers nil
  "*An assoc list containing MIME types and how to create a new body
of that type.  A %s in the cdr is replaced with a temporary file name
that the program should put the new data into, otherwise data is assumed
to be written to standard output.")  

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
    (""          . "text/plain")
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
    (".dvi"      . "application/x-dvi")
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
    (".tex"      . "application/tex")
    (".texi"     . "application/texinfo")
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

(defvar w3-mime-mailcap-overrides nil
  "*Controls whether the users mailcap entries override the builtin defaults
for external viewers.")

(defvar w3-mime-mimetypes-overrides nil
  "*Controls whether the users mime-types entries override the builtin defaults
for mapping file extensions to MIME types.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How to look up styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-style-regexp nil "*Regular expression that matches style tags")

(defvar w3-style-assoc
  '(
    ("ABBREV" . w3-tt-style)
    ("ADDRESS" . w3-address-style)
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
    ("ADDRESS" . ("*" . "*"))
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
    ("1" . (?* ?* w3-upcase-region))
    ("2" . (?* ?* w3-upcase-region))
    ("3" . (?- ?- w3-upcase-region))
    ("4" . (nil ?= nil))
    ("5" . (nil ?= nil))
    ("6" . (nil ?: nil)))
  "*An assoc list of header tags and a list of formatting instructions.
This list consists of 3 items - the first item is a character that is
inserted before the header X number of times, where X is the length of
the header.  A <BR> is automatically inserted after this.
The second item is the character to insert after the header.  A <BR>
is inserted before and after this string. And the third is a function
to call on the region between the start and end of the header.  This
will be called with 2 arguments, the buffer positions of the start and
end of the headers.")

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
      w3-running-lemacs w3-running-old-lemacs)
  (defvar w3-tt-style 'w3-tt-style "Face used for fixed-width text")
  (defvar w3-bold-style 'w3-bold-style "Face used for bold text")
  (defvar w3-italic-style 'w3-italic-style "Face used for italicized text")
  (defvar w3-underline-style 'w3-underline-style "Face for underlined text")
  (defvar w3-node-style 'w3-node-style "Face used for hypertext links")
  (defvar w3-visited-node-style 'w3-visited-node-style "Visited links")
  (defvar w3-header-style 'w3-header-style "Face used for all headers")
  (defvar w3-address-style 'w3-address-style "Face used for address tags")
  (defvar w3-superscript-style 'w3-superscript-style "Face for superscripts")
  (defvar w3-subscript-style 'w3-subscript-style "Face for subscripts")
  (defvar w3-strikethru-style 'w3-strikethru-style "Face for strikethru")
  (defvar w3-default-style 'w3-default-style "Face used for all text"))
 (t
  (defvar w3-node-style nil)
  (defvar w3-visited-node-style nil)
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
    ("&AElig;"      . "Æ")
    ("&Aacute;"     . "Á")
    ("&Acirc;"      . "Â")
    ("&Agrave;"     . "À")
    ("&Aring;"      . "Å")
    ("&Atilde;"     . "Ã")
    ("&Auml;"       . "Ä")
    ("&Ccedil;"     . "Ç")
    ("&ETH;"        . "Ð")
    ("&Eacute;"     . "É")
    ("&Ecirc;"      . "Ê")
    ("&Egrave;"     . "È")
    ("&Euml;"       . "Ë")
    ("&Iacute;"     . "Í")
    ("&Icirc;"      . "Î")
    ("&Igrave;"     . "Ì")
    ("&Iuml;"       . "Ï")
    ("&Ntilde;"     . "Ñ")
    ("&Oacute;"     . "Ó")
    ("&Ocirc;"      . "Ô")
    ("&Ograve;"     . "Ò")
    ("&Oslash;"     . "Ø")
    ("&Otilde;"     . "Õ")
    ("&Ouml;"       . "Ö")
    ("&THORN;"      . "Þ")
    ("&Uacute;"     . "Ú")
    ("&Ucirc;"      . "Û")
    ("&Ugrave;"     . "Ù")
    ("&Uuml;"       . "Ü")
    ("&Yacute;"     . "Ý")
    ("&aacute;"     . "á")
    ("&acirc;"      . "â")
    ("&aelig;"      . "æ")
    ("&agrave;"     . "à")
    ("&apos;"       . "'")
    ("&aring;"      . "å")
    ("&ast;"        . "*")
    ("&atilde;"     . "ã")
    ("&auml;"       . "ä")
    ("&brvbar;"     . "¦")
    ("&ccedil;"     . "ç")
    ("&cent;"       . "¢")
    ("&colon;"      . ":")
    ("&comma;"      . ",")
    ("&commat;"     . "@") ; Commercial at sign
    ("&copy;"       . "©")
    ("&deg;"        . "°")
    ("&dollar;"     . "$")
    ("&eacute;"     . "é")
    ("&ecirc;"      . "ê")
    ("&egrave;"     . "è")
    ("&emsp;"       . "  ")
    ("&ensp;"       . " ")
    ("&equals;"     . "=")
    ("&eth;"        . "ð")
    ("&euml;"       . "ë")
    ("&excl;"       . "!")
    ("&frac12;"     . "½")
    ("&frac14;"     . "¼")
    ("&frac34;"     . "¾")
    ("&frac18;"     . "1/8")
    ("&frac38;"     . "3/8")
    ("&frac58;"     . "5/8")
    ("&frac78;"     . "7/8")
    ("&gt;"         . ">")
    ("&gt"          . ">")
    ("&half;"       . "½")
    ("&hyphen;"     . "-")
    ("&iacute;"     . "í")
    ("&icirc;"      . "î")
    ("&iexcl;"      . "¡")
    ("&igrave;"     . "ì")
    ("&iquest;"     . "¿")
    ("&iuml;"       . "ï")
    ("&laquo;"      . "«")
    ("&lpar;"       . "(")
    ("&lsqb;"       . "[")
    ("&lt;"         . "<")
    ("&lt"          . "<")
    ("&mdash;"      . "--")
    ("&micro;"      . "µ")
    ("&middot;"     . "·")
    ("&nbsp;"       . " ")
    ("&ndash;"      . "-")
    ("&not;"        . "¬")
    ("&ntilde;"     . "ñ")
    ("&oacute;"     . "ó")
    ("&ocirc;"      . "ô")
    ("&ograve;"     . "ò")
    ("&oslash;"     . "ø")
    ("&otilde;"     . "õ")
    ("&ouml;"       . "ö")
    ("&para;"       . "¶")
    ("&percnt;"     . "%")
    ("&period;"     . ".")
    ("&plus;"       . "+")
    ("&plusmn;"     . "±")
    ("&pound;"      . "£")
    ("&quest;"      . "?")
    ("&quot;"       . "\"")
    ("&raquo;"      . "»")
    ("&reg;"        . "®")
    ("&rpar;"       . ")")
    ("&rsqb;"       . "]")
    ("&sect;"       . "§")
    ("&semi;"       . ";")
    ("&shy;"        . "")
    ("&sup1;"       . "¹")
    ("&sup2;"       . "²")
    ("&sup3;"       . "³")
    ("&szlig;"      . "ß")
    ("&thorn;"      . "þ")
    ("&tilde;"      . "~")
    ("&trade;"      . "(TM)")
    ("&uacute;"     . "ú")
    ("&ucirc;"      . "û")
    ("&ugrave;"     . "ù")
    ("&uuml;"       . "ü")
    ("&yacute;"     . "ý")
    ("&yen;"        . "¥")
    ("&yuml;"       . "ÿ")
    ("&verbar;"     . "|")
    ("&amp;"        . "&")
    )
  "*An assoc list of entity names and how to actually display them.")

(defvar w3-graphics-entities-alist
  '(
    ("&archive;"             . ("archive.xbm"))
    ("&audio;"               . ("audio.xbm"))
    ("&binary.document;"     . ("binary.document.xbm"))
    ("&binhex.document;"     . ("binhex.document.xbm"))
    ("&caution;"             . ("caution.xbm" . "[CAUTION]"))
    ("&clock;"               . ("clock.xbm"))
    ("&compressed.document;" . ("compressed.document.xbm"))
    ("&disk.drive;"          . ("disk.drive.xbm"))
    ("&diskette;"            . ("diskette.xbm"))
    ("&display;"             . ("display.xbm"))
    ("&document;"            . ("unknown.document.xbm"))
    ("&fax;"                 . ("fax.xbm"))
    ("&filing.cabinet;"      . ("filing.cabinet.xbm"))
    ("&film;"                . ("film.xbm"))
    ("&fixed.disk;"          . ("fixed.disk.xbm"))
    ("&folder;"              . ("folder.xbm"))
    ("&form;"                . ("form.xbm")) ; draw one
    ("&ftp;"                 . ("ftp.xbm"))
    ("&glossary;"            . ("glossary.xbm" . "[glossary]"))
    ("&home;"                . ("home.xbm" . "[HOME]"))
    ("&image;"               . ("image.xbm"))
    ("&index;"               . ("index.xbm" . "[index]"))
    ("&keyboard;"            . ("keyboard.xbm"))
    ("&mail;"                . ("mail.xbm"))
    ("&mail.in;"             . ("mail.in.xbm"))
    ("&mail.out;"            . ("mail.out.xbm"))
    ("&map;"                 . ("map.xbm"))
    ("&mouse;"               . ("mouse.xbm"))
    ("&network;"             . ("network.xbm"))
    ("&next;"                . ("next.xbm" . "[next]"))
    ("&notebook;"            . ("notebook.xbm"))
    ("&parent;"              . ("parent.xbm" . "[parent]")) ; draw one
    ("&previous;"            . ("previous.xbm" . "[previous]")) ; draw one
    ("&printer;"             . ("printer.xbm"))
    ("&scheduler;"           . ("scheduler.xbm"))
    ("&stop;"                . ("stop.xbm" . "[STOP]"))
    ("&summary;"             . ("summary.xbm")) ; draw one
    ("&symlink;"             . ("symlink.xbm"))
    ("&telephone;"           . ("telephone.xbm"))
    ("&telnet;"              . ("telnet.xbm"))
    ("&text.document;"       . ("unknown.document.xbm"))
    ("&tn3270;"              . ("tn3270.xbm"))
    ("&toc;"                 . ("toc.xbm" . "[Table Of Contents]")); Draw one!
    ("&trash;"               . ("trash.xbm"))
    ("&unknown.document;"    . ("unknown.document.xbm"))
    ("&uuencoded.document;"  . ("uuencoded.document.xbm"))
    ("&workstation;"         . ("workstation.xbm"))
    )
    "*An assoc list of entity nams and the names of bitmaps to
display them. The car is the entity name, the cdr is a list of the
form (bitmapfile . alttag), where bitmapfile is a filename that
specifies a bitmap file - w3-icon-directory is prepended to this
automatically.  Alttag is the text to use if the bitmap is
unavailable.  If nil, no text is used.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-main-menu 
  '("WWW Browser"
    ["Open Local File" w3-open-local t]
    ["Open URL" w3-fetch t]
    ["Show document's address" w3-view-url t]
    ["Copy document's address into cut buffer" w3-save-url t]
    "----------------------------------"
    ["View Source" w3-source-document t]
    ["Edit Document Source" w3-find-this-file t]
    ["Reload Current Document" w3-reload-document t]
    "----------------------------------"
    ["Mail Formatted Document " w3-mail-current-document t]
    ["Print Current Document" w3-print-this-url t]
    "---------------------------------"
    ["Leave & Bury Buffer" w3-leave-buffer t]
    ["Leave & Kill Buffer" w3-quit t]
    )
  "The main w3 menu"
  )

(defvar w3-popup-menu
  '("WWW Browser"
    ["Open Local File" w3-open-local t]
    ["Open URL" w3-fetch t]
    ["Load Delayed Images" w3-load-delayed-images w3-delayed-images]
    ["Load Delayed MPEGs" w3-load-delayed-mpegs w3-delayed-movies]
    "-------"
    ["Mail Document Under Cursor" (w3-mail-current-document t) t]
    ["Print Document Under Cursor" (w3-print-this-url t) t]
    "---"
    ["Show address of link under cursor" w3-view-this-url t]
    ["Information on link under cursor" w3-link-info t]
    ["Copy address under point into cut buffer" (w3-save-url t) t]
    ["Print Document Under Cursor" w3-print-url-under-point t]
    "-------"
    ["Leave & Bury Buffer" w3-leave-buffer t]
    ["Leave & Kill Buffer" w3-quit t]
    )
  "The shorter popup menu")

(defvar w3-documentation-root "http://www.cs.indiana.edu/elisp/w3/"
  "*Where the w3 documentation lives.  This MUST end in a slash.")

(defvar w3-help-menu
  (list
   "Help"
   (vector "About"
	   (list 'w3-fetch (concat w3-documentation-root "docs.html")) t)
   (vector "Manual"
	   (list 'w3-fetch (concat w3-documentation-root "w3_toc.html")) t)
   "----"
   (vector (concat "Help on v" w3-version-number)
	   (list 'w3-fetch (concat w3-documentation-root "help_on_" 
				   w3-version-number ".html")) t)
   (vector "On Window" (list 'w3-fetch (concat w3-documentation-root
					       "window-help.html")) t)
   (vector "On FAQ" (list 'w3-fetch (concat w3-documentation-root
					    "FAQ.html")) t)
   "----"
   ["On HTML" (w3-fetch "http://www.ncsa.uiuc.edu/General/Internet/WWW/HTMLPrimer.html") t]
   ["On URLs" (w3-fetch "http://www.ncsa.uiuc.edu/demoweb/url-primer.html") t]
   ["Mail Developer(s)" w3-submit-bug t])
  "The help menu for w3")

(defvar w3-annotation-menu
  '("Annotations"
    ("Group Annotations"
     ["Add Annotation" w3-add-group-annotation t]
     ["Edit Annotation" w3-edit-group-annotation t]
     ["Delete Annotation" w3-delete-group-annotation t])
    ("Personal Annotations"
     ["Add Annotation" w3-add-personal-annotation t]
     ["Edit Annotation" w3-edit-personal-annotation t]
     ["Delete Annotation" w3-delete-personal-annotation t]))
  "The annotation menu for w3")

(defvar w3-navigate-menu
  '("Navigate"
    ["Back" w3-backward-in-history t]
    ["Forward" w3-forward-in-history t]
    "-----------------------------"
    ["Goto Home Document" w3 t]
    ["Show History" w3-show-history-list w3-keep-history]
    ["Show Hotlist" w3-show-hotlist w3-hotlist]
    ("Hotlist Maintenance"
     ["Add this document to hotlist" w3-hotlist-add-document t]
     ["Delete item from hotlist" w3-hotlist-delete t]
     ["Rename item in hotlist" w3-hotlist-rename-entry t]
     ["Append new hotlist file" w3-hotlist-append t])
    "------------------------------")
    "The navigation menu")

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
(defconst w3-color-display nil "Is this display color or not?")
(defconst w3-color-planes nil "How many bits of color can this display show?")
(defvar w3-continuation '(w3-uncompress w3-clean-text)
  "List of functions to call to process a document completely.")
(defvar w3-current-annotation nil "URL of document we are annotating...")
(defvar w3-current-content-length nil "Current content length")
(defvar w3-current-file nil "Filename of current document")
(defvar w3-current-isindex nil "Is the current document a searchable index?")
(defvar w3-current-last-buffer nil "Last W3 buffer seen before this one")
(defvar w3-current-links nil "An assoc list of <LINK> tags for this doc.")
(defvar w3-current-mime-encoding nil "MIME encoding of current document")
(defvar w3-current-mime-headers nil "An alist of MIME headers")
(defvar w3-current-mime-type nil "MIME type of current document")
(defvar w3-current-mime-viewer nil "How to view the current MIME doc")
(defvar w3-current-nntp-server nil "What nntp server currently opened.")
(defvar w3-current-port nil "Port # of the current document")
(defvar w3-current-server nil "Server of the current document")
(defvar w3-current-user nil "Username for ftp login")
(defvar w3-current-type nil "We currently in http or file mode?")
(defvar w3-current-source nil "Source of current document")
(defconst w3-default-continuation '(w3-uncompress w3-clean-text) 
  "Default action to start with - cleans text and uncompresses if necessary")
(defvar w3-editing-annotation nil "Are we editing an annotation or not?")
(defvar w3-find-this-link nil "Link to go to within a document")
(defvar w3-global-history-completion-list nil
  "Assoc-list of for global history completion")
(defvar w3-gopher-types "0123456789+gIThws:;<"
  "A string containing character representations of all the gopher types.")
(defvar w3-hidden-forms nil "List of hidden form areas and their info")
(defvar w3-hotlist nil "Default hotlist")
(defvar w3-history-list nil "List of urls visited this session")
(defvar w3-icon-path-cache nil "Cache of where we found icons for entities.")
(defvar w3-link-begin-regexp "<A[ \t\n]+\\([^>]*\\)>"
  "*The beginning of a url")
(defvar w3-link-end-regexp   "</A>" "*The end of a url")
(defvar w3-mime-separator-chars (mapcar 'identity
					(concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
						"abcdefghijklmnopqrstuvwxyz"
						"0123456789'()+_,-./=?"))
  "Characters allowable in a MIME multipart separator.")
(defvar w3-mode-map (make-keymap) "*Keymap to use in w3-mode")
(defvar w3-nonrelative-link
  "^\\(wais\\|x-exec\\|newspost\\|www\\|mailto\\|news\\|tn3270\\|ftp\\|http\\|file\\|telnet\\|gopher\\):"
  "A regular expression that will match an absolute URL.")
(defvar w3-old-kill-emacs-hook nil "Old value of kill-emacs-hook, if any")
(defvar w3-personal-annotations nil "Assoc list of personal annotations")
(defvar w3-pre-data nil "Data extracted from <PRE> regions")
(defvar w3-pre-data-count 0 "# of items from <PRE> regions")
(defvar w3-print-next nil "Should we latex & print the next doc?")
(defvar w3-request-data nil "Any data to send with the next request.")
(defvar w3-request-extra-headers nil
  "A list of extra headers to send with the next request.  Should be
an assoc list of headers/contents.")
(defvar w3-request-method nil "The method to use for the next request.")  
(defvar w3-roman-characters "ivxLCDMVX" "Roman numerals")
(defvar w3-setup-done nil "Have we been through setup code yet?")
(defvar w3-source nil "Should we source the next document or not?")
(defvar w3-strict-width nil
  "*This variable will control how wide emacs thinks the current window is.
This is useful when working in batch mode, and (window-width) returns the
wrong value.  If the value is nil, it will use the value (window-width)
returns.")
(defvar w3-using-proxy nil "Whether we are currently using a proxy gateway.")
(defvar w3-working-buffer " *W3*" "The buffer to do all the processing in.")
(defvar w3-xmp-data nil "Data extracted from <XMP> regions")
(defvar w3-xmp-data-count 0 "# of items from <XMP> regions")
(defvar w3-zones-list nil "*List of 'zones' in a dumb emacs buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-local variables to keep around when going into w3-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-persistent-variables
  '(
    tab-stop-list
    tab-width
    w3-current-annotation
    w3-current-content-length
    w3-current-file
    w3-current-isindex
    w3-current-last-buffer
    w3-current-links
    w3-current-mime-encoding
    w3-current-mime-headers
    w3-current-mime-type
    w3-current-mime-viewer
    w3-current-port
    w3-current-server
    w3-current-source
    w3-current-type
    w3-current-user
    w3-delayed-images
    w3-delayed-movies
    w3-hidden-forms
    w3-zones-list
    )
  "A list of variables that should be preserved when entering w3-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-parse-args-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "A syntax table for parsing sgml attributes")

(modify-syntax-entry ?' "\"" w3-parse-args-syntax-table)
(modify-syntax-entry ?< "(>" w3-parse-args-syntax-table)
(modify-syntax-entry ?> ")<" w3-parse-args-syntax-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapcar 'make-variable-buffer-local w3-persistent-variables)

(suppress-keymap w3-mode-map)
(define-key w3-mode-map " "	   'scroll-up)
(define-key w3-mode-map "<"        'w3-start-of-document)
(define-key w3-mode-map ">"        'w3-end-of-document)
(define-key w3-mode-map "?"        'describe-mode)
(define-key w3-mode-map "A"        'w3-hotlist-add-document-at-point)
(define-key w3-mode-map "B"        'w3-backward-in-history)
(define-key w3-mode-map "F"        'w3-forward-in-history)
(define-key w3-mode-map "H"	   'w3-use-hotlist)
(define-key w3-mode-map "K"        'w3-save-this-url)
(define-key w3-mode-map "P"        'w3-print-url-under-point)
(define-key w3-mode-map "Q"        'w3-leave-buffer)
(define-key w3-mode-map "S"        'w3-source-document-at-point)
(define-key w3-mode-map "U"        'w3-use-links)
(define-key w3-mode-map "V"        'w3-view-this-url)
(define-key w3-mode-map "\C-?"     'scroll-down)
(define-key w3-mode-map "\C-c\C-b" 'w3-show-history-list)
(define-key w3-mode-map "\C-c\C-v" 'w3-version)
(define-key w3-mode-map "\C-k"     'w3-save-url)
(define-key w3-mode-map "\C-o"     'w3-fetch)
(define-key w3-mode-map "\C-q"     'w3-write-global-history)
(define-key w3-mode-map "\M-M"     'w3-mail-document-under-point)
(define-key w3-mode-map "\M-\C-i"  'w3-insert-this-url)
(define-key w3-mode-map "\M-m"	   'w3-mail-current-document)
(define-key w3-mode-map "\M-s"	   'w3-search)
(define-key w3-mode-map "\M-\r"    'w3-follow-inlined-image)
(define-key w3-mode-map "\r"       'w3-follow-link)
(define-key w3-mode-map "a"	   'w3-hotlist-add-document)
(define-key w3-mode-map "b"	   'w3-back-link)
(define-key w3-mode-map "d"        'w3-hotlist-delete)
(define-key w3-mode-map "f"	   'w3-forward-link)
(define-key w3-mode-map "g"        'w3-reload-document)
(define-key w3-mode-map "h"        'w3-help)
(define-key w3-mode-map "k"        'w3-save-url)
(define-key w3-mode-map "l"        'w3-goto-last-buffer)
(define-key w3-mode-map "m"        'w3-complete-link)
(define-key w3-mode-map "n"        'w3-forward-link)
(define-key w3-mode-map "o"	   'w3-open-local)
(define-key w3-mode-map "p"        'w3-print-this-url)
(define-key w3-mode-map "q"	   'w3-quit)
(define-key w3-mode-map "r"        'w3-reload-document)
(define-key w3-mode-map "s"        'w3-source-document)
(define-key w3-mode-map "u"        'w3-leave-buffer)
(define-key w3-mode-map "v"	   'w3-view-url)
(define-key w3-mode-map "w"        'w3-submit-bug)

(provide 'w3-vars)
