;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         ange-ftp.el
;; RCS:          $Header: ange-ftp.el,v 3.143 92/03/01 14:40:49 jwz Exp $
;; Description:  transparent FTP support for GNU Emacs
;; Author:       Andy Norman, ange@hplb.hpl.hp.com
;; Created:      Thu Oct 12 14:00:05 1989
;; Modified:     Tue Oct 22 16:04:11 1991 (Ange) ange@anorman
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1989, 1990, 1991 Andy Norman.
;;;
;;; Author: Andy Norman (ange@hplb.hpl.hp.com)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to ange@hplb.hpl.hp.com) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;; Description:
;;;
;;; This package attempts to make accessing files and directories using FTP
;;; from within GNU Emacs as simple and transparent as possible.  A subset of
;;; the common file-handling routines are extended to interact with FTP.

;;; Installation:
;;;
;;; Byte-compile ange-ftp.el to ange-ftp.elc and put them both in a directory
;;; on your load-path.  Load the package from your .emacs file with:
;;;
;;;   (require 'ange-ftp).
;;;
;;; ange-ftp can't sensibly be auto-loaded; you are either using it, or you
;;; ain't.

;;; Usage:
;;;
;;; Some of the common GNU Emacs file-handling operations have been made
;;; FTP-smart.  If one of these routines is given a filename that matches
;;; '/user@host:path' then it will spawn an FTP process connecting to machine
;;; 'host' as account 'user' and perform its operation on the file 'path'.
;;;
;;; For example: if find-file is given a filename of:
;;;
;;;   /ange@anorman:/tmp/notes
;;;
;;; then ange-ftp will spawn an FTP process, connect to the host 'anorman' as
;;; user 'ange', get the file '/tmp/notes' and pop up a buffer containing the
;;; contents of that file as if it were on the local filesystem.  If ange-ftp
;;; needed a password to connect then it would prompt the user in the
;;; minibuffer.

;;; Extended filename syntax:
;;;
;;; The default extended filename syntax is '/user@host:path', where the
;;; 'user@' part may be omitted.  This syntax can be customised to a certain
;;; extent by changing ange-ftp-path-format.  There are limitations.
;;;
;;; If the user part is omitted then ange-ftp will generate a default user
;;; instead whose value depends on the variable ange-ftp-default-user.

;;; Passwords:
;;;
;;; A password is required for each host / user pair.  This will be prompted
;;; for when needed, unless already set by calling ange-ftp-set-passwd, or
;;; specified in a *valid* ~/.netrc file.

;;; Passwords for user "anonymous":
;;;
;;; Passwords for the user "anonymous" (or "ftp") are handled specially.  The
;;; variable ange-ftp-generate-anonymous-password controls what happens: if
;;; the value of this variable is a string, then this is used as the password;
;;; if non-nil, then a password is created from the name of the user and the
;;; hostname of the machine on which GNU Emacs is running; if nil (the
;;; default) then the user is prompted for a password as normal.

;;; "Dumb" hosts:
;;;
;;; The FTP servers on some machines have problems if the 'ls' command is
;;; used.  The usual indication that something is wrong is when ange-ftp
;;; erroneously thinks that a directory is just a plain file.
;;;
;;; The routine ange-ftp-add-dumb-host can be called to tell ange-ftp to limit
;;; itself to the DIR command and not 'ls' for a given host.  Note that this
;;; change will take effect for the current GNU Emacs session only.
;;;
;;; If a large number of machines with similar hostnames have this problem
;;; then it is easier to change the value of ange-ftp-dumb-host-regexp
;;; directly.

;;; File name completion:
;;;
;;; Full file-name completion is supported on UNIX hosts and VMS hosts running
;;; MultiNet. If ange-ftp works at all on other hosts, don't expect file-name
;;; completion to work.

;;; FTP processes:
;;;
;;; When ange-ftp starts up an FTP process, it leaves it running for speed
;;; purposes.  Some FTP servers will close the connection after a period of
;;; time, but ange-ftp should be able to quietly reconnect the next time that
;;; the process is needed.
;;;
;;; The FTP process will be killed should the associated "*ftp user@host*"
;;; buffer be deleted.  This should not cause ange-ftp any grief.

;;; Binary file transfers:
;;;
;;; By default ange-ftp will transfer files in ASCII mode.  If a file being
;;; transferred matches the value of ange-ftp-binary-file-name-regexp then the
;;; FTP process will be toggled into BINARY mode before the transfer and back
;;; to ASCII mode after the transfer.

;;; Account passwords:
;;;
;;; Some FTP servers require an additional password which is sent by the
;;; ACCOUNT command.  ange-ftp partially supports this by allowing the user to
;;; specify an account password by either calling ange-ftp-set-account, or by
;;; specifying an account token in the .netrc file.  If the account password
;;; is set by either of these methods then ange-ftp will issue an ACCOUNT
;;; command upon starting the FTP process.

;;; Preloading:
;;;
;;; ange-ftp can be preloaded, but must be put in the site-init.el file and
;;; not the site-load.el file in order for the documentation strings for the
;;; functions being overloaded to be available.

;;; Status reports:
;;;
;;; Most ange-ftp commands that talk to the FTP process output a status
;;; message on what they are doing.  In addition, ange-ftp can take advantage
;;; of the FTP client's HASH command to display the status of transferring
;;; files and listing directories.  See the documentation for the variables
;;; ange-ftp-hash-mark-size and ange-ftp-process-verbose for more details.

;;; Gateways:
;;;
;;; Sometimes it is neccessary for the FTP process to be run on a different
;;; machine than the machine running GNU Emacs.  This can happen when the
;;; local machine has restrictions on what hosts it can access.
;;;
;;; ange-ftp has support for running the ftp process on a different (gateway)
;;; machine.  The way it works is as follows:
;;;
;;; 1) Set the variable 'ange-ftp-gateway-host' to the name of a machine
;;;    that doesn't have the access restrictions.
;;;
;;; 2) Set the variable 'ange-ftp-local-host-regexp' to a regular expression
;;;    that matches hosts that can be contacted from running a local ftp
;;;    process, but fails to match hosts that can't be accessed locally.  For
;;;    example:
;;;
;;;    "\\.hp\\.com$\\|^[^.]*$"
;;;
;;;    will match all hosts that are in the .hp.com domain, or don't have an
;;;    explicit domain in their name, but will fail to match hosts with
;;;    explicit domains or that are specified by their ip address.
;;;
;;; 3) Using NFS and symlinks, make sure that there is a shared directory with
;;;    the *same* name between the local machine and the gateway machine.
;;;    This directory is neccessary for temporary files created by ange-ftp.
;;;
;;; 4) Set the variable 'ange-ftp-gateway-tmp-name-template' to the name of
;;;    this directory plus an identifying filename prefix.  For example:
;;;
;;;    "/nfs/hplose/ange/ange-ftp"
;;;
;;;    where /nfs/hplose/ange is a directory that is shared between the
;;;    gateway machine and the local machine.
;;;
;;; The simplest way of getting a ftp process running on the gateway machine
;;; is if you can spawn a remote shell using either 'rsh' or 'remsh'.  If you
;;; can't do this for some reason such as security then points 7 onwards will
;;; discuss an alternative approach.
;;;
;;; 5) Set the variable ange-ftp-gateway-program to the name of the remote
;;;    shell process such as 'remsh' or 'rsh' if the default isn't correct.
;;;
;;; 6) Set the variable ange-ftp-gateway-program-interactive to nil if it
;;;    isn't already.  This tells ange-ftp that you are using a remote shell
;;;    rather than logging in using telnet or rlogin.
;;;
;;; That should be all you need to allow ange-ftp to spawn a ftp process on
;;; the gateway machine.  If you have to use telnet or rlogin to get to the
;;; gateway machine then follow the instructions below.
;;;
;;; 7) Set the variable ange-ftp-gateway-program to the name of the program
;;;    that lets you log onto the gateway machine.  This may be something like
;;;    telnet or rlogin.
;;;
;;; 8) Set the variable ange-ftp-gateway-prompt-pattern to a regular
;;;    expression that matches the prompt you get when you login to the
;;;    gateway machine.  Be very specific here; this regexp must not match
;;;    *anything* in your login banner except this prompt.
;;;    shell-prompt-pattern is far too general as it appears to match some
;;;    login banners from Sun machines.  For example:
;;;
;;;    "^$*$ *"
;;;
;;; 9) Set the variable ange-ftp-gateway-program-interactive to 't' to let
;;;    ange-ftp know that it has to "hand-hold" the login to the gateway
;;;    machine.
;;;
;;; 10) Set the variable ange-ftp-gateway-setup-term-command to a UNIX command
;;;     that will put the pty connected to the gateway machine into a
;;;     no-echoing mode, and will strip off carriage-returns from output from
;;;     the gateway machine.  For example:
;;;
;;;     "stty -onlcr -echo"
;;;
;;;     will work on HP-UX machines, whereas:
;;;
;;;     "stty -echo nl"
;;;
;;;     appears to work for some Sun machines.
;;;
;;; That's all there is to it.

;;; Smart gateways:
;;;
;;; If you have a "smart" ftp program that allows you to issue commands like
;;; "USER foo@bar" which do nice proxy things, then look at the variables
;;; ange-ftp-smart-gateway and ange-ftp-smart-gateway-port.

;;; VMS support:
;;;
;;; ange-ftp can handle (in a very simple way) FTP to machines running VMS
;;; with MultiNet.  Filename completion and file retrieval (are supposed to)
;;; work.
;;;
;;; VMS machines:
;;;
;;; The variable ange-ftp-vms-host-regexp if non-nil holds a regular
;;; expression matching hostnames that run the VMS operating system.  To add a
;;; host, say ymir.claremont.edu to the variable, do the following:
;;;
;;;   (setq ange-ftp-vms-host-regexp "^ymir.claremont.edu$")
;;;
;;; Auto-detection of hosts running MultiNet will be added shortly by checking
;;; the banner string after a "220" message.
;;;
;;; Filename Syntax:
;;;
;;; For ease of *implementation*, the user enters the VMS filename syntax in a
;;; UNIX-y way.  For example:
;;;
;;;   PUB$:[ANONYMOUS.SDSCPUB.NEXT]Readme.txt;1
;;;
;;; would be entered as:
;;;
;;;   PUB$$:/ANONYMOUS/SDSCPUB/NEXT/Readme.txt;1
;;;
;;; i.e. to log in as anonymous on ymir.claremont.edu and grab the file:
;;;
;;;   [.CSV.POLICY]RULES.MEM;1
;;;
;;; you would type:
;;;
;;;   C-x C-f /anonymous@ymir.claremont.edu:CSV/POLICY/RULES.MEM;1
;;;
;;; Note that the extension can be omitted from a filename to get the latest
;;; version.
;;;
;;; Bugs:
;;;
;;; - Doesn't handle .. ==> [-]
;;;
;;; - No dired support
;;;
;;; - Needs a . in a filename to distinguish from directory name
;;;
;;; - May not handle absolute directories correctly.

;;; Umask problems:
;;;
;;; Be warned that files created by using ange-ftp will take account of the
;;; umask of the ftp daemon process rather than the umask of the creating
;;; user.  This is particulary important when logging in as the root user.
;;; The way that I tighten up the ftp daemon's umask under HP-UX is to make
;;; sure that the umask is changed to 027 before I spawn /etc/inetd.  I
;;; suspect that there is something similar on other systems.

;;; Bugs:
;;;
;;; - expand-file-name has problems with "~" and ".." on relative paths.
;;;
;;; - symlinks pointing to directories don't work right.
;;;
;;; - some combinations of FTP clients and servers break and get out of sync
;;;   when asked to list a non-existent directory.  Some of the ai.mit.edu
;;;   machine cause this problem for some FTP clients.
;;;
;;; - wierd characters in filenames confuse ange-ftp.
;;;
;;; - null (blank) passwords confuse both ange-ftp and some FTP daemons.
;;;
;;; - ange-ftp likes to use pty's to talk to its FTP processes.  If GNU Emacs
;;;   for some reason creates a FTP process that only talks via pipes then
;;;   ange-ftp won't be getting the information it requires at the time that
;;;   it wants it since pipes flush at different times to pty's.  One
;;;   disgusting way around this problem is to talk to the FTP process via
;;;   rlogin which does the 'right' things with pty's.
;;;
;;; If you find any bugs or problems with this package, PLEASE e-mail the
;;; above author.  Ideas and constructive comments are especially welcome.

;;; ange-ftp-lovers:
;;;
;;; ange-ftp has its own mailing list modestly called ange-ftp-lovers.  All
;;; users of ange-ftp are welcome to subscribe (see below) and to discuss
;;; aspects of ange-ftp.  New versions of ange-ftp are posted periodically to
;;; the mailing list.
;;;
;;; To [un]subscribe to ange-ftp-lovers, or to report mailer problems with the
;;; list, please mail one of the following addresses:
;;;
;;;     ange-ftp-lovers-request@anorman.hpl.hp.com
;;; or
;;;     ange-ftp-lovers-request%anorman.hpl.hp.com@hplb.hpl.hp.com
;;; or
;;;     hplb.hpl.hp.com!anorman.hpl.hp.com!ange-ftp-lovers-request
;;; or
;;;     hplabs.hpl.hp.com!anorman.hpl.hp.com!ange-ftp-lovers-request
;;; 
;;; Please don't forget the -request part.
;;; 
;;; For mail to be posted directly to ange-ftp-lovers, send to one of the
;;; following addresses:
;;; 
;;;     ange-ftp-lovers@anorman.hpl.hp.com
;;; or
;;;     ange-ftp-lovers%anorman.hpl.hp.com@hplb.hpl.hp.com
;;; or
;;;     hplb.hpl.hp.com!anorman.hpl.hp.com!ange-ftp-lovers
;;; or
;;;     hplabs.hpl.hp.com!anorman.hpl.hp.com!ange-ftp-lovers
;;; 
;;; The latest version of ange-ftp can always be obtained via anonymous ftp
;;; from:
;;; 
;;;   ftp.gnu.ai.mit.edu:ange-ftp/ange-ftp.el.Z

;;; Warning:
;;;
;;; The following GNU Emacs functions are replaced by this program:
;;;
;;;   write-region
;;;   insert-file-contents
;;;   dired-readin
;;;   dired-revert
;;;   dired-call-process
;;;   diff
;;;   delete-file
;;;   read-file-name-internal
;;;   verify-visited-file-modtime
;;;   directory-files
;;;   backup-buffer
;;;   file-directory-p
;;;   file-writable-p
;;;   file-exists-p
;;;   file-readable-p
;;;   file-attributes
;;;   copy-file
;;;   rename-file
;;;   file-name-as-directory
;;;   file-name-directory
;;;   file-name-nondirectory
;;;   file-name-completion
;;;   directory-file-name
;;;   expand-file-name
;;;   file-name-all-completions

;;; LISPDIR ENTRY for the Elisp Archive
;;;    LCD Archive Entry:
;;;    ange-ftp|Andy Norman|ange@hplb.hpl.hp.com
;;;    |transparent FTP Support for GNU Emacs
;;;    |$Date: 92/03/01 14:40:49 $|$Revision: 3.143 $|

;;; Thanks:
;;;
;;; Many thanks to Roland McGrath for improving the filename syntax handling,
;;; for suggesting many enhancements and for numerous cleanups to the code.
;;;
;;; Thanks to Jamie Zawinski for bugfixes and for ideas such as gateways.
;;;
;;; Thanks to Ken Laprade for improved .netrc parsing and password reading.
;;;
;;; Thanks to Sebastian Kremer for tree dired support and for many ideas and
;;; bugfixes.
;;;
;;; Thanks to Joe Wells for bugfixes, non-UNIX system support, VOS support,
;;; and hostname completion.
;;;
;;; Finally, thanks to Keith Waclena, Mark D. Baushke, Terence Kelleher, Ping
;;; Zhou, Edward Vielmetti, Jack Repenning, Mike Balenger, Todd Kaufmann,
;;; Kjetil Svarstad, Tom Wurgler, Linus Tolke, Niko Makila, Carl Edman, Bill
;;; Trost, Dave Brennan, Dan Jacobson, Andy Scott, Steve Anderson, Sanjay
;;; Mathur, the folks on the ange-ftp-lovers mailing list and many others
;;; whose names I've forgotten who have helped to debug and fix problems with
;;; ange-ftp.el.

;;;; ------------------------------------------------------------
;;;; User customization variables.
;;;; ------------------------------------------------------------

(defvar ange-ftp-path-format
  '("^/\\(\\([^@/:]*\\)@\\)?\\([^@/:]*\\):\\(.*\\)" . (3 2 4))
  "*Format of a fully expanded remote pathname.  This is a cons
\(REGEXP . \(HOST USER PATH\)\), where REGEXP is a regular expression matching
the full remote pathname, and HOST, USER, and PATH are the numbers of
parenthesized expressions in REGEXP for the components (in that order).")

(defvar ange-ftp-multi-msgs
  "^[0-9][0-9][0-9]-"
  "*Regular expression matching messages from the ftp process that start
a multiline reply.")

(defvar ange-ftp-good-msgs
  "^220 \\|^230 \\|^226 \\|^25. \\|^221 \\|^200 \\|^[Hh]ash mark"
  "*Regular expression matching messages from the ftp process that indicate
that the action that was initiated has completed successfully.")

(defvar ange-ftp-skip-msgs
  (concat "^200 PORT \\|^331 \\|^150 \\|^350 \\|^[0-9]+ bytes \\|"
	  "^Connected \\|^$\\|^Remote system\\|^Using\\|^ \\|Password:\\|"
	  "^local:\\|^Trying\\|^125 ")
  "*Regular expression matching messages from the ftp process that can be
ignored.")

(defvar ange-ftp-fatal-msgs
  (concat "^ftp: \\|^Not connected\\|^530 \\|^4[25]1 \\|rcmd: \\|"
	  "^No control connection\\|unknown host\\|^lost connection")
  "*Regular expression matching messages from the FTP process that indicate
something has gone drastically wrong attempting the action that was
initiated and that the FTP process should (or already has) been killed.")

(defvar ange-ftp-gateway-fatal-msgs
  "No route to host\\|Connection closed\\|No such host\\|Login incorrect"
  "*Regular expression matching messages from the rlogin / telnet process that
indicates that logging in to the gateway machine has gone wrong.")

(defvar ange-ftp-tmp-name-template "/tmp/ange-ftp"
  "*Template given to make-temp-name to create temporary files.")

(defvar ange-ftp-gateway-tmp-name-template "/tmp/ange-ftp"
  "*Template given to make-temp-name to create temporary files when
ftp-ing through a gateway.  Files starting with this prefix need to
be accessible from BOTH the local machine and the gateway machine, 
and need to have the SAME name on both machines, that is, /tmp is probably
NOT what you want, since that is rarely cross-mounted.")

(defvar ange-ftp-netrc-filename "~/.netrc"
  "*File in .netrc format to search for passwords.")

(defvar ange-ftp-default-user nil
  "*User name to use when none is specied in a pathname.
If nil, then the name under which the user is logged in is used.
If non-nil but not a string, the user is prompted for the name.")

(defvar ange-ftp-default-password nil
  "*Password to use when the user is the same as ange-ftp-default-user.")

(defvar ange-ftp-default-account nil
  "*Account password to use when the user is the same as ange-ftp-default-user.")

(defvar ange-ftp-generate-anonymous-password nil
  "*If t, use a password of user@host when logging in as the anonymous user.
If a string then use that as the password.
If nil then prompt the user for a password.")

(defvar ange-ftp-dumb-host-regexp nil
  "*If non-nil, if the host being ftp'd to matches this regexp then the FTP
process uses the \'dir\' command to get directory information.")

(defvar ange-ftp-binary-file-name-regexp
  (concat "\\.Z$\\|\\.lzh$\\|\\.arc$\\|\\.zip$\\|\\.zoo$\\|\\.tar$\\|"
	  "\\.dvi$\\|\\.ps$\\|\\.elc$\\|TAGS$\\|\\.gif$")
  "*If a file matches this regexp then it is transferred in binary mode.")

(defvar ange-ftp-gateway-host nil
  "*Name of host to use as gateway machine when local FTP isn't possible.")

(defvar ange-ftp-local-host-regexp ".*"
  "*If a host being FTP'd to matches this regexp then the ftp process is started
locally, otherwise the FTP process is started on \`ange-ftp-gateway-host\'
instead.")

(defvar ange-ftp-gateway-program-interactive nil
  "*If non-nil then the gateway program is expected to connect to the gateway
machine and eventually give a shell prompt.  Both telnet and rlogin do something
like this.")

(defvar ange-ftp-gateway-program (if (eq system-type 'hpux) "remsh" "rsh")
  "*Name of program to spawn a shell on the gateway machine.  Valid candidates
are rsh (remsh on hp-ux), telnet and rlogin.  See also the gateway variable
above.")

(defvar ange-ftp-gateway-prompt-pattern "^[^#$%>;]*[#$%>;] *"
  "*Regexp used to detect that the logging-in sequence is completed on the
gateway machine and that the shell is now awaiting input.  Make this regexp as
strict as possible; it shouldn't match *anything* at all except the user's
initial prompt.  The above string will fail under most SUN-3's since it
matches the login banner.")

(defvar ange-ftp-gateway-setup-term-command
  (if (eq system-type 'hpux)
      "stty -onlcr -echo\n"
    "stty -echo nl\n")
  "*Command to use after logging in to the gateway machine to stop the terminal
echoing each command and to strip out trailing ^M characters.")

(defvar ange-ftp-smart-gateway nil
  "*If the gateway FTP is smart enough to use proxy server, then don't bother
telnetting etc, just issue a user@host command instead.")

(defvar ange-ftp-smart-gateway-port "21"
  "*Port on gateway machine to use when smart gateway is in operation.")

(defvar ange-ftp-hash-mark-size 1
  "*Number of kilobytes represented by the FTP client's #-mark.
If NIL then don't use #-marks.")

(defvar ange-ftp-process-verbose t
  "*If non-NIL then be chatty about interaction with the FTP process.")

(defvar ange-ftp-ftp-program-name "ftp"
  "*Name of FTP program to run.")

(defvar ange-ftp-gateway-ftp-program-name "ftp"
  "*Name of FTP program to run on gateway machine.
Some AT&T folks claim to use something called `pftp' here.")

(defvar ange-ftp-make-backup-files nil
  "*If non-nil, ange-ftp will make Emacs backup files on the remote host.")

;;;; ------------------------------------------------------------
;;;; Hash table support.
;;;; ------------------------------------------------------------

(require 'backquote)

(defun ange-ftp-make-hashtable (&optional size)
  "Make an obarray suitable for use as a hashtable.
SIZE, if supplied, should be a prime number."
  (make-vector (or size 511) 0))

(defun ange-ftp-map-hashtable (fun tbl)
  "Call FUNCTION on each key and value in HASHTABLE."
  (mapatoms
   (function 
    (lambda (sym)
      (and (get sym 'active)
	   (funcall fun (get sym 'key) (get sym 'val)))))
   tbl))

(defmacro ange-ftp-make-hash-key (key)
  "Convert KEY into a suitable key for a hashtable."
  (` (if (stringp (, key))
	 (, key)
       (prin1-to-string (, key)))))

(defun ange-ftp-get-hash-entry (key tbl)
  "Return the value associated with KEY in HASHTABLE."
  (let ((sym (intern-soft (ange-ftp-make-hash-key key) tbl)))
    (and sym
	 (get sym 'active)
	 (get sym 'val))))

(defun ange-ftp-put-hash-entry (key val tbl)
  "Record an association between KEY and VALUE in HASHTABLE."
  (let ((sym (intern (ange-ftp-make-hash-key key) tbl)))
    (put sym 'val val)
    (put sym 'key key)
    (put sym 'active t)))

(defun ange-ftp-del-hash-entry (key tbl)
  "Delete KEY from HASHTABLE."
  (let ((sym (intern-soft (ange-ftp-make-hash-key key) tbl)))
    (and sym (put sym 'active nil))))

(defun ange-ftp-hash-entry-exists-p (key tbl)
  "Return whether there is an association for KEY in TABLE."
  (let ((sym (intern-soft (ange-ftp-make-hash-key key) tbl)))
    (and sym (get sym 'active))))

(defun ange-ftp-hash-table-keys (tbl)
  "Return a sorted list of all the active keys in the hashtable, as strings."
  (sort (all-completions ""
			 tbl
			 (function (lambda (x) (get x 'active))))
	(function string-lessp)))

;;;; ------------------------------------------------------------
;;;; Internal variables.
;;;; ------------------------------------------------------------

(defconst ange-ftp-RCS-id "$Revision: 3.143 $")

(defvar ange-ftp-data-buffer-name "*ftp data*"
  "Buffer name to hold directory listing data received from ftp process.")

(defvar ange-ftp-process-line ""
  "Last line of output from FTP process that indicated success or failure of
FTP command.")

(defvar ange-ftp-process-string ""
  "Currently unprocessed output from the FTP process.")

(defvar ange-ftp-process-running nil
  "Boolean indicates whether the FTP process is currently handling
an action.")

(defvar ange-ftp-process-status nil
  "Set to t if an action sent to the FTP process succeeds.")

(defvar ange-ftp-process-msg nil
  "Message output before / after the FTP process has done a command.")

(defvar ange-ftp-process-multi-skip nil
  "Set to t while skipping a multi-line reply.")

(defvar ange-ftp-have-read-netrc nil
  "Boolean indicating whether the user's .netrc file has been read yet.")

(defvar ange-ftp-user-hashtable (ange-ftp-make-hashtable)
  "Hash table holding associations between HOST, USER pairs.")

(defvar ange-ftp-passwd-hashtable (ange-ftp-make-hashtable)
  "Mapping between a HOST, USER pair and a PASSWORD for them.")

(defvar ange-ftp-account-hashtable (ange-ftp-make-hashtable)
  "Mapping between a HOST, USER pair and a ACCOUNT password for them.")

(defvar ange-ftp-files-hashtable (ange-ftp-make-hashtable)
  "Hash table for storing directories and their respective files.")

(defvar ange-ftp-ls-cache-cmd nil
  "Last `ls' command issued by ange-ftp-ls.")

(defvar ange-ftp-ls-cache-file nil
  "Last file passed to ange-ftp-ls.")

(defvar ange-ftp-ls-cache-res nil
  "Last result returned from ange-ftp-ls.")

;;; ------------------------------------------------------------
;;; Match-data support (stolen from Kyle I think)
;;; ------------------------------------------------------------

(defmacro ange-ftp-save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list
     'let (list (list original '(match-data)))
     (list 'unwind-protect
           (cons 'progn body)
           (list 'store-match-data original)))))

(put 'ange-ftp-save-match-data 'lisp-indent-hook 0)
(put 'ange-ftp-save-match-data 'edebug-form-hook '(&rest form))

;;; ------------------------------------------------------------
;;; Enhanced message support.
;;; ------------------------------------------------------------

(defun ange-ftp-message (fmt &rest args)
  "Output the given message, but truncate to the size of the minibuffer
window."
  (let ((msg (apply (function format) fmt args))
	(max (window-width (minibuffer-window))))
    (if (>= (length msg) max)
	(setq msg (concat "> " (substring msg (- 3 max)))))
    (message "%s" msg)))

(defun ange-ftp-abbreviate-filename (file &optional new)
  "Abbreviate the given filename relative to the default-directory.  If the
optional parameter NEW is given and the non-directory parts match, only return
the directory part of the file."
  (ange-ftp-save-match-data
    (if (and default-directory
	     (string-match (concat "^"
				   (regexp-quote default-directory)
				   ".") file))
	(setq file (substring file (1- (match-end 0)))))
    (if (and new
	     (string-equal (file-name-nondirectory file)
			   (file-name-nondirectory new)))
	(setq file (file-name-directory file)))
    (or file "./")))

;;;; ------------------------------------------------------------
;;;; User / Host mapping support.
;;;; ------------------------------------------------------------

(defun ange-ftp-set-user (host user)
  "For a given HOST, set or change the default USER."
  (interactive "sHost: \nsUser: ")
  (ange-ftp-put-hash-entry host user ange-ftp-user-hashtable))

(defun ange-ftp-get-user (host)
  "Given a HOST, return the default USER."
  (or ange-ftp-have-read-netrc (ange-ftp-parse-netrc))
  (let ((user (ange-ftp-get-hash-entry host ange-ftp-user-hashtable)))
    (or user
	(cond ((stringp ange-ftp-default-user)
	       ;; We have a default name.  Use it.
	       ange-ftp-default-user)
	      (ange-ftp-default-user
	       ;; Ask the user and remember the response.
	       (let* ((enable-recursive-minibuffers t)
		      (user (read-string (format "User for %s: " host)
					 (user-login-name))))
		 (ange-ftp-set-user host user)
		 user))
	      ;; Default to the user's login name.
	      (t (user-login-name))))))

;;;; ------------------------------------------------------------
;;;; Password support.
;;;; ------------------------------------------------------------

(defun ange-ftp-read-passwd (prompt &optional default)
  "Read a password from the user. Echos a . for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
Optional DEFAULT is password to start with."
  (let ((pass (if default default ""))
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "%s%s"
	       prompt
	       (make-string (length pass) ?.))
      (setq c (read-char))
      (if (= c ?\C-u)
	  (setq pass "")
	(if (and (/= c ?\b) (/= c ?\177))
	    (setq pass (concat pass (char-to-string c)))
	  (if (> (length pass) 0)
	      (setq pass (substring pass 0 -1))))))
    (message "")
    (substring pass 0 -1)))

(defmacro ange-ftp-generate-passwd-key (host user)
  (` (concat (, host) "/" (, user))))

(defmacro ange-ftp-lookup-passwd (host user)
  (` (ange-ftp-get-hash-entry (ange-ftp-generate-passwd-key (, host) (, user))
			      ange-ftp-passwd-hashtable)))

(defun ange-ftp-set-passwd (host user passwd)
  "For a given HOST and USER, set or change the associated PASSWORD."
  (interactive (list (read-string "Host: ")
		     (read-string "User: ")
		     (ange-ftp-read-passwd "Password: ")))
  (ange-ftp-put-hash-entry (ange-ftp-generate-passwd-key host user)
			   passwd
			   ange-ftp-passwd-hashtable))

(defun ange-ftp-get-host-with-passwd (user)
  "Given a USER, return a host we know the password for."
  (or ange-ftp-have-read-netrc (ange-ftp-parse-netrc))
  (catch 'found-one
    (ange-ftp-map-hashtable
     (function (lambda (host val)
		 (if (ange-ftp-lookup-passwd host user)
		     (throw 'found-one host))))
     ange-ftp-user-hashtable)
    nil))

(defun ange-ftp-get-passwd (host user)
  "Given a HOST and USER, return the FTP password, prompting if it was not
previously set."
  (or ange-ftp-have-read-netrc (ange-ftp-parse-netrc))

  ;; look up password in the hash table first; user might have overriden the
  ;; defaults.
  (cond ((ange-ftp-lookup-passwd host user))
	
	;; see if default user and password set from the .netrc file.
	((and (stringp ange-ftp-default-user)
	      ange-ftp-default-password
	      (string-equal user ange-ftp-default-user))
	 ange-ftp-default-password)
	
	;; anonymous ftp password is handled specially since there is an
	;; unwritten rule about how that is used on the Internet.
	((and (or (string-equal user "anonymous")
		  (string-equal user "ftp"))
	      ange-ftp-generate-anonymous-password)
	 (if (stringp ange-ftp-generate-anonymous-password)
	     ange-ftp-generate-anonymous-password
	   (concat (user-login-name) "@" (system-name))))
	
	;; see if same user has logged in to other hosts; if so then prompt
	;; with the password that was used there.
	(t
	 (let* ((other (ange-ftp-get-host-with-passwd user))
		(passwd (if other
			    
			    ;; found another machine with the same user.
			    ;; Try that account.
			    (ange-ftp-read-passwd
			     (format "passwd for %s@%s (same as %s@%s): "
				     user host user other)
			     (ange-ftp-lookup-passwd other user))
			  
			  ;; I give up.  Ask the user for the password.
			  (ange-ftp-read-passwd
			   (format "Password for %s@%s: " user host)))))
	   (ange-ftp-set-passwd host user passwd)
	   passwd))))

;;;; ------------------------------------------------------------
;;;; Account support
;;;; ------------------------------------------------------------

;; Account passwords must be either specified in the .netrc file, or set
;; manually by calling ange-ftp-set-account.  For the moment, ange-ftp doesn't
;; check to see whether the FTP process is actually prompting for an account
;; password.
 
(defun ange-ftp-set-account (host user account)
  "For a given HOST and USER, set or change the associated ACCOUNT password."
  (interactive (list (read-string "Host: ")
		     (read-string "User: ")
		     (ange-ftp-read-passwd "Account password: ")))
  (ange-ftp-put-hash-entry (ange-ftp-generate-passwd-key host user)
			   account
			   ange-ftp-account-hashtable))

(defun ange-ftp-get-account (host user)
  "Given a HOST and USER, return the FTP account."
  (or ange-ftp-have-read-netrc (ange-ftp-parse-netrc))
  (or (ange-ftp-get-hash-entry (ange-ftp-generate-passwd-key host user)
			       ange-ftp-account-hashtable)
      (and (stringp ange-ftp-default-user)
	   (string-equal user ange-ftp-default-user)
	   ange-ftp-default-account)))

;;;; ------------------------------------------------------------
;;;; ~/.netrc support
;;;; ------------------------------------------------------------

(defun ange-ftp-chase-symlinks (file)
  "Return the filename that FILENAME references, following all symbolic links."
  (let (temp)
    (while (setq temp (file-symlink-p file))
      (setq file
	    (if (file-name-absolute-p temp)
		temp
	      (concat (file-name-directory file) temp)))))
  file)

(defun ange-ftp-parse-netrc-token (token limit)
  "Move along current line looking for the value of the TOKEN.  Valid
separators between TOKEN and its value are commas and whitespace.
Second arg LIMIT is a limit for the search."
  (if (search-forward token limit t)
      (let (beg)
	(skip-chars-forward ", \t\r\n" limit)
	(if (looking-at "\"")		;quoted token value
	    (progn (forward-char 1)
		   (setq beg (point))
		   (skip-chars-forward "^\"" limit)
		   (forward-char 1)
		   (buffer-substring beg (1- (point))))
	  (setq beg (point))
	  (skip-chars-forward "^, \t\r\n" limit)
	  (buffer-substring beg (point))))))

(defun ange-ftp-parse-netrc-group ()
  "Extract the values for the tokens  \`machine\', \`login\', \`password\'
and \`account\' in the current buffer.  If successful, record the information
found."
  (beginning-of-line)
  (let ((start (point))
	(end (progn (re-search-forward "machine\\|default"
				       (point-max) 'end 2) (point)))
	machine login password account)
    (goto-char start)
    (setq machine  (ange-ftp-parse-netrc-token "machine"  end)
	  login    (ange-ftp-parse-netrc-token "login"    end)
	  password (ange-ftp-parse-netrc-token "password" end)
	  account  (ange-ftp-parse-netrc-token "account"  end))
    (if (and machine login)
	;; found a `machine` token.
	(progn
	  (ange-ftp-set-user machine login)
	  (ange-ftp-set-passwd machine login password)
	  (and account
	       (ange-ftp-set-account machine login account)))
      (goto-char start)
      (if (search-forward "default" end t)
	  ;; found a `default' token
	  (progn
	    (setq login    (ange-ftp-parse-netrc-token "login"    end)
		  password (ange-ftp-parse-netrc-token "password" end)
		  account  (ange-ftp-parse-netrc-token "account"  end))
	    (and login
		 (setq ange-ftp-default-user login))
	    (and password
		 (setq ange-ftp-default-password password))
	    (and account
		 (setq ange-ftp-default-account account)))))
    (goto-char end)))

(defun ange-ftp-parse-netrc ()
  "If ~/.netrc file exists and has the correct permissions then extract the
\`machine\', \`login\', \`password\' and \`account\' information from within."

  ;; We set this before actually doing it to avoid the possibility
  ;; of an infinite loop if ange-ftp-netrc-filename is an FTP file.
  (setq ange-ftp-have-read-netrc t)

  (let* ((file (ange-ftp-chase-symlinks
		(expand-file-name ange-ftp-netrc-filename)))
	 (attr (file-attributes file)))
    (if attr				; File exits.
	(ange-ftp-save-match-data
	  (if (and (eq (nth 2 attr) (user-uid)) ; Same uids.
		   (string-match ".r..------" (nth 8 attr)))
	      (save-excursion
		(set-buffer (generate-new-buffer "*ftp-.netrc*"))
		(insert-file-contents file)
		(goto-char (point-min))
		(while (not (eobp))
		  (ange-ftp-parse-netrc-group))
		(kill-buffer (current-buffer)))
	    (ange-ftp-message "skipping badly configured .netrc file"))))))

(defun ange-ftp-generate-root-prefixes ()
  "Return a list of prefixes of the form '/user@host:' to be used when
completion is done in the root directory."
  (or ange-ftp-have-read-netrc (ange-ftp-parse-netrc))
  (ange-ftp-save-match-data
    (let (res)
      (ange-ftp-map-hashtable
       (function
	(lambda (key value)
	  (if (string-match "^[^/]*\\(/\\).*$" key)
	      (let ((host (substring key 0 (match-beginning 1)))
		    (user (substring key (match-end 1))))
		(setq res (cons (list (concat user "@" host ":"))
				res))))))
       ange-ftp-passwd-hashtable)
      (ange-ftp-map-hashtable
       (function (lambda (host user)
		   (setq res (cons (list (concat host ":"))
				   res))))
       ange-ftp-user-hashtable)
      (or res '(())))))

;;;; ------------------------------------------------------------
;;;; Remote pathname syntax support.
;;;; ------------------------------------------------------------

(defmacro ange-ftp-ftp-path-component (n ns path)
  "Extract the Nth ftp path component from NS."
  (` (let ((elt (nth (, n) (, ns))))
       (substring (, path) (match-beginning elt) (match-end elt)))))

(defvar ange-ftp-ftp-path-arg "")
(defvar ange-ftp-ftp-path-res nil)

(defun ange-ftp-ftp-path (path)
  "Parse PATH according to ange-ftp-path-format (which see).
Returns a list (HOST USER PATH), or nil if PATH does not match the format."
  (if (string-equal path ange-ftp-ftp-path-arg)
      ange-ftp-ftp-path-res
    (setq ange-ftp-ftp-path-arg path
	  ange-ftp-ftp-path-res
	  (ange-ftp-save-match-data
	    (if (string-match (car ange-ftp-path-format) path)
		(let* ((ns (cdr ange-ftp-path-format))
		       (host (ange-ftp-ftp-path-component 0 ns path))
		       (user (ange-ftp-ftp-path-component 1 ns path))
		       (path (ange-ftp-ftp-path-component 2 ns path)))
		  (if (zerop (length user))
		      (setq user (ange-ftp-get-user host)))
		  (list host user path))
	      nil)))))

(defun ange-ftp-replace-path-component (fullpath path)
  "Take a FULLPATH that matches according to ange-ftp-path-format and
replace the path component with PATH."
  (ange-ftp-save-match-data
    (if (string-match (car ange-ftp-path-format) fullpath)
	(let* ((ns (cdr ange-ftp-path-format))
	       (elt (nth 2 ns)))
	  (concat (substring fullpath 0 (match-beginning elt))
		  path
		  (substring fullpath (match-end elt)))))))

;;;; ------------------------------------------------------------
;;;; Miscellaneous utils.
;;;; ------------------------------------------------------------

(defun ange-ftp-ftp-process-buffer (host user)
  "Return the name of the buffer that collects output from the ftp process
connected to the given HOST and USER pair."
  (concat "*ftp " user "@" host "*"))

(defun ange-ftp-error (host user msg)
  "Display the last chunk of output from the ftp process for the given HOST
USER pair, and signal an error including MSG in the text."
  (let ((cur (selected-window))
	(pop-up-windows t))
    (pop-to-buffer
     (get-buffer-create
      (ange-ftp-ftp-process-buffer host user)))
    (goto-char (point-max))
    (select-window cur))
  (error "ange-ftp: %s" msg))

(defun ange-ftp-set-buffer-mode ()
  "Set the correct modes for the current buffer if it is visiting a remote
file."
  (if (and (stringp buffer-file-name)
	   (ange-ftp-ftp-path buffer-file-name))
      (progn
	(auto-save-mode 0)
	(make-variable-buffer-local 'revert-buffer-function)
	(setq revert-buffer-function 'ange-ftp-revert-buffer))))

(defun ange-ftp-kill-ftp-process (buffer)
  "If the BUFFER's visited filename or default-directory is an ftp filename
then kill the related ftp process."
  (interactive "bKill FTP process associated with buffer: ")
  (if (null buffer)
      (setq buffer (current-buffer)))
  (let ((file (or (buffer-file-name) default-directory)))
    (if file
	(let ((parsed (ange-ftp-ftp-path (expand-file-name file))))
	  (if parsed
	      (let ((host (nth 0 parsed))
		    (user (nth 1 parsed)))
		(kill-buffer (ange-ftp-ftp-process-buffer host user))))))))

(defun ange-ftp-quote-string (string)
  "Quote any characters in STRING that may confuse the ftp process."
  (apply (function concat)
	 (mapcar (function
		   (lambda (char)
		     (if (or (<= char ? )
			     (> char 126)
			     (= char ?\\))
			 (vector ?\\ char)
		       (vector char))))
		 string)))

;;;; ------------------------------------------------------------
;;;; FTP process filter support.
;;;; ------------------------------------------------------------

(defun ange-ftp-process-handle-line (line proc)
  "Look at the given LINE from the ftp process PROC.  Try to catagorize it
into one of four categories: good, skip, fatal, or unknown."
  (cond ((string-match ange-ftp-skip-msgs line)
	 t)
	((string-match ange-ftp-good-msgs line)
	 (setq ange-ftp-process-running nil
	       ange-ftp-process-status t
	       ange-ftp-process-line line))
	((string-match ange-ftp-fatal-msgs line)
	 (delete-process proc)
	 (setq ange-ftp-process-running nil
	       ange-ftp-process-line line))
	((string-match ange-ftp-multi-msgs line)
	 (setq ange-ftp-process-multi-skip t))
	(ange-ftp-process-multi-skip
	 t)
	(t
	 (setq ange-ftp-process-running nil
	       ange-ftp-process-line line))))

(defun ange-ftp-process-log-string (proc str)
  "For a given PROCESS, log the given STRING at the end of its
associated buffer."
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer proc))
	  (setq moving (= (point) (process-mark proc)))
	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark proc))
	    (insert str)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

(defvar ange-ftp-hash-mark-count 0
  "Number of #-marks seen so far this transaction.")

(defun ange-ftp-process-handle-hash (str)
  "Remove hash marks from STRING and display count so far."
  (setq str (concat (substring str 0 (match-beginning 0))
		    (substring str (match-end 0)))
	ange-ftp-hash-mark-count (+ (- (match-end 0)
				       (match-beginning 0))
				    ange-ftp-hash-mark-count))
  (and ange-ftp-process-msg
       ange-ftp-process-verbose
       (ange-ftp-message "%s...%dk" ange-ftp-process-msg
			 (* ange-ftp-hash-mark-size
			    ange-ftp-hash-mark-count)))
  str)

(defun ange-ftp-process-filter (proc str)
  "Build up a complete line of output from the ftp PROCESS and pass it
on to ange-ftp-process-handle-line to deal with."
  (ange-ftp-save-match-data
    ;; handle hash mark printing
    (and ange-ftp-hash-mark-size
	 ange-ftp-process-running
	 (string-match "^#+$" str)
	 (setq str (ange-ftp-process-handle-hash str)))
    (ange-ftp-process-log-string proc str)
    (if ange-ftp-process-running
	(progn
	  (setq ange-ftp-process-string (concat ange-ftp-process-string str))
	  ;; if we gave an empty password to the USER command earlier then we
	  ;; should send a null password now.
	  (if (string-match "Password: *$" ange-ftp-process-string)
	      (send-string proc "\n"))))
    (while (and ange-ftp-process-running
		(string-match "\n" ange-ftp-process-string))
      (let ((line (substring ange-ftp-process-string 0 (match-beginning 0))))
	(setq ange-ftp-process-string (substring ange-ftp-process-string
						 (match-end 0)))
	(while (string-match "^ftp> *" line)
	  (setq line (substring line (match-end 0))))
	(ange-ftp-process-handle-line line proc)))))

(defun ange-ftp-process-sentinel (proc str)
  "When ftp process changes state, nuke all file-entries in cache."
  (ange-ftp-save-match-data
    (let ((name (process-name proc)))
      (if (string-match "\\*ftp \\([^@]+\\)@\\([^*]+\\)*" name)
	  (let ((user (substring name (match-beginning 1) (match-end 1)))
		(host (substring name (match-beginning 2) (match-end 2))))
	    (ange-ftp-wipe-file-entries host user))))
    (setq ange-ftp-ls-cache-file nil)))

;;;; ------------------------------------------------------------
;;;; Gateway support.
;;;; ------------------------------------------------------------

(defun ange-ftp-use-gateway-p (host)
  "Returns whether to access this host via a normal (non-smart) gateway."
  ;; yes, I know that I could simplify the following expression, but it is
  ;; clearer (to me at least) this way.
  (and (not ange-ftp-smart-gateway)
       (ange-ftp-save-match-data
	 (not (string-match ange-ftp-local-host-regexp host)))))

(defun ange-ftp-use-smart-gateway-p (host)
  "Returns whether to access this host via a smart gateway."
  (and ange-ftp-smart-gateway
       (ange-ftp-save-match-data
	 (not (string-match ange-ftp-local-host-regexp host)))))
       
(defun ange-ftp-make-tmp-name (host)
  (make-temp-name (if (ange-ftp-use-gateway-p host)
		      ange-ftp-gateway-tmp-name-template
		    ange-ftp-tmp-name-template)))


;;;; ------------------------------------------------------------
;;;; Interactive gateway program support.
;;;; ------------------------------------------------------------

(defvar ange-ftp-gwp-running t)
(defvar ange-ftp-gwp-status nil)

(defun ange-ftp-gwp-sentinel (proc str)
  (setq ange-ftp-gwp-running nil))

(defun ange-ftp-gwp-filter (proc str)
  (ange-ftp-save-match-data
    (ange-ftp-process-log-string proc str)
    (cond ((string-match "login: *$" str)
	   (send-string proc
			(concat
			 (let ((ange-ftp-default-user t))
			   (ange-ftp-get-user ange-ftp-gateway-host))
			 "\n")))
	  ((string-match "Password: *$" str)
	   (send-string proc
			(concat
			 (ange-ftp-get-passwd ange-ftp-gateway-host
					      (ange-ftp-get-user
					       ange-ftp-gateway-host))
			 "\n")))
	  ((string-match ange-ftp-gateway-fatal-msgs str)
	   (delete-process proc)
	   (setq ange-ftp-gwp-running nil))
	  ((string-match ange-ftp-gateway-prompt-pattern str)
	   (setq ange-ftp-gwp-running nil
		 ange-ftp-gwp-status t)))))

(defun ange-ftp-gwp-start (host user name args)
  "Login to the gateway machine and fire up an ftp process."
  (let* ((gw-user (ange-ftp-get-user ange-ftp-gateway-host))
	 (proc (start-process name name 
			      ange-ftp-gateway-program
			      ange-ftp-gateway-host))
	 (ftp (mapconcat (function (lambda (x) x)) args " ")))
    (process-kill-without-query proc)
    (set-process-sentinel proc (function ange-ftp-gwp-sentinel))
    (set-process-filter proc (function ange-ftp-gwp-filter))
    (set-marker (process-mark proc) (point))
    (setq ange-ftp-gwp-running t
	  ange-ftp-gwp-status nil)
    (ange-ftp-message "Connecting to gateway %s..." ange-ftp-gateway-host)
    (while ange-ftp-gwp-running		;perform login sequence
      (accept-process-output proc))
    (if (not ange-ftp-gwp-status)
	(ange-ftp-error host user "unable to login to gateway"))
    (ange-ftp-message "Connecting to gateway %s...done" ange-ftp-gateway-host)
    (setq ange-ftp-gwp-running t
	  ange-ftp-gwp-status nil)
    (process-send-string proc ange-ftp-gateway-setup-term-command)
    (while ange-ftp-gwp-running		;zap ^M's and double echoing.
      (accept-process-output proc))
    (if (not ange-ftp-gwp-status)
	(ange-ftp-error host user "unable to set terminal modes on gateway"))
    (setq ange-ftp-gwp-running t
	  ange-ftp-gwp-status nil)
    (process-send-string proc (concat "exec " ftp "\n")) ;spawn ftp process
    proc))

;;;; ------------------------------------------------------------
;;;; Support for sending commands to the ftp process.
;;;; ------------------------------------------------------------

(defun ange-ftp-raw-send-cmd (proc cmd &optional msg)
  "Low-level routine to send the given ftp CMD to the ftp PROCESS.
MSG is an optional message to output before and after the command.
Returns non-nil if successful."
  (if (eq (process-status proc) 'run)
      (save-excursion
	(setq ange-ftp-process-string ""
	      ange-ftp-process-line ""
	      ange-ftp-process-running t
	      ange-ftp-process-status nil
	      ange-ftp-process-multi-skip nil
	      ange-ftp-process-msg msg
	      ange-ftp-hash-mark-count 0
	      cmd (concat cmd "\n"))
	(and msg ange-ftp-process-verbose (ange-ftp-message "%s..." msg))
	(set-buffer (process-buffer proc))
	(goto-char (point-max))
	(move-marker last-input-start (point))
	;; don't insert the password into the buffer on the USER command.
	(ange-ftp-save-match-data
	  (if (string-match "^user \"[^\"]*\"" cmd)
	      (insert (substring cmd 0 (match-end 0)) " Turtle Power!\n")
	    (insert cmd)))
	(move-marker last-input-end (point))
	(send-string proc cmd)
	(set-marker (process-mark proc) (point))
	(while ange-ftp-process-running
	  (accept-process-output proc))
	(and msg ange-ftp-process-verbose ange-ftp-process-status
	     (ange-ftp-message "%s...done" msg))
	ange-ftp-process-status)))

(defun ange-ftp-start-process (host user name)
  "Spawn a new ftp process ready to connect to machine HOST and give it NAME.
If HOST is only ftp-able through a gateway machine then spawn a shell
on the gateway machine to do the ftp instead."
  (let* ((use-gateway (ange-ftp-use-gateway-p host))
	 (ftp-prog (if use-gateway 
		       ange-ftp-gateway-ftp-program-name
		     ange-ftp-ftp-program-name))
	 (args (list ftp-prog "-i" "-n" "-g" "-v"))
	 proc)
    (if use-gateway
	(if ange-ftp-gateway-program-interactive
	    (setq proc (ange-ftp-gwp-start host user name args))
	  (setq proc (apply 'start-process name name
			    (append (list ange-ftp-gateway-program
					  ange-ftp-gateway-host)
				    args))))
      (setq proc (apply 'start-process name name args)))
    (process-kill-without-query proc)
    (set-process-sentinel proc (function ange-ftp-process-sentinel))
    (set-process-filter proc (function ange-ftp-process-filter))
    (save-excursion
      (set-buffer (process-buffer proc))
      (ange-ftp-shell-mode))
    (accept-process-output proc)	;wait for ftp startup message
    proc))

(defun ange-ftp-get-process (host user)
  "Return the process object for a ftp process connected to HOST and
logged in as USER.  Create a new process if needed."
  (let* ((name (ange-ftp-ftp-process-buffer host user))
	 (proc (get-process name)))
    (if (and proc (eq (process-status proc) 'run))
	proc
      (let ((pass (ange-ftp-quote-string
		   (ange-ftp-get-passwd host user)))
	    (account (ange-ftp-get-account host user)))
	(let (ange-ftp-hash-mark-size)	;don't want #-s fouling up login
	  (setq proc (ange-ftp-start-process host user name))
	  (if (ange-ftp-use-smart-gateway-p host)
	      (progn
		(or (ange-ftp-raw-send-cmd proc
					   (format "open %s %s"
						   ange-ftp-gateway-host
						   ange-ftp-smart-gateway-port)
					   (format "Opening FTP connection to %s via %s"
						   host
						   ange-ftp-gateway-host))
		    (ange-ftp-error host user "OPEN request failed"))
		(or (ange-ftp-raw-send-cmd proc (format "user \"%s\"@%s %s"
							user host pass)
					   (format "Logging in as user %s@%s"
						   user host))
		    (progn
		      (ange-ftp-set-passwd host user nil) ;reset password.
		      (ange-ftp-error host user "USER request failed"))))

	    ;; not going through a "smart" gateway
	    (or (ange-ftp-raw-send-cmd proc (format "open %s" host)
				       (format "Opening FTP connection to %s" host))
		(ange-ftp-error host user "OPEN request failed"))
	    (or (ange-ftp-raw-send-cmd proc (format "user \"%s\" %s" user pass)
				       (format "Logging in as user %s@%s" user host))
		(progn
		  (ange-ftp-set-passwd host user nil) ;reset password.
		  (ange-ftp-error host user "USER request failed")))
	    (if account
		(or (ange-ftp-raw-send-cmd proc
					   (format "account %s"
						   (ange-ftp-quote-string account))
					   (format "sending account password"))
		    (progn
		      (ange-ftp-set-account host user nil) ;reset account password
		      (ange-ftp-error host user "ACCOUNT request failed"))))))
	(if ange-ftp-hash-mark-size
	    (ange-ftp-raw-send-cmd proc "hash"))) ;not fatal if this fails
      proc)))

(defun ange-ftp-host-type (host)
  "Return a symbol which represents the type of the HOST given."
  (cond ((ange-ftp-dumb-host host)
	 'dumb-unix)
	((and (fboundp 'ange-ftp-vos-host)
	      (ange-ftp-vos-host host))
	 'vos)
	((and (fboundp 'ange-ftp-vms-host)
	      (ange-ftp-vms-host host))
	 'vms)
	(t
	 'unix)))

(defvar ange-ftp-fix-path-func-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC is a routine
which can change a UNIX path into a path more suitable for a host of type
TYPE.")

(defvar ange-ftp-fix-dir-path-func-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC is a routine
which can change UNIX directory path into a directory path more suitable
for a host of type TYPE.")

(defvar ange-ftp-dumb-host-types '(dumb-unix)
  "Beats me!")

(defun ange-ftp-send-cmd (host user cmd &optional msg)
  "Find an ftp process connected to HOST logged in as USER and send it CMD.
MSG is an optional status message to be output before and after issuing the
command.
Returns whether successful."
  
  ;; Handle conversion to remote pathname syntax and remote ls option
  ;; capability.
  (let* ((cmd0 (car cmd))
	 cmd1 cmd2
	 (host-type (ange-ftp-host-type host))
	 ;; should really abstract this next idiom -- ange.
	 (fix-pathname-func (or (cdr (assq host-type
					   ange-ftp-fix-path-func-alist))
				'identity)))
    (cond
     ((eq cmd0 'dir)
      ;; cmd == 'dir "remote-path" "local-path" "ls-switches"
      (setq cmd1 (funcall
		  (or (cdr (assq host-type ange-ftp-fix-dir-path-func-alist))
		      'identity)
		  (nth 1 cmd)))
      ;; If the remote ls can take switches, put them in
      (or (memq host-type ange-ftp-dumb-host-types)
	  (setq cmd0 'ls
		cmd1 (format "\"%s %s\"" (nth 3 cmd) cmd1))))
     ;; First argument is the remote pathname
     ((memq cmd0 '(get delete mkdir rmdir))
      (setq cmd1 (funcall fix-pathname-func (nth 1 cmd))))
     ;; Second argument is the remote pathname
     ((memq cmd0 '(append put chmod))
      (setq cmd2 (funcall fix-pathname-func (nth 2 cmd))))
     ;; Both arguments are remote pathnames
     ((eq cmd0 'rename)
      (setq cmd1 (funcall fix-pathname-func (nth 1 cmd))
	    cmd2 (funcall fix-pathname-func (nth 2 cmd))))
     )
    
    ;; Turn the command into one long string
    (setq cmd0 (symbol-name cmd0))
    (setq cmd (mapconcat 'identity (list cmd0
					 (or cmd1 (nth 1 cmd) "")
					 (or cmd2 (nth 2 cmd) ""))
			 " ")))
  
  ;; Actually send the resulting command.
  (let ((proc (ange-ftp-get-process host user)))
    (or (ange-ftp-raw-send-cmd proc cmd msg)
	;; Failed, try ONCE more.
	(and (setq proc (ange-ftp-get-process host user))
	     (ange-ftp-raw-send-cmd proc cmd msg)))))

;;;; ------------------------------------------------------------
;;;; Simple FTP process shell support.
;;;; ------------------------------------------------------------

(require 'shell)

(defvar ange-ftp-shell-mode-map nil)

(defun ange-ftp-shell-mode ()
  "Major mode for interacting with an FTP process.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

The following commands imitate the usual Unix interrupt and editing
control characters:
\\{ange-ftp-shell-mode-map}
Runs ange-ftp-shell-mode-hook if not nil."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (kill-all-local-variables)
    (if (not ange-ftp-shell-mode-map)
	(progn
	  (setq ange-ftp-shell-mode-map (copy-keymap shell-mode-map))
	  (define-key ange-ftp-shell-mode-map "\C-m" 'ange-ftp-shell-send-input)))
    (use-local-map ange-ftp-shell-mode-map)
    (setq major-mode 'ange-ftp-shell-mode)
    (setq mode-name "ange-ftp")
    (setq mode-line-process '(": %s"))
    (make-local-variable 'last-input-start)
    (setq last-input-start (make-marker))
    (make-local-variable 'last-input-end)
    (setq last-input-end (make-marker))
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (run-hooks 'ange-ftp-shell-mode-hook)))

(defun ange-ftp-shell-send-input ()
  "Send input to FTP process.
At end of buffer, sends all text after last output as input to the subshell,
including a newline inserted at the end.  When not at end, copies current line
to the end of the buffer and sends it, after first attempting to discard any
prompt at the beginning of the line."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (or process
	(error "Current buffer has no process"))
    (end-of-line)
    (if (eobp)
	(progn
	  (move-marker last-input-start
		       (process-mark process))
	  (insert ?\n)
	  (move-marker last-input-end (point)))
      (beginning-of-line)
      (re-search-forward "ftp> *"
			 (save-excursion (end-of-line) (point))
			 t)
      (let ((copy (buffer-substring (point)
				    (progn (forward-line 1) (point)))))
	(goto-char (point-max))
	(move-marker last-input-start (point))
	(insert copy)
	(move-marker last-input-end (point))))
    (process-send-region process last-input-start last-input-end)
    (set-marker (process-mark process) (point))))

;;;; ------------------------------------------------------------
;;;; Remote file and directory listing support.
;;;; ------------------------------------------------------------

(defun ange-ftp-dumb-host (host)
  "Returns whether HOST's FTP server doesn't like \'ls\' or \'dir\' commands
to take switch arguments."
  (and ange-ftp-dumb-host-regexp
       (ange-ftp-save-match-data
	 (string-match ange-ftp-dumb-host-regexp host))))

(defun ange-ftp-add-dumb-host (host)
  "Interactively adds a given HOST to ange-ftp-dumb-host-regexp."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name)
				      (and (eq major-mode 'dired-mode)
					   dired-directory))))
			(and name (car (ange-ftp-ftp-path name)))))))
  (if (not (ange-ftp-dumb-host host))
      (setq ange-ftp-dumb-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-dumb-host-regexp "\\|")
		    ange-ftp-dumb-host-regexp))))

(defvar ange-ftp-parse-list-func-alist nil
  "Association list of \( TYPE \. FUNC \) pairs.  The FUNC is a routine
which can parse the output from a DIR listing for a host of type TYPE.")

(defun ange-ftp-ls (file lsargs parse)
  "Return the output of an `DIR' or `ls' command done over ftp.
FILE is the full name of the remote file, LSARGS is any args to pass to the
`ls' command, and PARSE specifies that the output should be parsed and stored
away in the internal cache."
  (let ((parsed (ange-ftp-ftp-path file)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (path (ange-ftp-quote-string (nth 2 parsed)))
	       (temp (ange-ftp-make-tmp-name host))
	       lscmd)
	  (if (string-equal path "")
	      (setq path ".")
	    (setq path (ange-ftp-real-directory-file-name path)))
	  (setq lscmd (list 'dir path temp lsargs))
	  (if (and ange-ftp-ls-cache-file
		   (string-equal file ange-ftp-ls-cache-file)
		   (equal lscmd ange-ftp-ls-cache-cmd))
	      ange-ftp-ls-cache-res
	    (unwind-protect
		(if (ange-ftp-send-cmd host user lscmd
				       (format "Listing %s"
					       (ange-ftp-abbreviate-filename file)))
		    (save-excursion
		      (set-buffer (get-buffer-create ange-ftp-data-buffer-name))
		      (erase-buffer)
		      (if (file-readable-p temp)
			  (insert-file-contents temp)
			(sit-for 5)	;wait for file to possibly appear
			(ange-ftp-error host user
					(format "list data file %s not readable"
						temp)))
		      (if parse
			  (ange-ftp-put-hash-entry
			   file
			   (funcall (or (cdr (assq (ange-ftp-host-type host)
						   ange-ftp-parse-list-func-alist))
					'ange-ftp-parse-dired-listing))
			   ange-ftp-files-hashtable))
		      (setq ange-ftp-ls-cache-file file
			    ange-ftp-ls-cache-cmd lscmd
			    ange-ftp-ls-cache-res (buffer-string))
		      (kill-buffer (current-buffer))
		      ange-ftp-ls-cache-res)
		  (ange-ftp-error host user "Unable to get a remote ls"))
	      (condition-case () (ange-ftp-real-delete-file temp) (error nil))))))))

;;;; ------------------------------------------------------------
;;;; Directory information caching support.
;;;; ------------------------------------------------------------

(defun ange-ftp-parse-filename ()
  "Extract the filename from the current line of a dired-like listing."
  (save-excursion
    (let ((eol (progn (end-of-line) (point))))
      (beginning-of-line)
      (if (re-search-forward
	   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
	   eol t)
	  (progn (skip-chars-forward " ")
		 (skip-chars-forward "^ " eol)
		 (skip-chars-forward " " eol)
		 (let ((beg (point)))
		   (skip-chars-forward "^ \n")
	           (skip-chars-backward "*/@") ;keep Jamie happy.
		   (ange-ftp-real-file-name-nondirectory
		    (buffer-substring beg (point)))))))))

(defconst ange-ftp-ls-regexp
  "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+")

(defun ange-ftp-parse-dired-listing ()
  "Parse the current buffer which is assumed to be in a dired-like listing
format, and return a hashtable as the result."
  (let ((tbl (ange-ftp-make-hashtable)))
    (goto-char (point-min))
    (ange-ftp-save-match-data
      (let ((count 0))
	(if (re-search-forward "total [0-9]+" nil t)
	    (setq count 2))
	(if (re-search-forward ange-ftp-ls-regexp nil t)
	    (progn
	      (beginning-of-line)
	      (let (file)
		(while (setq file (ange-ftp-parse-filename))
		  (beginning-of-line)
		  (skip-chars-forward "\t 0-9")
		  (ange-ftp-put-hash-entry file (looking-at "d") tbl)
		  (forward-line 1)
		  (setq count (1+ count))))
	      (if (> count 1)		;must be a directory
		  (progn
		    (ange-ftp-put-hash-entry "." t tbl)
		    (ange-ftp-put-hash-entry ".." t tbl))))))
      tbl)))

(defun ange-ftp-set-files (directory files)
  "For a given DIRECTORY, set or change the associated FILES hashtable."
  (ange-ftp-put-hash-entry directory files ange-ftp-files-hashtable))

(defun ange-ftp-get-files (directory)
  "Given a given DIRECTORY, return a hashtable of file entries."
  (setq directory (file-name-as-directory directory)) ;normalize
  (or (ange-ftp-get-hash-entry directory ange-ftp-files-hashtable)
      (and (ange-ftp-ls directory "-al" t)
	   (ange-ftp-get-hash-entry directory ange-ftp-files-hashtable))))

(defmacro ange-ftp-get-file-part (path)
  "Given PATH, return the file part that can be used for looking up the file's
entry in a hashtable."
  (` (let ((file (file-name-nondirectory (, path))))
       (if (string-equal file "")
	   "."
	 file))))

(defmacro ange-ftp-allow-child-lookup (file)
  "Return whether ange-ftp-file-entry-p and ange-ftp-get-file-entry are
allowed to list a file directly if the file is called FILE.
This is actually a kludge for efficiently when using dired-x so that the
whole directory gets listed once only when looking for a .dired file."
  (` (not (and (boundp 'dired-local-variables-file)
	       (string-equal dired-local-variables-file (, file))))))

(defun ange-ftp-file-entry-p (path)
  "Given PATH, return whether there is a file entry for it."
  (let* ((path (directory-file-name path))
	 (dir (file-name-as-directory (file-name-directory path)))
	 (ent (ange-ftp-get-hash-entry dir ange-ftp-files-hashtable))
	 (file (ange-ftp-get-file-part path)))
    (if ent
	(ange-ftp-hash-entry-exists-p file ent)
      (or (if (ange-ftp-allow-child-lookup file)
	      (let ((ent (ange-ftp-get-files path)))
		(or (ange-ftp-get-hash-entry "." ent)
		    (ange-ftp-hash-entry-exists-p file ent))))
	  (ange-ftp-hash-entry-exists-p file
                (ange-ftp-get-files (file-name-directory path)))))))

(defun ange-ftp-get-file-entry (path)
  "Given PATH, return the given file entry.  At the moment this returns
whether PATH is a directory or not."
  (let* ((path (directory-file-name path))
	 (dir (file-name-as-directory (file-name-directory path)))
	 (ent (ange-ftp-get-hash-entry dir ange-ftp-files-hashtable))
	 (file (ange-ftp-get-file-part path)))
    (if ent
	(ange-ftp-get-hash-entry file ent)
      (if (ange-ftp-allow-child-lookup file)
	  (let ((ent (ange-ftp-get-files path)))
	    (cond ((ange-ftp-get-hash-entry "." ent))
		  ((ange-ftp-hash-entry-exists-p file ent)
		   nil)
		  ((ange-ftp-get-hash-entry file
		         (ange-ftp-get-files (file-name-directory path))))))
	(ange-ftp-get-hash-entry file
	      (ange-ftp-get-files (file-name-directory path)))))))

(defun ange-ftp-delete-file-entry (path)
  "Given a PATH, delete the file entry for it, if it exists."
  (let ((files (ange-ftp-get-hash-entry (file-name-directory path)
					ange-ftp-files-hashtable)))
    (if files
	(ange-ftp-del-hash-entry (ange-ftp-get-file-part path)
				 files)))
  (setq ange-ftp-ls-cache-file nil))

(defun ange-ftp-add-file-entry (path &optional dir-p)
  "Given a PATH, add the file entry for it, if its directory info exists."
  (let ((files (ange-ftp-get-hash-entry (file-name-directory path)
					ange-ftp-files-hashtable)))
    (if files
	(ange-ftp-put-hash-entry (ange-ftp-get-file-part path)
				 dir-p
				 files)))
  (setq ange-ftp-ls-cache-file nil))

(defun ange-ftp-wipe-file-entries (host user)
  "Remove all file entry information for the given HOST, USER pair."
  (ange-ftp-map-hashtable
   (function
    (lambda (key val)
      (let ((parsed (ange-ftp-ftp-path key)))
	(if parsed
	    (let ((h (nth 0 parsed))
		  (u (nth 1 parsed)))
	      (if (and (equal host h) (equal user u))
		  (ange-ftp-del-hash-entry key
					   ange-ftp-files-hashtable)))))))
   ange-ftp-files-hashtable))

;;;; ------------------------------------------------------------
;;;; File transfer mode support.
;;;; ------------------------------------------------------------

(defun ange-ftp-set-binary-mode (host user)
  "Tell the ftp process for the given HOST & USER to switch to binary mode."
  (ange-ftp-send-cmd host user '(type "binary")))

(defun ange-ftp-set-ascii-mode (host user)
  "Tell the ftp process for the given HOST & USER to switch to ascii mode."
  (ange-ftp-send-cmd host user '(type "ascii")))

;;;; ------------------------------------------------------------
;;;; Redefinitions of standard GNU Emacs functions.
;;;; ------------------------------------------------------------

(defun ange-ftp-canonize-filename (n)
  "Take a string and short-circuit //, /. and /.."
  (if (string-match ".+//" n)		;don't upset Apollo users
      (setq n (substring n (1- (match-end 0)))))
  (let ((path (nth 2 (ange-ftp-ftp-path n))))
    (if path				;this is an ange-ftp filename

	;; See if remote path is absolute.  If so then just expand it and
        ;; replace the path component of the overall path.
	(cond ((string-match "^/" path)
	       (ange-ftp-replace-path-component n
		     (ange-ftp-real-expand-file-name path)))

	      ;; Remote path starts with ~user.  Since I'm not going to resolve
              ;; this remotely, just expand the rest of the path and bolt the
              ;; ~user back on the front.  Finally replace the path component in
              ;; the overall path.
	      ((string-match "^~[^/]*/" path) ;~user
	       (let ((first (substring path 0 (1- (match-end 0))))
		     (rest  (ange-ftp-real-expand-file-name
			    (substring path (match-end 0)) "/\001/")))
		 (if (string-match "^/\001/?" rest)
		     (ange-ftp-replace-path-component n
		           (concat first (substring rest (match-end 0))))
		   n)))

	      ;; Path is relative.  Expand it, but if ..'s cause us to back up
	      ;; too far then leave it unexpanded.
	      (t
	       (condition-case nil
		   (setq path (ange-ftp-real-expand-file-name path "/\001/"))
		 (error nil))
	       (if (string-match "^/\001/?" path)
		   (ange-ftp-replace-path-component n
			     (substring path (match-end 0)))
		 n)))
      (ange-ftp-real-expand-file-name
            (ange-ftp-real-file-name-nondirectory n)
            (ange-ftp-real-file-name-directory n)))))

(defun ange-ftp-expand-file-name (name &optional default)
  "Documented as original."
  (let (lose)
    (ange-ftp-save-match-data
      (if (string-match "^/" name)
	  (progn
	    (while (cond ((string-match ".+//" name) ;don't upset Apollo users
			  (setq name (substring name (1- (match-end 0)))))
			 ((string-match "/~" name)
			  (setq name (substring name (1- (match-end 0)))
				lose t))))
	    (setq lose
		  (or lose
		      (string-match "/\\./\\|/\\.$\\|/\\.\\./\\|/\\.\\.$" name))))
	(setq lose t))
      (if lose
	  (cond ((string-match "^~" name)
		 (ange-ftp-real-expand-file-name name))
		((string-match "^/" name)
		 (ange-ftp-canonize-filename name))
		((zerop (length name))
		 (ange-ftp-canonize-filename (or default default-directory)))
		(t
		 (ange-ftp-canonize-filename
		  (concat (file-name-as-directory (or default default-directory))
			  name))))
	name))))

(defun ange-ftp-file-name-as-directory (name)
  "Documented as original."
  (let ((parsed (ange-ftp-ftp-path name)))
    (if (and parsed (string-equal (nth 2 parsed) ""))
	name
      (ange-ftp-real-file-name-as-directory name))))
	
(defun ange-ftp-file-name-directory (name)
  "Documented as original."
  (let ((parsed (ange-ftp-ftp-path name)))
    (if parsed
	(let ((path (nth 2 parsed)))
	  (if (ange-ftp-save-match-data
		(string-match "^~[^/]*$" path))
	      name
	    (ange-ftp-replace-path-component
	     name
	     (ange-ftp-real-file-name-directory path))))
      (ange-ftp-real-file-name-directory name))))

(defun ange-ftp-file-name-nondirectory (name)
  "Documented as original."
  (let ((parsed (ange-ftp-ftp-path name)))
    (if parsed
	(let ((path (nth 2 parsed)))
	  (if (ange-ftp-save-match-data
		(string-match "^~[^/]*$" path))
	      ""
	    (ange-ftp-real-file-name-nondirectory path)))
      (ange-ftp-real-file-name-nondirectory name))))

(defun ange-ftp-directory-file-name (dir)
  "Documented as original."
  (let ((parsed (ange-ftp-ftp-path dir)))
    (if parsed
	(ange-ftp-replace-path-component
	   dir
	   (ange-ftp-real-directory-file-name (nth 2 parsed)))
      (ange-ftp-real-directory-file-name dir))))

(defun ange-ftp-binary-file (file)
  "Returns whether the given FILE is to be considered as a binary file for
ftp transfers."
  (ange-ftp-save-match-data
    (string-match ange-ftp-binary-file-name-regexp file)))

(defun ange-ftp-write-region (start end filename &optional append visit)
  "Documented as original."
  (interactive "r\nFWrite region to file: ")
  (setq filename (expand-file-name filename))
  (let ((parsed (ange-ftp-ftp-path filename)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (path (ange-ftp-quote-string (nth 2 parsed)))
	       (temp (ange-ftp-make-tmp-name host))
	       (binary (ange-ftp-binary-file filename))
	       (cmd (if append 'append 'put))
	       (abbr (ange-ftp-abbreviate-filename filename)))
	  (unwind-protect
	      (progn
		(ange-ftp-real-write-region start end temp nil 'nomsg)
		(if binary
		    (ange-ftp-set-binary-mode host user))
		(or (ange-ftp-send-cmd host user
				       (list cmd temp path)
				       (format "Writing %s" abbr))
		    (signal 'file-error
			    (list
			     "Opening output file"
			     (format "FTP error: \"%s\"" ange-ftp-process-line)
			     filename))))
	    (condition-case () (ange-ftp-real-delete-file temp) (error nil))
	    (if binary 
		(ange-ftp-set-ascii-mode host user)))
	  (if (eq visit t)
	      (progn
		(ange-ftp-set-buffer-mode)
		(setq buffer-file-name filename)
		(set-buffer-modified-p nil)))
	  (ange-ftp-message "Wrote %s" abbr)
	  (ange-ftp-add-file-entry filename))
      (ange-ftp-real-write-region start end filename append visit))))

(defun ange-ftp-insert-file-contents (filename &optional visit)
  "Documented as original."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (let ((parsed (ange-ftp-ftp-path filename)))
    (if parsed
	(progn
	  (if visit
	      (setq buffer-file-name filename))
	  (if (ange-ftp-file-entry-p filename)
	      (let* ((host (nth 0 parsed))
		     (user (nth 1 parsed))
		     (path (ange-ftp-quote-string (nth 2 parsed)))
		     (temp (ange-ftp-make-tmp-name host))
		     (binary (ange-ftp-binary-file filename))
		     (abbr (ange-ftp-abbreviate-filename filename))
		     result)
		(unwind-protect
		    (progn
		      (if binary
			  (ange-ftp-set-binary-mode host user))
		      (or (ange-ftp-send-cmd host user
					     (list 'get path temp)
					     (format "Retrieving %s" abbr))
			  (signal 'file-error
				  (list
				   "Opening input file"
				   (format "FTP error: \"%s\"" ange-ftp-process-line)
				   filename)))
		      (setq result (ange-ftp-real-insert-file-contents temp
								       visit)))
		  (condition-case () (ange-ftp-real-delete-file temp) (error nil))
		  (if binary
		      (ange-ftp-set-ascii-mode host user)))
		(if visit
		    (setq buffer-file-name filename))
		result)
	    (signal 'file-error
		    (list 
		     "Opening input file"
		     filename))))
      (ange-ftp-real-insert-file-contents filename visit))))

(defun ange-ftp-revert-buffer (arg noconfirm)
  "Revert this buffer from a remote file using ftp."
  (let ((opoint (point)))
    (cond ((null buffer-file-name)
	   (error "Buffer does not seem to be associated with any file"))
	  ((or noconfirm
	       (yes-or-no-p (format "Revert buffer from file %s? "
				    buffer-file-name)))
	   (let ((buffer-read-only nil))
	     ;; Set buffer-file-name to nil
	     ;; so that we don't try to lock the file.
	     (let ((buffer-file-name nil))
	       (unlock-buffer)
	       (erase-buffer))
	     (insert-file-contents buffer-file-name t))
	   (goto-char (min opoint (point-max)))
	   (after-find-file nil)
	   t))))

(defun ange-ftp-file-exists-p (file)
  "Documented as original."
  (setq file (expand-file-name file))
  (if (ange-ftp-ftp-path file)
      (ange-ftp-file-entry-p file)
    (ange-ftp-real-file-exists-p file)))

(defun ange-ftp-file-directory-p (file)
  "Documented as original."
  (setq file (expand-file-name file))
  (if (ange-ftp-ftp-path file)
      (ange-ftp-get-file-entry file)
    (ange-ftp-real-file-directory-p file)))

(defun ange-ftp-directory-files (directory &optional full match)
  "Documented as original."
  (setq directory (expand-file-name directory))
  (if (ange-ftp-ftp-path directory)
      (let (files)
	(setq directory (file-name-as-directory directory))
	(ange-ftp-save-match-data
	  (mapcar (function
		   (lambda (f)
		     (if full
			 (setq f (concat directory f)))
		     (if match
			 (if (string-match match f)
			     (setq files (cons f files)))
		       (setq files (cons f files)))))
		  (ange-ftp-hash-table-keys (ange-ftp-get-files directory)))
	  (nreverse files)))
    (ange-ftp-real-directory-files directory full match)))

(defun ange-ftp-file-attributes (file)
  "Documented as original."
  (setq file (expand-file-name file))
  (let ((parsed (ange-ftp-ftp-path file)))
    (if parsed
	(if (ange-ftp-file-entry-p file)
	    (let ((host (nth 0 parsed))
		  (user (nth 1 parsed))
		  (path (nth 2 parsed))
		  (dirp (ange-ftp-get-file-entry file)))
	      (list dirp		;0 file type
		    -1			;1 link count
		    -1			;2 uid
		    -1			;3 gid
		    '(0 0)		;4 atime
		    '(0 0)		;5 mtime
		    '(0 0)		;6 ctime
		    -1			;7 size
		    (concat (if dirp "d" "-") ;8 mode
			    "?????????")
		    nil			;9 gid weird
		    ;; Hack to give remote files a unique "inode number".
		    ;; It's actually the sum of the characters in its name.
		    (apply '+ (nconc (mapcar 'identity host)
				     (mapcar 'identity user)
				     (mapcar 'identity path)))
		    -1			;11 device number [v19 only]
		    )))
      (ange-ftp-real-file-attributes file))))

(defun ange-ftp-file-writable-p (file)
  "Documented as original."
  (setq file (expand-file-name file))
  (or (ange-ftp-ftp-path file)
      (ange-ftp-real-file-writable-p file)))

(defun ange-ftp-file-readable-p (file)
  "Documented as original."
  (setq file (expand-file-name file))
  (if (ange-ftp-ftp-path file)
      (ange-ftp-file-entry-p file)
    (ange-ftp-real-file-readable-p file)))

(defun ange-ftp-delete-file (file)
  "Documented as original."
  (interactive "fDelete file: ")
  (setq file (expand-file-name file))
  (let ((parsed (ange-ftp-ftp-path file)))
    (if parsed
	(let ((host (nth 0 parsed))
	      (user (nth 1 parsed))
	      (path (ange-ftp-quote-string (nth 2 parsed)))
	      (abbr (ange-ftp-abbreviate-filename file)))
	  (or (ange-ftp-send-cmd host user (list 'delete path)
				 (format "Deleting %s" abbr))
	      (signal 'file-error
		      (list
		       "Removing old name"
		       (format "FTP error: \"%s\"" ange-ftp-process-line)
		       file)))
	  (ange-ftp-delete-file-entry file))
      (ange-ftp-real-delete-file file))))

(defun ange-ftp-verify-visited-file-modtime (buf)
  "Documented as original."
  (let ((name (buffer-file-name buf)))
    (if (and (stringp name) (ange-ftp-ftp-path name))
	t
      (ange-ftp-real-verify-visited-file-modtime buf))))

(defun ange-ftp-backup-buffer ()
  "Documented as original."
  (if (and (not ange-ftp-make-backup-files)
	   (stringp buffer-file-name)
	   (ange-ftp-ftp-path buffer-file-name))
      nil
    (ange-ftp-real-backup-buffer)))

;;;; ------------------------------------------------------------
;;;; File copying support.
;;;; ------------------------------------------------------------

(defun ange-ftp-barf-or-query-if-file-exists (absname querystring interactive)
  (if (file-exists-p absname)
      (if (not interactive)
	  (signal 'file-already-exists (list absname))
	(if (not (yes-or-no-p (format "File %s already exists; %s anyway? "
				      absname querystring)))
	    (signal 'file-already-exists (list absname))))))

(defun ange-ftp-copy-remote-to-local (remote local parsed binary &optional msg)
  "Copy REMOTE file to LOCAL file, where the former is on a remote machine."
  (let ((host (nth 0 parsed))
	(user (nth 1 parsed))
	(path (ange-ftp-quote-string (nth 2 parsed)))
	(rabbr (ange-ftp-abbreviate-filename remote))
	(labbr (ange-ftp-abbreviate-filename local remote))
	temp
	cmd)
    (if (not (ange-ftp-use-gateway-p host))
	(setq cmd (list 'get path local))
      ;; need temp file for gateway <-> host intermediate xfer
      (setq temp (ange-ftp-make-tmp-name host))
      (setq cmd (list 'get path temp)))
    (unwind-protect
	(progn
	  (if binary
	      (ange-ftp-set-binary-mode host user))
	  (or (ange-ftp-send-cmd host user cmd
		        (or msg (format "Copying %s to %s" rabbr labbr)))
	      (signal 'file-error
		      (list
		       "Opening output file"
		       (format "FTP error: \"%s\"" ange-ftp-process-line)
		       remote)))
	  (if temp (copy-file temp local t)))
      (if temp (condition-case () (ange-ftp-real-delete-file temp) (error nil)))
      (if binary
	  (ange-ftp-set-ascii-mode host user)))))

(defun ange-ftp-copy-local-to-remote (local remote parsed binary &optional msg)
  "Copy LOCAL file to REMOTE file where the latter is a file on a remote machine."
  (let ((host (nth 0 parsed))
	(user (nth 1 parsed))
	(path (ange-ftp-quote-string (nth 2 parsed)))
	(labbr (ange-ftp-abbreviate-filename local))
	(rabbr (ange-ftp-abbreviate-filename remote local))
	temp
	cmd)
    (if (not (ange-ftp-use-gateway-p host))
	(setq cmd (list 'put local path))
      ;; need temp file for intermediate gateway <-> local xfer.
      (setq temp (ange-ftp-make-tmp-name host))
      (setq cmd (list 'put temp path)))
    (unwind-protect
	(progn
	  (if binary
	      (ange-ftp-set-binary-mode host user))
	  (if temp (copy-file local temp t))
	  (or (ange-ftp-send-cmd host user cmd
			(or msg (format "Copying %s to %s" labbr rabbr)))
	      (signal 'file-error
		      (list
		       "Opening output file"
		       (format "FTP error: \"%s\"" ange-ftp-process-line)
		       remote))))
      (if temp (condition-case () (ange-ftp-real-delete-file temp) (error nil)))
      (if binary
	  (ange-ftp-set-ascii-mode host user)))
    (ange-ftp-add-file-entry remote)))

(defun ange-ftp-copy-remote-to-remote (f-file t-file f-parsed t-parsed binary)
  "Copy F-FILE to T-FILE, where both files are on remote machines.
The copy uses an intermediate file on the local machine."
  (let ((tmp (make-temp-name (concat ange-ftp-tmp-name-template "-cpy")))
	(msg1 (format "Getting %s" f-file))
	(msg2 (format "Putting %s" t-file)))
    (unwind-protect
	(progn
	  (ange-ftp-copy-remote-to-local f-file tmp f-parsed binary msg1)
	  (ange-ftp-copy-local-to-remote tmp t-file t-parsed binary msg2))
      (condition-case () (ange-ftp-real-delete-file tmp) (error nil)))))

(defun ange-ftp-copy-file (filename newname &optional ok-if-already-exists
				    keep-date)
  "Documented as original."
  (interactive "fCopy file: \nFCopy %s to file: \np")
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  (let* ((f-parsed (ange-ftp-ftp-path filename))
	 (t-parsed (ange-ftp-ftp-path newname))
	 (binary (if (or f-parsed t-parsed) (ange-ftp-binary-file filename))))
    (if (and (or f-parsed t-parsed)
	     (or (not ok-if-already-exists)
		 (numberp ok-if-already-exists)))
	(ange-ftp-barf-or-query-if-file-exists newname "copy to it"
					       (numberp ok-if-already-exists)))
    (if f-parsed
	(if t-parsed
	    (ange-ftp-copy-remote-to-remote filename newname
					    f-parsed t-parsed binary)
	  (ange-ftp-copy-remote-to-local filename newname f-parsed binary))
      (if t-parsed
	  (ange-ftp-copy-local-to-remote filename newname t-parsed binary)
	(ange-ftp-real-copy-file filename newname ok-if-already-exists keep-date)))))

;;;; ------------------------------------------------------------
;;;; File renaming support.
;;;; ------------------------------------------------------------

(defun ange-ftp-rename-remote-to-remote (filename newname f-parsed t-parsed
						  binary)
  "Rename remote file FILE to remote file NEWNAME."
  (let ((f-host (nth 0 f-parsed))
	(f-user (nth 1 f-parsed))
	(t-host (nth 0 t-parsed))
	(t-user (nth 1 t-parsed)))
    (if (and (string-equal f-host t-host)
	     (string-equal f-user t-user))
	(let* ((f-path (ange-ftp-quote-string (nth 2 f-parsed)))
	       (t-path (ange-ftp-quote-string (nth 2 t-parsed)))
	       (cmd (list 'rename f-path t-path))
	       (fabbr (ange-ftp-abbreviate-filename filename))
	       (nabbr (ange-ftp-abbreviate-filename newname filename)))
	  (or (ange-ftp-send-cmd f-host f-user cmd
		        (format "Renaming %s to %s" fabbr nabbr))
	      (signal 'file-error
		      (list
		       "Renaming"
		       (format "FTP error: \"%s\"" ange-ftp-process-line)
		       filename
		       newname)))
	  (ange-ftp-add-file-entry newname)
	  (ange-ftp-delete-file-entry filename))
      (ange-ftp-copy-remote-to-remote filename newname
				      f-parsed t-parsed binary)
      (delete-file filename))))

(defun ange-ftp-rename-local-to-remote (filename newname t-parsed binary)
  "Rename local FILE to remote file NEWNAME."
  (let* ((fabbr (ange-ftp-abbreviate-filename filename))
	 (nabbr (ange-ftp-abbreviate-filename newname filename))
	 (msg (format "Renaming %s to %s" fabbr nabbr)))
    (ange-ftp-copy-local-to-remote filename newname t-parsed binary msg)
    (let (ange-ftp-process-verbose)
      (delete-file filename))))

(defun ange-ftp-rename-remote-to-local (filename newname f-parsed binary)
  "Rename remote file FILE to local file NEWNAME." 
  (let* ((fabbr (ange-ftp-abbreviate-filename filename))
	 (nabbr (ange-ftp-abbreviate-filename newname filename))
	 (msg (format "Renaming %s to %s" fabbr nabbr)))
    (ange-ftp-copy-remote-to-local filename newname f-parsed binary msg)
    (let (ange-ftp-process-verbose)
      (delete-file filename))))

(defun ange-ftp-rename-file (filename newname &optional ok-if-already-exists)
  "Documented as original."
  (interactive "fRename file: \nFRename %s to file: \np")
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  (let* ((f-parsed (ange-ftp-ftp-path filename))
	 (t-parsed (ange-ftp-ftp-path newname))
	 (binary (if (or f-parsed t-parsed) (ange-ftp-binary-file filename))))
    (if (and (or f-parsed t-parsed)
	     (or (not ok-if-already-exists)
		 (numberp ok-if-already-exists)))
	(ange-ftp-barf-or-query-if-file-exists newname "rename to it"
					       (numberp ok-if-already-exists)))
    (if f-parsed
	(if t-parsed
	    (ange-ftp-rename-remote-to-remote filename newname f-parsed
					      t-parsed binary)
	  (ange-ftp-rename-remote-to-local filename newname f-parsed binary))
      (if t-parsed
	  (ange-ftp-rename-local-to-remote filename newname t-parsed binary)
	(ange-ftp-real-rename-file filename newname ok-if-already-exists)))))

;;;; ------------------------------------------------------------
;;;; Classic Dired support.
;;;; ------------------------------------------------------------

(require 'dired)

(defun ange-ftp-dired-readin (dirname buffer)
  "Documented as original."
  (let ((file (ange-ftp-abbreviate-filename dirname)))
    (save-excursion
      (ange-ftp-message "Reading directory %s..." file)
      (set-buffer buffer)
      (let ((buffer-read-only nil))
	(widen)
	(erase-buffer)
	(setq dirname (expand-file-name dirname))
	(if (ange-ftp-ftp-path dirname)
	    (insert (ange-ftp-ls dirname dired-listing-switches t))
	  (if (file-directory-p dirname)
	      (call-process "ls" nil buffer nil
			    dired-listing-switches dirname)
	    (let ((default-directory (file-name-directory dirname)))
	      (call-process shell-file-name nil buffer nil
			    "-c" (concat "ls " dired-listing-switches " "
					 (file-name-nondirectory dirname))))))
	(goto-char (point-min))
	(while (not (eobp))
	  (insert "  ")
	  (forward-line 1))
	(goto-char (point-min))))
    (ange-ftp-message "Reading directory %s...done" file)))

(defun ange-ftp-dired-revert (&optional arg noconfirm)
  "Documented as original."
  (if (and dired-directory
	   (ange-ftp-ftp-path (expand-file-name dired-directory)))
      (setq ange-ftp-ls-cache-file nil))
  (ange-ftp-real-dired-revert arg noconfirm))

;;;; ------------------------------------------------------------
;;;; Tree Dired support (ange & Sebastian Kremer)
;;;; ------------------------------------------------------------

(defun ange-ftp-dired-fixup-subdirs (start file)
  "Turn each subdir name into a valid ange-ftp filename."

  ;; We haven't indented the listing yet.
  ;; Must be careful about filelines ending in a colon: exclude spaces!
  (let ((subdir-regexp "^\\([^ \n\r]+\\)\\(:\\)[\n\r]"))
    (save-restriction
      (save-excursion
	(narrow-to-region start (point))
	(goto-char start)
	(while (re-search-forward subdir-regexp nil t)
	  (goto-char (match-beginning 1))
	  (let ((name (buffer-substring (point)
					(match-end 1))))
	    (delete-region (point) (match-end 1))
	    (insert (ange-ftp-replace-path-component
		     file
		     name))))))))

(defun ange-ftp-dired-ls (file switches &optional wildcard full-directory-p)
  "Documented as original."
  (let ((parsed (ange-ftp-ftp-path file)))
    (if parsed
	(let ((pt (point)))
	  (if wildcard
	      (progn
		;; Prevent ls from inserting subdirs, as the subdir header
		;; line format would be wrong (it would have no "/user@host:"
		;; prefix)
		(insert (ange-ftp-ls file (concat switches "d") nil))

		;; Quoting the path part of the file name seems to be a good
		;; idea (using dired.el's shell-quote function), but ftpd
		;; always globs ls args before passing them to /bin/ls or even
		;; doing the ls formatting itself.  --> So wildcard characters
		;; in FILE lose.  Sigh...

		;; When using wildcards, some ftpd's put the whole directory
		;; name in front of each filename.  Walk down the listing
		;; generated and remove this stuff.
		(let ((dir (file-name-directory (nth 2 parsed))))
		  (if dir
		      (let ((dirq (regexp-quote dir)))
			(save-restriction
			  (save-excursion
			    (narrow-to-region pt (point))
			    (goto-char pt)
			    (while (not (eobp))
			      (if (dired-move-to-filename)
				  (if (re-search-forward dirq nil t)
				      (replace-match "")))
			      (forward-line 1))))))))
	    (insert (ange-ftp-ls file switches full-directory-p))
	    (if (string-match "R" switches)
		;; fix up the subdirectory names in the recursive
		;; listing.
		(ange-ftp-dired-fixup-subdirs pt file))))
      (ange-ftp-real-dired-ls file switches wildcard))))

(defvar ange-ftp-remote-shell-file-name
  (if (memq system-type '(hpux usg-unix-v)) ; hope that's right
      "remsh"
    "rsh")
  "Remote shell used by ange-ftp.")

(defun ange-ftp-dired-run-shell-command (command &optional in-background)
  "Documented as original."
  (let* ((parsed (ange-ftp-ftp-path default-directory))
	 (host (nth 0 parsed))
	 (user (nth 1 parsed))
	 (path (nth 2 parsed)))
    (if (not parsed)
	(ange-ftp-real-dired-run-shell-command command in-background)
      (if (> (length path) 0)		; else it's $HOME
	  (setq command (concat "cd " path "; " command)))
      (setq command
	    (format  "%s %s \"%s\""	; remsh -l USER does not work well
					; on a hp-ux machine I tried
		     ange-ftp-remote-shell-file-name host command))
      (ange-ftp-message "Remote command '%s' ..." command)
      ;; Cannot call ange-ftp-real-dired-run-shell-command here as it
      ;; would prepend "cd default-directory" --- which bombs because
      ;; default-directory is in ange-ftp syntax for remote path names.
      (if in-background
	  (comint::background command)
	(shell-command command)))))

(defun ange-ftp-make-directory (dir)
  "Documented as original."
  (interactive (list (expand-file-name (read-file-name "Make directory: "))))
  (if (file-exists-p dir)
      (error "Cannot make directory %s: file already exists" dir)
    (let ((parsed (ange-ftp-ftp-path dir)))
      (if parsed
	  (let ((host (nth 0 parsed))
		(user (nth 1 parsed))
		(path (ange-ftp-quote-string (nth 2 parsed)))
		(abbr (ange-ftp-abbreviate-filename dir)))
	    (or (ange-ftp-send-cmd host user (list 'mkdir path)
				   (format "Making directory %s" abbr))
		(error "Could not make directory %s" dir))
	    (ange-ftp-add-file-entry dir t))
	(ange-ftp-real-make-directory dir)))))

(defun ange-ftp-remove-directory (dir)
  "Documented as original."
  (interactive 
   (list (expand-file-name (read-file-name "Remove directory: "
					   nil nil 'confirm)))) 
  (if (file-directory-p dir)
      (let ((parsed (ange-ftp-ftp-path dir)))
	(if parsed
	    (let ((host (nth 0 parsed))
		  (user (nth 1 parsed))
		  (path (ange-ftp-quote-string (nth 2 parsed)))
		  (abbr (ange-ftp-abbreviate-filename dir)))
	      (or (ange-ftp-send-cmd host user (list 'rmdir path)
				     (format "Removing directory %s" abbr))
		  (error "Could not remove directory %s" dir))
	      (ange-ftp-delete-file-entry dir))
	  (ange-ftp-real-remove-directory dir)))
    (error "Not a directory: %s" dir)))

(defun ange-ftp-diff (fn1 fn2 &optional switches)
  "Documented as original."
  (interactive (diff-read-args "Diff: " "Diff %s with: "
			       "Diff with switches: "))
  (or (and (stringp fn1)
	   (stringp fn2))
      (error  "diff: arguments must be strings: %s %s" fn1 fn2))
  (or switches
      (setq switches (if (stringp diff-switches)
			 diff-switches
		       (if (listp diff-switches)
			   (mapconcat 'identity diff-switches " ")
			 ""))))
  (let* ((fn1 (expand-file-name fn1))
	 (fn2 (expand-file-name fn2))
	 (pa1 (ange-ftp-ftp-path fn1))
	 (pa2 (ange-ftp-ftp-path fn2)))
    (if (or pa1 pa2)
	(let* ((tmp1 (and pa1 (make-temp-name
			       (concat ange-ftp-tmp-name-template "-d1"))))
	       (tmp2 (and pa2 (make-temp-name 
			       (concat ange-ftp-tmp-name-template "-d2"))))
	       (bin1 (and pa1 (ange-ftp-binary-file fn1)))
	       (bin2 (and pa2 (ange-ftp-binary-file fn2)))
	       (dir1 (file-directory-p fn1))
	       (dir2 (file-directory-p fn2))
	       (old-dir default-directory)
	       (default-directory "/tmp")) ;fool FTP-smart compile.el
	  (unwind-protect
	      (progn
		(if (and dir1 dir2)
		    (error "can't compare remote directories"))
		(if dir1
		    (setq fn1 (expand-file-name (file-name-nondirectory fn2)
						(file-name-directory fn1))
			  pa1 (ange-ftp-ftp-path fn1)
			  bin1 (ange-ftp-binary-file fn1)))
		(if dir2
		    (setq fn2 (expand-file-name (file-name-nondirectory fn1)
						(file-name-directory fn2))
			  pa2 (ange-ftp-ftp-path fn2)
			  bin2 (ange-ftp-binary-file fn2)))
		(and pa1 (ange-ftp-copy-remote-to-local fn1 tmp1 pa1 bin1
				   (format "Getting %s" fn1)))
		(and pa2 (ange-ftp-copy-remote-to-local fn2 tmp2 pa2 bin2
				   (format "Getting %s" fn2)))
		(and ange-ftp-process-verbose (ange-ftp-message "doing diff..."))
		(sit-for 0)
		(ange-ftp-real-diff (or tmp1 fn1) (or tmp2 fn2) switches)
		(cond ((boundp 'compilation-process)
		       (while (and compilation-process
				   (eq (process-status compilation-process)
				       'run))
			 (accept-process-output compilation-process)))
		      ((boundp 'compilation-last-buffer)
		       (while (and compilation-last-buffer
				   (buffer-name compilation-last-buffer)
				   (get-buffer-process compilation-last-buffer)
				   (eq (process-status
					(get-buffer-process
					 compilation-last-buffer))
				       'run))
			 (accept-process-output))))
		(and ange-ftp-process-verbose (ange-ftp-message "doing diff...done"))
		(save-excursion
		  (set-buffer (get-buffer-create "*compilation*"))

		  ;; replace the default directory that we munged earlier.
		  (goto-char (point-min))
		  (if (search-forward (concat "cd " default-directory) nil t)
		      (replace-match (concat "cd " old-dir)))
		  (setq default-directory old-dir)

		  ;; massage the diff output, replacing the temporary file-
		  ;; names with their original names.
		  (if tmp1
		      (let ((q1 (shell-quote tmp1)))
			(goto-char (point-min))
			(while (search-forward q1 nil t)
			  (replace-match fn1))))
		  (if tmp2
		      (let ((q2 (shell-quote tmp2)))
			(goto-char (point-min))
			(while (search-forward q2 nil t)
			  (replace-match fn2))))))
	    (and tmp1 (condition-case () (ange-ftp-real-delete-file tmp1) (error nil)))
	    (and tmp2 (condition-case () (ange-ftp-real-delete-file tmp2) (error nil)))))
      (ange-ftp-real-diff fn1 fn2 switches))))
	    
(defun ange-ftp-dired-call-process (program discard &rest arguments)
  "Documented as original."
  ;; PROGRAM is always one of those below in the cond in dired.el.
  ;; The ARGUMENTS are (nearly) always files.
  (let* ((parsed (ange-ftp-ftp-path default-directory))
	 (host (nth 0 parsed))
	 (user (nth 1 parsed))
	 (path (nth 2 parsed)))
    (if (not parsed)
	(apply 'call-process program nil (not discard) nil arguments)
      (condition-case oops
	  (cond ((equal "compress" program)
		 (ange-ftp-call-compress arguments))
		((equal "uncompress" program)
		 (ange-ftp-call-uncompress arguments))
		((equal "chmod" program)
		 (ange-ftp-call-chmod arguments))
		;; ((equal "chgrp" program))
		;; ((equal dired-chown-program program))
		(t (error "Unknown remote command: %s" program)))
	(file-error (insert (format "%s: %s, %s\n"
				    (nth 1 oops)
				    (nth 2 oops)
				    (nth 3 oops))))
	(error (insert (format "%s\n" (nth 1 oops))))))))

(defun ange-ftp-call-compress (args)
  "Perform a compress command on a remote file.
Works by taking a copy of the file, compressing it and copying the file
back."
  (if (or (not (= (length args) 2))
	  (not (string-equal "-f" (car args))))
      (error "ange-ftp-call-compress: missing -f flag and/or missing filename: %s"
	     args))
  (let* ((file (nth 1 args))
	 (tmp1 (make-temp-name (concat ange-ftp-tmp-name-template "-c1")))
	 (tmp2 (make-temp-name (concat ange-ftp-tmp-name-template "-c2")))
	 (parsed (ange-ftp-ftp-path file))
	 (binary (ange-ftp-binary-file file))
	 (abbr (ange-ftp-abbreviate-filename file))
	 (msg1 (format "Getting %s" abbr))
	 (msg2 (format "Putting %s" abbr)))
    (unwind-protect
	(progn
	  (ange-ftp-copy-remote-to-local file tmp1 parsed binary msg1)
	  (and ange-ftp-process-verbose
	       (ange-ftp-message "Compressing %s..." abbr))
	  (call-process-region (point)
			       (point)
			       shell-file-name
			       nil
			       t
			       nil
			       "-c"
			       (format "compress -f -c < %s > %s" tmp1 tmp2))
	  (and ange-ftp-process-verbose
	       (ange-ftp-message "Compressing %s...done" abbr))
	  (if (zerop (buffer-size))
	      (let* ((nfile (concat file ".Z"))
		     (nparsed (ange-ftp-ftp-path nfile)))
		(let (ange-ftp-process-verbose)
		  (delete-file file))
		(ange-ftp-copy-local-to-remote tmp2 nfile nparsed t msg2))))
      (condition-case () (ange-ftp-real-delete-file tmp1) (error nil))
      (condition-case () (ange-ftp-real-delete-file tmp2) (error nil)))))
    
(defun ange-ftp-call-uncompress (args)
  "Perform an uncompress command on a remote file.
Works by taking a copy of the file, uncompressing it and copying the file
back."
  (if (not (= (length args) 1))
      (error "ange-ftp-call-uncompress: missing filename: %s" args))
  (let* ((file (car args))
	 (tmp1 (make-temp-name (concat ange-ftp-tmp-name-template "-u1")))
	 (tmp2 (make-temp-name (concat ange-ftp-tmp-name-template "-u2")))
	 (parsed (ange-ftp-ftp-path file))
	 (abbr (ange-ftp-abbreviate-filename file))
	 (msg1 (format "Getting %s" abbr))
	 (msg2 (format "Putting %s" abbr)))
    (unwind-protect
	(progn
	  (ange-ftp-copy-remote-to-local file tmp1 parsed t msg1)
	  (and ange-ftp-process-verbose
	       (ange-ftp-message "Uncompressing %s..." abbr))
	  (call-process-region (point)
			       (point)
			       shell-file-name
			       nil
			       t
			       nil
			       "-c"
			       (format "uncompress -c < %s > %s" tmp1 tmp2))
	  (and ange-ftp-process-verbose
	       (ange-ftp-message "Uncompressing %s...done" abbr))
	  (if (zerop (buffer-size))
	      (let* ((nfile (if (string-match "\\.Z$" file)
				(substring file 0 (match-beginning 0))
			      file))
		     (nparsed (ange-ftp-ftp-path nfile)))
		(let (ange-ftp-process-verbose)
		  (delete-file file))
		(ange-ftp-copy-local-to-remote tmp2 nfile nparsed t msg2))))
      (condition-case () (ange-ftp-real-delete-file tmp1) (error nil))
      (condition-case () (ange-ftp-real-delete-file tmp2) (error nil)))))

(defun ange-ftp-call-chmod (args)
  (if (< (length args) 2)
      (error "ange-ftp-call-chmod: missing mode and/or filename: %s" args))
  (let ((mode (car args)))
    (mapcar
     (function
      (lambda (file)
	(setq file (expand-file-name file))
	(let ((parsed (ange-ftp-ftp-path file)))
	  (if parsed
	      (let ((host (nth 0 parsed))
		    (user (nth 1 parsed))
		    (path (ange-ftp-quote-string (nth 2 parsed)))
		    (abbr (ange-ftp-abbreviate-filename file)))
		(or (ange-ftp-send-cmd host user
				       (list 'chmod mode path)
				       (format "doing chmod %s" abbr))
		    (error "chmod: %s: FTP error: \"%s\"" file 
			   ange-ftp-process-line)))))))
     (cdr args)))
  (setq ange-ftp-ls-cache-file nil))	;stop confusing dired
  

;;;; ------------------------------------------------------------
;;;; File name completion support.
;;;; ------------------------------------------------------------

(defun ange-ftp-get-files-for-completion (dir)
  "Return a list of files in the given directory.  Each filename is wrapped
in a singleton list and has a trailing slash if it is a directory."
  (let (res)
    (ange-ftp-map-hashtable
     (function (lambda (key val)
		 (setq res (cons (list (if val
					   (concat key "/")
					 key))
				 res))))
     (ange-ftp-get-files dir))
    (or res '(()))))

(defun ange-ftp-file-name-all-completions (file dir)
  "Return a list of all completions of file name FILE in directory DIR."
  (if (ange-ftp-ftp-path dir)
      (all-completions file (ange-ftp-get-files-for-completion dir))
    (append (if (string-equal "/" dir)
		(all-completions file (ange-ftp-generate-root-prefixes)))
	    (ange-ftp-real-file-name-all-completions file dir))))

(defun ange-ftp-file-name-completion (file dir)
  "Complete file name FILE in directory DIR.
Returns the longest string common to all filenames in DIR that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIR contains no name starting with FILE."
  (if (ange-ftp-ftp-path dir)
      (try-completion file (ange-ftp-get-files-for-completion dir))
    (if (string-equal "/" dir)
	(try-completion
	 file
	 (append (ange-ftp-generate-root-prefixes)
		 (mapcar 'list
			 (ange-ftp-real-file-name-all-completions "" "/"))))
      (ange-ftp-real-file-name-completion file dir))))

(defun ange-ftp-quote-filename (file)
  "Quote $ as $$ to get it past substitute-in-file-name."
  (apply (function concat)
	 (mapcar (function
		   (lambda (char)
		     (if (= char ?$)
			 "$$"
			 (vector char))))
		 file)))

(defun ange-ftp-read-file-name-internal (string dir action)
  "Documented as original."
  (let (name realdir)
    (if (eq action 'lambda)
	(if (> (length string) 0)
	    (ange-ftp-file-exists-p (substitute-in-file-name string)))
      (if (zerop (length string))
	  (setq name string realdir dir)
	(setq string (substitute-in-file-name string)
	      name (file-name-nondirectory string)
	      realdir (file-name-directory string))
	(setq realdir (if realdir (expand-file-name realdir dir) dir)))
      (if action
	  (ange-ftp-file-name-all-completions name realdir)
	(let ((specdir (file-name-directory string))
	      (val (ange-ftp-file-name-completion name realdir)))
	  (if (and specdir (stringp val))
	      (ange-ftp-quote-filename (concat specdir val))
	    val))))))

;;;; ------------------------------------------------------------
;;;; Bits and bobs to bolt ange-ftp into GNU Emacs.
;;;; ------------------------------------------------------------

(defvar ange-ftp-overwrite-msg
  "Note: This function has been modifed to work with ange-ftp.")

(defun ange-ftp-safe-documentation (fun)
  "A documentation function that isn't quite as fragile."
  (condition-case ()
      (documentation fun)
    (error nil)))

(defun ange-ftp-overwrite-fn (fun)
  "Replace FUN's function definition with ange-ftp-FUN's, saving the
original definition as ange-ftp-real-FUN.  The original documentation is
placed on the new definition suitably augmented."
  (let* ((name (symbol-name fun))
	 (saved (intern (concat "ange-ftp-real-" name)))
	 (new (intern (concat "ange-ftp-" name)))
	 (exec-directory (if (or (equal (nth 3 command-line-args) "dump")
				 (equal (nth 4 command-line-args) "dump"))
			     "../etc/"
			   exec-directory)))			 
    (or (fboundp saved)
	(fset saved (symbol-function fun)))
	(let ((nfun (symbol-function new)))
	  (if (listp nfun)
	      (let* ((doc-str (ange-ftp-safe-documentation fun))
		     (ndoc-cdr (nthcdr 2 nfun))
		     (ndoc-str (concat doc-str
				       (and doc-str "\n")
				       ange-ftp-overwrite-msg)))
		(condition-case nil
		    (setcar ndoc-cdr ndoc-str)
		  (error
		   ;; the function must be preloaded, make a copy
		   (setq nfun (copy-sequence nfun)
			 ndoc-cdr (nthcdr 2 nfun))
		   (setcar ndoc-cdr ndoc-str)
		   (fset new nfun))))
	    ;; it's an emacs19 compile-code object
	    (let* ((doc-str (ange-ftp-safe-documentation fun))
		   (ndoc-str (concat doc-str
				     (and doc-str "\n")
				     ange-ftp-overwrite-msg))
		   (new-code (append nfun nil))) ; turn it into a list
	      (if (nthcdr 4 new-code)
		  (setcar (nthcdr 4 new-code) ndoc-str)
		(setcdr (nthcdr 3 new-code) (cons ndoc-str nil)))
	      (fset new (apply 'make-byte-code new-code))))
      (fset fun new))))

(if (not (fboundp 'dired-ls))		;dired should have been loaded by now
    (ange-ftp-overwrite-fn 'dired-readin) ; 18.57 dired
  (ange-ftp-overwrite-fn 'dired-run-shell-command) ;tree dired
  (ange-ftp-overwrite-fn 'dired-ls)
  (ange-ftp-overwrite-fn 'make-directory)
  (ange-ftp-overwrite-fn 'remove-directory)
  (ange-ftp-overwrite-fn 'diff)
  (ange-ftp-overwrite-fn 'dired-call-process))
(ange-ftp-overwrite-fn 'insert-file-contents)
(ange-ftp-overwrite-fn 'dired-revert)
(ange-ftp-overwrite-fn 'directory-files)
(ange-ftp-overwrite-fn 'file-directory-p)
(ange-ftp-overwrite-fn 'file-writable-p)
(ange-ftp-overwrite-fn 'file-readable-p)
(ange-ftp-overwrite-fn 'delete-file)
(ange-ftp-overwrite-fn 'read-file-name-internal)
(ange-ftp-overwrite-fn 'verify-visited-file-modtime)
(ange-ftp-overwrite-fn 'file-exists-p)
(ange-ftp-overwrite-fn 'write-region)
(ange-ftp-overwrite-fn 'backup-buffer)
(ange-ftp-overwrite-fn 'copy-file)
(ange-ftp-overwrite-fn 'rename-file)
(ange-ftp-overwrite-fn 'file-attributes)
(ange-ftp-overwrite-fn 'file-name-directory)
(ange-ftp-overwrite-fn 'file-name-nondirectory)
(ange-ftp-overwrite-fn 'file-name-as-directory)
(ange-ftp-overwrite-fn 'directory-file-name)
(ange-ftp-overwrite-fn 'expand-file-name)
(ange-ftp-overwrite-fn 'file-name-all-completions)
(ange-ftp-overwrite-fn 'file-name-completion)

(or (memq 'ange-ftp-set-buffer-mode find-file-hooks)
    (setq find-file-hooks
	  (cons 'ange-ftp-set-buffer-mode find-file-hooks)))

;;;; ------------------------------------------------------------
;;;; VOS support
;;;; ------------------------------------------------------------

(defun ange-ftp-fix-path-for-vos (path &optional reverse)
  (ange-ftp-save-match-data
    (save-excursion
      (let ((from (if reverse ">" "/"))
	    (to (if reverse "/" ">")))
	(set-buffer (get-buffer-create " *vos path*"))
	(erase-buffer)
	(buffer-flush-undo (current-buffer))
	(insert path)
	(goto-char (point-min))
	(while (search-forward from nil t)
	  (replace-match to))
	(buffer-string)))))

(or (assq 'vos ange-ftp-fix-path-func-alist)
    (setq ange-ftp-fix-path-func-alist
	  (cons '(vos . ange-ftp-fix-path-for-vos)
		ange-ftp-fix-path-func-alist)))

(or (memq 'vos ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'vos ange-ftp-dumb-host-types)))

(defun ange-ftp-fix-dir-path-for-vos (dir-path)
  (ange-ftp-fix-path-for-vos
   (concat dir-path
	   (if (eq ?/ (aref dir-path (1- (length dir-path))))
	       "" "/")
	   "*")))

(or (assq 'vos ange-ftp-fix-dir-path-func-alist)
    (setq ange-ftp-fix-dir-path-func-alist
	  (cons '(vos . ange-ftp-fix-dir-path-for-vos)
		ange-ftp-fix-dir-path-func-alist)))

(defvar ange-ftp-vos-host-regexp "agate")

(defun ange-ftp-vos-host (host)
  (and ange-ftp-vos-host-regexp
       (ange-ftp-save-match-data
	 (string-match ange-ftp-vos-host-regexp host))))

(defun ange-ftp-parse-vos-listing ()
  "Parse the current buffer which is assumed to be in VOS list -all
format, and return a hashtable as the result."
  (let ((tbl (ange-ftp-make-hashtable))
	(type-list
	 '(("^Files: [0-9]+ +Blocks: [0-9]+\n+" nil 40)
	   ("^Dirs: [0-9]+\n+" t 30)))
	type-regexp type-is-dir type-col file)
    (goto-char (point-min))
    (ange-ftp-save-match-data
      (while type-list
	(setq type-regexp (car (car type-list))
	      type-is-dir (nth 1 (car type-list))
	      type-col (nth 2 (car type-list))
	      type-list (cdr type-list))
	(if (re-search-forward type-regexp nil t)
	    (while (eq (char-after (point)) ? )
	      (move-to-column type-col)
	      (setq file (buffer-substring (point)
					   (progn
					     (end-of-line 1)
					     (point))))
	      (ange-ftp-put-hash-entry file type-is-dir tbl)
	      (forward-line 1))))
      (ange-ftp-put-hash-entry "." 'vosdir tbl)
      (ange-ftp-put-hash-entry ".." 'vosdir tbl))
    tbl))

(or (assq 'vos ange-ftp-parse-list-func-alist)
    (setq ange-ftp-parse-list-func-alist
	  (cons '(vos . ange-ftp-parse-vos-listing)
		ange-ftp-parse-list-func-alist)))

;;;; ------------------------------------------------------------
;;;; VMS support.
;;;; ------------------------------------------------------------

(defun ange-ftp-fix-path-for-vms (path &optional reverse)
  "Convert PATH from UNIX-ish to VMS.  If REVERSE given then convert from VMS
to UNIX-ish."
  (ange-ftp-save-match-data
    (if reverse
	(if (string-match "^\\([^:]+:\\)?\\(\\[.*\\]\\)?\\([^][]*\\)$" path)
	    (let (drive dir file)
	      (if (match-beginning 1)
		  (setq drive (substring path
					 (match-beginning 1)
					 (match-end 1))))
	      (if (match-beginning 2)
		  (setq dir (substring path (match-beginning 2) (match-end 2))))
	      (if (match-beginning 3)
		  (setq file (substring path (match-beginning 3) (match-end 3))))
	      (and dir
		   (setq dir (apply (function concat)
				    (mapcar (function
					     (lambda (char)
					       (if (= char ?.)
						   (vector ?/)
						 (vector char))))
					    (substring dir 1 -1)))))
	      (concat drive (and drive "/")
		      dir (and dir "/")
		      file))
	  (error "path %s didn't match" path))
      (let (drive dir file tmp)
	(if (string-match "^[^:]+:/" path)
	    (setq drive (substring path (match-beginning 0)
				   (1- (match-end 0)))
		  path (substring path (match-end 0))))
	(setq tmp (ange-ftp-real-file-name-directory path))
	(if tmp
	    (setq dir (apply (function concat)
			     (mapcar (function
				      (lambda (char)
					(if (= char ?/)
					    (vector ?.)
					  (vector char))))
				     (substring tmp 0 -1)))))
	(setq file (ange-ftp-real-file-name-nondirectory path))
	(concat drive
		(and dir (concat "[" (if drive nil ".") dir "]"))
		file)))))

;; (ange-ftp-fix-path-for-vms "PUB$:/ANONYMOUS/SDSCPUB/NEXT/Readme.txt;1")
;; (ange-ftp-fix-path-for-vms "PUB$:[ANONYMOUS.SDSCPUB.NEXT]Readme.txt;1" t)

(or (assq 'vms ange-ftp-fix-path-func-alist)
    (setq ange-ftp-fix-path-func-alist
	  (cons '(vms . ange-ftp-fix-path-for-vms)
		ange-ftp-fix-path-func-alist)))

(or (memq 'vms ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'vms ange-ftp-dumb-host-types)))

(defun ange-ftp-fix-dir-path-for-vms (dir-path)
  "Convert path from UNIX-ish to VMS ready for a DIRectory listing."
  (cond ((string-equal dir-path ".")
	 "[]")
	((ange-ftp-save-match-data
	   (string-match "\\.[^.]*/$" dir-path))
	 (ange-ftp-fix-path-for-vms
	  (ange-ftp-real-directory-file-name dir-path)))
	((ange-ftp-fix-path-for-vms dir-path))))

(or (assq 'vms ange-ftp-fix-dir-path-func-alist)
    (setq ange-ftp-fix-dir-path-func-alist
	  (cons '(vms . ange-ftp-fix-dir-path-for-vms)
		ange-ftp-fix-dir-path-func-alist)))

(defvar ange-ftp-vms-host-regexp nil)

(defun ange-ftp-vms-host (host)
  "Return whether HOST is running VMS."
  (and ange-ftp-vms-host-regexp
       (ange-ftp-save-match-data
	 (string-match ange-ftp-vms-host-regexp host))))

(defun ange-ftp-parse-vms-filename ()
  "Extract the filename from the current line of a dired-like listing."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward
	 "^\\([^. ]*\\)\\.\\([^;]*\\);\\([0-9]+\\)"
	 nil t)
	(buffer-substring (match-beginning 0) (match-end 0)))))

(defun ange-ftp-parse-vms-listing ()
  "Parse the current buffer which is assumed to be in MultiNet FTP dir
format, and return a hashtable as the result."
  (let ((tbl (ange-ftp-make-hashtable))
	(count 0)
	file)
    (goto-char (point-min))
    (ange-ftp-save-match-data
     (if (re-search-forward ":\\[.*\\] *$" nil t) ;directory name line
	 (progn
	   (forward-line 2)		;Skip over directory name and empty line
	   (while (setq file (ange-ftp-parse-vms-filename))
	     (if (string-match "\.DIR;\\([0-9]+\\)" file) ; deal with directories
		 (ange-ftp-put-hash-entry
		  (substring file 0 (match-beginning 0)) t tbl)
	       (ange-ftp-put-hash-entry file nil tbl)
	       (if (string-match ";[0-9]+$" file) ; deal with extension
		   ;; sans extension
		   (ange-ftp-put-hash-entry
		    (substring file 0 (match-beginning 0)) nil tbl)))
	     (setq count (1+ count))
	     (forward-line 1))
	   (if (> count 1)
	       (progn
		 (ange-ftp-put-hash-entry "." t tbl)
		 (ange-ftp-put-hash-entry ".." t tbl))))))
    tbl))

(or (assq 'vms ange-ftp-parse-list-func-alist)
    (setq ange-ftp-parse-list-func-alist
	  (cons '(vms . ange-ftp-parse-vms-listing)
		ange-ftp-parse-list-func-alist)))

;;;; ------------------------------------------------------------
;;;; Finally provide package.
;;;; ------------------------------------------------------------

(provide 'ange-ftp)
