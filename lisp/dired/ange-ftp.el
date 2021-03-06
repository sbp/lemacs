;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         ange-ftp.el
;; RCS:          Header: ange-ftp.el,v 4.20 92/08/14 17:04:34 ange Exp
;; Description:  transparent FTP support for GNU Emacs
;; Author:       Andy Norman, ange@hplb.hpl.hp.com
;; Created:      Thu Oct 12 14:00:05 1989
;; Modified:     Fri Aug 14 17:03:57 1992 (Ange) ange@anorman
;; Modified for Lucid Emacs by jwz
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1989, 1990, 1991, 1992  Andy Norman.
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

;;; "Dumb" UNIX hosts:
;;;
;;; The FTP servers on some UNIX machines have problems if the 'ls' command is
;;; used.
;;;
;;; The routine ange-ftp-add-dumb-unix-host can be called to tell ange-ftp to
;;; limit itself to the DIR command and not 'ls' for a given UNIX host.  Note
;;; that this change will take effect for the current GNU Emacs session only.
;;; See below for a discussion of non-UNIX hosts.  If a large number of
;;; machines with similar hostnames have this problem then it is easier to set
;;; the value of ange-ftp-dumb-unix-host-regexp in your .emacs file. ange-ftp
;;; is unable to automatically recognize dumb unix hosts.

;;; File name completion:
;;;
;;; Full file-name completion is supported on UNIX, VMS, CMS, and MTS hosts.
;;; To do filename completion, ange-ftp needs a listing from the remote host.
;;; Therefore, for very slow connections, it might not save any time.

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
;;; ange-ftp-{ascii,binary}-hash-mark-size, ange-ftp-send-hash and
;;; ange-ftp-process-verbose for more details.

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

;;; Tips for using ange-ftp:
;;;
;;; 1. For dired to work on a host which marks symlinks with a trailing @ in
;;;    an ls -alF listing, you need to (setq dired-ls-F-marks-symlinks t).
;;;    Most UNIX systems do not do this, but ULTRIX does. If you think that
;;;    there is a chance you might connect to an ULTRIX machine (such as
;;;    prep.ai.mit.edu), then set this variable accordingly.  This will have
;;;    the side effect that dired will have problems with symlinks whose names
;;;    end in an @. If you get youself into this situation then editing
;;;    dired's ls-switches to remove "F", will temporarily fix things.
;;;
;;; 2. If you know that you are connecting to a certain non-UNIX machine
;;;    frequently, and ange-ftp seems to be unable to guess its host-type,
;;;    then setting the appropriate host-type regexp
;;;    (ange-ftp-vms-host-regexp, ange-ftp-mts-host-regexp, or
;;;    ange-ftp-cms-host-regexp) accordingly should help. Also, please report
;;;    ange-ftp's inability to recognize the host-type as a bug.
;;;
;;; 3. For slow connections, you might get "listing unreadable" error
;;;    messages, or get an empty buffer for a file that you know has something
;;;    in it. The solution is to increase the value of ange-ftp-retry-time.
;;;    Its default value is 5 which is plenty for reasonable connections.
;;;    However, for some transatlantic connections I set this to 20.
;;;
;;; 4. Beware of compressing files on non-UNIX hosts. Ange-ftp will do it by
;;;    copying the file to the local machine, compressing it there, and then
;;;    sending it back. Binary file transfers between machines of different
;;;    architectures can be a risky business. Test things out first on some
;;;    test files. See "Bugs" below. Also, note that ange-ftp copies files by
;;;    moving them through the local machine. Again, be careful when doing
;;;    this with binary files on non-Unix machines.
;;;
;;; 5. Beware that dired over ftp will use your setting of dired-no-confirm
;;;    (list of dired commands for which confirmation is not asked).  You
;;;    might want to reconsider your setting of this variable, because you
;;;    might want confirmation for more commands on remote direds than on
;;;    local direds. For example, I strongly recommend that you not include
;;;    compress and uncompress in this list. If there is enough demand it
;;;    might be a good idea to have an alist ange-ftp-dired-no-confirm of
;;;    pairs ( TYPE . LIST ), where TYPE is an operating system type and LIST
;;;    is a list of commands for which confirmation would be suppressed.  Then
;;;    remote dired listings would take their (buffer-local) value of
;;;    dired-no-confirm from this alist. Who votes for this?

;;; ---------------------------------------------------------------------
;;; Non-UNIX support:
;;; ---------------------------------------------------------------------

;;; VMS support:
;;;
;;; Ange-ftp has full support for VMS hosts, including tree dired support.  It
;;; should be able to automatically recognize any VMS machine. However, if it
;;; fails to do this, you can use the command ange-ftp-add-vms-host.  As well,
;;; you can set the variable ange-ftp-vms-host-regexp in your .emacs file. We
;;; would be grateful if you would report any failures to automatically
;;; recognize a VMS host as a bug.
;;;
;;; Filename Syntax:
;;;
;;; For ease of *implementation*, the user enters the VMS filename syntax in a
;;; UNIX-y way.  For example:
;;;  PUB$:[ANONYMOUS.SDSCPUB.NEXT]README.TXT;1
;;; would be entered as:
;;;  /PUB$$:/ANONYMOUS/SDSCPUB/NEXT/README.TXT;1
;;; i.e. to log in as anonymous on ymir.claremont.edu and grab the file:
;;;  [.CSV.POLICY]RULES.MEM
;;; you would type:
;;;  C-x C-f /anonymous@ymir.claremont.edu:CSV/POLICY/RULES.MEM
;;;
;;; A legal VMS filename is of the form: FILE.TYPE;##
;;; where FILE can be up to 39 characters
;;;       TYPE can be up to 39 characters
;;;       ## is a version number (an integer between 1 and 32,767)
;;; Valid characters in FILE and TYPE are A-Z 0-9 _ - $
;;; $ cannot begin a filename, and - cannot be used as the first or last
;;; character.
;;;
;;; Tips:
;;; 1. Although VMS is not case sensitive, EMACS running under UNIX is.
;;;    Therefore, to access a VMS file, you must enter the filename with upper
;;;    case letters.
;;; 2. To access the latest version of file under VMS, you use the filename
;;;    without the ";" and version number. You should always edit the latest
;;;    version of a file. If you want to edit an earlier version, copy it to a
;;;    new file first. This has nothing to do with ange-ftp, but is simply
;;;    good VMS operating practice. Therefore, to edit FILE.TXT;3 (say 3 is
;;;    latest version), do C-x C-f /ymir.claremont.edu:FILE.TXT. If you
;;;    inadvertently do C-x C-f /ymir.claremont.edu:FILE.TXT;3, you will find
;;;    that VMS will not allow you to save the file because it will refuse to
;;;    overwrite FILE.TXT;3, but instead will want to create FILE.TXT;4, and
;;;    attach the buffer to this file. To get out of this situation, M-x
;;;    write-file /ymir.claremont.edu:FILE.TXT will attach the buffer to
;;;    latest version of the file. For this reason, in tree dired "f"
;;;    (dired-find-file), always loads the file sans version, whereas "v",
;;;    (dired-view-file), always loads the explicit version number. The
;;;    reasoning being that it reasonable to view old versions of a file, but
;;;    not to edit them.
;;; 3. EMACS has a feature in which it does environment variable substitution
;;;    in filenames. Therefore, to enter a $ in a filename, you must quote it
;;;    by typing $$. There is a bug in EMACS, in that it neglects to quote the
;;;    $'s in the default directory when it writes it in the minibuffer.  You
;;;    must edit the minibuffer to quote the $'s manually. Hopefully, this bug
;;;    will be fixed in EMACS 19. If you use Sebastian Kremer's gmhist (V 4.26
;;;    or newer), you will not have this problem.

;;; MTS support:
;;;
;;; Ange-ftp has full support, including tree dired support, for hosts running
;;; the Michigan terminal system.  It should be able to automatically
;;; recognize any MTS machine. However, if it fails to do this, you can use
;;; the command ange-ftp-add-mts-host.  As well, you can set the variable
;;; ange-ftp-mts-host-regexp in your .emacs file. We would be grateful if you
;;; would report any failures to automatically recognize a MTS host as a bug.
;;;
;;; Filename syntax:
;;; 
;;; MTS filenames are entered in a UNIX-y way. For example, if your account
;;; was YYYY, the file FILE in the account XXXX: on mtsg.ubc.ca would be
;;; entered as
;;;   /YYYY@mtsg.ubc.ca:/XXXX:/FILE
;;; In other words, MTS accounts are treated as UNIX directories. Of course,
;;; to access a file in another account, you must have access permission for
;;; it.  If FILE were in your own account, then you could enter it in a
;;; relative path fashion as
;;;   /YYYY@mtsg.ubc.ca:FILE
;;; MTS filenames can be up to 12 characters. Like UNIX, the structure of the
;;; filename does not contain a TYPE (i.e. it can have as many "."'s as you
;;; like.) MTS filenames are always in upper case, and hence be sure to enter
;;; them as such! MTS is not case sensitive, but an EMACS running under UNIX
;;; is.

;;; CMS support:
;;; 
;;; Ange-ftp has full support, including tree dired support, for hosts running
;;; CMS.  It should be able to automatically recognize any CMS machine.
;;; However, if it fails to do this, you can use the command
;;; ange-ftp-add-cms-host.  As well, you can set the variable
;;; ange-ftp-cms-host-regexp in your .emacs file. We would be grateful if you
;;; would report any failures to automatically recognize a CMS host as a bug.
;;; 
;;; Filename syntax:
;;;
;;; CMS filenames are entered in a UNIX-y way. In otherwords, minidisks are
;;; treated as UNIX directories. For example to access the file READ.ME in
;;; minidisk *.311 on cuvmb.cc.columbia.edu, you would enter
;;;   /anonymous@cuvmb.cc.columbia.edu:/*.311/READ.ME
;;; If *.301 is the default minidisk for this account, you could access
;;; FOO.BAR on this minidisk as
;;;   /anonymous@cuvmb.cc.columbia.edu:FOO.BAR
;;; CMS filenames are of the form FILE.TYPE, where both FILE and TYPE can be
;;; up to 8 characters. Again, beware that CMS filenames are always upper
;;; case, and hence must be entered as such.
;;;
;;; Tips:
;;; 1. CMS machines, with the exception of anonymous accounts, nearly always
;;;    need an account password. To have ange-ftp send an account password,
;;;    you can either include it in your .netrc file, or use
;;;    ange-ftp-set-account.
;;; 2. Ange-ftp cannot send "write passwords" for a minidisk. Hopefully, we
;;;    can fix this.
;;;
;;; ------------------------------------------------------------------
;;; Bugs:
;;; ------------------------------------------------------------------
;;; 
;;; 1. Umask problems:
;;;    Be warned that files created by using ange-ftp will take account of the
;;;    umask of the ftp daemon process rather than the umask of the creating
;;;    user.  This is particulary important when logging in as the root user.
;;;    The way that I tighten up the ftp daemon's umask under HP-UX is to make
;;;    sure that the umask is changed to 027 before I spawn /etc/inetd.  I
;;;    suspect that there is something similar on other systems.
;;;
;;; 2. Some combinations of FTP clients and servers break and get out of sync
;;;    when asked to list a non-existent directory.  Some of the ai.mit.edu
;;;    machines cause this problem for some FTP clients. Using
;;;    ange-ftp-kill-process can be used to restart the ftp process, which
;;;    should get things back in synch.
;;;
;;; 3. Ange-ftp does not check to make sure that when creating a new file,
;;;    you provide a valid filename for the remote operating system.
;;;    If you do not, then the remote FTP server will most likely
;;;    translate your filename in some way. This may cause ange-ftp to
;;;    get confused about what exactly is the name of the file. The
;;;    most common causes of this are using lower case filenames on systems
;;;    which support only upper case, and using filenames which are too
;;;    long.
;;;
;;; 4. Null (blank) passwords confuse both ange-ftp and some FTP daemons.
;;;
;;; 5. Ange-ftp likes to use pty's to talk to its FTP processes.  If GNU Emacs
;;;    for some reason creates a FTP process that only talks via pipes then
;;;    ange-ftp won't be getting the information it requires at the time that
;;;    it wants it since pipes flush at different times to pty's.  One
;;;    disgusting way around this problem is to talk to the FTP process via
;;;    rlogin which does the 'right' things with pty's.
;;;
;;; 6. For CMS support, we send too many cd's. Since cd's are cheap, I haven't
;;;    worried about this too much. Eventually, we should have some caching
;;;    of the current minidisk.
;;;    
;;; 7. Some CMS machines do not assign a default minidisk when you ftp them as
;;;    anonymous. It is then necessary to guess a valid minidisk name, and cd
;;;    to it. This is (understandably) beyond ange-ftp.
;;;
;;; 8. Remote to remote copying of files on non-Unix machines can be risky.
;;;    Depending on the variable ange-ftp-binary-file-name-regexp, ange-ftp
;;;    will use binary mode for the copy. Between systems of different
;;;    architecture, this still may not be enough to guarantee the integrity
;;;    of binary files. Binary file transfers from VMS machines are
;;;    particularly problematical. Should ange-ftp-binary-file-name-regexp be
;;;    an alist of OS type, regexp pairs?
;;;
;;; 9. The code to do compression of files over ftp is not as careful as it
;;;    should be. It deletes the old remote version of the file, before
;;;    actually checking if the local to remote transfer of the compressed
;;;    file succeeds. Of course to delete the original version of the file
;;;    after transferring the compressed version back is also dangerous,
;;;    because some OS's have severe restrictions on the length of filenames,
;;;    and when the compressed version is copied back the "-Z" or ".Z" may be
;;;    truncated. Then, ange-ftp would delete the only remaining version of
;;;    the file.  Maybe ange-ftp should make backups when it compresses files
;;;    (of course, the backup "~" could also be truncated off, sigh...).
;;;    Suggestions?
;;;

;;; 10. If a dir listing is attempted for an empty directory on (at least
;;;     some) VMS hosts, an ftp error is given. This is really an ftp bug, and
;;;     I don't know how to get ange-ftp work to around it.
;;;
;;; 11. Bombs on filenames that start with a space. Deals well with filenames
;;;     containing spaces, but beware that the remote ftpd may not like them
;;;     much.
;;;
;;; 12. No classic dired support for non-UNIX systems. Tree dired was enough.
;;; 
;;; 13. The famous @ bug. As mentioned above in TIPS, ULTRIX marks symlinks
;;;     with a trailing @ in a ls -alF listing. In order to account for this
;;;     ange-ftp looks to chop trailing @'s off of symlink names when it is
;;;     parsing a listing with the F switch. This will cause ange-ftp to
;;;     incorrectly get the name of a symlink on a non-ULTRIX host if its name
;;;     ends in an @. ange-ftp will correct itself if you take F out of the
;;;     dired ls switches (C-u s will allow you to edit the switches). The
;;;     dired buffer will be automatically reverted, which will allow ange-ftp
;;;     to fix its files hashtable.  A cookie to anyone who can think of a
;;;     fast, sure-fire way to recognize ULTRIX over ftp.

;;; If you find any bugs or problems with this package, PLEASE either e-mail
;;; the above author, or send a message to the ange-ftp-lovers mailing list
;;; below.  Ideas and constructive comments are especially welcome.

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
;;;
;;; Please don't forget the -request part.
;;;
;;; For mail to be posted directly to ange-ftp-lovers, send to one of the
;;; following addresses:
;;; 
;;;     ange-ftp-lovers@anorman.hpl.hp.com
;;; or
;;;     ange-ftp-lovers%anorman.hpl.hp.com@hplb.hpl.hp.com
;;;
;;; Alternatively, there is a mailing list that only gets announcements of new
;;; ange-ftp releases.  This is called ange-ftp-lovers-announce, and can be
;;; subscribed to by e-mailing to the -request address as above.  Please make
;;; it clear in the request which mailing list you wish to join.

;;; The latest version of ange-ftp can usually be obtained via anonymous ftp
;;; from:
;;;     alpha.gnu.ai.mit.edu:ange-ftp/ange-ftp.tar.Z
;;; or:
;;;     ugle.unit.no:/pub/gnu/emacs-lisp/ange-ftp.tar.Z
;;; or:
;;;   archive.cis.ohio-state.edu:pub/gnu/emacs/elisp-archive/packages/ange-ftp.tar.Z

;;; The archives for ange-ftp-lovers can be found via anonymous ftp under:
;;;
;;;     ftp.reed.edu:pub/mailing-lists/ange-ftp/

;;; -----------------------------------------------------------
;;; Technical information on this package:
;;; -----------------------------------------------------------

;;; The following GNU Emacs functions are replaced by this package:
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
;;;   file-symlink-p
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
;;; 
;;;    LCD Archive Entry:
;;;    ange-ftp|Andy Norman|ange@hplb.hpl.hp.com
;;;    |transparent FTP Support for GNU Emacs
;;;    |Date: 92/08/14 17:04:34 |Revision: 4.20 |

;;; Checklist for adding non-UNIX support for TYPE
;;; 
;;; The following functions may need TYPE versions:
;;; (not all functions will be needed for every OS)
;;;
;;; ange-ftp-fix-path-for-TYPE
;;; ange-ftp-fix-dir-path-for-TYPE
;;; ange-ftp-TYPE-host
;;; ange-ftp-TYPE-add-host
;;; ange-ftp-parse-TYPE-listing
;;; ange-ftp-TYPE-delete-file-entry
;;; ange-ftp-TYPE-add-file-entry
;;; ange-ftp-TYPE-file-name-as-directory
;;;
;;; Variables:
;;;
;;; ange-ftp-TYPE-host-regexp
;;; May need to add TYPE to ange-ftp-dumb-host-types
;;;
;;; Check the following functions for OS dependent coding:
;;;
;;; ange-ftp-host-type
;;; ange-ftp-guess-host-type
;;; ange-ftp-allow-child-lookup
;;;
;;; For Tree Dired support:
;;;
;;; ange-ftp-dired-TYPE-insert-headerline
;;; ange-ftp-dired-TYPE-move-to-filename
;;; ange-ftp-dired-TYPE-move-to-end-of-filename
;;; ange-ftp-dired-TYPE-get-filename
;;; ange-ftp-dired-TYPE-between-files
;;; ange-ftp-TYPE-make-compressed-filename
;;; ange-ftp-dired-TYPE-ls-trim
;;; ange-ftp-TYPE-bob-version
;;; ange-ftp-dired-TYPE-clean-directory
;;; ange-ftp-dired-TYPE-flag-backup-files
;;; ange-ftp-dired-TYPE-backup-diff
;;;
;;; Variables for dired:
;;;
;;; ange-ftp-dired-TYPE-re-exe
;;; ange-ftp-dired-TYPE-re-dir

;;; Host type conventions:
;;;
;;; The function ange-ftp-host-type and the variable ange-ftp-dired-host-type
;;; (mostly) follow the following conventions for remote host types.  At
;;; least, I think that future code should try to follow these conventions,
;;; and the current code should eventually be made compliant.
;;;
;;; nil = local host type, whatever that is (probably unix).
;;;       Think nil as in "not a remote host". This value is used by
;;;       ange-ftp-dired-host-type for local buffers.
;;;
;;; t = a remote host of unknown type. Think t is in true, it's remote.
;;;     Currently, 'unix is used as the default remote host type.
;;;     Maybe we should use t.
;;;
;;; 'type = a remote host of TYPE type.
;;;
;;; 'type:list = a remote host of TYPE type, using a specialized ftp listing
;;;              program called list. This is currently only used for Unix
;;;              dl (descriptive listings), when ange-ftp-dired-host-type
;;;              is set to 'unix:dl.

;;; Bug report codes:
;;;
;;; Because of their naive faith in this code, there are certain situations
;;; which the writers of this program believe could never happen. However,
;;; being realists they have put calls to 'error in the program at these
;;; points. These errors provide a code, which is an integer, greater than 1.
;;; To aid debugging.  the error codes, and the functions in which they reside
;;; are listed below.
;;; 
;;; 1: See ange-ftp-ls
;;;

;;; Lemacs changes from 4.20
;;;
;;; - added gzip support
;;; - added "lazy" messages
;;; - fixed completion list in the root dir (nil vs (nil))
;;; - use (message nil) to repaint minibuf instead of that awful kludge
;;; - call compute-buffer-file-truename to set truenames properly for
;;;   when find-file-compare-truenames is set
;;; - make-directory takes a second optional argument
;;; - made ange-ftp-overwrite-fn use the 19.8 interface to byte-code objects
;;; - made ange-ftp-shell-mode work better with the latest comint
;;; - insert-file-contents takes 2-5 args in v19
;;; - moved invocation of shell-mode to get along with the latest shell-font.el
;;; - implemented ange-ftp-read-passwd in terms of read-passwd (from passwd.el)
;;; - initialize all buffer-local variables to nil


;;; -----------------------------------------------------------
;;; Hall of fame:
;;; -----------------------------------------------------------
;;; 
;;; Thanks to Roland McGrath for improving the filename syntax handling,
;;; for suggesting many enhancements and for numerous cleanups to the code.
;;;
;;; Thanks to Jamie Zawinski for bugfixes and for ideas such as gateways.
;;;
;;; Thanks to Ken Laprade for improved .netrc parsing, password reading, and
;;; dired / shell auto-loading.
;;;
;;; Thanks to Sebastian Kremer for tree dired support and for many ideas and
;;; bugfixes.
;;;
;;; Thanks to Joe Wells for bugfixes, the original non-UNIX system support,
;;; VOS support, and hostname completion.
;;;
;;; Thanks to Nakagawa Takayuki for many good ideas, filename-completion, help
;;; with file-name expansion, efficiency worries, stylistic concerns and many
;;; bugfixes.
;;;
;;; Thanks to Sandy Rutherford who re-wrote most of ange-ftp to support VMS,
;;; MTS, CMS and UNIX-dls.  Sandy also added dired-support for non-UNIX OS and
;;; auto-recognition of the host type.
;;;
;;; Thanks to Dave Smith who wrote the info file for ange-ftp.
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

;; ange-ftp-multi-skip-msgs should only match ###-, where ### is one of
;; the number codes corresponding to ange-ftp-good-msgs or ange-ftp-fatal-msgs.
;; Otherwise, ange-ftp will go into multi-skip mode, and never come out.

(defvar ange-ftp-multi-msgs
  "^220-\\|^230-\\|^226\\|^25.-\\|^221-\\|^200-\\|^530-\\|^4[25]1-"
  "*Regular expression matching messages from the ftp process that start
a multiline reply.")

(defvar ange-ftp-good-msgs
  "^220 \\|^230 \\|^226 \\|^25. \\|^221 \\|^200 \\|^[Hh]ash mark"
  "*Regular expression matching messages from the ftp process that indicate
that the action that was initiated has completed successfully.")

;; CMS and the odd VMS machine say 200 Port rather than 200 PORT.
;; Also CMS machines use a multiline 550- reply to say that you
;; don't have write permission. ange-ftp gets into multi-line skip
;; mode and hangs. Have it ignore 550- instead. It will then barf
;; when it gets the 550 line, as it should.

(defvar ange-ftp-skip-msgs
  (concat "^200 \\(PORT\\|Port\\) \\|^331 \\|^150 \\|^350 \\|^[0-9]+ bytes \\|"
	  "^Connected \\|^$\\|^Remote system\\|^Using\\|^ \\|Password:\\|"
	  "^local:\\|^Trying\\|^125 \\|^550-\\|^221 .*oodbye")
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

(defvar ange-ftp-xfer-size-msgs
  "^150 .* connection for .* (\\([0-9]+\\) bytes)"
  "*Regular expression used to determine the number of bytes in a FTP transfer.")

(defvar ange-ftp-tmp-name-template "/tmp/ange-ftp"
  "*Template used to create temporary files.")

(defvar ange-ftp-gateway-tmp-name-template "/tmp/ange-ftp"
  "*Template used to create temporary files when ftp-ing through a gateway.
Files starting with this prefix need to be accessible from BOTH the local
machine and the gateway machine, and need to have the SAME name on both
machines, that is, /tmp is probably NOT what you want, since that is rarely
cross-mounted.")

(defvar ange-ftp-netrc-filename "~/.netrc"
  "*File in .netrc format to search for passwords.")

(defvar ange-ftp-disable-netrc-security-check nil
  "*If non-nil avoid checking permissions on the .netrc file.")

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

(defvar ange-ftp-dumb-unix-host-regexp nil
  "*If non-nil, if the host being ftp'd to matches this regexp then the FTP
process uses the \'dir\' command to get directory information.")

(defvar ange-ftp-binary-file-name-regexp
  (concat "\\.g?z$\\|\\.Z$\\|\\.lzh$\\|\\.arc$\\|\\.zip$\\|\\.zoo$\\|\\.tar$\\|"
	  "\\.dvi$\\|\\.ps$\\|\\.elc$\\|TAGS$\\|\\.gif$\\|"
	  "\\.EXE\\(;[0-9]+\\)?$\\|\\.g?z-part-..$\\|\\.Z-part-..$")
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

(defvar ange-ftp-send-hash t
  "*If non-nil, send the HASH command to the FTP client.")

(defvar ange-ftp-binary-hash-mark-size nil
  "*Default size, in bytes, between hash-marks when transferring a binary file.
If NIL, this variable will be locally overridden if the FTP client outputs a
suitable response to the HASH command.  If non-NIL then this value takes
precedence over the local value.")

(defvar ange-ftp-ascii-hash-mark-size 1024
  "*Default size, in bytes, between hash-marks when transferring an ASCII file.
This variable is buffer-local and will be locally overridden if the FTP client
outputs a suitable response to the HASH command.")

(defvar ange-ftp-process-verbose t
  "*If non-NIL then be chatty about interaction with the FTP process.")

(defvar ange-ftp-ftp-program-name "ftp"
  "*Name of FTP program to run.")

(defvar ange-ftp-gateway-ftp-program-name "ftp"
  "*Name of FTP program to run on gateway machine.
Some AT&T folks claim to use something called `pftp' here.")

(defvar ange-ftp-ftp-program-args '("-i" "-n" "-g" "-v")
  "*A list of arguments passed to the FTP program when started.")

(defvar ange-ftp-nslookup-program nil
  "*If non-NIL then a string naming nslookup program." )

(defvar ange-ftp-make-backup-files ()
  "*A list of operating systems for which ange-ftp will make Emacs backup
files files on the remote host. For example, '\(unix\) makes sense, but
'\(unix vms\) or '\(vms\) would be silly, since vms makes its own backups.")

(defvar ange-ftp-retry-time 5
  "*Number of seconds to wait before retrying if a file or listing
doesn't arrive. This might need to be increased for very slow connections.")

(defvar ange-ftp-auto-save 0
  "If 1, allows ange-ftp files to be auto-saved.
If 0, suppresses auto-saving of ange-ftp files.
Don't use any other value.")

;;;; ------------------------------------------------------------
;;;; Hash table support.
;;;; ------------------------------------------------------------

(require 'backquote)

(defun ange-ftp-make-hashtable (&optional size)
  "Make an obarray suitable for use as a hashtable.
SIZE, if supplied, should be a prime number."
  (make-vector (or size 31) 0))

(defun ange-ftp-map-hashtable (fun tbl)
  "Call FUNCTION on each key and value in HASHTABLE."
  (mapatoms
   (function 
    (lambda (sym)
      (funcall fun (get sym 'key) (get sym 'val))))
   tbl))

(defmacro ange-ftp-make-hash-key (key)
  "Convert KEY into a suitable key for a hashtable."
  (` (if (stringp (, key))
	 (, key)
       (prin1-to-string (, key)))))

(defun ange-ftp-get-hash-entry (key tbl)
  "Return the value associated with KEY in HASHTABLE."
  (let ((sym (intern-soft (ange-ftp-make-hash-key key) tbl)))
    (and sym (get sym 'val))))

(defun ange-ftp-put-hash-entry (key val tbl)
  "Record an association between KEY and VALUE in HASHTABLE."
  (let ((sym (intern (ange-ftp-make-hash-key key) tbl)))
    (put sym 'val val)
    (put sym 'key key)))

(defun ange-ftp-del-hash-entry (key tbl)
  "Copy all symbols except KEY in HASHTABLE and return modified hashtable."
  (let* ((len (length tbl))
	 (new-tbl (ange-ftp-make-hashtable len))
	 (i (1- len)))
    (ange-ftp-map-hashtable
     (function
      (lambda (k v)
	(or (equal k key)
	    (ange-ftp-put-hash-entry k v new-tbl))))
     tbl)
    (while (>= i 0)
      (aset tbl i (aref new-tbl i))
      (setq i (1- i)))
    tbl))

(defun ange-ftp-hash-entry-exists-p (key tbl)
  "Return whether there is an association for KEY in TABLE."
  (intern-soft (ange-ftp-make-hash-key key) tbl))

(defun ange-ftp-hash-table-keys (tbl)
  "Return a sorted list of all the active keys in the hashtable, as strings."
  (sort (all-completions "" tbl)
	(function string-lessp)))

;;;; ------------------------------------------------------------
;;;; Internal variables.
;;;; ------------------------------------------------------------

(defconst ange-ftp-version "Revision: 4.20.Lucid")

(defvar ange-ftp-data-buffer-name " *ftp data*"
  "Buffer name to hold directory listing data received from ftp process.")

(defvar ange-ftp-netrc-modtime nil
  "Last modified time of the netrc file from file-attributes.")

(defvar ange-ftp-user-hashtable (ange-ftp-make-hashtable)
  "Hash table holding associations between HOST, USER pairs.")

(defvar ange-ftp-passwd-hashtable (ange-ftp-make-hashtable)
  "Mapping between a HOST, USER pair and a PASSWORD for them.")

(defvar ange-ftp-account-hashtable (ange-ftp-make-hashtable)
  "Mapping between a HOST, USER pair and a ACCOUNT password for them.")

(defvar ange-ftp-files-hashtable (ange-ftp-make-hashtable 97)
  "Hash table for storing directories and their respective files.")

(defvar ange-ftp-ls-cache-lsargs nil
  "Last set of args used by ange-ftp-ls.")

(defvar ange-ftp-ls-cache-file nil
  "Last file passed to ange-ftp-ls.")

(defvar ange-ftp-ls-cache-res nil
  "Last result returned from ange-ftp-ls.")

;; New error symbols.
(put 'ftp-error 'error-conditions '(ftp-error file-error error))
;; (put 'ftp-error 'error-message "FTP error")

;;; ------------------------------------------------------------
;;; Match-data support (stolen from Kyle I think)
;;; ------------------------------------------------------------

(defmacro ange-ftp-save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data.
Before executing BODY, case-fold-search is locally bound to nil."
  (let ((original (make-symbol "match-data"))
	case-fold-search)
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

(defvar ange-ftp-lazy-message-time 0)
(defun ange-ftp-lazy-message (fmt &rest args)
  "Output the given message, but truncate to the size of the minibuffer
window, and don't print the message if we've printed another message
less than one second ago."
  (if (= ange-ftp-lazy-message-time
	 (setq ange-ftp-lazy-message-time (nth 1 (current-time))))
      nil
    (apply 'ange-ftp-message fmt args)))

(or (fboundp 'current-time) (fset 'ange-ftp-lazy-message 'ange-ftp-message))


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
  (ange-ftp-parse-netrc)
  (let ((user (ange-ftp-get-hash-entry host ange-ftp-user-hashtable)))
    (or user
	(prog1
	    (setq user
		  (cond ((stringp ange-ftp-default-user)
			 ;; We have a default name.  Use it.
			 ange-ftp-default-user)
			(ange-ftp-default-user
			 ;; Ask the user.
			 (let ((enable-recursive-minibuffers t))
			   (read-string (format "User for %s: " host)
					(user-login-name))))
			;; Default to the user's login name.
			(t
			 (user-login-name))))
	  (ange-ftp-set-user host user)))))

;;;; ------------------------------------------------------------
;;;; Password support.
;;;; ------------------------------------------------------------

(defun ange-ftp-read-passwd (prompt &optional default)
  "Read a password from the user.
See documentation of `read-passwd' for more info."
  (read-passwd prompt nil default))

;(defun ange-ftp-read-passwd (prompt &optional default)
;  "Read a password from the user. Echos a . for each character typed.
;End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
;Optional DEFAULT is password to start with."
;  (let ((pass (if default default ""))
;	(c 0)
;	(echo-keystrokes 0)
;	(cursor-in-echo-area t))
;    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
;      (message "%s%s"
;	       prompt
;	       (make-string (length pass) ?.))
;      (setq c (read-char))
;      (if (= c ?\C-u)
;	  (setq pass "")
;	(if (and (/= c ?\b) (/= c ?\177))
;	    (setq pass (concat pass (char-to-string c)))
;	  (if (> (length pass) 0)
;	      (setq pass (substring pass 0 -1))))))
;    (ange-ftp-repaint-minibuffer)
;    (substring pass 0 -1)))

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
  (ange-ftp-parse-netrc)
  (catch 'found-one
    (ange-ftp-map-hashtable
     (function (lambda (host val)
		 (if (ange-ftp-lookup-passwd host user)
		     (throw 'found-one host))))
     ange-ftp-user-hashtable)
    (ange-ftp-save-match-data
      (ange-ftp-map-hashtable
       (function
	(lambda (key value)
	  (if (string-match "^[^/]*\\(/\\).*$" key)
	      (let ((host (substring key 0 (match-beginning 1))))
		(if (and (string-equal user (substring key (match-end 1)))
			 value)
		    (throw 'found-one host))))))
       ange-ftp-passwd-hashtable))
    nil))

(defun ange-ftp-get-passwd (host user)
  "Given a HOST and USER, return the FTP password, prompting if it was not
previously set."
  (ange-ftp-parse-netrc)

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
  (ange-ftp-parse-netrc)
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
    (while (setq temp (ange-ftp-real-file-symlink-p file))
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
	(if (eq (following-char) ?\")	;quoted token value
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
  (interactive)
  (let* ((file (ange-ftp-chase-symlinks
		(ange-ftp-real-expand-file-name ange-ftp-netrc-filename)))
	 (attr (ange-ftp-real-file-attributes file)))
    (if (and attr			; file exists.
	     (not (equal (nth 5 attr) ange-ftp-netrc-modtime)))	; file changed
	(ange-ftp-save-match-data
	  (if (or ange-ftp-disable-netrc-security-check
		  (and (eq (nth 2 attr) (user-uid)) ; Same uids.
		       (string-match ".r..------" (nth 8 attr))))
	      (save-excursion
		;; we are cheating a bit here.  I'm trying to do the equivalent
		;; of find-file on the .netrc file, but then nuke it afterwards.
		;; with the bit of logic below we should be able to have
		;; encrypted .netrc files.
		(set-buffer (generate-new-buffer "*ftp-.netrc*"))
		(ange-ftp-real-insert-file-contents file)
		(setq buffer-file-name file)
		(setq default-directory (file-name-directory file))
		(normal-mode t)
		(mapcar 'funcall find-file-hooks)
		(setq buffer-file-name nil)
		(goto-char (point-min))
		(while (not (eobp))
		  (ange-ftp-parse-netrc-group))
		(kill-buffer (current-buffer)))
	    (ange-ftp-message "%s either not owned by you or badly protected."
			      ange-ftp-netrc-filename)
	    (sit-for 1))
	  (setq ange-ftp-netrc-modtime (nth 5 attr))))))

(defun ange-ftp-generate-root-prefixes ()
  "Return a list of prefixes of the form 'user@host:' to be used when
completion is done in the root directory."
  (ange-ftp-parse-netrc)
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
;;      (or res (list nil))
      res
      )))

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

(setq ange-ftp-tmp-keymap (make-sparse-keymap))
(define-key ange-ftp-tmp-keymap "\C-m" 'exit-minibuffer)

(defun ange-ftp-repaint-minibuffer ()
  "Gross hack to set minibuf_message = 0, so that the contents of the
minibuffer will show."
  (if (eq (selected-window) (minibuffer-window))
      (if (string-match "Lucid" emacs-version)
	  (message nil)
	;; v18 GNU Emacs
	(let ((unread-command-char ?\C-m)
	      (enable-recursive-minibuffers t))
	  (read-from-minibuffer "" nil ange-ftp-tmp-keymap nil)))))

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
  (signal 'ftp-error (list (format "FTP Error: %s" msg))))

(defun ange-ftp-set-buffer-mode ()
  "Set the correct modes for the current buffer if it is visiting a remote
file."
  (if (and (stringp buffer-file-name)
	   (ange-ftp-ftp-path buffer-file-name))
      (progn
	(auto-save-mode ange-ftp-auto-save)
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
			     (> char ?\~)
		             (= char ?\")
			     (= char ?\\))
			 (vector ?\\ char)
		       (vector char))))
		 string)))

(defun ange-ftp-barf-if-not-directory (directory)
  (or (file-directory-p directory)
      (signal 'file-error
	      (list "Opening directory"
		    (if (file-exists-p directory)
			"not a directory"
		      "no such file or directory")
		    directory))))

;;;; ------------------------------------------------------------
;;;; FTP process filter support.
;;;; ------------------------------------------------------------

(defun ange-ftp-process-handle-line (line proc)
  "Look at the given LINE from the ftp process PROC.  Try to catagorize it
into one of four categories: good, skip, fatal, or unknown."
  (cond ((string-match ange-ftp-xfer-size-msgs line)
	 (setq ange-ftp-xfer-size
	       (ash (string-to-int (substring line
					      (match-beginning 1)
					      (match-end 1)))
		    -10)))
	((string-match ange-ftp-skip-msgs line)
	 t)
	((string-match ange-ftp-good-msgs line)
	 (setq ange-ftp-process-busy nil
	       ange-ftp-process-result t
	       ange-ftp-process-result-line line))
	((string-match ange-ftp-fatal-msgs line)
	 (delete-process proc)
	 (setq ange-ftp-process-busy nil
	       ange-ftp-process-result-line line))
	((string-match ange-ftp-multi-msgs line)
	 (setq ange-ftp-process-multi-skip t))
	(ange-ftp-process-multi-skip
	 t)
	(t
	 (setq ange-ftp-process-busy nil
	       ange-ftp-process-result-line line))))

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

(defun ange-ftp-set-xfer-size (host user bytes)
  "Set the size of the next FTP transfer in bytes."
  (let ((proc (ange-ftp-get-process host user)))
    (if proc
	(let ((buf (process-buffer proc)))
	  (if buf
	      (save-excursion
		(set-buffer buf)
		(setq ange-ftp-xfer-size (ash bytes -10))))))))

(defun ange-ftp-process-handle-hash (str)
  "Remove hash marks from STRING and display count so far."
  (setq str (concat (substring str 0 (match-beginning 0))
		    (substring str (match-end 0)))
	ange-ftp-hash-mark-count (+ (- (match-end 0)
				       (match-beginning 0))
				    ange-ftp-hash-mark-count))
  (and ange-ftp-process-msg
       ange-ftp-process-verbose
       (not (eq (selected-window) (minibuffer-window)))
       (not (boundp 'search-message))	;screws up isearch otherwise
       (not cursor-in-echo-area)	;screws up y-or-n-p otherwise
       (let ((kbytes (ash (* ange-ftp-hash-mark-unit
			     ange-ftp-hash-mark-count)
			  -6)))
       (if (zerop ange-ftp-xfer-size)
	   (ange-ftp-lazy-message "%s...%dk" ange-ftp-process-msg kbytes)
	 (let ((percent (/ (* 100 kbytes) ange-ftp-xfer-size)))
	   ;; cut out the redisplay of identical %-age messages.
	   (if (not (eq percent ange-ftp-last-percent))
	       (progn
		 (setq ange-ftp-last-percent percent)
		 (ange-ftp-lazy-message "%s...%d%%"
					ange-ftp-process-msg percent)))))))
  str)

(defun ange-ftp-call-cont (cont result line)
  "Call the function specified by CONT.  CONT can be either a function or a
list of a function and some args.  The first two parameters passed to the
function will be RESULT and LINE.  The remaining args will be taken from CONT
if a list was passed."
  (if cont
      (if (and (listp cont)
	       (not (eq (car cont) 'lambda)))
	  (apply (car cont) result line (cdr cont))
	(funcall cont result line))))

(defun ange-ftp-process-filter (proc str)
  "Build up a complete line of output from the ftp PROCESS and pass it
on to ange-ftp-process-handle-line to deal with."
  (let ((buffer (process-buffer proc))
	(old-buffer (current-buffer)))

    ;; see if the buffer is still around... it could have been deleted.
    (if (buffer-name buffer)
	(unwind-protect
	    (ange-ftp-save-match-data
	      (set-buffer (process-buffer proc))
	      
	      ;; handle hash mark printing
	      (and ange-ftp-hash-mark-unit
		   ange-ftp-process-busy
		   (string-match "^#+$" str)
		   (setq str (ange-ftp-process-handle-hash str)))
	      (ange-ftp-process-log-string proc str)
	      (if ange-ftp-process-busy
		  (progn
		    (setq ange-ftp-process-string (concat ange-ftp-process-string
							  str))
		    
		    ;; if we gave an empty password to the USER command earlier
		    ;; then we should send a null password now.
		    (if (string-match "Password: *$" ange-ftp-process-string)
			(send-string proc "\n"))))
	      (while (and ange-ftp-process-busy
			  (string-match "\n" ange-ftp-process-string))
		(let ((line (substring ange-ftp-process-string
				       0
				       (match-beginning 0))))
		  (setq ange-ftp-process-string (substring ange-ftp-process-string
							   (match-end 0)))
		  (while (string-match "^ftp> *" line)
		    (setq line (substring line (match-end 0))))
		  (ange-ftp-process-handle-line line proc)))

	      ;; has the ftp client finished?  if so then do some clean-up
	      ;; actions.
	      (if (not ange-ftp-process-busy)
		  (progn
		    ;; reset the xfer size
		    (setq ange-ftp-xfer-size 0)

		    ;; issue the "done" message since we've finished.
		    (if (and ange-ftp-process-msg
			     ange-ftp-process-verbose
			     ange-ftp-process-result)
			(progn
			  (ange-ftp-message "%s...done" ange-ftp-process-msg)
			  (ange-ftp-repaint-minibuffer)
			  (setq ange-ftp-process-msg nil)))
		    
		    ;; is there a continuation we should be calling?  if so,
		    ;; we'd better call it, making sure we only call it once.
		    (if ange-ftp-process-continue
			(let ((cont ange-ftp-process-continue))
			  (setq ange-ftp-process-continue nil)
			  (ange-ftp-call-cont cont
					      ange-ftp-process-result
					      ange-ftp-process-result-line))))))
	  (set-buffer old-buffer)))))

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


;;; ------------------------------------------------------------
;;; Temporary file location and deletion...
;;; ------------------------------------------------------------

(defvar ange-ftp-tmp-name-files ())
(defvar ange-ftp-tmp-name-hashtable (ange-ftp-make-hashtable 10))
(defvar ange-ftp-pid nil)

(defun ange-ftp-get-pid ()
  "Half-hearted attempt to get the current process's id."
  (setq ange-ftp-pid (substring (make-temp-name "") 1)))

(defun ange-ftp-make-tmp-name (host)
  "This routine will return the name of a new file."
  (let* ((template (if (ange-ftp-use-gateway-p host)
		       ange-ftp-gateway-tmp-name-template
		     ange-ftp-tmp-name-template))
	 (pid (or ange-ftp-pid (ange-ftp-get-pid)))
	 (start ?a)
	 file entry)
    (while 
	(progn
	  (setq file (format "%s%c%s" template start pid))
	  (setq entry (intern file ange-ftp-tmp-name-hashtable))
	  (or (memq entry ange-ftp-tmp-name-files)
	      (ange-ftp-real-file-exists-p file)))
      (if (> (setq start (1+ start)) ?z)
	  (progn
	    (setq template (concat template "X"))
	    (setq start ?a))))
    (setq ange-ftp-tmp-name-files
	  (cons entry ange-ftp-tmp-name-files))
    file))

(defun ange-ftp-del-tmp-name (temp)
  (setq ange-ftp-tmp-name-files
	(delq (intern temp ange-ftp-tmp-name-hashtable)
	      ange-ftp-tmp-name-files))
  (condition-case ()
      (ange-ftp-real-delete-file temp)
    (error nil)))

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
	 (ftp (mapconcat (function identity) args " ")))
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

(defun ange-ftp-raw-send-cmd (proc cmd &optional msg cont nowait)
  "Low-level routine to send the given ftp CMD to the ftp PROCESS.
MSG is an optional message to output before and after the command.
If CONT is non-NIL then it is either a function or a list of function and
some arguments.  The function will be called when the ftp command has completed.
If CONT is NIL then this routine will return \( RESULT . LINE \) where RESULT
is whether the command was successful, and LINE is the line from the FTP
process that caused the command to complete.
If NOWAIT is given then the routine will return immediately the command has
been queued with no result.  CONT will still be called, however."
  (if (memq (process-status proc) '(run open))
      (save-excursion
	(set-buffer (process-buffer proc))
	(while ange-ftp-process-busy
	  (accept-process-output))
	(setq ange-ftp-process-string ""
	      ange-ftp-process-result-line ""
	      ange-ftp-process-busy t
	      ange-ftp-process-result nil
	      ange-ftp-process-multi-skip nil
	      ange-ftp-process-msg msg
	      ange-ftp-process-continue cont
	      ange-ftp-hash-mark-count 0
	      ange-ftp-last-percent -1
	      cmd (concat cmd "\n"))
	(and msg ange-ftp-process-verbose (ange-ftp-message "%s..." msg))
	(goto-char (point-max))
;	(move-marker last-input-start (point))
	;; don't insert the password into the buffer on the USER command.
	(ange-ftp-save-match-data
	  (if (string-match "^user \"[^\"]*\"" cmd)
	      (insert (substring cmd 0 (match-end 0)) " Turtle Power!\n")
	    (insert cmd)))
;	(move-marker last-input-end (point))
	(send-string proc cmd)
	(set-marker (process-mark proc) (point))
	(if nowait
	    nil
	  ;; hang around for command to complete
	  (while ange-ftp-process-busy
	    (accept-process-output proc))
	  (if cont
	      nil			;cont has already been called
	    (cons ange-ftp-process-result ange-ftp-process-result-line))))))

(defun ange-ftp-nslookup-host (host)
  "Attempt to resolve the given HOSTNAME using nslookup if possible."
  (interactive "sHost:  ")
  (if ange-ftp-nslookup-program
      (let ((proc (start-process " *nslookup*" " *nslookup*"
				 ange-ftp-nslookup-program host))
	    (res host))
	(process-kill-without-query proc)
	(save-excursion
	  (set-buffer (process-buffer proc))
	  (while (memq (process-status proc) '(run open))
	    (accept-process-output proc))
	  (goto-char (point-min))
	  (if (re-search-forward "Name:.*\nAddress: *\\(.*\\)$" nil t)
	      (setq res (buffer-substring (match-beginning 1)
					  (match-end 1))))
	  (kill-buffer (current-buffer)))
	res)
    host))

(defun ange-ftp-start-process (host user name)
  "Spawn a new ftp process ready to connect to machine HOST and give it NAME.
If HOST is only ftp-able through a gateway machine then spawn a shell
on the gateway machine to do the ftp instead."
  (let* ((use-gateway (ange-ftp-use-gateway-p host))
	 (ftp-prog (if use-gateway 
		       ange-ftp-gateway-ftp-program-name
		     ange-ftp-ftp-program-name))
	 (args (append (list ftp-prog) ange-ftp-ftp-program-args))
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
    ;; jwz: turn on shell mode after setting the proc filter for the
    ;; benefit of shell-font.
    (require 'shell)
    (save-excursion
      (set-buffer (process-buffer proc))
      (ange-ftp-shell-mode))
    (accept-process-output proc)	;wait for ftp startup message
    proc))

(defun ange-ftp-smart-login (host user pass account proc)
  "Connect to the FTP-server on HOST as USER using PASSWORD and ACCOUNT.
PROC is the FTP-client's process.  This routine uses the smart-gateway
host specified in ``ange-ftp-gateway-host''."
  (let ((result (ange-ftp-raw-send-cmd
		 proc
		 (format "open %s %s"
			 (ange-ftp-nslookup-host ange-ftp-gateway-host)
			 ange-ftp-smart-gateway-port)
		 (format "Opening FTP connection to %s via %s"
			 host
			 ange-ftp-gateway-host))))
    (or (car result)
	(ange-ftp-error host user 
			(concat "OPEN request failed: "
				(cdr result))))
    (setq result (ange-ftp-raw-send-cmd
		  proc (format "user \"%s\"@%s %s %s"
			       user
			       (ange-ftp-nslookup-host host)
			       pass
			       account)
		  (format "Logging in as user %s@%s"
			  user host)))
    (or (car result)
	(progn
	  (ange-ftp-set-passwd host user nil) ; reset password
	  (ange-ftp-set-account host user nil) ; reset account
	  (ange-ftp-error host user
			  (concat "USER request failed: "
				  (cdr result)))))))

(defun ange-ftp-normal-login (host user pass account proc)
  "Connect to the FTP-server on HOST as USER using PASSWORD and ACCOUNT.
PROC is the process to the FTP-client."
  (let ((result (ange-ftp-raw-send-cmd
		 proc
		 (format "open %s" (ange-ftp-nslookup-host host))
		 (format "Opening FTP connection to %s" host))))
    (or (car result)
	(ange-ftp-error host user
			(concat "OPEN request failed: "
				(cdr result))))
    (setq result (ange-ftp-raw-send-cmd
		  proc
		  (format "user \"%s\" %s %s" user pass account)
		  (format "Logging in as user %s@%s" user host)))
    (or (car result)
	(progn
	  (ange-ftp-set-passwd host user nil) ;reset password.
	  (ange-ftp-set-account host user nil) ;reset account.
	  (ange-ftp-error host user
			  (concat "USER request failed: "
				  (cdr result)))))))

(defvar ange-ftp-hash-mark-msgs
  "[hH]ash mark [^0-9]*\\([0-9]+\\)"
  "*Regexp matching the FTP client's output upon doing a HASH command.")

(defun ange-ftp-guess-hash-mark-size (proc)
  (if ange-ftp-send-hash
      (save-excursion
	(set-buffer (process-buffer proc))
	(let* ((status (ange-ftp-raw-send-cmd proc "hash"))
	       (result (car status))
	       (line (cdr status)))
	  (ange-ftp-save-match-data
	    (if (string-match ange-ftp-hash-mark-msgs line)
		(let ((size (string-to-int
			    (substring line
				       (match-beginning 1)
				       (match-end 1)))))
		  (setq ange-ftp-ascii-hash-mark-size size
			ange-ftp-hash-mark-unit (ash size -4))

		  ;; if a default value for this is set, use that value.
		  (or ange-ftp-binary-hash-mark-size
		      (setq ange-ftp-binary-hash-mark-size size)))))))))

(defun ange-ftp-get-process (host user)
  "Return the process object for a FTP process connected to HOST and
logged in as USER.  Create a new process if needed."
  (let* ((name (ange-ftp-ftp-process-buffer host user))
	 (proc (get-process name)))
    (if (and proc (memq (process-status proc) '(run open)))
	proc
      (let ((pass (ange-ftp-quote-string
		   (ange-ftp-get-passwd host user)))
	    (account (ange-ftp-quote-string
		      (ange-ftp-get-account host user))))
	;; grab a suitable process.
	(setq proc (ange-ftp-start-process host user name))
	
	;; login to FTP server.
	(if (ange-ftp-use-smart-gateway-p host)
	    (ange-ftp-smart-login host user pass account proc)
	  (ange-ftp-normal-login host user pass account proc))
      
	;; Tell client to send back hash-marks as progress.  It isn't usually
	;; fatal if this command fails.
	(ange-ftp-guess-hash-mark-size proc)

	;; Guess at the host type.
	(ange-ftp-guess-host-type host user)

	;; Run any user-specified hooks.  Note that proc, host and user are
	;; dynamically bound at this point.
	(run-hooks 'ange-ftp-process-startup-hook))
      proc)))

;; Variables for caching host and host-type
(defvar ange-ftp-host-cache nil)
(defvar ange-ftp-host-type-cache nil)

;; If ange-ftp-host-type is called with the optional user
;; argument, it will attempt to guess the host type by connecting
;; as user, if necessary. For efficiency, I have tried to give this
;; optional second argument only when necessary. Have I missed any calls
;; to ange-ftp-host-type where it should have been supplied?

(defun ange-ftp-host-type (host &optional user)
  "Return a symbol which represents the type of the HOST given.
If the optional argument USER is given, attempts to guess the
host-type by logging in as USER."
  (if (eq host ange-ftp-host-cache)
      ange-ftp-host-type-cache
    ;; Trigger an ftp connection, in case we need to guess at the host type.
    (if (and user (ange-ftp-get-process host user) (eq host ange-ftp-host-cache))
	ange-ftp-host-type-cache
      (setq ange-ftp-host-cache host
	    ange-ftp-host-type-cache
	    (cond ((ange-ftp-dumb-unix-host host)
		   'dumb-unix)
		  ((and (fboundp 'ange-ftp-vos-host)
			(ange-ftp-vos-host host))
		   'vos)
		  ((and (fboundp 'ange-ftp-vms-host)
			(ange-ftp-vms-host host))
		   'vms)
		  ((and (fboundp 'ange-ftp-mts-host)
			(ange-ftp-mts-host host))
		   'mts)
		  ((and (fboundp 'ange-ftp-cms-host)
			(ange-ftp-cms-host host))
		   'cms)
		  (t
		   'unix))))))

;; It would be nice to abstract the functions ange-ftp-TYPE-host and
;; ange-ftp-add-TYPE-host. The trick is to abstract these functions
;; without sacrificing speed. Also, having separate variables
;; ange-ftp-TYPE-regexp is more user friendly then requiring the user to
;; set an alist to indicate that a host is of a given type. Even with
;; automatic host type recognition, setting a regexp is still a good idea
;; (for efficiency) if you log into a particular non-UNIX host frequently.

(defvar ange-ftp-fix-path-func-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC is a routine
which can change a UNIX path into a path more suitable for a host of type
TYPE.")

(defvar ange-ftp-fix-dir-path-func-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC is a routine
which can change UNIX directory path into a directory path more suitable
for a host of type TYPE.")

;; *** Perhaps the sense of this variable should be inverted, since there
;; *** is only 1 host type that can take ls-style listing options.
(defvar ange-ftp-dumb-host-types '(dumb-unix)
  "List of host types that can't take UNIX ls-style listing options.")

(defun ange-ftp-send-cmd (host user cmd &optional msg cont nowait)
  "Find an ftp process connected to HOST logged in as USER and send it CMD.
MSG is an optional status message to be output before and after issuing the
command.
See the documentation for ange-ftp-raw-send-cmd for a description of CONT
and NOWAIT."
  ;; Handle conversion to remote pathname syntax and remote ls option
  ;; capability.
  (let ((cmd0 (car cmd))
	(cmd1 (nth 1 cmd))
	cmd2 cmd3 host-type fix-pathname-func)

    (cond
     
     ;; pwd case (We don't care what host-type.)
     ((null cmd1))
     
     ;; cmd == 'dir "remote-path" "local-path" "ls-switches"
     ((progn
       (setq cmd2 (nth 2 cmd)
	     host-type (ange-ftp-host-type host user))
       ;; This will trigger an FTP login, if one doesn't exist
       (eq cmd0 'dir))
      (setq cmd1 (funcall
		  (or (cdr (assq host-type ange-ftp-fix-dir-path-func-alist))
		      'identity)
		  cmd1)
	    cmd3 (nth 3 cmd))
      ;; Need to deal with the HP-UX ftp bug. This should also allow
      ;; us to resolve symlinks to directories on SysV machines. (Sebastian will
      ;; be happy.)
      (and (eq host-type 'unix)
	   (string-match "/$" cmd1)
	   (not (string-match "R" cmd3))
	   (setq cmd1 (concat cmd1 ".")))
      ;; If the remote ls can take switches, put them in
      (or (memq host-type ange-ftp-dumb-host-types)
	  (setq cmd0 'ls
		cmd1 (format "\"%s %s\"" cmd3 cmd1))))
     
     ;; First argument is the remote pathname
     ((progn
	(setq fix-pathname-func (or (cdr (assq host-type
					       ange-ftp-fix-path-func-alist))
				    'identity))
	(memq cmd0 '(get delete mkdir rmdir cd)))
      (setq cmd1 (funcall fix-pathname-func cmd1)))

     ;; Second argument is the remote pathname
     ((memq cmd0 '(append put chmod))
      (setq cmd2 (funcall fix-pathname-func cmd2)))

     ;; Both arguments are remote pathnames
     ((eq cmd0 'rename)
      (setq cmd1 (funcall fix-pathname-func cmd1)
	    cmd2 (funcall fix-pathname-func cmd2))))
	  
    ;; Turn the command into one long string 
    (setq cmd0 (symbol-name cmd0))
    (setq cmd (concat cmd0
		      (and cmd1 (concat " " cmd1))
		      (and cmd2 (concat " " cmd2))))

    ;; Actually send the resulting command.
    (let (afsc-result
	  afsc-line)
      (ange-ftp-raw-send-cmd
       (ange-ftp-get-process host user)
       cmd
       msg
       (list
	(function (lambda (result line host user
				  cmd msg cont nowait)
		    (or cont
			(setq afsc-result result
			      afsc-line line))
		    (if result
			(ange-ftp-call-cont cont result line)
		      (ange-ftp-raw-send-cmd
		       (ange-ftp-get-process host user)
		       cmd
		       msg
		       (list
			(function (lambda (result line cont)
				    (or cont
					(setq afsc-result result
					      afsc-line line))
				    (ange-ftp-call-cont cont result line)))
			cont)
		       nowait))))
	host user cmd msg cont nowait)
       nowait)

      (if nowait
	  nil
	(if cont
	    nil
	  (cons afsc-result afsc-line))))))

;; It might be nice to message users about the host type identified,
;; but there is so much other messaging going on, it would not be
;; seen. No point in slowing things down just so users can read
;; a host type message.

(defconst ange-ftp-cms-path-template
  (concat
   "^[-A-Z0-9$*][-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?"
   "[-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?\\.[0-9][0-9][0-9A-Z]$"))
(defconst ange-ftp-vms-path-template
  "^[-A-Z0-9_$]+:\\[[-A-Z0-9_$]+\\(\\.[-A-Z0-9_$]+\\)*\\]$")
(defconst ange-ftp-mts-path-template
  "^[A-Z0-9._][A-Z0-9._][A-Z0-9._][A-Z0-9._]:$")

(defun ange-ftp-guess-host-type (host user)
  "Guess at the the host type of HOST by doing a pwd, and examining
the directory syntax."
  (let ((host-type (ange-ftp-host-type host))
	(key (concat host "/" user "/~")))
    (if (eq host-type 'unix)
	;; Note that ange-ftp-host-type returns unix as the default value.
	(ange-ftp-save-match-data
	  (let* ((result (ange-ftp-get-pwd host user))
		 (dir (car result))
		 fix-path-func)
	    (cond ((null dir)
		   (message "Warning! Unable to get home directory")
		   (sit-for 1)
		   (if (string-match
			"^450 No current working directory defined$"
			(cdr result))
		       
		       ;; We'll assume that if pwd bombs with this
		       ;; error message, then it's CMS.
		       (progn
			 (ange-ftp-add-cms-host host)
			 (setq ange-ftp-host-cache host
			       ange-ftp-host-type-cache 'cms))))

		  ;; try for VMS
		  ((string-match ange-ftp-vms-path-template dir)
		   (ange-ftp-add-vms-host host)
		   ;; The add-host functions clear the host type cache.
		   ;; Therefore, need to set the cache afterwards.
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'vms))

		  ;; try for MTS
		  ((string-match ange-ftp-mts-path-template dir)
		   (ange-ftp-add-mts-host host)
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'mts))
		  
		  ;; try for CMS
		  ((string-match ange-ftp-cms-path-template dir)
		   (ange-ftp-add-cms-host host)
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'cms))

		  ;; assume UN*X
		  (t
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'unix)))

	    ;; Now that we have done a pwd, might as well put it in
	    ;; the expand-dir hashtable.
	    (setq fix-path-func (cdr (assq ange-ftp-host-type-cache
					   ange-ftp-fix-path-func-alist)))
	    (if fix-path-func
		(setq dir (funcall fix-path-func dir 'reverse)))
	    (ange-ftp-put-hash-entry key dir
				     ange-ftp-expand-dir-hashtable))))

    ;; In the special case of CMS make sure that know the
    ;; expansion of the home minidisk now, because we will
    ;; be doing a lot of cd's.
    (if (and (eq host-type 'cms)
	     (not (ange-ftp-hash-entry-exists-p
		   key ange-ftp-expand-dir-hashtable)))
	(let ((dir (car (ange-ftp-get-pwd host user))))
	  (if dir
	      (ange-ftp-put-hash-entry key (concat "/" dir)
				       ange-ftp-expand-dir-hashtable)
	    (message "Warning! Unable to get home directory")
	    (sit-for 1))))))


;;;; ------------------------------------------------------------
;;;; Simple FTP process shell support.
;;;; ------------------------------------------------------------

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
    (shell-mode)
    (if (null ange-ftp-shell-mode-map)
	(progn
	  (setq ange-ftp-shell-mode-map (make-sparse-keymap))
	  (set-keymap-parent ange-ftp-shell-mode-map shell-mode-map)
	  (set-keymap-name ange-ftp-shell-mode-map 'ange-ftp-shell-mode-map)))
    (use-local-map ange-ftp-shell-mode-map)
    (setq major-mode 'ange-ftp-shell-mode)
    (setq mode-name "ange-ftp")
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (set (make-local-variable 'ange-ftp-process-string) nil)
    (setq ange-ftp-process-string "")
    (set (make-local-variable 'ange-ftp-process-busy) nil)
    (set (make-local-variable 'ange-ftp-process-result) nil)
    (set (make-local-variable 'ange-ftp-process-msg) nil)
    (set (make-local-variable 'ange-ftp-process-multi-skip) nil)
    (set (make-local-variable 'ange-ftp-process-result-line) nil)
    (set (make-local-variable 'ange-ftp-process-continue) nil)
    (set (make-local-variable 'ange-ftp-hash-mark-count) nil)
    (set (make-local-variable 'ange-ftp-binary-hash-mark-size) nil)
    (set (make-local-variable 'ange-ftp-ascii-hash-mark-size) nil)
    (set (make-local-variable 'ange-ftp-hash-mark-unit) nil)
    (set (make-local-variable 'ange-ftp-xfer-size) nil)
    (set (make-local-variable 'ange-ftp-last-percent) nil)
    (setq ange-ftp-hash-mark-count 0)
    (setq ange-ftp-xfer-size 0)
    (setq ange-ftp-process-result-line "")
    (run-hooks 'ange-ftp-shell-mode-hook)))

;;;; ------------------------------------------------------------
;;;; Remote file and directory listing support.
;;;; ------------------------------------------------------------

(defun ange-ftp-dumb-unix-host (host)
  "Returns whether HOST's FTP server doesn't like \'ls\' or \'dir\' commands
to take switch arguments."
  (and ange-ftp-dumb-unix-host-regexp
       (ange-ftp-save-match-data
	 (string-match ange-ftp-dumb-unix-host-regexp host))))

(defun ange-ftp-add-dumb-unix-host (host)
  "Interactively adds a given HOST to ange-ftp-dumb-unix-host-regexp."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name)
				      (and (eq major-mode 'dired-mode)
					   dired-directory))))
			(and name (car (ange-ftp-ftp-path name)))))))
  (if (not (ange-ftp-dumb-unix-host host))
      (setq ange-ftp-dumb-unix-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-dumb-unix-host-regexp "\\|")
		    ange-ftp-dumb-unix-host-regexp)
	    ange-ftp-host-cache nil)))

(defvar ange-ftp-parse-list-func-alist nil
  "Association list of \( TYPE \. FUNC \) pairs.  The FUNC is a routine
which can parse the output from a DIR listing for a host of type TYPE.")

;; With no-error nil, this function returns:
;; an error if file is not an ange-ftp-path
;;                      (This should never happen.) 
;; an error if either the listing is unreadable or there is an ftp error.
;; the listing (a string), if everything works.
;; 
;; With no-error t, it returns:
;; an error if not an ange-ftp-path
;; error if listing is unreable (most likely caused by a slow connection)
;; nil if ftp error (this is because although asking to list a nonexistent
;;                   directory on a remote unix machine usually (except
;;                   maybe for dumb hosts) returns an ls error, but no
;;                   ftp error, if the same is done on a VMS machine,
;;                   an ftp error is returned. Need to trap the error
;;                   so we can go on and try to list the parent.)
;; the listing, if everything works.

(defun ange-ftp-ls (file lsargs parse &optional no-error)
  "Return the output of an `DIR' or `ls' command done over ftp.
FILE is the full name of the remote file, LSARGS is any args to pass to the
`ls' command, and PARSE specifies that the output should be parsed and stored
away in the internal cache."
  ;; If parse is t, we assume that file is a directory. i.e. we only parse
  ;; full directory listings.
  (setq file (ange-ftp-expand-file-name file))
  (let ((parsed (ange-ftp-ftp-path file)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (path (ange-ftp-quote-string (nth 2 parsed)))
	       (key (directory-file-name file))
	       (host-type (ange-ftp-host-type host user))
	       (dumb (memq host-type ange-ftp-dumb-host-types))
	       result
	       temp
	       lscmd parse-func)
	  (if (string-equal path "")
	      (setq path
		    (ange-ftp-real-file-name-as-directory
		          (ange-ftp-expand-dir host user "~"))))
	  (if (and ange-ftp-ls-cache-file
		   (string-equal key ange-ftp-ls-cache-file)
		   ;; Don't care about lsargs for dumb hosts.
		   (or dumb (string-equal lsargs ange-ftp-ls-cache-lsargs)))
	      ange-ftp-ls-cache-res
	    (setq temp (ange-ftp-make-tmp-name host))
	    (setq lscmd (list 'dir path temp lsargs))
	    (unwind-protect
		(if (car (setq result (ange-ftp-send-cmd
				       host
				       user
				       lscmd
				       (format "Listing %s"
					       (ange-ftp-abbreviate-filename
						file)))))
		    (save-excursion
		      (set-buffer (get-buffer-create
				   ange-ftp-data-buffer-name))
		      (erase-buffer)
		      (if (ange-ftp-real-file-readable-p temp)
			  (ange-ftp-real-insert-file-contents temp)
			(sleep-for ange-ftp-retry-time)
					;wait for file to possibly appear
			(if (ange-ftp-real-file-readable-p temp)
			    ;; Try again.
			    (ange-ftp-real-insert-file-contents temp)
			  (ange-ftp-error host user
					  (format
					   "list data file %s not readable"
					   temp))))
		      (if parse
			  (ange-ftp-set-files
			   file
			   (if (setq
				parse-func
				(cdr (assq host-type
					   ange-ftp-parse-list-func-alist)))
			       (funcall parse-func)
			     (ange-ftp-parse-dired-listing lsargs))))
		      (setq ange-ftp-ls-cache-file key
			    ange-ftp-ls-cache-lsargs lsargs
					; For dumb hosts-types this is
					; meaningless but harmless.
			    ange-ftp-ls-cache-res (buffer-string))
		      ;; (kill-buffer (current-buffer))
		      ange-ftp-ls-cache-res)
		  (if no-error
		      nil
		    (ange-ftp-error host user
				    (concat "DIR failed: " (cdr result)))))
	      (ange-ftp-del-tmp-name temp))))
      (error "Should never happen. Please report. Bug ref. no.: 1"))))

;;;; ------------------------------------------------------------
;;;; Directory information caching support.
;;;; ------------------------------------------------------------

(defconst ange-ftp-date-regexp
  (concat
   " \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct"
   "\\|Nov\\|Dec\\) +[0-3]?[0-9] "))

(defvar ange-ftp-add-file-entry-alist nil
  "Association list of pairs \( TYPE \. FUNC \), where FUNC
is a function to be used to add a file entry for the OS TYPE. The
main reason for this alist is to deal with file versions in VMS.")

(defvar ange-ftp-delete-file-entry-alist nil
  "Association list of pairs \( TYPE \. FUNC \), where FUNC
is a function to be used to delete a file entry for the OS TYPE.
The main reason for this alist is to deal with file versions in
VMS.")

(defun ange-ftp-add-file-entry (path &optional dir-p)
  "Given a PATH, add the file entry for it, if its directory
info exists."
  (funcall (or (cdr (assq (ange-ftp-host-type
			   (car (ange-ftp-ftp-path path)))
			  ange-ftp-add-file-entry-alist))
	       'ange-ftp-internal-add-file-entry)
	   path dir-p)
  (setq ange-ftp-ls-cache-file nil))

(defun ange-ftp-delete-file-entry (path &optional dir-p)
  "Given a PATH, delete the file entry for it, if its directory
info exists."
  (funcall (or (cdr (assq (ange-ftp-host-type
			   (car (ange-ftp-ftp-path path)))
			  ange-ftp-delete-file-entry-alist))
	       'ange-ftp-internal-delete-file-entry)
	   path dir-p)
  (setq ange-ftp-ls-cache-file nil))

(defmacro ange-ftp-parse-filename ()
  ;;Extract the filename from the current line of a dired-like listing.
  (` (let ((eol (progn (end-of-line) (point))))
       (beginning-of-line)
       (if (re-search-forward ange-ftp-date-regexp eol t)
	   (progn
	     (skip-chars-forward " ")
	     (skip-chars-forward "^ " eol)
	     (skip-chars-forward " " eol)
	     ;; We bomb on filenames starting with a space.
	     (buffer-substring (point) eol))))))
  
;; This deals with the F switch. Should also do something about
;; unquoting names obtained with the SysV b switch and the GNU Q
;; switch. See Sebastian's dired-get-filename.

(defmacro ange-ftp-ls-parser ()
  ;; Note that switches is dynamically bound.
  ;; Meant to be called by ange-ftp-parse-dired-listing
  (` (let ((tbl (ange-ftp-make-hashtable))
	   (used-F (and (stringp switches)
			(string-match "F" switches)))
	   file-type symlink directory file)
       (while (setq file (ange-ftp-parse-filename))
	 (beginning-of-line)
	 (skip-chars-forward "\t 0-9")
	 (setq file-type (following-char)
	       directory (eq file-type ?d))
	 (if (eq file-type ?l)
	     (if (string-match " -> " file)
		 (setq symlink (substring file (match-end 0))
		       file (substring file 0 (match-beginning 0)))
	       ;; Shouldn't happen
	       (setq symlink ""))
	   (setq symlink nil))
	 ;; Only do a costly regexp search if the F switch was used.
	 (if (and used-F
		  (not (string-equal file ""))
		  (looking-at
		   ".[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)"))
	     (let ((socket (eq file-type ?s))
		   (executable
		    (and (not symlink) ; x bits don't mean a thing for symlinks
			 (string-match "[xst]"
				       (concat
					(buffer-substring
					 (match-beginning 1)
					 (match-end 1))
					(buffer-substring
					 (match-beginning 2)
					 (match-end 2))
					(buffer-substring
					 (match-beginning 3)
					 (match-end 3)))))))
	       ;; Some ls's with the F switch mark symlinks with an @ (ULTRIX)
	       ;; and others don't. (sigh...) Beware, that some Unix's don't
	       ;; seem to believe in the F-switch
	       (if (or (and symlink (string-match "@$" file))
		       (and directory (string-match "/$" file))
		       (and executable (string-match "*$" file))
		       (and socket (string-match "=$" file)))
		   (setq file (substring file 0 -1)))))
	 (ange-ftp-put-hash-entry file (or symlink directory) tbl)
	 (forward-line 1))
       (ange-ftp-put-hash-entry "." t tbl)
       (ange-ftp-put-hash-entry ".." t tbl)
       tbl)))

;;; The dl stuff for descriptive listings

(defvar ange-ftp-dl-dir-regexp nil
  "Regexp matching directories which are listed in dl format. This regexp
shouldn't be anchored with a trailing $ so that it will match subdirectories
as well.")

(defun ange-ftp-add-dl-dir (dir)
  "Interactively adds a given directory to ange-ftp-dl-dir-regexp."
  (interactive
   (list (read-string "Directory: "
		      (let ((name (or (buffer-file-name)
				      (and (eq major-mode 'dired-mode)
					   dired-directory))))
			(and name (ange-ftp-ftp-path name)
			     (file-name-directory name))))))
  (if (not (and ange-ftp-dl-dir-regexp
		(string-match ange-ftp-dl-dir-regexp dir)))
      (setq ange-ftp-dl-dir-regexp
	    (concat "^" (regexp-quote dir)
		    (and ange-ftp-dl-dir-regexp "\\|")
		    ange-ftp-dl-dir-regexp))))

(defmacro ange-ftp-dl-parser ()
  ;; Parse the current buffer, which is assumed to be a descriptive
  ;; listing, and return a hashtable.
  (` (let ((tbl (ange-ftp-make-hashtable)))
       (while (not (eobp))
	 (ange-ftp-put-hash-entry
	  (buffer-substring (point)
			    (progn
			      (skip-chars-forward "^ /\n")
			      (point)))
	  (eq (following-char) ?/)
	  tbl)
	 (forward-line 1))
       (ange-ftp-put-hash-entry "." t tbl)
       (ange-ftp-put-hash-entry ".." t tbl)
       tbl)))

(defun ange-ftp-parse-dired-listing (&optional switches)
  "Parse the current buffer which is assumed to be in a dired-like listing
format, and return a hashtable as the result. If the listing is not really
a listing, then return nil."
  (ange-ftp-save-match-data
    (cond
     ((looking-at "^total [0-9]+$")
      (forward-line 1)
      (ange-ftp-ls-parser))
     ((looking-at "[^\n]+\\( not found\\|: Not a directory\\)\n\\'")
      ;; It's an ls error message.
      nil)
     ((eobp) ; i.e. (zerop (buffer-size))
      ;; This could be one of:
      ;; (1) An Ultrix ls error message
      ;; (2) A listing with the A switch of an empty directory
      ;;     on a machine which doesn't give a total line.
      ;; (3) The twilight zone.
      ;; We'll assume (1) for now.
      nil)
     ((re-search-forward ange-ftp-date-regexp nil t)
      (beginning-of-line)
      (ange-ftp-ls-parser))
     ((re-search-forward "^[^ \n\t]+ +\\([0-9]+\\|-\\|=\\) " nil t)
      ;; It's a dl listing (I hope).
      ;; file is bound by the call to ange-ftp-ls
      (ange-ftp-add-dl-dir file)
      (beginning-of-line)
      (ange-ftp-dl-parser))
     (t nil))))

(defun ange-ftp-set-files (directory files)
  "For a given DIRECTORY, set or change the associated FILES hashtable."
  (and files (ange-ftp-put-hash-entry (file-name-as-directory directory)
				      files ange-ftp-files-hashtable)))

(defun ange-ftp-get-files (directory &optional no-error)
  "Given a given DIRECTORY, return a hashtable of file entries.
This will give an error or return nil, depending on the value of
NO-ERROR, if a listing for DIRECTORY cannot be obtained."
  (setq directory (file-name-as-directory directory)) ;normalize
  (or (ange-ftp-get-hash-entry directory ange-ftp-files-hashtable)
      (ange-ftp-save-match-data
	(and (ange-ftp-ls directory
			  ;; This is an efficiency hack. We try to
			  ;; anticipate what sort of listing dired
			  ;; might want, and cache just such a listing.
			  (if (and (boundp 'dired-actual-switches)
				   (stringp dired-actual-switches)
				   ;; We allow the A switch, which lists
				   ;; all files except "." and "..".
				   ;; This is OK because we manually
				   ;; insert these entries
				   ;; in the hash table.
				   (string-match
				    "[aA]" dired-actual-switches)
				   (string-match
				    "l" dired-actual-switches)
				   (not (string-match
					 "R" dired-actual-switches)))
			      dired-actual-switches
			    (if (and (boundp 'dired-listing-switches)
				     (stringp dired-listing-switches)
				     (string-match
				      "[aA]" dired-listing-switches)
				     (string-match
				      "l" dired-listing-switches)
				     (not (string-match
					   "R" dired-listing-switches)))
				dired-listing-switches
			      "-al"))
			  t no-error)
	     (ange-ftp-get-hash-entry
	      directory ange-ftp-files-hashtable)))))

(defmacro ange-ftp-get-file-part (path)
  "Given PATH, return the file part that can be used for looking up the
file's entry in a hashtable."
  (` (let ((file (file-name-nondirectory (, path))))
       (if (string-equal file "")
	   "."
	 file))))

(defmacro ange-ftp-allow-child-lookup (dir file)
  "Return whether ange-ftp-file-entry-p and ange-ftp-get-file-entry are
allowed to determine if PATH is a sub-directory by listing it directly,
rather than listing its parent directory. This is used for efficiency so
that a wasted listing is not done:
1. When looking for a .dired file in dired-x.el.
2. The syntax of FILE and DIR make it impossible that FILE could be a valid
    subdirectory. This is of course an OS dependent judgement."
  (` (not
      (let* ((efile (, file)) ; expand once.
	     (edir (, dir))
	     (parsed (ange-ftp-ftp-path edir))
	     (host-type (ange-ftp-host-type
			 (car parsed))))
	(or
	 ;; Deal with dired
	 (and (boundp 'dired-local-variables-file)
	      (stringp dired-local-variables-file)
	      (string-equal dired-local-variables-file efile))
	 ;; No dots in dir names in vms.
	 (and (eq host-type 'vms)
	      (string-match "\\." efile))
	 ;; No subdirs in mts of cms.
	 (and (memq host-type '(mts cms))
	      (not (string-equal "/" (nth 2 parsed)))))))))

(defun ange-ftp-file-entry-p (path)
  "Given PATH, return whether there is a file entry for it."
  (let* ((path (directory-file-name path))
	 (dir (file-name-directory path))
	 (ent (ange-ftp-get-hash-entry dir ange-ftp-files-hashtable))
	 (file (ange-ftp-get-file-part path)))
    (if ent
	(ange-ftp-hash-entry-exists-p file ent)
      (or (and (ange-ftp-allow-child-lookup dir file)
	       (setq ent (ange-ftp-get-files path t))
	       ;; Try a child lookup. i.e. try to list file as a
	       ;; subdirectory of dir. This is a good idea because
	       ;; we may not have read permission for file's parent. Also,
	       ;; people tend to work down directory trees anyway. We use
	       ;; no-error ;; because if file does not exist as a subdir.,
	       ;; then dumb hosts will give an ftp error. Smart unix hosts
	       ;; will simply send back the ls
	       ;; error message.
	       (ange-ftp-get-hash-entry "." ent))
	  ;; Child lookup failed. Try the parent. If this bombs,
	  ;; we are at wits end -- signal an error.
	  ;; Problem: If this signals an error, the error message
	  ;; may  not have a lot to do with what went wrong.
	  (ange-ftp-hash-entry-exists-p file
					(ange-ftp-get-files dir))))))

(defun ange-ftp-get-file-entry (path)
  "Given PATH, return the given file entry which will be either t for a
directory, nil for a normal file, or a string for a symlink. If the file
isn't in the hashtable, this also returns nil."
  (let* ((path (directory-file-name path))
	 (dir (file-name-directory path))
	 (ent (ange-ftp-get-hash-entry dir ange-ftp-files-hashtable))
	 (file (ange-ftp-get-file-part path)))
    (if ent
	(ange-ftp-get-hash-entry file ent)
      (or (and (ange-ftp-allow-child-lookup dir file)
	       (setq ent (ange-ftp-get-files path t))
	       (ange-ftp-get-hash-entry "." ent))
	       ;; i.e. it's a directory by child lookup
	  (ange-ftp-get-hash-entry file
				   (ange-ftp-get-files dir))))))

(defun ange-ftp-internal-delete-file-entry (path &optional dir-p)
  (if dir-p
      (progn 
	(setq path (file-name-as-directory path))
	(ange-ftp-del-hash-entry path ange-ftp-files-hashtable)
	(setq path (directory-file-name path))))
  ;; Note that file-name-as-directory followed by directory-file-name
  ;; serves to canonicalize directory file names to their unix form.
  ;; i.e. in VMS, FOO.DIR -> FOO/ -> FOO
  (let ((files (ange-ftp-get-hash-entry (file-name-directory path)
					ange-ftp-files-hashtable)))
    (if files
	(ange-ftp-del-hash-entry (ange-ftp-get-file-part path)
				 files))))

(defun ange-ftp-internal-add-file-entry (path &optional dir-p)
  (and dir-p
       (setq path (directory-file-name path)))
  (let ((files (ange-ftp-get-hash-entry (file-name-directory path)
					ange-ftp-files-hashtable)))
    (if files
	(ange-ftp-put-hash-entry (ange-ftp-get-file-part path)
				 dir-p
				 files))))

(defun ange-ftp-wipe-file-entries (host user)
  "Replace the file entry information hashtable with one that doesn't have any
entries for the given HOST, USER pair."
  (let ((new-tbl (ange-ftp-make-hashtable (length ange-ftp-files-hashtable))))
    (ange-ftp-map-hashtable
     (function
      (lambda (key val)
	(let ((parsed (ange-ftp-ftp-path key)))
	  (if parsed
	      (let ((h (nth 0 parsed))
		    (u (nth 1 parsed)))
		(or (and (equal host h) (equal user u))
		    (ange-ftp-put-hash-entry key val new-tbl)))))))
     ange-ftp-files-hashtable)
    (setq ange-ftp-files-hashtable new-tbl)))

;;;; ------------------------------------------------------------
;;;; File transfer mode support.
;;;; ------------------------------------------------------------

(defun ange-ftp-set-binary-mode (host user)
  "Tell the ftp process for the given HOST & USER to switch to binary mode."
  (let ((result (ange-ftp-send-cmd host user '(type "binary"))))
    (if (not (car result))
	(ange-ftp-error host user (concat "BINARY failed: " (cdr result)))
      (save-excursion
	(set-buffer (process-buffer (ange-ftp-get-process host user)))
	(setq ange-ftp-hash-mark-unit (ash ange-ftp-binary-hash-mark-size -4))))))

(defun ange-ftp-set-ascii-mode (host user)
  "Tell the ftp process for the given HOST & USER to switch to ascii mode."
  (let ((result (ange-ftp-send-cmd host user '(type "ascii"))))
    (if (not (car result))
	(ange-ftp-error host user (concat "ASCII failed: " (cdr result)))
      (save-excursion
	(set-buffer (process-buffer (ange-ftp-get-process host user)))
	(setq ange-ftp-hash-mark-unit (ash ange-ftp-ascii-hash-mark-size -4))))))

;;; ------------------------------------------------------------
;;; expand-file-name and friends...
;;; ------------------------------------------------------------

(defun ange-ftp-cd (host user dir)
  (let ((result (ange-ftp-send-cmd host user (list 'cd dir) "Doing CD")))
    (or (car result)
	(ange-ftp-error host user (concat "CD failed: " (cdr result))))))

(defun ange-ftp-get-pwd (host user)
  "Attempts to get the current working directory for the given HOST/USER pair.
Returns \( DIR . LINE \) where DIR is either the directory or NIL if not found,
and LINE is the relevant success or fail line from the FTP-client."
  (let* ((result (ange-ftp-send-cmd host user '(pwd) "Getting PWD"))
	 (line (cdr result))
	 dir)
    (if (car result)
	(ange-ftp-save-match-data
	  (and (or (string-match "\"\\([^\"]*\\)\"" line)
		   (string-match " \\([^ ]+\\) " line)) ; stone-age VMS servers!
	       (setq dir (substring line
				    (match-beginning 1)
				    (match-end 1))))))
    (cons dir line)))

(defconst ange-ftp-expand-dir-hashtable (ange-ftp-make-hashtable))

(defconst ange-ftp-expand-dir-regexp "^5.0 \\([^: ]+\\):")

(defun ange-ftp-expand-dir (host user dir)
  "Return the result of doing a PWD in the current FTP session to machine HOST
logged in as user USER and cd'd to directory DIR."
  (let* ((host-type (ange-ftp-host-type host user))
	 ;; It is more efficient to call ange-ftp-host-type
	 ;; before binding res, because ange-ftp-host-type sometimes
	 ;; adds to the info in the expand-dir-hashtable.
	 (fix-pathname-func
	  (cdr (assq host-type ange-ftp-fix-path-func-alist)))
	 (key (concat host "/" user "/" dir))
	 (res (ange-ftp-get-hash-entry key ange-ftp-expand-dir-hashtable)))
    (or res
	(progn
	  (or
	   (string-equal user "anonymous")
	   (string-equal user "ftp")
	   (not (eq host-type 'unix))
	   (let* ((ange-ftp-good-msgs (concat ange-ftp-expand-dir-regexp
					      "\\|"
					      ange-ftp-good-msgs))
		  (result (ange-ftp-send-cmd host user
					     (list 'get dir "/dev/null")
					     (format "expanding %s" dir)))
		  (line (cdr result)))
	     (setq res
		   (if (string-match ange-ftp-expand-dir-regexp line)
		       (substring line 
				  (match-beginning 1)
				  (match-end 1))))))
	  (or res
	      (if (string-equal dir "~")
		  (setq res (car (ange-ftp-get-pwd host user)))
		(let ((home (ange-ftp-expand-dir host user "~")))
		  (unwind-protect
		      (and (ange-ftp-cd host user dir)
			   (setq res (car (ange-ftp-get-pwd host user))))
		    (ange-ftp-cd host user home)))))
	  (if res
	      (progn
		(if fix-pathname-func
		    (setq res (funcall fix-pathname-func res 'reverse)))
		(ange-ftp-put-hash-entry
		 key res ange-ftp-expand-dir-hashtable)))
	  res))))

(defun ange-ftp-canonize-filename (n)
  "Take a string and short-circuit //, /. and /.."
  (if (string-match ".+//" n)		;don't upset Apollo users
      (setq n (substring n (1- (match-end 0)))))
  (let ((parsed (ange-ftp-ftp-path n)))
    (if parsed
	(let ((host (car parsed))
	      (user (nth 1 parsed))
	      (path (nth 2 parsed)))
	  
	  ;; See if remote path is absolute.  If so then just expand it and
	  ;; replace the path component of the overall path.
	  (cond ((string-match "^/" path)
		 path)
		
		;; Path starts with ~ or ~user.  Resolve that part of the path
		;; making it absolute then re-expand it.
		((string-match "^~[^/]*" path)
		 (let* ((tilda (substring path
					  (match-beginning 0) 
					  (match-end 0)))
			(rest (substring path (match-end 0)))
			(dir (ange-ftp-expand-dir host user tilda)))
		   (if dir
		       (setq path (concat dir rest))
		     (error "User \"%s\" is not known" 
			    (substring tilda 1)))))
		
		;; relative path.  Tack on homedir and re-expand.
		(t
		 (let ((dir (ange-ftp-expand-dir host user "~")))
		   (if dir
		       (setq path (concat
				   (ange-ftp-real-file-name-as-directory dir)
				   path))
		     (error "Unable to obtain CWD")))))
	  
	  (setq path (ange-ftp-real-expand-file-name path))
	  
	  ;; see if hit real expand-file-name bug...  this will probably annoy
	  ;; some Apollo people.  I'll wait until they shout, however.
	  (if (string-match "^//" path)
	      (setq path (substring path 1)))
	  
	  ;; Now substitute the expanded path back into the overall filename.
	  (ange-ftp-replace-path-component n path))
      
      ;; non-ange-ftp path.  Just expand normally.
      (if (eq (string-to-char n) ?/)
	  (ange-ftp-real-expand-file-name n)
	(ange-ftp-real-expand-file-name
	 (ange-ftp-real-file-name-nondirectory n)
	 (ange-ftp-real-file-name-directory n))))))

(defun ange-ftp-expand-file-name (name &optional default)
  "Documented as original."
  (ange-ftp-save-match-data
    (if (eq (string-to-char name) ?/)
	(while (cond ((string-match ".+//" name) ;don't upset Apollo users
		      (setq name (substring name (1- (match-end 0)))))
		     ((string-match "/~" name)
		      (setq name (substring name (1- (match-end 0))))))))
    (cond ((eq (string-to-char name) ?~)
	   (ange-ftp-real-expand-file-name name))
	  ((eq (string-to-char name) ?/)
	   (ange-ftp-canonize-filename name))
	  ((zerop (length name))
	   (ange-ftp-canonize-filename (or default default-directory)))
	  ((ange-ftp-canonize-filename
	    (concat (file-name-as-directory (or default default-directory))
		    name))))))

;;;; ------------------------------------------------------------
;;;; Redefinitions of standard GNU Emacs functions.
;;;; ------------------------------------------------------------

(defvar ange-ftp-file-name-as-directory-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where
FUNC converts a filename to a directory name for the operating
system TYPE.")

(defun ange-ftp-file-name-as-directory (name)
  "Documented as original."
  (let ((parsed (ange-ftp-ftp-path name)))
    (if parsed
	(if (string-equal (nth 2 parsed) "")
	    name
	  (funcall (or (cdr (assq
			     (ange-ftp-host-type (car parsed))
			     ange-ftp-file-name-as-directory-alist))
		       'ange-ftp-real-file-name-as-directory)
		   name))
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
		(let ((executing-macro t)
		      (filename (buffer-file-name))
		      (mod-p (buffer-modified-p)))
		  (unwind-protect
		      (ange-ftp-real-write-region start end temp nil visit)
		    ;; cleanup forms
		    (setq buffer-file-name filename)
		    (if (fboundp 'compute-buffer-file-truename)
			(compute-buffer-file-truename))
		    (set-buffer-modified-p mod-p)))
		(if binary
		    (ange-ftp-set-binary-mode host user))

		;; tell the process filter what size the transfer will be.
		(let ((attr (file-attributes temp)))
		  (if attr
		      (ange-ftp-set-xfer-size host user (nth 7 attr))))

		;; put or append the file.
		(let ((result (ange-ftp-send-cmd host user
						 (list cmd temp path)
						 (format "Writing %s" abbr))))
		  (or (car result)
		      (signal 'ftp-error
			      (list
			       "Opening output file"
			       (format "FTP Error: \"%s\"" (cdr result))
			       filename)))))
	    (ange-ftp-del-tmp-name temp)
	    (if binary 
		(ange-ftp-set-ascii-mode host user)))
	  (if (eq visit t)
	      (progn
		(ange-ftp-set-buffer-mode)
		(setq buffer-file-name filename)
		(if (fboundp 'compute-buffer-file-truename)
		    (compute-buffer-file-truename))
		(set-buffer-modified-p nil)))
	  (ange-ftp-message "Wrote %s" abbr)
	  (ange-ftp-add-file-entry filename))
      (ange-ftp-real-write-region start end filename append visit))))

(defun ange-ftp-insert-file-contents (filename &optional visit beg end replace)
  "Documented as original."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (let ((parsed (ange-ftp-ftp-path filename)))
    (if parsed
	(progn
	  (if visit
	      (progn
		(setq buffer-file-name filename)
		(if (fboundp 'compute-buffer-file-truename)
		    (compute-buffer-file-truename))))
	  (if (or (file-exists-p filename)
		  (progn
		    (setq ange-ftp-ls-cache-file nil)
		    (ange-ftp-del-hash-entry (file-name-directory filename)
					     ange-ftp-files-hashtable)
		    (file-exists-p filename)))
	      (let* ((host (nth 0 parsed))
		     (user (nth 1 parsed))
		     (path (ange-ftp-quote-string (nth 2 parsed)))
		     (temp (ange-ftp-make-tmp-name host))
		     (binary (ange-ftp-binary-file filename))
		     (abbr (ange-ftp-abbreviate-filename filename))
		     size)
		(unwind-protect
		    (progn
		      (if binary
			  (ange-ftp-set-binary-mode host user))
		      (let ((result (ange-ftp-send-cmd host user
					      (list 'get path temp)
					      (format "Retrieving %s" abbr))))
			(or (car result)
			    (signal 'ftp-error
				    (list
				     "Opening input file"
				     (format "FTP Error: \"%s\"" (cdr result))
				     filename))))
		      (if (or (ange-ftp-real-file-readable-p temp)
			      (sleep-for ange-ftp-retry-time)
			      ;; Wait for file to hopefully appear.
			      (ange-ftp-real-file-readable-p temp))
			  (setq
			   size
			   (nth 1 (progn
				    (if replace ; kludge...
					(delete-region (point-min)
						       (point-max)))
				    (ange-ftp-real-insert-file-contents
				     temp visit beg end nil))))
			(signal 'ftp-error
				(list
				 "Opening input file:"
				 (format
				  "FTP Error: %s not arrived or readable"
				  filename)))))
		  (if binary
		      (ange-ftp-set-ascii-mode host user))
		  (ange-ftp-del-tmp-name temp))
		(if visit
		    (progn
		      (setq buffer-file-name filename)
		      (if (fboundp 'compute-buffer-file-truename)
			  (compute-buffer-file-truename))))
		(list filename size))
	    (signal 'file-error
		    (list 
		     "Opening input file"
		     filename))))
      (ange-ftp-real-insert-file-contents filename visit beg end replace))))
 
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

(defun ange-ftp-expand-symlink (file dir)
  (if (file-name-absolute-p file)
      (ange-ftp-replace-path-component dir file)
    (expand-file-name file dir)))

(defun ange-ftp-file-symlink-p (file)
  "Documented as original."
  ;; call ange-ftp-expand-file-name rather than the normal
  ;; expand-file-name to stop loops when using a package that
  ;; redefines both file-symlink-p and expand-file-name.
  (setq file (ange-ftp-expand-file-name file))
  (if (ange-ftp-ftp-path file)
      (let ((file-ent
	     (ange-ftp-get-hash-entry
	      (ange-ftp-get-file-part file)
	      (ange-ftp-get-files (file-name-directory file)))))
	(if (stringp file-ent)
	    (if (file-name-absolute-p file-ent)
		(ange-ftp-replace-path-component
		      (file-name-directory file) file-ent)
	      file-ent)))
    (ange-ftp-real-file-symlink-p file)))

(defun ange-ftp-file-exists-p (path)
  "Documented as original."
  (setq path (expand-file-name path))
  (if (ange-ftp-ftp-path path)
      (if (ange-ftp-file-entry-p path)
	  (let ((file-ent (ange-ftp-get-file-entry path)))
	    (if (stringp file-ent)
		(file-exists-p
		 (ange-ftp-expand-symlink file-ent
					  (file-name-directory
					   (directory-file-name path))))
	      t)))
    (ange-ftp-real-file-exists-p path)))

(defun ange-ftp-file-directory-p (path)
  "Documented as original."
  (setq path (expand-file-name path))
  (if (ange-ftp-ftp-path path)
      ;; We do a file-name-as-directory on path here because some
      ;; machines (VMS) use a .DIR to indicate the filename associated
      ;; with a directory. This needs to be canonicalized.
      (let ((file-ent (ange-ftp-get-file-entry
		       (ange-ftp-file-name-as-directory path))))
	(if (stringp file-ent)
	    (file-directory-p
	     (ange-ftp-expand-symlink file-ent
				      (file-name-directory
				       (directory-file-name path))))
	  file-ent))
    (ange-ftp-real-file-directory-p path)))

(defun ange-ftp-directory-files (directory &optional full match
					   &rest v19-args)
  "Documented as original."
  (setq directory (expand-file-name directory))
  (if (ange-ftp-ftp-path directory)
      (progn
	(ange-ftp-barf-if-not-directory directory)
	(let ((tail (ange-ftp-hash-table-keys
		     (ange-ftp-get-files directory)))
	      files f)
	  (setq directory (file-name-as-directory directory))
	  (ange-ftp-save-match-data
	    (while tail
	      (setq f (car tail)
		    tail (cdr tail))
	      (if (or (not match) (string-match match f))
		  (setq files
			(cons (if full (concat directory f) f) files)))))
	  (nreverse files)))
    (apply 'ange-ftp-real-directory-files directory full match v19-args)))

(defun ange-ftp-file-attributes (file)
  "Documented as original."
  (setq file (expand-file-name file))
  (let ((parsed (ange-ftp-ftp-path file)))
    (if parsed
	(let ((part (ange-ftp-get-file-part file))
	      (files (ange-ftp-get-files (file-name-directory file))))
	  (if (ange-ftp-hash-entry-exists-p part files)
	      (let ((host (nth 0 parsed))
		    (user (nth 1 parsed))
		    (path (nth 2 parsed))
		    (dirp (ange-ftp-get-hash-entry part files)))
		(list (if (and (stringp dirp) (file-name-absolute-p dirp))
			  (ange-ftp-expand-symlink dirp
						   (file-name-directory file))
			dirp)		;0 file type
		      -1		;1 link count
		      -1		;2 uid
		      -1		;3 gid
		      '(0 0)		;4 atime
		      '(0 0)		;5 mtime
		      '(0 0)		;6 ctime
		      -1		;7 size
		      (concat (if (stringp dirp) "l" (if dirp "d" "-"))
			      "?????????") ;8 mode
		      nil		;9 gid weird
		      ;; Hack to give remote files a unique "inode number".
		      ;; It's actually the sum of the characters in its name.
		      (apply '+ (nconc (mapcar 'identity host)
				       (mapcar 'identity user)
				       (mapcar 'identity
					       (directory-file-name path))))
		      -1		;11 device number [v19 only]
		      ))))
      (ange-ftp-real-file-attributes file))))

(defun ange-ftp-file-writable-p (file)
  "Documented as original."
  (setq file (expand-file-name file))
  (if (ange-ftp-ftp-path file)
      (or (file-exists-p file)		;guess here for speed
	  (file-directory-p (file-name-directory file)))
    (ange-ftp-real-file-writable-p file)))

(defun ange-ftp-file-readable-p (file)
  "Documented as original."
  (setq file (expand-file-name file))
  (if (ange-ftp-ftp-path file)
      (file-exists-p file)
    (ange-ftp-real-file-readable-p file)))

(defun ange-ftp-delete-file (file)
  "Documented as original."
  (interactive "fDelete file: ")
  (setq file (expand-file-name file))
  (let ((parsed (ange-ftp-ftp-path file)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (path (ange-ftp-quote-string (nth 2 parsed)))
	       (abbr (ange-ftp-abbreviate-filename file))
	       (result (ange-ftp-send-cmd host user
					  (list 'delete path)
					  (format "Deleting %s" abbr))))
	  (or (car result)
	      (signal 'ftp-error
		      (list
		       "Removing old name"
		       (format "FTP Error: \"%s\"" (cdr result))
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
  (let (parsed)
    (if (and
	 (listp ange-ftp-make-backup-files)
	 (stringp buffer-file-name)
	 (setq parsed (ange-ftp-ftp-path buffer-file-name))
	 (or
	  (null ange-ftp-make-backup-files)
	  (not
	   (memq
	    (ange-ftp-host-type
	     (car parsed))
	    ange-ftp-make-backup-files))))
	nil
      (ange-ftp-real-backup-buffer))))

;;;; ------------------------------------------------------------
;;;; File copying support... totally re-written 6/24/92.
;;;; ------------------------------------------------------------

(defun ange-ftp-barf-or-query-if-file-exists (absname querystring interactive)
  (if (file-exists-p absname)
      (if (not interactive)
	  (signal 'file-already-exists (list absname))
	(if (not (yes-or-no-p (format "File %s already exists; %s anyway? "
				      absname querystring)))
	    (signal 'file-already-exists (list absname))))))

;; async local copy commented out for now since I don't seem to get
;; the process sentinel called for some processes.
;;
;; (defun ange-ftp-copy-file-locally (filename newname ok-if-already-exists
;; 					    keep-date cont)
;;   "Kludge to copy a local file and call a continuation when the copy
;; finishes."
;;   ;; check to see if we can overwrite
;;   (if (or (not ok-if-already-exists)
;; 	  (numberp ok-if-already-exists))
;;       (ange-ftp-barf-or-query-if-file-exists newname "copy to it" 
;; 					     (numberp ok-if-already-exists)))
;;   (let ((proc (start-process " *copy*"
;; 			     (generate-new-buffer "*copy*")
;; 			     "cp"
;; 			     filename
;; 			     newname))
;; 	res)
;;     (set-process-sentinel proc (function ange-ftp-copy-file-locally-sentinel))
;;     (process-kill-without-query proc)
;;     (save-excursion
;;       (set-buffer (process-buffer proc))
;;       (make-variable-buffer-local 'copy-cont)
;;       (setq copy-cont cont))))
;; 
;; (defun ange-ftp-copy-file-locally-sentinel (proc status)
;;   (save-excursion
;;     (set-buffer (process-buffer proc))
;;     (let ((cont copy-cont)
;; 	  (result (buffer-string)))
;;       (unwind-protect
;; 	  (if (and (string-equal status "finished\n")
;; 		   (zerop (length result)))
;; 	      (ange-ftp-call-cont cont t nil)
;; 	    (ange-ftp-call-cont cont
;; 				nil
;; 				(if (zerop (length result))
;; 				    (substring status 0 -1)
;; 				  (substring result 0 -1))))
;; 	(kill-buffer (current-buffer))))))

;; this is the extended version of ange-ftp-copy-file-internal that works
;; asyncronously if asked nicely.
(defun ange-ftp-copy-file-internal (filename newname ok-if-already-exists
					     keep-date &optional msg cont nowait)
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  ;; canonicalize newname if a directory.
  (if (file-directory-p newname)
      (setq newname (expand-file-name (file-name-nondirectory filename) newname)))

  (let ((f-parsed (ange-ftp-ftp-path filename))
	(t-parsed (ange-ftp-ftp-path newname)))

    ;; local file to local file copy?
    (if (and (not f-parsed) (not t-parsed))
	(progn
	  (ange-ftp-real-copy-file filename newname ok-if-already-exists
				   keep-date)
	  (if cont
	      (ange-ftp-call-cont cont t "Copied locally")))
      ;; one or both files are remote.
      (let* ((f-host (and f-parsed (nth 0 f-parsed)))
	     (f-user (and f-parsed (nth 1 f-parsed)))
	     (f-path (and f-parsed (ange-ftp-quote-string (nth 2 f-parsed))))
	     (f-abbr (ange-ftp-abbreviate-filename filename))
	     (t-host (and t-parsed (nth 0 t-parsed)))
	     (t-user (and t-parsed (nth 1 t-parsed)))
	     (t-path (and t-parsed (ange-ftp-quote-string (nth 2 t-parsed))))
	     (t-abbr (ange-ftp-abbreviate-filename newname filename))
	     (binary (or (ange-ftp-binary-file filename)
			 (ange-ftp-binary-file newname)))
	     temp1
	     temp2)

	;; check to see if we can overwrite
	(if (or (not ok-if-already-exists)
		(numberp ok-if-already-exists))
	    (ange-ftp-barf-or-query-if-file-exists newname "copy to it" 
						   (numberp ok-if-already-exists)))

	;; do the copying.
	(if f-parsed
	    
	    ;; filename was remote.
	    (progn
	      (if (or (ange-ftp-use-gateway-p f-host)
		      t-parsed)
		  ;; have to use intermediate file if we are getting via
		  ;; gateway machine or we are doing a remote to remote copy.
		  (setq temp1 (ange-ftp-make-tmp-name f-host)))
	      
	      (if binary
		  (ange-ftp-set-binary-mode f-host f-user))

	      (ange-ftp-send-cmd
	       f-host
	       f-user
	       (list 'get f-path (or temp1 newname))
	       (or msg
		   (if (and temp1 t-parsed)
		       (format "Getting %s" f-abbr)
		     (format "Copying %s to %s" f-abbr t-abbr)))
	       (list (function ange-ftp-cf1)
		     filename newname binary msg
		     f-parsed f-host f-user f-path f-abbr
		     t-parsed t-host t-user t-path t-abbr
		     temp1 temp2 cont nowait)
	       nowait))

	  ;; filename wasn't remote.  newname must be remote.  call the
	  ;; function which does the remainder of the copying work.
	  (ange-ftp-cf1 t nil
			filename newname binary msg
			f-parsed f-host f-user f-path f-abbr
			t-parsed t-host t-user t-path t-abbr
			nil nil cont nowait))))))

;; next part of copying routine.
(defun ange-ftp-cf1 (result line
			    filename newname binary msg
			    f-parsed f-host f-user f-path f-abbr
			    t-parsed t-host t-user t-path t-abbr
			    temp1 temp2 cont nowait)
  (if line
      ;; filename must have been remote, and we must have just done a GET.
      (unwind-protect
	  (or result
	      ;; GET failed for some reason.  Clean up and get out.
	      (progn
		(and temp1 (ange-ftp-del-tmp-name temp1))
		(or cont
		    (signal 'ftp-error (list "Opening input file"
					     (format "FTP Error: \"%s\"" line)
					     filename)))))
	;; cleanup
	(if binary
	    (ange-ftp-set-ascii-mode f-host f-user))))

  (if result
      ;; We now have to copy either temp1 or filename to newname.
      (if t-parsed
      
	  ;; newname was remote.
	  (progn
	    (if (ange-ftp-use-gateway-p t-host)
		(setq temp2 (ange-ftp-make-tmp-name t-host)))
	    
	    ;; make sure data is moved into the right place for the
	    ;; outgoing transfer.  gateway temporary files complicate
	    ;; things nicely.
	    (if temp1
		(if temp2
		    (if (string-equal temp1 temp2)
			(setq temp1 nil)
		      (ange-ftp-real-copy-file temp1 temp2 t))
		  (setq temp2 temp1 temp1 nil))
	      (if temp2
		  (ange-ftp-real-copy-file filename temp2 t)))
	    
	    (if binary
		(ange-ftp-set-binary-mode t-host t-user))

	    ;; tell the process filter what size the file is.
	    (let ((attr (file-attributes (or temp2 filename))))
	      (if attr
		  (ange-ftp-set-xfer-size t-host t-user (nth 7 attr))))

	    (ange-ftp-send-cmd
	     t-host
	     t-user
	     (list 'put (or temp2 filename) t-path)
	     (or msg
		 (if (and temp2 f-parsed)
		     (format "Putting %s" newname)
		   (format "Copying %s to %s" f-abbr t-abbr)))
	     (list (function ange-ftp-cf2)
		   newname t-host t-user binary temp1 temp2 cont)
	     nowait))
    
	;; newname wasn't remote.
	(ange-ftp-cf2 t nil newname t-host t-user binary temp1 temp2 cont))

    ;; first copy failed, tell caller
    (ange-ftp-call-cont cont result line)))

;; last part of copying routine.
(defun ange-ftp-cf2 (result line newname t-host t-user binary temp1 temp2 cont)
  (unwind-protect
      (if line
	  ;; result from doing a local to remote copy.
	  (unwind-protect
	      (progn
		(or result
		    (or cont
			(signal 'ftp-error
				(list "Opening output file"
				      (format "FTP Error: \"%s\"" line)
				      newname))))
		
		(ange-ftp-add-file-entry newname))
	    
	    ;; cleanup.
	    (if binary
		(ange-ftp-set-ascii-mode t-host t-user)))
	      
	;; newname was local.
	(if temp1
	    (ange-ftp-real-copy-file temp1 newname t)))
	  
    ;; clean up
    (and temp1 (ange-ftp-del-tmp-name temp1))
    (and temp2 (ange-ftp-del-tmp-name temp2))
    (ange-ftp-call-cont cont result line)))

(defun ange-ftp-copy-file (filename newname &optional ok-if-already-exists
				    keep-date)
  "Documented as original."
  (interactive "fCopy file: \nFCopy %s to file: \np")
  (ange-ftp-copy-file-internal filename
			       newname
			       ok-if-already-exists
			       keep-date
			       nil
			       nil
			       (interactive-p)))

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
	       (nabbr (ange-ftp-abbreviate-filename newname filename))
	       (result (ange-ftp-send-cmd f-host f-user cmd
					  (format "Renaming %s to %s"
						  fabbr
						  nabbr))))
	  (or (car result)
	      (signal 'ftp-error
		      (list
		       "Renaming"
		       (format "FTP Error: \"%s\"" (cdr result))
		       filename
		       newname)))
	  (ange-ftp-add-file-entry newname)
	  (ange-ftp-delete-file-entry filename))
      (ange-ftp-copy-file-internal filename newname t nil)
      (delete-file filename))))

(defun ange-ftp-rename-local-to-remote (filename newname)
  "Rename local FILE to remote file NEWNAME."
  (let* ((fabbr (ange-ftp-abbreviate-filename filename))
	 (nabbr (ange-ftp-abbreviate-filename newname filename))
	 (msg (format "Renaming %s to %s" fabbr nabbr)))
    (ange-ftp-copy-file-internal filename newname t nil msg)
    (let (ange-ftp-process-verbose)
      (delete-file filename))))

(defun ange-ftp-rename-remote-to-local (filename newname)
  "Rename remote file FILE to local file NEWNAME."
  (let* ((fabbr (ange-ftp-abbreviate-filename filename))
	 (nabbr (ange-ftp-abbreviate-filename newname filename))
	 (msg (format "Renaming %s to %s" fabbr nabbr)))
    (ange-ftp-copy-file-internal filename newname t nil msg)
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
	(ange-ftp-barf-or-query-if-file-exists
	 newname
	 "rename to it"
	 (numberp ok-if-already-exists)))
    (if f-parsed
	(if t-parsed
	    (ange-ftp-rename-remote-to-remote filename newname f-parsed
					      t-parsed binary)
	  (ange-ftp-rename-remote-to-local filename newname))
      (if t-parsed
	  (ange-ftp-rename-local-to-remote filename newname)
	(ange-ftp-real-rename-file filename newname ok-if-already-exists)))))

;;;; ------------------------------------------------------------
;;;; Classic Dired support.
;;;; ------------------------------------------------------------

(defvar ange-ftp-dired-host-type nil
  "The host type associated with a dired buffer. (buffer local)")
(make-variable-buffer-local 'ange-ftp-dired-host-type)

(defun ange-ftp-dired-readin (dirname buffer)
  "Documented as original."
  (let ((file (ange-ftp-abbreviate-filename dirname))
	(parsed (ange-ftp-ftp-path dirname)))
    (save-excursion
      (ange-ftp-message "Reading directory %s..." file)
      (set-buffer buffer)
      (let ((buffer-read-only nil))
	(widen)
	(erase-buffer)
	(setq dirname (expand-file-name dirname))
	(if parsed
	    (let ((host-type (ange-ftp-host-type (car parsed))))
	      (setq ange-ftp-dired-host-type host-type)
	      (insert (ange-ftp-ls dirname dired-listing-switches t)))
	  (if (ange-ftp-real-file-directory-p dirname)
	      (call-process "ls" nil buffer nil
			    dired-listing-switches dirname)
	    (let ((default-directory
		    (ange-ftp-real-file-name-directory dirname)))
	      (call-process
	       shell-file-name nil buffer nil
	       "-c" (concat
		     "ls " dired-listing-switches " "
		     (ange-ftp-real-file-name-nondirectory dirname))))))
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

(defvar ange-ftp-dired-re-exe-alist nil
  "Association list of regexps \(strings\) which match file lines of
 executable files.")

(defvar ange-ftp-dired-re-dir-alist nil
  "Association list of regexps \(strings\) which match file lines of
 subdirectories.")

(defvar ange-ftp-dired-insert-headerline-alist nil
  "Association list of \(TYPE \. FUNC \) pairs, where FUNC is
the function to be used by dired to insert the headerline of
the dired buffer.")

(defvar ange-ftp-dired-move-to-filename-alist nil
  "Association list of \(TYPE \. FUNC \) pairs, where FUNC is
the function to be used by dired to move to the beginning of a
filename.")

(defvar ange-ftp-dired-move-to-end-of-filename-alist nil
  "Association list of \(TYPE \. FUNC \) pairs, where FUNC is
the function to be used by dired to move to the end of a
filename.")

(defvar ange-ftp-dired-get-filename-alist nil
  "Association list of \(TYPE \. FUNC \) pairs, where FUNC is
the function to be used by dired to get a filename from the
current line.")

(defvar ange-ftp-dired-between-files-alist nil
  "Association list of \(TYPE \. FUNC \) pairs, where FUNC is
the function to be used by dired to determine when the point
is on a line between files.")

(defvar ange-ftp-dired-ls-trim-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC is
a function which trims extraneous lines from a directory listing.")

(defvar ange-ftp-dired-clean-directory-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC is
a function which cleans out old versions of files in the OS TYPE.")

(defvar ange-ftp-dired-flag-backup-files-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC is
a functions which flags the backup files for deletion in the OS TYPE.")

(defvar ange-ftp-dired-backup-diff-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC diffs
a file with its backup. The backup file is determined according to
the OS TYPE.")

;; Could use dired-before-readin-hook here, instead of overloading
;; dired-readin. However, if people change this hook after ange-ftp
;; is loaded, they'll break things.
;; Also, why overload dired-readin rather than dired-mode?
;; Because I don't want to muck up virtual dired (see dired-x.el).

(defun ange-ftp-tree-dired-readin (dirname buffer)
  "Documented as original."
  (let ((parsed (ange-ftp-ftp-path dirname)))
    (if parsed
	(save-excursion
	  (set-buffer buffer)
	  (setq ange-ftp-dired-host-type
		(ange-ftp-host-type (car parsed)))
	  (and ange-ftp-dl-dir-regexp
	       (eq ange-ftp-dired-host-type 'unix)
	       (string-match ange-ftp-dl-dir-regexp dirname)
	       (setq ange-ftp-dired-host-type 'unix:dl))
	  (let ((eentry (assq ange-ftp-dired-host-type
			      ange-ftp-dired-re-exe-alist))
		(dentry (assq ange-ftp-dired-host-type
			      ange-ftp-dired-re-dir-alist)))
	    (if eentry
		(set (make-local-variable 'dired-re-exe) (cdr eentry)))
	    (if dentry
		(set (make-local-variable 'dired-re-dir) (cdr dentry)))
	    ;; No switches are sent to dumb hosts, so don't confuse dired.
	    ;; I hope that dired doesn't get excited if it doesn't see the l
	    ;; switch. If it does, then maybe fake things by setting this to
	    ;; "-Al".
	    (if (memq ange-ftp-dired-host-type ange-ftp-dumb-host-types)
		(setq dired-actual-switches "-Al"))))))
  (ange-ftp-real-dired-readin dirname buffer))

(defun ange-ftp-dired-insert-headerline (dir)
  "Documented as original."
  (funcall (or (and ange-ftp-dired-host-type
		    (cdr (assq ange-ftp-dired-host-type
			       ange-ftp-dired-insert-headerline-alist)))
	       'ange-ftp-real-dired-insert-headerline)
	   dir))

(defun ange-ftp-dired-move-to-filename (&optional raise-error eol)
  "Documented as original."
  (funcall (or (and ange-ftp-dired-host-type
		    (cdr (assq ange-ftp-dired-host-type
			       ange-ftp-dired-move-to-filename-alist)))
	       'ange-ftp-real-dired-move-to-filename)
	   raise-error eol))

(defun ange-ftp-dired-move-to-end-of-filename (&optional no-error)
  "Documented as original."
  (funcall (or (and ange-ftp-dired-host-type
		    (cdr (assq ange-ftp-dired-host-type
			       ange-ftp-dired-move-to-end-of-filename-alist)))
	       'ange-ftp-real-dired-move-to-end-of-filename)
	   no-error))

(defun ange-ftp-dired-get-filename (&optional localp no-error-if-not-filep)
  "Documented as original."
  (funcall (or (and ange-ftp-dired-host-type
		    (cdr (assq ange-ftp-dired-host-type
			       ange-ftp-dired-get-filename-alist)))
	       'ange-ftp-real-dired-get-filename)
	   localp no-error-if-not-filep))

(defun ange-ftp-dired-between-files ()
  "Documented as original."
  (funcall (or (and ange-ftp-dired-host-type
		    (cdr (assq ange-ftp-dired-host-type
			       ange-ftp-dired-between-files-alist)))
	       'ange-ftp-real-dired-between-files)))

(defvar ange-ftp-bob-version-alist nil
  "Association list of pairs \( TYPE \. FUNC \), where FUNC is
a function to be used to bob the version number off of a filename
in OS TYPE.")

(defun ange-ftp-dired-find-file ()
  "Documented as original."
  (interactive)
  (find-file (funcall (or (and ange-ftp-dired-host-type
			       (cdr (assq ange-ftp-dired-host-type
					  ange-ftp-bob-version-alist)))
			  'identity)
		      (dired-get-filename))))

;; Need the following functions for making filenames of compressed
;; files, because some OS's (unlike UNIX) do not allow a filename to
;; have two extensions.

(defvar ange-ftp-dired-compress-make-compressed-filename-alist nil
  "Association list of \( TYPE \. FUNC \) pairs, where FUNC converts a
filename to the filename of the associated compressed file.")

;;; this overwrites dired's `dired-compress-make-compressed-filename'
(defun ange-ftp-dired-compress-make-compressed-filename (name &optional reverse)
  "Converts a filename to the filename of the associated compressed
file. With an optional reverse argument, the reverse conversion is done.

Modified to work with gzip (GNU zip) files."
  (let ((parsed (ange-ftp-ftp-path name))
	conversion-func)
    (if (and parsed
	     (setq conversion-func
		   (cdr (assq (ange-ftp-host-type (car parsed))
			      ange-ftp-dired-compress-make-compressed-filename-alist))))
	(funcall conversion-func name reverse)
      (if reverse

          ;; uncompress...
          ;; return `nil' if no match found -- better than nothing
          (let (case-fold-search ; case-sensitive search
                (string
                 (concat "\\.\\(g?z\\|" (regexp-quote dired-gzip-file-extension)
                         "$\\|Z\\)$")))
            
            (and (string-match string name)
                 (substring name 0 (match-beginning 0))))

        ;; add appropriate extension
        ;; note: it could be that `gz' is not the proper extension for gzip
        (concat name
                (if dired-use-gzip-instead-of-compress
                    dired-gzip-file-extension ".Z"))))))

(defun ange-ftp-dired-clean-directory (keep)
  "Documented as original."
  (interactive "P")
  (funcall (or (and ange-ftp-dired-host-type
		    (cdr (assq ange-ftp-dired-host-type
			       ange-ftp-dired-clean-directory-alist)))
	       'ange-ftp-real-dired-clean-directory)
	   keep))

(defun ange-ftp-dired-backup-diff (&optional switches)
  "Documented as original."
  (interactive (list (if (fboundp 'diff-read-switches)
			 (diff-read-switches "Diff with switches: "))))
  (funcall (or (and ange-ftp-dired-host-type
		    (cdr (assq ange-ftp-dired-host-type
			       ange-ftp-dired-backup-diff-alist)))
	       'ange-ftp-real-dired-backup-diff)
	   switches))


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
	(let* ((pt (point))
	       (path (nth 2 parsed))
	       (host-type (ange-ftp-host-type (car parsed)))
	       (dumb (memq host-type ange-ftp-dumb-host-types))
	       trim-func case-fold-search)
	  ;; Make sure that case-fold-search is nil
	  ;; so that we can look at the switches.
	  (if wildcard
	      (if (not (memq host-type '(unix dumb-unix)))
		  (insert (ange-ftp-ls file switches nil))
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
		(let ((dir (ange-ftp-real-file-name-directory path)))
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

               ;;;;;;;;;;;;;;;;;;;;;;;;;;
               ;; Big issue here Andy! ;;
	       ;;;;;;;;;;;;;;;;;;;;;;;;;;
	       ;; In tree dired V5.245 Sebastian has used the following
	       ;; trick to resolve symbolic links to directories. This causes
	       ;; havoc with ange-ftp, because ange-ftp expands dots, with
	       ;; expand-file-name before it sends them. This means that this
	       ;; trick currently fails for remote SysV machines. But worse,
	       ;; /vms:/DEV:/FOO/. expands to /vms:/DEV:/FOO, which converts
	       ;; to DEV:FOO and not DEV:[FOO]. i.e it is only in UNIX that
	       ;; we can play fast and loose with the difference between
	       ;; directory names and their associated filenames.
	       ;; My temporary fix is to knock Sebastian's dot off.
	       ;; Maybe things can be made real clever in
	       ;; the future, so that Sebastian can have his way with remote
	       ;; SysV machines.
	       ;; Sebastian in dired-readin-insert says:

	          ;; On SysV derived system, symbolic links to
		  ;; directories are not resolved, while on BSD
		  ;; derived it suffices to let DIRNAME end in slash.
		  ;; We always let it end in "/." since it does no
		  ;; harm on BSD and makes Dired work on such links on
		  ;; SysV.

	    (if (string-match "/\\.$" path)
		(setq
		 file
		 (ange-ftp-replace-path-component
		  file (substring path 0 -1))))
	    (if (string-match "R" switches)
		(progn
		  (insert (ange-ftp-ls file switches nil))
		  ;; fix up the subdirectory names in the recursive
		  ;; listing.
		  (ange-ftp-dired-fixup-subdirs pt file))
	      (insert
	       (ange-ftp-ls file
			    switches
			    (and (or dumb (string-match "[aA]" switches))
				 full-directory-p))))
	    (if (and (null full-directory-p)
		     (setq trim-func
			   (cdr (assq host-type
				      ange-ftp-dired-ls-trim-alist))))
		;; If full-directory-p and wild-card are null, then only one
		;; line per file must be inserted.
		;; Some OS's (like VMS) insert other crap. Clean it out.
		(save-restriction
		  (narrow-to-region pt (point))
		  (funcall trim-func)))))
      (ange-ftp-real-dired-ls file switches wildcard full-directory-p))))

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

(defun ange-ftp-make-directory (dir &optional parents)
  "Documented as original."
  (interactive (list (let ((current-prefix-arg current-prefix-arg))
		       (read-directory-name "Create directory: "))
		     current-prefix-arg))
  (if (file-exists-p dir)
      (error "Cannot make directory %s: file already exists" dir)
    (let ((parsed (ange-ftp-ftp-path dir)))
      (if parsed
	  (let* ((host (nth 0 parsed))
		 (user (nth 1 parsed))
		 ;; Some ftp's on unix machines (at least on Suns)
		 ;; insist that mkdir take a filename, and not a
		 ;; directory-name name as an arg. Argh!! This is a bug.
		 ;; Non-unix machines will probably always insist
		 ;; that mkdir takes a directory-name as an arg
		 ;; (as the ftp man page says it should).
		 (path (ange-ftp-quote-string
			(if (eq (ange-ftp-host-type host) 'unix)
			    (ange-ftp-real-directory-file-name (nth 2 parsed))
			  (ange-ftp-real-file-name-as-directory
			   (nth 2 parsed)))))
		 (abbr (ange-ftp-abbreviate-filename dir))
		 (result (ange-ftp-send-cmd host user
					    (list 'mkdir path)
					    (format "Making directory %s"
						    abbr))))
	    (or (car result)
		(ange-ftp-error host user
				(format "Could not make directory %s: %s"
					dir
					(cdr result))))
	    (ange-ftp-add-file-entry dir t))
	(ange-ftp-real-make-directory dir parents)))))

(defun ange-ftp-remove-directory (dir)
  "Documented as original."
  (interactive 
   (list (expand-file-name (read-file-name "Remove directory: "
					   nil nil 'confirm)))) 
  (if (file-directory-p dir)
      (let ((parsed (ange-ftp-ftp-path dir)))
	(if parsed
	    (let* ((host (nth 0 parsed))
		   (user (nth 1 parsed))
		   ;; Some ftp's on unix machines (at least on Suns)
		   ;; insist that rmdir take a filename, and not a
		   ;; directory-name name as an arg. Argh!! This is a bug.
		   ;; Non-unix machines will probably always insist
		   ;; that rmdir takes a directory-name as an arg
		   ;; (as the ftp man page says it should).
		   (path (ange-ftp-quote-string
			  (if (eq (ange-ftp-host-type host) 'unix)
			      (ange-ftp-real-directory-file-name
			       (nth 2 parsed))
			    (ange-ftp-real-file-name-as-directory
			     (nth 2 parsed)))))
		  (abbr (ange-ftp-abbreviate-filename dir))
		  (result (ange-ftp-send-cmd host user
					     (list 'rmdir path)
					     (format "Removing directory %s"
						     abbr))))
	      (or (car result)
		  (ange-ftp-error host user
				  (format "Could not remove directory %s: %s"
					  dir
					  (cdr result))))
	      (ange-ftp-delete-file-entry dir t))
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
	(let* ((tmp1 (and pa1 (ange-ftp-make-tmp-name (car pa1))))
	       (tmp2 (and pa2 (ange-ftp-make-tmp-name (car pa2))))
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
						fn1)
			  pa1 (ange-ftp-ftp-path fn1)
			  bin1 (ange-ftp-binary-file fn1)))
		(if dir2
		    (setq fn2 (expand-file-name (file-name-nondirectory fn1)
						fn2)
			  pa2 (ange-ftp-ftp-path fn2)
			  bin2 (ange-ftp-binary-file fn2)))
		(and pa1 (ange-ftp-copy-file-internal fn1 tmp1 t nil
				   (format "Getting %s" fn1)))
		(and pa2 (ange-ftp-copy-file-internal fn2 tmp2 t nil
				   (format "Getting %s" fn2)))
		(and ange-ftp-process-verbose
		     (ange-ftp-message "doing diff..."))
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
				   (get-buffer-process
				    compilation-last-buffer)
				   (eq (process-status
					(get-buffer-process
					 compilation-last-buffer))
				       'run))
			 (accept-process-output))))
		(and ange-ftp-process-verbose
		     (ange-ftp-message "doing diff...done"))
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
	    (and tmp1 (ange-ftp-del-tmp-name tmp1))
	    (and tmp2 (ange-ftp-del-tmp-name tmp2))))
      (ange-ftp-real-diff fn1 fn2 switches))))
	    
(defun ange-ftp-dired-call-process (program discard &rest arguments)
  "Documented as original."
  ;; PROGRAM is always one of those below in the cond in dired.el.
  ;; The ARGUMENTS are (nearly) always files.
  (if (ange-ftp-ftp-path default-directory)
      ;; Can't use ange-ftp-dired-host-type here because the current
      ;; buffer is *dired-check-process output*
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
	(ftp-error (insert (format "%s: %s, %s\n"
				    (nth 1 oops)
				    (nth 2 oops)
				    (nth 3 oops))))
	(error (insert (format "%s\n" (nth 1 oops)))))
    (apply 'call-process program nil (not discard) nil arguments)))


(defun ange-ftp-call-compress (args)
  "Perform a compress command on a remote file.
Works by taking a copy of the file, compressing it and copying the file
back."
  (if (or (not (= (length args) 2))
	  (not (string-equal "-f" (car args))))
      (error
       "ange-ftp-call-compress: missing -f flag and/or missing filename: %s"
       args))
  (let* ((file (nth 1 args))
	 (parsed (ange-ftp-ftp-path file))
	 (tmp1 (ange-ftp-make-tmp-name (car parsed)))
	 (tmp2 (ange-ftp-make-tmp-name (car parsed)))
	 (abbr (ange-ftp-abbreviate-filename file))
	 (nfile (ange-ftp-dired-compress-make-compressed-filename file))
	 (nabbr (ange-ftp-abbreviate-filename nfile))
	 (msg1 (format "Getting %s" abbr))
	 (msg2 (format "Putting %s" nabbr)))
    (unwind-protect
	(progn
	  (ange-ftp-copy-file-internal file tmp1 t nil msg1)
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
	      (progn
		(let (ange-ftp-process-verbose)
		  (delete-file file))
		(ange-ftp-copy-file-internal tmp2 nfile t nil msg2))))
      (ange-ftp-del-tmp-name tmp1)
      (ange-ftp-del-tmp-name tmp2))))
    
(defun ange-ftp-call-uncompress (args)
  "Perform an uncompress command on a remote file.
Works by taking a copy of the file, uncompressing it and copying the file
back."
  (if (not (= (length args) 1))
      (error "ange-ftp-call-uncompress: missing filename: %s" args))
  (let* ((file (car args))
	 (parsed (ange-ftp-ftp-path file))
	 (tmp1 (ange-ftp-make-tmp-name (car parsed)))
	 (tmp2 (ange-ftp-make-tmp-name (car parsed)))
	 (abbr (ange-ftp-abbreviate-filename file))
	 (nfile (ange-ftp-dired-compress-make-compressed-filename file 'reverse))
	 (nabbr (ange-ftp-abbreviate-filename nfile))
	 (msg1 (format "Getting %s" abbr))
	 (msg2 (format "Putting %s" nabbr))
;;	 ;; Cheap hack because of problems with binary file transfers from
;;	 ;; VMS hosts.
;;	 (gbinary (not (eq 'vms (ange-ftp-host-type (car parsed)))))
	 )
    (unwind-protect
	(progn
	  (ange-ftp-copy-file-internal file tmp1 t nil msg1)
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
	      (progn
		(let (ange-ftp-process-verbose)
		  (delete-file file))
		(ange-ftp-copy-file-internal tmp2 nfile t nil msg2))))
      (ange-ftp-del-tmp-name tmp1)
      (ange-ftp-del-tmp-name tmp2))))

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
	      (let* ((host (nth 0 parsed))
		     (user (nth 1 parsed))
		     (path (ange-ftp-quote-string (nth 2 parsed)))
		     (abbr (ange-ftp-abbreviate-filename file))
		     (result (ange-ftp-send-cmd host user
						(list 'chmod mode path)
						(format "doing chmod %s"
							abbr))))
		(or (car result)
		    (ange-ftp-error host user
				    (format "chmod: %s: \"%s\""
					    file 
					    (cdr result)))))))))
     (cdr args)))
  (setq ange-ftp-ls-cache-file nil))	;stop confusing dired

;; Need to abstract the way dired computes the names of compressed files.
;; I feel badly about these two overloads.

(defun ange-ftp-dired-compress ()
  ;; Compress current file.  Return nil for success, offending filename else.
  (let* (buffer-read-only
	 (from-file (dired-get-filename))
	 (to-file (ange-ftp-dired-compress-make-compressed-filename from-file)))
    (cond ((save-excursion (beginning-of-line)
			   (looking-at dired-re-sym))
	   (dired-log (concat "Attempt to compress a symbolic link:\n"
			      from-file))
	   (dired-make-relative from-file))
	  ((dired-check-process (concat "Compressing " from-file)
				"compress" "-f" from-file)
	   ;; errors from the process are already logged by
	   ;; dired-check-process
	   (dired-make-relative from-file))
	  (t
	 (dired-update-file-line to-file)
	 nil))))

(defun ange-ftp-dired-uncompress ()
  ;; Uncompress current file.  Return nil for success,
  ;; offending filename else.
  (let* (buffer-read-only
	 (from-file (dired-get-filename))
	 (to-file (ange-ftp-dired-compress-make-compressed-filename from-file 'reverse)))
    (if (dired-check-process (concat "Uncompressing " from-file)
			     "uncompress" from-file)
	(dired-make-relative from-file)
      (dired-update-file-line to-file)
      nil)))

(defun ange-ftp-dired-flag-backup-files (&optional unflag-p)
  "Documented as original."
  (interactive "P")
  (funcall (or (and ange-ftp-dired-host-type
		    (cdr (assq ange-ftp-dired-host-type
			       ange-ftp-dired-flag-backup-files-alist)))
	       'ange-ftp-real-dired-flag-backup-files)
	   unflag-p))

;;; ------------------------------------------------------------
;;; Noddy support for async copy-file within dired.
;;; ------------------------------------------------------------

(defun ange-ftp-dired-copy-file (from to ok-flag &optional cont nowait)
  "Documented as original."
  (dired-handle-overwrite to)
  (ange-ftp-copy-file-internal from to ok-flag dired-copy-preserve-time nil
			       cont nowait))

(defun ange-ftp-dired-do-create-files (op-symbol file-creator operation arg
						 &optional marker-char op1
						 how-to)
  "Documented as original."
  ;; we need to let ange-ftp-dired-create-files know that we indirectly
  ;; called it rather than somebody else.
  (let ((ange-ftp-dired-do-create-files t)) ; tell who caller is
    (ange-ftp-real-dired-do-create-files op-symbol file-creator operation
					 arg marker-char op1 how-to)))

(defun ange-ftp-dired-create-files (file-creator operation fn-list name-constructor
						 &optional marker-char)
  "Documented as original."
  (if (and (boundp 'ange-ftp-dired-do-create-files)
	   ;; called from ange-ftp-dired-do-create-files?
	   ange-ftp-dired-do-create-files
	   ;; any files worth copying?
	   fn-list
	   ;; we only support async copy-file at the mo.
	   (eq file-creator 'dired-copy-file)
	   ;; it is only worth calling the alternative function for remote files
	   ;; as we tie ourself in recursive knots otherwise.
	   (or (ange-ftp-ftp-path (car fn-list))
	       ;; we can only call the name constructor for dired-do-create-files
	       ;; since the one for regexps starts prompting here, there and
	       ;; everywhere.
	       (ange-ftp-ftp-path (funcall name-constructor (car fn-list)))))
      ;; use the process-filter driven routine rather than the iterative one.
      (ange-ftp-dcf-1 file-creator
   		      operation
 		      fn-list
 		      name-constructor
 		      (and (boundp 'target) target)	;dynamically bound
 		      marker-char
 		      (current-buffer)
 		      nil	;overwrite-query
 		      nil	;overwrite-backup-query
 		      nil	;failures
 		      nil	;skipped
 		      0		;success-count
 		      (length fn-list) ;total
 		      )
    ;; normal case... use the interative routine... much cheaper.
    (ange-ftp-real-dired-create-files file-creator operation fn-list
				      name-constructor marker-char)))

(defun ange-ftp-dcf-1 (file-creator operation fn-list name-constructor
		       target marker-char buffer overwrite-query 
		       overwrite-backup-query failures skipped
		       success-count total)
  (let ((old-buf (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (if (null fn-list)
	      (ange-ftp-dcf-3 failures operation total skipped
			      success-count buffer)
	    
	    (let* ((from (car fn-list))
		   (to (funcall name-constructor from)))
	      (if (equal to from)
		  (progn
		    (setq to nil)
		    (dired-log "Cannot %s to same file: %s\n"
			       (downcase operation) from)))
	      (if (not to)
		  (ange-ftp-dcf-1 file-creator
				  operation
				  (cdr fn-list)
				  name-constructor
				  target
				  marker-char
				  buffer
				  overwrite-query
				  overwrite-backup-query
				  failures
				  (cons (dired-make-relative from) skipped)
				  success-count
				  total)
		(let* ((overwrite (file-exists-p to))
		       (overwrite-confirmed	; for dired-handle-overwrite
			(and overwrite
			     (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
			       (dired-query 'overwrite-query
					    "Overwrite `%s'?" to))))
		       ;; must determine if FROM is marked before file-creator
		       ;; gets a chance to delete it (in case of a move).
		       (actual-marker-char
			(cond  ((integerp marker-char) marker-char)
			       (marker-char (dired-file-marker from)) ; slow
			       (t nil))))
		  (condition-case err
		      (funcall file-creator from to overwrite-confirmed
			       (list (function ange-ftp-dcf-2)
				     nil	;err
				     file-creator operation fn-list
				     name-constructor
				     target
				     marker-char actual-marker-char
				     buffer to from
				     overwrite
				     overwrite-confirmed
				     overwrite-query 
				     overwrite-backup-query
				     failures skipped success-count
				     total)
			       t)
		    (file-error		; FILE-CREATOR aborted
		     (ange-ftp-dcf-2 nil ;result
				     nil ;line
				     err
				     file-creator operation fn-list
				     name-constructor
				     target
				     marker-char actual-marker-char
				     buffer to from
				     overwrite
				     overwrite-confirmed
				     overwrite-query 
				     overwrite-backup-query
				     failures skipped success-count
				     total))))))))
      (set-buffer old-buf))))

(defun ange-ftp-dcf-2 (result line err
			      file-creator operation fn-list
			      name-constructor
			      target
			      marker-char actual-marker-char
			      buffer to from
			      overwrite
			      overwrite-confirmed
			      overwrite-query 
			      overwrite-backup-query
			      failures skipped success-count
			      total)
  (let ((old-buf (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (if (or err (not result))
	      (progn
		(setq failures (cons (dired-make-relative from) failures))
		(dired-log "%s `%s' to `%s' failed:\n%s\n"
			   operation from to (or err line)))
	    (if overwrite
		;; If we get here, file-creator hasn't been aborted
		;; and the old entry (if any) has to be deleted
		;; before adding the new entry.
		(dired-remove-file to))
	    (setq success-count (1+ success-count))
	    (message "%s: %d of %d" operation success-count total)
	    (dired-add-file to actual-marker-char))
	  
	  (ange-ftp-dcf-1 file-creator operation (cdr fn-list)
			  name-constructor
			  target
			  marker-char
			  buffer
			  overwrite-query 
			  overwrite-backup-query
			  failures skipped success-count
			  total))
      (set-buffer old-buf))))

(defun ange-ftp-dcf-3 (failures operation total skipped success-count
				buffer)
  (let ((old-buf (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (cond
	   (failures
	    (dired-log-summary
	     (message "%s failed for %d of %d file%s %s"
		      operation (length failures) total
		      (dired-plural-s total) failures)))
	   (skipped
	    (dired-log-summary
	     (message "%s: %d of %d file%s skipped %s"
		      operation (length skipped) total
		      (dired-plural-s total) skipped)))
	   (t
	    (message "%s: %s file%s."
		     operation success-count (dired-plural-s success-count))))
	  (dired-move-to-filename))
      (set-buffer old-buf))))

;;;; -----------------------------------------------
;;;; Unix Descriptive Listing (dl) Support
;;;; -----------------------------------------------

(defconst ange-ftp-dired-dl-re-dir
  "^. [^ /]+/[ \n]"
  "Regular expression to use to search for dl directories.")

(or (assq 'unix:dl ange-ftp-dired-re-dir-alist)
    (setq ange-ftp-dired-re-dir-alist
	  (cons (cons 'unix:dl  ange-ftp-dired-dl-re-dir)
		ange-ftp-dired-re-dir-alist)))

(defun ange-ftp-dired-dl-move-to-filename (&optional raise-error eol)
  "In dired, move to the first character of the filename on this line."
  ;; This is the Unix dl version.
  (or eol (setq eol (progn (end-of-line) (point))))
  (let (case-fold-search)
    (beginning-of-line)
    (if (looking-at ". [^ ]+ +\\([0-9]+\\|-\\|=\\) ")
	(goto-char (+ (point) 2))
      (if raise-error
	  (error "No file on this line")
	nil))))

(or (assq 'unix:dl ange-ftp-dired-move-to-filename-alist)
    (setq ange-ftp-dired-move-to-filename-alist
	  (cons '(unix:dl . ange-ftp-dired-dl-move-to-filename)
		ange-ftp-dired-move-to-filename-alist)))

(defun ange-ftp-dired-dl-move-to-end-of-filename (&optional no-error eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the Unix dl version.
  (let ((opoint (point))
	case-fold-search hidden)
    (or eol (setq eol (save-excursion (end-of-line) (point))))
    (setq hidden (and selective-display
		       (save-excursion
			 (search-forward "\r" eol t))))
    (if hidden
	(if no-error
	    nil
	  (error
	   (substitute-command-keys
	    "File line is hidden, type \\[dired-hide-subdir] to unhide")))
      (skip-chars-forward "^ /" eol)
      (if (eq opoint (point))
	  (if no-error
	      nil
	    (error "No file on this line"))
	(point)))))

(or (assq 'unix:dl ange-ftp-dired-move-to-end-of-filename-alist)
    (setq ange-ftp-dired-move-to-end-of-filename-alist
	  (cons '(unix:dl . ange-ftp-dired-dl-move-to-end-of-filename)
		ange-ftp-dired-move-to-end-of-filename-alist)))

;;;; ------------------------------------------------------------
;;;; File name completion support.
;;;; ------------------------------------------------------------

(defun ange-ftp-file-entry-active-p (sym)
  "If the file entry is a symlink, returns whether the file pointed to exists.
Note that DIR is dynamically bound."
  (let ((val (get sym 'val)))
    (or (not (stringp val))
	(file-exists-p (ange-ftp-expand-symlink val dir)))))

(defun ange-ftp-file-entry-not-ignored-p (sym)
  "If the file entry is not a directory (nor a symlink pointing to a directory)
returns whether the file (or file pointed to by the symlink) is ignored
by completion-ignored-extensions.
Note that DIR and COMPLETION-IGNORED-PATTERN are dynamically bound."
  (let ((val (get sym 'val))
	(symname (symbol-name sym)))
    (if (stringp val)
	(let ((file (ange-ftp-expand-symlink val dir)))
	  (or (file-directory-p file)
	      (and (file-exists-p file)
		   (not (string-match completion-ignored-pattern
				      symname)))))
      (or val ; is a directory name
	  (not (string-match completion-ignored-pattern symname))))))

(defun ange-ftp-file-name-all-completions (file dir)
  "Documented as original."
  (setq dir (expand-file-name dir))
  (if (ange-ftp-ftp-path dir)
      (progn
	(ange-ftp-barf-if-not-directory dir)
	(setq dir (ange-ftp-real-file-name-as-directory dir))
	(let* ((tbl (ange-ftp-get-files dir))
	       (completions
		(all-completions file tbl
				 (function ange-ftp-file-entry-active-p))))
	  
	  ;; see whether each matching file is a directory or not...
	  (mapcar
	   (function
	    (lambda (file)
	      (let ((ent (ange-ftp-get-hash-entry file tbl)))
		(if (and ent
			 (or (not (stringp ent))
			     (file-directory-p
			      (ange-ftp-expand-symlink ent dir))))
		    (concat file "/")
		  file))))
	   completions)))
	
    (if (string-equal "/" dir)
	(nconc (all-completions file (ange-ftp-generate-root-prefixes))
	       (ange-ftp-real-file-name-all-completions file dir))
      (ange-ftp-real-file-name-all-completions file dir))))

(defun ange-ftp-file-name-completion (file dir)
  "Documented as original."
  (setq dir (expand-file-name dir))
  (if (ange-ftp-ftp-path dir)
      (progn
	(ange-ftp-barf-if-not-directory dir)
	(if (equal file "")
	    ""
	  (setq dir (ange-ftp-real-file-name-as-directory dir))	;real?
	  (let* ((tbl (ange-ftp-get-files dir))
		 (completion-ignored-pattern
		  (mapconcat (function
			      (lambda (s) (if (stringp s)
					      (concat (regexp-quote s) "$")
					    "/"))) ; / never in filename
			     completion-ignored-extensions
			     "\\|")))
	    (ange-ftp-save-match-data
	      (or (ange-ftp-file-name-completion-1
		   file tbl dir (function ange-ftp-file-entry-not-ignored-p))
		  (ange-ftp-file-name-completion-1
		   file tbl dir (function ange-ftp-file-entry-active-p)))))))
    (if (string-equal "/" dir)
	(try-completion
	 file
	 (nconc (ange-ftp-generate-root-prefixes)
		(mapcar 'list
			(ange-ftp-real-file-name-all-completions file "/"))))
      (ange-ftp-real-file-name-completion file dir))))


(defun ange-ftp-file-name-completion-1 (file tbl dir predicate)
  "Internal subroutine for ange-ftp-file-name-completion.  Do not call this."
  (let ((bestmatch (try-completion file tbl predicate)))
    (if bestmatch
	(if (eq bestmatch t)
	    (if (file-directory-p (expand-file-name file dir))
		(concat file "/")
	      t)
	  (if (and (eq (try-completion bestmatch tbl predicate) t)
		   (file-directory-p
		    (expand-file-name bestmatch dir)))
	      (concat bestmatch "/")
	    bestmatch)))))

(defun ange-ftp-quote-filename (file)
  "Quote `$' as `$$' in FILE to get it past function `substitute-in-file-name.'"
  (let ((pos 0))
    (while (setq pos (string-match "\\$" file pos))
      (setq file (concat (substring file 0 pos)
			 "$";; precede by escape character (also a $)
			 (substring file pos))
	    ;; add 2 instead 1 since another $ was inserted
	    pos (+ 2 pos)))
    file))

(defun ange-ftp-read-file-name-internal (string dir action)
  "Documented as original."
  (let (name realdir)
    (if (eq action 'lambda)
	(if (> (length string) 0)
	    (file-exists-p (substitute-in-file-name string)))
      (if (zerop (length string))
	 (setq name string realdir dir)
       (setq string (substitute-in-file-name string)
	      name (file-name-nondirectory string)
	      realdir (file-name-directory string))
	(setq realdir (if realdir (expand-file-name realdir dir) dir)))
     (if action
	  (file-name-all-completions name realdir)
	(let ((specdir (file-name-directory string))
	      (val (file-name-completion name realdir)))
	  (if (and specdir (stringp val))
	      (ange-ftp-quote-filename (concat specdir val))
	    val))))))

;; Put these lines uncommmented in your .emacs if you want C-r to refresh
;; ange-ftp's cache whilst doing filename completion.
;;
;;(define-key minibuffer-local-completion-map "\C-r" 'ange-ftp-re-read-dir)
;;(define-key minibuffer-local-must-match-map "\C-r" 'ange-ftp-re-read-dir)

(defun ange-ftp-re-read-dir (&optional dir)
  "Forces a re-read of the directory DIR.  If DIR is omitted then it defaults
to the directory part of the contents of the current buffer."
  (interactive)
  (if dir
      (setq dir (expand-file-name dir))
    (setq dir (file-name-directory (expand-file-name (buffer-string)))))
  (if (ange-ftp-ftp-path dir)
      (progn
	(setq ange-ftp-ls-cache-file nil)
	(ange-ftp-del-hash-entry dir ange-ftp-files-hashtable)
	(ange-ftp-get-files dir t))))

;;;; ------------------------------------------------------------
;;;; Bits and bobs to bolt ange-ftp into GNU Emacs.
;;;; ------------------------------------------------------------

(defvar ange-ftp-overwrite-msg
  "Note: This function has been modified to work with ange-ftp.")

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
	 (nfun (symbol-function new))
	 (exec-directory (if (or (equal (nth 3 command-line-args) "dump")
				 (equal (nth 4 command-line-args) "dump"))
			     "../etc/"
			   exec-directory)))			 
    
    ;; *** This is unnecessary for any ange-ftp function (I think):
    (while (symbolp nfun)
      (setq nfun (symbol-function nfun)))
    
    ;; Interpose the ange-ftp function between the function symbol and the
    ;; original definition of the function symbol AT TIME OF FIRST LOAD.
    ;; We must only redefine the symbol-function of FUN the very first
    ;; time, to avoid blowing away stuff that overloads FUN after this.
    
    ;; We direct the function symbol to the ange-ftp's function symbol
    ;; rather than function definition to allow reloading of this file or
    ;; redefining of the individual function (e.g., during debugging)
    ;; later after some other code has been loaded on top of our stuff.
    
    (or (fboundp saved)
	(progn
	  (fset saved (symbol-function fun))
	  (fset fun new)))
    
    ;; Rewrite the doc string on the new ange-ftp function.  This should
    ;; be done every time the file is loaded (or a function is redefined),
    ;; because the underlying overloaded function may have changed its doc
    ;; string.
    
    (let* ((doc-str (ange-ftp-safe-documentation saved))
	   (ndoc-str (concat doc-str (and doc-str "\n")
			     ange-ftp-overwrite-msg)))
      
      (cond ((listp nfun)
	     ;; Probe to test whether function is in preloaded read-only
	     ;; memory, and if so make writable copy:
	     (condition-case nil
		 (setcar nfun (car nfun))
	       (error
		(setq nfun (copy-sequence nfun)) ; shallow copy only
		(fset new nfun)))
	     (let ((ndoc-cdr (nthcdr 2 nfun)))
	       (if (stringp (car ndoc-cdr))
		   ;; Replace the existing docstring.
		   (setcar ndoc-cdr ndoc-str)
		 ;; There is no docstring.  Insert the overwrite msg.
		 (setcdr ndoc-cdr (cons (car ndoc-cdr) (cdr ndoc-cdr)))
		 (setcar ndoc-cdr ange-ftp-overwrite-msg))))
	    (t
	     ;; it's an emacs19 compiled-code object
	     (if (not (fboundp 'compiled-function-arglist))
		 ;; the old way (typical emacs lack of abstraction)
		 (let ((new-code (append nfun nil))) ; turn it into a list
		   (if (nthcdr 4 new-code)
		       (setcar (nthcdr 4 new-code) ndoc-str)
		     (setcdr (nthcdr 3 new-code) (cons ndoc-str nil)))
		   (fset new (apply 'make-byte-code new-code)))
	       ;; the new way (marginally less random) for lemacs 19.8+
	       (fset new
		     (apply 'make-byte-code
			    (compiled-function-arglist nfun)
			    (compiled-function-instructions nfun)
			    (compiled-function-constants nfun)
			    (compiled-function-stack-depth nfun)
			    ndoc-str
			    (if (commandp nfun)
				(list (nth 1 (compiled-function-interactive
					      nfun)))
			      nil)))
	       ))))))

(defun ange-ftp-overwrite-dired ()
  (if (not (fboundp 'dired-ls))		;dired should have been loaded by now
      (ange-ftp-overwrite-fn 'dired-readin) ; classic dired
    (ange-ftp-overwrite-fn 'make-directory) ; tree dired and v19 stuff
    (ange-ftp-overwrite-fn 'remove-directory)
    (ange-ftp-overwrite-fn 'diff)
    (ange-ftp-overwrite-fn 'dired-run-shell-command)
    (ange-ftp-overwrite-fn 'dired-ls)
    (ange-ftp-overwrite-fn 'dired-call-process)
    ;; Can't use (fset 'ange-ftp-dired-readin 'ange-ftp-tree-dired-readin)
    ;; here because it confuses ange-ftp-overwrite-fn.
    (fset 'ange-ftp-dired-readin (symbol-function 'ange-ftp-tree-dired-readin))
    (ange-ftp-overwrite-fn 'dired-readin)
    (ange-ftp-overwrite-fn 'dired-insert-headerline)
    (ange-ftp-overwrite-fn 'dired-move-to-filename)
    (ange-ftp-overwrite-fn 'dired-move-to-end-of-filename)
    (ange-ftp-overwrite-fn 'dired-get-filename)
    (ange-ftp-overwrite-fn 'dired-between-files)
    (ange-ftp-overwrite-fn 'dired-clean-directory)
    (ange-ftp-overwrite-fn 'dired-flag-backup-files)
    (ange-ftp-overwrite-fn 'dired-backup-diff)
    (if (fboundp 'dired-do-create-files)
	;; dired 6.0 or later.
	(progn
	  (ange-ftp-overwrite-fn 'dired-copy-file)
	  (ange-ftp-overwrite-fn 'dired-create-files)
	  (ange-ftp-overwrite-fn 'dired-do-create-files)))
    (if (fboundp 'dired-compress-make-compressed-filename)
	;; it's V5.255 or later
	(ange-ftp-overwrite-fn 'dired-compress-make-compressed-filename)
      ;; ange-ftp-overwrite-fn confuses dired-mark-map here.
      (fset 'ange-ftp-real-dired-compress (symbol-function 'dired-compress))
      (fset 'dired-compress 'ange-ftp-dired-compress)
      (fset 'ange-ftp-real-dired-uncompress (symbol-function 'dired-uncompress))
      (fset 'dired-uncompress 'ange-ftp-dired-uncompress)))

  (ange-ftp-overwrite-fn 'dired-find-file)
  (ange-ftp-overwrite-fn 'dired-revert))

;; Attention!
;; It would be nice if ange-ftp-add-hook was generalized to
;; (defun ange-ftp-add-hook (hook-var hook-function &optional postpend),
;; where the optional postpend variable stipulates that hook-function
;; should be post-pended to the hook-var, rather than prepended.
;; Then, maybe we should overwrite dired with
;; (ange-ftp-add-hook 'dired-load-hook 'ange-ftp-overwrite-dired t).
;; This is because dired-load-hook is commonly used to add the dired extras
;; features (dired-x.el, dired-trns.el, dired-nstd.el, ...). Some of these
;; extras features overwrite functions in dired.el with fancier versions.
;; The "extras" overwrites would then clobber the ange-ftp overwrites.
;; As long as the ange-ftp overwrites are carefully written to use
;; ange-ftp-real-... when the directory is local, then doing the ange-ftp
;; overwrites after the extras overwites should be OK.
;; At the moment, I think that there aren't any conflicts between the extras
;; overwrites, and the ange-ftp overwrites. This may not last though.

(defun ange-ftp-add-hook (hook-var hook-function)
  "Prepend hook-function to hook-var's value, if it is not already an element.
hook-var's value may be a single function or a list of functions."
  (if (boundp hook-var)
      (let ((value (symbol-value hook-var)))
        (if (and (listp value) (not (eq (car value) 'lambda)))
            (and (not (memq hook-function value))
                 (set hook-var
                      (if value (cons hook-function value) hook-function)))
          (and (not (eq hook-function value))
               (set hook-var
                    (list hook-function value)))))
    (set hook-var hook-function)))

;; To load ange-ftp and not dired (leaving it to autoload), define
;; dired-load-hook and make sure dired.el ends with:
;;	(run-hooks 'dired-load-hook)
;;
(if (and (boundp 'dired-load-hook)
	 (not (featurep 'dired)))
    (ange-ftp-add-hook 'dired-load-hook 'ange-ftp-overwrite-dired)
  (require 'dired)
  (ange-ftp-overwrite-dired))

;; In case v19 or emacs-19.el already loaded:
;; (Can't use fboundp to check if emacs-19.el is
;; loaded, because these functions are probably
;; bound to autoloads.)

(if (and (fboundp 'make-directory)
	 (not (and (listp (symbol-function 'make-directory))
		   (eq (car (symbol-function 'make-directory)) 'autoload))))
    (ange-ftp-overwrite-fn 'make-directory))
(if (and (fboundp 'remove-directory)
	 (not (and (listp (symbol-function 'remove-directory))
		   (eq (car (symbol-function 'remove-directory)) 'autoload))))
    (ange-ftp-overwrite-fn 'remove-directory))
(if (and (fboundp 'diff)
	 (not (and (listp (symbol-function 'diff))
		   (eq (car (symbol-function 'diff)) 'autoload))))
      (ange-ftp-overwrite-fn 'diff))

(ange-ftp-overwrite-fn 'insert-file-contents)
(ange-ftp-overwrite-fn 'directory-files)
(ange-ftp-overwrite-fn 'file-directory-p)
(ange-ftp-overwrite-fn 'file-writable-p)
(ange-ftp-overwrite-fn 'file-readable-p)
(ange-ftp-overwrite-fn 'file-symlink-p)
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
;;;; VOS support (VOS support is probably broken,
;;;; but I don't know anything about VOS.)
;;;; ------------------------------------------------------------
;
;(defun ange-ftp-fix-path-for-vos (path &optional reverse)
;  (setq path (copy-sequence path))
;  (let ((from (if reverse ?\> ?\/))
;	(to (if reverse ?\/ ?\>))
;	(i (1- (length path))))
;    (while (>= i 0)
;      (if (= (aref path i) from)
;	  (aset path i to))
;      (setq i (1- i)))
;    path))
;
;(or (assq 'vos ange-ftp-fix-path-func-alist)
;    (setq ange-ftp-fix-path-func-alist
;	  (cons '(vos . ange-ftp-fix-path-for-vos)
;		ange-ftp-fix-path-func-alist)))
;
;(or (memq 'vos ange-ftp-dumb-host-types)
;    (setq ange-ftp-dumb-host-types
;	  (cons 'vos ange-ftp-dumb-host-types)))
;
;(defun ange-ftp-fix-dir-path-for-vos (dir-path)
;  (ange-ftp-fix-path-for-vos
;   (concat dir-path
;	   (if (eq ?/ (aref dir-path (1- (length dir-path))))
;	       "" "/")
;	   "*")))
;
;(or (assq 'vos ange-ftp-fix-dir-path-func-alist)
;    (setq ange-ftp-fix-dir-path-func-alist
;	  (cons '(vos . ange-ftp-fix-dir-path-for-vos)
;		ange-ftp-fix-dir-path-func-alist)))
;
;(defvar ange-ftp-vos-host-regexp nil
;  "If a host matches this regexp then it is assumed to be running VOS.")
;
;(defun ange-ftp-vos-host (host)
;  (and ange-ftp-vos-host-regexp
;       (ange-ftp-save-match-data
;	 (string-match ange-ftp-vos-host-regexp host))))
;
;(defun ange-ftp-parse-vos-listing ()
;  "Parse the current buffer which is assumed to be in VOS list -all
;format, and return a hashtable as the result."
;  (let ((tbl (ange-ftp-make-hashtable))
;	(type-list
;	 '(("^Files: [0-9]+ +Blocks: [0-9]+\n+" nil 40)
;	   ("^Dirs: [0-9]+\n+" t 30)))
;	type-regexp type-is-dir type-col file)
;    (goto-char (point-min))
;    (ange-ftp-save-match-data
;      (while type-list
;	(setq type-regexp (car (car type-list))
;	      type-is-dir (nth 1 (car type-list))
;	      type-col (nth 2 (car type-list))
;	      type-list (cdr type-list))
;	(if (re-search-forward type-regexp nil t)
;	    (while (eq (char-after (point)) ? )
;	      (move-to-column type-col)
;	      (setq file (buffer-substring (point)
;					   (progn
;					     (end-of-line 1)
;					     (point))))
;	      (ange-ftp-put-hash-entry file type-is-dir tbl)
;	      (forward-line 1))))
;      (ange-ftp-put-hash-entry "." 'vosdir tbl)
;      (ange-ftp-put-hash-entry ".." 'vosdir tbl))
;    tbl))
;
;(or (assq 'vos ange-ftp-parse-list-func-alist)
;    (setq ange-ftp-parse-list-func-alist
;	  (cons '(vos . ange-ftp-parse-vos-listing)
;		ange-ftp-parse-list-func-alist)))

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
		  (setq dir
			(substring path (match-beginning 2) (match-end 2))))
	      (if (match-beginning 3)
		  (setq file
			(substring path (match-beginning 3) (match-end 3))))
	      (and dir
		   (setq dir (apply (function concat)
				    (mapcar (function
					     (lambda (char)
					       (if (= char ?.)
						   (vector ?/)
						 (vector char))))
					    (substring dir 1 -1)))))
	      (concat (and drive
			   (concat "/" drive "/"))
		      dir (and dir "/")
		      file))
	  (error "path %s didn't match" path))
      (let (drive dir file tmp)
	(if (string-match "^/[^:]+:/" path)
	    (setq drive (substring path 1
				   (1- (match-end 0)))
		  path (substring path (match-end 0))))
	(setq tmp (file-name-directory path))
	(if tmp
	    (setq dir (apply (function concat)
			     (mapcar (function
				      (lambda (char)
					(if (= char ?/)
					    (vector ?.)
					  (vector char))))
				     (substring tmp 0 -1)))))
	(setq file (file-name-nondirectory path))
	(concat drive
		(and dir (concat "[" (if drive nil ".") dir "]"))
		file)))))

;; (ange-ftp-fix-path-for-vms "/PUB$:/ANONYMOUS/SDSCPUB/NEXT/Readme.txt;1")
;; (ange-ftp-fix-path-for-vms "/PUB$:[ANONYMOUS.SDSCPUB.NEXT]Readme.txt;1" t)

(or (assq 'vms ange-ftp-fix-path-func-alist)
    (setq ange-ftp-fix-path-func-alist
	  (cons '(vms . ange-ftp-fix-path-for-vms)
		ange-ftp-fix-path-func-alist)))

(or (memq 'vms ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'vms ange-ftp-dumb-host-types)))

;; It is important that this function barf for directories for which we know
;; that we cannot possibly get a directory listing, such as "/" and "/DEV:/".
;; This is because it saves an unnecessary FTP error, or possibly the listing
;; might succeed, but give erroneous info. This last case is particularly
;; likely for OS's (like MTS) for which we need to use a wildcard in order
;; to list a directory.

(defun ange-ftp-fix-dir-path-for-vms (dir-path)
  "Convert path from UNIX-ish to VMS ready for a DIRectory listing."
  ;; Should there be entries for .. -> [-] and . -> [] below. Don't
  ;; think so, because expand-filename should have already short-circuited
  ;; them.
  (cond ((string-equal dir-path "/")
	 (error "Cannot get listing for fictitious \"/\" directory."))
	((string-match "^/[-A-Z0-9_$]+:/$" dir-path)
	 (error "Cannot get listing for device."))
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

;; Because some VMS ftp servers convert filenames to lower case
;; we allow a-z in the filename regexp. I'm not too happy about this.

(defconst ange-ftp-vms-filename-regexp
  (concat
   "\\(\\([_A-Za-z0-9$]?\\|[_A-Za-z0-9$][_A-Za-z0-9$---]*\\)\\."
   "[_A-Za-z0-9$---]*;+[0-9]*\\)")
  "Regular expression to match for a valid VMS file name in Dired buffer.
Stupid freaking bug! Position of _ and $ shouldn't matter but they do.
Having [A-Z0-9$_] bombs on filename _$$CHANGE_LOG$.TXT$ and $CHANGE_LOG$.TX
Other orders of $ and _ seem to all work just fine.")

;; These parsing functions are as general as possible because the syntax
;; of ftp listings from VMS hosts is a bit erratic. What saves us is that
;; the VMS filename syntax is so rigid. If they bomb on a listing in the
;; standard VMS Multinet format, then this is a bug. If they bomb on a listing
;; from vms.weird.net, then too bad.

(defun ange-ftp-parse-vms-filename ()
  "Extract the next filename from a VMS dired-like listing."
  (if (re-search-forward
	 ange-ftp-vms-filename-regexp
	 nil t)
	(buffer-substring (match-beginning 0) (match-end 0))))

(defun ange-ftp-parse-vms-listing ()
  "Parse the current buffer which is assumed to be in MultiNet FTP dir
format, and return a hashtable as the result."
  (let ((tbl (ange-ftp-make-hashtable))
	file)
    (goto-char (point-min))
    (ange-ftp-save-match-data
      (while (setq file (ange-ftp-parse-vms-filename))
	(if (string-match "\\.\\(DIR\\|dir\\);[0-9]+" file)
	    ;; deal with directories
	    (ange-ftp-put-hash-entry
	     (substring file 0 (match-beginning 0)) t tbl)
	  (ange-ftp-put-hash-entry file nil tbl)
	  (if (string-match ";[0-9]+$" file) ; deal with extension
	      ;; sans extension
	      (ange-ftp-put-hash-entry
	       (substring file 0 (match-beginning 0)) nil tbl)))
	(forward-line 1))
      ;; Would like to look for a "Total" line, or a "Directory" line to
      ;; make sure that the listing isn't complete garbage before putting
      ;; in "." and "..", but we can't even count on all VAX's giving us
      ;; either of these.
	   (ange-ftp-put-hash-entry "." t tbl)
	   (ange-ftp-put-hash-entry ".." t tbl))
    tbl))

(or (assq 'vms ange-ftp-parse-list-func-alist)
    (setq ange-ftp-parse-list-func-alist
	  (cons '(vms . ange-ftp-parse-vms-listing)
		ange-ftp-parse-list-func-alist)))

;; This version only deletes file entries which have
;; explicit version numbers, because that is all VMS allows.

;; Can the following two functions be speeded up using file
;; completion functions?

(defun ange-ftp-vms-delete-file-entry (path &optional dir-p)
  (if dir-p
      (ange-ftp-internal-delete-file-entry path t)
    (ange-ftp-save-match-data
      (let ((file (ange-ftp-get-file-part path)))
	(if (string-match ";[0-9]+$" file)
	    ;; In VMS you can't delete a file without an explicit	
	    ;; version number, or wild-card (e.g. FOO;*)
	    ;; For now, we give up on wildcards.
	    (let ((files (ange-ftp-get-hash-entry
			  (file-name-directory path)
			  ange-ftp-files-hashtable)))
	      (if files
		  (let* ((root (substring file 0
					  (match-beginning 0)))
			 (regexp (concat "^"
					 (regexp-quote root)
					 ";[0-9]+$"))
			 versions)
		    (ange-ftp-del-hash-entry file files)
		    ;; Now we need to check if there are any
		    ;; versions left. If not, then delete the
		    ;; root entry.
		    (mapatoms
		     '(lambda (sym)
			(and (string-match regexp (get sym 'key))
			     (setq versions t)))
		     files)
		    (or versions
			(ange-ftp-del-hash-entry root files))))))))))

(or (assq 'vms ange-ftp-delete-file-entry-alist)
    (setq ange-ftp-delete-file-entry-alist
	  (cons '(vms . ange-ftp-vms-delete-file-entry)
		ange-ftp-delete-file-entry-alist)))

(defun ange-ftp-vms-add-file-entry (path &optional dir-p)
  (if dir-p
      (ange-ftp-internal-add-file-entry path t)
    (let ((files (ange-ftp-get-hash-entry
		  (file-name-directory path)
		  ange-ftp-files-hashtable)))
      (if files
	  (let ((file (ange-ftp-get-file-part path)))
	    (ange-ftp-save-match-data
	      (if (string-match ";[0-9]+$" file)
		  (ange-ftp-put-hash-entry
		   (substring file 0 (match-beginning 0))
		   nil files)
		;; Need to figure out what version of the file
		;; is being added.
		(let ((regexp (concat "^"
				      (regexp-quote file)
				      ";\\([0-9]+\\)$"))
		      (version 0))
		  (mapatoms
		   '(lambda (sym)
		      (let ((name (get sym 'key)))
			(and (string-match regexp name)
			     (setq version
				   (max version
					(string-to-int
					 (substring name
						    (match-beginning 1)
						    (match-end 1))))))))
		   files)
		  (setq version (1+ version))
		  (ange-ftp-put-hash-entry
		   (concat file ";" (int-to-string version))
		   nil files))))
	    (ange-ftp-put-hash-entry file nil files))))))

(or (assq 'vms ange-ftp-add-file-entry-alist)
    (setq ange-ftp-add-file-entry-alist
	  (cons '(vms . ange-ftp-vms-add-file-entry)
		ange-ftp-add-file-entry-alist)))


(defun ange-ftp-add-vms-host (host)
  "Interactively adds a given HOST to ange-ftp-vms-host-regexp."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name)
				      (and (eq major-mode 'dired-mode)
					   dired-directory))))
			(and name (car (ange-ftp-ftp-path name)))))))
  (if (not (ange-ftp-vms-host host))
      (setq ange-ftp-vms-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-vms-host-regexp "\\|")
		    ange-ftp-vms-host-regexp)
	    ange-ftp-host-cache nil)))


(defun ange-ftp-vms-file-name-as-directory (name)
  (ange-ftp-save-match-data
    (if (string-match "\\.\\(DIR\\|dir\\)\\(;[0-9]+\\)?$" name)
	(setq name (substring name 0 (match-beginning 0))))
    (ange-ftp-real-file-name-as-directory name)))

(or (assq 'vms ange-ftp-file-name-as-directory-alist)
    (setq ange-ftp-file-name-as-directory-alist
	  (cons '(vms . ange-ftp-vms-file-name-as-directory)
		ange-ftp-file-name-as-directory-alist)))

;;; Tree dired support:

;; For this code I have borrowed liberally from Sebastian Kremer's
;; dired-vms.el


;; These regexps must be anchored to beginning of line.
;; Beware that the ftpd may put the device in front of the filename.

(defconst ange-ftp-dired-vms-re-exe "^. [^ \t.]+\\.\\(EXE\\|exe\\)[; ]"
  "Regular expression to use to search for VMS executable files.")

(defconst ange-ftp-dired-vms-re-dir "^. [^ \t.]+\\.\\(DIR\\|dir\\)[; ]"
  "Regular expression to use to search for VMS directories.")

(or (assq 'vms ange-ftp-dired-re-exe-alist)
    (setq ange-ftp-dired-re-exe-alist
	  (cons (cons 'vms  ange-ftp-dired-vms-re-exe)
		ange-ftp-dired-re-exe-alist)))

(or (assq 'vms ange-ftp-dired-re-dir-alist)
    (setq ange-ftp-dired-re-dir-alist
	  (cons (cons 'vms  ange-ftp-dired-vms-re-dir)
		ange-ftp-dired-re-dir-alist)))

(defun ange-ftp-dired-vms-insert-headerline (dir)
  ;; VMS inserts a headerline. I would prefer the headerline
  ;; to be in ange-ftp format. This version tries to
  ;; be careful, because we can't count on a headerline
  ;; over ftp, and we wouldn't want to delete anything
  ;; important.
  (save-excursion
    (if (looking-at "^  wildcard ")
	(forward-line 1))
    (if (looking-at "^[ \n\t]*[^\n]+\\][ \t]*\n")
	(delete-region (point) (match-end 0))))
  (ange-ftp-real-dired-insert-headerline dir))

(or (assq 'vms ange-ftp-dired-insert-headerline-alist)
    (setq ange-ftp-dired-insert-headerline-alist
	  (cons '(vms . ange-ftp-dired-vms-insert-headerline)
		ange-ftp-dired-insert-headerline-alist)))

(defun ange-ftp-dired-vms-move-to-filename (&optional raise-error eol)
  "In dired, move to first char of filename on this line.
Returns position (point) or nil if no filename on this line."
  ;; This is the VMS version.
  (let (case-fold-search)
    (or eol (setq eol (progn (end-of-line) (point))))
    (beginning-of-line)
    (if (re-search-forward ange-ftp-vms-filename-regexp eol t)
	(goto-char (match-beginning 1))
      (if raise-error
	  (error "No file on this line")
	nil))))

(or (assq 'vms ange-ftp-dired-move-to-filename-alist)
    (setq ange-ftp-dired-move-to-filename-alist
	  (cons '(vms . ange-ftp-dired-vms-move-to-filename)
		ange-ftp-dired-move-to-filename-alist)))

(defun ange-ftp-dired-vms-move-to-end-of-filename (&optional no-error eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the VMS version.
  (let (opoint hidden case-fold-search)
    (setq opoint (point))
    (or eol (setq eol (save-excursion (end-of-line) (point))))
    (setq hidden (and selective-display
		      (save-excursion (search-forward "\r" eol t))))
    (if hidden
	nil
      (re-search-forward ange-ftp-vms-filename-regexp eol t))
    (or no-error
	(not (eq opoint (point)))
	(error
	 (if hidden
	     (substitute-command-keys
	      "File line is hidden, type \\[dired-hide-subdir] to unhide")
	   "No file on this line")))
    (if (eq opoint (point))
	nil
      (point))))

(or (assq 'vms ange-ftp-dired-move-to-end-of-filename-alist)
    (setq ange-ftp-dired-move-to-end-of-filename-alist
	  (cons '(vms . ange-ftp-dired-vms-move-to-end-of-filename)
		ange-ftp-dired-move-to-end-of-filename-alist)))

(defun ange-ftp-dired-vms-between-files ()
  (save-excursion
    (beginning-of-line)
    (or (equal (following-char) 10) ; newline
     (equal (following-char) 9)     ; tab
     (progn (forward-char 2)
	    (or (looking-at "Total of")
		(equal (following-char) 32))))))

(or (assq 'vms ange-ftp-dired-between-files-alist)
    (setq ange-ftp-dired-between-files-alist
	  (cons '(vms . ange-ftp-dired-vms-between-files)
		ange-ftp-dired-between-files-alist)))

;; Beware! In VMS filenames must be of the form "FILE.TYPE".
;; Therefore, we cannot just append a ".Z" to filenames for
;; compressed files. Instead, we turn "FILE.TYPE" into
;; "FILE.TYPE-Z". Hope that this is a reasonable thing to do.

(defun ange-ftp-vms-make-compressed-filename (name &optional reverse)
  (if reverse
      (cond
       ((string-match "-Z;[0-9]+$" name)
	(substring name 0 (match-beginning 0)))
       ((string-match ";[0-9]+$" name)
	(substring name 0 (match-beginning 0)))
       ((string-match "-Z$" name)
	(substring name 0 -2))
       (t name))
    (if (string-match ";[0-9]+$" name)
	(concat (substring name 0 (match-beginning 0))
		"-Z")
      (concat name "-Z"))))

(or (assq 'vms ange-ftp-dired-compress-make-compressed-filename-alist)
    (setq ange-ftp-dired-compress-make-compressed-filename-alist
	  (cons '(vms . ange-ftp-vms-make-compressed-filename)
		ange-ftp-dired-compress-make-compressed-filename-alist)))

;; When the filename is too long, VMS will use two lines to list a file
;; (damn them!) This will confuse dired. To solve this, need to convince
;; Sebastian to use a function dired-go-to-end-of-file-line, instead of
;; (forward-line 1). This would require a number of changes to dired.el.
;; If dired gets confused, revert-buffer will fix it.

(defun ange-ftp-dired-vms-ls-trim ()
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (re-search-forward  ange-ftp-vms-filename-regexp))
  (beginning-of-line)
  (delete-region (point-min) (point))
  (forward-line 1)
  (delete-region (point) (point-max)))


(or (assq 'vms ange-ftp-dired-ls-trim-alist)
    (setq ange-ftp-dired-ls-trim-alist
	  (cons '(vms . ange-ftp-dired-vms-ls-trim)
		ange-ftp-dired-ls-trim-alist)))	

(defun ange-ftp-vms-bob-version (name)
  (ange-ftp-save-match-data
    (if (string-match ";[0-9]+$" name)
	(substring name 0 (match-beginning 0))
      name)))

(or (assq 'vms ange-ftp-bob-version-alist)
    (setq ange-ftp-bob-version-alist
	  (cons '(vms . ange-ftp-vms-bob-version)
		ange-ftp-bob-version-alist)))

;;; The vms version of clean-directory has 2 more optional args
;;; than the usual dired version. This is so that it can be used by
;;; ange-ftp-dired-vms-flag-backup-files.

(defun ange-ftp-dired-vms-clean-directory (keep &optional marker msg)
  "Flag numerical backups for deletion.
Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
Positive prefix arg KEEP overrides `dired-kept-versions';
Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

To clear the flags on these files, you can use \\[dired-flag-backup-files]
with a prefix argument."
;  (interactive "P") ; Never actually called interactively.
  (setq keep (max 1 (if keep (prefix-numeric-value keep) dired-kept-versions)))
  (let ((early-retention (if (< keep 0) (- keep) kept-old-versions))
	;; late-retention must NEVER be allowed to be less than 1 in VMS!
	;; This could wipe ALL copies of the file.
	(late-retention (max 1 (if (<= keep 0) dired-kept-versions keep)))
	(action (or msg "Cleaning"))
	(trample-marker (or marker dired-del-marker))
	(file-version-assoc-list ()))
    (message (concat action
		     " numerical backups (keeping %d late, %d old)...")
	     late-retention early-retention)
    ;; Look at each file.
    ;; If the file has numeric backup versions,
    ;; put on file-version-assoc-list an element of the form
    ;; (FILENAME . VERSION-NUMBER-LIST)
    (dired-map-dired-file-lines (function
				 ange-ftp-dired-vms-collect-file-versions))
    ;; Sort each VERSION-NUMBER-LIST,
    ;; and remove the versions not to be deleted.
    (let ((fval file-version-assoc-list))
      (while fval
	(let* ((sorted-v-list (cons 'q (sort (cdr (car fval)) '<)))
	       (v-count (length sorted-v-list)))
	  (if (> v-count (+ early-retention late-retention))
	      (rplacd (nthcdr early-retention sorted-v-list)
		      (nthcdr (- v-count late-retention)
			      sorted-v-list)))
	  (rplacd (car fval)
		  (cdr sorted-v-list)))
	(setq fval (cdr fval))))
    ;; Look at each file.  If it is a numeric backup file,
    ;; find it in a VERSION-NUMBER-LIST and maybe flag it for deletion.
    (dired-map-dired-file-lines
     (function
      ange-ftp-dired-vms-trample-file-versions mark))
    (message (concat action " numerical backups...done"))))

(or (assq 'vms ange-ftp-dired-clean-directory-alist)
    (setq ange-ftp-dired-clean-directory-alist
	  (cons '(vms . ange-ftp-dired-vms-clean-directory)
		ange-ftp-dired-clean-directory-alist)))

(defun ange-ftp-dired-vms-collect-file-versions (fn)
  ;;  "If it looks like file FN has versions, return a list of the versions.
  ;;That is a list of strings which are file names.
  ;;The caller may want to flag some of these files for deletion."
(let ((path (nth 2 (ange-ftp-ftp-path fn))))
  (if (string-match ";[0-9]+$" path)
      (let* ((path (substring path 0 (match-beginning 0)))
	     (fn (ange-ftp-replace-path-component fn path)))
	(if (not (assq fn file-version-assoc-list))
	    (let* ((base-versions
		    (concat (file-name-nondirectory path) ";"))
		   (bv-length (length base-versions))
		   (possibilities (file-name-all-completions
				   base-versions
				   (file-name-directory fn)))
		   (versions (mapcar
			      '(lambda (arg)
				 (if (and (string-match
					   "[0-9]+$" arg bv-length)
					  (= (match-beginning 0) bv-length))
				     (string-to-int (substring arg bv-length))
				   0))
			      possibilities)))
	      (if versions
		  (setq
		   file-version-assoc-list
		   (cons (cons fn versions)
			 file-version-assoc-list)))))))))

(defun ange-ftp-dired-vms-trample-file-versions (fn)
  (let* ((start-vn (string-match ";[0-9]+$" fn))
	 base-version-list)
    (and start-vn
	 (setq base-version-list	; there was a base version to which
	       (assoc (substring fn 0 start-vn)	; this looks like a
		      file-version-assoc-list))	; subversion
	 (not (memq (string-to-int (substring fn (1+ start-vn)))
		    base-version-list))	; this one doesn't make the cut
	 (progn (beginning-of-line)
		(delete-char 1)
		(insert trample-marker)))))

(defun ange-ftp-dired-vms-flag-backup-files (&optional unflag-p)
  (let ((dired-kept-versions 1)
	(kept-old-versions 0)
	marker msg)
    (if unflag-p
	(setq marker ?\040 msg "Unflagging")
      (setq marker dired-del-marker msg "Cleaning"))
    (ange-ftp-dired-vms-clean-directory nil marker msg)))

(or (assq 'vms ange-ftp-dired-flag-backup-files-alist)
    (setq ange-ftp-dired-flag-backup-files-alist
	  (cons '(vms . ange-ftp-dired-vms-flag-backup-files)
		ange-ftp-dired-flag-backup-files-alist)))

(defun ange-ftp-dired-vms-backup-diff (&optional switches)
  (let ((file (dired-get-filename 'no-dir))
	bak)
    (if (and (string-match ";[0-9]+$" file)
	     ;; Find most recent previous version.
	     (let ((root (substring file 0 (match-beginning 0)))
		   (ver
		    (string-to-int (substring file (1+ (match-beginning 0)))))
		   found)
	       (setq ver (1- ver))
	       (while (and (> ver 0) (not found))
		 (setq bak (concat root ";" (int-to-string ver)))
		 (and (file-exists-p bak) (setq found t))
		 (setq ver (1- ver)))
	       found))
	(if switches
	    (diff (expand-file-name bak) (expand-file-name file) switches)
	  (diff (expand-file-name bak) (expand-file-name file)))
      (error "No previous version found for %s" file))))

(or (assq 'vms ange-ftp-dired-backup-diff-alist)
    (setq ange-ftp-dired-backup-diff-alist
	  (cons '(vms . ange-ftp-dired-vms-backup-diff)
		ange-ftp-dired-backup-diff-alist)))


;;;; ------------------------------------------------------------
;;;; MTS support
;;;; ------------------------------------------------------------


(defun ange-ftp-fix-path-for-mts (path &optional reverse)
  "Convert PATH from UNIX-ish to MTS. If REVERSE given then convert from
MTS to UNIX-ish."
  (ange-ftp-save-match-data
    (if reverse
	(if (string-match "^\\([^:]+:\\)?\\(.*\\)$" path)
	    (let (acct file)
	      (if (match-beginning 1)
		  (setq acct (substring path 0 (match-end 1))))
	      (if (match-beginning 2)
		  (setq file (substring path
					(match-beginning 2) (match-end 2))))
	      (concat (and acct (concat "/" acct "/"))
		      file))
	  (error "path %s didn't match" path))
      (if (string-match "^/\\([^:]+:\\)/\\(.*\\)$" path)
	  (concat (substring path 1 (match-end 1))
		  (substring path (match-beginning 2) (match-end 2)))
	;; Let's hope that mts will recognize it anyway.
	path))))

(or (assq 'mts ange-ftp-fix-path-func-alist)
    (setq ange-ftp-fix-path-func-alist
	  (cons '(mts . ange-ftp-fix-path-for-mts)
		ange-ftp-fix-path-func-alist)))

(defun ange-ftp-fix-dir-path-for-mts (dir-path)
  "Convert path from UNIX-ish to MTS ready for a DIRectory listing.
Remember that there are no directories in MTS."
  (if (string-equal dir-path "/")
      (error "Cannot get listing for fictitious \"/\" directory.")
    (let ((dir-path (ange-ftp-fix-path-for-mts dir-path)))
      (cond
       ((string-equal dir-path "")
	"?")
       ((string-match ":$" dir-path)
	(concat dir-path "?"))
       (dir-path))))) ; It's just a single file.

(or (assq 'mts ange-ftp-fix-dir-path-func-alist)
    (setq ange-ftp-fix-dir-path-func-alist
	  (cons '(mts . ange-ftp-fix-dir-path-for-mts)
		ange-ftp-fix-dir-path-func-alist)))

(or (memq 'mts ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'mts ange-ftp-dumb-host-types)))

(defvar ange-ftp-mts-host-regexp nil)

(defun ange-ftp-mts-host (host)
  "Return whether HOST is running MTS."
  (and ange-ftp-mts-host-regexp
       (ange-ftp-save-match-data
	 (string-match ange-ftp-mts-host-regexp host))))

(defun ange-ftp-parse-mts-listing ()
  "Parse the current buffer which is assumed to be in
mts ftp dir format."
  (let ((tbl (ange-ftp-make-hashtable)))
    (goto-char (point-min))
    (ange-ftp-save-match-data
      (while (re-search-forward ange-ftp-date-regexp nil t)
	(end-of-line)
	(skip-chars-backward " ")
	(let ((end (point)))
	  (skip-chars-backward "-A-Z0-9_.!")
	  (ange-ftp-put-hash-entry (buffer-substring (point) end) nil tbl))
	(forward-line 1)))
      ;; Don't need to bother with ..
    (ange-ftp-put-hash-entry "." t tbl)
    tbl))

(or (assq 'mts ange-ftp-parse-list-func-alist)
    (setq ange-ftp-parse-list-func-alist
	  (cons '(mts . ange-ftp-parse-mts-listing)
		ange-ftp-parse-list-func-alist)))

(defun ange-ftp-add-mts-host (host)
  "Interactively adds a given HOST to ange-ftp-mts-host-regexp."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name)
				      (and (eq major-mode 'dired-mode)
					   dired-directory))))
			(and name (car (ange-ftp-ftp-path name)))))))
  (if (not (ange-ftp-mts-host host))
      (setq ange-ftp-mts-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-mts-host-regexp "\\|")
		    ange-ftp-mts-host-regexp)
	    ange-ftp-host-cache nil)))

;;; Tree dired support:

;; There aren't too many systems left that use MTS. This dired support will
;; work for the implementation of ftp on mtsg.ubc.ca. I hope other mts systems
;; implement ftp in the same way. If not, it might be necessary to make the
;; following more flexible.

(defun ange-ftp-dired-mts-move-to-filename (&optional raise-error eol)
  "In dired, move to first char of filename on this line.
Returns position (point) or nil if no filename on this line."
  ;; This is the MTS version.
  (or eol (setq eol (progn (end-of-line) (point))))
  (beginning-of-line)
  (if (re-search-forward
       ange-ftp-date-regexp eol t)
      (progn
	(skip-chars-forward " ")          ; Eat blanks after date
	(skip-chars-forward "0-9:" eol)   ; Eat time or year
	(skip-chars-forward " " eol)      ; one space before filename
	;; When listing an account other than the users own account it appends
	;; ACCT: to the beginning of the filename. Skip over this.
	(and (looking-at "[A-Z0-9_.]+:")
	     (goto-char (match-end 0)))
	(point))
    (if raise-error
	(error "No file on this line")
      nil)))

(or (assq 'mts ange-ftp-dired-move-to-filename-alist)
    (setq ange-ftp-dired-move-to-filename-alist
	  (cons '(mts . ange-ftp-dired-mts-move-to-filename)
		ange-ftp-dired-move-to-filename-alist)))

(defun ange-ftp-dired-mts-move-to-end-of-filename (&optional no-error eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the MTS version.
  (let (opoint hidden case-fold-search)
    (setq opoint (point)
	  eol (save-excursion (end-of-line) (point))
	  hidden (and selective-display
		      (save-excursion (search-forward "\r" eol t))))
    (if hidden
	nil
      (skip-chars-forward "-A-Z0-9._!" eol))
    (or no-error
	(not (eq opoint (point)))
	(error
	 (if hidden
	     (substitute-command-keys
	      "File line is hidden, type \\[dired-hide-subdir] to unhide")
	   "No file on this line")))
    (if (eq opoint (point))
	nil
      (point))))

(or (assq 'mts ange-ftp-dired-move-to-end-of-filename-alist)
    (setq ange-ftp-dired-move-to-end-of-filename-alist
	  (cons '(mts . ange-ftp-dired-mts-move-to-end-of-filename)
		ange-ftp-dired-move-to-end-of-filename-alist)))

;;;; ------------------------------------------------------------
;;;; CMS support
;;;; ------------------------------------------------------------

;; Since CMS doesn't have any full pathname syntax, we have to fudge
;; things with cd's. We actually send too many cd's, but is dangerous
;; to try to remember the current minidisk, because if the connection
;; is closed and needs to be reopened, we will find ourselves back in
;; the default minidisk. This is fairly likely since CMS ftp servers
;; usually close the connection after 5 minutes of inactivity.

;; Have I got the filename character set right?

(defun ange-ftp-fix-path-for-cms (path &optional reverse)
  "Convert PATH from UNIX-ish to CMS. If REVERSE is given, convert
from CMS to UNIX. Actually, CMS doesn't have a full pathname syntax,
so we fudge things by sending cd's."
  (ange-ftp-save-match-data
    (if reverse
	;; Since we only convert output from a pwd in this direction,
	;; we'll assume that it's a minidisk, and make it into a
	;; directory file name. Note that the expand-dir-hashtable
	;; stores directories without the trailing /. Is this
	;; consistent?
	(concat "/" path)
      (if (string-match "^/\\([-A-Z0-9$*._]+\\)/\\([-A-Z0-9$._]+\\)?$"
			path)
	  (let ((minidisk (substring path 1 (match-end 1))))
	    (if (match-beginning 2)
		(let ((file (substring path (match-beginning 2)
				       (match-end 2)))
		      (cmd (concat "cd " minidisk))

		      ;; Note that host and user are bound in the call
		      ;; to ange-ftp-send-cmd
		      (proc (ange-ftp-get-process host user)))

		  ;; Must use ange-ftp-raw-send-cmd here to avoid
		  ;; an infinite loop.
		  (if (car (ange-ftp-raw-send-cmd proc cmd msg))
		      file
		    ;; failed... try ONCE more.
		    (setq proc (ange-ftp-get-process host user))
		    (let ((result (ange-ftp-raw-send-cmd proc cmd msg)))
		      (if (car result)
			  file
			;; failed.  give up.
			(ange-ftp-error host user
					(format "cd to minidisk %s failed: %s"
						minidisk (cdr result)))))))
	      ;; return the minidisk
	      minidisk))
	(error "Invalid CMS filename")))))

(or (assq 'cms ange-ftp-fix-path-func-alist)
    (setq ange-ftp-fix-path-func-alist
	  (cons '(cms . ange-ftp-fix-path-for-cms)
		ange-ftp-fix-path-func-alist)))

(or (memq 'cms ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'cms ange-ftp-dumb-host-types)))

(defun ange-ftp-fix-dir-path-for-cms (dir-path)
  "Convert path from UNIX-ish to VMS ready for a DIRectory listing."
  (cond
   ((string-equal "/" dir-path)
    (error "Cannot get listing for fictitious \"/\" directory."))
   ((string-match "^/\\([-A-Z0-9$*._]+\\)/\\([-A-Z0-9$._]+\\)?$" dir-path)
    (let* ((minidisk (substring dir-path (match-beginning 1) (match-end 1)))
	   ;; host and user are bound in the call to ange-ftp-send-cmd
	   (proc (ange-ftp-get-process host user))
	   (cmd (concat "cd " minidisk))
	   (file (if (match-beginning 2)
		     ;; it's a single file
		     (substring path (match-beginning 2)
				(match-end 2))
		   ;; use the wild-card
		   "*")))
      (if (car (ange-ftp-raw-send-cmd proc cmd))
	  file
	;; try again...
	(setq proc (ange-ftp-get-process host user))
	(let ((result (ange-ftp-raw-send-cmd proc cmd)))
	  (if (car result)
	      file
	    ;; give up
	    (ange-ftp-error host user
			    (format "cd to minidisk %s failed: "
				    minidisk (cdr result))))))))
   (t (error "Invalid CMS pathname"))))

(or (assq 'cms ange-ftp-fix-dir-path-func-alist)
    (setq ange-ftp-fix-dir-path-func-alist
	  (cons '(cms . ange-ftp-fix-dir-path-for-cms)
		ange-ftp-fix-dir-path-func-alist)))

(defvar ange-ftp-cms-host-regexp nil
  "Regular expression to match hosts running the CMS operating system.")

(defun ange-ftp-cms-host (host)
  "Return whether the host is running CMS."
  (and ange-ftp-cms-host-regexp
       (ange-ftp-save-match-data
	 (string-match ange-ftp-cms-host-regexp host))))

(defun ange-ftp-add-cms-host (host)
  "Interactively adds a given HOST to ange-ftp-cms-host-regexp."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name)
				      (and (eq major-mode 'dired-mode)
					   dired-directory))))
			(and name (car (ange-ftp-ftp-path name)))))))
  (if (not (ange-ftp-cms-host host))
      (setq ange-ftp-cms-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-cms-host-regexp "\\|")
		    ange-ftp-cms-host-regexp)
	    ange-ftp-host-cache nil)))

(defun ange-ftp-parse-cms-listing ()
  "Parse the current buffer which is assumed to be a CMS directory listing."
  ;; If we succeed in getting a listing, then we will assume that the minidisk
  ;; exists. file is bound by the call to ange-ftp-ls. This doesn't work
  ;; because ange-ftp doesn't know that the root hashtable has only part of
  ;; the info. It will assume that if a minidisk isn't in it, then it doesn't
  ;; exist. It would be nice if completion worked for minidisks, as we
  ;; discover them.
;  (let* ((dir-file (directory-file-name file))
;	 (root (file-name-directory dir-file))
;	 (minidisk (ange-ftp-get-file-part dir-file))
;	 (root-tbl (ange-ftp-get-hash-entry root ange-ftp-files-hashtable)))
;    (if root-tbl
;	(ange-ftp-put-hash-entry minidisk t root-tbl)
;      (setq root-tbl (ange-ftp-make-hashtable))
;      (ange-ftp-put-hash-entry minidisk t root-tbl)
;      (ange-ftp-put-hash-entry "." t root-tbl)
;      (ange-ftp-set-files root root-tbl)))
  ;; Now do the usual parsing
  (let ((tbl (ange-ftp-make-hashtable)))
    (goto-char (point-min))
    (ange-ftp-save-match-data
      (while
	  (re-search-forward
	   "^\\([-A-Z0-9$_]+\\) +\\([-A-Z0-9$_]+\\) +[VF] +[0-9]+ " nil t)
	(ange-ftp-put-hash-entry
	 (concat (buffer-substring (match-beginning 1)
				   (match-end 1))
		 "."
		 (buffer-substring (match-beginning 2)
				   (match-end 2)))
	 nil tbl)
	(forward-line 1))
      (ange-ftp-put-hash-entry "." t tbl))
    tbl))

(or (assq 'cms ange-ftp-parse-list-func-alist)
    (setq ange-ftp-parse-list-func-alist
	  (cons '(cms . ange-ftp-parse-cms-listing)
		ange-ftp-parse-list-func-alist)))
    
;;; Tree dired support:

(defconst ange-ftp-dired-cms-re-exe
  "^. [-A-Z0-9$_]+ +EXEC "
  "Regular expression to use to search for CMS executables.")

(or (assq 'cms ange-ftp-dired-re-exe-alist)
    (setq ange-ftp-dired-re-exe-alist
	  (cons (cons 'cms  ange-ftp-dired-cms-re-exe)
		ange-ftp-dired-re-exe-alist)))


(defun ange-ftp-dired-cms-insert-headerline (dir)
  ;; CMS has no total line, so we insert a blank line for
  ;; aesthetics.
  (insert "\n")
  (forward-char -1)
  (ange-ftp-real-dired-insert-headerline dir))

(or (assq 'cms ange-ftp-dired-insert-headerline-alist)
    (setq ange-ftp-dired-insert-headerline-alist
	  (cons '(cms . ange-ftp-dired-cms-insert-headerline)
		ange-ftp-dired-insert-headerline-alist)))

(defun ange-ftp-dired-cms-move-to-filename (&optional raise-error eol)
  "In dired, move to the first char of filename on this line."
  ;; This is the CMS version.
  (or eol (setq eol (progn (end-of-line) (point))))
  (let (case-fold-search)
    (beginning-of-line)
    (if (re-search-forward " [-A-Z0-9$_]+ +[-A-Z0-9$_]+ +[VF] +[0-9]+ " eol t)
	(goto-char (1+ (match-beginning 0)))
      (if raise-error
	  (error "No file on this line")
	nil))))

(or (assq 'cms ange-ftp-dired-move-to-filename-alist)
    (setq ange-ftp-dired-move-to-filename-alist
	  (cons '(cms . ange-ftp-dired-cms-move-to-filename)
		ange-ftp-dired-move-to-filename-alist)))

(defun ange-ftp-dired-cms-move-to-end-of-filename (&optional no-error eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; case-fold-search must be nil, at least for VMS.
  ;; On failure, signals an error or returns nil.
  ;; This is the CMS version.
  (let ((opoint (point))
	case-fold-search hidden)
    (or eol (setq eol (save-excursion (end-of-line) (point))))
    (setq hidden (and selective-display
		      (save-excursion
			(search-forward "\r" eol t))))
    (if hidden
	(if no-error
	    nil
	  (error
	   (substitute-command-keys
	    "File line is hidden, type \\[dired-hide-subdir] to unhide")))
      (skip-chars-forward "-A-Z0-9$_" eol)
      (skip-chars-forward " " eol)
      (skip-chars-forward "-A-Z0-9$_" eol)
      (if (eq opoint (point))
	  (if no-error
	      nil
	    (error "No file on this line"))
	(point)))))

(or (assq 'cms ange-ftp-dired-move-to-end-of-filename-alist)
    (setq ange-ftp-dired-move-to-end-of-filename-alist
	  (cons '(cms . ange-ftp-dired-cms-move-to-end-of-filename)
		ange-ftp-dired-move-to-end-of-filename-alist)))

(defun ange-ftp-cms-make-compressed-filename (name &optional reverse)
  (if reverse
      (if (string-match "-Z$" name)
	  (substring name 0 -2)
	name)
    (concat name "-Z")))

(or (assq 'cms ange-ftp-dired-compress-make-compressed-filename-alist)
    (setq ange-ftp-dired-compress-make-compressed-filename-alist
	  (cons '(cms . ange-ftp-cms-make-compressed-filename)
		ange-ftp-dired-compress-make-compressed-filename-alist)))

(defun ange-ftp-dired-cms-get-filename (&optional localp no-error-if-not-filep)
  (let ((name (ange-ftp-real-dired-get-filename localp no-error-if-not-filep)))
    (and name
	 (if (string-match "^\\([^ ]+\\) +\\([^ ]+\\)$" name)
	     (concat (substring name 0 (match-end 1))
		     "."
		     (substring name (match-beginning 2) (match-end 2)))
	   name))))

(or (assq 'cms ange-ftp-dired-get-filename-alist)
    (setq ange-ftp-dired-get-filename-alist
	  (cons '(cms . ange-ftp-dired-cms-get-filename)
		ange-ftp-dired-get-filename-alist)))

;;;; ------------------------------------------------------------
;;;; Finally provide package.
;;;; ------------------------------------------------------------

(provide 'ange-ftp)
