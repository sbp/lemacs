;;; VM user and internal variable initialization
;;; Copyright (C) 1989, 1990, 1991, 1993, 1994 Kyle E. Jones
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
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defvar vm-init-file "~/.vm"
  "*Startup file for VM that is loaded the first time you run VM
in an Emacs session.")

(defvar vm-primary-inbox "~/INBOX"
  "*Mail is moved from the system mailbox to this file for reading.")

(defvar vm-crash-box "~/INBOX.CRASH"
  "*File in which to store mail temporarily while it is transferred from
the system mailbox to the primary inbox.  If a crash occurs
during this mail transfer, any missing mail will be found in this
file.  VM will do crash recovery from this file automatically at
startup, as necessary.")

(defvar vm-keep-crash-boxes nil
  "*Non-nil value should be string specifying a directory where
your crash boxes should be moved after VM has copied new mail
out of them.  This is a safety measure.  In at least one case a
pointer corruption bug inside Emacs has caused VM to believe that
it had copied information out of the crash box when it in fact
had not.  VM then deleted the crash box, losing the batch of
incoming mail.  This is an exceedingly rare problem, but if you
want to avoid losing mail if it happens, set vm-keep-crash-boxes
to point to a directory in the same filesystem as all your
crash boxes.  Each saved crash box will have a unique name based
on the current date and time the box was saved.  You will need to
clean out this directory from time to time; VM does not do so.

A nil value means VM should just delete crash boxes after it
has copied out the mail.")

;; use this function to access vm-spool-files on the fly.  this
;; allows us to use environmental variables without setting
;; vm-spool-files at load time and thereby making it hard to dump an
;; Emacs containing a preloaded VM.
(defun vm-spool-files ()
  (or vm-spool-files
      (and (setq vm-spool-files (getenv "MAILPATH"))
	   (setq vm-spool-files
		 (vm-parse vm-spool-files
			   "\\([^:%?]+\\)\\([%?][^:]*\\)?\\(:\\|$\\)")))
      (and (setq vm-spool-files (getenv "MAIL"))
	   (setq vm-spool-files (list vm-spool-files)))))

(defvar vm-spool-files nil
  "*If non-nil this variable's value should be a list of strings 
or a list of lists.

If the value is a list of strings, the strings should name files
that VM will check for incoming mail instead of the default place
VM thinks your system mailbox is.  Mail will be moved from these
mailboxes to your primary inbox as specified by vm-primary-inbox,
using vm-crash-box as a waystation.

If the value is a list of lists, each sublist should be of the form

    (INBOX SPOOLNAME CRASHBOX)

INBOX, SPOOLNAME and CRASHBOX are all strings.

INBOX is the folder where you want your new mail to be moved when
you type 'g' (running vm-get-new-mail) in VM.  It is where you
will read the mail.

SPOOLNAME is where the mail system leaves your incoming mail,
e.g. /var/spool/mail/kyle.  It can also be a POP maildrop,
provided it can be matched by the value of vm-recognize-pop-maildrops.

    A POP maildrop specification has the following format:

       \"HOST:PORT:AUTH:USER:PASSWORD\"

    HOST is the host name of the POP server
    PORT is the TCP port number to connect to (should normally be 110).
    USER is the user name sent to the server.
    PASSWORD is the secret shared by you and the server for
    authentication purposes.  How is it used depends on the value of
    the AUTH parameter.  If the PASSWORD is \"*\", VM will prompt
    you for the password the first time you try to retrieve mail from
    maildrop.  If the password is valid, VM will not ask you for the
    password again during this Emacs session.

    AUTH is the authentication method used to convince the server you
    should have access to the maildrop.  Acceptable values are
    \"pass\", \"rpop\" and \"apop\".  For \"pass\", the PASSWORD is sent to
    the server with the POP PASS command.  For \"rpop\", the PASSWORD
    should be the string to be sent to the server via the RPOP
    command.  In this case the string is not really a secret;
    authentication is done by other means.  For \"apop\", an MD5 digest
    of the PASSWORD appended to the server timestamp will be sent to
    the server with the APOP command.  In order to use \"apop\" you
    will have to set the value of vm-pop-md5-program appropriately to
    point at the program that will generate the MD5 digest that VM
    needs.

CRASHBOX is the temporary file that VM uses to store mail in
between the SPOOLNAME and the INBOX.  If the system crashes or
Emacs dies while mail is being moved, and the new mail is not in
the SPOOLNAME or the INBOX, then it will be in the CRASHBOX.

There can be multiple entries with the same INBOX value, but a
particular SPOOLNAME should appear only once.  CRASHBOXes should
not be shared among different INBOXes, but you can use the same
CRASHBOX/INBOX pair with a different SPOOLNAME.

NOTE:  The values of vm-primary-inbox and vm-crash-box are ignored
when getting new mail if vm-spool-files is a list of lists.

vm-spool-files will default to the value of the shell
environmental variables MAILPATH or MAIL if either of these
variables are defined and no particular value for vm-spool-files
has been specified.")

(defvar vm-pop-md5-program "md5"
  "*Program that reads a message on its standard input and writes an
MD5 digest on its output.")

(defvar vm-recognize-pop-maildrops "^[^:]+:[^:]+:[^:]+:[^:]+:[^:]+"
  "*Value if non-nil should be a regular expression that matches
spool names found in vm-spool-files that should be considered POP
maildrops.  A nil value tells VM that all the spool names are to
be considered files.")

(defvar vm-auto-get-new-mail t
  "*Non-nil value causes VM to automatically move mail from spool files
to a mail folder when the folder is first visited.  Nil means
you must always use vm-get-new-mail to pull in newly arrived messages.

If the value is a number, then it specifies how often (in
seconds) VM should check for new mail and try to retrieve it.
This is done asynchronously and may occur while you are editing
other files.  It should not disturb your editing, except perhaps
for a pause while the work is being done.")

(defvar vm-default-folder-type 'From_
  "*Default folder type for empty folders.
If VM has to add messages that have no specific folder type to an
empty folder, the folder will become this default type.
Allowed types are:

   From_
   From_-with-Content-Length
   mmdf
   babyl

Value must be a symbol, not a string. i.e. write

  (setq vm-default-folder-type 'From_)

in your .emacs or .vm file.

If you set this variable's value to From_-with-Content-Length you
must set vm-trust-From_-with-Content-Length non-nil.")

(defvar vm-check-folder-types t
  "*Non-nil value causes VM to check folder and message types for
compatibility before it performs certain operations.

Before saving a message to a folder, VM will check that the destination folder
is of the same type as the message to be saved.

Before incorporating message into a visited folder, VM will check that the
messages are of the same type as that folder.

A nil value means don't do the checks.

Depending on the value of vm-convert-folder-types VM will either
convert the messages to the appropriate type before saving or
incorporating them, or it will signal an error.")

(defvar vm-convert-folder-types nil
  "*Non-nil value means that when VM checks folder types and finds
a mismatch (see vm-check-folder-types), it will convert the
source messages to the type of the destination folder, if it can.

If vm-check-folder-types is nil, then this variable isn't
consulted.")

(defvar vm-trust-From_-with-Content-Length nil
  "*Non-nil value means that if the first message in a folder contains
a Content-Length header and begins with \"From \" VM can safely
assume that all messages in the folder have Content-Length headers
that specify the length of the text section of each message.  VM
will then use these headers to determine message boundaries
instead of the usual way of searching for two newlines followed by a
line that begins with \"From \".

If you set vm-default-folder-type to From_-with-Content-Length you
must set this variable non-nil.")

(defvar vm-visible-headers
  '("Resent-"
    "From:" "Sender:"
    "To:" "Apparently-To:" "Cc:"
    "Subject:"
    "Date:")
  "*List of headers that should be visible when VM first displays a message.
These should be listed in the order you wish them presented.
Regular expressions are allowed.  There's no need to anchor
patterns with \"^\", as searches always start at the beginning of
a line.  Put a colon at the end of patterns to get exact matches.
For example, \"Date\" matches \"Date\" and \"Date-Sent\".  Header names
are always matched case insensitively.

If the value of vm-invisible-header-regexp is nil, only the
headers matched by vm-visible-headers will be displayed.
Otherwise all headers are displayed except those matched by
vm-invisible-header-regexp.  In this case vm-visible-headers
specifies the order in which headers are displayed.  Headers not
matching vm-visible-headers are display last.")

;; rmail-ignored-headers is given a sensible default value in loaddefs.el.
(defvar vm-invisible-header-regexp (concat "^X-VM-\\|" rmail-ignored-headers)
  "*Non-nil value should be a regular expression that tells what headers
VM should NOT normally display when presenting a message.  All other
headers will be displayed.  The variable vm-visible-headers specifies
the presentation order of headers; headers not matched by
vm-visible-headers are displayed last.

Nil value causes VM to display ONLY those headers specified in
vm-visible-headers.")

(defvar vm-highlighted-header-regexp nil
  "*Value specifies which headers to highlight.
This is a regular expression that matches the names of headers that should
be highlighted when a message is first presented.  For example setting
this variable to \"From:\\\\|Subject:\" causes the From and Subject
headers to be highlighted.

This does not work under version 18 Emacs.

Under Lucid Emacs the function `highlight-headers' is used to do
highlighting.  See the documentation for this function to find
out how to customize header (and text) highlighting under Lucid
Emacs.  If `vm-highlighted-header-regexp' is non-nil, it
overrides the value of `highlighted-headers-regexp'.  This is so
you can have different header highlighting for VM if you wish.")

(defvar vm-highlighted-header-face 'highlight
  "*Face to be used to highlight headers.
This variable is ignored under Lucid Emacs.
See the documentation for the function `highlight-headers'
to find out how to customize header highlighting under Lucid Emacs.")

(defvar vm-preview-lines 0
  "*Non-nil value N causes VM to display the visible headers + N lines of text
of a message when it is first presented.  The message is not actually
flagged as read until it is exposed in its entirety.

A value of t causes VM to display as much of the message as will
fit in the window associated with the folder buffer.

A nil value causes VM not to preview messages; no text lines are hidden and 
messages are immediately flagged as read.")

(defvar vm-preview-read-messages nil
  "*Non-nil value means to preview messages even if they've already been read.
A nil value causes VM to preview messages only if new or unread.")

(defvar vm-auto-next-message t
  "*Non-nil value causes VM to use vm-next-message to advance to the next
message in the folder if the user attempts to scroll past the end of the
current messages.  A nil value disables this behavior.")

(defvar vm-honor-page-delimiters nil
  "*Non-nil value causes VM to honor page delimiters (as specified by the
Emacs page-delimiter variable) when scrolling through a message.")

(defvar vm-default-window-configuration
  ;; startup = folder on bottom, summary on top
  ;; reading-message = folder on bottom, summary on top
  ;; composing-message = composition on bottom, summary on top
  ;; editing-message = edit on bottom, summary on top
  ;; vm-summarize = folder on bottom, summary on top
  '(
    (startup
     ((((top . 70) (left . 70)))
      (((- (0 0 80 10) (0 10 80 40))
	((nil summary) (nil message))
	((nil nil nil t) (nil nil nil nil))))))
    (quitting
     ((((top . 70) (left . 70)))
      (((0 0 80 40)
	((nil message))
	((nil nil nil t))))))
    (reading-message
     ((((top . 70) (left . 70)))
      (((- (0 0 80 10) (0 10 80 40))
	((nil summary) (nil message))
	((nil nil nil t) (nil nil nil nil))))))
    (composing-message
     ((((top . 70) (left . 70)))
      (((- (0 0 80 10) (0 10 80 40))
	((nil summary) (nil composition))
	((nil nil nil nil) (nil nil nil t))))))
    (editing-message
     ((((top . 70) (left . 70)))
      (((- (0 0 80 10) (0 10 80 40))
	((nil summary) (nil edit))
	((nil nil nil nil) (nil nil nil t))))))
    (vm-summarize
     ((((top . 70) (left . 70)))
      (((- (0 0 80 10) (0 10 80 40))
	((nil summary) (nil message))
	((nil nil nil t) (nil nil nil nil))))))
   )
  "Default window configuration for VM if the user does not specify one.
If you want to completely turn off VM's window configuration
feature, set this variable and vm-window-configuration-file to
nil in your .vm file.

If you want to have a different window configuration setup than
this, you should not set this variable directly.  Rather you
should set the variable vm-window-configuration-file to point at
a file, and use the command vm-save-window-configuration
(normally bound to `WS') to modify part of this configuration to
your liking.

WARNING: Don't point vm-window-configuration-file at your .vm or
.emacs file.  Your window configuration file should start out as
an empty or nonexistent file.  VM will repeatedly overwrite this
file as you update your window configuration settings, so
anything else you put into this file will go away.")

(defvar vm-window-configuration-file "~/.vm.windows"
  "*Non-nil value should be a string that tells VM where to load
and save your window configuration settings.  Your window
configuration settings are loaded automatically the first time
you run VM in an Emacs session, and tells VM how to set up
windows depending on what you are doing inside VM.

The commands vm-save-window-configuration (normally bound to `WS') and
vm-delete-window-configuration (bound to `WD') let you update this
information; see their documentation for more information.

You cannot change your window configuration setup without giving
vm-window-configuration-file a non-nil value.  A nil value causes
VM to use the default window setup specified by the value of
vm-default-window-configuration.

WARNING: Don't point vm-window-configuration-file at your .vm or
.emacs file.  Your window configuration file should start out as
an empty or nonexistent file.  VM will repeatedly overwrite this
file as you update your window configuration settings, so
anything else you put into this file will go away.")

(defvar vm-confirm-quit 0
  "*Value of t causes VM to always ask for confirmation before quitting
a VM visit of a folder.  A nil value means VM will ask only when messages
will be lost unwittingly by quitting, i.e. not removed by intentional
delete and expunge.  A value that is not nil and not t causes VM to ask
only when there are unsaved changes to message attributes, or when messages
will be unwittingly lost.")

(defvar vm-folder-directory nil
  "*Directory where folders of mail are kept.")

(defvar vm-confirm-new-folders nil
  "*Non-nil value causes interactive calls to vm-save-message
to ask for confirmation before creating a new folder.")

(defvar vm-delete-empty-folders t
  "*Non-nil value means remove empty (zero length) folders after saving
A value of t means always remove the folders.
A value of nil means never remove empty folders.
A value that's not t or nil means ask before removing empty folders.")

(defvar vm-flush-interval t
  "*Non-nil value specifies how often VM flushes its cached internal
data.  A numeric value gives the number of seconds between
flushes.  A value of t means flush every time there is a change.
Nil means don't do flushing until a message or folder is saved.

Normally when a message attribute is changed. VM keeps the record
of the change in its internal memory and doesn't insert the
changed data into the folder buffer until a particular message or
the whole folder is saved to disk.  This makes normal Emacs
auto-saving useless for VM folder buffers because the information
you'd want to auto-save, i.e. the attribute changes, isn't in
the buffer when it is auto-saved.

Setting vm-flush-interval to a numeric value will cause the VM's
internal memory caches to be periodically flushed to the folder
buffer.  This is done non-obtrusively, so that if you type
something while flushing is occurring, the flush will abort
cleanly and Emacs will respond to your keystrokes as usual.")

(defvar vm-visit-when-saving 0
  "*Value determines whether VM will visit folders when saving messages.
`Visiting' means that VM will read the folder into Emacs and append the
message to the buffer instead of appending to the folder file directly.
This behavior is ideal when folders are encrypted or compressed since
appending plaintext directly to such folders is a ghastly mistake.

A value of t means VM will always visit folders when saving.

A nil value means VM will never visit folders before saving to them, and
VM will generate an error if you attempt to save messages to a folder
that is being visited.  The latter restriction is necessary to insure
that the buffer and disk copies of the folder being visited remain
consistent.

A value that is not nil and not t means VM will save to a folder's
buffer if that folder is being visited, otherwise VM saves to the folder
file itself.")

(defvar vm-auto-folder-alist nil
  "*Non-nil value should be an alist that VM will use to choose a default
folder name when messages are saved.  The alist should be of the form
\((HEADER-NAME-REGEXP
   (REGEXP . FOLDER-NAME) ...
  ...))
where HEADER-NAME-REGEXP and REGEXP are strings, and FOLDER-NAME
is a string or an s-expression that evaluates to a string.

If any part of the contents of the message header whose name is
matched by HEADER-NAME-REGEXP is matched by the regular
expression REGEXP, VM will evaluate the corresponding FOLDER-NAME
and use the result as the default when prompting for a folder to
save the message in.  If the resulting folder name is a relative
pathname, then it will be rooted in the directory named by
vm-folder-directory, or the default-directory of the currently
visited folder if vm-folder-directory is nil.

When FOLDER-NAME is evaluated, the current buffer will contain
only the contents of the header matched by HEADER-NAME-REGEXP.
It is safe to modify this buffer.  You can use the match data
from any \\( ... \\) grouping constructs in REGEXP along with the
function buffer-substring to build a folder name based on the
header information.  If the result of evaluating FOLDER-NAME is a
list, then the list will be treated as another auto-folder-alist
and will be descended recursively.

Whether REGEXP is matched case sensitively depends on the value
of the variable vm-auto-folder-case-fold-search.  Header names
are always matched case insensitively.")

(defvar vm-auto-folder-case-fold-search nil
  "*Non-nil value means VM will ignore case when matching header
contents while doing automatic folder selection via the variable
vm-auto-folder-alist.")

(defvar vm-virtual-folder-alist nil
  "*Non-nil value should be a list of virtual folder definitions.

A virtual folder is a mapping of messages from one or more real folders
into what appears to be a single folder.  A virtual folder definition
specifies which real folders should be searched for prospective messages
and what the inclusion criteria are.

Each virtual folder definition should have the following form:

  (VIRTUAL-FOLDER-NAME
    ( (FOLDER-NAME ...)
      (SELECTOR [ARG ...]) ... )
    ... )

VIRTUAL-FOLDER-NAME is the name of the virtual folder being defined.
This is the name by which you and VM will refer to this folder.

FOLDER-NAME should be the name of a real folder.  There may be more than
one FOLDER-NAME listed, the SELECTORs within that sublist will apply to
them all.  If FOLDER-NAME is a directory, VM will assume this to mean that
all the folders in that directory should be searched.

The SELECTOR is a Lisp symbol that tells VM how to decide whether a message
from one of the folders specified by the FOLDER-NAMEs should be included
in the virtual folder.  Some SELECTORs require an argument ARG; unless
otherwise noted ARG may be omitted.

The recognized SELECTORs are:

   author          - matches message if ARG matches the author; ARG should be a
                     regular expression.
   and             - matches the message if all its argument
                     selectors match the message.  Example:
                        (and (author \"Derek McGinty\") (new))
                     matches all new messages from Derek McGinty.
                     `and' takes any number of arguments. 
   any             - matches any message.
   deleted         - matches message if it is flagged for deletion.
   edited          - matches message if it has been edited.
   filed           - matched message if it has been saved with its headers.
   forwarded       - matches message if it has been forwarded.
   header          - matches message if ARG matches any part of the header
                     portion of the message; ARG should be a
                     regular expression. 
   label           - matches message if message has a label named ARG.
   less-chars-than - matches message if message has less than ARG
                     characters.  ARG should be a number.
   less-lines-than - matches message if message has less than ARG
                     lines.  ARG should be a number.
   more-chars-than - matches message if message has more than ARG
                     characters.  ARG should be a number.
   more-lines-than - matches message if message has more than ARG
                     lines.  ARG should be a number.
   marked          - matches message if it is marked, as with vm-mark-message.
   new             - matches message if it is new.
   not             - matches message only if its selector argument
                     does NOT match the message.  Example:
                       (not (deleted))
                     matches messages that are not deleted.
   or              - matches the message if any of its argument
                     selectors match the message.  Example:
                        (or (author \"Dave Weckl\") (subject \"drum\"))
                     matches messages from Dave Weckl or messages
                     with the word \"drum\" in their Subject header.
                     `or' takes any number of arguments.
   read            - matches message if it is neither new nor unread.
   recipient       - matches message if ARG matches any part of the recipient
                     list of the message.  ARG should be a regular expression.
   replied         - matches message if it has been replied to.
   sent-after      - matches message if it was sent after the date ARG.
                     A fully specified date looks like this:
                       \"31 Dec 1999 23:59:59 GMT\"
                     although the parts can appear in any order.
                     You can leave out any part and it will
                     default to the current date's value for that
                     part, with the exception of the hh:mm:ss
                     part which defaults to midnight.
   sent-before     - matches message if it was sent before the date ARG.
                     A fully specified date looks like this:
                       \"31 Dec 1999 23:59:59 GMT\"
                     although the parts can appear in any order.
                     You can leave out any part and it will
                     default to the current date's value for that
                     part, with the exception of the hh:mm:ss
                     part which defaults to midnight.
   subject         - matches message if ARG matches any part of the message's
                     subject; ARG should be a regular expression.
   text            - matches message if ARG matches any part of the text
                     portion of the message; ARG should be a
                     regular expression.
   unread          - matches message if it is old but unread.
   written         - matches message if it has been saved without its headers.
")

(defvar vm-virtual-mirror t
  "*Non-nil value causes the attributes of messages in virtual folders
to mirror the changes in the attributes of the underlying real messages.
Similarly, changes in the attributes of virtual messages will change the
attributes of the underlying real messages.  A nil value causes virtual
messages to have their own distinct set of attributes, apart from the
underlying real message.

This variable automatically becomes buffer-local when set in any
fashion.  You should set this variable only in your .vm or .emacs
file.  Use setq-default.  Once VM has been started, you should not
set this variable directly, rather you should use the command
vm-toggle-virtual-mirror, normally bound to `V M'.")
(make-variable-buffer-local 'vm-virtual-mirror)

(defvar vm-folder-read-only nil
  "*Non-nil value causes a folder to be considered unmodifiable by VM.
Commands that modify message attributes or messages themselves are disallowed.
Commands that add or delete messages from the folder are disallowed.
Commands that scan or allow the reading of messages are allowed but the
`new' and `unread' message flags are not changed by them.

This variable automatically becomes buffer-local when set in any
fashion.  You should set this variable only in your .vm or .emacs
file.  Use setq-default.  Once VM has been started, you should not
set this variable directly, rather you should use the command
vm-toggle-read-only, normally bound to C-x C-q.")
(make-variable-buffer-local 'vm-folder-read-only)

(defvar vm-included-text-prefix " > "
  "*String used to prefix included text in replies.")

(defvar vm-keep-sent-messages 1
  "*Non-nil value N causes VM to keep the last N messages sent from within VM.
`Keep' means that VM will not kill the VM mail buffer after you send a message
with C-c C-c (vm-mail-send-and-exit).  A value of 0 or nil causes VM never
to keep such buffers.  A value of t causes VM never to kill such buffers.

Note that these buffers will vanish once you exit Emacs.  To keep a permanent
record of your outgoing mail, use the mail-archive-file-name variable.")

(defvar vm-confirm-mail-send nil
  "*Non-nil means ask before sending a mail message.
This affects vm-mail-send and vm-mail-send-and-exit in Mail mode.")

(defvar vm-mail-header-from nil
  "*Non-nil value should be a string that will be appear as the body
of the From header in outbound mail messages.  A nil value means don't
insert a From header.  This variable also controls the inclusion and
format of the Resent-From header, when resending a message with
vm-resend-message.")

(defvar vm-reply-subject-prefix nil
  "*Non-nil value should be a string that VM should add to the beginning
of the Subject header in replies, if the string is not already present.
Nil means don't prefix the Subject header.")

(defvar vm-reply-ignored-addresses nil
  "*Non-nil value should be a list of regular expressions that match
addresses that VM should automatically remove from the recipient
headers of replies.  These addresses are removed from the headers
before you are placed in the message composition buffer.  So if
you see an address in the header you don't want you should remove
it yourself.

Case is ignored when matching the addresses.")

(defvar vm-reply-ignored-reply-tos nil
  "*Non-nil value should be a list of regular expressions that match
addresses that, if VM finds in a message's Reply-To header, VM
should ignore the Reply-To header and not use it for replies.  VM
will use the From header instead.

Case is ignored when matching the addresses.

This variable exists solely to provide a escape chute from
mailing lists that add a Reply-To: mailing list header, thereby
leaving no way to reply to just the author of a message.")

(defvar vm-in-reply-to-format "%i"
  "*String which specifies the format of the contents of the In-Reply-To
header that is generated for replies.  See the documentation for the
variable vm-summary-format for information on what this string may
contain.  The format should *not* end with a newline.
Nil means don't put an In-Reply-To header in replies.")

(defvar vm-included-text-attribution-format "%F writes:\n"
  "*String which specifies the format of the attribution that precedes the
included text from a message in a reply.  See the documentation for the
variable vm-summary-format for information on what this string may contain.
Nil means don't attribute included text in replies.")

(defvar vm-included-text-headers nil
  "*List of headers that should be retained in a message included in
a reply.  These should be listed in the order you wish them to
appear in the included text.  Regular expressions are allowed.
There's no need to anchor patterns with \"^\", as searches always
start at the beginning of a line.  Put a colon at the end of
patterns to get exact matches.  (E.g. \"Date\" matches \"Date\"
and \"Date-Sent\".)  Header names are always matched case
insensitively.

If the value of vm-included-text-discard-header-regexp is nil,
the headers matched by vm-included-text-headers are the only
headers that will be retained.

If vm-included-text-discard-header-regexp is non-nil, then only
headers matched by that variable will be omitted; all others will
be included.  vm-included-text-headers determines the header
order in that case, with headers not matching any in the
vm-included-text-headers list appearing last in the header
section of the included text.")

(defvar vm-included-text-discard-header-regexp nil
  "*Non-nil value should be a regular expression header that tells
what headers should not be retained in a message included in a
reply.  This variable along with vm-included-text-headers determines
which headers are retained.

If the value of vm-included-text-discard--header-regexp is nil,
the headers matched by vm-included-text-headers are the only headers
that will be retained.

If vm-included-text-discard-header-regexp is non-nil, then only
headers matched by this variable will not be retained; all
others will be included.  vm-included-text-headers determines the
header order in that case, with headers not matching any in
the vm-included-text-headers list appearing last in the header
section of the included text.")

(defvar vm-forwarding-subject-format "forwarded message from %F"
  "*String which specifies the format of the contents of the Subject
header that is generated for a forwarded message.  See the documentation
for the variable vm-summary-format for information on what this string
may contain.  The format should *not* end with nor contain a newline.
Nil means leave the Subject header empty when forwarding.")

(defvar vm-forwarded-headers nil
  "*List of headers that should be forwarded by vm-forward-message.
These should be listed in the order you wish them to appear in
the forwarded message.  Regular expressions are allowed.
There's no need to anchor patterns with \"^\", as searches always
start at the beginning of a line.  Put a colon at the end of
patterns to get exact matches.  (E.g. \"Date\" matches \"Date\"
and \"Date-Sent\".)  Header names are always matched case
insensitively.

If the value of vm-unforwarded-header-regexp is nil, the headers
matched by vm-forwarded-headers are the only headers that will be
forwarded.

If vm-unforwarded-header-regexp is non-nil, then only headers
matched by that variable will be omitted; all others will be
forwarded.  vm-forwarded-headers determines the forwarding order
in that case, with headers not matching any in the
vm-forwarded-headers list appearing last in the header section of
the forwarded message.")

(defvar vm-unforwarded-header-regexp "only-drop-this-header"
  "*Non-nil value should be a regular expression header that tells
what headers should not be forwarded by vm-forward-message.  This
variable along with vm-forwarded-headers determines which headers
are forwarded.

If the value of vm-unforwarded-header-regexp is nil, the headers
matched by vm-forwarded-headers are the only headers that will be
forwarded.

If vm-unforwarded-header-regexp is non-nil, then only headers
matched by this variable will not be forwarded; all others will
be forwarded.  vm-forwarded-headers determines the forwarding
order in that case, with headers not matching any in the
vm-forwarded-headers list appearing last in the header section of
the forwarded message.")

(defvar vm-forwarding-digest-type "rfc934"
  "*Non-nil value should be a string that specifies the type of
message encapsulation format to use when forwarding a message.
Legal values of this variable are:

\"rfc934\"
\"rfc1153\"
nil

A nil value means don't use a digest, just mark the beginning and
end of the forwarded message.")

(defvar vm-digest-preamble-format "\"%s\" (%F)"
  "*String which specifies the format of the preamble lines generated by 
vm-send-digest when it is invoked with a prefix argument.  One
line will be generated for each message put into the digest.  See the
documentation for the variable vm-summary-format for information
on what this string may contain.  The format should *not* end
with nor contain a newline.")

(defvar vm-digest-center-preamble t
  "*Non-nil value means VM will center the preamble lines that precede
the start of a digest.  How the lines will be centered depends on the
ambient value of fill-column.   A nil value suppresses centering.")

(defvar vm-digest-identifier-header-format "X-Digest: %s\n"
  "*Header to insert into messages burst from a digest.
Value should be a format string of the same type as vm-summary-format that describes a header to be inserted into each message burst from a digest.  The format string must end with a newline.")

(defvar vm-digest-burst-type "rfc934"
  "*Value specifies the default digest type offered by vm-burst-digest
when it asks you what type of digest you want to unpack.  Allowed
values of this variable are:

   \"rfc934\"
   \"rfc1153\"
   \"guess\"

If the value is \"guess\", and you take the default
response when vm-burst-digest queries you, VM will try to guess
the digest type.")

(defvar vm-digest-send-type "rfc934"
  "*String that specifies the type of digest vm-send-digest will use.
Legal values of this variable are:

\"rfc934\"
\"rfc1153\"

")

(defvar vm-rfc934-digest-headers
  '("Resent-"
    "From:" "Sender:"
    "To:" "Cc:"
    "Subject:"
    "Date:"
    "Message-ID:"
    "Keywords:")
  "*List of headers that should be appear in RFC 934 digests
created by VM.  These should be listed in the order you wish them
to appear in the digest.  Regular expressions are allowed.
There's no need to anchor patterns with \"^\", as searches always
start at the beginning of a line.  Put a colon at the end of
patterns to get exact matches.  (E.g. \"Date\" matches \"Date\"
and \"Date-Sent\".)  Header names are always matched case
insensitively.

If the value of vm-rfc934-digest-discard-header-regexp is nil, the headers
matched by vm-rfc934-digest-headers are the only headers that will be
kept.

If vm-rfc934-digest-discard-header-regexp is non-nil, then only
headers matched by that variable will be discarded; all others
will be kept.  vm-rfc934-digest-headers determines the order of
appearance in that case, with headers not matching any in the
vm-rfc934-digest-headers list appearing last in the headers
of the digestified messages.")

(defvar vm-rfc934-digest-discard-header-regexp nil
  "*Non-nil value should be a regular expression header that tells
what headers should not appear in RFC 934 digests created by VM.  This
variable along with vm-rfc934-digest-headers determines which headers
are kept and which are discarded.

If the value of vm-rfc934-digest-discard-header-regexp is nil, the headers
matched by vm-rfc934-digest-headers are the only headers that will be
kept.

If vm-rfc934-digest-discard-header-regexp is non-nil, then only
headers matched by this variable will be discarded; all others
will be kept.  vm-rfc934-digest-headers determines the order of
appearance in that case, with headers not matching any in the
vm-rfc934-digest-headers list appearing last in the headers
of the digestified messages.")

(defvar vm-rfc1153-digest-headers
  '("Resent-"
    "Date:"
    "From:" "Sender:"
    "To:" "Cc:"
    "Subject:"
    "Message-ID:"
    "Keywords:")
  "*List of headers that should be appear in RFC 1153 digests
created by VM.  These should be listed in the order you wish them
to appear in the digest.  Regular expressions are allowed.
There is no need to anchor patterns with \"^\", as searches always
start at the beginning of a line.  Put a colon at the end of
patterns to get exact matches.  (E.g. \"Date\" matches \"Date\"
and \"Date-Sent\".)  Header names are always matched case
insensitively.

If the value of vm-rfc1153-digest-discard-header-regexp is nil, the headers
matched by vm-rfc1153-digest-headers are the only headers that will be
kept.

If vm-rfc1153-digest-discard-header-regexp is non-nil, then only
headers matched by that variable will be discarded; all others
will be kept.  vm-rfc1153-digest-headers determines the order of
appearance in that case, with headers not matching any in the
vm-rfc1153-digest-headers list appearing last in the headers of
the digestified messages.")

(defvar vm-rfc1153-digest-discard-header-regexp "\\(X400-\\)?Received:"
  "*Non-nil value should be a regular expression header that tells
what headers should not appear in RFC 1153 digests created by VM.  This
variable along with vm-rfc1153-digest-headers determines which headers
are kept and which headers are discarded.

If the value of vm-rfc1153-digest-discard-header-regexp is nil, the headers
matched by vm-rfc1153-digest-headers are the only headers that will be
kept.

If vm-rfc1153-digest-discard-header-regexp is non-nil, then only
headers matched by this variable will be discarded; all others
will be kept.  vm-rfc1153-digest-headers determines the order of
appearance in that case, with headers not matching any in the
vm-rfc1153-digest-headers list appearing last in the headers of
the digestified messages.")

(defvar vm-resend-bounced-headers
  '("Resent-"
    "From:" "Sender:"
    "To:" "Cc:"
    "Subject:"
    "Newsgroups:"
    "In-Reply-To:" "References:"
    "Keywords:"
    "X-")
  "*List of headers that should be appear in messages resent with
vm-resend-bounced-message.  These should be listed in the order you wish them
to appear in the message.  Regular expressions are allowed.
There is no need to anchor patterns with \"^\", as searches always
start at the beginning of a line.  Put a colon at the end of
patterns to get exact matches.  (E.g. \"Date\" matches \"Date\"
and \"Date-Sent\".)  Header names are always matched case
insensitively.

If the value of vm-resend-bounced-discard-header-regexp is nil, the headers
matched by vm-resend-bounced-headers are the only headers that will be
kept.

If vm-resend-bounced-discard-header-regexp is non-nil, then only
headers matched by that variable will be discarded; all others
will be kept.  vm-resend-bounced-headers determines the order of
appearance in that case, with headers not matching any in the
vm-resend-bounced-headers list appearing last in the headers of
the message.")

(defvar vm-resend-bounced-discard-header-regexp nil
  "*Non-nil value should be a regular expression that tells
what headers should not appear in a resent bounced message.  This
variable along with vm-resend-bounced-headers determines which headers
are kept and which headers are discarded.

If the value of vm-resend-bounced-discard-header-regexp is nil,
the headers matched by vm-resend-bounced-headers are the only
headers that will be kept.

If vm-resend-bounced-discard-header-regexp is non-nil, then only
headers matched by this variable will be discarded; all others
will be kept.  vm-resend-bounced-headers determines the order of
appearance in that case, with headers not matching any in the
vm-resend-bounced-headers list appearing last in the headers of
the message.")

(defvar vm-resend-headers nil
  "*List of headers that should be appear in messages resent with
vm-resend-message.  These should be listed in the order you wish them
to appear in the message.  Regular expressions are allowed.
There is no need to anchor patterns with \"^\", as searches always
start at the beginning of a line.  Put a colon at the end of
patterns to get exact matches.  (E.g. \"Date\" matches \"Date\"
and \"Date-Sent\".)  Header names are always matched case
insensitively.

If the value of vm-resend-discard-header-regexp is nil, the headers
matched by vm-resend-headers are the only headers that will be
kept.

If vm-resend-discard-header-regexp is non-nil, then only
headers matched by that variable will be discarded; all others
will be kept.  vm-resend-headers determines the order of
appearance in that case, with headers not matching any in the
vm-resend-headers list appearing last in the headers of
the message.")

(defvar vm-resend-discard-header-regexp "\\(\\(X400-\\)?Received:\\|Resent-\\)"
  "*Non-nil value should be a regular expression that tells
what headers should not appear in a resent message.  This
variable along with vm-resend-bounced-headers determines which
headers are kept and which headers are discarded.

If the value of vm-resend-discard-header-regexp is nil,
the headers matched by vm-resend--headers are the only
headers that will be kept.

If vm-resend-discard-header-regexp is non-nil, then only
headers matched by this variable will be discarded; all others
will be kept.  vm-resend-headers determines the order of
appearance in that case, with headers not matching any in the
vm-resend-headers list appearing last in the headers of
the message.")

(defvar vm-summary-format "%n %*%a %-17.17F %-3.3m %2d %4l/%-5c %I\"%s\"\n"
  "*String which specifies the message summary line format.
The string may contain the printf-like `%' conversion specifiers which
substitute information about the message into the final summary line.

Recognized specifiers are:
   a - attribute indicators (always four characters wide)
       The first char is  `D', `N', `U' or ` ' for deleted, new, unread
       and read messages respectively.
       The second char is `F', `W' or ` ' for filed (saved) or written
       messages.
       The third char is `R', `Z' or ` ' for messages replied to,
       and forwarded messages.
       The fourth char is `E' if the message has been edited, ` ' otherwise.
   A - longer version of attributes indicators (seven characters wide)
       The first char is  `D', `N', `U' or ` ' for deleted, new, unread
       and read messages respectively.
       The second is `r' or ` ', for message replied to.
       The third is `z' or ` ', for messages forwarded.
       The fourth is `b' or ` ', for messages redistributed.
       The fifth is `f' or ` ', for messages filed.
       The sixth is `w' or ` ', for messages written.
       The seventh is `e' or ` ', for messages that have been edited.
   c - number of characters in message (ignoring headers)
   d - numeric day of month message sent
   f - author's address
   F - author's full name (same as f if full name not found)
   h - hour:min:sec message sent
   H - hour:min message sent
   i - message ID
   I - thread indention
   l - number of lines in message (ignoring headers)
   L - labels (as a comma list)
   m - month message sent
   M - numeric month message sent (January = 1)
   n - message number
   s - message subject
   t - addresses of the recipients of the message, in a comma-separated list
   T - full names of the recipients of the message, in a comma-separated list
       If a full name cannot be found, the corresponding address is used
       instead.
   U - user defined specifier.  The next character in the format
       string should be a letter.  VM will call the function
       vm-summary-function-<letter> (e.g. vm-summary-function-A for
       \"%UA\") in the folder buffer with the message being summarized
       bracketed by (point-min) and (point-max).  The function
       will be passed a message struct as an argument.
       The function should return a string, which VM will insert into
       the summary as it would for information from any other summary
       specifier.
   w - day of the week message sent
   y - year message sent
   z - timezone of date when the message was sent
   * - `*' if the message is marked, ` ' otherwise

Use %% to get a single %.

A numeric field width may be given between the `%' and the specifier;
this causes right justification of the substituted string.  A negative field
width causes left justification.

The field width may be followed by a `.' and a number specifying
the maximum allowed length of the substituted string.  If the
string is longer than this value the right end of the string is
truncated.  If the value is negative, the string is truncated on
on the left instead of the right.

The summary format need not be one line per message but it must end with
a newline, otherwise the message pointer will not be displayed correctly
in the summary window.")

(defvar vm-summary-arrow "->"
  "*String that is displayed to the left of the summary of the
message VM consider to be the current message.  The value takes
effect when the summary buffer is created.  Changing this
variable's value has no effect on existing summary buffers.")

(defvar vm-summary-highlight-face nil
  "*Face to use to highlight the summary entry for the current message.
Nil means don't highlight the current message's summary entry.")

(defvar vm-summary-show-threads nil
  "*Non-nil value means VM should display and maintain
message thread trees in the summary buffer.  This means that
messages with a common ancestor will be displayed contiguously in
the summary.  (If you have vm-move-messages-physically set
non-nil the folder itself will be reordered to match the thread
ordering.)  If you use the `%I' summary format specifier in your
vm-summary-format, indention will be provided as described in the
documentation for vm-summary-thread-indent-level (which see).

A nil value means don't display thread information.  The `%I'
specifier does nothing in the summary format.

This variable automatically becomes buffer-local when set in any
fashion.  You should set this variable only in your .vm or .emacs
file.  Use setq-default.  Once VM has been started, you should not
set this variable directly, rather you should use the command
vm-toggle-threads-display, normally bound to C-t.")
(make-variable-buffer-local 'vm-summary-show-threads)

(defvar vm-summary-thread-indent-level 2
  "*Value should be a number that specifies how much
indention the '%I' summary format specifier should provide per
thread level.  A message's `thread level' refers to the number of
direct ancestors from the message to the oldest ancestor the
message has that is in the current folder.  For example, the
first message of a thread is generally a message about a new
topic, e.g. a message that is not a reply to some other message.
Therefore it has no ancestor and would cause %I to generate no
indention.  A reply to this message will be indented by the value
of vm-summary-thread-indent-level.  A reply to that reply will be
indented twice the value of vm-summary-thread-indent-level.")

(defvar vm-summary-uninteresting-senders nil
  "*Non-nil value should be a regular expressions that matches
addresses that you don't consider interesting enough to
appear in the summary.  When such senders would be displayed by
the %F or %f summary format specifiers VM will substitute the
value of vm-summary-uninteresting-senders-arrow (default \"To:
\") followed by what would be shown by the %T and %t specifiers
respectively.")

(defvar vm-summary-uninteresting-senders-arrow "To: "
  "*String to display before the string that is displayed instead of an
\"uninteresting\" sender.  See vm-summary-uninteresting-senders.")

(defvar vm-auto-center-summary nil
  "*Value controls whether VM will keep the summary arrow vertically
centered within the summary window. A value of t causes VM to always
keep arrow centered.  A value of nil means VM will never bother centering
the arrow.  A value that is not nil and not t causes VM to center the
arrow only if the summary window is not the only existing window.")

(defvar vm-summary-subject-no-newlines t
  "*Non-nil value means VM should replace newlines with spaces in the subject
displayed in the summary.")

(defvar vm-subject-ignored-prefix "^\\(re: *\\)+"
  "*Non-nil value should be a regular expression that matches
strings at the beginning of the Subject header that you want VM to ignore
when threading, sorting, marking, and killing messages by subject.

Matches are done case-insensitively.")

(defvar vm-subject-ignored-suffix "\\( (fwd)\\| \\)+$"
  "*Non-nil value should be a regular expression that matches
strings at the end of the Subject header that you want VM to ignore
when threading, sorting, marking and killing messages by subject.

Matches are done case-insensitively.")

(defvar vm-mutable-windows pop-up-windows
  "*This variable's value controls VM's window usage.

A value of t gives VM free run of the Emacs display; it will commandeer
the entire screen for its purposes.

A value of nil restricts VM's window usage to the window from which
it was invoked.  VM will not create, delete, or use any other windows,
nor will it resize its own window.

A value that is neither t nor nil allows VM to create and delete
windows within the window in which it was started.  That is, VM
will use its root window as if it were the whole screen.")

(defvar vm-mutable-frames nil
  "*Non-nil value means VM is allowed to create and destroy frames
to display and undisplay buffers.

VM can create a frame to display a buffer, and delete frame to
undisplay a buffer.  A nil value means VM should not create or
delete frames.  This does not apply to the VM commands whose
names end in -other-frame, which always create a new frame.

This variable has no meaning if you're not running Emacs native
under X Windows.")

(defvar vm-startup-with-summary nil
  "*Value tells VM whether to generate a summary when a folder is visited.
Nil means don't automatically generate a summary.

A value of t means always generate a summary.

A positive numeric value N means only generate a summary if there
are N or more messages.

A negative numeric value -N means only generate a summary if
there are N or less messages.")

(defvar vm-follow-summary-cursor nil
  "*Non-nil value causes VM to select the message under the cursor in the
summary window before executing commands that operate on the current message.
This occurs only when the summary buffer window is the selected window.")

(defvar vm-jump-to-new-messages t
  "*Non-nil value causes VM to jump to the first new message
whenever such messages arrive in a folder or the first time a
folder is visited.

See also vm-jump-to-unread-messages.")

(defvar vm-jump-to-unread-messages t
  "*Non-nil value causes VM to jump to the first unread message
whenever such messages arrive in a folder or the first time a
folder is visited.  New messages are considered unread in this
context so new messages will be jumped to as well.

The value of vm-jump-to-new-messages takes precedence over the
setting of this variable.  So if there are unread messages and
new messages VM will jump to the first new message, even if an
unread message appears before it in the folder, provided
vm-jump-to-new-messages is non-nil.")

(defvar vm-skip-deleted-messages t
  "*Non-nil value causes VM's `n' and 'p' commands to skip over
deleted messages.  A value of t causes deleted messages to always be skipped.
A value that is not nil and not t causes deleted messages to be skipped only
if there are other messages that are not flagged for deletion in the desired
direction of motion.")

(defvar vm-skip-read-messages nil
  "*Non-nil value causes VM's `n' and `p' commands to skip over
messages that have already been read, in favor of new or unread messages.
A value of t causes read messages to always be skipped.  A value that is
not nil and not t causes read messages to be skipped only if there are
unread messages in the desired direction of motion.")

(defvar vm-move-after-deleting nil
  "*Non-nil value causes VM's `d' command to automatically invoke
vm-next-message or vm-previous-message after deleting, to move
past the deleted messages.  A value of t means motion should
honor the value of vm-circular-folders.  A value that is not t
and not nil means that motion should be done as if
vm-circular-folders is set to nil.")

(defvar vm-move-after-undeleting nil
  "*Non-nil value causes VM's `u' command to automatically invoke
vm-next-message or vm-previous-message after undeleting, to move
past the undeleted messages.  A value of t means motion should
honor the value of vm-circular-folders.  A value that is not t
and not nil means that motion should be done as if
vm-circular-folders is set to nil.")

(defvar vm-delete-after-saving nil
  "*Non-nil value causes VM automatically to mark messages for deletion
after successfully saving them to a folder.")

(defvar vm-delete-after-archiving nil
  "*Non-nil value causes VM automatically to mark messages for deletion
after successfully auto-archiving them with the vm-auto-archive-messages
command.")

(defvar vm-delete-after-bursting nil
  "*Non-nil value causes VM automatically to mark a message for deletion
after it has been successfully burst by the vm-burst-digest command.")

(defvar vm-circular-folders 0
  "*Value determines whether VM folders will be considered circular by
various commands.  `Circular' means VM will wrap from the end of the folder
to the start and vice versa when moving the message pointer, or deleting,
undeleting or saving messages before or after the current message.

A value of t causes all VM commands to consider folders circular.

A value of nil causes all of VM commands to signal an error if the start
or end of the folder would have to be passed to complete the command.
For movement commands, this occurs after the message pointer has been
moved as far as possible in the specified direction.  For other commands,
the error occurs before any part of the command has been executed, i.e.
no deletions, saves, etc. will be done unless they can be done in their
entirety.

A value that is not nil and not t causes only VM's movement commands to
consider folders circular.  Saves, deletes and undelete commands will
behave the same as if the value is nil.")

(defvar vm-mark-search-whole-folder t
  "*Whether marking commands should operate on the whole folder,
rather limiting their search to the messages from the selected message 
onward.")

(defvar vm-search-using-regexps nil
  "*Non-nil value causes VM's search command to interpret user input as a
regular expression instead of as a literal string.")

(defvar vm-move-messages-physically nil
  "*Non-nil value causes VM's commands that change the message order
of a folder to always move the physical messages involved and not
just change the presentation order.  Nil means that commands just
change the order in which VM displays messages and leave the
folder itself undisturbed.")

(defvar vm-edit-message-mode 'text-mode
  "*Major mode to use when editing messages in VM.")

(defvar vm-berkeley-mail-compatibility
  (memq system-type '(berkeley-unix))
  "*Non-nil means to read and write BSD Mail(1) style Status: headers.
This makes sense if you plan to use VM to read mail archives created by
Mail.")

(defvar vm-strip-reply-headers nil
  "*Non-nil value causes VM to strip away all comments and extraneous text
from the headers generated in reply messages.  If you use the \"fakemail\"
program as distributed with Emacs, you probably want to set this variable to
to t, because as of Emacs v18.52 \"fakemail\" could not handle unstripped
headers.")

(defvar vm-inhibit-startup-message nil
  "*Non-nil causes VM not to display its copyright notice, disclaimers
etc. when started in the usual way.")

(defvar vm-select-new-message-hook nil
  "*List of hook functions called every time a message with the 'new'
attribute is made to be the current message.  When the hooks are run the
current buffer will be the folder containing the message and the
start and end of the message will be bracketed by (point-min) and
(point-max).")

(defvar vm-select-unread-message-hook nil
  "*List of hook functions called every time a message with the 'unread'
attribute is made to be the current message.  When the hooks are run the
current buffer will be the folder containing the message and the
start and end of the message will be bracketed by (point-min) and
(point-max).")

(defvar vm-select-message-hook nil
  "*List of hook functions called every time a message
is made to be the current message.  When the hooks are run the
current buffer will be the folder containing the message and the
start and end of the message will be bracketed by (point-min) and
(point-max).")

(defvar vm-arrived-message-hook nil
  "*List of hook functions called once for each message gathered from
the system mail spool, or from another folder with vm-get-new-mail.
When the hooks are run the current buffer will be the folder
containing the message and the start and end of the message will
be bracketed by (point-min) and (point-max).")

(defvar vm-reply-hook nil
  "*List of hook functions to be run after a Mail mode
composition buffer has been created for a reply.  VM runs this
hook and then runs vm-mail-mode-hook before leaving the user in
the Mail mode buffer.")

(defvar vm-forward-message-hook nil
  "*List of hook functions to be run after a Mail mode
composition buffer has been created to forward a message.  VM
runs this hook and then runs vm-mail-mode-hook before leaving the
user in the Mail mode buffer.")

(defvar vm-resend-bounced-message-hook nil
  "*List of hook functions to be run after a Mail mode
composition buffer has been created to resend a bounced message.
VM runs this hook and then runs vm-mail-mode-hook before leaving
the user in the Mail mode buffer.")

(defvar vm-resend-message-hook nil
  "*List of hook functions to be run after a Mail mode
composition buffer has been created to resend a message.
VM runs this hook and then runs vm-mail-mode-hook before leaving
the user in the Mail mode buffer.")

(defvar vm-send-digest-hook nil
  "*List of hook functions to be run after a Mail mode
composition buffer has been created to send a digest.
VM runs this hook and then runs vm-mail-mode-hook before leaving
the user in the Mail mode buffer.")

(defvar vm-mail-hook nil
  "*List of hook functions to be run after a Mail mode
composition buffer has been created to send a non specialized
message, i.e. a message that is not a reply, forward, digest,
etc.  VM runs this hook and then runs vm-mail-mode-hook before
leaving the user in the Mail mode buffer.")

(defvar vm-summary-redo-hook nil
  "*List of hook functions called just after VM adds or deletes
entries from a folder summary.")

(defvar vm-visit-folder-hook nil
  "*List of hook functions called just after VM visits a folder.
It doesn't matter if the folder buffer already exists, this hook
is run each time vm or vm-visit-folder is called interactively It
is NOT run after vm-mode is called.")

(defvar vm-retrieved-spooled-mail-hook nil
  "*List of hook functions called just after VM has retrieved
a group of messages from your system mailbox(es).  When the hooks
are run, the current buffer will be the folder where the messages
were incorporated.")

(defvar vm-edit-message-hook nil
  "*List of hook functions to be run just before a message is edited.
This is the last thing vm-edit-message does before leaving the user
in the edit buffer.")

(defvar vm-mail-mode-hook nil
  "*List of hook functions to be run after a Mail mode
composition buffer has been created.  This is the last thing VM
does before leaving the user in the Mail mode buffer.")

(defvar vm-mode-hook nil
  "*List of hook functions to run when a buffer enters vm-mode.
These hook functions should generally be used to set key bindings
and local variables.")

(defvar vm-mode-hooks nil
  "*Old name for vm-mode-hook.
Supported for backward compatibility.
You should use the new name.")

(defvar vm-summary-mode-hook nil
  "*List of hook functions to run when a VM summary buffer is first
created.  The current buffer will be the summary buffer when the
hooks are run.")

(defvar vm-summary-mode-hooks nil
  "*Old name for vm-summary-mode-hook.
Supported for backward compatibility.
You should use the new name.")

(defvar vm-quit-hook nil
  "*List of hook functions to run when you quit VM.
This applies to any VM quit command.")

(defvar vm-summary-pointer-update-hook nil
  "*List of hook functions to run when VM summary pointer is updated.
When the hooks are run, the current buffer will be the summary buffer.")

(defvar vm-display-buffer-hook nil
  "*List of hook functions that are run every time VM wants to
display a buffer.  When the hooks are run the current buffer will
be the buffer that VM wants to display.  The hooks are expected
to select a window and VM will display the buffer in that
window.

If you use display hooks, you should probably not use VM's
builtin window configuration system as the result is likely to be
confusing.")

(defvar vm-undisplay-buffer-hook nil
  "*List of hook functions that are run every time VM wants to
remove a buffer from the display.  When the hooks are run the
current buffer will be the buffer that VM wants to disappear.
The hooks are expected to do the work of removing the buffer from
the display.  The hook functions should not kill the buffer.

If you use undisplay hooks, you should probably not use VM's
builtin window configuration system as the result is likely to be
confusing.")

(defvar mail-yank-hooks nil
  "Hooks called after a message is yanked into a mail composition.

(This hook is deprecated, you should use mail-citation-hook instead.)

Value is a list of functions to be run.
Each hook function can find the newly yanked message between point and mark.
Each hook function should return with point and mark around the yanked message.

See the documentation for vm-yank-message to see when VM will run
these hooks.")

(defvar mail-citation-hook nil
  "*Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t).
And each hook function should leave point and mark around the citation
text as modified.

If this hook is entirely empty (nil), a default action is taken
instead of no action.")

(defvar mail-default-headers nil
  "*A string containing header lines, to be inserted in outgoing messages.
It is inserted before you edit the message,
so you can edit or delete these lines.")

(defvar mail-signature nil
  "*Text inserted at end of mail buffer when a message is initialized.
If t, it means to insert the contents of the file `~/.signature'.")

(defvar vm-movemail-program "movemail"
  "*Name of program to use to move mail from the system spool
to another location.  Normally this should be the movemail program
distributed with Emacs.")

(defvar vm-tale-is-an-idiot nil
  "*Non-nil value causes vm-mail-send to check multi-line recipient
headers of outbound mail for lines that don't end with a
comma.  If such a line is found, an error is signaled and the
mail is not sent.")

(defvar vm-maintainer-address "bug-vm@uunet.uu.net"
  "Where to send VM bug reports.")

(defvar vm-mode-map
  (let ((map (make-keymap)))

    (cond ((string-match "Lucid" emacs-version)
	   (set-keymap-name map 'vm-mode-map)
	   ))
    (suppress-keymap map)
    (define-key map "h" 'vm-summarize)
    (define-key map "\M-n" 'vm-next-unread-message)
    (define-key map "\M-p" 'vm-previous-unread-message)
    (define-key map "n" 'vm-next-message)
    (define-key map "p" 'vm-previous-message)
    (define-key map "N" 'vm-next-message-no-skip)
    (define-key map "P" 'vm-previous-message-no-skip)
    (define-key map "\C-\M-n" 'vm-move-message-forward)
    (define-key map "\C-\M-p" 'vm-move-message-backward)
    (define-key map "\t" 'vm-goto-message-last-seen)
    (define-key map "\r" 'vm-goto-message)
    (define-key map "^" 'vm-goto-parent-message)
    (define-key map "t" 'vm-expose-hidden-headers)
    (define-key map " " 'vm-scroll-forward)
    (define-key map "b" 'vm-scroll-backward)
    (define-key map "\C-?" 'vm-scroll-backward)
    (define-key map "d" 'vm-delete-message)
    (define-key map "\C-d" 'vm-delete-message-backward)
    (define-key map "u" 'vm-undelete-message)
    (define-key map "U" 'vm-unread-message)
    (define-key map "e" 'vm-edit-message)
    (define-key map "a" 'vm-set-message-attributes)
    (define-key map "j" 'vm-discard-cached-data)
    (define-key map "k" 'vm-kill-subject)
    (define-key map "f" 'vm-followup)
    (define-key map "F" 'vm-followup-include-text)
    (define-key map "r" 'vm-reply)
    (define-key map "R" 'vm-reply-include-text)
    (define-key map "\M-r" 'vm-resend-bounced-message)
    (define-key map "B" 'vm-resend-message)
    (define-key map "z" 'vm-forward-message)
    (define-key map "c" 'vm-continue-composing-message)
    (define-key map "@" 'vm-send-digest)
    (define-key map "*" 'vm-burst-digest)
    (define-key map "m" 'vm-mail)
    (define-key map "g" 'vm-get-new-mail)
    (define-key map "G" 'vm-sort-messages)
    (define-key map "v" 'vm-visit-folder)
    (define-key map "s" 'vm-save-message)
    (define-key map "w" 'vm-save-message-sans-headers)
    (define-key map "A" 'vm-auto-archive-messages)
    (define-key map "S" 'vm-save-folder)
    (define-key map "|" 'vm-pipe-message-to-command)
    (define-key map "#" 'vm-expunge-folder)
    (define-key map "q" 'vm-quit)
    (define-key map "x" 'vm-quit-no-change)
    (define-key map "?" 'vm-help)
    (define-key map "\C-_" 'vm-undo)
    (define-key map "\C-xu" 'vm-undo)
    (define-key map "!" 'shell-command)
    (define-key map "<" 'vm-beginning-of-message)
    (define-key map ">" 'vm-end-of-message)
    (define-key map "\M-s" 'vm-isearch-forward)
    (define-key map "=" 'vm-summarize)
    (define-key map "L" 'vm-load-init-file)
    (define-key map "l" (make-sparse-keymap))
    (define-key map "la" 'vm-add-message-labels)
    (define-key map "ld" 'vm-delete-message-labels)
    (define-key map "V" (make-sparse-keymap))
    (define-key map "VV" 'vm-visit-virtual-folder)
    (define-key map "VC" 'vm-create-virtual-folder)
    (define-key map "VA" 'vm-apply-virtual-folder)
    (define-key map "VM" 'vm-toggle-virtual-mirror)
    (define-key map "V?" 'vm-virtual-help)
    (define-key map "M" (make-sparse-keymap))
    (define-key map "MN" 'vm-next-command-uses-marks)
    (define-key map "Mn" 'vm-next-command-uses-marks)
    (define-key map "MM" 'vm-mark-message) 
    (define-key map "MU" 'vm-unmark-message)
    (define-key map "Mm" 'vm-mark-all-messages)
    (define-key map "Mu" 'vm-clear-all-marks)
    (define-key map "MC" 'vm-mark-matching-messages)
    (define-key map "Mc" 'vm-unmark-matching-messages)
    (define-key map "MT" 'vm-mark-thread-subtree)
    (define-key map "Mt" 'vm-unmark-thread-subtree)
    (define-key map "MS" 'vm-mark-messages-same-subject)
    (define-key map "Ms" 'vm-unmark-messages-same-subject)
    (define-key map "MA" 'vm-mark-messages-same-author)
    (define-key map "Ma" 'vm-unmark-messages-same-author)
    (define-key map "M?" 'vm-mark-help)
    (define-key map "W" (make-sparse-keymap))
    (define-key map "WW" 'vm-apply-window-configuration)
    (define-key map "WS" 'vm-save-window-configuration)
    (define-key map "WD" 'vm-delete-window-configuration)
    (define-key map "W?" 'vm-window-help)
    (define-key map "\C-t" 'vm-toggle-threads-display)
    (define-key map "\C-x\C-s" 'vm-save-buffer)
    (define-key map "\C-x\C-w" 'vm-write-file)
    (define-key map "\C-x\C-q" 'vm-toggle-read-only)
    (define-key map "%" 'vm-change-folder-type)
    (define-key map "\M-C" 'vm-show-copying-restrictions)
    (define-key map "\M-W" 'vm-show-no-warranty)
    map )
  "Keymap for VM mode.")

(defvar vm-summary-mode-map
  (cond ((string-match "Lucid" emacs-version)
	 (let ((map (make-sparse-keymap)))
	   (set-keymap-name map 'vm-summary-mode-map)
	   (set-keymap-parent map vm-mode-map)
	   map))
	(t vm-mode-map))
  "Keymap for VM Summary mode")

(defvar vm-mail-mode-map 
  (let (map)
    (require 'sendmail) ; for mail-mode-map: don't sweat it, it's small.
    ;; (If it does bother you, then move this defvar to vm-reply.el.)
    (cond ((string-match "Lucid" emacs-version)
	   (setq map (make-sparse-keymap))
	   (set-keymap-name map 'vm-mail-mode-map)
	   (set-keymap-parent map mail-mode-map))
	  (t
	   (setq map (copy-keymap mail-mode-map))))

    ;; The one new binding.
    (define-key map "\C-c\C-v" vm-mode-map)

    ;; New functions attached to old keys, with identical semantics.
    (let ((substitutions
	   '((mail-yank-original  vm-yank-message	"\C-c\C-y")
	     (mail-send		  vm-mail-send		"\C-c\C-s")
	     (mail-send-and-exit  vm-mail-send-and-exit	"\C-c\C-c"))))
      (while substitutions
	(let* ((old (nth 0 (car substitutions)))
	       (new (nth 1 (car substitutions)))
	       (def (nth 2 (car substitutions)))
	       (wi (or (where-is-internal old mail-mode-map)
		       (list def))))
	  (while wi
	    (define-key map (car wi) new)
	    (setq wi (cdr wi))))
	(setq substitutions (cdr substitutions))))

;; Inherited from mail-mode-map
;    (define-key map "\C-c\C-w" 'mail-signature)
;    (define-key map "\C-c\C-t" 'mail-text)
;    (define-key map "\C-c\C-q" 'mail-fill-yanked-message)
;    (define-key map "\C-c\C-f\C-t" 'mail-to)
;    (define-key map "\C-c\C-f\C-b" 'mail-bcc)
;    (define-key map "\C-c\C-f\C-s" 'mail-subject)
;    (define-key map "\C-c\C-f\C-c" 'mail-cc)
;    (define-key map "\C-c\C-f\C-f" 'mail-fcc)

    map)
  "Keymap for VM Mail mode buffers.")

(defvar vm-edit-message-map
  (let ((map (make-sparse-keymap)))
    (cond ((string-match "Lucid" emacs-version)
	   (set-keymap-name map 'vm-edit-message-map)
	   (set-keymap-parent map text-mode-map)))
    (define-key map "\C-c\C-v" vm-mode-map)
    (define-key map "\C-c\e" 'vm-edit-message-end)
    (define-key map "\C-c\C-c" 'vm-edit-message-end)
    (define-key map "\C-c\C-]" 'vm-edit-message-abort)
    map)
  "Keymap for the buffers created by VM's vm-edit-message command.")

;; internal vars
(defvar vm-folder-type nil)
(make-variable-buffer-local 'vm-folder-type)
(defvar vm-message-list nil)
(make-variable-buffer-local 'vm-message-list)
(defvar vm-virtual-folder-definition nil)
(make-variable-buffer-local 'vm-virtual-folder-definition)
(defvar vm-virtual-buffers nil)
(make-variable-buffer-local 'vm-virtual-buffers)
(defvar vm-real-buffers nil)
(make-variable-buffer-local 'vm-real-buffers)
(defvar vm-message-pointer nil)
(make-variable-buffer-local 'vm-message-pointer)
(defvar vm-message-order-changed nil)
(make-variable-buffer-local 'vm-message-order-changed)
(defvar vm-message-order-header-present nil)
(make-variable-buffer-local 'vm-message-order-header-present)
(defvar vm-last-message-pointer nil)
(make-variable-buffer-local 'vm-last-message-pointer)
(defvar vm-mail-buffer nil)
(make-variable-buffer-local 'vm-mail-buffer)
(defvar vm-summary-buffer nil)
(make-variable-buffer-local 'vm-summary-buffer)
(defvar vm-summary-pointer nil)
(make-variable-buffer-local 'vm-summary-pointer)
(defvar vm-system-state nil)
(make-variable-buffer-local 'vm-system-state)
(defvar vm-undo-record-list nil)
(make-variable-buffer-local 'vm-undo-record-list)
(defvar vm-saved-undo-record-list nil)
(make-variable-buffer-local 'vm-saved-undo-record-list)
(defvar vm-undo-record-pointer nil)
(make-variable-buffer-local 'vm-undo-record-pointer)
(defvar vm-last-save-folder nil)
(make-variable-buffer-local 'vm-last-save-folder)
(defvar vm-last-written-file nil)
(make-variable-buffer-local 'vm-last-written-file)
(defvar vm-last-visit-folder nil)
(defvar vm-last-pipe-command nil)
(make-variable-buffer-local 'vm-last-pipe-command)
(defvar vm-messages-not-on-disk 0)
(make-variable-buffer-local 'vm-messages-not-on-disk)
(defvar vm-totals nil)
(make-variable-buffer-local 'vm-totals)
(defvar vm-modification-counter 0)
(make-variable-buffer-local 'vm-modification-counter)
(defvar vm-flushed-modification-counter nil)
(make-variable-buffer-local 'vm-flushed-modification-counter)
(defvar vm-messages-needing-summary-update nil)
(defvar vm-buffers-needing-display-update nil)
(defvar vm-numbering-redo-start-point nil)
(make-variable-buffer-local 'vm-numbering-redo-start-point)
(defvar vm-numbering-redo-end-point nil)
(make-variable-buffer-local 'vm-numbering-redo-end-point)
(defvar vm-summary-redo-start-point nil)
(make-variable-buffer-local 'vm-summary-redo-start-point)
(defvar vm-need-summary-pointer-update nil)
(make-variable-buffer-local 'vm-need-summary-pointer-update)
(defvar vm-thread-obarray nil)
(make-variable-buffer-local 'vm-thread-obarray)
(defvar vm-thread-subject-obarray nil)
(make-variable-buffer-local 'vm-thread-subject-obarray)
(defvar vm-label-obarray nil)
(make-variable-buffer-local 'vm-label-obarray)
(defvar vm-block-new-mail nil)
(make-variable-buffer-local 'vm-block-new-mail)
(defvar vm-root-window-edges nil)
(make-variable-buffer-local 'vm-root-window-edges)
(defvar vm-saved-buffer-modified-p nil)
(make-variable-buffer-local 'vm-saved-buffer-modified-p)
(defvar vm-mark-count 0)
(make-variable-buffer-local 'vm-mark-count)
(defvar vm-kept-mail-buffers nil)
(defvar vm-inhibit-write-file-hook nil)
(defvar vm-chop-full-name-function 'vm-choose-chop-full-name-function)
(defvar vm-session-beginning t)
(defvar vm-init-file-loaded nil)
(defvar vm-window-configurations nil)
(defvar vm-window-configuration nil)
(defvar vm-message-id-number 0)
(defconst vm-spool-directory
  (or (and (boundp 'rmail-spool-directory) rmail-spool-directory)
      "/usr/spool/mail/"))
(defconst vm-content-length-search-regexp "^Content-Length:.*\n\\|\\(\n\n\\)")
(defconst vm-content-length-header "Content-Length:")
(defconst vm-attributes-header-regexp
  "^X-VM-\\(Attributes\\|v5-Data\\):\\(.*\n\\([ \t].*\n\\)*\\)")
(defconst vm-attributes-header "X-VM-v5-Data:")
(defconst vm-message-order-header-regexp "^X-VM-Message-Order:")
(defconst vm-message-order-header "X-VM-Message-Order:")
(defconst vm-bookmark-header-regexp "^X-VM-Bookmark:")
(defconst vm-bookmark-header "X-VM-Bookmark:")
(defconst vm-summary-header-regexp "^X-VM-Summary-Format:")
(defconst vm-summary-header "X-VM-Summary-Format:")
(defconst vm-vheader-header-regexp "^X-VM-VHeader:")
(defconst vm-vheader-header "X-VM-VHeader:")
(defconst vm-labels-header-regexp "^X-VM-Labels:")
(defconst vm-labels-header "X-VM-Labels:")
(defconst vm-berkeley-mail-status-header "Status: ")
(defconst vm-berkeley-mail-status-header-regexp "^Status: \\(..?\\)\n")
(defvar vm-matched-header-vector (make-vector 6 nil))
(defconst vm-supported-folder-types
  '("From_" "From_-with-Content-Length" "mmdf" "babyl"))
(defconst vm-supported-window-configurations
  '(("default")
    ("startup")
    ("quitting")
    ("composing-message")
    ("editing-message")
    ("marking-message")
    ("reading-message")
    ("searching-message")
    ("vm-delete-message")
    ("vm-delete-message-backward")
    ("vm-undelete-message")
    ("vm-kill-subject")
    ("vm-expunge-folder")
    ("vm-burst-digest")
    ("vm-burst-rfc934-digest")
    ("vm-burst-rfc1153-digest")
    ("vm-edit-message")
    ("vm-discard-cached-data")
    ("vm-edit-message-end")
    ("vm-edit-message-abort")
    ("vm-unread-message")
    ("vm-quit-no-change")
    ("vm-quit")
    ("vm-save-buffer")
    ("vm-write-file")
    ("vm-save-folder")
    ("vm-save-and-expunge-folder")
    ("vm-visit-folder")
    ("vm-visit-folder-other-frame")
    ("vm-visit-folder-other-window")
    ("vm-help")
    ("vm-get-new-mail")
    ("vm-load-init-file")
    ("vm")
    ("vm-other-frame")
    ("vm-other-window")
    ("vm-toggle-read-only")
    ("vm-mode")
    ("vm-show-copying-restrictions")
    ("vm-show-no-warranty")
    ("vm-clear-all-marks")
    ("vm-mark-all-messages")
    ("vm-mark-message")
    ("vm-unmark-message")
    ("vm-mark-messages-same-subject")
    ("vm-unmark-messages-same-subject")
    ("vm-mark-matching-messages")
    ("vm-unmark-matching-messages")
    ("vm-mark-thread-subtree")
    ("vm-unmark-thread-subtree")
    ("vm-next-command-uses-marks")
    ("vm-mark-help")
    ("vm-submit-bug-report")
    ("vm-goto-message")
    ("vm-goto-message-last-seen")
    ("vm-next-message")
    ("vm-previous-message")
    ("vm-next-message-no-skip")
    ("vm-previous-message-no-skip")
    ("vm-next-unread-message")
    ("vm-previous-unread-message")
    ("vm-scroll-forward")
    ("vm-scroll-backward")
    ("vm-expose-hidden-headers")
    ("vm-beginning-of-message")
    ("vm-end-of-message")
    ("vm-yank-message-other-folder")
    ("vm-yank-message")
    ("vm-mail-send-and-exit")
    ("vm-mail-send")
    ("vm-reply")
    ("vm-reply-include-text")
    ("vm-followup")
    ("vm-followup-include-text")
    ("vm-forward-message")
    ("vm-forward-message-all-headers")
    ("vm-mail")
    ("vm-resend-bounced-message")
    ("vm-resend-message")
    ("vm-send-digest")
    ("vm-send-rfc934-digest")
    ("vm-send-rfc1153-digest")
    ("vm-reply-other-frame")
    ("vm-reply-include-text-other-frame")
    ("vm-followup-other-frame")
    ("vm-followup-include-text-other-frame")
    ("vm-forward-message-other-frame")
    ("vm-forward-message-all-headers-other-frame")
    ("vm-mail-other-frame")
    ("vm-mail-other-window")
    ("vm-resend-bounced-message-other-frame")
    ("vm-resend-message-other-frame")
    ("vm-send-digest-other-frame")
    ("vm-send-rfc934-digest-other-frame")
    ("vm-send-rfc1153-digest-other-frame")
    ("vm-continue-composing-message")
    ("vm-auto-archive-messages")
    ("vm-save-message")
    ("vm-save-message-sans-headers")
    ("vm-pipe-message-to-command")
    ("vm-isearch-forward")
    ("vm-move-message-forward")
    ("vm-move-message-backward")
    ("vm-move-message-forward-physically")
    ("vm-move-message-backward-physically")
    ("vm-sort-messages")
    ("vm-toggle-threads-display")
    ("vm-summarize")
    ("vm-summarize-other-frame")
    ("vm-undo")
    ("vm-visit-virtual-folder")
    ("vm-visit-virtual-folder-other-frame")
    ("vm-visit-virtual-folder-other-window")))
(defconst vm-supported-sort-keys
  '("date" "reversed-date"
    "author" "reversed-author"
    "subject" "reversed-subject"
    "recipients" "reversed-recipients"
    "line-count" "reversed-line-count"
    "byte-count" "reversed-byte-count"
    "physical-order" "reversed-physical-order"))
(defconst vm-supported-interactive-virtual-selectors
  '(("any")
    ("header")
    ("label")
    ("text")
    ("recipient")
    ("author")
    ("subject")
    ("sent-before")
    ("sent-after")
    ("more-chars-than")
    ("less-chars-than")
    ("more-lines-than")
    ("less-lines-than")
    ("new")
    ("unread")
    ("read")
    ("deleted")
    ("replied")
    ("forwarded")
    ("filed")
    ("written")
    ("edited")
    ("marked")))
(defconst vm-supported-attribute-names
  '("new"
    "unread"
    "read"
    "deleted"
    "replied"
    "forwarded"
    "redistributed"
    "filed"
    "written"
    "edited"
    "undeleted"
    "unreplied"
    "unforwarded"
    "unredistributed"
    "unfiled"
    "unwritten"
    "unedited"
    ;; for babyl cogniscenti
    "recent"
    "unseen"
    "answered"
    "unanswered"))

(defvar vm-key-functions nil)
(defconst vm-digest-type-alist '(("rfc934") ("rfc1153")))
(defvar vm-completion-auto-correct t
  "Non-nil means that minibuffer-complete-file should aggressively erase
the trailing part of a word that caused completion to fail, and retry
the completion with the resulting word.")
(defvar vm-minibuffer-completion-table nil
  "Completion table used by vm-minibuffer-complete-word.
Should be just a list of strings, not an alist or an obarray.")
(defvar vm-completion-auto-space t
  "Non-nil value means that vm-minibuffer-complete-word should automatically
append a space to words that complete unambiguously.")
(defconst vm-attributes-vector-length 9)
(defconst vm-cache-vector-length 20)
(defconst vm-softdata-vector-length 16)
(defconst vm-location-data-vector-length 6)
(defconst vm-mirror-data-vector-length 5)
(defconst vm-startup-message-lines
  '("Please use \\[vm-submit-bug-report] to report bugs."
    "You may give out copies of VM.  Type \\[vm-show-copying-restrictions] to see the conditions"
    "VM comes with ABSOLUTELY NO WARRANTY; type \\[vm-show-no-warranty] for full details"
    "In Stereo (where available)"))
;    '("Please use \\[vm-submit-bug-report] to report bugs."
;      "You may give out copies of VM.  Type \\[vm-show-copying-restrictions] to see the conditions"
;      "VM comes with ABSOLUTELY NO WARRANTY; type \\[vm-show-no-warranty] for full details"
;      "In Stereo (where available)"))
(defconst vm-startup-message-displayed nil)
;; for the mode line
;; Hey Kyle, why so random?  Everyone else uses dashes.
(defvar vm-mode-line-format
  '("" "--%+%+-"
    ("VM: %b"
     (vm-mail-buffer (vm-ml-sort-keys ("" " by " vm-ml-sort-keys)))
     (vm-message-list
      ("   " vm-ml-message-number
       " (of " vm-ml-highest-message-number ")")
      (vm-folder-type
       "   (unrecognized folder type)"
       "   (no messages)")))
    (vm-message-list
     ("--%[" vm-ml-message-attributes-alist
      (vm-ml-labels ("; " vm-ml-labels)) "%]---")
     ("--%[%]---"))
    "%p" "   " global-mode-string "-%-"))

(defvar vm-ml-message-attributes-alist
  '((vm-ml-message-new
     "new"
     (vm-ml-message-unread
      "unread"
      (vm-ml-message-read "read")))
    (vm-ml-message-edited " edited")
    (vm-ml-message-filed " filed")
    (vm-ml-message-written " written")
    (vm-ml-message-replied " replied")
    (vm-ml-message-forwarded " forwarded")
    (vm-ml-message-redistributed " redistributed")
    (vm-ml-message-deleted " deleted")
    (vm-ml-message-marked " MARKED")))
(defvar vm-ml-message-number nil)
(make-variable-buffer-local 'vm-ml-message-number)
(defvar vm-ml-highest-message-number nil)
(make-variable-buffer-local 'vm-ml-highest-message-number)
(defvar vm-ml-sort-keys nil)
(make-variable-buffer-local 'vm-ml-sort-keys)
(defvar vm-ml-labels nil)
(make-variable-buffer-local 'vm-ml-labels)
; unused now
;(defvar vm-ml-attributes-string nil)
;(make-variable-buffer-local 'vm-ml-attributes-string)
(defvar vm-summary-overlay nil)
(make-variable-buffer-local 'vm-summary-overlay)
(defvar vm-ml-message-new nil)
(make-variable-buffer-local 'vm-ml-message-new)
(defvar vm-ml-message-unread nil)
(make-variable-buffer-local 'vm-ml-message-unread)
(defvar vm-ml-message-read nil)
(make-variable-buffer-local 'vm-ml-message-read)
(defvar vm-ml-message-edited nil)
(make-variable-buffer-local 'vm-ml-message-edited)
(defvar vm-ml-message-replied nil)
(make-variable-buffer-local 'vm-ml-message-replied)
(defvar vm-ml-message-forwarded nil)
(make-variable-buffer-local 'vm-ml-message-forwarded)
(defvar vm-ml-message-redistributed nil)
(make-variable-buffer-local 'vm-ml-message-redistributed)
(defvar vm-ml-message-deleted nil)
(make-variable-buffer-local 'vm-ml-message-deleted)
(defvar vm-ml-message-filed nil)
(make-variable-buffer-local 'vm-ml-message-filed)
(defvar vm-ml-message-written nil)
(make-variable-buffer-local 'vm-ml-message-written)
(defvar vm-ml-message-marked nil)
(make-variable-buffer-local 'vm-ml-message-marked)
;; to make the tanjed compiler shut up
(defvar vm-pop-read-point nil)
(defvar vm-reply-list nil)
(defvar vm-forward-list nil)
(defvar vm-redistribute-list nil)
(defvar current-itimer nil)
(defconst vm-month-alist
  '(("jan" "January" "1")
    ("feb" "February" "2")
    ("mar" "March" "3")
    ("apr" "April" "4")
    ("may" "May" "5")
    ("jun" "June" "6")
    ("jul" "July" "7")
    ("aug" "August" "8")
    ("sep" "September" "9")
    ("oct" "October" "10")
    ("nov" "November" "11")
    ("dec" "December" "12")))
(defvar vm-pop-passwords nil)
(defvar pop-up-frames nil)
(defvar vm-parse-date-workspace (make-vector 6 nil))
;; cache so we don't call timezone-make-date-sortable so much.
;; messages have their own cache; this is for the virtual folder
;; alist selectors.
(defvar vm-sortable-date-alist nil)
(defvar vm-summary-=> nil)
(defvar vm-summary-no-=> nil)

(defvar vm-thread-loop-obarray (make-vector 29 0))
(defvar vm-delete-duplicates-obarray (make-vector 29 0))

;; must be after the variables, so this can't go at the end of vm-startup.el
(if (string-match "Lucid" emacs-version)
    (require 'vm-lucid))
