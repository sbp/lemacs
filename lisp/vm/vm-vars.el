;;; VM user and internal variable initialization
;;; Copyright (C) 1989, 1990, 1991 Kyle E. Jones
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

;; use this function to access vm-spool-files on the fly this
;; allows us to use environmental variables without setting
;; vm-spool-files at load time and making it hard to dump an
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
    the AUTH parameter.

    AUTH is the authentication method used to convince the server you
    should have access to the maildrop.  Acceptable values are
    \"pass\", \"rpop\" and \"apop\".  For \"pass\", the PASSWORD is sent to
    the server with the POP PASS command.  For \"rpop\", the PASSWORD
    should be the string to be sent to the server via the RPOP
    command.  In this case the stirng is not really a secret
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

(defvar vm-pop-md5-program "mddriver"
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

If the value is a number, then it specifies how often VM should
check for new mail and try to retrieve it.  This is done
asynchronously and may occur while you are editing other files.  It
should not disturb your editing, except perhaps for a pause while
the work is being done.")

(defvar vm-default-folder-type 'From_
  "*Default folder type for empty folders.
If VM has to add messages that have no specific folder type to an
empty folder, the folder will become this default type.
Allowed types are:

   From_
   mmdf

Value must be a symbol, not a string.")

(defvar vm-check-folder-types nil
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
source messages to the type of the desintation folder, if it can.")

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
\(E.g. \"Date\" matches \"Date\" and \"Date-Sent\".)  Header names
are always matched case insensitively.

If the value of vm-invisible-header-regexp is nil, only the
headers matched by vm-visible-headers will be displayed.
Otherwise all headers are displayed except those matched by
vm-invisible-header-regexp.  In this case vm-visible-headers
specifies the order in which headers are displayed.  Headers not
matching vm-visible-headers are display last.")

;; rmail-ignored-headers is given a sensible default value in loaddefs.el.
(defvar vm-invisible-header-regexp rmail-ignored-headers
  "*Non-nil value should be a regular expression that tells what headers
VM should NOT normally display when presenting a message.  All other
headers will be displayed.  The variable vm-visible-headers specifies
the presentation order of headers; headers not matched by
vm-visible-headers are displayed last.

Nil value causes VM to display ONLY those headers specified in
vm-visible-headers.")

(defvar vm-highlighted-header-regexp nil
  "
Currently this is unimplemented.

*Regular expression that matches the beginnings of headers that should
be highlighted when a message is first presented.  For example setting
this variable to \"^From\\\\|^Subject\" causes the From: and Subject:
headers to be highlighted.")

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

(defvar vm-window-configuration-file nil
  "*Non-nil value should be a string that tells VM where to load
and save its window configuration information.  The window
configuration information is loaded automatically the first time
you run VM in an Emacs session, and tells VM how to set up
windows depending on what you are doing inside VM.

The command vm-save-window-configuration (normally bound to `WS')
lets you update this information; see its documentation for more
information.")

(defvar vm-confirm-quit 0
  "*Value of t causes VM to always ask for confirmation before quitting
a VM visit of a folder.  A nil value means VM will ask only when messages
will be lost unwittingly by quitting, i.e. not removed by intentional
delete and expunge.  A value that is not nil and not t causes VM to ask
only when there are unsaved changes to message attributes, or when messages
will be unwittingly lost.")

(defvar vm-retain-message-order 0
  "*Non-nil value causes VM to save the message presentation order
when the folder is saved.  The next time the folder is visited
the saved presentation order will be used.

A value of t causes VM to consider message order changes
sufficient reason to mark the folder buffer as modified.  This
will ensure that message order changes will be saved even if no
other buffer modifications (e.g. deleting messages, or reading
unread message) are made.

A value that is not nil and not t causes VM note the message
order changes, but not to save them unless there are other folder
buffer modifications that need saving.")

(defvar vm-folder-directory nil
  "*Directory where folders of mail are kept.")

(defvar vm-confirm-new-folders nil
  "*Non-nil value causes interactive calls to vm-save-message
to ask for confirmation before creating a new folder.")

(defvar vm-delete-empty-folders t
  "*Non-nil value causes VM to remove empty (zero length) folder files
after saving them.")

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
      (SELECTOR [ARG]) ... )
    ... )

VIRTUAL-FOLDER-NAME is the name of the virtual folder being defined.
This is the name by which VM will refer to this folder.

FOLDER-NAME should be the name of a real folder.  There may be more than
one FOLDER-NAME listed, the SELECTORs within that sublist will apply to
them all.  If FOLDER-NAME is a directory, VM will assume this to mean that
all the folders in that directory should be searched.

The SELECTOR is a Lisp symbol that tells VM how to decide whether a message
from one of the folders specified by the FOLDER-NAMEs should be included
in the virtual folder.  Some SELECTORs require an argument ARG; unless
otherwise noted ARG may be omitted.

The recognized SELECTORs are:

   any       - matches any message.
   author    - matches message if ARG matches the author; ARG should be a
               regular expression.
   header    - matches message if ARG matches any part of the header
               portion of the message; ARG should be a regular expression.
   recipient - matches message if ARG matches any part of the recipient list;
               ARG should be a regular expression.
   subject   - matches message if ARG matches any part of the message's
               subject; ARG should be a regular expression.
   text      - matches message if ARG matches any part of the text
               portion of the message; ARG should be a regular expression.")

(defvar vm-virtual-mirror nil
  "*Non-nil value causes the attributes of messages in virtual folders
to mirror the changes in the attributes of the underlying real messages.
Similarly, changes in the attributes of virtual messages will change the
attributes of the underlying real messages.  A nil value causes virtual
messages to have their own distinct set of attributes, apart from the
underlying real message.

The value of vm-virtual-mirror is considered only at the time a
virtual folder is created.  Changing the value of vm-virtual-mirror
does not affect the behavior of existing virtual folders.")

(defvar vm-folder-read-only nil
  "*Non-nil value causes a folder to be considered unmodifiable by VM.
Commands that modify message attributes or messages themselves are disallowed.
Commands that add or delete messages from the folder are disallowed.
Commands that scan or allow the reading of messages are allowed but the
`new' and `unread' message flags are not changed by them.")

(defvar vm-included-text-prefix " > "
  "*String used to prefix included text in replies.")

(defvar vm-keep-sent-messages 1
  "*Non-nil value N causes VM to keep the last N messages sent from within VM.
`Keep' means that VM will not kill the VM mail buffer after you send a message
with C-c C-c (vm-mail-send-and-exit).  A value of 0 or nil causes VM never
to keep such buffers.  A value of t causes VM never to kill such buffers.

Note that these buffers will vanish once you exit Emacs.  To keep a permanent
record of your outgoing mail, use the mail-archive-file-name variable.")

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
it yourself.")

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

(defvar vm-summary-format "%2n %*%a %-17.17F %-3.3m %2d %4l/%-5c \"%s\"\n"
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
   A - longer version of attributes indicators (six characters wide)
       The first char is  `D', `N', `U' or ` ' for deleted, new, unread
       and read messages respectively.
       The second is `r' or ` ', for message replied to.
       The third is `z' or ` ', for messages forwarded.
       The fourth is `f' or ` ', for messages filed.
       The fifth is `w' or ` ', for messages written.
       The sixth is `e' or ` ', for messages that have been edited.
   c - number of characters in message (ignoring headers)
   d - numeric day of month message sent
   f - author's address
   F - author's full name (same as f if full name not found)
   h - hour:min:sec message sent
   h - hour:min message sent
   i - message ID
   l - number of lines in message (ignoring headers)
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
       the summary as it information from any other summary specifier.
   w - day of the week message sent
   y - year message sent
   z - timezone of date when the message was sent
   * - `*' if the message is marked, ` ' otherwise

Use %% to get a single %.

A numeric field width may be given between the `%' and the specifier;
this causes right justification of the substituted string.  A negative field
width causes left justification.

The field width may be followed by a `.' and a number specifying the maximum
allowed length of the substituted string.  If the string is longer than this
value it is truncated.  If the field width is negative, it is truncated on
the left instead of the right.

The summary format need not be one line per message but it must end with
a newline, otherwise the message pointer will not be displayed correctly
in the summary window.")

(defvar vm-summary-uninteresting-senders nil
  "*Non-nil value should be a regular expressions that matches
addresses that you don't consider interesting enough to
appear in the summary.  When such senders would be displayed by
the %F or %f summary format specifiers VM will substitute \"To: \"
(actually the value of vm-summary-uninteresting-senders-pointer)
followed by what would be shown by the %T and %t specifiers
respectively.")

(defvar vm-summary-uninteresting-senders-arrow "To: "
  "*String to display before the string that is displayed instead of an
\"uninteresting\" sender.  See vm-summary-uninteresting-senders.")

(defvar vm-summary-no-newlines-in-subject t
  "*If true, then any newlines in the subject of a message will be replaced 
with spaces in the summary buffer.")

(defvar vm-auto-center-summary nil
  "*Value controls whether VM will keep the summary arrow vertically
centered within the summary window. A value of t causes VM to always
keep arrow centered.  A value of nil means VM will never bother centering
the arrow.  A value that is not nil and not t causes VM to center the
arrow only if the summary window is not the only existing window.")

(defvar vm-mail-window-percentage 75
  "*Percentage of the screen that should be used to show mail messages.
The rest of the screen will be used by the summary buffer, if displayed.")

(defvar vm-mutable-windows pop-up-windows
  "*This variable's value controls VM's window usage.

A value of t gives VM free run of the Emacs display; it will commandeer
the entire screen for its purposes.

A value of nil restricts VM's window usage to the window from which
it was invoked.  VM will not create, delete, or use any other windows,
nor will it resize its own window.

A value that is neither t nor nil allows VM to use other windows, but it
will not create new ones, or resize or delete the current ones.")

(defvar vm-startup-with-summary nil
  "*Value tells VM what to display when a folder is visited.
Nil means display folder only, t means display the summary only.  A
value that is neither t not nil means to display both folder and summary.
The latter only works if the variable pop-up-windows's value is non-nil.
See the documentation for vm-mail-window-percentage to see how to change how
the screen is apportioned between the folder and summary windows.")

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

(defvar vm-search-using-regexps nil
  "*Non-nil value causes VM's search command to interpret user input as a
regular expression instead of as a literal string.")

(defvar vm-edit-message-mode 'text-mode
  "*Major mode to use when editing messages in VM.")

(defvar vm-berkeley-mail-compatibility
  (memq system-type '(berkeley-unix))
  "*Non-nil means to read and write BSD Mail(1) style Status: headers.
This makes sense if you plan to use VM to read mail archives created by
Mail.")

(defvar vm-gargle-uucp nil
  "*Non-nil value means to use a crufty regular expression that does
surprisingly well at beautifying UUCP addresses that are substituted for
%f and %t as part of summary and attribution formats.")

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
and local variables.  Mucking about in the folder buffer is certainly
possible but it is not encouraged.")

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

(defvar vm-maintainer-address "bug-vm@uunet.uu.net"
  "Where to send VM bug reports.")

(defvar vm-mode-map
  (let ((map (make-keymap)))
    (if (fboundp 'set-keymap-name) ; lemacs
	(set-keymap-name map 'vm-mode-map))
    (suppress-keymap map)
    (define-key map "h" 'vm-summarize)
    (define-key map "\M-n" 'vm-next-unread-message)
    (define-key map "\M-p" 'vm-previous-unread-message)
    (define-key map "n" 'vm-next-message)
    (define-key map "p" 'vm-previous-message)
    (define-key map "N" 'vm-Next-message)
    (define-key map "P" 'vm-Previous-message)
    (define-key map "\C-\M-n" 'vm-move-message-forward)
    (define-key map "\C-\M-p" 'vm-move-message-backward)
    (define-key map "\t" 'vm-goto-message-last-seen)
    (define-key map "\r" 'vm-goto-message)
    (define-key map "t" 'vm-expose-hidden-headers)
    (define-key map " " 'vm-scroll-forward)
    (define-key map "b" 'vm-scroll-backward)
    (define-key map "\C-?" 'vm-scroll-backward)
    (define-key map "d" 'vm-delete-message)
    (define-key map "\C-d" 'vm-delete-message-backward)
    (define-key map "u" 'vm-undelete-message)
    (define-key map "U" 'vm-unread-message)
    (define-key map "e" 'vm-edit-message)
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
    (define-key map "V" 'vm-visit-virtual-folder)
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
    (define-key map "M" (make-sparse-keymap))
    (define-key map "MN" 'vm-next-command-uses-marks)
    (define-key map "Mn" 'vm-next-command-uses-marks)
    (define-key map "MM" 'vm-mark-message) 
    (define-key map "MU" 'vm-unmark-message)
    (define-key map "Mm" 'vm-mark-all-messages)
    (define-key map "Mu" 'vm-clear-all-marks)
    (define-key map "M?" 'vm-mark-help)
    (define-key map "W" (make-sparse-keymap))
    (define-key map "WW" 'vm-apply-window-configuration)
    (define-key map "WS" 'vm-save-window-configuration)
    (define-key map "WD" 'vm-delete-window-configuration)
    (define-key map "W?" 'vm-window-help)
    (define-key map "\C-x\C-s" 'vm-save-buffer)
    (define-key map "\C-x\C-w" 'vm-write-file)
    (define-key map "\C-x\C-q" 'vm-toggle-read-only)
    (define-key map "\M-C" 'vm-show-copying-restrictions)
    (define-key map "\M-W" 'vm-show-no-warranty)
    map )
  "Keymap for VM mode.")

(defvar vm-summary-mode-map
  (cond ((fboundp 'set-keymap-parent)		; lemacs
	 (let ((map (make-sparse-keymap)))
	   (set-keymap-parent map vm-mode-map)
	   (set-keymap-name map 'vm-summary-mode-map)
	   map))
	(t					; v18 or whatever
	 (copy-keymap vm-mode-map)))
  "Keymap for VM Summary mode; inherits from vm-mode-map.")

(defvar vm-mail-mode-map
  (progn
    (require 'sendmail) ; to get `mail-mode-map'
    (let* ((map (cond ((fboundp 'set-keymap-parent)
		       ;; lemacs
		       (let ((map (make-sparse-keymap)))
			 (set-keymap-parent map mail-mode-map)
			 (set-keymap-name map 'vm-mail-mode-map)
			 map))
		      (t
		       ;; Emacs 18 or whatever
		       (copy-keymap mail-mode-map))))
	   (shadow (function (lambda (o n)
			       (let* ((w (where-is-internal o mail-mode-map)))
				 (while w
				   (define-key map (car w) n)
				   (setq w (cdr w))))))))
      ;; ## this should really be done with mail-send-actions in v19
      (funcall shadow 'mail-send 'vm-mail-send)
      (funcall shadow 'mail-send-and-exit 'vm-mail-send-and-exit)

      (funcall shadow 'mail-yank-original 'vm-yank-message)
      ;;>>> Evil: c-c letter is reserved to the user
      (or (lookup-key mail-mode-map "\C-cy")
	  (define-key map "\C-cy" 'vm-yank-message-other-folder))
      (define-key map "\C-c\C-v" vm-mode-map)
      map))
  "Keymap for VM-Mail mode; inherits from mail-mode-map.")

(defvar vm-edit-message-map
  (let ((map (cond ((fboundp 'set-keymap-parent) ; lemacs
		    (let ((map (make-sparse-keymap)))
		      (set-keymap-parent map text-mode-map)
		      (set-keymap-name map 'vm-edit-message-map)
		      map))
		   (t			; v18 or whatever
		    (copy-keymap text-mode-map)))))
    (define-key map "\C-c\e" 'vm-edit-message-end)
    (define-key map "\C-c\C-c" 'vm-edit-message-end)
    (define-key map "\C-c\C-]" 'vm-edit-message-abort)
    (define-key map "\C-c\C-v" vm-mode-map)
    map)
  "Keymap for VM-Edit mode; inherits from text-mode-map.")

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
(defvar vm-folder-read-only nil)
(make-variable-buffer-local 'vm-folder-read-only)
(defvar vm-message-order-changed nil)
(make-variable-buffer-local 'vm-message-order-changed)
(defvar vm-message-order-stuffed nil)
(make-variable-buffer-local 'vm-message-order-stuffed)
(defvar vm-last-message-pointer nil)
(make-variable-buffer-local 'vm-last-message-pointer)
(defvar vm-primary-inbox-p nil)
(make-variable-buffer-local 'vm-primary-inbox-p)
(defvar vm-visible-header-alist nil)
(make-variable-buffer-local 'vm-visible-header-alist)
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
(defvar vm-undo-record-pointer nil)
(make-variable-buffer-local 'vm-undo-record-pointer)
(defvar vm-last-save-folder nil)
(make-variable-buffer-local 'vm-last-save-folder)
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
(defvar vm-block-new-mail nil)
(make-variable-buffer-local 'vm-block-new-mail)
(defvar vm-kept-mail-buffers nil)
(defvar vm-inhibit-write-file-hook nil)
(defvar vm-chop-full-name-hook 'vm-choose-chop-full-name-hook)
(defvar vm-session-beginning t)
(defvar vm-init-file-loaded nil)
(defvar vm-window-configurations nil)
(defvar vm-window-configuration nil)
;; 0 is reserved as a marker for vm-virtual-quit
;; so start with 1 and hope to God we never wrap.
(defvar vm-message-id-number 1)
(defconst vm-spool-directory
  (or (and (boundp 'rmail-spool-directory) rmail-spool-directory)
      "/usr/spool/mail/"))
(defconst vm-attributes-header-regexp
  "^X-VM-\\(Attributes\\|v5-Data\\):\\(.*\n\\([ \t].*\n\\)*\\)")
(defconst vm-attributes-header "X-VM-v5-Data:")
(defconst vm-message-order-header-regexp "^X-VM-Message-Order:")
(defconst vm-message-order-header "X-VM-Message-Order:")
(defconst vm-bookmark-header-regexp "^X-VM-Bookmark:")
(defconst vm-bookmark-header "X-VM-Bookmark:")
(defconst vm-vheader-header-regexp "^X-VM-VHeader:")
(defconst vm-vheader-header "X-VM-VHeader:")
(defconst vm-berkeley-mail-status-header "Status: ")
(defconst vm-berkeley-mail-status-header-regexp "^Status: \\(..?\\)\n")
(defvar vm-matched-header-vector (make-vector 6 nil))
(defconst vm-supported-window-configurations
  '(("composing-message") ("editing-message")
    ("showing-message") ("paging-message") ("auto-next-message")
    ("searching-folder")
    ("end-of-message")
    ("mail-arrived")
    ("startup") ("summarize")))
(defconst vm-supported-sort-keys
  '("date" "reversed-date"
    "author" "reversed-author"
    "subject" "reversed-subject"
    "recipients" "reversed-recipients"
    "line-count" "reversed-line-count"
    "byte-count" "reversed-byte-count"
    "physical-order" "reversed-physical-order"))
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
(defconst vm-cache-vector-length 18)
(defconst vm-softdata-vector-length 4)
(defconst vm-location-data-vector-length 6)
(defconst vm-mirror-data-vector-length 3)
(defconst vm-startup-message-lines
      '("Please use \\[vm-submit-bug-report] to report bugs."
	"This is prerelease software.  Giving out copies of it means DEATH!"
	"See Revelation 6:8 for full details"
	"VM comes with ABSOLUTELY NO WARRANTY; type \\[vm-show-no-warranty] for full details"
	"In Stereo (where available)"))
;    '("Please use \\[vm-submit-bug-report] to report bugs."
;      "You may give out copies of VM.  Type \\[vm-show-copying-restrictions] to see the conditions"
;      "VM comes with ABSOLUTELY NO WARRANTY; type \\[vm-show-no-warranty] for full details"
;      "In Stereo (where available)"))
(defconst vm-startup-message-displayed nil)
;; for the mode line
(defvar vm-mode-line-format
  '("" "--%*%*-"
    ("VM " vm-version ": %b"
     (vm-message-list
      ("   " vm-ml-message-number
       " (of " vm-ml-highest-message-number ")")
      "  (no messages)"))
    "---"
    global-mode-string
    (vm-message-list
     ("---%[" vm-ml-message-attributes-alist "%]----")
     ("---%[%]----"))
    (-3 . "%p") "-%-"))
(defvar vm-ml-message-attributes-alist
  '((vm-ml-message-new "new")
    (vm-ml-message-unread "unread")
    (vm-ml-message-read "read")
    (vm-ml-message-edited " edited")
    (vm-ml-message-filed " filed")
    (vm-ml-message-written " written")
    (vm-ml-message-replied " replied")
    (vm-ml-message-forwarded " forwarded")
    (vm-ml-message-deleted " deleted")
    (vm-ml-message-marked " MARKED")))
(defvar vm-ml-message-number nil)
(make-variable-buffer-local 'vm-ml-message-number)
(defvar vm-ml-highest-message-number nil)
(make-variable-buffer-local 'vm-ml-highest-message-number)
;(defvar vm-ml-attributes-string nil)
;(make-variable-buffer-local 'vm-ml-attributes-string)
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
(defconst vm-su-month-sym-jan '("January" "1"))
(defconst vm-su-month-sym-feb '("February" "2"))
(defconst vm-su-month-sym-mar '("March" "3"))
(defconst vm-su-month-sym-apr '("April" "4"))
(defconst vm-su-month-sym-may '("May" "5"))
(defconst vm-su-month-sym-jun '("June" "6"))
(defconst vm-su-month-sym-jul '("July" "7"))
(defconst vm-su-month-sym-aug '("August" "8"))
(defconst vm-su-month-sym-sep '("September" "9"))
(defconst vm-su-month-sym-oct '("October" "10"))
(defconst vm-su-month-sym-nov '("November" "11"))
(defconst vm-su-month-sym-dec '("December" "12"))
