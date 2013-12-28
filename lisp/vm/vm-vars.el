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

(defvar vm-primary-inbox "~/INBOX"
  "*Mail is moved from the system mailbox to this file for reading.")

(defvar vm-crash-box "~/INBOX.CRASH"
  "*File in which to store mail temporarily while it is transferrred from
the system mailbox to the primary inbox.  If the something happens
during this mail transfer, any missing mail will be found in this file.
VM will do crash recovery from this file automatically at startup, as
necessary.")

(defvar vm-spool-files
  (or (and (setq vm-spool-files (getenv "MAILPATH"))
	   (setq vm-spool-files
		 (vm-parse vm-spool-files
			   "\\([^:%?]+\\)\\([%?][^:]*\\)?\\(:\\|$\\)")))
      (and (setq vm-spool-files (getenv "MAIL"))
	   (setq vm-spool-files (list vm-spool-files))))
  "*If non-nil this variable's value should be a list of strings naming files
that VM will check for incoming mail instead of the where VM thinks your
system mailbox is.  This variable can be used to specify multiple spool files
or to point VM in the right direction if its notion of your system mailbox is
incorrect.

VM will default to the value of the shell environmental variables
MAILPATH or MAIL if either of these variables are defined.")

(defvar vm-visible-headers
  '("From:" "Sender:" "Resent-From"
    "To:" "Apparently-To:" "Cc:"
    "Subject:"
    "Date:" "Resent-Date:")
  "*List of headers that should be visible when VM first displays a message.
These should be listed in the order you wish them presented.
Regular expressions are allowed.")

(defvar vm-invisible-header-regexp nil
  "*Non-nil value should be a regular expression that tells what headers
VM should NOT normally display when presenting a message.  All other
headers will be displayed.  The variable vm-visible-headers specifies
the presentation order of headers; headers not matched by
vm-visible-headers are displayed last.

Nil value causes VM to display ONLY those headers specified in
vm-visible-headers.")

(defvar vm-highlighted-header-regexp nil
  "*Regular expression that matches the beginnings of headers that should
be highlighted when a message is first presented.  For exmaple setting
this variable to \"^From\\\\|^Subject\" causes the From: and Subject:
headers to be highlighted.")

(defvar vm-preview-lines 0
  "*Non-nil value N causes VM to display the visible headers + N lines of text
from a message when it is first presented.  The message is not actually
flagged as read until it is exposed in its entirety.  Nil causes VM not
to preview a message at all; it is displayed in its entirety when first
presented and is immediately flagged as read.")

(defvar vm-preview-read-messages t
  "*Non-nil value means to preview messages even if they've already been read.
A nil value causes VM to preview messages only if new or unread.")

(defvar vm-auto-next-message t
  "*Non-nil value causes VM to use vm-next-mesage to advance to the next
message in the folder if the user attempts to scroll past the end of the
current messages.  A nil value disables this behavior.")

(defvar vm-honor-page-delimiters nil
  "*Non-nil value causes VM to honor pages delimiters (as specified by the
page-delimiter variable) when scrolling through a message.")

(defvar vm-window-configuration-file nil
  "*Non-nil value should be a string that tells VM where to load
and store its window configuration information.  The window
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

(defvar vm-flush-interval 90
  "*Non-nil value specifies how often VM flushes its cached internal
data.  A numeric value gives the number of seconds between
flushes.  A value of t means flush every time there is a change.
Nil means don't do flushing until a message or folder is saved.

Normally when a message attribute is changed. VM keeps the record
of the change in its internal memory and doesn't insert the
changed data into the folder buffer until a particular message or
the whole folder is saved to disk.  This makes normal Emacs
auto-saving useless of VM folder buffers because the information
you'd want to auto-save, i.e. the attribute changes, isn't in
the buffer when it is auto-saved.

Setting vm-flsuh-interval to a numeric value will cause the VM's
internal memory caches to be periodically flushed to the folder
buffer.  This is done non-obtrusively, so that if you type
something while flushing is occuring, the flush will abort
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
\((HEADER-NAME
   (REGEXP . FOLDER-NAME) ...
  ...))
where HEADER-NAME and REGEXP are strings, and FOLDER-NAME is a string or an
s-expression that evaluates to a string.

If any part of the contents of the message header named by HEADER-NAME
is matched by the regular expression REGEXP, VM will evaluate the
corresponding FOLDER-NAME and use the result as the default when
prompting for a folder to save the message in.  If the resulting folder
name is a relative pathname, then it will resolve to the directory named by
vm-folder-directory, or the default-directory of the currently visited
folder if vm-folder-directory is nil.

When FOLDER-NAME is evaluated, the current buffer will contain only the
contents of the header named by HEADER-NAME.  It is safe to modify this
buffer.  You can use the match data from any \\( ... \\) grouping
constructs in REGEXP along with the function buffer-substring to build a
folder name based on the header information.  If the result of
evaluating FOLDER-NAME is a list, then the list will be treated as
another auto-folder-alist and will be descended recursively.

Whether matching is case sensitive depends on the value of the variable
vm-auto-folder-case-fold-search.")

(defvar vm-auto-folder-case-fold-search nil
  "*Non-nil value means VM will ignore case when matching header
contents while doing automatic folder selection via the variable
vm-auto-folder-alist.")

(defvar vm-virtual-folder-alist nil
  "*Non-nil value should be a list of virtual folder definitions.
A virtual folder is a mapping of messages from one or more real folders
into what appears to be a single folder.  A virtual folder definition
specifes which real folders should be searched for prospective messages
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
from one of the folders specifed by the FOLDER-NAMEs should be included
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
messages to have their own dinstinct set of attributes, apart from the
underlying real message.

The value of vm-virtual-mirror is considered only at the time a
virtaul folder is visited.  Changing the value of vm-virtual-mirror
does not affect the behavior of existing virtual folders.")

(defvar vm-folder-read-only nil
  "*Non-nil value causes a folder to be considered unmodifiable by VM.
Commands that modify message attributes or messages themselves are disallowed.
Commands that scan or allow the reading of messages are allowed but the
`new' and `unread' message flags are not changed by them.")

(defvar vm-included-text-prefix " > "
  "*String used to prefix included text in replies.")

(defvar vm-keep-sent-messages 1
  "*Non-nil value N causes VM to keep the last N messages sent from within VM.
`Keep' means that VM will not kill the VM mail buffer after you send a message
with C-c C-c (vm-mail-send-and-exit).  A value of 0 or nil causes VM never
to keep such buffers.  A value of t cuases VM never to kill such buffers.

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
addresses that VM should automatically remove from the recipient headers
of replies.")

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

(defvar vm-digest-preamble-format "\"%s\" (%F)"
  "*String which specifies the format of the preamble lines gneerated by 
vm-send-digest when it is invoked with a prefix argument.  See the
documentation for the variable vm-summary-format for information on what
this string may contain.  The format should *not* end with nor contain a
newline.")

(defvar vm-digest-center-preamble t
  "*Non-nil value means VM will center the preamble lines that precede
the start of a digest.  How the lines will be centered depends on the
ambient value of fill-column.   A nil value suppresses centering.")

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
   w - day of the week message sent
   y - year message sent
   z - timezone of date when the message was sent
   * - `*' if the message is marked, ` ' otherwise

Use %% to get a single %.

A numeric field width may be specified between the `%' and the specifier;
this causes right justification of the substituted string.  A negative field
width causes left justification.

The field width may be followed by a `.' and a number specifying the maximum
allowed length of the substituted string.  If the string is longer than this
value it is truncated.

The summary format need not be one line per message but it must end with
a newline, otherwise the message pointer will not be displayed correctly
in the summary window.")

(defvar vm-auto-center-summary nil
  "*Value controls whether VM will keep the summary arrow vertically
centered within the summary window. A value of t causes VM to always
keep arrow cenered.  A value of nil means VM will never bother centering
the arrow.  A value that is not nil and not t causes VM to center the
arrow only if the summary window is not the only existing window.")

(defvar vm-mail-window-percentage 75
  "*Percentage of the screen that should be used to show mail messages.
The rest of the screen will be used by the summary buffer, if displayed.")

(defvar vm-mutable-windows t
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

(defvar vm-group-by nil
  "*Non-nil value tells VM how to group message presentation.
Currently, the valid non-nil values for this variable are
  \"subject\", which causes messages with the same subject (ignoring
    Re:'s) to be presented together,
  \"author\", which causes messages with the same author to be presented
    together,
  \"recipient\", which causes messages with the same set of recipients to
    be presented together,
  \"date-sent\", which causes message sent on the same day to be
    presented together,
  \"physical-order\" which appears only for completeness, this is the
    default behavior and is the same as nil.

The ordering of the messages in the folder itself is not altered, messages
are simply numbered and ordered differently internally.")

(defvar vm-skip-deleted-messages t
  "*Non-nil value causes VM's `n' and 'p' commands to skip over
deleted messages.  A value of t causes deleted message to always be skipped.
A value that is not nil and not t cuases deleted messages to be skipped only
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
past the deleted messages.")

(defvar vm-move-after-undeleting nil
  "*Non-nil value causes VM's `u' command to automatically invoke
vm-next-message or vm-previous-message after undeleting, to move
past the undeleted messages.")

(defvar vm-delete-after-saving nil
  "*Non-nil value causes VM automatically to mark messages for deletion
after successfully saving them to a folder.")

(defvar vm-delete-after-archiving nil
  "*Non-nil value causes VM automatically to mark messages for deletion
after successfully auto-archiving them with the vm-auto-archive-messages
command.")

(defvar vm-delete-after-bursting nil
  "*Non-nil value causes VM automatically to mark messages for deletion
after successfully bursting them with the vm-burst-digest command.")

(defvar vm-circular-folders 0
  "*Value determines whether VM folders will be considered circular by
various commands.  `Circular' means VM will wrap from the end of the folder
to the start and vice versa when moving the message pointer or deleting,
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

(defvar vm-mode-hooks nil
  "*List of hook functions to run when a buffer enters vm-mode.
These hook functions should generally be used to set key bindings
and local variables.  Mucking about in the folder buffer is certainly
possible but it is not encouraged.")

(defvar vm-berkeley-mail-compatibility
  (memq system-type '(berkeley-unix))
  "*Non-nil means to read and write BSD Mail(1) style Status: headers.
This makes sense if you plan to use VM to read mail archives created by
Mail.")

(defvar vm-gargle-uucp nil
  "*Non-nil value means to use a crufty regular expression that does
surprisingly well at beautifying UUCP addresses that are substitued for
%f and %t as part of summary and attribution formats.")

(defvar vm-strip-reply-headers nil
  "*Non-nil value causes VM to strip away all comments and extraneous text
from the headers generated in reply messages.  If you use the \"fakemail\"
program as distributed with Emacs, you probably want to set this variable to
to t, because as of Emacs v18.52 \"fakemail\" could not handle unstripped
headers.")

(defvar vm-rfc934-forwarding t
  "*Non-nil value causes VM to use char stuffing as described in RFC 934
when packaging a message to be forwarded.  This will allow the recipient
to use a standard bursting agent on the message and act upon it as if it
were sent directly.")

(defvar vm-inhibit-startup-message nil
  "*Non-nil causes VM not to display its copyright notice, disclaimers
etc. when started in the usual way.")

(defvar mail-yank-hooks nil
  "*List of hook functions called after yanking a message into a *mail*
buffer.  See the documentation for the function vm-yank-message for details.")

(defvar vm-movemail-program "movemail"
  "*Name of program to use to move mail from the system spool
to another location.  Normally this shoul be the movemail program
distributed with Emacs.")

(defvar vm-mode-map
  (let ((map (make-keymap)))
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
    (define-key map "G" 'vm-group-messages)
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
    (define-key map "L" 'vm-load-rc)
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
    (define-key map "\M-C" 'vm-show-copying-restrictions)
    (define-key map "\M-W" 'vm-show-no-warranty)
    map )
  "Keymap for VM mode and VM Summary mode.")

(defvar vm-edit-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\e" 'vm-edit-message-end)
    (define-key map "\C-c\C-c" 'vm-edit-message-end)
    (define-key map "\C-c\C-]" 'vm-edit-message-abort)
    (define-key map "\C-c\C-v" vm-mode-map)
    map )
  "*Keymap used while editing a message with vm-edit-message.
This keymap is used in addition to the keymap used by the major mode
specified by vm-edit-message-mode.")

;; internal vars
(defvar vm-mode-line-format
  '("" (vm-buffer-modified-p vm-buffer-modified-p "-----")
    ("VM " vm-version ": %b"
     (vm-message-list
      ("   " vm-ml-message-number
       " (of " vm-ml-highest-message-number ")")
      "  (no messages)"))
    "   "
    global-mode-string
    (vm-message-list
     ("   %[{" vm-ml-attributes-string "}%]----")
     ("   %[%]----"))
    (-3 . "%p") "-%-"))
(defvar vm-folder-type nil)
(make-variable-buffer-local 'vm-folder-type)
(defvar vm-message-list nil)
(make-variable-buffer-local 'vm-message-list)
(defvar vm-virtual-buffers nil)
(make-variable-buffer-local 'vm-virtual-buffers)
(defvar vm-real-buffers nil)
(make-variable-buffer-local 'vm-real-buffers)
(defvar vm-message-pointer nil)
(defvar vm-message-pointer nil)
(make-variable-buffer-local 'vm-message-pointer)
(defvar vm-folder-read-only nil)
(make-variable-buffer-local 'vm-folder-read-only)
(defvar vm-buffer-modified-p nil)
(make-variable-buffer-local 'vm-buffer-modified-p)
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
(defvar vm-current-grouping nil)
(make-variable-buffer-local 'vm-current-grouping)
(defvar vm-last-save-folder nil)
(make-variable-buffer-local 'vm-last-save-folder)
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
(defvar vm-checkpoint-modification-counter nil)
(make-variable-buffer-local 'vm-checkpoint-modification-counter)
(defvar vm-messages-needing-display-update nil)
(make-variable-buffer-local 'vm-messages-needing-display-update)
(defvar vm-numbering-redo-start-point nil)
(make-variable-buffer-local 'vm-numbering-redo-start-point)
(defvar vm-numbering-redo-end-point nil)
(make-variable-buffer-local 'vm-numbering-redo-end-point)
(defvar vm-summary-redo-start-point nil)
(make-variable-buffer-local 'vm-summary-redo-start-point)
(defvar vm-need-summary-pointer-update nil)
(make-variable-buffer-local 'vm-need-summary-pointer-update)
(defvar vm-deferred-message nil)
(defvar vm-block-new-mail nil)
(defvar vm-kept-mail-buffers nil)
(defvar vm-inhibit-write-file-hook nil)
(defvar vm-session-beginning t)
(defvar vm-rc-loaded nil)
(defvar vm-window-configurations nil)
(defvar vm-window-configuration nil)
(defconst vm-spool-directory
  (or (and (boundp 'rmail-spool-directory) rmail-spool-directory)
      "/usr/spool/mail/"))
(defconst vm-attributes-header-regexp
  "^X-VM-\\(Attributes\\|v5-Data\\):\\(.*\n\\([ \t]+.*\n\\)*\\)")
(defconst vm-attributes-header "X-VM-v5-Data:")
(defconst vm-message-order-header-regexp
  "X-VM-Message-Order:\\(.*\n\\([ \t]+.*\n\\)*\\)")
(defconst vm-message-order-header "X-VM-Message-Order:")
(defconst vm-bookmark-header-regexp
  "X-VM-Bookmark:\\(.*\n\\([ \t]+.*\n\\)*\\)")
(defconst vm-bookmark-header "X-VM-Bookmark:")
(defconst vm-vheader-header-regexp
  "X-VM-VHeader:\\(.*\n\\([ \t]+.*\n\\)*\\)")
(defconst vm-vheader-header "X-VM-VHeader:")
(defconst vm-berkeley-mail-status-header "Status: ")
(defconst vm-berkeley-mail-status-header-regexp "^Status: \\(..?\\)\n")
(defconst vm-generic-header-regexp
  "^\\([^: \t\n]+\\)[\t ]*:\\(.*\n\\([ \t]+.*\n\\)*\\)")
(defconst vm-header-regexp-format "^%s:[ \t]*\\(.*\\(\n[ \t]+.*\\)*\\)")
(defconst vm-supported-groupings-alist
  '(("physical-order") ("subject") ("author") ("date-sent") ("recipient")))
(defconst vm-supported-window-configurations
  '(("composing-message") ("editing-message")
    ("showing-message") ("paging-message") ("auto-next-message")
    ("searching-folder")
    ("end-of-message")
    ("startup") ("summarize")))
(defconst vm-attributes-vector-length 9)
(defconst vm-cache-vector-length 16)
(defconst vm-softdata-vector-length 4)
(defconst vm-virtual-data-vector-length 9)
(defconst vm-startup-message-lines
      '("Mail bug reports to bug-vm@uunet.uu.net or post them to gnu.emacs.vm.bug"
	"This is prerelease software.  Giving out copies of it means DEATH!"
	"See Revelations 6:8 for full details"
	"VM comes with ABSOLUTELY NO WARRANTY; type \\[vm-show-no-warranty] for full details"
	"In Stereo (where available)"))
;    '("Mail bug reports to bug-vm@uunet.uu.net or post them to gnu.emacs.vm.bug"
;      "You may give out copies of VM.  Type \\[vm-show-copying-restrictions] to see the conditions"
;      "VM comes with ABSOLUTELY NO WARRANTY; type \\[vm-show-no-warranty] for full details"
;      "In Stereo (where available)"))
(defconst vm-startup-message-displayed nil)
;; for the mode line
(defvar vm-ml-message-number nil)
(make-variable-buffer-local 'vm-ml-message-number)
(defvar vm-ml-highest-message-number nil)
(make-variable-buffer-local 'vm-ml-highest-message-number)
(defvar vm-ml-attributes-string nil)
(make-variable-buffer-local 'vm-ml-attributes-string)
