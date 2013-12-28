$ ! VMS command file to make the definitions needed to run the installed Emacs.
$ ! You must execute this in each session in order to run Emacs
$ !  or else it must be executed by the system at each boot.
$
$ ! If you execute at boot time, specify "/SYSTEM" as the first parameter.
$
$ ! This file must reside in the top directory of the Emacs subtree
$ ! when it is executed, because it uses its own directory
$ ! to initialize a logical name.
$
$ fdev=f$parse(f$environment("procedure"),,,"DEVICE")-":"
$ fdir=f$parse(f$environment("procedure"),,,"DIRECTORY")-"["
$ ftrn=f$trnlnm(fdev)
$ if ftrn .eqs. "" then ftrn = fdev + ":[]"
$ ndef= ftrn-"]"+fdir-".000000"
$ base=ndef-"]"+".]"
$
$ define 'p1' /translation=concealed emacs_library 'base'
$
$ !
$ ! The following logical name is needed for M-x shell to work.
$ ! process.c\create_process keys on the string *dcl*.
$ !
$ define 'p1' eshell "*dcl*"
$
$ runemacs :== $emacs_library:[000000]emacs -map emacs_library:[000000]emacs.dump
$ emacs :== @emacs_library:[000000]kepteditor emacs
