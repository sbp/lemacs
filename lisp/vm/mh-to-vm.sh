#!/bin/sh

# Date: Thu, 9 Dec 93 18:23:56 -0500
# From: kyle@crystal.WonderWorks.com (Kyle Jones)
# To: lipp@networx.com
# Cc: info-vm@uunet.uu.net
# Subject: Converting MH folders to VM
# 
# Damon Lipparelli writes:
#  > Does anyone have a way of converting MH folders to VM folders?  I've just
#  > switched to using VM and I don't want to have to re-mail myself all of my
#  > old mail messages stored with MH to get them into VM folders.
# 
# If I remember correctly MH folders are just directories, with one
# message per file.  No UNIX From lines control-As or any of that
# cruft.  So you need to roll these messages into one file that VM
# can handle.  To get UNIX From style folders this script ought to
# do it.  Run it like this
# 
# % mh-to-vm.sh MH-folder-directory new-VM-folder-file
# 
# It doesn't remove anything so if it doesn't work, then no harm done.

source=$1
dest=$2

leader="From nobody `date`"
trailer=""

for message in $source/*
do
   echo $leader >> $dest
   sed 's/^From />From /' < $message >> $dest
   echo $trailer >> $dest
done

exit
