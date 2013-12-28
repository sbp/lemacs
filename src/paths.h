/* Think twice before editing this file.

 The file startup.el guesses at reasonable values for load-path, exec-path,
 and lock-directory.  This means that if you move emacs and its associated
 sub-tree to a different place in the filesystem, or to a different machine,
 you won't have to do anything for it to work.

 If you define the paths in this file, then you are hardcoding their locations
 into the emacs executable, and if you move emacs to a different place, you
 will have to recompile it.

 See the NEWS file for a description of the heuristic used to locate the lisp
 and exec directories at startup time.  If you are looking at this file
 because you are having trouble, then you would be much better off arranging
 for those heuristics to succeed than defining the paths in this file.

 If it defines anything, this file should define some subset of the following:

   PATH_LOADSEARCH	The default value of `load-path'.
			If you set this, you must set it to a colon-seperated
			list of the fully-qualified names of the appropriate
			lisp library, and all of its subdirectories.

   PATH_EXEC		The default value of `exec-directory' and `exec-path'.
			(exec-path also contains the value of whatever is in
			the PATH environment variable.)

   PATH_LOCK		The name of the directory that contains lock files
			with which we record what files are being modified in 
			Emacs.  This directory should be writable by everyone.
			If this is specified, the string must end with a slash!

   PATH_SUPERLOCK	The name of the file !!!SuperLock!!! in the lock 
			directory.  You probably should let this default...
 */
