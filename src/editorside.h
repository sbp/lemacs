/****************************************************************************
 ***
 ***        (c) Copyright 1990 by Sun Microsystems Inc.,  All Rights Reserved.
 ***
*****************************************************************************/
#ifndef EDITORSIDE
#define EDITORSIDE

/* 
 * Key Lucid Emacs <-> Energize Server interface
 */

/* define const away to avoid gcc vs ansi-c problem in connection.h */
#define const		       
#include <connection.h>
#include "hash.h"

#define Boolean	int
#ifdef TRUE
#undef TRUE
#endif
#ifdef FALSE
#undef FALSE
#endif

#define TRUE	(1)
#define FALSE	(0)

#define NIL     (0)

/* Types */

typedef struct {
  Connection*	conn;		/* connection to Energize */
  Lisp_Object	proc;		/* Emacs process */
  c_hashtable	binfo_hash;	/* hashtable for buffers */
  Lisp_Object	gc_save;	/* Objects saved for GC */
  int		major;		/* protocol version number */
  int		minor;
} Editor;


#endif /* EDITORSIDE */
