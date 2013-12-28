/****************************************************************************
 ***
 ***	Copyright © 1990 by Sun/Lucid,	All Rights Reserved.
 ***	Copyright © 1991-1993 by Lucid, Inc.  All Rights Reserved.
 ***
 *****************************************************************************/
#ifndef _EMACS_EDITORSIDE_H_
#define _EMACS_EDITORSIDE_H_

#ifndef ENERGIZE
ERROR! editorside.h is an Energize-only file
#endif

#include <connection.h>
#include <editorreq.h>
#include <editorconn.h>
#include <editoption.h>
#include <request.h>

#include "hash.h"
#include "extents.h"

/* Product name... */
#ifndef NCR486
# define IDENTITY_CRISIS "Energize"
#else
# define IDENTITY_CRISIS "C++ Workbench"
#endif

/* This represents the emacs connection to the Energize server.
 */
typedef struct {
  Connection*	conn;		/* connection to Energize */
  Lisp_Object	proc;		/* Emacs process */
  c_hashtable	binfo_hash;	/* hashtable for buffers */
  Lisp_Object	gc_save;	/* Objects saved for GC */
  int		major;		/* protocol version number */
  int		minor;
  c_hashtable image_table;      /* glyphs table */
} Editor;


typedef BITS32 EId;		/* Energize id */

/* In general, we keep positions in Energize form in our data structures,
   and only convert to Emacs positions as needed for Emacs operations. */
typedef BITS32 EnergizePos;	/* Energize position = (Emacs position-1) */
typedef BITS32 EmacsPos;
     


/* Generic extent data and classes
   This "seal" junk is a completely bogus data type system that should be
   replaced with something implemented using Lisp_Records.
 */
#define GDATA_CLASS_SEAL 0x12345678
#define GDATA_SEAL       0x67654321
#define EXTENT_SEAL      0x45612378
#define DUP_SEAL         0x13524768
#define BUF_INFO_SEAL    0x5F7F6F5F
#define OBJECT_SEAL_MASK 0x7FFFFFFF
#define OBJECT_FREE_BIT  0x80000000

#define OBJECT_SEAL(x) (((Energize_Extent_Data *)(x))->seal & OBJECT_SEAL_MASK)
#define OBJECT_FREE(x) (((Energize_Extent_Data *)(x))->seal & OBJECT_FREE_BIT)
#define SET_OBJECT_FREE(x) \
{ \
  /* if (OBJECT_FREE (x)) \
    error ("Free'ing already freed object 0x%x", x); \
  ((Energize_Extent_Data *)(x))->seal |= OBJECT_FREE_BIT; \  */ \
  xfree(x); \
}

#define CHECK_OBJECT(x, y) \
{ /* if (OBJECT_FREE (x)) error ("Using freed object 0x%x", x); */ }

#define PUT_ABLE_OBJECT(x) \
((OBJECT_SEAL(x) == BUF_INFO_SEAL) || (OBJECT_SEAL(x) == EXTENT_SEAL) || \
 (OBJECT_SEAL(x) == GDATA_CLASS_SEAL) || (OBJECT_SEAL(x) == GDATA_SEAL))

enum Energize_Object_Free_Type  
{ OFT_MAPHASH, OFT_STANDALONE, OFT_GC };

typedef struct 
{
  int seal;                     /* must be GDATA_CLASS_SEAL */
  EId id;
  short flags;
  GLYPH glyph;
} GDataClass;

typedef struct 
{
  int seal;                     /* must be GDATA_SEAL */
  EId id;
  GDataClass *cl;
  GLYPH glyph;
  short flags;
  short attribute;              /* graphic attribute for extent chars */
  short modified_state;
} GenericData;



/* Internal, Energize-specific information about each Energize-created extent.
 */

typedef struct Energize_Extent_Data
{
  int seal;                     /* must be EXTENT_SEAL */
  EId id;
  int extentType;               /* oneof CEAttribute, CEAbbreviation, etc. */
  Lisp_Object extent;           /* corresponding extent (never nil) */
  struct x_pixmap *start_pixmap;
  struct x_pixmap *end_pixmap;
  int warn_modify;
  union
    {
      struct
        {                       /* CEAttribute */
          int attrValue;
        } attr;
      struct 
        {                       /* CEAbbreviation */
          Boolean isOpened;
        } abbrev;
      struct 
        {                       /* CEGeneric */
          GenericData* gData;
        } generic;
    } u;
} Energize_Extent_Data;


/* Internal, Energize-specific information about each Energize-created buffer.
 */

typedef struct 
{
  int seal;                     /* must be BUF_INFO_SEAL */
  EId id;                        /* Energize id for buffer */
  int flags;                    /* e.g. CBReadOnly or CBStandard */
  Editor *editor;               /* corresponding editor structure */
  c_hashtable id_to_object;     /* energize ids to extents, gdata & classes */
  Lisp_Object emacs_buffer;     /* corresponding emacs buffer */
  char	modified_state;		/* modified state as notified to Energize */
  char	editable;		/* Energize authorized user to edit buffer */
  Lisp_Object output_mark;      /* analog to process_mark (see process.c) */
  char *buffer_type;            /* string denoting buffer type; see below */
  int*	p_sheet_ids;		/* psheets associated with the buffer */
  int	n_p_sheets;		/* number of p_sheets */
  int*	note_ids;		/* id of opened postit notes */
  int	n_notes;		/* number of notes */
  Lisp_Object screen;		/* screen that was created for the buffer */
} BufferInfo;

/* "Types" of buffers, according to the Energize server.

   Source files: SOURCE			source_class, source_as_component_class
   Projects:	 PROJECT		project_class
   Log Files:	 LOG_FILE		log_file_class
   Debugger log: DEBUGGER		debugger_tool_class
   Breakpoints:	 BREAKPOINT		breaklist_class
   Unix Man:	 UNIX_MANUAL		unix_man_file_class (no longer used?)
   File:	 FILE			lazy_file_class
   Energize:	 ENERGIZE_BUFFER	energize_class

   Unspecified:	 UNINITIALIZED_BUFFER_TYPE
   Unknown:	 UNKNOWN_BUFFER_TYPE

   There should be a BROWSER type as well, but there isn't, so we have
   a kludge to fake it up in lisp.
*/

struct buffer_type_struct
{
  char *kernel_name;
  char *elisp_name;
};

static struct buffer_type_struct
  kernel_buffer_types_to_elisp_buffer_types_vector[] =
{ 
  { "FILE", "energize-source-buffer" }, /* #### Is this necessary? */
  { "SOURCE", "energize-source-buffer" },
  { "PROJECT", "energize-project-buffer" },
  { "LOG_FILE", "energize-log-file-buffer" },
  { "DEBUGGER", "energize-debugger-buffer" },
  { "BREAKPOINT", "energize-breakpoint-buffer" },
  { "UNIX_MANUAL", "energize-unix-manual-buffer" },
  { "ENERGIZE_BUFFER", "energize-top-level-buffer" },
/*  { "CADILLAC_BUFFER", "energize-top-level-buffer" }, /* #### rename me */
/*  { "POSTIT", "energize-postit-buffer-type" }, */
  /* #### There needs to be a "browser" buffer type in here too */
  { 0 , 0 }
};

#define UNINITIALIZED_BUFFER_TYPE "energize-unspecified-buffer"
#define UNKNOWN_BUFFER_TYPE "energize-unknown-buffer"

Energize_Extent_Data *energize_extent_data (EXTENT);

#endif /* _EMACS_EDITORSIDE_H_ */
