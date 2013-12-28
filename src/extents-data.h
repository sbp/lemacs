/* This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef EXTENTSDATA
#define EXTENTSDATA

/****************************** Types *********************************/

#ifdef ENERGIZE
#include <request.h>
#include <editorside.h>
#include "extents.h"
#include <wimage.h>

#else /* ! ENERGIZE */

#define BITS32 unsigned int

#endif /* ! ENERGIZE */

typedef BITS32 Id;		/* Energize id */

/* In general, we keep positions in Energize form in our data structures,
   and only convert to Emacs positions as needed for Emacs operations. */
typedef BITS32 EnergizePos;	/* Energize position = (Emacs position-1) */
typedef BITS32 EmacsPos;
     
/* Generic extent data and classes */
     
#define GDATA_CLASS_SEAL 0x12345678
#define GDATA_SEAL       0x67654321
#define EXTENT_SEAL      0x45612378
#define DUP_SEAL         0x13524768
#define BUF_INFO_SEAL    0x5F7F6F5F
#define OBJECT_SEAL_MASK 0x7FFFFFFF
#define OBJECT_FREE_BIT  0x80000000

#define OBJECT_SEAL(x) (((Extent_Data *)(x))->seal & OBJECT_SEAL_MASK)
#define OBJECT_FREE(x) (((Extent_Data *)(x))->seal & OBJECT_FREE_BIT)
#define SET_OBJECT_FREE(x) \
{ \
  /* if (OBJECT_FREE (x)) \
    error ("Free'ing already freed object 0x%x", x); \
  ((Extent_Data *)(x))->seal |= OBJECT_FREE_BIT; \  */ \
  free(x); \
}

#define CHECK_OBJECT(x, y) \
{ /* if (OBJECT_FREE (x)) error ("Using freed object 0x%x", x); */ }

#define PUT_ABLE_OBJECT(x) \
((OBJECT_SEAL(x) == BUF_INFO_SEAL) || (OBJECT_SEAL(x) == EXTENT_SEAL) || \
 (OBJECT_SEAL(x) == GDATA_CLASS_SEAL) || (OBJECT_SEAL(x) == GDATA_SEAL))

enum Object_Free_Type  
{ OFT_MAPHASH, OFT_STANDALONE, OFT_GC };
typedef enum Object_Free_Type OFT;


     
enum Extent_Glyph_Type  
{ EGT_START_GLYPH, EGT_END_GLYPH };
                            
typedef enum Extent_Glyph_Type EGT;

#ifdef ENERGIZE

struct Image_Entry_Struct
{
  Image *image;
  int refcount;
  char *defining_string;
  Pixmap pixmap;
  short height;
  short width;
  /* 0 if not assigned */
  int	bitmap_index;
};

typedef struct Image_Entry_Struct *IMAGE;

#endif /* ENERGIZE */

typedef struct 
{
  int seal;                     /* must be GDATA_CLASS_SEAL */
  Id id;
  short flags;
#ifdef ENERGIZE
  IMAGE image;
#endif
} GDataClass;

typedef struct 
{
  int seal;                     /* must be GDATA_SEAL */
  Id id;
  GDataClass *cl;
#ifdef ENERGIZE
  IMAGE image;
#endif
  short flags;
  short attribute;              /* graphic attribute for extent chars */
  short modified_state;
} GenericData;

/* Information about each extent */

typedef struct 
{
  int seal;                     /* must be EXTENT_SEAL */
  Id id;
  int extentType;               /* oneof CEAttribute, CEAbbreviation, etc. */
  Lisp_Object extent;           /* corresponding extent */
#ifdef ENERGIZE
  Lisp_Object start_glyph_index;
  Lisp_Object end_glyph_index;
#endif
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
} Extent_Data;


#ifdef ENERGIZE

/* Information about each Energize buffer */

typedef struct 
{
  int seal;                     /* must be BUF_INFO_SEAL */
  Id id;                        /* Energize id for buffer */
  int flags;                    /* e.g. CBReadOnly or CBStandard */
  Editor *editor;               /* corresponding editor structure */
  c_hashtable id_to_object;     /* energize ids to extents, gdata & classes */
  c_hashtable image_table;      /* buffer local glyphs table */
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

/* Buffer types */
/*
   Source files: SOURCE -- source_class, source_as_component_class
   Projects: PROJECT -- project_class
   Log Files: LOG_FILE -- log_file_class
   Debugger log: DEBUGGER -- debugger_tool_class
   Breakpoints: BREAKPOINT -- breaklist_class
   Unix Man: UNIX_MANUAL -- unix_man_file_class
   File: FILE -- lazy_file_class
   Energize: ENERGIZE_BUFFER -- energize_class

   Unspecified: UNINITIALIZED_BUFFER_TYPE
   Unknown: UNKNOWN_BUFFER_TYPE
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
  { "CADILLAC_BUFFER", "energize-top-level-buffer" }, /* #### rename me */
  /* #### There needs to be a "browser" buffer type in here too */
  { "POSTIT", "energize::postit-buffer-type" },
  { 0 , 0 }
};

#define UNINITIALIZED_BUFFER_TYPE "energize-unspecified-buffer"
#define UNKNOWN_BUFFER_TYPE "energize-unknown-buffer"

#endif /* ENERGIZE */

#endif /* EXTENTSDATA */
