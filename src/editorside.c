/****************************************************************************
 ***
 ***	Copyright � 1990 by Sun/Lucid,  All Rights Reserved.
 ***	Copyright � 1991-1993 by Lucid, Inc.  All Rights Reserved.
 ***
 *****************************************************************************/

#include "config.h"

#ifdef ENERGIZE 	/* whole file */

#include "lisp.h"
#include "events.h"

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>		/* some typedefs are used in sys/file.h */
#include <sys/file.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>		/* if not provided BLOCK_INPUT loses */
#include <signal.h>		/* must be before xterm.c ?? */
#include <string.h>
#include <errno.h>

/* editing and buffer operations */
#include "buffer.h" 
#include "extents.h"
#include "process.h"

/* screen management */
#include "xterm.h"
#include "screen.h"
#include "window.h" 

#include <lwlib.h>

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/DialogS.h>

/* Energize editor requests and I/O operations */
#include "editorside.h"

#include <editorreq.h>
#include <editorconn.h>
#include <editoption.h>

#include "extents-data.h"


extern void CWriteQueryChoicesRequest ();
extern void CWriteExecuteChoicesRequest();
extern void CWriteSheetRequest();
extern void CWriteSetControlRequest();
extern void CWriteChoice();
extern void CWriteProtocol();
extern int  CGetPortNumber();
extern int  CReadSomeMore(Connection*, int);
extern void NewPixmapImage();
extern void NewAttributeImage();


/************** Typedefs and Structs ***********************/

/* structure argument used by the next mapping function */
typedef struct 
{
  BufferInfo *binfo;
  int n_extents;
} binfo_and_n_extents;

typedef struct 
{
  BufferInfo* 	binfo;
  int 		state;
  int		tell_energize;
} binfo_and_state;

struct reply_wait {
  int		serial;
  EId		objectId;
  EId		genericId;
  EId		itemId;
  char		answered_p;
  char		status;
  char*		message;
  Lisp_Object	menu_result;
  Lisp_Object	only_name;
  struct reply_wait*	next;
};

static struct reply_wait*
global_reply_wait;

Lisp_Object Venergize_kernel_busy;
Lisp_Object Qenergize_kernel_busy;
Lisp_Object Venergize_attributes_mapping;
Lisp_Object Venergize_kernel_busy_hook;
Lisp_Object Venergize_menu_update_hook;
Lisp_Object Qenergize_extent_data;

/*static int energize_font_lock_p;*/


/************************ Functions ********************/
extern Lisp_Object Venergize_kernel_busy;
extern Lisp_Object make_extent_for_data
(BufferInfo *binfo, Energize_Extent_Data *ext, int from, int to, int set_endpoints);

static int
wait_for_reply (struct reply_wait* rw);
static Energize_Extent_Data *extent_to_data (Lisp_Object extent_obj);
static char *copy_string (char *s);
Lisp_Object word_to_lisp (unsigned int item);
unsigned int lisp_to_word (Lisp_Object obj);
static void *get_object (EId id, BufferInfo *binfo);
static void put_object (EId id, BufferInfo *binfo, void *object);
static void remove_object (EId id, BufferInfo *binfo);
static void free_object (void *key, void *contents, void *arg);
static GDataClass *alloc_GDataclass (EId id, BufferInfo *binfo);
static void free_GDataclass (GDataClass *cl, BufferInfo *binfo);
static GenericData *alloc_GenericData (EId id, GDataClass *cl, 
                                       BufferInfo *binfo);
static void free_GenericData (GenericData *gen, BufferInfo *binfo);
static Energize_Extent_Data *alloc_Energize_Extent_Data (EId id, BufferInfo *binfo);
static void free_Energize_Extent_Data (Energize_Extent_Data *ext, BufferInfo *binfo, 
                              enum Energize_Object_Free_Type free_type);
void energize_extent_finalization (EXTENT extent);
static BufferInfo *alloc_BufferInfo
(EId id, Lisp_Object name, Lisp_Object filename, 
 char *class_str, Editor *editor, Window win);
static void free_buffer_info (BufferInfo *binfo);
static BufferInfo *get_buffer_info_for_emacs_buffer (Lisp_Object emacs_buf, Editor *editor);
static long get_energize_buffer_id (Lisp_Object emacs_buf);
static char *kernel_buffer_type_to_elisp_type (char *kernel_type);
static Lisp_Object get_buffer_type_for_emacs_buffer 
(Lisp_Object emacs_buf, Editor *editor);
static Lisp_Object set_buffer_type_for_emacs_buffer 
(Lisp_Object emacs_buf, Editor *editor, Lisp_Object type);
static BufferInfo *get_buffer_info_for_id (EId id, Editor *editor);
static void put_buffer_info 
(EId id, Lisp_Object emacs_buf, BufferInfo *binfo, Editor *editor);
static void remove_buffer_info (EId id, Lisp_Object emacs_buf, Editor *editor);
static void Post (char *msg);
static void Post1 (char *msg, int a1);
static void Post2 (char *msg, int a1, int a2);
static EmacsPos EmacsPosForEnergizePos (EnergizePos energizePos);
static EnergizePos EnergizePosForEmacsPos (EmacsPos emacs_pos);
Lisp_Object Fenergize_update_menubar (Lisp_Object screen);
Lisp_Object Fenergize_extent_menu_p (Lisp_Object extent_obj);
void free_zombie_bitmaps (void);
static void ParseClasses (Connection *conn, unsigned int number, 
                          BufferInfo *binfo, unsigned int modify_ok);
static void ParseGenerics (Connection *conn, unsigned int number, 
                           BufferInfo *binfo, unsigned int modify_ok);
static void insert_one_extent (CExtent *ptr, BufferInfo *binfo, int modify_ok);
static void ParseExtents (Connection *conn, unsigned int number, 
                          BufferInfo *binfo, unsigned int modify_ok, 
                          int extent_offset);
static int string_buffer_compare (char *string, int string_len, 
                                  struct buffer *buf, int bufpos);
static void rename_the_buffer (Lisp_Object new_name);
static void ParseBuffer (Connection *conn, CBuffer *cbu, Editor *editor, 
                         EnergizePos delete_from, EnergizePos delete_to, 
                         Window win, int relative_p);
static void forget_buffer (BufferInfo *binfo);
static void WriteExtent (Connection *conn, Energize_Extent_Data *ext, 
                  unsigned int start, unsigned int end);
static char *get_buffer_as_string (unsigned int *len);
static int write_an_extent (Lisp_Object extent_obj, void *arg);
static void SaveBufferToEnergize (BufferInfo *binfo);
static Lisp_Object
get_energize_menu (Lisp_Object buffer, Lisp_Object extent_obj, int selection_p,
		   Lisp_Object only_name);
static int something_answered_p (void *arg);
void wait_delaying_user_input (int (*)(), void*);
Lisp_Object Fenergize_request_menu (Lisp_Object buffer, Lisp_Object extent);
Lisp_Object Fenergize_list_menu (Lisp_Object buffer, Lisp_Object extent_obj,
				 Lisp_Object selection_p,
				 Lisp_Object only_name);
Lisp_Object Fenergize_execute_menu_item 
(Lisp_Object buffer, Lisp_Object extent_obj, 
 Lisp_Object item, Lisp_Object selection, Lisp_Object no_confirm);
Lisp_Object Fenergize_buffer_type (Lisp_Object buffer);
Lisp_Object Fset_energize_buffer_type_internal (Lisp_Object buffer, 
                                                Lisp_Object type);
Lisp_Object Fenergize_buffer_p (Lisp_Object buffer);
Lisp_Object Fenergize_buffer_id (Lisp_Object buffer);
Lisp_Object Fenergize_request_kill_buffer (Lisp_Object buffer);
static void HandleControlChange (Widget, EId sheet_id, void* arg);
static void HandleLoggingRequest (Editor *editor, CLoggingRequest *creq);
static void HandleNewBufferRequest (Editor *editor, CNewBufferRequest *creq);
static void HandleModifyBufferRequest (Editor *editor, CModifyBufferRequest *creq);
static void MakeBufferAndExtentVisible (Lisp_Object list, Lisp_Object go_there);
static void HandleEnsureVisibleRequest (Editor *editor, CEnsureVisibleRequest *creq);
static void HandleEnsureManyVisibleRequest (Editor *editor, CEnsureManyVisibleRequest *creq);
static void HandleProposeChoicesRequest (Editor *editor, CProposeChoicesRequest *creq);
static void unmodify_buffer_and_kill_it (Lisp_Object buffer);
static void HandleKillBufferRequest (Editor *editor, CKillBufferRequest *creq);
static void HandleRemoveExtentsRequest (Editor *editor, CRemoveExtentsRequest *creq);
static void HandleSaveBufferRequest (Editor *editor, CSaveBufferRequest *creq);
static void HandleSetModifiedFlagRequest (Editor *editor, CSetModifiedFlagRequest *creq);
static void add_in_list_of_ids (int **ids, int *n_ids, int id);
static void remove_from_list_of_ids (int **ids, int *n_ids, int id);
static void HandleBufferSheetRequest (Editor *editor, CSheetRequest *sreq, EId buffer_id);
static void HandlePostitRequest (Editor *editor, CGenericRequest *preq);
static void HandleShowBusyRequest (Editor *editor, CGenericRequest *preq);
static void add_in_connection_input_buffer (Connection *conn, char *s, int l);
static Lisp_Object ProcessJustOneEnergizeRequest (void);
static Lisp_Object ProcessEnergizeRequest1 (void);
static void setup_connection (Editor *ed, unsigned int id1, unsigned int id2);
static Connection *EditorSideConnection (Editor *editor, int fdin, int fdout);
Lisp_Object Fhandle_energize_request (Lisp_Object proc, Lisp_Object string);
static void ConnectToEnergize (char *server_str, char *arg);
static void CloseConnection (void);
Lisp_Object Fconnect_to_energize_internal (Lisp_Object server_name, 
                                           Lisp_Object energize_arg);
Lisp_Object Fclose_connection_to_energize (void);
static void SendBufferModificationState (Editor *editor, BufferInfo *binfo, 
                                         int flag);
static int CheckBufferLock (Editor *editor, BufferInfo *binfo);
static Lisp_Object buffer_locked_p (Lisp_Object buffer);
static int notify_extent_modified (Lisp_Object extent_obj, void *arg);
static int ceiwme_lower_mf (EXTENT extent, void *arg);
static int ceiwme_upper_mf (EXTENT extent, void *arg);
static void coerce_endpoints_to_be_inside_warn_on_modify_extents 
(int *from_ptr,  int *to_ptr,  struct buffer *b);
static void mark_all_extents_as_unmodified (BufferInfo *binfo);
Lisp_Object Fsend_buffer_modified_request 
(Lisp_Object state, Lisp_Object from, Lisp_Object to);
Lisp_Object Fenergize_barf_if_buffer_locked (void);
Lisp_Object Fenergize_send_region (Lisp_Object start, Lisp_Object end);
Lisp_Object Fconnected_to_energize_p (void);
Lisp_Object Fenergize_user_input_buffer_mark (void);
Lisp_Object Fenergize_query_buffer (Lisp_Object filename, Lisp_Object just_ask);
static int get_energize_connection_and_buffer_id (Lisp_Object buffer, void **conn_ptr, long *buffer_id_ptr);
static int get_energize_connection_and_current_buffer_id (void **conn_ptr, long *buffer_id_ptr);
int *get_psheets_for_buffer (Lisp_Object buffer, int *count_ptr);
void notify_that_sheet_has_been_hidden (EId id);
void syms_of_editorside (void);



/**************************** Variables *****************************/

/* debugging variable */
int ignore_kernel;

Lisp_Object Venergize_kernel_modification_hook;
Lisp_Object Venergize_create_buffer_hook;

Lisp_Object Venergize_buffer_modified_hook;
Lisp_Object Qenergize_buffer_modified_hook;
Lisp_Object Qfirst_change_function;
Lisp_Object Qbefore_change_function;
Lisp_Object Qafter_change_function;
Lisp_Object Qbuffer_locked_by_kernel;
Lisp_Object Qdefault_directory;
Lisp_Object Qbuffer_file_name;
Lisp_Object Qenergize_user_input_buffer_mark;
Lisp_Object Qenergize_user_input_mode;
Lisp_Object Qenergize_make_many_buffers_visible;
Lisp_Object Qbuffer_undo_list;

extern Lisp_Object Qerror_conditions, Qerror_message;

int inside_parse_buffer;
Lisp_Object Qinside_parse_buffer;

/* List of all buffers currently managed by Energize.  This is
Staticpro'ed so that they don't get GC'ed from under us. */
static Lisp_Object energize_buffers_list;

static Editor *energize_connection;
static protocol_edit_options *peo;

static int request_serial_number;

extern int current_debuggerpanel_exposed_p;
extern int desired_debuggerpanel_exposed_p;
extern int debuggerpanel_sheet;

/* extern DisplayContext *display_context; */

/**************************** Macros *****************************/

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) <= (B) ? (A) : (B))
#define xnew(type) ((type*)xmalloc (sizeof (type)))

#define BUFFER_NOTIFY_BACKGROUND_BIT_SET_P(buffer) 1
  
#define get_extent_data(id,binfo) (Energize_Extent_Data*)get_object(id, binfo)
#define get_class(id,binfo) (GDataClass*)get_object(id, binfo)
#define get_generic(id,binfo) (GenericData*)get_object(id, binfo)

#define put_extent_data(id,binfo,obj) put_object(id, binfo, obj)
#define put_class(id,binfo,obj) put_object(id, binfo, obj)
#define put_generic(id,binfo,obj) put_object(id, binfo, obj)

#define remove_extent_data(id,binfo) remove_object(id, binfo)
#define remove_class(id,binfo) remove_object(id, binfo)
#define remove_generic(id,binfo) remove_object(id, binfo)

#define DEBUGGER_PSHEET_NAME  "DEBUGGER_P_SHEET"

/**************************** Utilities *****************************/

static int
emacs_CWriteRequestBuffer (Connection* conn)
{
  int result;
  signal (SIGPIPE, SIG_IGN);	/* don't kill emacs with SIGPIPE */
  result = CWriteRequestBuffer (conn);	/* the real one; macroized later */
  signal (SIGPIPE, SIG_DFL);
  return result;
}

#define CWriteRequestBuffer emacs_CWriteRequestBuffer


static Energize_Extent_Data *
extent_to_data (Lisp_Object extent_obj)
{
  Energize_Extent_Data *ext = 0;
  
  if (!EXTENTP (extent_obj)) 
    return 0;
  else
    ext = energize_extent_data (XEXTENT (extent_obj));
  
  if (ext)
    {
      if (EQ (ext->extent, extent_obj))
        return ext;
      else 
        abort ();
    }
  else
    return 0;
}


/* duplicate a string */
static char*
copy_string (char *s)
{
  if (!s)
    return 0;
  else
    {
      int len = strlen (s);
      char *res = (char *) xmalloc (len + 1);
      return strcpy (res, s);
    }
}

/* Get objects from the hashtables */
static void *
get_object_internal (EId id, c_hashtable table)
{
  void *res;
  void *found = gethash ((void*)id, table, &res);
  
  if (found) CHECK_OBJECT (res, 0);

  return found ? res : 0;
}

static void *
get_object (EId id, BufferInfo *binfo)
{
  return get_object_internal (id, binfo->id_to_object);
}

static void
put_object_internal (EId id, c_hashtable table, void *object)
{
  if (!PUT_ABLE_OBJECT (object))
    error ("Can't put 0x%x in table", object);
  CHECK_OBJECT (object, 0);
  puthash ((void*)id, object, table);
}

static void
put_object (EId id, BufferInfo *binfo, void *object)
{
  put_object_internal (id, binfo->id_to_object, object);
  return;
}

static void
remove_object_internal (EId id, c_hashtable table)
{
  void *res;

  if (gethash ((void*)id, table, &res))
    {
      if (OBJECT_FREE (res))
        error ("Free'd object 0x%x still in table!", res);
      remhash ((void*)id, table);
    }
  else if (id)
    error ("EId %d not in table!", id);
}

static void
remove_object (EId id, BufferInfo *binfo)
{
  remove_object_internal (id, binfo->id_to_object);
  return;
}

/* maphash_function called by free_buffer_info */
static void
free_object (void *key, void *contents, void *arg)
{
  BufferInfo *binfo = arg;

  if (contents)
    {
      switch (OBJECT_SEAL (contents))
        {
        case BUF_INFO_SEAL:
          break;
        case EXTENT_SEAL:
          free_Energize_Extent_Data ((Energize_Extent_Data *) contents, binfo, OFT_MAPHASH);
          break;
        case GDATA_CLASS_SEAL:
          free_GDataclass ((GDataClass *) contents, binfo);
          break;
        case GDATA_SEAL:
          free_GenericData ((GenericData *) contents, binfo);
          break;
        default:
          error ("Bad argument 0x%x to free_object()", contents);
          return;
        }
    }
}

static GDataClass *
alloc_GDataclass (EId id, BufferInfo *binfo)
{
  GDataClass *cl = xnew (GDataClass);
  memset (cl, 0, sizeof (GDataClass));
  cl->seal = GDATA_CLASS_SEAL;
  cl->id = id;
  put_class (cl->id, binfo, cl);
  return cl;
} 

static void
free_GDataclass (GDataClass *cl, BufferInfo *binfo)
{
  if (cl)
    {
      remove_class (cl->id, binfo);
      SET_OBJECT_FREE (cl);
    }
  return;
} 


static GenericData *
alloc_GenericData (EId id, GDataClass *cl, BufferInfo *binfo)
{
  GenericData *gen = xnew (GenericData);
  gen->seal = GDATA_SEAL;
  gen->id = id;
  gen->cl = cl;
/*  gen->image = 0;*/
  gen->flags = 0;
  gen->modified_state = 0;
  put_generic (gen->id, binfo, gen);
  return gen;
}
          
static void
free_GenericData (GenericData *gen, BufferInfo *binfo)
{
  if (gen)
    {
      remove_generic (gen->id, binfo);
      gen->cl = 0;
      SET_OBJECT_FREE (gen);
    }
  return;
} 

static Energize_Extent_Data *
alloc_Energize_Extent_Data (EId id, BufferInfo *binfo)
{              
  Energize_Extent_Data *ext = xnew (Energize_Extent_Data);
  ext->seal = EXTENT_SEAL;
  ext->id = id;
  ext->extent = 0;
  put_extent_data (ext->id, binfo, ext);
  return ext;
}

static void
free_Energize_Extent_Data 
(Energize_Extent_Data *ext, BufferInfo *binfo, enum Energize_Object_Free_Type free_type)
{              
  if (ext)
    {
      Lisp_Object extent_obj = ext->extent;
      ext->extent = 0;

      if (extent_obj && ext == energize_extent_data (XEXTENT (extent_obj)))
        {
          detach_extent (XEXTENT(extent_obj));
	  set_energize_extent_data (XEXTENT (extent_obj), 0);
        }
      remove_extent_data (ext->id, binfo);
      ext->id = 0;

      /* don't free this "sub-guy" via maphash, as it will get taken care
         of during the course of the maphash without our doing anything */
      if (free_type != OFT_MAPHASH)
        {
          if (ext->extentType == CEGeneric)
            free_GenericData (ext->u.generic.gData, binfo);
        }

      SET_OBJECT_FREE (ext);
    }
  return;
}

/* called from the GC when an extent is returned to the free list */
void
energize_extent_finalization (EXTENT extent)
{
  Energize_Extent_Data *ext = energize_extent_data (extent);
  Lisp_Object buffer = extent->buffer;
  BufferInfo *binfo;

  /* the if 0 case bypasses the freeing of extentData objects.  It's a big
     memory leak but could be causing some problems. */
#if 0
  if (ext)
    {
      ext->extent = 0;
      set_energize_extent_data (extent, 0);
    }
  return;
#else
  if (!ext || (!BUFFERP (buffer)))
    return;

  binfo = 
    energize_connection ?
      get_buffer_info_for_emacs_buffer (buffer, energize_connection) : 0;
    
  if (binfo && ext)
    free_Energize_Extent_Data (ext, binfo, OFT_GC);
  else
    ext->extent = 0;

  set_energize_extent_data (extent, 0);
  return;
#endif
}

extern int find_file_compare_truenames;

static BufferInfo *
alloc_BufferInfo (EId id, Lisp_Object name, Lisp_Object filename,
		  char *class_str, Editor *editor, Window win)
{
  BufferInfo *binfo = xnew (BufferInfo);
  Widget nw = 0;
  Lisp_Object buffer = Qnil;

  if (win){
    char win_as_string [16];
    nw = XtWindowToWidget (x_current_display, win);
    if (nw)
     nw = XtParent (nw);

    if (nw)
      sprintf (win_as_string, "w%x", nw);
    else
      sprintf (win_as_string, "0x%x", win);

    binfo->screen =
      Fx_create_screen (Qnil, build_string (win_as_string));
  }else
    binfo->screen = Qnil;

  /* try to re-use a buffer with the same file name if one already exists.
   * If a buffer already exists but is modified we should do a dialog and
   * ask the user what to do.  For now I'll just use a new buffer in that case.
   * ParseBuffer will erase the buffer.
   **/
  if (!NILP (filename))
    {
      int offct = find_file_compare_truenames;
      find_file_compare_truenames = 1;
      buffer = Fget_file_buffer (filename);
      find_file_compare_truenames = offct;

      if (!NILP (buffer) && !NILP (Fbuffer_modified_p (buffer)))
	buffer = Qnil;
    }

  if (NILP (buffer))
    buffer = Fget_buffer_create (Fgenerate_new_buffer_name (name));

  binfo->seal = BUF_INFO_SEAL;
  binfo->id = id;
  binfo->flags = 0;
  binfo->editor = editor;
  binfo->id_to_object = make_hashtable (100);
  binfo->emacs_buffer = buffer;
  binfo->modified_state = 0;
  binfo->editable = 0;
  binfo->output_mark = Qnil;
  binfo->buffer_type = kernel_buffer_type_to_elisp_type (class_str);
  binfo->p_sheet_ids = 0;
  binfo->n_p_sheets = 0;
  binfo->note_ids = 0;
  binfo->n_notes = 0;
  put_buffer_info (id, binfo->emacs_buffer, binfo, editor);

  energize_buffers_list = Fcons (buffer, energize_buffers_list);

#if 0
 *  if (nw){
 *    Lisp_Object window = Fscreen_selected_window (binfo->screen);
 *    Fset_window_buffer (window, binfo->emacs_buffer);
 *    BLOCK_INPUT;
 *    set_text_widget ((NoteWidget)nw,
 *		     XSCREEN(binfo->screen)->display.x->widget);
 *    UNBLOCK_INPUT;
 *  }
#endif

  return binfo;
}

/* free a buffer_info */
static void
free_buffer_info (BufferInfo *binfo)
{
  maphash (free_object, binfo->id_to_object, (void *)binfo);
  free_hashtable (binfo->id_to_object);
  if (energize_connection && energize_connection->binfo_hash) 
    {
      if (binfo->id)
        remhash ((void *)binfo->id, energize_connection->binfo_hash);
      if (binfo->emacs_buffer)
        remhash ((void *)binfo->emacs_buffer, energize_connection->binfo_hash);
    }
  binfo->id = 0;
  binfo->emacs_buffer = 0;
  SET_OBJECT_FREE (binfo);
}

/* hash for BufferInfo structures */
static BufferInfo*
get_buffer_info_for_emacs_buffer (Lisp_Object emacs_buf, Editor *editor)
{
  BufferInfo *res;
  if (!editor || !editor->binfo_hash)
    return 0;
  else
    return (gethash ((void *)emacs_buf, editor->binfo_hash, (void *)&res)
	    ? res : 0);
}


struct buffer_and_sheet_ids
{
  EId	buffer_id;
  EId	sheet_id;
};

static void
find_sheet_id (void* key, void* contents, void* arg)
{
  BufferInfo* binfo = (BufferInfo*)contents;
  EId buffer_id = (EId)key;
  struct buffer_and_sheet_ids* result = (struct buffer_and_sheet_ids*)arg;
  int i;

  if (!result->buffer_id)
    for (i = 0; i < binfo->n_p_sheets; i++)
      if (binfo->p_sheet_ids [i] == result->sheet_id)
	{
	  result->buffer_id = buffer_id;
	  return;
	}
}

static EId
buffer_id_of_sheet (EId id)
{
  Editor *editor = energize_connection;
  struct buffer_and_sheet_ids basi;
  if (!energize_connection)
    return 0;

  basi.buffer_id = 0;
  basi.sheet_id = id;
  maphash (find_sheet_id, editor->binfo_hash, (void*)&basi);
  return basi.buffer_id;
}

static long
get_energize_buffer_id (Lisp_Object emacs_buf)
{
  Editor *editor = energize_connection;
  BufferInfo *res;
  if (!editor || !editor->binfo_hash)
    return -1;
  else if (!gethash ((void *)emacs_buf, editor->binfo_hash, (void *)&res))
    return -1;
  else
    return (long) res->id;
}

static char *
kernel_buffer_type_to_elisp_type (char *kernel_type)
{
  struct buffer_type_struct *bts = 
    kernel_buffer_types_to_elisp_buffer_types_vector;
  char *elisp_type = 0;

  if (!kernel_type)
    return UNINITIALIZED_BUFFER_TYPE;

  while (bts->kernel_name)
    {
      if (!strcmp (bts->kernel_name, kernel_type))
        {
          elisp_type = bts->elisp_name;
          break;
        }
      bts++;
    }
  
  if (!elisp_type)
    return kernel_type;
  else
    return elisp_type;
}

static Lisp_Object
get_buffer_type_for_emacs_buffer (Lisp_Object emacs_buf, Editor *editor)
{
  BufferInfo *binfo;
  if (!(binfo = get_buffer_info_for_emacs_buffer (emacs_buf, editor)))
    return Qnil;
  else
    {
      if (!binfo->buffer_type) binfo->buffer_type = 
        UNINITIALIZED_BUFFER_TYPE;

      return intern (binfo->buffer_type);
    }
}

static Lisp_Object
set_buffer_type_for_emacs_buffer (Lisp_Object emacs_buf, Editor *editor,
                                    Lisp_Object type)
{
  BufferInfo *binfo;
  if (!(binfo = get_buffer_info_for_emacs_buffer (emacs_buf, editor)))
    return Qnil;
  else
    {
      char *type_string;

      if (NILP (type)) return Qnil;
        
      if (SYMBOLP (type))
        XSET (type, Lisp_String, XSYMBOL (type)->name);

      if (STRINGP (type)) 
        type_string = (char *)XSTRING (type)->data;

      type_string = copy_string (type_string);

      if (!type_string) return Qnil;

      binfo->buffer_type = type_string;

      return intern (binfo->buffer_type);
    }
}

static BufferInfo*
get_buffer_info_for_id (EId id, Editor *editor)
{
  BufferInfo *res;
  return (gethash ((void *)id, editor->binfo_hash, (void *)&res))?res:0;
}

static void
put_buffer_info (EId id, Lisp_Object emacs_buf, BufferInfo *binfo,
                 Editor *editor)
{
  puthash ((void *)id, binfo, editor->binfo_hash);
  puthash ((void *)emacs_buf, binfo, editor->binfo_hash);
}

static void
remove_buffer_info (EId id, Lisp_Object emacs_buf, Editor *editor)
{
  remhash ((void *)id, editor->binfo_hash);
  remhash ((void *)emacs_buf, editor->binfo_hash);
}

/* Display messages in minibuffer */
static void
  Post (char *msg) 
{
  message (msg, 0, 0, 0);
}

static void
  Post1 (char *msg, int a1) 
{
  message (msg, a1, 0, 0);
}

static void
Post2 (char *msg, int a1, int a2) 
{
  message (msg, a1, a2, 0);
}


/* Conversion between Energize and Emacs buffer positions */

static EmacsPos
EmacsPosForEnergizePos (EnergizePos energizePos)
{
  return ((energizePos >= (1 << VALBITS))?Z:(energizePos + 1));
}

static EnergizePos
EnergizePosForEmacsPos (EmacsPos emacs_pos)
{
  return (emacs_pos - 1);
}


/* extent data */

Energize_Extent_Data *
energize_extent_data (EXTENT extent)
{
  Lisp_Object data = extent->user_data;
  if (!CONSP (data) ||
      !EQ (XCONS (data)->car, Qenergize_extent_data))
    return 0;
  data = XCONS (data)->cdr;
  if (!FIXNUMP (XCONS (data)->car) || !FIXNUMP (XCONS (data)->cdr))
    abort ();
  return ((Energize_Extent_Data *)
	  ((XUINT (XCONS (data)->car) << 16) | (XUINT (XCONS (data)->cdr))));
}

void
set_energize_extent_data (EXTENT extent, void *data)
{
  unsigned short high = ((unsigned int) data) >> 16;
  unsigned short low = ((unsigned int) data) & 0xFFFF;
  Lisp_Object old = extent->user_data;
  if (old == 0)
    extent->user_data = Qnil;
  else if (NILP (old))
    extent->user_data = Fcons (Qenergize_extent_data,
			       Fcons (make_number (high), make_number (low)));
  else if (!CONSP (old) || !EQ (XCONS (old)->car, Qenergize_extent_data))
    abort ();
  else
    {
      XFASTINT (XCONS (XCONS (old)->cdr)->car) = high;
      XFASTINT (XCONS (XCONS (old)->cdr)->cdr) = low;
    }
}



extern int windows_or_buffers_changed;

extern void recompute_screen_menubar (struct screen *);

DEFUN ("energize-update-menubar", Fenergize_update_menubar,
       Senergize_update_menubar, 0, 1, 0,
       "Updates the menubar of optional SCREEN argument to reflect the \n\
commands currently enabled by Energize.")
     (screen)
     Lisp_Object screen;
{
  struct screen* s;

  if (!energize_connection) return Qnil;
  
  if (NILP (screen))
    s = selected_screen;
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }
  
  if (!SCREEN_IS_X (s))
    error ("not an X screen");

  recompute_screen_menubar (selected_screen);
  return Qnil;
}


/* Return true if the extent is an Energize extent that can have a menu */
DEFUN ("energize-extent-menu-p", Fenergize_extent_menu_p,
       Senergize_extent_menu_p, 1, 1, 0,
       "Return true if the extent has a set of commands defined by Energize")
     (extent_obj)
     Lisp_Object extent_obj;
{
  CHECK_EXTENT (extent_obj, 0);

  if (NILP (Fconnected_to_energize_p())) 
    return Qnil;
  else
    {
      Energize_Extent_Data *ext = extent_to_data (extent_obj);
      return (ext && ext->extentType == CEGeneric) ? Qt : Qnil;
    }
}


/* Do what is needed so that the delayed requests will be notified by
** the event loop */

static void 
notify_delayed_requests ()
{
  extern void mark_process_as_being_ready (struct Lisp_Process* process);

  if (energize_connection && !NILP (energize_connection->proc) && energize_connection->conn
      && CRequestDelayedP (energize_connection->conn))
    mark_process_as_being_ready (XPROCESS (energize_connection->proc));
}


/******************* IMAGE storage maintenance *******************/

/* called by redisplay() -- presumably by the time this is called,
   all "dead" references into the x_bitmaps[] vector have been flushed
   from the display state */
void
free_zombie_bitmaps ()
{
}


extern struct x_pixmap *x_get_pixmap (Lisp_Object lisp_name, char *hash);

static c_hashtable image_cache;

/* Parses an image from the image language */
static GLYPH
ParseAnImage (Connection *conn, BufferInfo *binfo)
{
  ReqLen l;
  char *s = CGetVstring (conn, &l);
  char pix_name [255];
  char buf [255];
  int attr_number, pix_len;
  char *file;
  GLYPH result = 0;
  /* I don't know if it's ok to pass the address of a short to gethash... */
  int hashed = 0;

  if (s[0] != 'f')
    return 0;

  if (gethash ((void *) s, image_cache, (void *) &hashed))
    /* If we have already parsed this image spec (string=) then just return
       the old glyph, instead of calling the lisp code, x_get_pixmap, and
       XtGetSubResources again.  The result may be 0 if there is no pixmap
       file name in the resource database.
     */
    return (GLYPH) hashed;

  if (3 != sscanf (s, "f %d p %d %s", &attr_number, &pix_len, pix_name))
    {
      sprintf (buf, "unparsable image: \"%s\"", s);
      error (buf);
    }

  if (pix_len != strlen (pix_name))
    abort ();

  /* Read the pixmap file name for this image from the resource db */
  {
    XtResource resource [1];
    resource[0].resource_name = pix_name;
    resource[0].resource_class = XtCBitmap;
    resource[0].resource_type = XtRString;
    resource[0].resource_size = sizeof (char *);
    resource[0].resource_offset = 0;
    resource[0].default_type = XtRImmediate;
    resource[0].default_addr = 0;
    file = 0;
    BLOCK_INPUT;
    XtGetSubresources (selected_screen->display.x->widget, (XtPointer) &file,
		       "image", "Image", resource, 1, NULL, 0);
    UNBLOCK_INPUT;
  }

  if (! file)
    result = 0;
  else
    {
      struct x_pixmap *p;
      sprintf (buf, "attribute%d", attr_number);
      p = x_get_pixmap (build_string (file), buf);
      result = p->glyph_id;

      if (p->depth == 0)
	/* if depth is >0 then this is an XPM, and its colors are not
	   controlled by the fg/bg of a face.  So don't bother making a
	   face for it. */
	{
	  Lisp_Object face, face_id;
	  /* run the lisp code to make a face for this image. */
	  face = call1 (intern ("find-face"), intern (buf));
	  if (NILP (face))
	    {
	      call1 (intern ("make-face"), intern (buf));
	      face = call1 (intern ("find-face"), intern (buf));
	      if (NILP (face)) error ("make-face is broken");
	    }
	  face_id = call1 (intern ("face-id"), face);
	  CHECK_FIXNUM (face_id, 0);
	  p->face_id = XINT (face_id);
	}
    }

  BLOCK_INPUT;
  /* CGetVstring returns a pointer into the connection buffer; we need to
     copy it to use it as a hash key. */
  s = strdup (s);
  UNBLOCK_INPUT;

  hashed = result;
  puthash ((void *) s, (void *) hashed, image_cache);
  return result;
}


/* Parses Classes from the connection buffer.  Defines them for
 * the buffer given as argument */
static void
ParseClasses (Connection *conn, unsigned int number, BufferInfo *binfo,
              unsigned int modify_ok)
{
  CClass *ptr;                  /* pointer to class data in buffer */
  GDataClass *cl;               /* unmodified class data */
  GLYPH g;
  int i;
  
  for (i = 0; i < number; i++)
    {
      ptr = CGet (conn, CClass);
      g = ParseAnImage (conn, binfo);
      cl = get_class (ptr->classId, binfo);
      
      if (!cl)
        cl = alloc_GDataclass (ptr->classId, binfo);
      else if (!modify_ok)
        Post1("Attempt to create class with existing Id %8x", ptr->classId);
      
      if (ignore_kernel) continue;

      /* if it did exist, we just clobber it */
      if (cl->flags != ptr->flags)
	{
	  cl->flags = ptr->flags;
	  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;
	}
      if (cl->glyph != g)
        {
	  cl->glyph = g;
	  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;
        }
    }
}

/* Parse GenericData form the connection buffer.  Defines them for the buffer
 * given as argument */
static void
ParseGenerics (Connection *conn, unsigned int number, 
                 BufferInfo *binfo, unsigned int modify_ok)
{
  CGeneric *ptr;
  GenericData *gen;
  GDataClass *cl;
  GLYPH g;
  int i;
  
  for (i = 0; i < number; i++)
    {
      ptr = CGet (conn, CGeneric);
      g = ParseAnImage (conn, binfo);
      gen = get_generic (ptr->genericId, binfo);
      cl = get_class (ptr->classId, binfo);
      
      if (!gen)
        { 
          /* create generic if it didn't already exist */
          
          if (!cl)
            {
              Post2 ("Attempt to create generic %8x with undefined class %8x",
                     ptr->genericId, ptr->classId);
              continue;
            }
          
          gen = alloc_GenericData (ptr->genericId, cl, binfo);
	  gen->glyph = g;
	  if (ptr->flags != 0xff) gen->flags = ptr->flags;
	  gen->attribute = ptr->attribute;
        }
      else if (!modify_ok)
        Post1("Attempt to create generic with existing id %8x", 
              ptr->genericId);
      else{
	/* modify the generic */
	int modified = 0;
	if (cl != gen->cl)
	  {
	    modified = 1;
	    gen->cl = cl;
	  }
	if (gen->glyph != g)
	  {
	    modified = 1;
	    gen->glyph = g;
	  }
	if (ptr->flags != 0xff) 
	  {
	    modified = 1;
	    gen->flags = ptr->flags;
	  }
	if (gen->attribute != ptr->attribute)
	  {
	    modified = 1;
	    gen->attribute = ptr->attribute;
	  }
	if (modified)
	  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;
      }
    }
}

static void
insert_one_extent (CExtent* ptr, BufferInfo* binfo, int modify_ok)
{
  Energize_Extent_Data *ext;
  GenericData *gen;
  int extent_start;
  int extent_end;
  int set_extent_endpoints = 1;

  ext = get_extent_data (ptr->extentId, binfo);
  
  if (!ext)
    ext = alloc_Energize_Extent_Data (ptr->extentId, binfo);
  else if (!modify_ok) 
    Post1 ("Creating extent with existing id %8x", ptr->extentId);

  ext->extentType = ptr->extentType;
  
  if (ignore_kernel) return;
  
  switch (ptr->extentType)
    {
    case CEAttribute:
      ext->u.attr.attrValue = ptr->data;
      break;
      
    case CEAbbreviation:
      ext->u.abbrev.isOpened = ptr->data;
      break;
      
    case CEWriteProtect:
      break;
      
    case CEGeneric:
      gen = get_generic (ptr->data, binfo);
      if (!gen) 
	{
	  Post1 ("NewExtents: Nonexistent generic data %8x", ptr->data);
	  return;
	}
      ext->u.generic.gData = gen;
      break;
      
    default:
      Post1 ("Unknown extent type %d", ptr->extentType);
      break;
    }
  
  /* instruct redisplay to recompute the screen */
  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;
  
  /* ptr->startPosition == ptr->endPosition == ~0 means to not change
   * the extent endpoints */
  if (ptr->startPosition == ~0 && ptr->endPosition == ~0)
    {
      set_extent_endpoints = 0;
      extent_start = ~0;
      extent_end = ~0;
    }
  else
    {
      struct buffer *b = XBUFFER (binfo->emacs_buffer);
      extent_start = EmacsPosForEnergizePos (ptr->startPosition);
      extent_end = EmacsPosForEnergizePos (ptr->endPosition);
      
      if (extent_end >= BUF_ZV (b))
	{
	  extent_end = BUF_ZV (b) - 1;
	  if (extent_start >= extent_end)
	    extent_start = extent_end - 1;
	}
    }      

  /* no zero width extent */
  if (set_extent_endpoints && extent_start == extent_end)
    extent_end += 1;

  /* this function calls update_extent if the the ext->extent already
     exists */

  make_extent_for_data (binfo, ext, extent_start, extent_end,
			set_extent_endpoints);
}

/* Parse GenericData from the connection buffer.  Defines them for the buffer
 * given as argument.  Creates the Emacs extents while parsing.
 * Energize sends the extents ordered by increasing starting position.
 * Emacs is __much__ faster at inserting them in decreasing starting position
 * also for overlaps to work correctly the outmost extents have to be
 * inserted first.  This is what the recursive function is trying to do.
 */
static void
ParseExtents (Connection *conn, unsigned int number, 
              BufferInfo *binfo, unsigned int modify_ok, int extent_offset)
{
  CExtent* all_extents;
  int i;

  /* Gets the extents from the connection */
  all_extents = CGetN (conn, CExtent, number);

  /* adjusts the endpoints with the offset */
  for (i = 0; i < number; i++)
    {
      if (all_extents [i].startPosition != ~0)
	all_extents [i].startPosition += extent_offset;
      if (all_extents [i].endPosition != ~0)
	all_extents [i].endPosition += extent_offset;
    }
  
  /* inserts them all */
  for (i = number - 1; i >= 0; i--)
    {
      insert_one_extent (all_extents + i, binfo, modify_ok);
    }
}

/* Parses a CBuffer in the connection stream. If (delete_from != delete_to) 
   all characters in this range must be deleted.
   */

static int
string_buffer_compare (char *string, int string_len, 
                       struct buffer *buf, int bufpos)
{
  int first_section_length =
    (bufpos < BUF_GPT(buf))?(min (string_len, (BUF_GPT(buf) - bufpos))):0;
  int second_section_length = 
    ((bufpos + string_len) >= BUF_GPT(buf))?
      (string_len - first_section_length):0;
  
  /* degenerate case, which we consider to be "true" */
  if (string_len == 0) return 0;

  /* string won't fit in the buffer, so comparison fails */
  if (BUF_Z(buf) < (bufpos + string_len)) return -1;

  /* bad starting position, so comparison fails */
  if (bufpos < BUF_BEG (buf)) return -1;

  if (first_section_length > 0)
    /* there is a first section */
    {
      char *first_section_chars = (char *) BUF_CHAR_ADDRESS (buf, bufpos);
      int comp = strncmp (string, first_section_chars, first_section_length);

      if (comp) return comp;
    }

  if (second_section_length > 0)
    /* there is a second section */
    {
      char *second_section_chars = 
        (char *) BUF_CHAR_ADDRESS (buf, BUF_GPT(buf));
      int comp = strncmp (string + first_section_length, 
                          second_section_chars, second_section_length);

      if (comp) return comp;
    }

  return 0;
}

/* called by unwind protect, from within ParseBuffer and HandleRemoveExtents */
static Lisp_Object
restore_buffer_state (Lisp_Object state_cons)
{
  BufferInfo *binfo;
  Lisp_Object bufferId_cons = Fcar (state_cons);
  unsigned int bufferId = lisp_to_word (bufferId_cons);
  Lisp_Object buffer_modified_state = Fcar (Fcdr (state_cons));
  Lisp_Object clear_undo_list = Fcdr (Fcdr (state_cons));
  
  if (bufferId != 0)
    {
      if (energize_connection && (binfo = get_buffer_info_for_id (bufferId, energize_connection))
          && binfo->emacs_buffer)
	{
	  /* Always ignore what Energize tells us about the buffer read-only
	     state.  For files Emacs knows better and for non-file buffers
	     Emacs is hacking the read-only state anyway so let it be. */
	  XBUFFER(binfo->emacs_buffer)->read_only = buffer_modified_state;
          if (!NILP (clear_undo_list))
            XBUFFER(binfo->emacs_buffer)->undo_list = Qnil;
	}
    }
  else
    /* this is just temporary */
    Post("Bad bufferId cons cell!");
  return Qnil;
}

static void
rename_the_buffer (Lisp_Object new_name)
{
  int count = 0;
  char number [8];
  struct gcpro gcpro1;
  
  Lisp_Object name = new_name;
  GCPRO1 (name);
  while (!NILP (Fget_buffer (name)))
    {
      sprintf (number, "<%d>", ++count);
      name = concat2 (new_name, build_string (number));
    }
  Frename_buffer (name, Qnil);
  UNGCPRO;
}

Lisp_Object
safe_funcall_hook (Lisp_Object hook, int nargs, Lisp_Object arg1,
		   Lisp_Object arg2, Lisp_Object arg3);

extern void del_range (int, int);

extern Lisp_Object Fdelete_extent (Lisp_Object);

static int
destroy_if_energize_extent (EXTENT ext, void* arg)
{
  if (energize_extent_data (ext))
    {
      Lisp_Object extent;
      XSET (extent, Lisp_Extent, ext);
      Fdelete_extent (extent);
    }
  return 0;
}

/*extern void process_extents_for_destruction (int, int, struct buffer *);*/
static void
destroy_all_energize_extents (struct buffer* buf)
{
/*  if (energize_font_lock_p)*/
    map_extents (BUF_BEG (buf), BUF_Z (buf), NULL, destroy_if_energize_extent,
		 NULL, buf, 1);
/*  else
    process_extents_for_destruction (BUF_BEG (buf), BUF_Z (buf), buf);*/
}

static void
ParseBuffer (Connection *conn, CBuffer *cbu, Editor *editor, 
             EnergizePos delete_from, EnergizePos delete_to, Window win, int relative_p)
{
  char *name;
  ReqLen name_len;
  char *pathname_str;
  ReqLen pathname_len;
  char *buffer_class_str;
  ReqLen buffer_class_len;
  Lisp_Object pathname = Qnil;
  Lisp_Object pathname_directory = Qnil;
  Lisp_Object buffer_name = Qnil;
  Lisp_Object filename = Qnil;
#if 1
  Lisp_Object display_window = Qnil;
#endif
  BufferInfo *binfo;
  int modifying_p = 0;
  EmacsPos previous_point;
  EmacsPos from;
  EmacsPos to;
#if 1
  EmacsPos display_start = 1;
#endif
  char *text;
  ReqLen text_len;
  Lisp_Object modified_buffer_flag;
  int count = specpdl_ptr - specpdl;
  int extent_offset;
  Lisp_Object restore_buffer_state_cons;
  int should_keep_window_start = 1;
  int no_text_deleted = 0;

  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;  

  record_unwind_protect (save_restriction_restore, save_restriction_save ());

  Fwiden ();

  GCPRO4 (buffer_name, pathname, pathname_directory, filename);

  name = CGetVstring (conn, &name_len);

  /* read the pathname and buffer-class -- Editor Protocol > 0 */
  pathname_str = CGetVstring (conn, &pathname_len);
  buffer_class_str = CGetVstring (conn, &buffer_class_len);

  if (name_len)
    buffer_name = build_string (name);
  if (pathname_len)
    pathname = build_string (pathname_str);
  
  /* set up pathname_directory */
  if (!NILP (pathname))
    {
      if (NILP (Ffile_directory_p (pathname)))
        pathname_directory = Ffile_name_directory (pathname);
      else
        pathname_directory = pathname;
    }

  /* make sure that pathname_directory ends with a '/', if it exists */
  if (!NILP (pathname_directory))
    {
      char *str = (char *) XSTRING (pathname_directory)->data;
      int size = XSTRING (pathname_directory)->size;
      if (str[size - 1] != '/')
        {
          Lisp_Object tmp = make_string (str, size + 1);
          XSTRING (tmp)->data[size] = '/';
          pathname_directory = tmp;
        }
    }


  /* get or create the BufferInfo */
  if (binfo = get_buffer_info_for_id (cbu->bufferId, editor)) 
    modifying_p = 1;
  else
    {      
      if (NILP (buffer_name))
        {
          char *dummy = "*Unnamed Energize Buffer*";
          buffer_name = build_string (dummy);
        }
      /* create new buffer */
      binfo = alloc_BufferInfo (cbu->bufferId, buffer_name, pathname,
				buffer_class_str, editor, win);
      XBUFFER (binfo->emacs_buffer)->read_only =
	cbu->flags == CBReadOnly ? Qt : Qnil;
    }
  
  /* remember where we were in which buffer before we change things */
  if (current_buffer != XBUFFER (binfo->emacs_buffer))
    {
      record_unwind_protect (save_excursion_restore, save_excursion_save ());
      Fset_buffer (binfo->emacs_buffer);
    }

  /* set default-directory */
  if (!NILP (pathname_directory))
    {
      if (!NILP (Ffile_directory_p (pathname_directory))
	  && !NILP (Ffile_executable_p (pathname_directory)))
	Fset (Qdefault_directory, pathname_directory);
      else
      	Fset (Qdefault_directory, Qnil);
    }

  /* set file name unless it's a directory */
  if (!NILP (pathname) && NILP (Ffile_directory_p (pathname)))
    {
      filename = Fexpand_file_name (pathname, Qnil);
      Fset (Qbuffer_file_name, filename);
    }

  /* set buffer name */
  if (!NILP (buffer_name))
    {
      if (modifying_p
	  && strcmp ((char*)XSTRING (buffer_name)->data,
		     (char*)
                     XSTRING (XBUFFER (binfo->emacs_buffer)->name)->data))
	rename_the_buffer (buffer_name);
    }
  
  if (modifying_p)
    {
      safe_funcall_hook (Venergize_kernel_modification_hook, 0, 0, 0, 0);
      /* Make sure buffer is current after the hook */
      Fset_buffer (binfo->emacs_buffer);
    }

  modified_buffer_flag = Fbuffer_modified_p (binfo->emacs_buffer);

  /* enables buffer edits */
  restore_buffer_state_cons =
    Fcons (word_to_lisp (cbu->bufferId),
	   Fcons (XBUFFER(binfo->emacs_buffer)->read_only, Qt));
  record_unwind_protect (restore_buffer_state, restore_buffer_state_cons);
  XBUFFER(binfo->emacs_buffer)->read_only = Qnil;
  
  /* any changes here should take place "underneath" these hooks, I think */
  specbind (Qenergize_buffer_modified_hook, Qnil);
  specbind (Qfirst_change_function, Qnil);
  specbind (Qbefore_change_function, Qnil);
  /* As energize does not use the after-change-function it's not useful to
     bind it to NIL */
  /* specbind (Qafter_change_function, Qnil); */
  specbind (Qinside_parse_buffer, Qt);
  specbind (Qbuffer_undo_list, Qt);

  XBUFFER (binfo->emacs_buffer)->undo_list = Qt;
  
  /* EmacsPosForEnergizePos uses the current-buffer */
  from = EmacsPosForEnergizePos (delete_from);
  to = EmacsPosForEnergizePos (delete_to);

  /* get text */
  text = CGetVstring (conn, &text_len);

  if (modifying_p && (from != to || text_len))
    /* updates the visited file modtime */
    Fset_buffer_modtime (binfo->emacs_buffer, Qnil);

  if (!modifying_p)
    {
      /* clears the buffer in case we re-use a non-energize buffer */
      previous_point = 1;
      Fset_buffer (binfo->emacs_buffer);
      del_range (BEG, Z);
    }
  else
    {
#if 1
      display_window = Fget_buffer_window (binfo->emacs_buffer, Qnil, Qnil);
#endif
      previous_point = point;

#if 1
      if (!NILP (display_window))
        display_start = Fmarker_position (XWINDOW (display_window)->start);
#endif

      if (from != to)
        {
          struct buffer *buf = XBUFFER (binfo->emacs_buffer);
                        
	  Fset_buffer (binfo->emacs_buffer);
          if (!NILP (binfo->output_mark)
              && marker_position (binfo->output_mark) >= from)
            Fset_marker (binfo->output_mark, make_number (from),
                         binfo->emacs_buffer);
          if (((to - from) == text_len) &&
              !string_buffer_compare (text, text_len, buf, from))
            /* the new text is the same as the old text, don't clear
               the undo list*/
            {
              Fsetcdr (Fcdr (restore_buffer_state_cons), Qnil);
              no_text_deleted = 1;
	      destroy_all_energize_extents (buf);
            }
	  else
            {
              /* Do not keep window start if we actually delete text */
              should_keep_window_start = 0;
	      Fset_buffer (binfo->emacs_buffer);
	      destroy_all_energize_extents (buf);
              del_range (from, to);
            }
        }
      else if (!text_len)
        /* if there is no text and we didn't delete anything, 
           don't clear the undo_list slot */
        Fsetcdr (Fcdr (restore_buffer_state_cons), Qnil);
    }
  
  /* buffer type */
  if (cbu->flags != 0xff && cbu->flags != binfo->flags)
    {
      if (!modifying_p)
	switch (cbu->flags)
	  {
	  case CBEditable:
	    break;
	    
	  case CBReadOnly:
	    /* handle it at the end !! */
	    break;
	    
	  case CBUserInput:
	    {
	      Lisp_Object buffer_local_variable_name = 
		Qenergize_user_input_buffer_mark;
	      binfo->output_mark = Fmake_marker ();	
	      Fset_marker (binfo->output_mark, 1, binfo->emacs_buffer);
	      /* make sure that this guy doesn't get GC'd out from under us */
	      Fmake_local_variable (buffer_local_variable_name);
	      Fput (buffer_local_variable_name, intern ("permanent-local"),
		    Qt);
	      Fset (buffer_local_variable_name, binfo->output_mark);
	      call0 (Qenergize_user_input_mode);
	      /* Make sure buffer is current after the hook */
	      Fset_buffer (binfo->emacs_buffer);
	    }
	    break;
	  }
      
      binfo->flags = cbu->flags;
    }
  
  if (!relative_p)
    extent_offset = 0;
  else if (!NILP (binfo->output_mark))
    extent_offset = Fmarker_position (binfo->output_mark) + text_len - 1;
  else
    extent_offset = BUF_Z (XBUFFER (binfo->emacs_buffer)) + text_len - 1;
  
  if (text_len)
    {
      if (!NILP (binfo->output_mark))
        {
	  Fset_buffer (binfo->emacs_buffer);
          if (XMARKER (binfo->output_mark)->buffer)
            Fgoto_char (binfo->output_mark);
          else
	    /* This should not happen */
	    Fgoto_char (BUF_ZV (XBUFFER (binfo->emacs_buffer)));
          
          if (point <= previous_point)
	    {
#if 1
              display_start += text_len;
#endif
	      previous_point += text_len;
	    }
	  insert_raw_string (text, text_len);
	  Fset_marker (binfo->output_mark, make_number (point),
		       binfo->emacs_buffer);
        }
      else if (modifying_p)
        {
          Fgoto_char (from);
          if (!no_text_deleted)
            insert_raw_string (text, text_len);
        }
      else
        insert_raw_string (text, text_len);
      
      previous_point = Fgoto_char (previous_point);
      
#if 1
      if (!NILP (display_window))
        {
	  
          Fset_marker (XWINDOW (display_window)->pointm, previous_point,
                       binfo->emacs_buffer);
	  if (NILP (binfo->output_mark) && should_keep_window_start)
	    {
	      Fset_marker (XWINDOW (display_window)->start, display_start,
			   binfo->emacs_buffer);
	      XWINDOW (display_window)->force_start = Qt;
	    }
        }
#endif
    }
    
  
  /* Classes, generics and extents */
  ParseClasses (conn, cbu->nClass, binfo, modifying_p);
  ParseGenerics (conn, cbu->nGeneric, binfo, modifying_p);
  ParseExtents (conn, cbu->nExtent, binfo, modifying_p, extent_offset);
  
  /* Restore the modified bit */
  Fset_buffer (binfo->emacs_buffer);
  Fset_buffer_modified_p (modified_buffer_flag);
  
  /* restore modified hooks and globals, and return the previous buffer */
  UNGCPRO;
  unbind_to (count, Qnil);
  inside_parse_buffer = 0;
}

/* kill an Energize buffer */
static void
forget_buffer (BufferInfo *binfo)
{
  int i;
  Lisp_Object buffer = binfo->emacs_buffer;

  remove_buffer_info (binfo->id, buffer, binfo->editor);
  energize_buffers_list = Fdelq (buffer, energize_buffers_list);

  /* if there was an associated screen */
  if (binfo->screen != Qnil)
    Fdelete_screen (binfo->screen);

  /* Also delete the dialog boxes associated to the buffer */
  BLOCK_INPUT;
  for (i = 0; i < binfo->n_p_sheets; i++)
    {
      lw_destroy_all_widgets (binfo->p_sheet_ids [i]);
      if (binfo->p_sheet_ids [i] == debuggerpanel_sheet)
	{
	  debuggerpanel_sheet = 0;
	  desired_debuggerpanel_exposed_p = 0;
	}
    }
  UNBLOCK_INPUT;

  free_buffer_info (binfo);

  XBUFFER(buffer)->undo_list = Qnil;
  /* flush the buffer SOE before flushing the extents */
  free_buffer_cached_stack (XBUFFER(buffer));
  XBUFFER(buffer)->extents = Qnil;
}

/********************** Request-related utilities ************************/

/* outputs a single extent in the connection buffer */
static void
WriteExtent 
  (Connection *conn, Energize_Extent_Data *ext, unsigned int start, unsigned int end)
{
  switch (ext->extentType)
    {
    case CEAttribute:
      CWriteExtent (conn, CEAttribute, ext->id, start, end,
                    (EId)ext->u.attr.attrValue);
      break;
      
    case CEAbbreviation:
      CWriteExtent (conn, CEAbbreviation, ext->id, start, end,
                    (EId)ext->u.abbrev.isOpened);
      break;
      
    case CEGeneric:
      CWriteExtent (conn, CEGeneric, ext->id, start, end, 0);
      break;

    case CEWriteProtect:
      CWriteExtent (conn, CEWriteProtect, ext->id, start, end, 0);
      break;
    }
}

extern void move_gap (int);

/* Utility to return a char* to the contents of the current buffer */
static char*
get_buffer_as_string (unsigned int *len)
{
  if (BEGV < GPT && ZV > GPT) move_gap (BEGV);
  if (len) *len = ZV - BEGV;
  return (char *)CHAR_ADDRESS (BEGV);
}

/* Function called by map_extents in SaveBufferToEnergize. Outputs the
   extents for the extents corresponding to Energize objects, and
   increments the n_extents count. */

static int
write_an_extent (Lisp_Object extent_obj, void *arg)
{
  binfo_and_n_extents *bane = (binfo_and_n_extents*)arg;
  Energize_Extent_Data *ext = extent_to_data (extent_obj);
  if (ext)
    {
      unsigned int first = XINT (Fextent_start_position (extent_obj));
      unsigned int last = XINT (Fextent_end_position (extent_obj));
      WriteExtent (bane->binfo->editor->conn, ext, EnergizePosForEmacsPos (first),
                   EnergizePosForEmacsPos (last));
      bane->n_extents += 1;
    }
  return 0;
}

/* Sends a BufferSaved request to energize for binfo */
static void
SaveBufferToEnergize (BufferInfo *binfo)
{
  Connection *conn = binfo->editor->conn;
  EId bufferId = binfo->id;
  CBuffer *cbu;
  CEditorRequest *req;
  char *text;
  int length;
  struct buffer *cur_buff = current_buffer;
  int count = specpdl_ptr - specpdl;
  Lisp_Object file_name;
  
  binfo_and_n_extents bane;
  
  /* selects the buffer as current */
  Fset_buffer (binfo->emacs_buffer);
  
  /* write header */
  cbu = CWriteBufferSavedHeader (conn);
  cbu->bufferId = bufferId;
  cbu->flags = 0;
  cbu->nClass = 0;
  cbu->nGeneric = 0;

  /* file name */
  file_name = current_buffer->filename;
  if (STRINGP (file_name))
    CWriteVstring0 (conn, (char *)XSTRING (file_name)->data);
  else
    CWriteVstring0 (conn, "");
  CWriteVstring0 (conn, "");    /* directory name */
  CWriteVstring0 (conn, "");    /* buffer name */
  
  /* write the text */
  CNeedOutputSize (conn, (Z - BEG) + 9);
  text = get_buffer_as_string ((unsigned int *) &length);
  CWriteVstringLen (conn, text, length);
  
  /* write the extents */
  bane.binfo = binfo;
  bane.n_extents = 0;
  map_extents (BUF_BEG (current_buffer), BUF_Z (current_buffer), 
	       write_an_extent, 0, &bane, XBUFFER(binfo->emacs_buffer), 1);
  
  /* update nextent in request's header */
  req = (CEditorRequest *)conn->header;
  req->buffersaved.buffer.nExtent = bane.n_extents;
  CWriteLength (conn);
  CWriteRequestBuffer (conn);
  
  /* sets the flags so that we will warn Energize about more modifications */
  binfo->modified_state = 0;

  /* Mark the buffer as non editable so that we will ask Energize about it
     before modifying it again */
  binfo->editable = 0;

  /* restores the buffer as current */
  internal_set_buffer (cur_buff);
  unbind_to (count, Qnil);
}

unsigned long
generic_id_for_extent (Energize_Extent_Data *ext)
{
  return ext ? ext->id : 0;
}


/********************** Menu ("keywords") operations **********************/


/* gets the menu for the buffer/extent pair at the head of the request buffer.
** returns the propose choice request if succeeds, 0 otherwise (kernel 
** connection closed, or not connected) */

static Lisp_Object
get_energize_menu (Lisp_Object buffer, Lisp_Object extent_obj, int selection_p,
		   Lisp_Object only_name)
{
  Connection*	conn;
  EId	buffer_id;
  EId	extent_id;
  int		result;
  struct reply_wait rw;
  struct gcpro gcpro1, gcpro2;

  if (!get_energize_connection_and_buffer_id (buffer,
					      (void**)&conn,
					      (long*)&buffer_id))
    return 0;

  if (EXTENTP (extent_obj))
    extent_id = generic_id_for_extent (extent_to_data (extent_obj));
  else
    extent_id = 0;
  
  CWriteQueryChoicesRequest (conn, buffer_id, extent_id);
  conn->header->data =
    selection_p ? CEChasCharSelection | CEChasObjectSelection : 0;
  conn->header->serial = ++request_serial_number;
  CWriteRequestBuffer (conn);

  /* wait for the acknowledge */
  rw.serial = request_serial_number;
  rw.objectId = buffer_id;
  rw.genericId = extent_id;
  rw.menu_result = Qnil;
  rw.only_name = only_name;
  
  GCPRO2 (rw.menu_result, rw.only_name);
  wait_for_reply (&rw);
  result = rw.menu_result;
  UNGCPRO;
  return result;
}

static int
something_answered_p (void* arg)
{
  struct reply_wait* rw = (struct reply_wait*)arg;
  return rw->answered_p || !energize_connection || !energize_connection->conn;
}


static void
push_wait (struct reply_wait* rw)
{
  rw->next = global_reply_wait;
  global_reply_wait = rw;
}

static Lisp_Object
remove_wait (Lisp_Object obj)
{
  struct reply_wait* gw;
  struct reply_wait* previous;
  struct reply_wait* rw = (struct reply_wait*)lisp_to_word (obj);

  for (previous = 0, gw = global_reply_wait;
       gw != rw;
       previous = gw, gw = gw->next);
  if (previous)
    previous->next = gw->next;
  else
    global_reply_wait = gw->next;
  return Qnil;
}

static struct reply_wait*
find_wait_reply (int serial)
{
  struct reply_wait* gw;
  for (gw = global_reply_wait; gw && gw->serial != serial; gw = gw->next);
  return gw;
}


static int
wait_for_reply (struct reply_wait* rw)
{
  int count = specpdl_ptr - specpdl;
  rw->answered_p = 0;
  push_wait (rw);
  record_unwind_protect (remove_wait, word_to_lisp ((int)rw));
  wait_delaying_user_input (something_answered_p, rw);
  unbind_to (count, Qnil);
  return rw->answered_p;
}

static void
execute_energize_menu (Lisp_Object buffer, Energize_Extent_Data* ext, char* name,
		       EId item_id, EId flags, Lisp_Object selection,
		       Lisp_Object no_confirm)
{
  Connection*	conn;
  EId	buffer_id;
  EId	extent_id;
  BufferInfo*	binfo;
  struct reply_wait rw;
  
  if (!get_energize_connection_and_buffer_id (buffer, (void**)&conn,
					      (long*)&buffer_id))
    return;

  extent_id = generic_id_for_extent (ext);
  
  if ((flags & CKBuffer) && Fbuffer_modified_p (buffer))
    {
      /* saves buffer if requested and needed */
      binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);
      if (binfo)
	SaveBufferToEnergize (binfo);
    }

  CWriteExecuteChoicesRequest (conn, buffer_id, extent_id, item_id, 0, 0);
  /* send the menu name */
  if (energize_connection->minor >= 7)
    CWriteVstring0 (conn, name);
  conn->header->serial = ++request_serial_number;
  conn->header->data = 0;
  switch (XTYPE (selection))
    {
    case Lisp_String:
      conn->header->data |= CEChasCharSelection;
      CWriteVstringLen (conn, (char*)XSTRING (selection)->data,
			XSTRING (selection)->size);
      break;
    case Lisp_Vector:
      {
	int i;
	EId data;
	conn->header->data |= CEChasObjectSelection;

	/* writes the length */
	data = XVECTOR (selection)->size;
	CWrite (conn, EId, &data);	

	/* writes the elements */
	for (i = 0; i < XVECTOR (selection)->size; i++)
	  {
	    if (CONSP (XVECTOR (selection)->contents [i]))
	      data = lisp_to_word (XVECTOR (selection)->contents [i]);
	    else
	      data = XVECTOR (selection)->contents [i];
	    CWrite (conn, EId, &data);
	  }
      }
      break;

    case Lisp_Cons:
      {
	Lisp_Object type = Fcar (selection);
	Lisp_Object value = Fcdr (selection);
	if (EQ (type, intern ("ENERGIZE_OBJECT"))
	    && STRINGP (value))
	  {
	    conn->header->data |= CEChasObjectSelection;
	    CWriteN (conn, char, XSTRING (value)->data,
		     XSTRING (value)->size);
	  }
      }
      break;

    default:
      if (!NILP (selection))
	error ("unrecognised energize selection");
    }
  if (!NILP (no_confirm))
    conn->header->data |= CECnoConfirm;
  CWriteLength (conn);
  CWriteRequestBuffer (conn);

  /* wait for the acknowledge */
  rw.serial = request_serial_number;
  rw.objectId = buffer_id;
  rw.genericId = extent_id;
  rw.itemId = item_id;
  rw.message = 0;
  
  if (wait_for_reply (&rw) && !rw.status)
    {
      char message [128];
      if (energize_connection && energize_connection->conn)
	sprintf (message, "Energize command failed: %.80s",
		 (rw.message ? rw.message : "(null)"));
      else
	sprintf (message, "Connection to Energize was closed.");
      if (rw.message)
	xfree (rw.message);
      error (message);
    }
  else
    {
      if (rw.message)
	xfree (rw.message);
      if (!energize_connection)
	error ("Connection to Energize was closed.");
    }
 }

/* Returns a list of vectors representing the menu choices.  Next request
   in connection must be a ProposeChoices.  The list is 
   (buffer extent <item1> ... <itemN>).  <itemI> is (name id1 id2 flags).
   Idi is (high .  low).  We build the list in reverse order and nreverse
   it.  If (only_name != 0), we only return the item of named only_name as
   a vector.  */

static Lisp_Object
list_choices (Lisp_Object buffer, Lisp_Object extent_obj, 
              Lisp_Object only_name, CProposeChoicesRequest* creq)
{
  Connection *conn;
  int i;
  Lisp_Object item_list;
  Lisp_Object item;
  struct Lisp_Vector *v;
  struct gcpro gcpro1, gcpro2, gcpro3;
  CChoice *choice;
  ReqLen name_length;
  char *name;
  char *arg_name;

  if (energize_connection && energize_connection->conn)
    conn = energize_connection->conn;
  else
    return Qnil;
  
  if (!creq || creq->head.reqType != ProposeChoicesRType)
    {
      CSkipRequest (conn);
      return Qnil;
    }
  
  item = Qnil;
  item_list = Qnil;
  
  GCPRO3 (only_name, item_list, item);
  
  for (i = 0; i < (int)(creq->nChoices); i++)
    {
      choice = CGet (conn, CChoice);
      name = CGetVstring (conn, &name_length);
      if (!name_length)
	continue;

      /* the argument, if passed, is another string after the NUL (!)
       * this is a quick hack to provide cheap arguments to menus entries */
      arg_name = strchr (name, 0240);
      if (arg_name)
	{
	  *arg_name= 0;
	  arg_name += 1;
	}

      if (!NILP (only_name))
	{
	  if (!strcmp ((char*)XSTRING(only_name)->data, name))
	    {
	      if (NILP (item))
		{
		  item = Fmake_vector (5, Qnil);
		  v = XVECTOR (item);
		  v->contents [0] = only_name;
		}
              v->contents [1] = word_to_lisp (choice->choiceId);
              v->contents [2] = Qnil;
              v->contents [3] = choice->flags;
	      v->contents [4] = arg_name ? build_string (arg_name) : Qnil;
	    }
	}
      else
	{
	  item = Fmake_vector (5, Qnil);
	  v = XVECTOR (item);
	  v->contents [0] = build_string (name);
	  v->contents [1] = word_to_lisp (choice->choiceId);
	  v->contents [2] = Qnil;
	  v->contents [3] = choice->flags;
	  v->contents [4] = arg_name ? build_string (arg_name) : Qnil;
	  item_list = Fcons (item, item_list); /* pushes in the list */
        }
    }
  
  if (NILP (only_name))
    item_list = Fcons (buffer, Fcons (extent_obj, Fnreverse (item_list)));
  UNGCPRO;
  
  return NILP (only_name) ? item_list : item;
}

DEFUN ("energize-list-menu", Fenergize_list_menu,
       Senergize_list_menu, 3, 4, 0,
       "(energize-list-menu buffer extent selection-p [only-name])\n\
Request the set of menu options from the Energize server that are\n\
appropriate to the buffer and the extent.  Extent can be (), in which case\n\
the options are requested for the whole buffer.  Selection-p tells\n\
if the selection is available on the dislpay emacs is using. \n\
Returns the options as\n\
a list that can be passed to energize-activate-menu.  Items\n\
in the list can also be passed to energize-execute-menu-item.\n\
The list is (buffer extent or () <item1> ... <itemN>).\n\
where <itemI> is (name id1 id2 flags); idI is (high . low).\n\
If optional argument only-name is provided only the item with name only-name\n\
is returned, or () if no such item exists.")
     (buffer, extent_obj, selection_p, only_name)
     Lisp_Object buffer, extent_obj, selection_p, only_name;
{
  Lisp_Object res;
  CHECK_BUFFER (buffer, 1);
  
  if (!energize_connection || !energize_connection->conn) return Qnil;

  if (!NILP (only_name))
    CHECK_STRING (only_name, 1);
  
  res = get_energize_menu (buffer, extent_obj, selection_p != Qnil,
			   only_name);
  notify_delayed_requests ();
  return res;
}

DEFUN ("energize-execute-menu-item", Fenergize_execute_menu_item,
       Senergize_execute_menu_item, 3, 5, 0,
       "(energize-execute-menu-item buffer extent item argument no-confirm)\n\
Item is a vector received by energize-list-menu.  Sends a request to\n\
execute the code associated to this menu inside the Energize server.\n\
Optional fourth argument is a string or a vector to be used as the selection\n\
for entry disabled because they need the selection.\n\
Optional fifth argument, if non NIL, tells Energize to not request \n\
confirmation before executing the command.")
(buffer, extent_obj, item, selection, no_confirm)
Lisp_Object buffer, extent_obj, item, selection, no_confirm;
{
  struct Lisp_Vector *v;
  
  if (!energize_connection || !energize_connection->conn) return Qnil;

  CHECK_BUFFER (buffer, 1);
  CHECK_VECTOR (item, 1);
  v = XVECTOR (item);

  if (v->size != 4)
    error ("Bad menu item to energize-execute-menu-item");
  
  /* ignore the flags for now */
  execute_energize_menu (buffer, extent_to_data (extent_obj),
			 (char*)XSTRING (v->contents [0])->data,
			 lisp_to_word (v->contents [1]),
			 v->contents [3],
			 selection,
			 no_confirm);

  return Qt;
}

DEFUN ("energize-execute-command-internal", Fenergize_execute_command_internal,
       Senergize_execute_command_internal, 3, 5, 0,
       "(energize-execute-command-internal buffer extent command argument no-confirm)\n\
Command is a string naming an energize command.  Sends a request to\n\
execute this command inside the Energize server.\n\
Optional fourth argument is a string or a vector to be used as the selection.\n\
Optional fifth argument, if non NIL, tells Energize to not request \n\
confirmation before executing the command.\n\
\n\
See also 'energize-list-menu'.")
(buffer, extent_obj, command, selection, no_confirm)
Lisp_Object buffer, extent_obj, command, selection, no_confirm;
{
  if (!energize_connection || !energize_connection->conn) return Qnil;

  CHECK_BUFFER (buffer, 1);
  CHECK_STRING (command, 1);
  
  execute_energize_menu (buffer, extent_to_data (extent_obj),
			 (char*)XSTRING (command)->data, 0, 0, selection,
			 no_confirm);

  return Qt;
}

/********************************* kill buffer interface ****************/

DEFUN ("energize-buffer-type-internal",
       Fenergize_buffer_type, Senergize_buffer_type,
       1, 1, 0,
       "(energize-buffer-type buffer) returns a symbol denoting the buffer \n\
type if buffer is an Energize buffer, else it returns NIL.")
	(buffer)
	Lisp_Object buffer;
{
  if (!energize_connection) return Qnil;
  
  CHECK_BUFFER (buffer, 1);
  return get_buffer_type_for_emacs_buffer (buffer, energize_connection);
}

DEFUN ("set-energize-buffer-type-internal", 
       Fset_energize_buffer_type_internal, 
       Sset_energize_buffer_type_internal, 2, 2, 0,
 "(set-energize-buffer-type-internal buffer type) returns the type \n\
symbol which is the new buffer-type, if the buffer is an Energize buffer and\n\
the type is non-NIL symbol, else it returns NIL.")
   (buffer, type)
    Lisp_Object buffer, type;
{
  BufferInfo *binfo;
  
  if (!energize_connection || (type == Qnil)) return Qnil;
  
  CHECK_BUFFER (buffer, 1);
  CHECK_SYMBOL (type, 1);
  
  if (!(binfo =
	get_buffer_info_for_emacs_buffer (buffer, energize_connection))) 
    return Qnil;
  else 
    return
      set_buffer_type_for_emacs_buffer (buffer, energize_connection, type);
}

DEFUN ("energize-buffer-p", Fenergize_buffer_p, Senergize_buffer_p, 1, 1, 0,
       "(energize-buffer-p buffer) returns T if buffer is \n\
an Energize buffer, otherwise NIL.")
  (buffer)
  Lisp_Object buffer;
{
  BufferInfo *binfo;
  
  if (!energize_connection) return Qnil;
  
  CHECK_BUFFER (buffer, 1);
  if (!(binfo =
	get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
    return Qnil;
  else
    return Qt;
}  

DEFUN ("energize-buffer-id", Fenergize_buffer_id, Senergize_buffer_id, 1, 1, 0,
       "(energize-buffer-id buffer)\n\
Returns (high . low) if buffer is an Energize buffer, () otherwise")
     (buffer)
     Lisp_Object buffer;
{
  BufferInfo *binfo;
  
  if (!energize_connection) return Qnil;
  
  CHECK_BUFFER (buffer, 1);
  if (!(binfo =
	get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
    return Qnil;
  else
    return word_to_lisp (binfo->id);
}  

DEFUN ("energize-request-kill-buffer", Fenergize_request_kill_buffer,
       Senergize_request_kill_buffer, 1, 1, 0,
       "(energize-request-kill-buffer buffer)\n\
Sends a request to energize for killing buffer")
   (buffer)
   Lisp_Object buffer;
{
  BufferInfo *binfo;
  
  if (!energize_connection) return Qnil;
  
  CHECK_BUFFER (buffer, 1);
  if (!(binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection))) 
    return Qnil;
  
  /* Tell Energize about it if connected */
  if (energize_connection->conn)
    {
      CWriteKillBufferHeader (energize_connection->conn, binfo->id);
      CWriteRequestBuffer (energize_connection->conn);
    }

  /* Clears the internal state */
  forget_buffer (binfo);

  return Qnil;
}

/******************** Handle requests from the kernel *********************/

#ifdef EMACS_BTL
#include "cadillac-btl-extern.h"
#endif

/* turn logging on or off, etc. */
static void
HandleLoggingRequest (Editor *editor, CLoggingRequest *creq)
     /* I'm a lumberjack and I'm ok... */
{
  ReqLen name_len;
  char* data_filename = CGetVstring (editor->conn, &name_len);

#ifdef EMACS_BTL
  {
    extern Lisp_Object Vexecution_path;
    char *execname = 
      (STRINGP (Vexecution_path))?
        ((char *) XSTRING(Vexecution_path)->data):0;

    switch (creq->type)
      {
      case CLRInitBTL:
        cadillac_terminate_logging(); /* #### rename me */
        cadillac_initialize_backtrace_logging /* #### rename me */
          (data_filename, execname, (long) creq->limit, (long) creq->interval);
        break;

      case CLRInitPCL:
        cadillac_terminate_logging(); /* #### rename me */
        cadillac_initialize_pc_logging /* #### rename me */
          (data_filename, execname, (long) creq->limit, (long) creq->interval);
        break;

      case CLRStart:
        cadillac_start_logging(); /* #### rename me */
        break;

      case CLRStop:
        cadillac_stop_logging(); /* #### rename me */
        break;

      case CLRTerminate:
        cadillac_terminate_logging(); /* #### rename me */
        break;
        
      case CLRSetLogSignal:
        cadillac_set_log_signal(creq->signal); /* #### rename me */
        break;
        
      default:
        error ("Bad logging request type %d", creq->type);
      }
  }
#else
  Post ("Logging request, but no such code in image.");
#endif
}



/* creates a new buffer */
static void
HandleNewBufferRequest (Editor *editor, CNewBufferRequest *creq)
{
  ParseBuffer (editor->conn, &creq->buffer, editor, 0, 0, creq->transientId,
	       0);
  if (!NILP (Venergize_create_buffer_hook))
    {
      CBuffer *cbu = &creq->buffer;
      BufferInfo *binfo = get_buffer_info_for_id (cbu->bufferId, editor);
      Lisp_Object buffer;
      if (binfo)
        {
	  struct screen* prev_screen;
          buffer = binfo->emacs_buffer;
	  if (!NILP (binfo->screen))
	    {
	      prev_screen = selected_screen;
	      selected_screen = XSCREEN(binfo->screen);
	    }
	  safe_funcall_hook (Venergize_create_buffer_hook, 1, buffer, 0, 0);
	  if (!NILP (binfo->screen))
	    selected_screen = prev_screen;
        }
    }
}

/* Modifies the contents of a buffer */
static void
HandleModifyBufferRequest (Editor *editor, CModifyBufferRequest *creq)
{
  windows_or_buffers_changed++;
  ParseBuffer (editor->conn, &creq->newData, editor, creq->startPosition,
               creq->endPosition, 0, creq->head.data);
}

static void
MakeBufferAndExtentVisible (Lisp_Object list, Lisp_Object go_there)
{
  call2 (Qenergize_make_many_buffers_visible, list, go_there);
}

/* pops a buffer and scroll to a extent: calls to lisp */
static void
HandleEnsureVisibleRequest (Editor *editor, CEnsureVisibleRequest *creq)
{
  BufferInfo *binfo;
  Energize_Extent_Data *ext;
  Lisp_Object buffer_extent_list;
  struct gcpro gcpro1;

  buffer_extent_list = Qnil;
  GCPRO1 (buffer_extent_list);
  
  binfo = get_buffer_info_for_id (creq->bufferId, editor);
  if (!binfo)
    {
      Post ("EnsureVisibleRequest: unknown buffer");
      goto finished;
    }
  
  if (binfo->screen != Qnil)
    {
      /* ignore ensure visible for postit note buffers */
      goto finished;
    }

  if (creq->extentId)
    {
      ext = get_extent_data (creq->extentId, binfo);
      if (!ext)
	Post ("EnsureVisibleRequest: ignoring unknown extent");
    }
  else
    ext = 0;
  
  buffer_extent_list = Fcons (ext ? ext->extent : Qnil, Qnil);
  buffer_extent_list = Fcons (binfo->emacs_buffer, buffer_extent_list);

  MakeBufferAndExtentVisible (buffer_extent_list, creq->head.data ? Qt : Qnil);
  
 finished:
  CSkipRequest (editor->conn);
  UNGCPRO;
}

static void
HandleEnsureManyVisibleRequest (Editor *editor,
				CEnsureManyVisibleRequest *creq)
{
  BufferInfo *binfo;
  Energize_Extent_Data *ext;
  Lisp_Object buffer_extent_list;
  int n;
  EId buffer_id;
  EId extent_id;
  struct gcpro gcpro1;

  buffer_extent_list = Qnil;
  GCPRO1 (buffer_extent_list);
  
  for (n = creq->head.data,
       buffer_id = creq->bufferId,
       extent_id = creq->extentId;
       n;
       n--,
       buffer_id = n ? *(CGet (editor->conn, EId)) : 0,
       extent_id = n ? *(CGet (editor->conn, EId)) : 0)
    {
      binfo = get_buffer_info_for_id (buffer_id, editor);
      if (!binfo)
	{
	  Post ("EnsureManyVisibleRequest: ignoring unknown buffer");
	  continue;
	}
      
      if (binfo->screen != Qnil)
	{
	  /* silently ignore ensure visible for postit note buffers */
	  continue;
	}
      
      if (extent_id)
	{
	  ext = get_extent_data (extent_id, binfo);
	  if (!ext)
	    Post ("EnsureManyVisibleRequest: ignoring unknown extent");
	}
      else
	ext = 0;

      /* cons in reverse order and reverse the list before
	 calling MakeBufferAndExtentVisible */
      buffer_extent_list = Fcons (binfo->emacs_buffer, buffer_extent_list);
      buffer_extent_list = Fcons (ext ? ext->extent : Qnil,
				  buffer_extent_list);
    }
  buffer_extent_list = Fnreverse (buffer_extent_list);
  MakeBufferAndExtentVisible (buffer_extent_list, Qt);
  
  UNGCPRO;
}

/* Update the cached menus, ie update the menubar for now. */
static void
HandleProposeChoicesRequest (Editor *editor, CProposeChoicesRequest *req)
{
  BufferInfo* binfo;
  Lisp_Object buffer = Qnil;
  Lisp_Object extent = Qnil;
  Lisp_Object choices = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;
  struct reply_wait* rw;

  GCPRO3 (buffer, extent, choices);
  
  /* get the buffer */
  binfo = get_buffer_info_for_id (req->objectId, editor);
  if (binfo)
    buffer = binfo->emacs_buffer;
  else
    buffer = Qnil;

  /* get the extent */
  if (binfo && req->genericId)
    {
      Energize_Extent_Data* ext = get_extent_data (req->genericId, binfo);
      if (ext)
	extent = ext->extent;
      else
	extent = Qnil;
    }
  else
    extent = Qnil;
  
  /* find if we were waiting for a reply */
  rw = find_wait_reply (req->head.serial);

  /* handle the request */
  if (rw && rw->objectId == req->objectId && rw->genericId == req->genericId)
    {
      /* It's a reply for a get_energize_menu call */
      rw->answered_p = True;
      rw->status = 1;
      rw->menu_result = list_choices (buffer, extent, rw->only_name, req);
    }
  else
    {
      /* It's a menu update, call the hook */
      choices = list_choices (buffer, extent, Qnil, req);
      safe_funcall_hook (Venergize_menu_update_hook, 1, choices, Qnil, Qnil);
    }
  UNGCPRO;
}

/* Kills a buffer */
static void
unmodify_buffer_and_kill_it (Lisp_Object buffer)
{
  int count = specpdl_ptr - specpdl;

  if ((!BUFFERP (buffer)) || NILP (XBUFFER (buffer)->name)) 
    return;
  
  /* unmodify the buffer */
  if (buffer != Fcurrent_buffer ())
    {
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      Fset_buffer (buffer);
    }
  Fset_buffer_modified_p (Qnil);
  unbind_to (count, Qnil);
  
  /* kill it.  This will call the Energize hook to do the right thing */
  Fkill_buffer (buffer);
}

static void
HandleKillBufferRequest (Editor *editor, CKillBufferRequest *creq)
{
  BufferInfo *binfo;
  
  if (!(binfo = get_buffer_info_for_id (creq->bufferId, editor)))
    {
      Post ("KillBufferVisibleRequest: unregistered buffer");
      return;
    }

  unmodify_buffer_and_kill_it (binfo->emacs_buffer);
}

static void
HandleRemoveExtentsRequest (Editor *editor, CRemoveExtentsRequest *creq)
{
  BufferInfo *binfo;
  int i;
  EId *ids;
  Lisp_Object restore_buffer_state_cons;
  int count = specpdl_ptr - specpdl;
  
  if (!(binfo = get_buffer_info_for_id (creq->bufferId, editor)))
    {
      Post ("RemoveExtentsRequest: unregistered buffer");
      CSkipRequest (editor->conn);
      return;
    }

  /* enable buffer edits */
  restore_buffer_state_cons =
    Fcons (word_to_lisp (creq->bufferId),
	   Fcons (XBUFFER(binfo->emacs_buffer)->read_only, Qnil));

  record_unwind_protect (restore_buffer_state, restore_buffer_state_cons);

  XBUFFER(binfo->emacs_buffer)->read_only = Qnil;

  /* save old hook values */
  specbind (Qenergize_buffer_modified_hook, Qnil); 
  
  BUF_FACECHANGE (XBUFFER (binfo->emacs_buffer))++;
  windows_or_buffers_changed++;
  
  ids = CGetN (editor->conn, EId, creq->nExtent);
  for (i = 0; i < creq->nExtent; i++)
    {
      Energize_Extent_Data *ext = get_extent_data (ids [i], binfo);
      if (ext)
        free_Energize_Extent_Data (ext, binfo, OFT_STANDALONE);
    }
  
  /* restore modified hooks and globals */
  unbind_to (count, Qnil);
}

/* handles a request to save a buffer from the kernel */
static void
HandleSaveBufferRequest (Editor *editor, CSaveBufferRequest *creq)
{
  BufferInfo *binfo;
  int count = specpdl_ptr - specpdl;
 
  if (!(binfo = get_buffer_info_for_id (creq->bufferId, editor)))
    {
      Post("Server attempt to save a non registered buffer");
      return;
    }
  
  if (binfo->emacs_buffer != Fcurrent_buffer ())
    {
      record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
      Fset_buffer (binfo->emacs_buffer);
    }

  if (creq->head.data == CSExecuteSave)
    {
      Lisp_Object args[2];
      args [0] = intern ("save-buffer");
      args [1] = Qnil;
      Fapply (2, args);
    }
  else
    SaveBufferToEnergize (binfo);
  
  unbind_to (count, Qnil);
}

static void
HandleSetModifiedFlagRequest (Editor* editor, CSetModifiedFlagRequest* creq)
{
  BufferInfo *binfo;
  int count = specpdl_ptr - specpdl;

  if (!(binfo = get_buffer_info_for_id (creq->bufferId, editor)))
    {
      Post("Server attempt to set modified flag of a non registered buffer");
      return;
    }
  
  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
  Fset_buffer (binfo->emacs_buffer);
  specbind (Qenergize_buffer_modified_hook, Qnil);
  Fset_buffer_modtime (binfo->emacs_buffer, Qnil);
  Fset_buffer_modified_p (creq->state ? Qt : Qnil);
  binfo->modified_state = creq->state;
  /* Mark the buffer so that we ask permission to Energize when the
   * user tries to modify it again */
  binfo->editable = 0;
  if (!creq->state)
    mark_all_extents_as_unmodified (binfo);
  unbind_to (count, Qnil);
}


/* handles requests regarding p_sheet associated to buffers */
static void
add_in_list_of_ids (int** ids, int* n_ids, int id){
  if (*n_ids == 0){
    *n_ids = 1;
    *ids = (int*)xmalloc (sizeof (int));
  }else{
    *n_ids += 1;
    *ids = (int*)xrealloc (*ids, sizeof (int) * (*n_ids));
  }
  (*ids) [(*n_ids) - 1] = id;
}

static void
remove_from_list_of_ids (int** ids, int* n_ids, int id){
  int i;
  if (*n_ids){
    /* look for id in *ids */
    for (i = 0; i < (*n_ids) && (*ids) [i] != id; i++);
    /* shift the remaining ones */
    for (; i < (*n_ids) - 1; i++)
      (*ids) [i] = (*ids) [i + 1];
    /* decrease the count */
    *n_ids -= 1;
    /* free array if empty */
    if (!*n_ids){
      xfree (*ids);
      *ids = 0;
    }
  }
}

extern void make_psheets_desired (struct screen *, Lisp_Object);

static void
HandleBufferSheetRequest (Editor *editor, CSheetRequest *sreq,
			  EId buffer_id)
{
  BufferInfo *binfo;
  char* name;
  Connection* conn = editor->conn;
  
  if (!(binfo = get_buffer_info_for_id (buffer_id, editor)))
    {
      Post("Server attempt to use p_sheet in a non registered buffer");
      CSkipRequest (conn);
      return;
    }
  
  name = CGetVstring (conn, (ReqLen *) 0);
  switch ((CSheetRSubtype) sreq->head.data)
    {
    case CSCreate:
      BLOCK_INPUT;
      lw_register_widget (name, name, sreq->sheetId, NULL, NULL,
			  HandleControlChange, NULL);
      UNBLOCK_INPUT;
      add_in_list_of_ids (&binfo->p_sheet_ids, &binfo->n_p_sheets,
			  sreq->sheetId);
      if (!strcmp (name, DEBUGGER_PSHEET_NAME)) 
	debuggerpanel_sheet = sreq->sheetId;
      break;
      
    case CSDelete:
      remove_from_list_of_ids (&binfo->p_sheet_ids, &binfo->n_p_sheets,
			       sreq->sheetId);
      BLOCK_INPUT;
      lw_destroy_all_widgets (sreq->sheetId);
      UNBLOCK_INPUT;
      if (sreq->sheetId == debuggerpanel_sheet)
	{
	  desired_debuggerpanel_exposed_p = 0;
	  debuggerpanel_sheet = 0;
	}
      break;
      
    case CSHide:
      {
	struct screen *screen;
	Lisp_Object rest;
	
	BLOCK_INPUT;
	if (sreq->sheetId == debuggerpanel_sheet)
	  desired_debuggerpanel_exposed_p = 0;
	else
	  for (rest = Vscreen_list; rest != Qnil; rest = Fcdr(rest))
	    {
	      screen = XSCREEN(Fcar(rest));
	      if (SCREEN_IS_X (screen))
		make_psheets_desired (screen, Qnil);
	    }
	UNBLOCK_INPUT;
      }
      break;
      
    case CSShow:
      if (sreq->sheetId == debuggerpanel_sheet)
	desired_debuggerpanel_exposed_p = 1;
      else
	{
	  struct screen *screen;
	  struct window *window;
	  Lisp_Object rest;
	  for (rest = Vscreen_list; rest != Qnil; rest = Fcdr(rest))
	    {
	      screen = XSCREEN(Fcar(rest));
	      if (SCREEN_IS_X (screen))
		{
		  window = XWINDOW(screen->selected_window);
		  if (window->buffer == binfo->emacs_buffer)
		    make_psheets_desired (screen, binfo->emacs_buffer);
		}
	    }
	}
      break;
    }
}


#if 0
static void
note_being_destroyed (Widget w, XtPointer call_data, XtPointer client_data)
{
#if 0
 *  NoteWidget nw = (NoteWidget)w;
 *  Widget tw = get_text_widget (nw);
 *  SCREEN_PTR s;
 *  Lisp_Object screen;
 *
 *  if (tw){
 *    s = (SCREEN_PTR)x_any_window_to_screen (XtWindow (tw));
 *    if (s){
 *      BLOCK_INPUT;
 *      set_text_widget (nw, 0);
 *      UNBLOCK_INPUT;
 *      XSET (screen, Lisp_Screen, s);
 *      Fdelete_screen (screen);
 *    }
 *  }
#endif
}
#endif

#if 0
 * static void
 * note_instantiated (EId bufferId, EId noteId, NoteWidget nw, void* arg)
 * {
 *   XtAddCallback ((Widget)nw, XtNdestroyCallback, note_being_destroyed, 0);
 *   XtVaSetValues ((Widget)nw, "forceXtLoop", 1, 0);
 * }
#endif

static void
HandlePostitRequest (Editor *editor, CGenericRequest *preq)
{
#if 0
 *   BufferInfo* binfo;
 *   Connection* conn = editor->conn;
 *   NoteWidget notew;
 *   
 *   switch (preq->head.reqType)
 *     {
 *     case OpenPostitRType:{
 *       if (!(binfo = get_buffer_info_for_id (preq->openpostit.bufferId,
 *  					    editor)))
 *  	{
 *  	  Post("Server attempt to use postit in a non registered buffer");
 *  	  CSkipRequest (conn);
 *  	  return;
 *  	}
 *       
 *       BLOCK_INPUT;
 *       xc_create_note_window 
 *         (&preq->openpostit, conn, note_instantiated, 0,
 *          FONT_WIDTH (SCREEN_NORMAL_FACE (selected_screen).font),
 *          TOTAL_HEIGHT (SCREEN_NORMAL_FACE (selected_screen).font));
 *       UNBLOCK_INPUT;
 *       add_in_list_of_ids (&binfo->note_ids, &binfo->n_notes,
 *  			  preq->openpostit.bufferId);
 *     }
 *       break;
 *       
 *     case KillPostitRType:{
 *       if (!(binfo = get_buffer_info_for_id (preq->killpostit.bufferId,
 *  					    editor)))
 *  	{
 *  	  Post("Server attempt to kill postit in a non registered buffer");
 *  	  CSkipRequest (conn);
 *  	  return;
 *  	}
 *       
 *       BLOCK_INPUT;
 *       xc_destroy_note (preq->killpostit.postitId);
 *       UNBLOCK_INPUT;
 *       remove_from_list_of_ids (&binfo->note_ids, &binfo->n_notes,
 *  			       preq->killpostit.postitId);
 *       break;
 *     }}
#endif
}

/* show busy */
/* int needs_to_recompute_menubar_when_kernel_not_busy; */

static void
show_all_menubars_busy (int busy)
{
  struct screen* s;
  struct x_display* x;
  Lisp_Object tail;

  BLOCK_INPUT;
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      s = XSCREEN (XCONS (tail)->car);
      if (SCREEN_IS_X (s))
	{
	  x = s->display.x;
	  if (x->menubar_widget)
	    lw_show_busy (x->menubar_widget, busy);
	}
    }
  UNBLOCK_INPUT;
}

static void
HandleShowBusyRequest (Editor *editor, CGenericRequest *preq)
{
  /* call the show busy routine of the  library for the menubar of
   * all screens */
  ReqLen len;
  extern Lisp_Object Venergize_kernel_busy;

  char* why = CGetVstring (editor->conn, &len);

  show_all_menubars_busy (preq->head.data);
  Venergize_kernel_busy = preq->head.data ? Qt : Qnil;
  safe_funcall_hook (Venergize_kernel_busy_hook, 1, build_string (why), Qnil,
		     Qnil);

  /* update the menubars if needed and kernel is not busy */
  if (!preq->head.data /*&& needs_to_recompute_menubar_when_kernel_not_busy*/)
    {
      SCREEN_PTR s;
      Lisp_Object tail;
      
/*      needs_to_recompute_menubar_when_kernel_not_busy = 0;*/
      for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  if (!SCREENP (XCONS (tail)->car))
	    continue;
	  
	  s = XSCREEN (XCONS (tail)->car);
	  if (SCREEN_IS_X (s) &&
	      ! MINI_WINDOW_P (XWINDOW (s->selected_window)))
	    recompute_screen_menubar (s);
	}
    }
}

static void
HandleSheetRequest (Connection* conn, CSheetRequest* sreq, Widget parent)
{
  char* name = CGetVstring (conn, NULL);

  switch ((CSheetRSubtype)sreq->head.data)
    {
    case CSCreate:
      lw_create_widget (name, name, sreq->sheetId, 0, parent,
			!sreq->bufferId, 0, HandleControlChange, 0);
      break;
    case CSDelete:
      lw_destroy_all_widgets (sreq->sheetId);
      break;
      
    case CSShow:
      lw_pop_up_all_widgets (sreq->sheetId);
      break;

    case CSHide:
      lw_pop_down_all_widgets (sreq->sheetId);
      break;
    }
}

static void
HandleSetControlRequest (Connection* conn, CGenericRequest* creq)
{
  CSetControlRequest* sreq = &creq->setcontrol;
  widget_value val;
  widget_value* contents;

  unsigned long i;
  unsigned long n = sreq->nChoices;
  
  if (n > 0)
    {
      contents = (widget_value *) xmalloc (n * sizeof (widget_value));
      memset (contents, 0, (n * sizeof (widget_value)));
    }
  else 
    contents = NULL;
  memset (&val, 0, sizeof (val));
  val.name = CGetVstring (conn, NULL);
  val.enabled = !(sreq->flags & CKInactive);
  val.selected = !!(sreq->flags & CKSelected);
  val.change = VISIBLE_CHANGE;
  val.contents = contents;

  for (i = 0; i < n; i++)
    {
      widget_value* cur = &contents [i];
      CChoice* choice = CGet (conn, CChoice);
      cur->name = CGetVstring (conn, NULL);
      cur->value = cur->name;
      cur->key = NULL;
      cur->enabled = !(choice->flags & CKInactive);
      cur->selected = !!(choice->flags & CKSelected);
      cur->change = VISIBLE_CHANGE;
      cur->contents = NULL;
      cur->call_data = NULL;
      cur->next = i == n - 1 ? NULL : &contents [i + 1];
      cur->toolkit_data = NULL;
      if ((i == 0 && n == 1) || cur->selected)
	{
	  val.value = cur->name;
	  if (!*val.value)
	    val.value = NULL;
	}
    }
  lw_modify_all_widgets (sreq->sheetId, &val, True);
  
  if (contents)
    xfree (contents);
}
    
static void
SendSheetStateChange (Connection* conn, EId buffer_id, EId sheet_id,
		      int shown)
{
  CWriteSheetRequest (conn, shown ? CSShow : CSHide, sheet_id, buffer_id, "");
}

static void
HandleControlChange (Widget widget, EId sheet_id, void* arg)
{
  Connection* 	conn;
  widget_value* val;
  widget_value* cur;
  widget_value* this_val = NULL;
  widget_value* cancel = NULL;
  char* 	this_name;
  int delete_window_p = (((int) arg) == -1);
  

  if (!energize_connection)
    return;

  conn = energize_connection->conn;
  if (!conn)
    return;

  this_name = XtName (widget);
  val = lw_get_all_values (sheet_id);

  if (delete_window_p)
    /* Complete and utter kludge.  If this dbox was dismissed with the
       WM close box (WM_DELETE_WINDOW, meaning the widget was destroyed)
       then we look for a likely "cancel" button and pretend the user
       clicked on that.  Really the protocol should be extended for this.
     */
    for (cur = val; cur; cur = cur->next)
      {
	char *v = cur->value;
	if (v &&
	    ((strlen (v) >= 6 && !strncmp (v, "cancel", 6)) ||
	     (strlen (v) >= 5 && !strncmp (v, "abort", 5))))
	  cancel = cur;
      }

  /* first send all the edited widgets */
  for (cur = val; cur; cur = cur->next)
    {
      /* do not send the widget that ran the callback */
      if (!strcmp (cur->name, this_name))
	this_val = cur;
      else if (cur == cancel)
	;
      /* send the edited widgets */
      else if (cur->edited)
	{
	  char* value = cur->value;
	  unsigned int flags = 0;

	  if (!cur->enabled)
	    flags |= CKInactive;
	  if (cur->selected)
	    flags |= CKSelected;

	  /* the kernel is brain dead and expect "1" and "0" as values
	     for the checkbox objects.  So if value is NULL, make it be "0"
	     or "1" depending on the selected state.  This is until we fix
	     the kernel. */
	  if (!value)
	    value = cur->selected ? "1" : "0";

	  CWriteSetControlRequest (conn, sheet_id, 0, cur->name, 1);
	  CWriteChoice (conn, 0, flags, value, 0);
	  CWriteLength (conn);
	}
    }

  if (delete_window_p && !this_val)
    {
      this_val = cancel;
/*      if (! this_val) abort (); */
    }

  /* Then send the widget that ran the callback */
  if (this_val)
    {
      CWriteSetControlRequest (conn, sheet_id, 0, this_val->name, 1);
      CWriteChoice (conn, 0, 0, this_val->value, 0);
      CWriteLength (conn);
      CWriteRequestBuffer (conn);
    }
}

/******************** Low level connection stuff ************************/
static void
add_in_connection_input_buffer (Connection *conn, char *s, int l)
{
  /* Should be in connection.c */
  if (conn->inread >= conn->infill) 
    conn->inread = conn->infill = conn->inbuffer;
  
  CNeedInputSize (conn, l);
  memcpy (conn->infill, s, l);
  conn->infill += l;
}

static Lisp_Object
ProcessJustOneEnergizeRequest ()
{
  Editor *editor = energize_connection;
  CEditorRequest *req;
  int res = 0;
  
  if (!editor) return make_number(res);
  
  if (!editor->conn)
    {
      CloseConnection();
      return make_number (res);
    }
  
  req = CReadEditorRequest(editor->conn);
  if (!req)
    {
      switch (errno)
        {
        case EWOULDBLOCK:
          /* Post("ProcessEnergizeRequest: internal error EWOULDBLOCK"); */
	  res = -1;
          break;
          
        case 0:
	case ECONNRESET:
          Post ("Connection to Energize was closed.");
          CloseConnection ();
          break;
          
        default:
          Post
            ("System error on connection to Energize, closing.");
          CloseConnection ();
          break;
        }
    }
  else
    {
      res = 1;
      switch (req->head.reqType)
        {
        case RefuseConnectionRType:
          Post("Energize connection refused");
          CloseConnection ();
          break;
          
        case AcceptConnectionRType:
	  {
	    CProtocol* proto = CGet (editor->conn, CProtocol);
	    editor->major = proto->major;
	    editor->minor = proto->minor;
	    Post("Energize connection accepted");
	    CSkipRequest(editor->conn);
	  }
          break;
          
        case NewBufferRType:
          HandleNewBufferRequest(editor, &req->newbuffer);
          break;
          
	case QueryBufferRType:
	  {
	    EId buffer_id;
	    struct reply_wait* rw = find_wait_reply (req->head.serial);
	    CGetVstring (editor->conn, 0); /* skip directory */
	    CGetVstring (editor->conn, 0); /* skip file */
	    buffer_id = *CGet (editor->conn, EId);
	    if (rw)
	      {
		rw->answered_p = 1;
		rw->status = req->head.data;
		rw->objectId = buffer_id;
	      }
	  }
	  break;

        case EnsureVisibleRType:
          HandleEnsureVisibleRequest(editor, &req->ensurevisible);
          break;
          
        case EnsureManyVisibleRType:
          HandleEnsureManyVisibleRequest(editor, &req->ensuremanyvisible);
          break;
          
        case ModifyBufferRType:
          HandleModifyBufferRequest(editor, &req->modifybuffer);
          break;
          
        case ProposeChoicesRType:
	  HandleProposeChoicesRequest (editor, &req->generic.proposechoices);
          break;
          
        case ChoiceExecutedRType:
	  {
	    struct reply_wait* rw = find_wait_reply (req->head.serial);
	    CChoiceExecutedRequest* ce = &req->generic.choiceexecuted;
	    if (rw)
	      {
		rw->answered_p = 1;
		rw->status = ce->head.data;
		BLOCK_INPUT;
		rw->message = CMakeVstring (editor->conn, 0);
		UNBLOCK_INPUT;
	      }
	  }
          break;
          
        case KillBufferRType:
          HandleKillBufferRequest(editor, &req->killbuffer);
          break;
          
	case ModifiedBufferRType:
	  {
	    struct reply_wait* rw = find_wait_reply (req->head.serial);
	    if (rw)
	      {
		rw->answered_p = 1;
		if (rw->objectId == req->modifiedbuffer.bufferId)
		  rw->status = req->modifiedbuffer.state;
		else
		  rw->status = CMBufferLocked;
	      }
	  }
	  break;

        case SetModifiedFlagRType:
          HandleSetModifiedFlagRequest(editor, &req->setmodifiedflag);
          break;
          
        case RemoveExtentsRType:
          HandleRemoveExtentsRequest (editor, &req->removeextents);
          break;
          
        case RenumberExtentsRType:
          /* HandleDuplicateExtentRequest(editor, req); */
          break;
          
#if 0
        case DialogRType:
          /* HandleDialogRequest(editor, req, CurrentBuffer(editor)); */
          break;
#endif
          
        case SaveBufferRType:
          HandleSaveBufferRequest (editor, &req->savebuffer);
          break;
          
        case SheetRType:{
	  EId buffer_id = req->generic.sheet.bufferId;
	  if (!buffer_id)
	    buffer_id = buffer_id_of_sheet (req->generic.sheet.sheetId);
	  if (buffer_id)
	    HandleBufferSheetRequest (editor, &req->generic.sheet, buffer_id);
	  else
	    {
	      CSheetRSubtype type = (CSheetRSubtype)req->head.data;
	      if (type == CSDelete || type ==CSHide)
		select_screen (selected_screen);
	      BLOCK_INPUT;
	      HandleSheetRequest (editor->conn, &req->generic.sheet,
				  selected_screen->display.x->widget);
	      UNBLOCK_INPUT;
	    }
	}
          break;
	  
        case SetControlRType:
	  BLOCK_INPUT;
          HandleSetControlRequest (editor->conn, (CGenericRequest*) req);
	  UNBLOCK_INPUT;
          break;

 	case OpenPostitRType:
  	case KillPostitRType:
 	  HandlePostitRequest (editor, (CGenericRequest*)req);
 	  break;

	case ShowBusyRType:
	  HandleShowBusyRequest (editor, (CGenericRequest*)req);
	  break;

	case LoggingRType:
	  HandleLoggingRequest (editor, (CLoggingRequest*)req);
	  break;

        default:
          Post1("ProcessEnergizeRequest: can't handle request of type %d", 
                req->head.reqType);
        }

    }

  return make_number (res);
}

static int inside_ProcessEnergizeRequest1;

/* this must be called ONLY by unwind_protect in ProcessEnergizeRequest1 */
static Lisp_Object
post_handle_request (Lisp_Object ignored)
{
  if (inside_ProcessEnergizeRequest1 <= 0)
    abort ();
  inside_ProcessEnergizeRequest1--;
  if (energize_connection && energize_connection->conn)
    CSkipRequest (energize_connection->conn);
  return Qnil;
}

static Lisp_Object
pop_conn (Lisp_Object arg)
{
  Connection *old_conn = (Connection *) lisp_to_word (arg);
  if (! old_conn)
    abort ();
  if (! energize_connection)
    return Qnil;
  if (energize_connection->conn == old_conn)
    abort ();

  if (CRequestDelayedP (energize_connection->conn))
    /* We don't call the CWait* functions any more so this shouldn't happen.
       But if it does someday, then we need to either copy the remaining
       bits from new_conn to old_conn, or loop processing requests until
       new_conn is drained.
     */
    abort ();

  DeleteConnection (energize_connection->conn);
  energize_connection->conn = old_conn;

  return Qnil;
}

static Lisp_Object
ProcessEnergizeRequest1 ()
{ 
  Lisp_Object result;
  int count = specpdl_depth;

  if (inside_ProcessEnergizeRequest1)
    {
      /* When the energize process filter is called recursively, push a new
	 connection object.  The read-pointer of the connection buffer could
	 be in the middle of a request.  However, we know that the fd itself
	 is always pointing between requests.  So making a new connection is
	 a way of skipping past the one request we were in the process of
	 reading when we allowed process output to be handled recursively.
       */
      Connection *old_conn = energize_connection->conn;
      Connection *new_conn =
	EditorSideConnection ((void *) energize_connection,
			      old_conn->fdin, old_conn->fdout);
      energize_connection->conn = new_conn;
      record_unwind_protect (pop_conn, word_to_lisp ((unsigned int) old_conn));
    }

  /* this must come after pop_conn() to get the right connection object */
  record_unwind_protect (post_handle_request, Qnil);

  inside_ProcessEnergizeRequest1++;

  result = ProcessJustOneEnergizeRequest ();
  notify_delayed_requests ();

  /* decrements inside_ProcessEnergizeRequest1 and possibly replaces
     energize_connection->conn with old_conn.
   */
  unbind_to (count, Qnil);

  return result;
}


/******** Initialize Energize-related state and set up connection ********/

static void
setup_connection (Editor *ed, unsigned int id1, unsigned int id2) 
{
  CEditorRequest *req = CWriteEditorRequest(ed->conn, QueryConnectionRType);
  
  /* these 2 slots are ignored */
  req->generic.queryconnection.major = 0;
  req->generic.queryconnection.minor = 0;

  req->generic.queryconnection.cadillacId1 = id1; /* #### */
  req->generic.queryconnection.cadillacId2 = id2;
  req->generic.queryconnection.nProtocols = 1;
  /* first numerical arg is major protocol number, second is minor */
  CWriteProtocol(ed->conn, 0, 8, "editor");
  CWriteLength (ed->conn);
  CWriteRequestBuffer (ed->conn);
}

/* this is used as the readMethod of the energize connection, so that 
   the connection library won't do some buffering that messes us up.
   It does this buffering only if conn->readMethod == read, so using
   another function turns it off.
 */
static int
my_read (int fd, char *buf, int nb)
{
  return read (fd, buf, nb);
}

static Connection *
EditorSideConnection (Editor *editor, int fdin, int fdout)
{
  Connection *conn = NewConnection ((void *)editor, fdin, fdout);
  if (conn)
    conn->readMethod = my_read;
  return conn;
}

DEFUN ("handle-energize-request", Fhandle_energize_request,
       Shandle_energize_request,
       2, 2, 0,
       "Filter called when a request is available from Energize.")
     (proc, string)
     Lisp_Object proc, string;
{
  if (!NILP (string))
    CHECK_STRING (string, 0);
  
  if (!energize_connection || !energize_connection->conn)
    {
      /* no need for a message here, Energize is dead */
      return make_number (0);
    }
  if (!energize_connection || (energize_connection->proc != proc))
    {
      Post("Got an Energize request but not from current connection ");
      return make_number (0);
    }
  
  if (!NILP (string))
    add_in_connection_input_buffer (energize_connection->conn,
				    (char *) XSTRING (string)->data,
				    XSTRING (string)->size);
  
  return ProcessEnergizeRequest1 ();
}


/* Opens a network connection to Energize.
 * server is a string.  It can end up with :<uid> or :<username>
 * in which case the uid is added to the TCP port to get the connection */
static void
ConnectToEnergize (char *server_str, char *arg)
{
  struct Lisp_Process *proc;
  Lisp_Object lp;
  Lisp_Object fil;
  char *host;
  unsigned int port;
  long flags;
  int id1;
  int id2;

  if (CGetPortNumber (server_str, &host, &port))
    {
      
      lp = Fopen_network_stream (build_string ("energize"),
                                 Qnil,
                                 build_string (host),
                                 make_number (port));
      if (lp)
        {
	  /* Don't ask the user for confirmation when exiting Emacs */
	  Fprocess_kill_without_query (lp, Qnil);
          proc = XPROCESS (lp);
          energize_connection = xnew (Editor);
          energize_connection->conn = EditorSideConnection (energize_connection, XFASTINT (proc->infd),
                                                XFASTINT (proc->infd));
          energize_connection->proc = lp;
          energize_connection->binfo_hash = make_hashtable (10);
	  energize_connection->image_table = 0;
          energize_connection->gc_save = Qnil;
          energize_connection->major = 0;
          energize_connection->minor = 0;
          peo = allocate_edit_options (10);
          request_serial_number = 0;
	  global_reply_wait = 0;

          if ((flags = fcntl (energize_connection->conn->fdin, F_GETFL, 0)) == -1)
	    abort(); /* perror ("get connection flags"); */
	  
#ifdef O_NONBLOCK
	  if (fcntl (energize_connection->conn->fdin, F_SETFL, flags & ~O_NONBLOCK) == -1)
#else
	  if (fcntl (energize_connection->conn->fdin, F_SETFL, flags & ~O_NDELAY) == -1)
#endif
            abort(); /* perror ("set connection flags"); */
          
          XSET (fil, Lisp_Subr, &Shandle_energize_request);
          proc->filter = fil;
          proc->filter_does_read = Qt;

	  Venergize_kernel_busy = Qnil;
/*	  needs_to_recompute_menubar_when_kernel_not_busy = 0;*/
          
          id1 = 0;
          id2 = 0;
          if (arg)
            sscanf (arg, "%x,%x", &id1, &id2);
          
	  energize_buffers_list = Qnil;

          setup_connection (energize_connection, id1, id2);
        }
      else
	error ("couldn't connect to Energize server");
    }
  else
    error ("couldn't determine Energize server port number");
}


/* Close the connection to energize.
 * Kills all the energize related buffer */
static void 
CloseConnection () 
{
  Editor *ed = energize_connection;

  if (ed)
    /* make this function as paranoid as we can */
    {
      BLOCK_INPUT;
      /* cleanup the busy state */
      show_all_menubars_busy (False);
      Venergize_kernel_busy = Qnil;
      /* destroy all pop_up boxes */
      lw_destroy_all_pop_ups ();
      UNBLOCK_INPUT;

      debuggerpanel_sheet = 0;
      desired_debuggerpanel_exposed_p = 0;

      if (ed->conn)
	DeleteConnection (ed->conn);
      ed->conn = 0;
      
      if (ed->binfo_hash)
        {
          int count = specpdl_ptr - specpdl;
	  
          /* we are flushing everything, so we just ignore any change
             hooks and don't make an effort to delete extents since they
             are all going away */
          specbind (Qenergize_buffer_modified_hook, Qnil);
          specbind (intern ("inhibit-quit"), Qt);
	  call0 (intern ("de-energize-all-buffers"));
          unbind_to (count, Qnil);
	  
          free_hashtable (ed->binfo_hash);
          ed->binfo_hash = 0;
        }
      
      free_edit_options (peo);
      
      if (ed->proc) Fdelete_process (ed->proc);
      ed->proc = Qnil;
      
      energize_buffers_list = Qnil;

      /* now kill buffers created to satisfy requests on old connection */
      xfree (ed);
    }
  
  /* marked as close */
  energize_connection = 0;
}


DEFUN ("connect-to-energize-internal", 
       Fconnect_to_energize_internal, Sconnect_to_energize_internal, 0, 2, 0,
       "Usage: (connect-to-energize-internal <server-name> <energizearg>)\n\
Energizearg representing two 32 bit Energize ids that will be passed\n\
to the Energize server when opening the Energize connection.\n\
Only one connection can be open at a time.")

  (server_name, energize_arg)
  Lisp_Object server_name, energize_arg;
{
  unsigned char *server; 
  unsigned char *arg;
  
  if (!NILP(energize_arg))
    {
      CHECK_STRING (energize_arg, 1);
      arg = XSTRING (energize_arg)->data;
    }
  else
    arg = 0;
  
  if (!NILP(server_name))
    {
      CHECK_STRING (server_name, 1);
      server = XSTRING (server_name)->data;
    }
  else
    server = 0;
  
  /* since we are going ahead with this, make sure that we are
     really and truly disconnected first */
  Fclose_connection_to_energize ();

  ConnectToEnergize ((char *)server, (char *)arg);
  return Qnil;
}

DEFUN ("close-connection-to-energize", Fclose_connection_to_energize, 
       Sclose_connection_to_energize, 0, 0, 0,
       "Close the open Energize connection, if any.")
     ()
{
  if (!energize_connection) return Qnil;

  CloseConnection();
  return Qnil;
}


/* buffer modified routines */
static void
SendBufferModificationState (Editor *editor, BufferInfo *binfo, int flag)
{
  Connection *conn = editor->conn;
  EId bufferId = binfo->id;
  
  if (conn)
    {
      int state = (flag)?(CMBufferSetModified):(CMBufferSetUnmodified);
      CWriteModifiedBufferHeader (conn, bufferId, state);
      CWriteRequestBuffer (conn);
    }
}

/* returns 1 if buffer is locked (non-editable),
**  0 if it isn't (editable), and -1 if it can't tell */
static int
CheckBufferLock (Editor *editor, BufferInfo *binfo)
{
  Connection *conn = editor->conn;
  struct reply_wait rw;
  
  /* If permision already granted by kernel dont' ask again */
  if (binfo->editable)
    return 0;

  /* If can't ask say we do not know */
  if (!conn)
    return -1;

  /* Ask the kernel */
  CWriteModifiedBufferHeader (conn, binfo->id, CMBufferQueryLock);
  conn->header->serial = ++request_serial_number;
  CWriteRequestBuffer (conn);
  
  rw.serial = request_serial_number;
  rw.objectId = binfo->id;
  rw.status = -1;
  
  if (!wait_for_reply (&rw))
    return -1;
  
  if (rw.status == CMBufferLocked)
    {
      /* Buffer is locked by kernel so we cannot edit it */
      return 1;
    }
  else if (rw.status == CMBufferUnlocked)
    {
      /* Buffer is unlocked by kernel: edit permission granted! */
      binfo->editable = 1;
      return 0;
    }
  else
    {
      /* This should never happen */
      return -1;
    }	 
}


/* Ask the kernel if BUFFER is currently locked -- waits for answer */
static Lisp_Object 
buffer_locked_p (Lisp_Object buffer)
{
  BufferInfo *binfo;

  if (!energize_connection)
    return Qnil;
  
  if (NILP (buffer))
    XSET (buffer, Lisp_Buffer, current_buffer);

  CHECK_BUFFER (buffer, 0);
  
  binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);

  if (!binfo)
    {
      /* Not an Energize buffer, return Qnil: can edit buffer */
      return Qnil;
    }
  else 
    {
      /* Energize buffer, check if editable */
      return CheckBufferLock (energize_connection, binfo) == 0 ? Qnil : Qt;
    }
}



/* Called by map_extents function called by Fsend_buffer_modified_request
 */
static int
notify_extent_modified (Lisp_Object extent_obj, void *arg)
{
  /* arg is a binfo_and_state */
  binfo_and_state *bans = (binfo_and_state*)arg;
  Energize_Extent_Data *ext;
  BufferInfo *binfo = bans->binfo;
  Connection *conn = binfo->editor->conn;
  EId *extent_id;
  
  if (!EXTENTP (extent_obj) ||  
      !(EXTENT_FLAGS (XEXTENT(extent_obj)) & EF_WARN_MODIFY))
    return 0;

  if ((ext = extent_to_data (extent_obj))
      && ext->extentType == CEGeneric
      && ext->u.generic.gData
      && ext->u.generic.gData->cl
      && (ext->u.generic.gData->cl->flags & CCWarnModified)
      && ext->u.generic.gData->modified_state != bans->state)
    {
      if (bans->tell_energize)
	{
	  CWriteModifiedExtentsHeader (conn, binfo->id, bans->state, 1);
	  extent_id = CPut (conn, EId);
	  *extent_id = ext->id;
	  CWriteLength (conn);
	  CWriteRequestBuffer (conn);
	}
      ext->u.generic.gData->modified_state = bans->state;
    }
  return 0;
}

static int
ceiwme_lower_mf (EXTENT extent, void *arg)
{
  if (EXTENT_FLAGS (extent) & EF_WARN_MODIFY)
    *((EXTENT *) arg) = extent;
  return 0;
}

static int
ceiwme_upper_mf (EXTENT extent, void *arg)
{
  if (EXTENT_FLAGS (extent) & EF_WARN_MODIFY)
    {
      *((EXTENT *) arg) = extent;
      return 1;
    }
  else
    return 0;
}

static void
coerce_endpoints_to_be_inside_warn_on_modify_extents (from_ptr, to_ptr, b)
     int *from_ptr;
     int *to_ptr;
     struct buffer *b;
{
  EXTENT lower_extent = 0;
  EXTENT upper_extent = 0;
  int lower_bound = *from_ptr;
  int upper_bound = *to_ptr;
  Lisp_Object tmp;
  
  /* make sure that from <= to */
  { 
    int tmp_int = max (lower_bound, upper_bound);
    *from_ptr = lower_bound = min (lower_bound, upper_bound);
    *to_ptr = upper_bound = tmp_int;
  }
  
  if (!BUFFER_NOTIFY_BACKGROUND_BIT_SET_P(b)) 
    return;
  
  map_extents (BUF_BEG(b), lower_bound, 0, ceiwme_lower_mf, 
               (void *) &lower_extent, b, 1);
  if (!lower_extent)
    {
      lower_bound = BUF_BEG (b);
      map_extents (upper_bound, BUF_Z(b), 0, ceiwme_upper_mf, 
                   (void *) &upper_extent, b, 1);
      if (!upper_extent)
        upper_bound = BUF_Z (b);
      else
        {
          int xstart;
          XSET (tmp, Lisp_Extent, upper_extent);
          xstart = XINT (Fextent_start_position (tmp));
          upper_bound = max (upper_bound, xstart);
        }
    }
  /* I forget why, but if we found an lower bound, we don't need to find
     an upper bound */
  else
    {
      int xstart;
      XSET (tmp, Lisp_Extent, lower_extent);
      xstart = XINT (Fextent_start_position (tmp));
      lower_bound = min (lower_bound, xstart);
    }

  *from_ptr = lower_bound;
  *to_ptr = upper_bound;
  return;
}

static void
mark_all_extents_as_unmodified (BufferInfo *binfo){
  binfo_and_state bans;
  bans.binfo = binfo;
  bans.state = FALSE;
  bans.tell_energize = FALSE;
  
  map_extents (BUF_BEG (XBUFFER (binfo->emacs_buffer)),
	       BUF_Z (XBUFFER (binfo->emacs_buffer)),
	       notify_extent_modified, 0, &bans, 
               XBUFFER(binfo->emacs_buffer), 1);
}

/* Send the BufferModified events for the current buffer.
 * Handles both global buffer modified and extents modified. */
DEFUN ("send-buffer-modified-request", Fsend_buffer_modified_request,
       Ssend_buffer_modified_request,
       3, 3, 0,
       "Send a BufferModified request for the current buffer.")
     (state, from, to)
     Lisp_Object state, from, to; /* dont use ANSI arglists in DEFUNs */
{
  int modifiedp = NILP (state)? 0 : 1;
  Lisp_Object buffer;
  BufferInfo *binfo;
  int from_int = XINT (from);
  int to_int = XINT (to);
  
  if (!energize_connection || !energize_connection->conn) return Qnil;
  
  XSET (buffer, Lisp_Buffer, current_buffer);

  Fenergize_barf_if_buffer_locked ();

  if (binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection))
    {
      /* now make sure that from and to
	 are inside some warn_on_modify extents somewhere */
      coerce_endpoints_to_be_inside_warn_on_modify_extents
        (&from_int, &to_int, current_buffer);
      XSET (from, Lisp_Int, from_int);
      XSET (to, Lisp_Int, to_int);
  
      if (binfo->modified_state != modifiedp)
        {
          SendBufferModificationState (energize_connection, binfo, modifiedp);
          binfo->modified_state = modifiedp;
        }
      if (modifiedp)
	{
	  binfo_and_state bans;
	  bans.binfo = binfo;
	  bans.state = TRUE;
	  bans.tell_energize = TRUE;
	  map_extents (from, to, notify_extent_modified, 0, &bans,
		       XBUFFER(binfo->emacs_buffer), 1);
	}
      else
	mark_all_extents_as_unmodified (binfo);
    }
  return Qnil;
}

DEFUN ("energize-barf-if-buffer-locked", Fenergize_barf_if_buffer_locked,
       Senergize_barf_if_buffer_locked, 0, 0, 0,
       "Error if the buffer is locked.")
     ()
{
  Lisp_Object buffer;
  XSET (buffer, Lisp_Buffer, current_buffer);
  
  if (!energize_connection || !energize_connection->conn)
    return Qnil;

  while (!NILP (buffer_locked_p (buffer)))
    {
      notify_delayed_requests ();
      Fsignal (Qbuffer_locked_by_kernel, (Fcons (buffer, Qnil)));
    }  
  return Qnil;
}


DEFUN ("energize-send-region", Fenergize_send_region,
       Senergize_send_region,
       2, 2, 0,
       "Send region as user input")
     (start, end)
     Lisp_Object start, end;
{
  BufferInfo *binfo;
  Lisp_Object b;
  int start1;
  CEditorRequest *req;
  
  if (!energize_connection || !energize_connection->conn) 
    error ("Not connected to Energize");
  
  XSET (b, Lisp_Buffer, current_buffer);
  if (binfo = get_buffer_info_for_emacs_buffer (b, energize_connection))
    {
      validate_region (&start, &end);
      
      if (XINT (start) < GPT && XINT (end) > GPT) move_gap (start);
      
      start1 = XINT (start);
      req = CWriteEditorRequest (energize_connection->conn, UserTypedSomethingRType);
      req->usertypedsomething.bufferId = binfo->id;
      CWriteVstringLen 
        (energize_connection->conn, (char *) CHAR_ADDRESS (start1), XINT (end) - start1);
      CWriteLength (energize_connection->conn);
      CWriteRequestBuffer (energize_connection->conn);
      return Qnil;
    }
  return Qnil;
}

DEFUN ("connected-to-energize-p", Fconnected_to_energize_p,
       Sconnected_to_energize_p,
       0, 0, 0, 
"Returns nil if no connection to Energize.")
     ()
{
  if (!energize_connection || !energize_connection->conn || !energize_connection->binfo_hash || 
      !energize_connection->proc || (XTYPE (energize_connection->proc != Lisp_Process)))
    return Qnil;
  else 
    return Qt;
}

DEFUN ("energize-user-input-buffer-mark", Fenergize_user_input_buffer_mark,
       Senergize_user_input_buffer_mark, 0, 0, 0,
       "Returns the mark associated to the current (user input) buffer")
     ()
{
  BufferInfo *binfo;
  Lisp_Object b;
  
  if (!energize_connection) return Qnil;
  
  XSET (b, Lisp_Buffer, current_buffer);
  if ((binfo = get_buffer_info_for_emacs_buffer (b, energize_connection)))
    return binfo->output_mark;
  else
    return Qnil;
}

DEFUN ("energize-query-buffer", Fenergize_query_buffer,
       Senergize_query_buffer, 1, 2, 0,
       "(energize-query-buffer filename just-ask)\n\
Ask Energize to create a buffer containing the file filename.\n\
Returns the buffer or NIL if Energize cannot create the buffer.\n\
If second argument just-ask is T, just ask if Energize\n\
already knows about the file and returns T if yes, NIL otherwise.")
     (filename, just_ask)
     Lisp_Object filename, just_ask;
{
  struct Lisp_String *filename_str;
  CEditorRequest *creq;
  char *dir_sep;
  struct reply_wait rw;
  
  if (!energize_connection || !energize_connection->conn)
    return Qnil;

  filename = Fexpand_file_name (filename, Qnil);
  filename_str = XSTRING (filename);

  dir_sep = (char *) strrchr ((char *)filename_str->data, '/');
  
  creq = CWriteEditorRequest (energize_connection->conn, QueryBufferRType);
  creq->head.data = !NILP (just_ask);
  creq->head.serial = ++request_serial_number;
  CWriteVstringLen (energize_connection->conn, (char *) filename_str->data,
		    (dir_sep)? (dir_sep - (char *) filename_str->data): 0);
  CWriteVstringLen (energize_connection->conn, 
		    (char *) filename_str->data, filename_str->size);
  CWriteLength (energize_connection->conn);
  CWriteRequestBuffer (energize_connection->conn);
  
  rw.serial = request_serial_number;
  rw.objectId = 0;
  
  if (!wait_for_reply (&rw))
    return Qnil;

  if (rw.status)
    {
      if (rw.objectId)
	{
	  BufferInfo* binfo = get_buffer_info_for_id (rw.objectId, energize_connection);
	  return binfo ? binfo->emacs_buffer : Qt;
	}
      else
	return Qt;
    }
  else
    return Qnil;
}


DEFUN ("energize-psheets-visible-p", Fenergize_psheets_visible_p,
       Senergize_psheets_visible_p, 0, 1, 0,
       "Whether the (optional) screen currently has open psheets.")
     (screen)
     Lisp_Object screen;
{
  if (NILP (screen))
    XSET (screen, Lisp_Screen, selected_screen);
  CHECK_SCREEN (screen, 0);
  if (XSCREEN (screen)->display.x->current_psheets)
    return Qt;
  return Qnil;
}

DEFUN ("energize-buffer-has-psheets-p", Fenergize_buffer_has_psheets_p,
       Senergize_buffer_has_psheets_p, 0, 1, 0,
       "Whether the buffer has psheets associated with it.")
     (buf)
     Lisp_Object buf;
{
  int count;
  if (NILP (buf))
    buf = Fcurrent_buffer ();
  CHECK_BUFFER (buf, 0);
  if (get_psheets_for_buffer (buf, &count))
    return Qt;
  return Qnil;
}

DEFUN ("energize-protocol-level", Fenergize_protocol_level,
       Senergize_protocol_level, 0, 0, 0,
       "Returns the Energize protocol level.")
     ()
{
  return
    energize_connection
      ? Fcons (energize_connection->major, energize_connection->minor)
	: Qnil;
}


static int
get_energize_connection_and_buffer_id (Lisp_Object buffer, void **conn_ptr,
				       long *buffer_id_ptr)
{
  BufferInfo *binfo;

  if (!energize_connection || !energize_connection->conn) return 0;
  
  binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);
  
  *conn_ptr = (void *) energize_connection->conn;
  *buffer_id_ptr = (long) binfo ? binfo->id : 0;
  return 1;
}

static int
get_energize_connection_and_current_buffer_id (void **conn_ptr,
					       long *buffer_id_ptr)
{
  Lisp_Object lisp_buffer;
  XSET (lisp_buffer, Lisp_Buffer, current_buffer);

  return get_energize_connection_and_buffer_id (lisp_buffer, conn_ptr,
						buffer_id_ptr);
}

int *
get_psheets_for_buffer (Lisp_Object buffer, int *count_ptr)
{
  BufferInfo *binfo;

  if (!energize_connection || !energize_connection->conn) return 0;
  
  binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);
  if (!binfo) return 0;

  if (count_ptr) *count_ptr = binfo->n_p_sheets;
  return binfo->p_sheet_ids;
}

void
notify_that_sheet_has_been_hidden (EId id)
{
  EId buffer_id = buffer_id_of_sheet (id);
  if (!buffer_id)
    return;

  if (buffer_id && energize_connection && energize_connection->conn)
    {
      SendSheetStateChange (energize_connection->conn, buffer_id, id, 0);
      CWriteRequestBuffer (energize_connection->conn);
    }
}


/* edit-mode dialog box 
 */

extern int dbox_up_p;
extern unsigned int popup_id_tick;
extern Lisp_Object Fpopup_dialog_box (Lisp_Object);

static struct editmode {
  int ok, external, view, edit, window, split;
  char *other;
} editmode;

static void
edit_mode_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  widget_value *data;
  char *name = (char *) client_data;

  if ((int) client_data == -1) name = "cancel";	/* WM_DELETE_WINDOW */

  if (!strcmp (XtName (widget), "otherText")) /* screw it */
    ;
  else if (!strcmp (name, "externalBox"))
    {
      /* make the text slot be active only if "other" is selected */
      BLOCK_INPUT;
      data = malloc_widget_value ();
      data->name = "externalOther";
      if (! lw_get_some_values (id, data)) abort ();
      data->enabled = data->selected;
      data->name = "otherText";
      lw_modify_all_widgets (id, data, True);
      free_widget_value (data);
      UNBLOCK_INPUT;
    }
  else if (!strcmp (name, "cancel"))
    {
      editmode.ok = -1;
      lw_destroy_all_widgets (id);
    }
  else if (!strcmp (name, "help"))
    {
      Lisp_Object v = Fmake_vector (3, Qt);
      XVECTOR (v)->contents [0] = build_string ("ok");
      XVECTOR (v)->contents [1] = list1 (intern ("ignore"));
      Fpopup_dialog_box (list2 (build_string ("dbx_editmode_help"), v));
    }
  else if (!strcmp (name, "ok"))
    {
      BLOCK_INPUT;
      editmode.ok = 1;
      data = malloc_widget_value ();
      data->name = "externalEmacs";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.external = 0;
      data->name = "externalViXterm";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.external = 1;
      data->name = "externalViCmdtool";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.external = 2;
      data->name = "externalOther";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.external = 3;
      data->name = "otherText";
      if (! lw_get_some_values (id, data)) abort ();
      editmode.other = data->value;

      data->name = "emacsView";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.view = 0;
      data->name = "viView";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.view = 1;
      data->name = "lessView";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.view = 2;

      data->name = "editEmacs";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.edit = 0;
      data->name = "editVi";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.edit = 1;

      data->name = "windowOne";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.window = 0;
      data->name = "windowSeveral";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.window = 1;
      data->name = "windowMany";
      if (! lw_get_some_values (id, data)) abort ();
      if (data->selected) editmode.window = 2;

      data->name = "splitScreens";
      if (! lw_get_some_values (id, data)) abort ();
      editmode.split = !!data->selected;

      free_widget_value (data);
      lw_destroy_all_widgets (id);
      UNBLOCK_INPUT;
    }
  else
    {
      abort ();
    }
}

static int
editmode_done (void *arg)
{
  return (editmode.ok != 0);
}

DEFUN ("energize-edit-mode-prompt", Fenergize_edit_mode_prompt,
       Senergize_edit_mode_prompt, 6, 6, 0, "")
     (external, edit_mode, view_mode, other_text, window, split)
     Lisp_Object external, edit_mode, view_mode, other_text, window, split;
{
  int dbox_id;
  struct screen *s = selected_screen;
  widget_value *data;
  Widget parent, dbox;

  if (!SCREEN_IS_X (s)) error ("not an X screen");
  parent = s->display.x->widget;

  CHECK_FIXNUM (external, 0);
  CHECK_FIXNUM (edit_mode, 0);
  CHECK_FIXNUM (view_mode, 0);
  CHECK_FIXNUM (window, 0);
  CHECK_FIXNUM (split, 0);
  CHECK_STRING (other_text, 0);

  editmode.ok = 0;
  editmode.external = XINT (external);
  editmode.view = XINT (view_mode);
  editmode.edit = XINT (edit_mode);
  editmode.window = XINT (window);
  editmode.split = XINT (split);
  editmode.other = 0;

  BLOCK_INPUT;
  data = malloc_widget_value ();
  data->name = "editmode";
  data->value = "editmode";
  data->enabled = 1;

  dbox_id = ++popup_id_tick;
  dbox = lw_create_widget ("editmode", "editmode", dbox_id, data, parent,
			   1, 0, edit_mode_callback, 0);
  data->value = 0;

  data->name = "button1"; data->call_data = data->value = "ok";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button2"; data->call_data = data->value = "cancel";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button3"; data->call_data = data->value = "help";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "externalBox";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "otherText"; data->call_data = "otherText";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "message"; data->value = "editmode";
  lw_modify_all_widgets (dbox_id, data, True);

  data->selected = 1;
  switch (editmode.external)
    {
    case 0: data->name = "externalEmacs"; break;
    case 1: data->name = "externalViXterm"; break;
    case 2: data->name = "externalViCmdtool"; break;
    case 3: data->name = "externalOther"; break;
    default: abort ();
    }
  lw_modify_all_widgets (dbox_id, data, True);
  switch (editmode.view)
    {
    case 0: data->name = "emacsView"; break;
    case 1: data->name = "viView"; break;
    case 2: data->name = "lessView"; break;
    default: abort ();
    }
  lw_modify_all_widgets (dbox_id, data, True);
  switch (editmode.edit)
    {
    case 0: data->name = "editEmacs"; break;
    case 1: data->name = "editVi"; break;
    default: abort ();
    }
  lw_modify_all_widgets (dbox_id, data, True);
  switch (editmode.window)
    {
    case 0: data->name = "windowOne"; break;
    case 1: data->name = "windowSeveral"; break;
    case 2: data->name = "windowMany"; break;
    default: abort ();
    }
  lw_modify_all_widgets (dbox_id, data, True);
  
  data->name = "otherText";
  data->selected = 0;
  data->value = (char *) XSTRING (other_text)->data;
  data->enabled = (editmode.external == 3);
  lw_modify_all_widgets (dbox_id, data, True);

  data->name = "splitScreens";
  data->enabled = 1;
  data->selected = editmode.split;
  data->value = 0;
  lw_modify_all_widgets (dbox_id, data, True);

  free_widget_value (data);

  dbox_up_p++;
  lw_pop_up_all_widgets (dbox_id);
  UNBLOCK_INPUT;

  wait_delaying_user_input (editmode_done, 0);

  if (editmode.ok == -1)
    return Fcons (external,
		  list5 (edit_mode, view_mode, other_text, window, split));
  else if (editmode.ok == 1)
    return Fcons (make_number (editmode.external),
		  list5 (make_number (editmode.view),
			 make_number (editmode.edit),
			 build_string (editmode.other ? editmode.other : ""),
			 make_number (editmode.window),
			 make_number (editmode.split)));
  else
    abort ();
}

extern Time mouse_timestamp;
extern Time global_mouse_timestamp;

static LWLIB_ID search_id;
static int last_search_up_p;

static void
hide_search_dialog (Widget w, LWLIB_ID id)
{
#if 0
  /* I'd like to do this, but the widget occasionally gets FUCKED */
  XUnmapWindow (XtDisplay (w), XtWindow (w));
#else
  lw_destroy_all_widgets (id);
#endif
}


static void
search_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  Widget parent = widget;
  Lisp_Object event;
  widget_value *data;
  char *name = (char *) client_data;
  Lisp_Object search, replace;
  Lisp_Object case_sensitive_p, regexp_p, direction;

  if ((int) client_data == -1) name = "done";	/* WM_DELETE_WINDOW */

  while (parent && XtClass (parent) != xmDialogShellWidgetClass)
    parent = XtParent (parent);
  if (! parent) abort ();

  if (!strcmp (name, "done"))
    {
      hide_search_dialog (parent, id);
      return;
    }
  else if (!strcmp (name, "gotoStart"))
    {
      event = Fallocate_event ();
      XEVENT (event)->event_type = menu_event;
      XEVENT (event)->event.eval.function = Qcall_interactively;
      XEVENT (event)->event.eval.object = intern ("beginning-of-buffer");
      mouse_timestamp = global_mouse_timestamp;
      enqueue_command_event (event);
      return;
    }
  else if (!strcmp (name, "gotoEnd"))
    {
      event = Fallocate_event ();
      XEVENT (event)->event_type = menu_event;
      XEVENT (event)->event.eval.function = Qcall_interactively;
      XEVENT (event)->event.eval.object = intern ("end-of-buffer");
      mouse_timestamp = global_mouse_timestamp;
      enqueue_command_event (event);
      return;
    }
  else if (!strcmp (name, "scrollForward"))
    {
      event = Fallocate_event ();
      XEVENT (event)->event_type = menu_event;
      XEVENT (event)->event.eval.function = Qcall_interactively;
      XEVENT (event)->event.eval.object = intern ("scroll-up");
      mouse_timestamp = global_mouse_timestamp;
      enqueue_command_event (event);
      return;
    }
  else if (!strcmp (name, "scrollBack"))
    {
      event = Fallocate_event ();
      XEVENT (event)->event_type = menu_event;
      XEVENT (event)->event.eval.function = Qcall_interactively;
      XEVENT (event)->event.eval.object = intern ("scroll-down");
      mouse_timestamp = global_mouse_timestamp;
      enqueue_command_event (event);
      return;
    }
#if 0
  else if (!strcmp (name, "help"))
    {
      Lisp_Object v = Fmake_vector (3, Qt);
      XVECTOR (v)->contents [0] = build_string ("ok");
      XVECTOR (v)->contents [1] = list1 (intern ("ignore"));
      Fpopup_dialog_box (list2 (build_string ("dbx_search_help"), v));
      return;
    }
#endif

  BLOCK_INPUT;
  data = malloc_widget_value ();
  data->name = "searchText";
  if (! lw_get_some_values (id, data)) abort ();
  search = build_string (data->value);
  data->name = "replaceText";
  if (! lw_get_some_values (id, data)) abort ();
  replace = build_string (data->value);
  data->name = "regexpSearch";
  if (! lw_get_some_values (id, data)) abort ();
  regexp_p = (data->selected ? Qt : Qnil);
  data->name = "caseSearch";
  if (! lw_get_some_values (id, data)) abort ();
  case_sensitive_p = (data->selected ? Qt : Qnil);

  data->name = "directionForward";
  if (! lw_get_some_values (id, data)) abort ();
  direction = data->selected ? Qt : Qnil;

  if (!strcmp (name, "search"))
    replace = Qnil;
  else if (!strcmp (name, "replace"))
    ;
  else if (!strcmp (name, "replace_all"))
    {
      replace = list1 (replace);
/*      hide_search_dialog (parent, id); */
    }
  else
    abort ();

  free_widget_value (data);
  UNBLOCK_INPUT;

  event = Fallocate_event ();
  XEVENT (event)->event_type = menu_event;
  XEVENT (event)->event.eval.function = intern ("energize-search-internal");
  XEVENT (event)->event.eval.object =
    (NILP (replace)
     ? list4 (case_sensitive_p, regexp_p, direction, search)
     : list5 (case_sensitive_p, regexp_p, direction, search, replace));

  mouse_timestamp = global_mouse_timestamp;
  enqueue_command_event (event);
}


DEFUN ("energize-search", Fenergize_search, Senergize_search, 0, 0, "",
       "Pop up the search-and-replace dialog box.")
     ()
{
  int dbox_id;
  struct screen *s = selected_screen;
  widget_value *data;
  Widget parent, dbox;

  if (!SCREEN_IS_X (s)) error ("not an X screen");
  parent = s->display.x->widget;

  BLOCK_INPUT;
  data = malloc_widget_value ();

  dbox_id = (search_id ? search_id : (++popup_id_tick));
  dbox = lw_create_widget ("search", "search", dbox_id, NULL, parent,
			   1, 0, search_callback, 0);
  data->enabled = 1;
  data->value = 0;

  data->name = "button1"; data->value = data->call_data = "search";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button2"; data->value = data->call_data = "replace";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button3"; data->value = data->call_data = "replace_all";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = "button4"; data->value = data->call_data = "done";
  lw_modify_all_widgets (dbox_id, data, True);

  data->value = 0;
  data->name = data->call_data = "gotoStart";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "gotoEnd";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "scrollBack";
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "scrollForward";
  lw_modify_all_widgets (dbox_id, data, True);

  data->value = 0;
  data->name = data->call_data = "caseSearch";
  data->selected = NILP (current_buffer->case_fold_search);
  lw_modify_all_widgets (dbox_id, data, True);

  data->name = data->call_data = "directionForward";
  data->selected = 1;
  lw_modify_all_widgets (dbox_id, data, True);
  data->name = data->call_data = "directionBackward";
  data->selected = 0;
  lw_modify_all_widgets (dbox_id, data, True);
  
  free_widget_value (data);

  lw_pop_up_all_widgets (dbox_id);
  last_search_up_p = 0;
  if (search_id)
    {
      Widget w = lw_get_widget (dbox_id, parent, True);
      if (! w) abort ();
      XMapRaised (XtDisplay (w), XtWindow (w));
    }
  else
    {
      search_id = dbox_id;
      dbox_up_p++;
    }
  UNBLOCK_INPUT;

  return Qnil;
}



/*************** Definition of Emacs Lisp-callable functions ***************/

void
syms_of_editorside() 
{
  energize_connection = 0;
  inside_ProcessEnergizeRequest1 = 0;

  image_cache = make_strings_hashtable (50);

  staticpro (&energize_buffers_list);
  energize_buffers_list = Qnil;

  defsubr(&Ssend_buffer_modified_request);
  defsubr(&Senergize_list_menu);
  defsubr(&Senergize_execute_menu_item);
  defsubr(&Senergize_execute_command_internal);
  defsubr(&Sconnect_to_energize_internal);
  defsubr(&Sconnected_to_energize_p);
  defsubr(&Sclose_connection_to_energize);
  defsubr(&Shandle_energize_request);
  defsubr(&Senergize_buffer_p);
  defsubr(&Senergize_buffer_type);
  defsubr(&Sset_energize_buffer_type_internal);
  defsubr(&Senergize_buffer_id);
  defsubr(&Senergize_request_kill_buffer);
  defsubr(&Senergize_send_region);
  defsubr(&Senergize_user_input_buffer_mark);
  defsubr(&Senergize_update_menubar);
  defsubr(&Senergize_extent_menu_p);
  defsubr(&Senergize_query_buffer);
  defsubr(&Senergize_barf_if_buffer_locked);
  defsubr(&Senergize_psheets_visible_p);
  defsubr(&Senergize_buffer_has_psheets_p);
  defsubr(&Senergize_protocol_level);
  defsubr(&Senergize_edit_mode_prompt);
  defsubr(&Senergize_search);

  search_id = 0;

  DEFVAR_BOOL ("   inside-parse-buffer", &inside_parse_buffer,
               "internal variable used to control extent deletion.");
  inside_parse_buffer = 0;
  Qinside_parse_buffer = intern ("   inside-parse-buffer");

  DEFVAR_LISP ("energize-create-buffer-hook", &Venergize_create_buffer_hook,
               "Hook called when buffer is created by energize; takes \n\
BUFFER as its only argument.");
  Venergize_create_buffer_hook = Qnil;
  
  DEFVAR_LISP ("energize-buffer-modified-hook", 
               &Venergize_buffer_modified_hook,
               "Hook to call when Energize buffer is modified.");
  Venergize_buffer_modified_hook = Qnil;
  Qenergize_buffer_modified_hook = intern ("energize-buffer-modified-hook");
  
  DEFVAR_LISP ("energize-kernel-modification-hook", 
               &Venergize_kernel_modification_hook,
               "Hook called when a buffer is being modified by energize;\n\
takes no arguments.");
  Venergize_kernel_modification_hook = Qnil;
  
  DEFVAR_BOOL ("ignore-kernel", 
               &ignore_kernel,
               "Set when the kernel should be ignored -- for debugging.");
  ignore_kernel = 0;
  
  DEFVAR_LISP ("energize-kernel-busy", &Venergize_kernel_busy,
               "True if the Energize kernel is busy.");
  Venergize_kernel_busy = Qnil;
  Qenergize_kernel_busy = intern ("energize-kernel-busy");

  DEFVAR_LISP ("energize-kernel-busy-hook", &Venergize_kernel_busy_hook,
               "Hook called when the Energize kernel becomes busy or non busy.");
  Venergize_kernel_busy_hook = Qnil;

  DEFVAR_LISP ("energize-menu-update-hook", &Venergize_menu_update_hook,
               "Hook called when the Energize kernel updates the menubar.");
  Venergize_menu_update_hook = Qnil;

  DEFVAR_LISP ("energize-attributes-mapping", &Venergize_attributes_mapping,
	       "A-list to map kernel attributes indexes to Emacs attributes");
  Venergize_attributes_mapping = Qnil;

/*  DEFVAR_INT ("energize-font-lock-p", &energize_font_lock_p,
               "Set to true enable to use Energize with font-lock");
  energize_font_lock_p = 1;
 */

  Qbefore_change_function = intern ("before-change-function");

  Qafter_change_function = intern ("after-change-function");

  Qfirst_change_function = intern ("first-change-function");

  Qbuffer_locked_by_kernel = intern ("buffer-locked-by-kernel");

  Qdefault_directory = intern ("default-directory");

  Qbuffer_file_name = intern ("buffer-file-name");

  Qenergize_user_input_buffer_mark = 
    intern ("energize-user-input-buffer-mark");

  Qenergize_user_input_mode = intern ("energize-user-input-mode");

  Qenergize_make_many_buffers_visible
    = intern ("energize-make-many-buffers-visible");

  Qenergize_extent_data = intern ("energize-extent-data");

  Qbuffer_undo_list = intern ("buffer-undo-list");

  Fput (Qbuffer_locked_by_kernel, Qerror_conditions,
	Fcons (Qbuffer_locked_by_kernel, Fcons (Qerror, Qnil)));
  Fput (Qbuffer_locked_by_kernel, Qerror_message,
	build_string ("Buffer is currently locked by kernel"));
}

#endif /* ENERGIZE */
