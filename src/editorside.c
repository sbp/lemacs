/****************************************************************************
 ***
 ***        (c) Copyright 1990 by Sun/Lucid,  All Rights Reserved.
 ***
 *****************************************************************************/

/* system */
#include "config.h"

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
#include "lisp.h"
#include "buffer.h" 
#include "extents.h"
#include "process.h"

/* screen management */
#include "xterm.h"
#include "screen.h"
#include "window.h" 

/* Display Context for the icons */ 
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/StringDefs.h>
#include <cdisplayctx.h>
#include <wimage.h>

/* Energize editor requests and I/O operations */
#include "editorside.h"

#include <editorreq.h>
#include <editorconn.h>
#include <editoption.h>

#include "extents-data.h"

#include <dboxlib.h>
#include <dbox.h>
#include <notelib.h>
#include <note.h>

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
  BITS32	objectId;
  BITS32	genericId;
  BITS32	itemId;
  char		answered_p;
  char		status;
  char*		message;
  struct reply_wait*	next;
};

static struct reply_wait*
global_reply_wait;

extern void free_all_x_bitmaps_indices (void);
Lisp_Object Venergize_kernel_busy;
Lisp_Object Qenergize_kernel_busy;

/************************ Functions ********************/
extern void free_all_x_bitmaps_indices (void);
extern Lisp_Object Venergize_kernel_busy;
extern Lisp_Object make_extent_for_data
(BufferInfo *binfo, Extent_Data *ext, int from, int to, int set_endpoints);

static Extent_Data *extent_to_data (Lisp_Object extent_obj);
static char *copy_string (char *s);
Lisp_Object word_to_lisp (unsigned int item);
unsigned int lisp_to_word (Lisp_Object obj);
void *get_object_internal (Id id, c_hashtable table);
static void *get_object (Id id, BufferInfo *binfo);
void put_object_internal (Id id, c_hashtable table, void *object);
static void put_object (Id id, BufferInfo *binfo, void *object);
void remove_object_internal (Id id, c_hashtable table);
static void remove_object (Id id, BufferInfo *binfo);
static void free_object (void *key, void *contents, void *arg);
static GDataClass *alloc_GDataclass (Id id, BufferInfo *binfo);
static void free_GDataclass (GDataClass *cl, BufferInfo *binfo);
static GenericData *alloc_GenericData (Id id, GDataClass *cl, 
                                       BufferInfo *binfo);
static void free_GenericData (GenericData *gen, BufferInfo *binfo);
static Extent_Data *alloc_Extent_Data (Id id, BufferInfo *binfo);
static void free_Extent_Data (Extent_Data *ext, BufferInfo *binfo, 
                              enum Object_Free_Type free_type);
void energize_extent_finalization (EXTENT extent);
static BufferInfo *alloc_BufferInfo
(Id id, Lisp_Object name, Lisp_Object filename, 
 char *class_str, Editor *editor, Window win);
static void free_buffer_info (BufferInfo *binfo);
BufferInfo *get_buffer_info_for_emacs_buffer 
(Lisp_Object emacs_buf, Editor *editor);
long get_energize_buffer_id (Lisp_Object emacs_buf);
static char *kernel_buffer_type_to_elisp_type (char *kernel_type);
static Lisp_Object get_buffer_type_for_emacs_buffer 
(Lisp_Object emacs_buf, Editor *editor);
static Lisp_Object set_buffer_type_for_emacs_buffer 
(Lisp_Object emacs_buf, Editor *editor, Lisp_Object type);
static BufferInfo *get_buffer_info_for_id (Id id, Editor *editor);
static void put_buffer_info 
(Id id, Lisp_Object emacs_buf, BufferInfo *binfo, Editor *editor);
static void remove_buffer_info (Id id, Lisp_Object emacs_buf, Editor *editor);
static void Post (char *msg);
static void Post1 (char *msg, int a1);
static void Post2 (char *msg, int a1, int a2);
static EmacsPos EmacsPosForEnergizePos (EnergizePos energizePos);
static EnergizePos EnergizePosForEmacsPos (EmacsPos emacs_pos);
Lisp_Object Fenergize_update_menubar (Lisp_Object screen);
Lisp_Object Fenergize_extent_menu_p (Lisp_Object extent_obj);
void notify_delayed_requests (void);
static void mark_IMAGE (IMAGE image);
void free_zombie_bitmaps (void);
static IMAGE alloc_IMAGE (char *string);
static void free_IMAGE (IMAGE image, c_hashtable table);
static IMAGE get_IMAGE (char *string, int len, c_hashtable table);
static IMAGE ParseAnImage (Connection *conn, BufferInfo *binfo);
static int install_an_IMAGE (IMAGE image);
static void uninstall_an_IMAGE (IMAGE image);
Lisp_Object install_extent_IMAGE (EXTENT extent, IMAGE image, EGT glyph_type);
Lisp_Object extent_glyph_at (EXTENT extent, int pos, int endp);
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
static void restore_buffer_state (Lisp_Object state_cons);
static void map_remhash_op (c_hashtable table, void *pred, 
                            void *arg, void *dummy);
static void rename_the_buffer (Lisp_Object new_name);
static void ParseBuffer (Connection *conn, CBuffer *cbu, Editor *editor, 
                         EnergizePos delete_from, EnergizePos delete_to, 
                         Window win, int relative_p);
static void forget_buffer (BufferInfo *binfo);
void WriteExtent (Connection *conn, Extent_Data *ext, 
                  unsigned int start, unsigned int end);
static char *get_buffer_as_string (unsigned int *len);
static int write_an_extent (Lisp_Object extent_obj, void *arg);
static void SaveBufferToEnergize (BufferInfo *binfo);
static BITS32 generic_id_for_extent (Extent_Data *ext);
static int get_energize_menu (Lisp_Object buffer, Lisp_Object extent_obj);
static int something_answered_p (void *arg);
void wait_delaying_user_input (int (*)(), void*);
Lisp_Object Fenergize_request_menu (Lisp_Object buffer, Lisp_Object extent);
Lisp_Object list_choices (Lisp_Object buffer, Lisp_Object extent_obj, 
                          Lisp_Object only_name);
Lisp_Object Fenergize_list_menu (Lisp_Object buffer, Lisp_Object extent_obj, 
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
void HandleLoggingRequest (Editor *editor, CLoggingRequest *creq);
void HandleNewBufferRequest (Editor *editor, CNewBufferRequest *creq);
void HandleModifyBufferRequest (Editor *editor, CModifyBufferRequest *creq);
static void MakeBufferAndExtentVisible 
(Lisp_Object list, Lisp_Object go_there);
void HandleEnsureVisibleRequest (Editor *editor, CEnsureVisibleRequest *creq);
void HandleEnsureManyVisibleRequest 
(Editor *editor, CEnsureManyVisibleRequest *creq);
Lisp_Object HandleProposeMenuRequest 
(Editor *editor, CProposeChoicesRequest *creq);
static void restore_current_buffer (Lisp_Object buffer);
static void unmodify_buffer_and_kill_it (Lisp_Object buffer);
void HandleKillBufferRequest (Editor *editor, CKillBufferRequest *creq);
void HandleRemoveExtentsRequest (Editor *editor, CRemoveExtentsRequest *creq);
static void HandleSaveBufferRequest (Editor *editor, CSaveBufferRequest *creq);
static void HandleSetModifiedFlagRequest 
(Editor *editor, CSetModifiedFlagRequest *creq);
static void add_in_list_of_ids (int **ids, int *n_ids, int id);
static void remove_from_list_of_ids (int **ids, int *n_ids, int id);
static void HandleBufferSheetRequest 
(Editor *editor, CSheetRequest *sreq, BITS32 buffer_id);
static void note_being_destroyed (Widget w, XtPointer call_data, 
                                  XtPointer client_data);
static void note_instantiated (BITS32 bufferId, BITS32 noteId, 
                               NoteWidget nw, void *arg);
static void HandlePostitRequest (Editor *editor, CGenericRequest *preq);
static void HandleShowBusyRequest (Editor *editor, CGenericRequest *preq);
static void add_in_connection_input_buffer (Connection *conn, char *s, int l);
Lisp_Object ProcessJustOneEnergizeRequest (void);
static void post_handle_request (Lisp_Object ignored);
static Lisp_Object ProcessEnergizeRequest1 (void);
static void setup_connection (Editor *ed, unsigned int id1, unsigned int id2);
static Connection *EditorSideConnection (Editor *editor, int fdin, int fdout);
Lisp_Object Fhandle_energize_request (Lisp_Object proc, Lisp_Object string);
void ConnectToEnergize (char *server_str, char *arg);
static void kill_the_buffer (void *key, void *contents, void *arg);
void CloseConnection (void);
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
Lisp_Object Fenergize_query_buffer 
(Lisp_Object filename, Lisp_Object just_ask);
int get_energize_connection_and_buffer_id 
(Lisp_Object buffer, void **conn_ptr, long *buffer_id_ptr);
int get_energize_connection_and_current_buffer_id 
(void **conn_ptr, long *buffer_id_ptr);
int *get_psheets_for_buffer (Lisp_Object buffer, int *count_ptr);
void notify_that_sheet_has_been_hidden (BITS32 id);
void syms_of_editorside (void);



/**************************** Variables *****************************/

/* used by free_zombie_bitmaps () */
static struct 
{
  IMAGE *vec;
  int free;
  int length;
} zombie_IMAGES;

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
Lisp_Object energize_buffers_list;

Editor *energize_connection;
protocol_edit_options *peo;

int request_serial_number;

extern int current_debuggerpanel_exposed_p;
extern int desired_debuggerpanel_exposed_p;
extern int debuggerpanel_sheet;

extern DisplayContext *display_context;

/**************************** Macros *****************************/

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) <= (B) ? (A) : (B))
#define xnew(type) ((type*)xmalloc (sizeof (type)))

#define BUFFER_NOTIFY_BACKGROUND_BIT_SET_P(buffer) 1
  
#define get_extent_data(id,binfo) (Extent_Data*)get_object(id, binfo)
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

static Extent_Data *
extent_to_data (Lisp_Object extent_obj)
{
  Extent_Data *ext = 0;
  
  if (!EXTENTP (extent_obj)) 
    return 0;
  else
    ext = (Extent_Data *) XEXTENT(extent_obj)->data;
  
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
void *
get_object_internal (Id id, c_hashtable table)
{
  void *res;
  void *found = gethash ((void*)id, table, &res);
  
  if (found) CHECK_OBJECT (res, 0);

  return found ? res : 0;
}

static void *
get_object (Id id, BufferInfo *binfo)
{
  return get_object_internal (id, binfo->id_to_object);
}

void
put_object_internal (Id id, c_hashtable table, void *object)
{
  if (!PUT_ABLE_OBJECT (object))
    error ("Can't put 0x%x in table", object);
  CHECK_OBJECT (object, 0);
  puthash ((void*)id, object, table);
}

static void
put_object (Id id, BufferInfo *binfo, void *object)
{
  put_object_internal (id, binfo->id_to_object, object);
  return;
}

void
remove_object_internal (Id id, c_hashtable table)
{
  void *res;

  if (gethash ((void*)id, table, &res))
    {
      if (OBJECT_FREE (res))
        error ("Free'd object 0x%x still in table!", res);
      remhash ((void*)id, table);
    }
  else if (id)
    error ("Id %d not in table!", id);
}

static void
remove_object (Id id, BufferInfo *binfo)
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
          free_Extent_Data ((Extent_Data *) contents, binfo, OFT_MAPHASH);
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
alloc_GDataclass (Id id, BufferInfo *binfo)
{
  GDataClass *cl = xnew (GDataClass);
  cl->seal = GDATA_CLASS_SEAL;
  cl->id = id;
  cl->flags = 0;
  cl->image = 0;
  put_class (cl->id, binfo, cl);
  return cl;
} 

static void
free_GDataclass (GDataClass *cl, BufferInfo *binfo)
{
  if (cl)
    {
      remove_class (cl->id, binfo);
      if (cl->image)
        {
          free_IMAGE (cl->image, binfo->image_table);
          cl->image = 0;
        }

      SET_OBJECT_FREE (cl);
    }
  return;
} 


static GenericData *
alloc_GenericData (Id id, GDataClass *cl, BufferInfo *binfo)
{
  GenericData *gen = xnew (GenericData);
  gen->seal = GDATA_SEAL;
  gen->id = id;
  gen->cl = cl;
  gen->image = 0;
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

      if (gen->image)
        {
          free_IMAGE (gen->image, binfo->image_table);
          gen->image = 0;
        }

      SET_OBJECT_FREE (gen);
    }
  return;
} 

static Extent_Data *
alloc_Extent_Data (Id id, BufferInfo *binfo)
{              
  Extent_Data *ext = xnew (Extent_Data);
  ext->seal = EXTENT_SEAL;
  ext->id = id;
  ext->extent = 0;
  ext->start_glyph_index = Qnil;
  ext->end_glyph_index = Qnil;
  put_extent_data (ext->id, binfo, ext);
  return ext;
}

static void
free_Extent_Data 
(Extent_Data *ext, BufferInfo *binfo, enum Object_Free_Type free_type)
{              
  if (ext)
    {
      Lisp_Object extent_obj = ext->extent;
      ext->extent = 0;

      if (extent_obj && (XEXTENT (extent_obj)->data == (void *) ext))
        {
          detach_extent (XEXTENT(extent_obj));
          XEXTENT (extent_obj)->data = 0;
        }

      ext->start_glyph_index = Qnil;
      ext->end_glyph_index = Qnil;

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
  Extent_Data *ext = (Extent_Data *)extent->data;
  Lisp_Object buffer = extent->buffer;
  BufferInfo *binfo;
#if 1
  if (ext)
    ext->extent = 0;
  extent->data = 0;
  return;
#else
  if (!ext || (XTYPE (buffer) != Lisp_Buffer))
    return;

  binfo = energize_connection ? get_buffer_info_for_emacs_buffer (buffer, energize_connection) : 0;
    
  if (binfo && ext)
    free_Extent_Data (ext, binfo, OFT_GC);
  else
    ext->extent = 0;

  extent->data = 0;
  return;
#endif
}

static BufferInfo *
alloc_BufferInfo (Id id, Lisp_Object name, Lisp_Object filename,
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

  /* try to re-use a buffer with the smae file name if one already exists.
   * If a buffer already exists but is modified we should do a dialog and
   * ask the user what to do.  For now I'll just use a new buffer in that case.
   * ParseBuffer will erase the buffer.
   **/
  if (!NILP (filename))
    {
      buffer = Fget_file_buffer (filename);

      if (!NILP (buffer) && !NILP (Fbuffer_modified_p (buffer)))
	buffer = Qnil;
    }

  if (NILP (buffer))
    buffer = Fgenerate_new_buffer (name);

  binfo->seal = BUF_INFO_SEAL;
  binfo->id = id;
  binfo->flags = 0;
  binfo->editor = editor;
  binfo->id_to_object = make_hashtable (100);
  binfo->image_table = 0;
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

  if (nw){
    Lisp_Object window = Fscreen_selected_window (binfo->screen);
    Fset_window_buffer (window, binfo->emacs_buffer);
    BLOCK_INPUT;
    set_text_widget ((NoteWidget)nw,
		     XSCREEN(binfo->screen)->display.x->widget);
    UNBLOCK_INPUT;
  }

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
BufferInfo*
get_buffer_info_for_emacs_buffer (Lisp_Object emacs_buf, Editor *editor)
{
  BufferInfo *res;
  if (!editor || !editor->binfo_hash)
    return 0;
  else
    return (gethash ((void *)emacs_buf, editor->binfo_hash, (void *)&res)
	    ? res : 0);
}

long
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
        
      if (XTYPE (type) == Lisp_Symbol)
        XSET (type, Lisp_String, XSYMBOL (type)->name);

      if (XTYPE (type) == Lisp_String) 
        type_string = (char *)XSTRING (type)->data;

      type_string = copy_string (type_string);

      if (!type_string) return Qnil;

      binfo->buffer_type = type_string;

      return intern (binfo->buffer_type);
    }
}

static BufferInfo*
get_buffer_info_for_id (Id id, Editor *editor)
{
  BufferInfo *res;
  return (gethash ((void *)id, editor->binfo_hash, (void *)&res))?res:0;
}

static void
put_buffer_info (Id id, Lisp_Object emacs_buf, BufferInfo *binfo,
                 Editor *editor)
{
  puthash ((void *)id, binfo, editor->binfo_hash);
  puthash ((void *)emacs_buf, binfo, editor->binfo_hash);
}

static void
remove_buffer_info (Id id, Lisp_Object emacs_buf, Editor *editor)
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
  return (energizePos >= (1 << VALBITS))?(Fpoint_max()):(energizePos + 1);
}

static EnergizePos
EnergizePosForEmacsPos (EmacsPos emacs_pos)
{
  return (emacs_pos - 1);
}



extern int windows_or_buffers_changed;

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
      Extent_Data *ext = extent_to_data (extent_obj);
      return (ext && ext->extentType == CEGeneric) ? Qt : Qnil;
    }
}


/* Do what is needed so that the delayed requests will be notified by
** the event loop */

void 
notify_delayed_requests ()
{
  extern void mark_process_as_being_ready (struct Lisp_Process* process);

  if (energize_connection && !NILP (energize_connection->proc) && energize_connection->conn
      && CRequestDelayedP (energize_connection->conn))
    mark_process_as_being_ready (XPROCESS (energize_connection->proc));
}


/******************* IMAGE storage maintenance *******************/

static void
mark_IMAGE (IMAGE image)
{
  if (!zombie_IMAGES.vec)
    {
      zombie_IMAGES.length = 64;
      zombie_IMAGES.vec = 
        (IMAGE *) xmalloc (zombie_IMAGES.length * sizeof (IMAGE));
      zombie_IMAGES.free = 0;
    }

  if (zombie_IMAGES.free == zombie_IMAGES.length)
    {
      zombie_IMAGES.length = (zombie_IMAGES.length * 2);
      zombie_IMAGES.vec = 
        (IMAGE *) xrealloc
          (zombie_IMAGES.vec, zombie_IMAGES.length * sizeof (IMAGE));
    }

  if (image->refcount != 0)
    error ("Can't mark image that is in use.");

  zombie_IMAGES.vec[zombie_IMAGES.free++] = image;
}

/* called by redisplay() -- presumably by the time this is called,
   all "dead" references into the x_bitmaps[] vector have been flushed
   from the display state */
void
free_zombie_bitmaps ()
{

  while (zombie_IMAGES.free)
    {
      struct image_index_cell *current;
      IMAGE image = zombie_IMAGES.vec[zombie_IMAGES.free - 1];
      
      if (image->refcount != 0)
	error ("Can't free image that is in use.");
      
      uninstall_an_IMAGE (image);
  
      BLOCK_INPUT;
      if (image->image)
	{
	  FreeImage (image->image);  
	}
      if (image->pixmap && image->pixmap != 1)
        XFreePixmap (x_current_display, image->pixmap);
      UNBLOCK_INPUT;
      free (image);

      zombie_IMAGES.free--;
    }
}


static IMAGE
alloc_IMAGE (char *string)
{
  IMAGE new_IMAGE = (IMAGE) xmalloc (sizeof (struct Image_Entry_Struct));
  bzero (new_IMAGE, sizeof (struct Image_Entry_Struct));
  new_IMAGE->defining_string = string;
  return new_IMAGE;
}

static void
free_IMAGE (IMAGE image, c_hashtable table)
{
  if (image)
    {
      image->refcount--;

      if (image->refcount == 0)
        {
          if (table && image->defining_string)
            remhash (image->defining_string, table);
          if (image->defining_string) free (image->defining_string);
          image->defining_string = 0;
          mark_IMAGE (image);
        }
    }
  return;
}

static IMAGE
get_IMAGE (char *string, int len, c_hashtable table)
{
  IMAGE res;
  Box box;

  if (!len) return 0;

  if (gethash ((void *)string, table, (void **)&res))
    return res;
  else
    {
      unsigned int width, height;

      string = copy_string (string);
      res = alloc_IMAGE (string);
      puthash ((void *) string, (void *)res, table);

      BLOCK_INPUT;
      res->image = ParseImage (&string, string + len);
      res->pixmap = 1;
      BboxImage (res->image, selected_screen->display_context, &box);
      res->width = box.w;
      res->height = box.h;
      UNBLOCK_INPUT;

      return res;
    }
}

/* Parses an image from the image language */
static IMAGE
ParseAnImage (Connection *conn, BufferInfo *binfo)
{
  ReqLen l;
  char *s = CGetVstring (conn, &l);
  if (!binfo->image_table) 
    binfo->image_table = make_strings_hashtable (50);
  return get_IMAGE (s, l, binfo->image_table);
}

/* Install an image in the glyph vector */
static int
install_an_IMAGE (IMAGE image)
{
  int index = allocate_x_bitmaps_index ();

  image->bitmap_index = index;
  x_bitmaps[index].image = image->pixmap;
  x_bitmaps[index].width = image->width;
  x_bitmaps[index].height = image->height;
  x_bitmaps[index].cimage = image->image;
  
  return index;
}

/* uninstall an image in the glyph vector */
static void
uninstall_an_IMAGE (IMAGE image)
{
  int index = image->bitmap_index;

  if (index)
    {
      free_x_bitmaps_index (index);
      image->bitmap_index = 0;
    }
}

Lisp_Object
install_extent_IMAGE (EXTENT extent, IMAGE image, EGT glyph_type)
{
  Extent_Data *ext = (Extent_Data *) extent->data;
  int image_index;
  Lisp_Object return_value;

  if (!ext)
    return Qnil;
  else if (XEXTENT(ext->extent) != extent)
    abort();

  if (image)
    {
      if (image->width <= 0 || image->height <= 0)
	return Qnil;
      
      if (!image->bitmap_index)
	image->bitmap_index = install_an_IMAGE (image);
      
      return_value = make_number (image->bitmap_index);
      
      /* install the glyph */
      if ((glyph_type == EGT_START_GLYPH) &&
	  !EQ (ext->start_glyph_index, return_value))
        {
          SET_EXTENT_FLAG (extent, EF_START_GLYPH);
          ext->start_glyph_index = return_value;
        }
      else if ((glyph_type == EGT_END_GLYPH) &&
	       !EQ (ext->end_glyph_index, return_value))
        {
	  SET_EXTENT_FLAG (extent, EF_END_GLYPH);
          ext->end_glyph_index = return_value;
        }
      
      return return_value;
    }
  else
    {
      if (glyph_type == EGT_START_GLYPH)
	{
	  CLEAR_EXTENT_FLAG (extent, EF_START_GLYPH);
	  ext->start_glyph_index = Qnil;
	}
      else
	{
	  CLEAR_EXTENT_FLAG (extent, EF_END_GLYPH);
	  ext->end_glyph_index = Qnil;
	}
    }
}

/* Parses Classes from the connection buffer.  Defines them for
 * the buffer given as argument */
static void
ParseClasses (Connection *conn, unsigned int number, BufferInfo *binfo,
              unsigned int modify_ok)
{
  CClass *ptr;                  /* pointer to class data in buffer */
  GDataClass *cl;               /* unmodified class data */
  IMAGE image;
  int i;
  
  for (i = 0; i < number; i++)
    {
      ptr = CGet (conn, CClass);
      image = ParseAnImage (conn, binfo);
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
      if (cl->image != image)
        {
          free_IMAGE (cl->image, binfo->image_table);
          cl->image = image;
          if (image)
	    {
	      image->refcount++;
	      install_an_IMAGE (image);
	    }
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
  IMAGE image;
  int i;
  
  for (i = 0; i < number; i++)
    {
      ptr = CGet (conn, CGeneric);
      image = ParseAnImage (conn, binfo);
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
	  gen->image = image;
	  if (image) image->refcount++;
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
	if (gen->image != image)
	  {
	    modified = 1;
	    free_IMAGE (gen->image, binfo->image_table);
	    gen->image = image;
	    if (image)
	      {
		image->refcount++;
		install_an_IMAGE (image);
	      }
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
  Extent_Data *ext;
  GenericData *gen;
  int extent_start;
  int extent_end;
  int modifying_p = 0;
  int set_extent_endpoints = 1;

  ext = get_extent_data (ptr->extentId, binfo);
  
  if (!ext)
    ext = alloc_Extent_Data (ptr->extentId, binfo);
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
static void
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
}

static void
map_remhash_op (c_hashtable table, void *pred, void *arg, void *dummy)
{
  remhash_predicate predicate = (remhash_predicate) pred;
  map_remhash (predicate, table, arg);
  return;
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
  Frename_buffer (name);
  UNGCPRO;
}

Lisp_Object
safe_funcall_hook (Lisp_Object hook, int nargs, Lisp_Object arg1,
		   Lisp_Object arg2, Lisp_Object arg3);

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
  EmacsPos display_start;
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
  specbind (Qafter_change_function, Qnil);
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
      del_range (BEG, Z);
    }
  else
    {
#if 1
      display_window = Fget_buffer_window (binfo->emacs_buffer, Qnil);
#endif
      previous_point = point;

#if 1
      if (!NILP (display_window))
        display_start = Fmarker_position (XWINDOW (display_window)->start);
#endif

      if (from != to)
        {
          struct buffer *buf = XBUFFER (binfo->emacs_buffer);
                        
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
              process_extents_for_destruction (from, to, current_buffer);
            }
	  else
            {
              /* Do not keep window start if we actually delete text */
              should_keep_window_start = 0;
              process_extents_for_destruction (from, to, current_buffer);
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
  Fset_buffer_modified_p (modified_buffer_flag);
  
  /* restore modified hooks and globals, and return the previous buffer */
  UNGCPRO;
  unbind_to (count);
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
      xc_destroy_dbox (binfo->p_sheet_ids [i]);
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
void
WriteExtent 
  (Connection *conn, Extent_Data *ext, unsigned int start, unsigned int end)
{
  switch (ext->extentType)
    {
    case CEAttribute:
      CWriteExtent (conn, CEAttribute, ext->id, start, end,
                    (BITS32)ext->u.attr.attrValue);
      break;
      
    case CEAbbreviation:
      CWriteExtent (conn, CEAbbreviation, ext->id, start, end,
                    (BITS32)ext->u.abbrev.isOpened);
      break;
      
    case CEGeneric:
      CWriteExtent (conn, CEGeneric, ext->id, start, end, 0);
      break;

    case CEWriteProtect:
      CWriteExtent (conn, CEWriteProtect, ext->id, start, end, 0);
      break;
    }
}

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
  Extent_Data *ext = extent_to_data (extent_obj);
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
  Id bufferId = binfo->id;
  CBuffer *cbu;
  CEditorRequest *req;
  char *text;
  int length;
  struct buffer *cur_buff = current_buffer;
  struct buffer *b = XBUFFER (binfo->emacs_buffer);
  int count = specpdl_ptr - specpdl;
  
  binfo_and_n_extents bane;
  
  /* selects the buffer as current */
  Fset_buffer (binfo->emacs_buffer);
  
  /* write header */
  cbu = CWriteBufferSavedHeader (conn);
  cbu->bufferId = bufferId;
  cbu->flags = 0;
  cbu->nClass = 0;
  cbu->nGeneric = 0;
  CWriteVstring0 (conn, "");    /* file name */
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
  unbind_to (count);
}

static BITS32
generic_id_for_extent (Extent_Data *ext)
{
  if (!ext)
    return 0;
  else if (!((ext->extentType == CEGeneric) && ext->u.generic.gData))
    return ext->id;
  else
    return ext->u.generic.gData->id;
}


/********************** Menu ("keywords") operations **********************/


/* gets the menu for the buffer/extent pair at the head of the request buffer.
** returns 1 if succeeds, 0 otherwise (kernel connection closed, or not
** connected) */
static int
get_energize_menu (Lisp_Object buffer, Lisp_Object extent_obj)
{
  Connection*	conn;
  BITS32	buffer_id;
  BITS32	extent_id;
  int result;
  
  if (!get_energize_connection_and_buffer_id (buffer,
					      (void**)&conn,
					      (long*)&buffer_id)
      || !NILP (Venergize_kernel_busy))
    return 0;

  if (EXTENTP (extent_obj))
    extent_id = generic_id_for_extent (extent_to_data (extent_obj));
  else
    extent_id = 0;
  
  CWriteQueryChoicesRequest (conn, buffer_id, extent_id);
  result = CWaitProposeChoiceRequest (conn, buffer_id, extent_id);
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

static void
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
  unbind_to (count);
  return rw->answered_p;
}

static int
execute_energize_menu (Lisp_Object buffer, Extent_Data* ext, char* name,
		       BITS32 item_id, BITS32 flags, Lisp_Object selection,
		       Lisp_Object no_confirm)
{
  Connection*	conn;
  BITS32	buffer_id;
  BITS32	extent_id;
  BufferInfo*	binfo;
  struct reply_wait rw;
  
  if (!get_energize_connection_and_buffer_id (buffer, (void**)&conn,
					      (long*)&buffer_id))
    return 0;

  extent_id = generic_id_for_extent (ext);
  
  if ((flags & CKBuffer) && Fbuffer_modified_p (buffer))
    {
      /* saves buffer if requested and needed */
      binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection);
      if (binfo)
	SaveBufferToEnergize (binfo);
    }

  CWriteExecuteChoicesRequest (conn, buffer_id, extent_id, item_id, 0, 0);
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
	BITS32 data;
	conn->header->data |= CEChasObjectSelection;

	/* writes the length */
	data = XVECTOR (selection)->size;
	CWrite (conn, BITS32, &data);	

	/* writes the elements */
	for (i = 0; i < XVECTOR (selection)->size; i++)
	  {
	    if (XTYPE (XVECTOR (selection)->contents [i]) == Lisp_Cons)
	      data = lisp_to_word (XVECTOR (selection)->contents [i]);
	    else
	      data = XVECTOR (selection)->contents [i];
	    CWrite (conn, BITS32, &data);
	  }
      }
      break;

    case Lisp_Cons:
      {
	Lisp_Object type = Fcar (selection);
	Lisp_Object value = Fcdr (selection);
	if (EQ (type, intern ("ENERGIZE_OBJECT"))
	    && XTYPE (value) == Lisp_String)
	  {
	    conn->header->data |= CEChasObjectSelection;
	    CWriteN (conn, char, XSTRING (value)->data,
		     XSTRING (value)->size);
	  }
      }
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

  
  if (wait_for_reply (&rw) && !rw.status)
    {
      char message [128];
      if (energize_connection && energize_connection->conn)
	sprintf (message, "Energize command failed: %.80s", rw.message);
      else
	sprintf (message, "Connection to Energize was closed.");
      free (rw.message);
      error (message);
    }
}

DEFUN ("energize::request-menu", Fenergize_request_menu,
       Senergize_request_menu, 2, 2, 0,
       "(energize::request-menu buffer extent)\n\
Request the set of menu options from the Energize server that are\n\
appropriate to the buffer and the extent.  Extent can be (), in which case\n\
the options are requested for the whole buffer.")
   (buffer, extent)
    Lisp_Object buffer, extent;
{
  Lisp_Object result = Qnil;
  struct gcpro gcpro1;

  CHECK_BUFFER (buffer, 1);
  
  if (!energize_connection || !energize_connection->conn) return Qnil;

  GCPRO1 (result);
  if (get_energize_menu (buffer, extent))
    if (energize_connection && energize_connection->conn)
      result = HandleProposeMenuRequest (energize_connection,
					 ((CProposeChoicesRequest*)
					  CReadEditorRequest (energize_connection->conn)));
  notify_delayed_requests ();
  UNGCPRO;
  return result;
}

/* Returns a list of vectors representing the menu choices.  Next request
   in connection must be a ProposeChoices.  The list is 
   (buffer extent <item1> ... <itemN>).  <itemI> is (name id1 id2 flags).
   Idi is (high .  low).  We build the list in reverse order and nreverse
   it.  If (only_name != 0), we only return the item of named only_name as
   a vector.  */

Lisp_Object
list_choices (Lisp_Object buffer, Lisp_Object extent_obj, 
              Lisp_Object only_name)
{
  Connection *conn;
  CProposeChoicesRequest *creq;
  int i;
  Lisp_Object item_list;
  Lisp_Object item;
  struct Lisp_Vector *v;
  struct gcpro gcpro1, gcpro2, gcpro3;
  CChoice *choice;
  ReqLen name_length;
  char *name;

  if (energize_connection && energize_connection->conn)
    conn = energize_connection->conn;
  else
    return Qnil;
  
  creq = (CProposeChoicesRequest*)CReadEditorRequest (conn);
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
      if (!NILP (only_name))
	{
	  if (!strcmp ((char*)XSTRING(only_name)->data, name))
	    {
	      if (NILP (item))
		{
		  item = Fmake_vector (4, Qnil);
		  v = XVECTOR (item);
		  v->contents [0] = only_name;
		}
              v->contents [1] = word_to_lisp (choice->choiceId);
              v->contents [2] = Qnil;
              v->contents [3] = choice->flags;
	    }
	}
      else
	{
	  item = Fmake_vector (4, Qnil);
	  v = XVECTOR (item);
	  v->contents [0] = build_string (name);
	  v->contents [1] = word_to_lisp (choice->choiceId);
	  v->contents [2] = Qnil;
	  v->contents [3] = choice->flags;
	  item_list = Fcons (item, item_list); /* pushes in the list */
        }
    }
  
  if (NILP (only_name))
    item_list = Fcons (buffer, Fcons (extent_obj, Fnreverse (item_list)));
  UNGCPRO;
  
  return NILP (only_name) ? item_list : item;
}

DEFUN ("energize-list-menu", Fenergize_list_menu,
       Senergize_list_menu, 2, 3, 0,
       "(energize-list-menu buffer extent [only-name])\n\
Request the set of menu options from the Energize server that are\n\
appropriate to the buffer and the extent.  Extent can be (), in which case\n\
the options are requested for the whole buffer.  Returns the options as\n\
a list that can be passed to energize-activate-menu.  Items\n\
in the list can also be passed to energize-execute-menu-item.\n\
The list is (buffer extent or () <item1> ... <itemN>).\n\
where <itemI> is (name id1 id2 flags); idI is (high . low).\n\
If optional argument only-name is provided only the item with name only-name\n\
is returned, or () if no such item exists.")
     (buffer, extent_obj, only_name)
     Lisp_Object buffer, extent_obj, only_name;
{
  Lisp_Object res;
  CHECK_BUFFER (buffer, 1);
  
  if (!energize_connection || !energize_connection->conn) return Qnil;

  if (!NILP (only_name))
    CHECK_STRING (only_name, 1);
  
  if (get_energize_menu (buffer, extent_obj))
    res = list_choices (buffer, extent_obj, only_name);
  else
    res = Qnil;
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

/********************************* kill buffer interface ****************/

DEFUN ("energize-buffer-type", Fenergize_buffer_type, Senergize_buffer_type,
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
  
  if (!(binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection))) 
    return Qnil;
  else 
    return set_buffer_type_for_emacs_buffer (buffer, energize_connection, type);
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
  if (!(binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
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
  if (!(binfo = get_buffer_info_for_emacs_buffer (buffer, energize_connection)))
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
void
HandleLoggingRequest (Editor *editor, CLoggingRequest *creq)
     /* I'm a lumberjack and I'm ok... */
{
  ReqLen name_len;
  char* data_filename = CGetVstring (editor->conn, &name_len);

#ifdef EMACS_BTL
  {
    extern Lisp_Object Vexecution_path;
    char *execname = 
      (XTYPE (Vexecution_path) == Lisp_String)?
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
void
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
void
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
void
HandleEnsureVisibleRequest (Editor *editor, CEnsureVisibleRequest *creq)
{
  BufferInfo *binfo;
  Extent_Data *ext;
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

void
HandleEnsureManyVisibleRequest (Editor *editor,
				CEnsureManyVisibleRequest *creq)
{
  BufferInfo *binfo;
  Extent_Data *ext;
  Lisp_Object buffer_extent_list;
  int n;
  BITS32 buffer_id;
  BITS32 extent_id;
  struct gcpro gcpro1;

  buffer_extent_list = Qnil;
  GCPRO1 (buffer_extent_list);
  
  for (n = creq->head.data,
       buffer_id = creq->bufferId,
       extent_id = creq->extentId;
       n;
       n--,
       buffer_id = n ? *(CGet (editor->conn, BITS32)) : 0,
       extent_id = n ? *(CGet (editor->conn, BITS32)) : 0)
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

/* propose a menu and returns the selected entry */
Lisp_Object
HandleProposeMenuRequest (Editor *editor, CProposeChoicesRequest *creq)
{
  BufferInfo *binfo;
  Extent_Data *ext = 0;
  Connection *conn = editor->conn;
  
  CChoice *cmd;
  char *cmdString;
  int i;
  unsigned int ignored;
  unsigned int x;
  unsigned int y;
  int item;
  struct gcpro gcpro1;
  Lisp_Object selected_item = Qnil;
  
  extern struct screen *selected_screen;
  Widget widget = selected_screen->display.x->widget;
  Window xid = widget->core.window;
  
  if (!(binfo = get_buffer_info_for_id (creq->objectId, editor)))
    {
      Post("Server attempt to show menu for a non registered buffer");
      CSkipRequest (editor->conn);
      return;
    }
  
  if (creq->genericId)
    {
      ext = get_extent_data (creq->genericId, binfo);
      if (!ext)
        {
          Post("HandleProposeMenuRequest:: extent has not been registered");
          return;
        }
    }
  
  GCPRO1 (selected_item);
  /* create and display a menu with commands proposed by kernel */
  clear_edit_options (peo);
  CParseEditOptions (conn, creq, peo);
  
  BLOCK_INPUT;
  
/* ####
  put_back_delayed_events (True);
 */

  XQueryPointer (XtDisplay (widget), xid,
                 (Window *)&ignored, (Window *)&ignored,
                 (int *)&x, (int *)&y,
                 (int *)&ignored, (int *)&ignored,
                 &ignored);

  {
#ifdef FREE_CHECKING
    extern void (*__free_hook)();
    int checking_free = (__free_hook != 0);
    
    if (checking_free)
      disable_strict_free_check ();
#endif
    item = activate_menu (widget, x, y, peo);
#ifdef FREE_CHECKING
    if (checking_free)
      enable_strict_free_check ();
#endif
  }

  UNBLOCK_INPUT;
  
  /* returns the selected item. */
  if (item != -1)
    {
      struct Lisp_Vector* v;
      selected_item = Fmake_vector (4, Qnil);

      v = XVECTOR (selected_item);
      v->contents [0] = build_string (peo->options [item].name);
      v->contents [1] = word_to_lisp (peo->options [item].id1);
      v->contents [2] = Qnil;
      v->contents [3] = peo->options [item].flags;
    }

  UNGCPRO;
  return selected_item;
}

/* called by unwind_protect */
static void
restore_current_buffer (Lisp_Object buffer)
{
  Fset_buffer (buffer);
}

/* Kills a buffer */
static void
unmodify_buffer_and_kill_it (Lisp_Object buffer)
{
  int count = specpdl_ptr - specpdl;

  if ((XTYPE (buffer) != Lisp_Buffer) || NILP (XBUFFER (buffer)->name)) 
    return;
  
  /* unmodify the buffer */
  if (buffer != Fcurrent_buffer ())
    {
      record_unwind_protect (restore_current_buffer, Fcurrent_buffer ());
      Fset_buffer (buffer);
    }
  Fset_buffer_modified_p (Qnil);
  unbind_to (count);
  
  /* kill it.  This will call the Energize hook to do the right thing */
  Fkill_buffer (buffer);
}

void
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

void
HandleRemoveExtentsRequest (Editor *editor, CRemoveExtentsRequest *creq)
{
  BufferInfo *binfo;
  int i;
  BITS32 *ids;
  Lisp_Object modified_hook = Qnil;
  Lisp_Object restore_buffer_state_cons;
  Lisp_Object extents_modify = Qnil;
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
  
  ids = CGetN (editor->conn, BITS32, creq->nExtent);
  for (i = 0; i < creq->nExtent; i++)
    {
      Extent_Data *ext = get_extent_data (ids [i], binfo);
      if (ext)
        free_Extent_Data (ext, binfo, OFT_STANDALONE);
    }
  
  /* restore modified hooks and globals */
  unbind_to (count);
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
      record_unwind_protect (restore_current_buffer, Fcurrent_buffer ());
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
  
  unbind_to (count);
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
  
  record_unwind_protect (restore_current_buffer, Fcurrent_buffer ());
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
  unbind_to (count);
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
      free (*ids);
      *ids = 0;
    }
  }
}

static void
HandleBufferSheetRequest (Editor *editor, CSheetRequest *sreq,
			  BITS32 buffer_id)
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
      /* The psheet should be created in all screens where the buffer is
	 visible and not in the selected_screen.
	 Use selected_screen temporarily to be able to see the p_sheet */
      BLOCK_INPUT;
      xc_create_dbox ((Widget)0, (Widget)0, name, sreq->sheetId,
		      buffer_id, 0, (select_callback)HandleControlChange,
		      (void*)conn);
      UNBLOCK_INPUT;
      add_in_list_of_ids (&binfo->p_sheet_ids, &binfo->n_p_sheets,
			  sreq->sheetId);
      if (!strcmp (name, DEBUGGER_PSHEET_NAME)) 
	debuggerpanel_sheet = sreq->sheetId;
      break;
      
    case CSDelete:{
      remove_from_list_of_ids (&binfo->p_sheet_ids, &binfo->n_p_sheets,
			       sreq->sheetId);

      BLOCK_INPUT;
      xc_destroy_dbox (sreq->sheetId);
      UNBLOCK_INPUT;
      if (sreq->sheetId == debuggerpanel_sheet)
	{
	  desired_debuggerpanel_exposed_p = 0;
	  debuggerpanel_sheet = 0;
	}
    }
      break;
      
    case CSHide:
      {
	struct screen *screen;
	Lisp_Object rest;
	Widget w;
	int id = sreq->sheetId;

	BLOCK_INPUT;
	xc_hide_dbox (id);
	if (sreq->sheetId == debuggerpanel_sheet)
	  desired_debuggerpanel_exposed_p = 0;
	else
	  for (rest = Vscreen_list; rest != Qnil; rest = Fcdr(rest))
	    {
	      screen = XSCREEN(Fcar(rest));
	      if (SCREEN_IS_X (screen))
		{
		  w = xc_widget_instance_from_id
		    (id, 0, screen->display.x->column_widget);
		  if (w)
		    {
		      screen->display.x->desired_psheets = 0;
		      screen->display.x->desired_psheet_count = 0;
		      screen->display.x->desired_psheet_buffer = Qnil;
		    }
		}
	    }
	UNBLOCK_INPUT;
      }
      break;
      
    case CSShow:
      BLOCK_INPUT;
      xc_show_dbox (sreq->sheetId);
      UNBLOCK_INPUT;
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
	      window = XWINDOW(screen->selected_window);
	      if (window->buffer == binfo->emacs_buffer)
		{
		  if (SCREEN_IS_X (screen) && !MINI_WINDOW_P (window))
		    energize_show_menubar_of_buffer (window->screen,
						     window->buffer,
						     Qt);
		}
	    }
	}
      break;
    }
}


static void
note_being_destroyed (Widget w, XtPointer call_data, XtPointer client_data)
{
  NoteWidget nw = (NoteWidget)w;
  Widget tw = get_text_widget (nw);
  SCREEN_PTR s;
  Lisp_Object screen;

  if (tw){
    s = (SCREEN_PTR)x_any_window_to_screen (XtWindow (tw));
    if (s){
      BLOCK_INPUT;
      set_text_widget (nw, 0);
      UNBLOCK_INPUT;
      XSET (screen, Lisp_Screen, s);
      Fdelete_screen (screen);
    }
  }
}

static void
note_instantiated (BITS32 bufferId, BITS32 noteId, NoteWidget nw, void* arg)
{
  XtAddCallback ((Widget)nw, XtNdestroyCallback, note_being_destroyed, 0);
  XtVaSetValues ((Widget)nw, "forceXtLoop", 1, 0);
}

static void
HandlePostitRequest (Editor *editor, CGenericRequest *preq)
{
  BufferInfo* binfo;
  Connection* conn = editor->conn;
  NoteWidget notew;
  
  switch (preq->head.reqType)
    {
    case OpenPostitRType:{
      if (!(binfo = get_buffer_info_for_id (preq->openpostit.bufferId,
 					    editor)))
 	{
 	  Post("Server attempt to use postit in a non registered buffer");
 	  CSkipRequest (conn);
 	  return;
 	}
      
      BLOCK_INPUT;
      xc_create_note_window 
        (&preq->openpostit, conn, note_instantiated, 0,
         FONT_WIDTH (SCREEN_NORMAL_FACE (selected_screen).font),
         TOTAL_HEIGHT (SCREEN_NORMAL_FACE (selected_screen).font));
      UNBLOCK_INPUT;
      add_in_list_of_ids (&binfo->note_ids, &binfo->n_notes,
 			  preq->openpostit.bufferId);
    }
      break;
      
    case KillPostitRType:{
      if (!(binfo = get_buffer_info_for_id (preq->killpostit.bufferId,
 					    editor)))
 	{
 	  Post("Server attempt to kill postit in a non registered buffer");
 	  CSkipRequest (conn);
 	  return;
 	}
      
      BLOCK_INPUT;
      xc_destroy_note (preq->killpostit.postitId);
      UNBLOCK_INPUT;
      remove_from_list_of_ids (&binfo->note_ids, &binfo->n_notes,
 			       preq->killpostit.postitId);
      break;
    }}
}

/* show busy */
int needs_to_recompute_menubar_when_kernel_not_busy;

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
  Lisp_Object tail;

  char* why = CGetVstring (editor->conn, &len);
  message (why);
  show_all_menubars_busy (preq->head.data);
  Venergize_kernel_busy = preq->head.data ? Qt : Qnil;

  /* update the menubars if needed and kernel is not busy */
  if (!preq->head.data && needs_to_recompute_menubar_when_kernel_not_busy)
    {
      SCREEN_PTR s;
      Lisp_Object tail;
      
      needs_to_recompute_menubar_when_kernel_not_busy = 0;
      for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
	{
	  if (XTYPE (XCONS (tail)->car) != Lisp_Screen)
	    continue;
	  
	  s = XSCREEN (XCONS (tail)->car);
	  if (SCREEN_IS_X (s) &&
	      ! MINI_WINDOW_P (XWINDOW (s->selected_window)))
	    recompute_screen_menubar (s);
	}
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

Lisp_Object
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
          Post("Energize connection accepted");
          CSkipRequest(editor->conn);
          break;
          
        case NewBufferRType:
          HandleNewBufferRequest(editor, &req->newbuffer);
          break;
          
	case QueryBufferRType:
	  {
	    BITS32 buffer_id;
	    struct reply_wait* rw = find_wait_reply (req->head.serial);
	    CGetVstring (editor->conn, 0); /* skip directory */
	    CGetVstring (editor->conn, 0); /* skip file */
	    buffer_id = *CGet (editor->conn, BITS32);
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
	  /* HandleProposeMenuRequest(editor, &req->generic.proposechoices); */
	  /* ignores extra propose choices now that menus are waited for */
	  CSkipRequest (editor->conn);
          break;
          
        case ChoiceExecutedRType:
	  {
	    struct reply_wait* rw = find_wait_reply (req->head.serial);
	    CChoiceExecutedRequest* ce = &req->generic.choiceexecuted;
	    if (rw)
	      {
		rw->answered_p = 1;
		rw->status = ce->head.data;
		rw->message = CMakeVstring (editor->conn, 0);
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
	  BITS32 buffer_id = req->generic.sheet.bufferId;
	  if (!buffer_id)
	    buffer_id = xc_buffer_id_of_dbox (req->generic.sheet.sheetId);
	  if (buffer_id)
	    HandleBufferSheetRequest (editor, &req->generic.sheet, buffer_id);
	  else
	    {
	      CSheetRSubtype type = (CSheetRSubtype)req->head.data;
	      BITS32 id = req->generic.sheet.sheetId;
	      if (type == CSDelete || type ==CSHide)
		select_screen (selected_screen);
	      BLOCK_INPUT;
	      HandleSheetRequest (editor->conn, (CGenericRequest*) req,
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

/* Called by unwind_protect after executing a request */
static void
post_handle_request (Lisp_Object ignored)
{
/*  in_display--; */
  if (energize_connection && energize_connection->conn)
    CSkipRequest (energize_connection->conn);
}

static Lisp_Object
ProcessEnergizeRequest1 ()
{
  Lisp_Object result;
  int count = specpdl_ptr - specpdl;
  
/*  in_display++; */
  record_unwind_protect (post_handle_request, 0);
  result = ProcessJustOneEnergizeRequest ();
  notify_delayed_requests ();
  unbind_to (count);
  
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
  CWriteProtocol(ed->conn, 0, 6, "editor");
  CWriteLength (ed->conn);
  CWriteRequestBuffer (ed->conn);
};

static Connection *
EditorSideConnection (Editor *editor, int fdin, int fdout)
{
  return NewConnection ((void *)editor, fdin, fdout);
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
void
ConnectToEnergize (char *server_str, char *arg)
{
  int fd;
  struct Lisp_Process *proc;
  Lisp_Object lp;
  Lisp_Object fil;
  char *host;
  unsigned int port;
  long flags;
  int id1;
  int id2;

  extern struct screen *selected_screen;
  Widget widget = selected_screen->display.x->widget;
  
  if (CParseCadillacSpec (server_str, &host, &port)) /* #### rename me */
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
          peo = allocate_edit_options (10);
          request_serial_number = 0;
	  global_reply_wait = 0;

          BLOCK_INPUT;
          /* forces linking */
          NewPixmapImage (0);
          NewAttributeImage (0, 0);
          NewNoteImage (0, 0);
          UNBLOCK_INPUT;
          
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
	  needs_to_recompute_menubar_when_kernel_not_busy = 0;
          
          id1 = 0;
          id2 = 0;
          if (arg)
            sscanf (arg, "%x,%x", &id1, &id2);
          
	  energize_buffers_list = Qnil;

          setup_connection (energize_connection, id1, id2);
        }
    }
}

/* called by maphash to kill all the buffers in CloseConnection */
static void
kill_the_buffer (void *key, void *contents, void *arg)
{
  /* contents is the bufferInfo */
  if (contents)
    {
      BufferInfo *binfo = (BufferInfo *) contents;

      if (OBJECT_SEAL (binfo) != BUF_INFO_SEAL)
        error ("bad object %x to kill_the_buffer()", binfo);
      unmodify_buffer_and_kill_it (binfo->emacs_buffer);
    }
}

/* Close the connection to energize.
 * Kills all the energize related buffer */
void 
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
      /* destroy all dialog boxes */
      xc_destroy_dialogs ();
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
          maphash (kill_the_buffer, ed->binfo_hash, ed);
          unbind_to (count);
	  
          free_hashtable (ed->binfo_hash);
          ed->binfo_hash = 0;
        }
      
      free_edit_options (peo);
      
      if (ed->proc) Fdelete_process (ed->proc);
      ed->proc = Qnil;
      
      energize_buffers_list = Qnil;

      /* now kill buffers created to satisfy requests on old connection */
      free (ed);
      /* finally, flush all of the x_bitmaps indices */
      free_all_x_bitmaps_indices ();
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
  Id bufferId = binfo->id;
  
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
  Id buffer_id = binfo->id;
  CEditorRequest *req;
  struct reply_wait rw;
  
  /* If permision already granted by kernel dont' ask again */
  if (binfo->editable)
    return 0;

  /* If can't ask say we do not know */
  if (!conn || !NILP (Venergize_kernel_busy))
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
  Extent_Data *ext;
  CEditorRequest *req;
  BufferInfo *binfo = bans->binfo;
  Connection *conn = binfo->editor->conn;
  BITS32 *extent_id;
  
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
	  extent_id = CPut (conn, BITS32);
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
  int tmp;
  
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


int
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

int
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
notify_that_sheet_has_been_hidden (BITS32 id)
{
  BITS32 buffer_id = xc_dbox_buffer_id (id);
  if (!buffer_id)
    return;

  if (buffer_id && energize_connection && energize_connection->conn)
    {
      SendSheetStateChange (energize_connection->conn, buffer_id, id, 0);
      CWriteRequestBuffer (energize_connection->conn);
    }
}


/*************** Definition of Emacs Lisp-callable functions ***************/

void
syms_of_editorside() 
{
  energize_connection = 0;
  
  staticpro (&energize_buffers_list);
  energize_buffers_list = Qnil;

  defsubr(&Ssend_buffer_modified_request);
  defsubr(&Senergize_request_menu);
  defsubr(&Senergize_list_menu);
  defsubr(&Senergize_execute_menu_item);
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
    = intern ("energize::make-many-buffers-visible");

  Qbuffer_undo_list = intern ("buffer-undo-list");

  Fput (Qbuffer_locked_by_kernel, Qerror_conditions,
	Fcons (Qbuffer_locked_by_kernel, Fcons (Qerror, Qnil)));
  Fput (Qbuffer_locked_by_kernel, Qerror_message,
	build_string ("Buffer is currently locked by kernel"));
}

