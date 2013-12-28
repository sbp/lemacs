/* "Face" primitives
This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config.h"
#include "lisp.h"
#include "intl.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h> /* for sprintf */

/* Display Context for the icons */ 
#include "xintrinsic.h"
#include <X11/StringDefs.h>
#ifdef USG	/* ####KLUDGE for Solaris 2.2 and up */
#undef USG
#include <X11/Xos.h>
#define USG
#else
#include <X11/Xos.h>
#endif

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

#include "xterm.h"
#include "xobjs.h"
#include "buffer.h"
#include "extents.h"
#include "screen.h"
#include "window.h"
#include "indent.h"

#include "dispmisc.h"
#include "dispextern.h"

#include "faces.h"
#include "hash.h"

#include "EmacsScreen.h"

#define MAX_CACHED_FACES	30

Lisp_Object Qface, Qfacep;
Lisp_Object Qfont, Qforeground, Qbackground, Qunderline, Qbackground_pixmap;
Lisp_Object Qmake_screen_initial_faces;

#ifdef HAVE_X_WINDOWS
Lisp_Object Qx_resource_face;
#endif

/* The priority of the mouse-highlighting attributes, for extent-merging.
 */
int mouse_highlight_priority;

extern int extent_cache_invalid;

void ensure_face_ready (struct screen* s, int f);
static struct face *get_screen_face (struct screen *s, int id);
static struct face *Lisp_Face_to_face (Lisp_Object lisp_face,
				       Lisp_Object screen);
Lisp_Object Fset_face_attribute_internal (Lisp_Object face,
					  Lisp_Object attr_name,
					  Lisp_Object attr_value);

/* Note the difference between `struct face' and `struct Lisp_Face'.

   `struct face' contains the data which redisplay needs to know;
   this is what face_ids point at.

   `struct Lisp_Face' is a GC-marked lisp object which redisplay
   doesn't know about.  It used to be implemented in lisp as a vector,
   but was moved here for speed.

   Eventually these two structures should be merged.

   Both of them are per-screen, but there is an additional set of them
   not associated with any screen (these are the defaults for newly-
   created screens.)
 */

struct Lisp_Face {
  struct lcrecord_header header;
  Lisp_Object name;
  /* Given a screen and an id, we can extract all other slots. */
  Lisp_Object screen;
  int face_id;
  /* We can get this via the screen and the face id, but having a pointer
     here makes things faster.  Note, this assumes that `struct face's
     aren't relocated when screen->faces is enlarged.
   */
  struct face *face;
};


static Lisp_Object mark_face (Lisp_Object, void (*) (Lisp_Object));
static void print_face (Lisp_Object, Lisp_Object, int);
static int face_equal (Lisp_Object, Lisp_Object, int depth);
DEFINE_LRECORD_IMPLEMENTATION ("face", lrecord_face,
			       mark_face, print_face, 0, face_equal,
			       sizeof (struct Lisp_Face));

#define FACEP(obj) RECORD_TYPEP((obj), lrecord_face)
#define XFACE(obj) ((struct Lisp_Face *) (XPNTR (obj)))
#define CHECK_FACE(x, i) \
  { if (!FACEP((x))) wrong_type_argument (Qfacep, (x)); }

/* #### This is external so that it can be called by dispnew.c:mark_redisplay()
   which won't be necessary once both kinds of faces are real lisp objects. */
void
mark_external_face (struct face *face, void (*markobj) (Lisp_Object))
{
  ((markobj) (face->font));
  ((markobj) (face->foreground));
  ((markobj) (face->background));
  ((markobj) (face->back_pixmap));
}

static Lisp_Object
mark_face (Lisp_Object obj, void (*markobj) (Lisp_Object))
{
  struct Lisp_Face *face =  XFACE (obj);
  ((markobj) (face->name));
  mark_external_face (face->face, markobj);
  return (face->screen);
}

extern Lisp_Object Fscreen_name (Lisp_Object);

static void
print_face (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
  struct Lisp_Face *lisp_face = XFACE (obj);
  struct face *face = Lisp_Face_to_face (obj, Qnil);
  if (print_readably)
    error ("printing unreadable object #<face %d>", lisp_face->face_id);
  write_string_1 ("#<face ", -1, printcharfun);
  print_internal (lisp_face->name, printcharfun, 1);
  if (!NILP (face->font))
    {
      write_string_1 (" ", -1, printcharfun);
      print_internal ((!NILP ((Ffontp (face->font)))
		       ? Ffont_name (face->font)
		       : face->font),
		      printcharfun, 0);
    }
  if (!NILP (face->foreground))
    {
      write_string_1 (" fg:", -1, printcharfun);
      print_internal ((!NILP (Fpixelp (face->foreground))
		       ? Fpixel_name (face->foreground)
		       : face->foreground),
		      printcharfun, 0);
    }
  if (!NILP (face->background))
    {
      write_string_1 (" bg:", -1, printcharfun);
      print_internal ((!NILP (Fpixelp (face->background))
		       ? Fpixel_name (face->background)
		       : face->background),
		      printcharfun, 0);
    }
  if (!NILP (face->back_pixmap))
    {
      write_string_1 (" ", -1, printcharfun);
      print_internal (((!NILP (Fpixmapp (face->back_pixmap)) &
			!NILP (Fpixmap_file_name (face->back_pixmap)))
		       ? Fpixmap_file_name (face->back_pixmap)
		       : face->back_pixmap),
		      printcharfun, 0);
    }
  if (face->underline)
    write_string_1 (" underlined", -1, printcharfun);
  if (face->hilited)
    write_string_1 (" hilited", -1, printcharfun);

  write_string_1 (" screen:", -1, printcharfun);
  print_internal (((!NILP (Fscreenp (lisp_face->screen)) &&
		    !NILP (Fscreen_name (lisp_face->screen)))
		   ? Fscreen_name (lisp_face->screen)
		   : lisp_face->screen),
		  printcharfun, 0);

  {
    char buf[100];
    sprintf (buf, " %d>", lisp_face->face_id);
    write_string_1 (buf, -1, printcharfun);
  }
}

/* Faces are equal if all of their display attributes are equal.
   We don't compare names or screens, because that would make equal be eq.
   This isn't concerned with "unspecified" attributes, that's what
   #'face-differs-from-default-p is for.
 */
static int
face_equal (Lisp_Object o1, Lisp_Object o2, int depth)
{
  struct face *f1 = Lisp_Face_to_face (o1, Qnil);
  struct face *f2 = Lisp_Face_to_face (o2, Qnil);
  depth++;
  return (f1->underline == f2->underline &&
	  f1->hilited == f2->hilited &&
	  internal_equal (f1->font, f2->font, depth) &&
	  internal_equal (f1->foreground, f2->foreground, depth) &&
	  internal_equal (f1->background, f2->background, depth) &&
	  internal_equal (f1->back_pixmap, f2->back_pixmap, depth));
}

static Lisp_Object Vnonscreen_faces;
static int face_id_tick;

DEFUN ("facep", Ffacep, Sfacep, 1, 1, 0, "Whether OBJECT is a FACE.")
     (object)
     Lisp_Object object;
{
  return (FACEP (object) ? Qt : Qnil);
}

DEFUN ("find-face", Ffind_face, Sfind_face, 1, 2, 0,
       "Retrieve the face of the given name.\n\
If NAME is a symbol and SCREEN is provided, the face is looked up on\n\
that screen; otherwise, the selected screen is used.\n\
If there is no such face, returns nil.\n\
If SCREEN is the symbol t, then the global, non-screen face is returned.\n\
If NAME is already a face, it is simply returned.")
     (name, screen)
     Lisp_Object name, screen;
{
  /* (find-face #<face@screen1> #<screen2>)  ==>  #<face@screen1>
     (find-face #<face@screen1> nil)	     ==>  #<face@screen1>
     (find-face #<face@screen1> t)           ==>  #<face@t>
     Remember:
     (face-font (get-face 'default t)) == (face-font 'default t)
     (face-font (get-face 'default t)) != (face-font 'default nil)
     (get-face #<face@screen1> t)          == (get-face 'face t)
     (get-face #<face@screen1> #<screen2>) != (get-face 'face #<screen2>)
   */
  if (EQ (screen, Qt) && FACEP (name))
    name = XFACE (name)->name;

  if (NILP (screen))
    screen = Fselected_screen ();

  if (!EQ (screen, Qt))
    CHECK_SCREEN (screen, 0);

  if (SYMBOLP (name))
    {
      Lisp_Object rest;
      for (rest = (EQ (screen, Qt)
		   ? Vnonscreen_faces
		   : XSCREEN (screen)->face_data);
	   !NILP (rest);
	   rest = Fcdr (rest))
	{
	  Lisp_Object face = XCONS (rest)->car;
	  if (!FACEP (face))
	    abort ();
	  if (!EQ (XFACE (face)->screen, screen))
	    abort ();
	  if (EQ (name, XFACE (face)->name))
	    return face;
	}
      return Qnil;
    }
  else
    {
      CHECK_FACE (name, 0);
      return name;
    }
}

DEFUN ("get-face", Fget_face, Sget_face, 1, 2, 0,
       "Retrieve the face of the given name.\n\
If NAME is a symbol and SCREEN is provided, the face is looked up on\n\
that screen; otherwise, the selected screen is used.\n\
If there is no such face, an error is signalled.  See also `find-face'.\n\
If SCREEN is the symbol t, then the global, non-screen face is returned.\n\
If NAME is already a face, it is simply returned.")
     (name, screen)
     Lisp_Object name, screen;
{
  Lisp_Object face = Ffind_face (name, screen);
  if (NILP (face))
    CHECK_FACE (name, 0); /* use `name' in error message */
  return face;
}


DEFUN ("face-name", Fface_name, Sface_name, 1, 1, 0,
       "Returns the name of the given face.")
     (face)
     Lisp_Object face;
{
  return (XFACE (Fget_face (face, Qt))->name);
}

DEFUN ("face-screen", Fface_screen, Sface_screen, 1, 1, 0,
    "Returns the screen of the given face, or t if this is a `default' face.")
     (face)
     Lisp_Object face;
{
  return (XFACE (Fget_face (face, Qt))->screen);
}

static struct face *
Lisp_Face_to_face (Lisp_Object lisp_face, Lisp_Object screen)
{
  int id;
  lisp_face = Fget_face (lisp_face, screen);
  id  = XFACE (lisp_face)->face_id;
  screen = XFACE (lisp_face)->screen;
  if (NILP (screen)) abort ();   /* should be a face or t */
  if (! XFACE (lisp_face)->face) abort ();
  return (XFACE (lisp_face)->face);
/*  if (EQ (screen, Qt)) 
   return (get_screen_face (0, id));
  else
    return (get_screen_face (XSCREEN (screen), id)); */
}

DEFUN ("face-font", Fface_font, Sface_font, 1, 2, 0,
       "Returns the font of the given face, or nil if it is unspecified.")
     (face, screen)
     Lisp_Object face, screen;
{
  return (Lisp_Face_to_face (face, screen)->font);
}

DEFUN ("face-foreground", Fface_foreground, Sface_foreground, 1, 2, 0,
      "Returns the foreground of the given face, or nil if it is unspecified.")
     (face, screen)
     Lisp_Object face, screen;
{
  return (Lisp_Face_to_face (face, screen)->foreground);
}

DEFUN ("face-background", Fface_background, Sface_background, 1, 2, 0,
      "Returns the background of the given face, or nil if it is unspecified.")
     (face, screen)
     Lisp_Object face, screen;
{
  return (Lisp_Face_to_face (face, screen)->background);
}

DEFUN ("face-background-pixmap", Fface_background_pixmap,
       Sface_background_pixmap, 1, 2, 0,
       "Returns the background pixmap of the given face, or nil if it is unspecified.")
     (face, screen)
     Lisp_Object face, screen;
{
  return (Lisp_Face_to_face (face, screen)->back_pixmap);
}

DEFUN ("face-underline-p", Fface_underline_p, Sface_underline_p, 1, 2, 0,
       "Returns whether the given face is underlined.")
     (face, screen)
     Lisp_Object face, screen;
{
  return ((Lisp_Face_to_face (face, screen)->underline) ? Qt :Qnil);
}


DEFUN ("list-faces", Flist_faces, Slist_faces, 0, 0, 0,
       "Returns a list of the names of all of the defined faces.")
     ()
{
  Lisp_Object list = Fcopy_sequence (Vnonscreen_faces);
  Lisp_Object rest;
  for (rest = list; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      Lisp_Object face = XCONS (rest)->car;
      if (!FACEP (face)) abort ();
      XCONS (rest)->car = XFACE (face)->name;
      if (!SYMBOLP (XCONS (rest)->car)) abort ();
    }
  return list;
}


DEFUN ("extent-face", Fextent_face, Sextent_face, 1, 1, 0,
       "Returns the name of the face in which EXTENT is displayed, or nil\n\
if the extent's face is unspecified.")
     (extent)
     Lisp_Object extent;
{
  int id;
  Lisp_Object rest;
  CHECK_EXTENT (extent, 0);
  id = extent_face_id (XEXTENT (extent));
  if (id == -1)
    return Qnil;
  for (rest = Vnonscreen_faces; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      Lisp_Object face = XCONS (rest)->car;
      if (id == XFACE (face)->face_id)
	return (XFACE (face)->name);
    }
  abort ();
}


DEFUN ("set-extent-face", Fset_extent_face, Sset_extent_face, 2, 2, 0,
       "Make the given EXTENT have the graphic attributes specified by FACE.")
     (extent, face)
     Lisp_Object extent, face;
{
  int face_id;
  EXTENT e;
  CHECK_EXTENT (extent, 0);
  e = XEXTENT (extent);
  if (!NILP (face))
    face_id = XFACE (Fget_face (face, Qt))->face_id;
  else
    face_id = -1;

  extent_face_id (e) = face_id;

  if (BUFFERP (extent_buffer (e)))
    {
      BUF_FACECHANGE (XBUFFER (extent_buffer (e)))++;
      extents_changed++;
    }

  return face;
}


DEFUN ("make-face", Fmake_face, Smake_face, 1, 1, 0,
       "Defines and returns a new FACE on all screens.\n\
You can modify the font, color, etc of a face with the set-face- functions.\n\
If the face already exists, it is unmodified.")
     (name)
     Lisp_Object name;
{
  struct Lisp_Face *f;
  Lisp_Object face;
  Lisp_Object rest;
  int id;
  CHECK_SYMBOL (name, 0);
  face = Ffind_face (name, Qnil);
  if (!NILP (face))
    return face;

  id = face_id_tick;

  /* First make the face on each of the existing screens. */
  for (rest = Vscreen_list; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      Lisp_Object screen = XCONS (rest)->car;
      struct screen *s = XSCREEN (screen);
      f = alloc_lcrecord (sizeof (struct Lisp_Face), lrecord_face);
      f->name = name;
      f->screen = screen;
      f->face_id = id;
      ensure_face_ready (s, id);
      f->face = get_screen_face (XSCREEN (screen), id);
      XSETR (face, Lisp_Face, f);
      s->face_data = nconc2 (s->face_data, Fcons (face, Qnil));
      if (! s->faces [id])
	abort ();
    }

  /* Then make the default, nonscreen version of the face. */
  f = alloc_lcrecord (sizeof (struct Lisp_Face), lrecord_face);
  f->name = name;
  f->screen = Qt;
  f->face_id = id;
  ensure_face_ready (0, id);
  f->face = get_screen_face (0, id);
  XSETR (face, Lisp_Face, f);
  Vnonscreen_faces = nconc2 (Vnonscreen_faces, Fcons (face, Qnil));

#ifdef HAVE_X_WINDOWS
  /* Set the resources on the faces, in case screens already exist...
     This could stand to be cleaner I guess. */
  if (EQ (Vwindow_system, Qx))
    call1 (Qx_resource_face, name);
#endif /* HAVE_X_WINDOWS */

  face_id_tick++;
  return name;
}

static void
init_screen_face_data (struct screen *s)
{
  Lisp_Object screen;
  Lisp_Object tail = Qnil;
  Lisp_Object rest;
  XSETR (screen, Lisp_Screen, s);
  for (rest = Vnonscreen_faces; !NILP (rest); rest = XCONS (rest)->cdr)
    {
      Lisp_Object face;
      struct Lisp_Face *old_lisp, *new_lisp;
      struct face *old, *new;
      old_lisp = XFACE (XCONS (rest)->car);
      new_lisp = alloc_lcrecord (sizeof (struct Lisp_Face), lrecord_face);
      XSETR (face, Lisp_Face, new_lisp);

      /* Copy the slots of `struct Lisp_Face' from the default... */
      new_lisp->name = old_lisp->name;
      new_lisp->face_id = old_lisp->face_id;
      new_lisp->screen = screen;

      /* Then copy the slots of `struct face' from the default... */
      ensure_face_ready (s, old_lisp->face_id);
      old = get_screen_face (0, new_lisp->face_id);
      new = get_screen_face (s, new_lisp->face_id);
      new_lisp->face = new;

      /* Doing `*new = *old' is no good because of widget SetValues hooks.
	 Maybe we really ought to just move copy-face from lisp to this file.
       */
# define FROB(wa,nk,er) \
      if (er || !NILP (nk)) Fset_face_attribute_internal (face, wa, nk)
      FROB (Qfont, old->font, 0);
      FROB (Qforeground, old->foreground, 0);
      FROB (Qbackground, old->background, 0);
      FROB (Qbackground_pixmap, old->back_pixmap, 1);
      FROB (Qunderline, (old->underline ? Qt : Qnil), 1);
# undef FROB

      /* Finally, install the face in the screen's list. */
      if (NILP (tail))
	{
	  s->face_data = Fcons (face, Qnil);
	  tail = s->face_data;
	}
      else
	{
	  XCONS (tail)->cdr = Fcons (face, Qnil);
	  tail = XCONS (tail)->cdr;
	}
    }
}


/* Caching functions for faces */


#if 0	/* this code is for when `face' and `Lisp_Face' are the same */

static Lisp_Object Vface_cache;

/* This is a hash function used in the lisp weak hash table Vface_cache.
   It only works on face objects (that's all that goes in that table.)
 */
static unsigned long
face_hash_function (CONST void *arg)
{
  Lisp_Object lface;
  struct face *face;
  CVOID_TO_LISP (lface, arg);
  if (!FACEP (lface))
    abort ();
  face = XFACE (lface);
#define slot_unless_nil(extractor, slot1, slot2) \
  (NILP(face->slot1) ? 0 : ((unsigned long)(extractor(face->slot1)->slot2)))
  return (slot_unless_nil (XFONT, font, font) ^
	  slot_unless_nil (XPIXEL, foreground, color.pixel) ^
	  slot_unless_nil (XPIXEL, background, color.pixel) ^
	  slot_unless_nil (XPIXMAP, back_pixmap, pixmap) ^
	  (unsigned long) face->underline);
#undef slot_unless_nil
}

static int
face_eql (CONST void *arg1, CONST void *arg2)
{
  if (arg1 == arg2) 
    return 1;
  else
    {
      Lisp_Object f1, d2;
      CONST struct face *face1, face2;
      CVOID_TO_LISP (f1, arg1);
      CVOID_TO_LISP (f2, arg2);
      if (!FACEP (f1)) abort ();	/* #### debugging */
      if (!FACEP (f2)) abort ();
      face1 = XFACE (f1);
      face2 = XFACE (f2);

      /* #### Might get better caching if this compared the underlying
	 X objects directly instead of comparing the lisp objects. */
      if (EQ (face1->font, face2->font) &&
          EQ (face1->foreground, face2->foreground) &&
          EQ (face1->background, face2->background) &&
          EQ (face1->back_pixmap, face2->back_pixmap) &&
          (face1->underline == face2->underline))
        return 1;
      else
        return 0;
    }
}


static Lisp_Object *Vface_cache_tmp;

static void 
init_face_cache ()
{
  struct face *f;
  Vface_cache = make_weak_hashtable (50, face_hash_function, face_eql);
  f = (struct face *) xmalloc (sizeof (struct face));
  XSETFACE (Vface_cache_tmp, f);
}


static struct face *
get_cached_face (struct face* face)
{
  /* Copy the given face* into the face* which underlies our dummy face
     lisp object (since we need to pass a lisp object to gethash.) */
  struct face *tmp = XFACE (Vface_cache_tmp);
  *tmp = *face;
  return Fgethash (Vface_cache_tmp, Vface_cache, Qnil);
}

static void
cache_face (struct face *face)
{
  puthash (face, face, face_cache);
}


static void
invalidate_face_cache (struct screen *s)
{
  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    SET_SCREEN_GARBAGED (XSCREEN (XCONS (tail)->car));
}


#else /* !0 */


static c_hashtable face_cache;
static c_hashtable face_cache_pending_flush;

static int face_cache_inited;
int face_cache_invalid;

static void hash_faces_in_windows (Lisp_Object window, c_hashtable face_hash);
static void hash_faces_in_window (Lisp_Object window, c_hashtable face_hash);


static struct face *allocate_face ();

static unsigned long
face_hash_function (CONST void *arg)
{
  CONST struct face* face = arg;
#define slot_unless_nil(extractor, slot1, slot2) \
  (NILP(face->slot1) ? 0 : ((unsigned long)(extractor(face->slot1)->slot2)))
  return (slot_unless_nil (XFONT, font, font) ^
	  slot_unless_nil (XPIXEL, foreground, color.pixel) ^
	  slot_unless_nil (XPIXEL, background, color.pixel) ^
	  slot_unless_nil (XPIXMAP, back_pixmap, pixmap) ^
	  (unsigned long) face->underline);
#undef slot_unless_nil
}

static int
face_eql (CONST void *arg1, CONST void *arg2)
{
  if (arg1 == arg2) 
    return 1;
  else
    {
      CONST struct face* face1 = arg1;
      CONST struct face* face2 = arg2;

      /* #### Might get better caching if this compared the underlying
	 X objects directly instead of comparing the lisp objects. */
      if (EQ (face1->font, face2->font) &&
          EQ (face1->foreground, face2->foreground) &&
          EQ (face1->background, face2->background) &&
          EQ (face1->back_pixmap, face2->back_pixmap) &&
          (face1->underline == face2->underline))
        return 1;
      else
        return 0;
    }
}

static void 
init_face_cache ()
{
  face_cache_inited = 1;
  face_cache = make_general_hashtable (MAX_CACHED_FACES,
				       face_hash_function, face_eql);
}

static struct face*
get_cached_face (struct face* face)
{
  struct face *result;

  if (!face_cache_inited)
    init_face_cache ();

  if (gethash (face, face_cache, (CONST void **) &result))
    return result;
  else
    return 0;
}

static void
cache_face (struct face *face)
{
  if (!face_cache_inited)
    init_face_cache ();

  puthash (face, face, face_cache);
}


/* flush_face_cache is called from redisplay when face_cache_invalid is true.
   we set face_cache_invalid when we change a font or color of some face,
   meaning that the GCs need to be recomputed.  We don't just flush the
   cache at the time that we change a face because the redisplay structures
   still have pointers to the faces we want to free.
 */

static void
flush_face_cache_mapper (CONST void *hash_key, void *hash_contents,
                         void *closure)
{
  struct face *face = hash_contents;
  xfree (face);
}

void
flush_face_cache ()
{
  Display *dpy = x_current_display;
  if (! face_cache_inited)
    return;
  if (! face_cache_pending_flush)
    return;

  BLOCK_INPUT;
  maphash (flush_face_cache_mapper,
	   face_cache_pending_flush,
	   dpy);
  UNBLOCK_INPUT;
  free_hashtable (face_cache_pending_flush);
  face_cache_pending_flush = 0;
  face_cache_invalid = 0;
}

static void
invalidate_face_cache (struct screen *s)
{
  Lisp_Object tail;
  face_cache_invalid = 1;
  extent_cache_invalid = 1;

  for (tail = Vscreen_list; CONSP (tail); tail = XCONS (tail)->cdr)
    SET_SCREEN_GARBAGED (XSCREEN (XCONS (tail)->car));

  if (face_cache_pending_flush)
    return;
  face_cache_pending_flush = face_cache;
  init_face_cache ();
}

#endif /* !0 */


/* Utility functions for faces */

#ifndef MAX
#define MAX(x,y) (((x)>(y))?(x):(y))
#endif

static void
reset_face (struct screen* s, struct face* face)
{
  struct face* normal_face = &SCREEN_NORMAL_FACE (s);
  memset (face, 0, sizeof (struct face));
  face->foreground = normal_face->foreground;
  face->background = normal_face->background;
  face->back_pixmap = normal_face->back_pixmap;
  face->underline = normal_face->underline;
  face->font = normal_face->font;
}

static void 
merge_faces (struct face* from, struct face* to)
{
  if (!NILP (from->font))
    to->font = from->font;
  if (!NILP (from->foreground))
    to->foreground = from->foreground;
  if (!NILP (from->background))
    to->background = from->background;
  if (!NILP (from->back_pixmap))
    to->back_pixmap = from->back_pixmap;
  /* #### It's not possible for a non-underlined face to override an
     underlined one.  That is, one can't specify *no* underlining.
     Is that reasonable? */
  if (from->underline)
    to->underline = from->underline;
}


static void
merge_extent_face (struct screen* s, EXTENT extent, struct face* face)
{
  int index = extent_face_id (extent);
  
  /* no attribute: do not change anything */
  if (index <= 0)
    return;
    ensure_face_ready (s, index);

  /* do the merge */
  merge_faces (s->faces [index], face);
}



/* Get a face suitable for display (ie which has a GC) for a given
   set of attributes. */

static struct face *
get_display_face (struct screen* s, struct face* face)
{
  struct face *result;

  /* cache the default face */
  if (EQ (face->font, SCREEN_NORMAL_FACE (s).font)
      && EQ (face->foreground, SCREEN_NORMAL_FACE (s).foreground)
      && EQ (face->background, SCREEN_NORMAL_FACE (s).background)
      && EQ (face->back_pixmap, SCREEN_NORMAL_FACE (s).back_pixmap)
      && face->underline == SCREEN_NORMAL_FACE (s).underline)
    return &SCREEN_NORMAL_FACE (s);

  /* cache the modeline face */
  if (EQ (face->font, SCREEN_MODELINE_FACE (s).font)
      && EQ (face->foreground, SCREEN_MODELINE_FACE (s).foreground)
      && EQ (face->background, SCREEN_MODELINE_FACE (s).background)
      && EQ (face->back_pixmap, SCREEN_MODELINE_FACE (s).back_pixmap)
      && face->underline == SCREEN_MODELINE_FACE (s).underline)
    return &SCREEN_MODELINE_FACE (s);

  /* Is it cached */
  result = get_cached_face (face);
  if (result)
    return result;

  /* Add one to the cache */
  result = allocate_face ();

  result->font = face->font;
  result->foreground = face->foreground;
  result->background = face->background;
  result->back_pixmap = face->back_pixmap;
  result->underline = face->underline;

  cache_face (result);

  return result;
}


/* External entry points */

extern Lisp_Object Vlast_highlighted_extent;

/* Computes the face associated with an overlapping set of extents */
void 
setup_extent_fragment_face_ptr (struct screen *s, EXTENT_FRAGMENT extfrag,
				int include_zero_width)
{
  struct face face;
  EXTENT *vec = extfrag->extents_stack;
  int len = extfrag->number_of_extents;
  int i;
  
  /* optimize the default case. */
  if (len == 0)
    extfrag->fp = &SCREEN_NORMAL_FACE (s);
  else
    {
      EXTENT ebuf [200];
      EXTENT *extents;
      struct extent dummy_extent;

      /* Make a copy of the vector of extents... */
      if (len < (sizeof(ebuf) / (sizeof(EXTENT)) - 1))
	extents = ebuf; /* use a static buffer if it's small, for speed */
      else
	extents = (EXTENT *) alloca ((len * sizeof(EXTENT)) + 1);
      memcpy (extents, vec, len * sizeof(EXTENT));

      /* determine whether the last-highlighted-extent is present... */
      if (EXTENTP (Vlast_highlighted_extent))
	{
	  EXTENT lhe = XEXTENT (Vlast_highlighted_extent);
	  int lhe_index = -1;
	  for (i = 0; i < len; i++)
	    if (extents [i] == lhe)
	      {
		lhe_index = i;
		break;
	      }
	  /* ...and if it is, make up a dummy extent of the appropriate
	     priority, and add it to the list to be sorted with the rest.
	   */
	  if (lhe_index != -1)
	    {
	      /* memset isn't really necessary; we only deref `priority' */
	      memset (&dummy_extent, 0, sizeof (dummy_extent));
	      extent_priority (&dummy_extent) = mouse_highlight_priority;
	      /* put the dummy extent just after the lhe in the stack,
		 as they're already sorted by size/starting point. */
	      for (i = len; i > lhe_index; i--)
		extents [i] = extents [i-1];
	      extents [lhe_index + 1] = &dummy_extent;
	      len++;
	    }
	}

      /* sort our copy of the stack by extent_priority (extent)... */
      for (i = 1; i < len; i++)
	{
	  int j = i - 1;
	  while (j >= 0 &&
		 (extent_priority (extents[j]) >
		  extent_priority (extents[j+1])))
	    {
	      EXTENT tmp = extents [j];
	      extents [j] = extents [j+1];
	      extents [j+1] = tmp;
	      j--;
	    }
	}

      /* Now merge the faces of the extents together in order.

	 Remember that one of the extents in the list might be our dummy
	 extent representing the highlighting that is attached to some other
	 extent that is currently mouse-highlighted.  When an extent is
	 mouse-highlighted, it is as if there are two extents there, of
	 potentially different priorities: the extent being highlighted, with
	 whatever face and priority it has; and an ephemeral extent in the
	 `highlight' face with `mouse-highlight-priority'.
       */
      reset_face (s, &face);
      for (i = 0; i < len; i++)
	{
	  EXTENT current = extents [i];
	  if (current == &dummy_extent)
	    /* this isn't a real extent; use the highlight face. */
	    merge_faces (&SCREEN_HIGHLIGHT_FACE (s), &face);
	  else if ((extent_end (current) != extent_start (current))
		    || include_zero_width)
	    /* Skip 0-length extents from the merge (is this necessary?) */
	    merge_extent_face (s, current, &face);
	}

      extfrag->fp = get_display_face (s, &face);
    }
}


/* Allocate a new face */
static struct face *
allocate_face ()
{
  struct face* result = (struct face*)xmalloc (sizeof (struct face));
  result->font = Qnil;
  result->foreground = Qnil;
  result->background = Qnil;
  result->back_pixmap = Qnil;
  result->underline = 0;
  result->hilited = 0;		/* #### only used by dumb-tty code now... */
  return result;
}


static int n_nonscreen_faces;
static struct face **nonscreen_faces;

void
ensure_face_ready (struct screen* s, int f)
{
  int n_faces;
  struct face **faces;

  if (s)
    {
      n_faces = s->n_faces;
      faces = s->faces;
    }
  else
    {
      n_faces = n_nonscreen_faces;
      faces = nonscreen_faces;
    }

  if (n_faces <= f)
    {
      int n = f + 10;
      int i;
      int start;
      if (n_faces == 0)
	{
	  faces = (struct face**) xmalloc (sizeof (struct face *) * n);
	  start = 0;
	}
      else
	{
	  faces = (struct face**) xrealloc (faces, sizeof (struct face *) * n);
	  start = n_faces;
	}

      n_faces = n;

      for (i = start; i < n; i++)
	faces [i] = allocate_face ();
    }

  if (s)
    {
      s->n_faces = n_faces;
      s->faces = faces;
    }
  else
    {
      n_nonscreen_faces = n_faces;
      nonscreen_faces = faces;
    }
}

static struct face *
get_screen_face (struct screen *s, int id)
{
  struct face *face;
  ensure_face_ready (s, id);
  if (s)
    face = s->faces [id];
  else
    face = nonscreen_faces [id];
  if (! face) abort ();
  return face;
}


/* screens */

extern Lisp_Object Vdebug_on_error;

void
init_screen_faces (struct screen *s)
{
  struct screen *other_screen = 0;
  Lisp_Object rest;

  for (rest = Vscreen_list; !NILP (rest); rest = Fcdr (rest))
    {
      struct screen *s2 = XSCREEN (Fcar (rest));
      if (s2 != s && SCREEN_IS_X (s2))
	{
	  other_screen = s2;
	  break;
	}
    }

  if (other_screen)
    /* make sure this screen's face vector is as big as the others */
    ensure_face_ready (s, other_screen->n_faces);

    {
      struct screen *oss = selected_screen;
      Lisp_Object odoe = Vdebug_on_error;
      int speccount = specpdl_depth ();

      if (purify_flag)
	return;

      init_screen_face_data (s);

      /* There's no reason to bother doing specbinds here, because if
	 make-initial-faces signals an error, emacs is going to crash
	 immediately.
       */
      gc_currently_forbidden = 1;
      Vinhibit_quit = Qt;
      /* startup.el plays games with debug-on-error, which breaks the
	 condition-case that `try-font' does from `make-screen-initial-faces'.
	 Oh for a real condition system!!) */
      Vdebug_on_error = Qnil;

      selected_screen = s;
      /* Might not really be necessary to change selected screen here, but
	 it's a good idea for the benefit of random code that might be taking
	 it as an implicit arg, like things run from `xpm-color-symbols'.
       */
      call1 (Qmake_screen_initial_faces, Fselected_screen ());
      selected_screen = oss;
      Vinhibit_quit = Qnil;
      Vdebug_on_error = odoe;
      gc_currently_forbidden = 0;

      if (SCREEN_IS_X (s))
	if (NILP (SCREEN_NORMAL_FACE (s).font))
	  fatal(GETTEXT ("Unable to load any useable ISO8859-1 font; check X resources."));
    }
}


void
free_screen_faces (struct screen *s)	/* called from Fdelete_screen() */
{
  int i;

  /* This may be able to free some GCs, but unfortunately it will cause
     all screens to redisplay. */
  invalidate_face_cache (s);

  for (i = 0; i < s->n_faces; i++)
    {
      struct face *face = s->faces [i];
      if (! face)
        continue;
      xfree (face);
    }
  xfree (s->faces);
  s->faces = 0;
  s->n_faces = 0;
  /* We must set the face_data to nil so that GC doesn't try to dereference
     (and mark) the objects in s->faces, which s->face_data also points at.
     Eventually this function will go away, and setting the face_data to
     nil will be all that we need to do (once face and Lisp_Face are
     combined.)
   */
  s->face_data = Qnil;
}


static void
hash_faces_in_windows (Lisp_Object window, c_hashtable face_hash)
{
  for (; !NILP (window); window = XWINDOW(window)->next)
    hash_faces_in_window (window, face_hash);
}

static void
hash_faces_in_window (Lisp_Object window, c_hashtable face_hash)
{
  struct window *w = XWINDOW(window);
  struct char_block *cb;
  struct line_header *l;

  if (!NILP (w->vchild))
    {
      hash_faces_in_windows (w->vchild, face_hash);
      return;
    }
  if (!NILP (w->hchild))
    {
      hash_faces_in_windows (w->hchild, face_hash);
      return;
    }

  if (window_display_lines (w))
    {
      l = window_display_lines (w);
      while (l)
	{
	  cb = l->body;
	  while (cb != l->end)
	    {
	      puthash (cb->face, cb->face, face_hash);
	      cb = cb->next;
	    }
	  l = l->next;
	}
    }
  if (window_display_modeline (w))
    {
      cb = window_display_modeline (w)->body;
      while (cb != window_display_modeline(w)->end)
	{
	  puthash (cb->face, cb->face, face_hash);
	  cb = cb->next;
	}
    }
}

/*
 * Apply a function to each face used in window.  The function has to
 * be suitable for passing to maphash.  The first two args will both
 * end up being struct face *, the third void *.
 */
void
map_redisplay_faces_in_window (Lisp_Object window,
			       void (*map_function)
			       (CONST void *, void *, void*),
			       void *arg)
{
  c_hashtable face_hash;

  face_hash = make_general_hashtable (MAX_CACHED_FACES,
				      face_hash_function, face_eql);

  hash_faces_in_windows (window, face_hash);
  maphash (map_function, face_hash, arg);

  free_hashtable (face_hash);
}

/*
 * Apply a function to each face used in screen.  The function has to
 * be suitable for passing to maphash.  The first two args will both
 * end up being struct face *, the third void *
 */
void
map_redisplay_faces_in_screen (struct screen *s,
			       void (*map_function)
			       (CONST void *, void *, void*),
			       void *arg)
{
  map_redisplay_faces_in_window (s->root_window, map_function, arg);
}

/*
 * Apply a function to each face used on display.  The function has to
 * be suitable for passing to maphash.  The first two args will both
 * end up being struct face *, the third void *.  This function assumes
 * the display argument for now since multi-display's are not yet
 * supported
 */
void
map_redisplay_faces_on_display (void (*map_function)
				(CONST void *, void *, void*),
				void *arg)
{
  c_hashtable face_hash;
  Lisp_Object cur_s, rest;

  face_hash = make_general_hashtable (MAX_CACHED_FACES,
				      face_hash_function, face_eql);

  for (rest = Vscreen_list ; !NILP (rest) ; rest = XCONS(rest)->cdr)
    {
      cur_s = XCONS(rest)->car;
      if (!SCREENP (cur_s))
	abort();

      hash_faces_in_windows (XSCREEN(cur_s)->root_window, face_hash);
    }

  maphash (map_function, face_hash, arg);

  free_hashtable (face_hash);
}


/* Lisp interface */


#ifdef HAVE_X_WINDOWS
static void
update_EmacsScreen (struct screen *s, CONST char *name, void *val)
{
  Arg av[10];
  int ac = 0;
  if (! SCREEN_IS_X (s))
    return;
  XtSetArg (av[ac], (char *) name, val); ac++;
  BLOCK_INPUT;
  XtSetValues (s->display.x->edit_widget, av, ac);
  UNBLOCK_INPUT;
  /* The intent of this code is to cause the screen size in characters
     to remain the same when the font changes, at the expense of changing
     the screen size in pixels.  It's not totally clear that this is the
     right thing to do, but it's not clearly wrong either.
   */
  if (!strcmp (name, XtNfont))
    {
      Lisp_Object screen;
      EmacsScreenRecomputeCellSize (s->display.x->edit_widget);
      XSETR (screen, Lisp_Screen, s);
      Fset_screen_size (screen,
			make_number (s->width), make_number (s->height),
			Qnil);
    }
}
#endif

static void
ensure_not_builtin_face (int id, Lisp_Object attr_name)
{
#ifdef HAVE_X_WINDOWS
  if ((id == 0 || id == 1 || id == 3 || id == 4) &&
      (EQ (attr_name, Qforeground) ||
       EQ (attr_name, Qbackground) ||
       EQ (attr_name, Qfont)))
    {
      char buf [255];
      sprintf (buf, GETTEXT ("can't set %s of `%s' face to nil"),
	       (char *) XSYMBOL (attr_name)->name->data,
	       (id == 0 ? GETTEXT ("default") :
		id == 3 ? GETTEXT ("left-margin") :
		id == 1 ? GETTEXT ("modeline") :
		id == 4 ? GETTEXT ("right-margin") : GETTEXT ("???")));
      error (buf);
    }
#endif /* HAVE_X_WINDOWS */
}

DEFUN ("set-face-attribute-internal", Fset_face_attribute_internal,
       Sset_face_attribute_internal, 3, 3, 0, "")
     (face_obj, attr_name, attr_value)
     Lisp_Object face_obj, attr_name, attr_value;
{
  struct face *face;
  struct screen *s;
  Lisp_Object screen;
  int id;
  CHECK_FACE (face_obj, 0);
  CHECK_SYMBOL (attr_name, 0);

  id = XFACE (face_obj)->face_id;
  screen = XFACE (face_obj)->screen;

  if (EQ (screen, Qt))
    {
      s = 0;
    }
  else
    {
      CHECK_SCREEN (screen, 0);
      s = XSCREEN (screen);
    }
  face = get_screen_face (s, id);

  if (NILP (attr_value)) ensure_not_builtin_face (id, attr_name);

  if (EQ (attr_name, Qfont))
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (attr_value)) CHECK_FONT (attr_value, 0);
      face->font = attr_value;
      if (s) invalidate_face_cache (s);
      if (s && id == 0)
	update_EmacsScreen (s, XtNfont, XFONT (FACE_FONT (face))->font);
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, Qforeground))
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (attr_value)) CHECK_PIXEL (attr_value, 0);
      face->foreground = attr_value;
      if (s) invalidate_face_cache (s);
      if (s && id == 0)
	update_EmacsScreen (s, XtNforeground, (void *) FACE_FG_PIXEL (face));
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, Qbackground))
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (attr_value)) CHECK_PIXEL (attr_value, 0);
      face->background = attr_value;
      if (s) invalidate_face_cache (s);
      if (s && id == 0)
	update_EmacsScreen (s, XtNbackground, (void *) FACE_BG_PIXEL (face));
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, Qbackground_pixmap))
    {
#ifdef HAVE_X_WINDOWS
      if (!NILP (attr_value)) CHECK_PIXMAP (attr_value, 0);
      face->back_pixmap = attr_value;
      if (s) invalidate_face_cache (s);
#endif /* HAVE_X_WINDOWS */
    }
  else if (EQ (attr_name, Qunderline))
    {
      int new = !NILP (attr_value);
      if (s && face->underline != new)
	invalidate_face_cache (s);
      face->underline = new;
    }
  else
    signal_error (Qerror, list2 (build_string
				 (GETTEXT ("unknown face attribute")),
                                 attr_name));
  return Qnil;
}



void
syms_of_faces ()
{
  face_cache_invalid = 0;
  face_cache_pending_flush = 0;

  defsubr (&Sfacep);
  defsubr (&Sfind_face);
  defsubr (&Sget_face);
  defsubr (&Sface_name);
  defsubr (&Sface_screen);
  defsubr (&Sface_font);
  defsubr (&Sface_foreground);
  defsubr (&Sface_background);
  defsubr (&Sface_background_pixmap);
  defsubr (&Sface_underline_p);
  defsubr (&Slist_faces);
  defsubr (&Sextent_face);
  defsubr (&Sset_extent_face);
  defsubr (&Smake_face);
  defsubr (&Sset_face_attribute_internal);

  face_id_tick = 0;
  n_nonscreen_faces = 0;
  nonscreen_faces = 0;
  Vnonscreen_faces = Qnil;
  staticpro (&Vnonscreen_faces);

  /* extent-property and set-extent-property use these. */
  defsymbol (&Qface, "face");
  defsymbol (&Qfacep, "facep");
  defsymbol (&Qfont, "font");
  defsymbol (&Qforeground, "foreground");
  defsymbol (&Qbackground, "background");
  defsymbol (&Qunderline, "underline");
  defsymbol (&Qbackground_pixmap, "background-pixmap");
  defsymbol (&Qmake_screen_initial_faces, "make-screen-initial-faces");

#ifdef HAVE_X_WINDOWS
  defsymbol (&Qx_resource_face, "x-resource-face");
#endif

  DEFVAR_INT ("mouse-highlight-priority", &mouse_highlight_priority,
"The priority to use for the mouse-highlighting pseudo-extent\n\
that is used to highlight extents with the `highlight' attribute set.\n\
See `set-extent-priority'.");
  mouse_highlight_priority = 10;
}
