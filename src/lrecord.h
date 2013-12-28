/* The "lrecord" structure (header of a compound lisp object.)
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#ifndef _EMACS_LRECORD_H_
#define _EMACS_LRECORD_H_

struct lrecord_header
  {
    /* It would be better to put a the mark-bit together with the
     * following datatype identification field in an 8- or 16-bit integer
     *  rather than playing funny games with changing header->implementation
     *  and "wasting" 32 bits on the below pointer.
     *  The type-id would then be a 7 or 15
     *  bit index into a table of lrecord-implementations rather than a
     *  direct pointer.)  There would be 8 (or 16) bits left over for
     *  datatype-specific per-instance flags
     * The below is the simplest thing to do for the present,
     *  and doesn't incur that much overhead as most Emacs records
     *  are of such a size that the overhead isn't too bad.
     *  (The marker datatype is the worst case.)
     *  It also has the very very very slight advantage that type-checking
     *  involves one memory read (of the "implementation" slot) and a
     *  comparison against a link-time constant address rather than a
     *  read and a comparison against a variable value. (Variable since
     *  it is a very good idea to assign the indices into the hypothetical
     *  type-code table dynamically rather that pre-defining them.)
     *  I think I remember that Elk Lisp does something like this.
     *  Gee, I wonder if some cretin has patented it?
     */
    CONST struct lrecord_implementation *implementation;
  };

struct lcrecord_header
  {
    struct lrecord_header lheader;
    /* The "next" field is normally used to chain all lrecords together
     *  so that the GC can find (and free) all of them.
     *  "alloc_lcrecord" threads records together.
     * The "next" field may be used for other purposes as long as some
     *  other mechanism is provided for letting the GC do its work.
     *  (For example, the event and marker datatypes allocates members out
     *  of memory chunks, and it are able to find all unmarked
     *  events by sweeping through the elements of the list of chunks)
     */
    struct lcrecord_header *next;
    /* This is just for debugging/printing convenience.
       Having this slot doesn't hurt us much spacewise, since an lcrecord
       already has the above slots together with malloc overhead. */
    int uid;
  };

/* This as the value of lheader->implementation->finaliser 
 *  means that this record is already marked */
extern void this_marks_a_marked_record (void *, int);

struct lrecord_implementation
  {
    Lisp_Object (*marker) (Lisp_Object, void (*mark_object) (Lisp_Object));
    void (*printer) (Lisp_Object, Lisp_Object printcharfun, int escapeflag);
    void (*finaliser) (void *header, int for_disksave);
    int (*size_in_bytes) (void *header);
    int (*equal) (Lisp_Object obj1, Lisp_Object obj2, int depth);
  };
#define DEFINE_LRECORD_IMPLEMENTATION(name,marker,printer,nuker,sizer,equal) \
 CONST struct lrecord_implementation name[2] = \
   { { (marker), (printer), (nuker), (sizer), (equal) }, \
     { 0, 0, this_marks_a_marked_record, 0, 0 } }

#define XRECORD(a) ((void *) XPNTR(a))
#define LRECORDP(a) (XTYPE((a)) == Lisp_Record)
#define XRECORD_LHEADER(a) ((struct lrecord_header *) XRECORD(a))
#define RECORD_TYPEP(x, ty) (LRECORDP ((x)) \
                             && XRECORD_LHEADER((x))->implementation == (ty))
#define CHECK_RECORD(x, ty, p, i) \
  do { if (!RECORD_TYPEP((x), (ty))) x = wrong_type_argument ((p), (x)); } \
  while (0)

/* For hysterical compatibility (and to make it easier to change something
 *  from a record implementation back to a directly tagged frob */
#define XSETR(var, type, ptr) XSET(var, Lisp_Record, ptr)

extern void *alloc_lcrecord (int size, CONST struct lrecord_implementation *);
extern void free_lcrecord (struct lcrecord_header *);

extern int gc_record_type_p (Lisp_Object frob,
			     CONST struct lrecord_implementation *type);

#endif /* _EMACS_LRECORD_H_ */
