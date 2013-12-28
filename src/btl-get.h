/*
 * btl-get.h, Module CADILLAC
 *
 * ***************************************************************************
 *
 *        Copyright (C) 1990 by Lucid Inc.,  All Rights Reserved
 *
 * ***************************************************************************
 *
 * Defines this macro which is shared between Ffuncall code and btl code.
 *
 * Revision:	20-Nov-90 19:43:01
 *
 * Programmer: Harlan Sexton
 *
 * $Header: btl-get.h,v 100.1 92/04/13 12:08:59 devin Exp $
 *
 * Edit-History:
 *
 * Created:  6-Nov-90 by hbs
 *
 * End-of-Edit-History
 */

/* this is just a 32bit random integer -- the idea is that we will
   put the symbol index in the fourth argument BEFORE
   putting this value in the third argument  -- so that if this value is 
   in the third arg, then we have a valid index in the fourth arg */

#define FFUNCALL_SYMBOL_INDEX_SET_FLAG 0x8803ea9


/* this is open-coded not for speed but so that there won't be
   lots of new spurious stack-frames when monitoring Ffuncall --
   I don't actually know that this was worth doing, but I suspect
   not... */

#define BTL_GET(sym, tag, id, found_it) \
{ \
  found_it = 0; \
 \
  if (XTYPE (sym) == Lisp_Symbol) \
    { \
      register Lisp_Object tail = XSYMBOL (sym)->plist; \
 \
      if (XTYPE (tail) == Lisp_Cons) \
        /* tail is ALWAYS a cons at the top of the loop! */ \
        while (!EQ(tail, Qnil)) \
          { \
            register Lisp_Object tem; \
            tem = XCONS(tail)->car; \
            if (EQ(tag, tem)) \
              { \
                found_it = 1; \
                tem = XCONS(tail)->cdr; \
                if (XTYPE (tem) == Lisp_Cons) id = XCONS(tem)->car; \
                break; \
              } \
            else \
              { \
                /* do CDR twice */ \
                tail = XCONS(tail)->cdr; \
                if (XTYPE (tail) != Lisp_Cons) \
                  break; \
                else \
                  tail = XCONS(tail)->cdr; \
                /* make sure that tail is a cons at the top of loop */ \
                if (XTYPE (tail) != Lisp_Cons) break; \
              } \
          } \
    } \
} 
