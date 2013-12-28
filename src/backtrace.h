#include <setjmp.h>
       
/* These definitions are used variously definition is used in alloc.c 
   and keyboard.c, and it is reported that putting some of then in
   lisp.h makes cc bomb out. */

struct backtrace
  {
    struct backtrace *next;
    Lisp_Object *function;
    Lisp_Object *args;		/* Points to vector of args. */
    int nargs;			/* length of vector */
				/* if nargs is UNEVALLED, args points to
				   slot holding list of unevalled args */
#ifdef EMACS_BTL
    /* The value of a Lisp integer that specifies the symbol being
       "invoked" by this node in the backtrace, or 0 if the backtrace
       doesn't correspond to a such an invocation */
    int id_number;
#endif
    char evalargs;
    char debug_on_exit;		/* Nonzero means call value of debugger
				   when done with this operation. */
  };


struct catchtag
  {
    Lisp_Object tag;
    Lisp_Object val;
    struct catchtag *next;
    struct gcpro *gcpro;
    jmp_buf jmp;
    struct backtrace *backlist;
    int lisp_eval_depth;
    int pdlcount;
  };

extern struct catchtag *catchlist;
extern struct backtrace *backtrace_list;

