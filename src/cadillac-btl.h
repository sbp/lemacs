/*
 * cadillac-btl.h, Module CADILLAC
 *
 * ***************************************************************************
 *
 *        Copyright (C) 1990 by Lucid Inc.,  All Rights Reserved
 *
 * ***************************************************************************
 *
 * Generic header file for Cadillac backtrace logging code.
 *
 * Revision:	29-Jan-92 11:46:11
 *
 * Programmer: Harlan Sexton
 *
 * $Header: cadillac-btl.h,v 100.1 92/04/13 12:09:16 devin Exp $
 *
 * Edit-History:
 *
 * Created:  2-Nov-90 by hbs
 *
 * End-of-Edit-History
 */

#include <sys/types.h>
#ifdef sun4
#include <a.out.h>
#endif

#define WEIGHT_MARKER_PC ((caddr_t) 1)

/***************************************************************************/
/*************             STACK FRAME OPERATIONS            ***************/
/***************************************************************************/

/* 
  This section defines the type FRAME, the operations INIT_FRAME, and
  PREVIOUS_FRAME, and the access forms FRAME_ARG and FRAME_PC.
 */


/* Generic definitions */

struct btl_stack_frame
{
#ifdef NCR486
  caddr_t previous_pc;
#endif
  caddr_t sp;
  caddr_t fp;
  caddr_t pc;
#ifndef NCR486
  caddr_t previous_pc;
#endif
#ifdef sun4
  caddr_t handler_fp;
  caddr_t o7_reg;
#endif
};

#define FRAME struct btl_stack_frame

#define FRAME_SLOT_REF(frame, slot, offset) \
  (*((caddr_t *)((frame).slot + offset)))

#define FRAME_ARG_REF(frame, index) \
 (FRAME_SLOT_REF \
  ((frame), ARG_REG, ARG0_OFFSET + (ARG_SIGN * sizeof(caddr_t) * (index))))

#define FRAME_SLOT_ADDR(frame, slot, offset) ((frame).slot + offset)

#define FRAME_ARG_ADDR(frame, index) \
 (FRAME_SLOT_ADDR \
  ((frame), ARG_REG, ARG0_OFFSET + (ARG_SIGN * sizeof(caddr_t) * (index))))

#define FRAME_PC(frame) ((frame).pc)

#define FRAME_ARG(frame, index) \
 (((index >= 0) && (index < ARG_INDEX_LIMIT))?(FRAME_ARG_REF (frame, index)):0)


/* Sun4 specific definitions */

#ifdef sun4
#define RETURN_PC_OFFSET 60
#define PREV_FRAME_SP_OFFSET 56
#define ARG0_OFFSET 68
#define ARG_REG fp
#define ARG_SIGN 1
#define ARG_INDEX_LIMIT 6

#define PREVIOUS_FRAME_INTERNAL(frame) \
  (((frame).previous_pc = (frame).pc) && \
   ((frame).pc = FRAME_SLOT_REF ((frame), sp, RETURN_PC_OFFSET)) && \
   ((frame).sp = (frame).fp) && \
   ((frame).fp = (FRAME_SLOT_REF ((frame), sp, PREV_FRAME_SP_OFFSET))))

#define PREVIOUS_FRAME(frame) \
  (((frame).o7_reg)? \
   (previous_to_first_frame (&(frame))): \
   (PREVIOUS_FRAME_INTERNAL (frame)))

/* o7 register would be the i7 register for the "next" frame, if 
   there were one */
#define FIRST_FRAME_O7_REG(frame) \
((cadillac_btl_always_ignore_the_o7_register_contents)?0: \
 (FRAME_SLOT_REF ((frame), handler_fp, RETURN_PC_OFFSET)))

#define INIT_FRAME(scp, frame, sig_handler_fp) \
{ \
  (frame).handler_fp = (sig_handler_fp); \
  (frame).o7_reg = FIRST_FRAME_O7_REG(frame); \
  (frame).sp = (caddr_t)(scp)->sc_sp; \
  (frame).fp = (FRAME_SLOT_REF ((frame), sp, PREV_FRAME_SP_OFFSET)); \
  (frame).pc = (caddr_t)(scp)->sc_pc; \
  (frame).previous_pc = 0; \
}

/* Sparc PC relative CALL instr: 01 <30-bit displacement> */
#define PCREL_MASK 0xc0000000
#define PCREL_INSTR 0x40000000
#define PCRE_P(instr) (((instr) & PCREL_MASK) == PCREL_INSTR)

/* Two register Sparc indirect CALL instr: 
   10 <5-bit Rd> 111000 <5-bit RS1> 0 <8 bits ignored> <5-bit RS2> */
#define INDIRECT_MASK1  0xc1f82000
#define INDIRECT_INSTR1 0x81c00000
#define INDR1_P(instr) (((instr) & INDIRECT_MASK1) == INDIRECT_INSTR1)

/* One register Sparc indirect CALL instr: 
   10 <5-bit Rd> 111000 <5-bit RS1> 1 <13-bit displacement> */
#define INDIRECT_MASK2  0xc1f82000
#define INDIRECT_INSTR2 0x81c02000
#define INDR2_P(instr) (((instr) & INDIRECT_MASK2) == INDIRECT_INSTR2)

/* Sparc indirect CALL instr: 
   10 <5-bit Rd> 111000 <5-bit RS1> 0/1
                         <13-bit disp>/<8 bits ignored><5bit RS2> */
#define INDIRECT_MASK  0xc1f80000
#define INDIRECT_INSTR 0x81c00000
#define INDR_P(instr) (((instr) & INDIRECT_MASK) == INDIRECT_INSTR)

#define CALL_INSTR_P(i) (PCRE_P(i) || INDR_P(i))

/* needed because some compilers don't make stack-frames for leaf calls */
static int previous_to_first_frame (bsf_ptr)
     struct btl_stack_frame *bsf_ptr;
{
  extern int etext();
  caddr_t o7_contents = bsf_ptr->o7_reg;
  int return_val = 0;
  
  bsf_ptr->o7_reg = 0;

  if (!(((long) o7_contents) & (sizeof (long) - 1)) &&
      (o7_contents < (caddr_t) etext) &&
      (o7_contents >= ((caddr_t) PAGSIZ)))
    {
      long possible_call_instr = *((long *) o7_contents);
      if (CALL_INSTR_P (possible_call_instr))
        {
          bsf_ptr->previous_pc = bsf_ptr->pc;
          bsf_ptr->pc = o7_contents;
          return_val = o7_contents && bsf_ptr->sp && bsf_ptr->fp;
        }
      else
        {
          return_val = PREVIOUS_FRAME_INTERNAL (*bsf_ptr);
        }
    }
  else
    {
      return_val = PREVIOUS_FRAME_INTERNAL (*bsf_ptr);
    }

  return return_val;
}

#endif


#ifdef sun3
#define RETURN_PC_OFFSET -4
#define PREV_FRAME_FP_OFFSET 0
#define PREV_SP_INCREMENT 8
#define ARG0_OFFSET 8
#define ARG_SIGN 1
#define ARG_REG fp
#define ARG_INDEX_LIMIT 6

#define PREVIOUS_FRAME(frame) \
  (((frame).previous_pc = (frame).pc) && \
   ((frame).sp = (frame).fp + PREV_SP_INCREMENT) && \
   ((frame).pc = FRAME_SLOT_REF ((frame), sp, RETURN_PC_OFFSET)) && \
   ((frame).fp = (FRAME_SLOT_REF ((frame), fp, PREV_FRAME_FP_OFFSET))))

/* WARNING: If you change the representation of a btl_stack_frame 
   IN ANY WAY, you will have the change the assembler code here. */
#define INIT_FRAME(scp, frame, dummy) \
{ \
  asm ("movel a6,a6@(-8)"); /* get fp into structure */ \
  (frame).sp = (caddr_t)(scp)->sc_sp; \
  while ((frame).fp <= (frame).sp) (frame).fp = *((caddr_t *)(frame).fp); \
  (frame).pc = (caddr_t)(scp)->sc_pc; \
  (frame).previous_pc = 0; \
}

#endif


/* See i486 MicroProcessor Programmers Reference Manual and
   High Performance C Compiler Programming Guide.  This particular code
   is machine-dependent and compiler-dependent. LTL. */

#ifdef NCR486
#define RETURN_PC_OFFSET 0
#define PREV_FRAME_FP_OFFSET 0
#define PREV_SP_INCREMENT 4
#define ARG0_OFFSET 8
#define ARG_SIGN 1
#define ARG_REG fp
#define ARG_INDEX_LIMIT 6

#define PREVIOUS_FRAME(frame) \
  (((frame).previous_pc = (frame).pc) && \
   ((frame).sp = (frame).fp + PREV_SP_INCREMENT) && \
   ((frame).pc = FRAME_SLOT_REF ((frame), sp, RETURN_PC_OFFSET)) && \
   ((frame).fp = (FRAME_SLOT_REF ((frame), fp, PREV_FRAME_FP_OFFSET))))

/* WARNING: If you change the representation of a btl_stack_frame
   IN ANY WAY, you will have to change the assembler code here. */
#define INIT_FRAME(scp, frame, dummy) \
{ \
  asm ("movl %ebp,-8(%ebp)"); /* get fp into structure */ \
  (frame).sp = (caddr_t)(scp)->sc_sp; \
  while ((frame).fp <= (frame).sp) (frame).fp = *((caddr_t *)(frame).fp); \
  (frame).pc = (caddr_t)(scp)->sc_pc; \
  (frame).previous_pc = 0; \
}
#endif

/***************************************************************************/
/*************              LOG VECTOR OPERATIONS            ***************/
/***************************************************************************/

/* 
  This section defines the macros ADD_TO_BT_LOG_DATA,
  ADD_DELIMITER_TO_BT_LOG_DATA, and ADD_TO_PC_LOG_DATA. The first takes a
  FRAME argument, the second one no argument, and the third a pc as an
  argument.
 */

#define ADD_TO_PC_LOG_DATA(frame) ADD_TO_LOG_DATA(FRAME_PC(frame), 0, 0)
#define ADD_DELIMITER_TO_BT_LOG_DATA() ADD_TO_LOG_DATA (0, 0, 0)

#define ADD_WEIGHT_TO_BT_LOG_DATA(weight, alternate_vector_p, vec) \
{ \
  ADD_TO_LOG_DATA(weight, alternate_vector_p, vec); \
  ADD_TO_LOG_DATA \
    (WEIGHT_MARKER_PC, alternate_vector_p, (((caddr_t *)vec)+1));\
}

#define ADD_TO_LOG_DATA(pc, alternate_vector_p, alternate_vector) \
{ \
  if (alternate_vector_p) \
    *((caddr_t *)alternate_vector) = (caddr_t) (pc); \
  else \
    { \
      log_data_vector[log_data_vector_index++] = (caddr_t) (pc); \
      if ((log_data_vector_index >= LOG_DATA_VECTOR_LENGTH)|| \
          (log_data_samples_count + log_data_vector_index >= \
           log_data_samples_limit)) \
        write_out_log_data_vector(); \
    } \
}

#define BDE_PC(x) (*((caddr_t *) (x)))

#define BDE_WEIGHTED_P(x) ((BDE_PC(x)) == WEIGHT_MARKER_PC)
#define BDE_WEIGHT(x) (*((int *)(((caddr_t)(x)) - (sizeof (caddr_t *)))))
#define BDE_WSIZE (2 * sizeof(caddr_t *))

#ifndef EMACS_BTL
/* normal case - when a data-entry is just a PC */
#define ADD_TO_BT_LOG_DATA(frame, alternate_vector_p, vec) \
ADD_TO_LOG_DATA (FRAME_PC (frame), alternate_vector_p, vec)
#else

#include "config.h"
#include "lisp.h"
#undef NULL

#include "btl-get.h"

extern void BTL_before_Fgarbage_collect_stub();
extern Lisp_Object Fgarbage_collect();

#define ELISP_GC_SKIP_LOWER ((caddr_t) BTL_before_Fgarbage_collect_stub)
#define ELISP_GC_SKIP_UPPER ((caddr_t) Fgarbage_collect)

/* messy case - when a data-entry may be an elisp function id */
#define ADD_TO_BT_LOG_DATA(frame, alternate_vector_p, vec) \
{ \
  caddr_t pc = FRAME_PC (frame); \
 \
  if (pc && \
      ((frame).pc != (frame).previous_pc) && \
      ((pc < ELISP_GC_SKIP_LOWER) || (pc >= ELISP_GC_SKIP_UPPER))) \
    ADD_TO_LOG_DATA (pc, alternate_vector_p, vec); \
}
#endif


/***************************************************************************/
/*************             DATA FILE FORMAT                  ***************/
/***************************************************************************/

enum Btl_Data_Format { BDF_PC_ONLY_FORMAT = 0, BDF_ELISP_FORMAT = 1};

#define CHARS_PER_BTL_DATA_ENTRY sizeof(caddr_t)/sizeof(char)
#define NO_LOGGING	0
#define PC_LOGGING	1
#define BACKTRACE_LOGGING 2
#define LEGAL_LOGGING_TYPE(type) \
  (((type) == BACKTRACE_LOGGING) || ((type) == PC_LOGGING))

#define BTL_DATA_MAGIC_NUMBER 0x81726354

/* the total size of this header (that is, the size of the stuff in the
   output file before the start of the data) is guaranteed to be a multiple
   of the size of a caddr_t object, whatever that happens to be */

struct btl_data_file_header
{
  long magic_number;
  long size;
  long log_type;
  enum Btl_Data_Format entry_format;
  /* we will process the backtrace log file by mapping it into
     memory when we can -- these slots will be set to be the
     start and end offsets of the data in the file, which will
     be used to compute the start and end "pointers" */
  long start_offset;
  long end_offset;
};

/***************************************************************************/
/*************             EXTERNAL FUNCTIONS                ***************/
/***************************************************************************/

#include "cadillac-btl-extern.h"
