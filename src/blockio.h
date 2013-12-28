
#ifndef _EMACS_BLOCKIO_H_
#define _EMACS_BLOCKIO_H_

/* Nonzero during a critical section.  At such a time, an input interrupt
   does nothing but set `x_pending_input'.  */
extern int x_input_blocked;

#ifdef DEBUG_INPUT_BLOCKING

/* Begin critical section. */
#define BLOCK_INPUT (note_block_input (__FILE__, __LINE__), x_input_blocked++)

/* End critical section. */
#define UNBLOCK_INPUT {                            \
			  note_unblock_input (__FILE__, __LINE__); \
			  x_input_blocked--;       \
			  if (x_input_blocked < 0) \
			    abort ();              \
		       }

#define TOTALLY_UNBLOCK_INPUT (note_totally_unblocked (__FILE__, __LINE__), x_input_blocked = 0)
#define UNBLOCK_INPUT_RESIGNAL UNBLOCK_INPUT

#else

/* Begin critical section. */
#define BLOCK_INPUT (x_input_blocked++)

/* End critical section. */
#define UNBLOCK_INPUT {                            \
			  x_input_blocked--;       \
			  if (x_input_blocked < 0) \
			    abort ();              \
		       }

#define TOTALLY_UNBLOCK_INPUT (x_input_blocked = 0)
#define UNBLOCK_INPUT_RESIGNAL UNBLOCK_INPUT

#endif /* DEBUG_INPUT_BLOCKING */

#endif /* _EMACS_BLOCKIO_H_ */
