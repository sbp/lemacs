/*
 * cadillac-btl.c, Module CADILLAC
 *
 * ***************************************************************************
 *
 *        Copyright (C) 1990 by Lucid Inc.,  All Rights Reserved
 *
 * ***************************************************************************
 *
 * Generic backtrace-logging code for Cadillac
 *
 * Revision:	29-Jan-92 16:47:31
 *
 * Programmer: Harlan Sexton
 *
 * $Header: cadillac-btl.c,v 100.1 92/04/13 12:09:13 devin Exp $
 *
 * Edit-History:
 *
 * Created:  2-Nov-90 by hbs
 *
 * End-of-Edit-History
 */

#include "cadillac-btl.h"

#include <stdio.h>
#include <signal.h>
#include <alloca.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/param.h>

#define LOG_DATA_VECTOR_LENGTH 20000
#define OPEN_LOG_FILE_FLAG  O_WRONLY | O_CREAT | O_TRUNC
#define OPEN_LOG_FILE_MODE  0644

static caddr_t log_data_vector[LOG_DATA_VECTOR_LENGTH];
static long log_data_vector_index;
static long log_data_samples_limit;
static long log_data_samples_count;

static int log_data_fd;
static char *log_data_file_name;

static void (*handler_function_ptr)();

/* functions defined in this file */
static void btl_message (int error_flag, char *format_string,
                         void *arg1, void *arg2, void *arg3, void *arg4);
static void btl_write_error_warning (void);
static void btl_write_limit_reached_warning (void);
static int btl_write (char *buf, int bytes);
static void write_out_log_data_vector_internal (void);
static void write_out_log_data_vector (void);
static void log_state_defaults (void);
static int initialize_log_state 
  (char *outfile, char *execfile, 
   long limit, long interval, long logging_type);
static void terminate_logging (int write_log_vector);
static void btl_signal_handler 
  (int sig, int code, struct sigcontext *scp, char *addr);
#ifdef EMACS_BTL
static void elisp_backtrace_logging_handler 
  (struct sigcontext *scp, caddr_t handler_fp);
static void elisp_pc_logging_handler 
  (struct sigcontext *scp, caddr_t handler_fp);
static void record_elisp_backtrace_internal
   (int skip, int height, int weight, caddr_t *to);
#endif
static void backtrace_logging_handler 
  (struct sigcontext *scp, caddr_t handler_fp);
static void pc_logging_handler 
  (struct sigcontext *scp, caddr_t handler_fp);
static void init_frame (FRAME *fptr);
static void record_backtrace_internal 
(int skip, int height, int weight, caddr_t *from, caddr_t *to);

/* 
  We may need to use this sig/itimer pair if the program being
  "traced" is too fragile:

     ITIMER_VIRTUAL      Decrements in process virtual time. It
                         runs only when the process is executing.
                         A SIGVTALRM signal is delivered when it
                         expires.

  The possible pairs are:

   (SIGPROF, 	ITIMER_PROF)
   (SIGVTALRM, 	ITIMER_VIRTUAL)
   (SIGALRM, 	ITIMER_REAL)

   (SIGINT, -1) -- for debugging purposes

*/

static int handler_signal_number;
static int handler_itimer_flag;
static int handler_set;
static struct itimerval set_itimer_struct;

#define LOGGING_OFF	0
#define LOGGING_ON	1

static long btl_log_flag, btl_log_state;
static long btl_log_type;
static enum Btl_Data_Format btl_log_format;

#if sun4
int cadillac_btl_always_ignore_the_o7_register_contents;
#endif

#ifdef EMACS_BTL
int emacs_btl_elisp_only_p;
#endif

/* for reporting error messages from _open lossage */
extern int sys_nerr;
extern char *sys_errlist[];
extern int errno;
#define SYS_ERR (((errno >=0) && (errno < sys_nerr))?\
  (sys_errlist[errno]):"Unknown error message")

/* COMMON INTERNAL FUNCTIONS */

#define PRINT_BUF_SIZE 1024

static void btl_message 
  (int error_flag, char *format_string, 
   void *arg1, void *arg2, void *arg3, void *arg4)
{
  char buf_space[PRINT_BUF_SIZE];
  char *buf = buf_space;

  bzero (buf, PRINT_BUF_SIZE);

  sprintf (buf, format_string, arg1, arg2, arg3, arg4);

  /* check to see that we haven't overrun the buffer allotted -- this
     will never happen here unless the data_file name is more than
     800 characters long... */
  {
    char *tmp = buf_space;
    while (*tmp++);
    if (tmp > buf + sizeof (buf_space))
      {
        bzero (buf, PRINT_BUF_SIZE);
        sprintf (buf, 
                 "\n\n\n     *** BTL error message too long = %d! ***\n\n\n", 
                 (int) (tmp - buf));
      }
  }

#ifdef EMACS_BTL
  if (error_flag)
    {
      extern void error ();
      error (buf);
    }  
  else
    {
      extern void insert_before_markers ();
      insert_before_markers (buf, strlen (buf));
    }
#else
  if (error_flag)
    {
      fprintf (stderr, buf);
      fflush (stderr);
    }
  else
    {
      fprintf (stdout, buf);
      fflush (stdout);
    }
#endif

  return;
}

static void btl_write_error_warning () 
{
  char *btl_type;

  switch (btl_log_type)
    {
    case PC_LOGGING:
      btl_type = "PC logging";
      break;

    case BACKTRACE_LOGGING:
      btl_type = "backtrace logging";
      break;

    default:
      btl_type = "an unknown type of logging";
      break;
    }

  btl_message 
    (1, 
     "\n\nWARNING: During %.200s - Error writing datafile %.200s: %.200s\n\n",
     btl_type, log_data_file_name, SYS_ERR, 0);
  return;
}

static void btl_write_limit_reached_warning () 
{
  char *btl_type;
  char *units;

  switch (btl_log_type)
    {
    case PC_LOGGING:
      btl_type = "PC logging";
      units = "PC samples";
      break;

    case BACKTRACE_LOGGING:
      btl_type = "backtrace logging";
      units = "backtrace stack-frames";
      break;

    default:
      btl_type = "an unknown type of logging";
      units = "units";
      break;
    }

  btl_message
    (0, "\n\n\n* WARNING: During %.200s - The data collection limit of %d \
\n*          %.200s has been reached. No more data will be \
\n*          collected in the file %.200s during this session. \n\n\n\n",
     btl_type, 
     (void *) log_data_samples_limit, 
     units, log_data_file_name);
  return;
}


static int btl_write (char *buf, int bytes)
{
  while (bytes)
    {
      long bytes_written = write(log_data_fd, buf, bytes);
    
      /* if we didn't write anything, we will give up */
      if(bytes_written <= 0)
        {
          btl_log_state = LOGGING_OFF;
          btl_log_flag = LOGGING_OFF;
          btl_write_error_warning();
          return 0;
        }
      else
        {
          buf += bytes_written;
          bytes -= bytes_written;
        }
    }
  return 1;
}


static void write_out_log_data_vector_internal ()
{
  long current_limit = log_data_samples_limit - log_data_samples_count;
  long number_to_write = 
    (log_data_vector_index>current_limit)?
    current_limit:log_data_vector_index;
  long bytes = number_to_write*CHARS_PER_BTL_DATA_ENTRY;
  char *buf = (char *) log_data_vector;

  /* if there is nothing to do, don't do anything */
  if (number_to_write <= 0) return;

  if ((number_to_write > 0) && LEGAL_LOGGING_TYPE (btl_log_type))
    {
      /* reset the "state", and then try to write out the vector */
      log_data_samples_count += number_to_write;
      log_data_vector_index = 0;
      btl_write (buf, bytes);

      /* if we have sampled enough things, then warn and quit */
      if(log_data_samples_limit == log_data_samples_count)
        {
          btl_log_state = LOGGING_OFF;
          btl_log_flag = LOGGING_OFF;
          /* just in case we got another clock tick between the call
             to terminate_logging() and the actual termination */
          log_data_vector_index = 0;
          btl_write_limit_reached_warning();
          terminate_logging(0); 
        }
    }
  return;
}

static void write_out_log_data_vector ()
{
  /* suspend the logging while we are writing out the results */
  btl_log_flag = LOGGING_OFF;
  write_out_log_data_vector_internal ();
  btl_log_flag = btl_log_state;
  return;
}

static void log_state_defaults()
{
  /* make sure that these handler vars have some assigned value */
  if (!handler_set)
    {
#ifdef NCR486
/* Our UNIX is losing.  The SIGPROF occasionally cause system calls to fail. */
/* change default signal to SIGVTALRM */
      if (!handler_signal_number) handler_signal_number = SIGVTALRM;
      if (!handler_itimer_flag) handler_itimer_flag = ITIMER_VIRTUAL;
#else
      if (!handler_signal_number) handler_signal_number = SIGPROF;
      if (!handler_itimer_flag) handler_itimer_flag = ITIMER_PROF;
#endif /* NCR486 */
    }

  /* turn off logging */
  btl_log_state = LOGGING_OFF;
  btl_log_flag = LOGGING_OFF;
  btl_log_type = NO_LOGGING;
  btl_log_format = BDF_PC_ONLY_FORMAT;

  /* before we lose this FD, try to close it */
  if (log_data_fd > 0) close(log_data_fd);

  /* set state variables to default values */
  log_data_fd = -1;
  log_data_file_name = "/dev/null";
  log_data_vector_index = 0;
  log_data_samples_count = 0;
  log_data_samples_limit = 0;
  return;
}

static int
start_btl_log_timer ()
{
  if ((handler_signal_number == SIGPROF) ||
      (handler_signal_number == SIGALRM) ||
      (handler_signal_number == SIGVTALRM))
    {
      int fail = setitimer (handler_itimer_flag, &set_itimer_struct, 0);

      if (fail)
        {
          btl_message
            (1, "\n\nCan't start timer with interval %d: %.200s\n\n", 
             (void *) set_itimer_struct.it_value.tv_usec, 
             SYS_ERR, 0, 0);
          return(0);
        }
    }
  return 1;
}

static int
stop_btl_log_timer ()
{
  /* next, turn off the timer if this is appropriate */
  if ((handler_signal_number == SIGPROF) ||
      (handler_signal_number == SIGALRM) ||
      (handler_signal_number == SIGVTALRM))
    {
      struct itimerval clear_itimer_struct =  { {0,0}, {0,0} };
      int fail = setitimer (handler_itimer_flag, &clear_itimer_struct, 0);
      if (fail)
        {
          btl_message 
            (1, "\n\nCan't stop timer: %.200s\n\n", SYS_ERR, 0, 0, 0);
          return (0);
        }
    }
  return 1;
}


#define ROUND_UP(val, mod) ((1 + ((val)/(mod))) * (mod))

static int 
initialize_log_structure
  (char *outfile, char *execfile, long limit, long logging_type)
{
  char expanded_execfile_space[MAXPATHLEN];

  /* first set everything to the default values */
  log_state_defaults();
  
  /* try to expand the execfile to get a absolute pathname to put in
     the data file */
  if (execfile)
    {
      extern char *realpath ();
      char *expanded_execfile = realpath (execfile, expanded_execfile_space);
    
      if (!expanded_execfile)
        {
          btl_message 
            (1, "\n\nCan't find path to execfile %.200s: %.200s\n\n", 
             execfile, SYS_ERR, 0, 0);
          return(0);
        } 
      else 
        {
          char *automount_prefix = "/tmp_mnt"; 
          int len = strlen (automount_prefix);
          if (!strncmp (automount_prefix, expanded_execfile, len))
            expanded_execfile += len;
          execfile = expanded_execfile;
        }
    }

  /* try to open the outfile */
  log_data_fd = open(outfile,OPEN_LOG_FILE_FLAG,OPEN_LOG_FILE_MODE);

  if(log_data_fd >= 0)
    {
      /* initialize the outfile header */
      struct btl_data_file_header *hdr;
      int size = sizeof (struct btl_data_file_header);

      if (execfile) size += strlen (execfile) + 1;
      size = ROUND_UP (size, CHARS_PER_BTL_DATA_ENTRY);

      hdr = (struct btl_data_file_header *) alloca (size);
      bzero (hdr, size);

      hdr->magic_number = BTL_DATA_MAGIC_NUMBER;
      hdr->size = size;
      hdr->log_type = logging_type;

#ifndef EMACS_BTL
      hdr->entry_format = BDF_PC_ONLY_FORMAT;
#else
      hdr->entry_format = 
        (emacs_btl_elisp_only_p)?BDF_ELISP_FORMAT:BDF_PC_ONLY_FORMAT;
#endif

      if (execfile)
        strcpy (((char *) hdr) + sizeof (struct btl_data_file_header), 
                execfile);

      /* write out the data file header, if you can */
      if (!btl_write ((char *)hdr, hdr->size))
        {
          if (log_data_fd > 0) close (log_data_fd);
          return 0;
        }
        
      /* do the state initialization */
      log_data_file_name = outfile;
      log_data_vector_index = 0;
      log_data_samples_count = 0;
      if (limit <= 0) limit = 1000000;
      log_data_samples_limit = limit;

      btl_log_type = logging_type;
      btl_log_format = hdr->entry_format;
      btl_log_flag = btl_log_state = LOGGING_OFF;
    }
  else
    {
      btl_message (1, "\n\nCan't open %.200s to make log file: %.200s\n\n", 
                   outfile, SYS_ERR, 0, 0);
      return(0);
    }
}

static int initialize_log_state
  (char *outfile, char *execfile, long limit, long interval, long logging_type)
{ 
  int return_status = initialize_log_structure(outfile, execfile,
                                               limit, logging_type);
  if (!return_status || !handler_signal_number)
    return (0);

  /* set up the handler, remembering the old one if it is interesting */
  if (!handler_function_ptr)
    {
      handler_function_ptr = 
        signal (handler_signal_number, btl_signal_handler);
      /* since SIG_DFL is 0, this is an expensive no-op */
      if (handler_function_ptr == SIG_DFL) handler_function_ptr = 0;
    }
  else 
    signal (handler_signal_number, btl_signal_handler);

  /* make interval a reasonable value, and then set the slots in the
     set_itimer struct */
  {
    /* initialize set_itimer_struct -- this is sort of ugly, but C is
       what it is... */
    struct itimerval tmp = { {0,1}, {0,1} };
    set_itimer_struct = tmp;
  }

  if (interval <= 0) 
    interval = 1;
  else if (interval >= 500000)
    interval = 500000;
  set_itimer_struct.it_interval.tv_usec = interval;
  set_itimer_struct.it_value.tv_usec = interval;
  return (1);
}

static void terminate_logging (int write_log_vector)
{
  /* if logging is already off, then quit */
  if (btl_log_type == NO_LOGGING) return;

  /* first, turn off the logging */
  cadillac_stop_logging ();
  
  /* reset the handler, if there was one -- otherwise leave this
     one in place, since there is no particular reason not to*/
  if (handler_function_ptr)
    {
      signal (handler_signal_number, handler_function_ptr);
      handler_function_ptr = 0;
    }

  if (write_log_vector) write_out_log_data_vector();
  if (log_data_fd > 0) close(log_data_fd);
  handler_set = 0;
  log_state_defaults();
  return;
}


static void btl_signal_handler 
  (int sig, int code, struct sigcontext *scp, char *addr)
{
  caddr_t handler_fp;
#ifdef sun4
  caddr_t *alias = (caddr_t *) scp;
  caddr_t tmp = *alias;
  asm ("ta 3");
  asm ("st %fp,[%i2]");
  handler_fp = *alias;
  *alias = tmp;
#endif

  if(btl_log_flag == LOGGING_ON)
    {
      /* prevent re-entrance */
      btl_log_flag = LOGGING_OFF;

      switch (btl_log_type)
	{
	case PC_LOGGING:
#ifdef EMACS_BTL
          if (btl_log_format == BDF_ELISP_FORMAT)
            elisp_pc_logging_handler(scp, handler_fp);
          else 
#endif
	  pc_logging_handler(scp, handler_fp);
	  break;
	  
	case BACKTRACE_LOGGING:
#ifdef EMACS_BTL
          if (btl_log_format == BDF_ELISP_FORMAT)
            elisp_backtrace_logging_handler(scp, handler_fp);
          else 
#endif
            backtrace_logging_handler(scp, handler_fp);
	  break;
	  
	default:
	  break;
	}
      /* reset flag */
      btl_log_flag = btl_log_state;
    }

  return;
}


/* Emacs Lisp BACKTRACE logging handler function */

#ifdef EMACS_BTL
#include "backtrace.h"

static void elisp_backtrace_logging_handler 
  (struct sigcontext *scp, caddr_t handler_fp)
{
  struct backtrace *bt_list = backtrace_list;
  struct backtrace *last = 0;

  /* first, write a delimiter into the backtrace */
  if (bt_list)
    ADD_DELIMITER_TO_BT_LOG_DATA ();
  
  while (bt_list && (bt_list != last))
    {
      int id = bt_list->id_number;

      if (id)
        ADD_TO_LOG_DATA (id, 0, 0);
      
      last = bt_list;
      bt_list = bt_list->next;
    }
  
  return;
}

static void elisp_pc_logging_handler
  (struct sigcontext *scp, caddr_t handler_fp)
{
  struct backtrace *bt_list = backtrace_list;
  struct backtrace *last = 0;

  while (bt_list && (bt_list != last))
    {
      int id = bt_list->id_number;

      if (id)
        {
          ADD_TO_LOG_DATA (id, 0, 0);
          return;
        }

      last = bt_list;
      bt_list = bt_list->next;
    }
  
  return;
}
#endif

/* BACKTRACE logging handler function */

static void backtrace_logging_handler 
  (struct sigcontext *scp, caddr_t handler_fp)
{
  FRAME current_frame;
  INIT_FRAME (scp, current_frame, handler_fp);
    
  /* first, write a delimiter into the backtrace */
  ADD_DELIMITER_TO_BT_LOG_DATA ();
    
  /* add the frame to the log data */
  ADD_TO_BT_LOG_DATA (current_frame, 0, 0);

  /* now do the basic loop adding data until there is no more */
  while (PREVIOUS_FRAME(current_frame)) ADD_TO_BT_LOG_DATA (current_frame, 0, 0);

  return;
}



/* PC logging handler function */


static void pc_logging_handler
  (struct sigcontext *scp, caddr_t handler_fp)
{
  FRAME current_frame;
  INIT_FRAME (scp, current_frame, handler_fp);

  ADD_TO_PC_LOG_DATA (current_frame);
  return;
}

#ifdef sun4
/* backtrace recording function, can be called from function */
static void
init_frame (FRAME *fptr)
{
  FRAME tmp_frame;

  /* Do the system trap ST_FLUSH_WINDOWS */
  asm ("ta 3");
  asm ("st %sp, [%i0+0]");
  asm ("st %fp, [%i0+4]");
  fptr->pc = (char *) init_frame;
  fptr->o7_reg = 0;
  tmp_frame = *fptr;

  PREVIOUS_FRAME (tmp_frame);

  *fptr = tmp_frame;
  return;
}
#endif

#ifdef EMACS_BTL
static void
record_elisp_backtrace_internal (int skip, int height, int weight, caddr_t *to)
{
  struct backtrace *bt_list = backtrace_list;
  struct backtrace *last = 0;
  int to_index = 0;

  /* if we are copying to something and are restricted in the
     amount of output and we are supposed to record a weight, 
     then if don't have enough room, just quit, else reduce the
     size of the height to accomodate the weight entry */
  if (to && (height > 0) && weight)
    {
      if (height <= (BDE_WSIZE / CHARS_PER_BTL_DATA_ENTRY))
        return;
      else
        height -= (BDE_WSIZE / CHARS_PER_BTL_DATA_ENTRY);
    }

  while (bt_list && (bt_list != last) && (skip > 0))
    {
      int id = bt_list->id_number;

      if (id)
        skip--;
      
      last = bt_list;
      bt_list = bt_list->next;
    }
  
  if (!bt_list || (bt_list == last))
    return;
  else if (!to)
    ADD_DELIMITER_TO_BT_LOG_DATA ();
  
  if (height == 0)
    height = -1;

  while (bt_list && (bt_list != last) && (height != 0))
    {
      int id = bt_list->id_number;

      if (id)
        {
          ADD_TO_LOG_DATA (id, to, to + to_index);
          to_index++;
          height--;
        }
      
      last = bt_list;
      bt_list = bt_list->next;
    }

  if ((weight != 0) && (to_index > 0))
    ADD_WEIGHT_TO_BT_LOG_DATA(weight, to, to + to_index);
}
#endif

static void record_backtrace_internal 
(int skip, int height, int weight, caddr_t *from, caddr_t *to)
{
#ifdef EMACS_BTL
  if (!from && (btl_log_format == BDF_ELISP_FORMAT))
    {
      record_elisp_backtrace_internal (skip, height, weight, to);
      return;
    }
#endif

  if (!from)
    {
      FRAME current_frame;
      int to_index = 0;
      int i;

      /* if we are copying to something and are restricted in the
         amount of output and we are supposed to record a weight, 
         then if don't have enough room, just quit, else reduce the
         size of the height to accomodate the weight entry */
      if (to && (height > 0) && weight)
        {
          if (height <= (BDE_WSIZE / CHARS_PER_BTL_DATA_ENTRY))
            return;
          else
            height -= BDE_WSIZE / CHARS_PER_BTL_DATA_ENTRY;
        }

#ifdef sun4
      init_frame (&current_frame);
#else
      return;
#endif 

      for (i = 0; i < skip; i++)
        PREVIOUS_FRAME (current_frame);
    
      /* first, write a delimiter into the backtrace if we are writing to
         the log_data_vector, otherwise, don't bother */
      if (!to)
        ADD_DELIMITER_TO_BT_LOG_DATA ();

      /* write the current frame -- we know that we have room for at least
         one frame because of the calculation done above */
      ADD_TO_BT_LOG_DATA (current_frame, to, to + to_index);
      to_index++;
      height--;

      /* now do the basic loop adding data until there is no more */
      while (PREVIOUS_FRAME(current_frame) && height)
        {
          height--;
          ADD_TO_BT_LOG_DATA (current_frame, to, to + to_index);
          to_index++;
        }

      if (weight != 0)
        ADD_WEIGHT_TO_BT_LOG_DATA(weight, to, to + to_index);

      return;
    }
  /* this is not a function for copying bits! */
  else if (!to)
    {
      int i;

      /* first, write a delimiter into the backtrace */
      ADD_DELIMITER_TO_BT_LOG_DATA ();

      from += skip;

      /* just copy stuff out of the "from" vector into the log_data_vector */
      for (i = 0; i < height; i++)
        ADD_TO_LOG_DATA (*(from + i), 0, 0);

      if (weight != 0)
        ADD_WEIGHT_TO_BT_LOG_DATA(weight, 0, 0);

      return;
    }
}

/* EXTERNAL FUNCTIONS */

/* records a weighted backtrace -- the first argument is the number 
   of top frames to skip, and the second the weight to assign to the 
   stack. A weight of zero means don't put down a weight frame. Negative
   weights are allowed. */
void cadillac_record_backtrace (int skip, int weight)
{
  if ((btl_log_flag == LOGGING_ON) && 
      (handler_signal_number == 0) && 
      (btl_log_type == BACKTRACE_LOGGING))
    {
      /* prevent re-entrance be unsetting and resetting flag */
      btl_log_flag = LOGGING_OFF;
      record_backtrace_internal (skip, 0, weight, 0, 0);
      btl_log_flag = btl_log_state;
    }
}

/* returns 1 if successful, else 0 -- you can't start the logging
   process until you have initialized it */
int cadillac_start_logging ()
{
  if(btl_log_state == LOGGING_OFF)
    {
      switch (btl_log_type)
	{
	case PC_LOGGING:
	case BACKTRACE_LOGGING:
          if (!start_btl_log_timer ()) return 0;
          btl_log_flag = btl_log_state = LOGGING_ON;
          return 1;
	  
	default:
          return 0;
	}
    }
  return 0;
}


/* Shuts logging off but doesn't terminate it -- that is, the log file is
   still open, and when we restart, the handler and the interval will be
   the same as they are now. Returns 1 if logging was on, else 0. */
int cadillac_stop_logging ()
{
  int logging_currently_on = (btl_log_state == LOGGING_ON)?1:0;
  btl_log_flag = btl_log_state = LOGGING_OFF;
  stop_btl_log_timer ();
  write_out_log_data_vector();
  
  switch (btl_log_type)
    {
    case PC_LOGGING:
    case BACKTRACE_LOGGING:
      return logging_currently_on;
	  
    default:
      return 0;
    }
  return 0;
}

int cadillac_terminate_logging () 
{
  int logging_on = 0;

  switch (btl_log_type)
    {
    case PC_LOGGING:
    case BACKTRACE_LOGGING:
      logging_on = 1;
      break;
	  
    default:
      break;
    }
  terminate_logging(1); 

  return logging_on;
}


/* PC logging exported functions */

int cadillac_initialize_pc_logging   
  (char *outfile, char *execfile, long limit, long interval)
{ 
  return
    initialize_log_state (outfile, execfile, limit, interval, PC_LOGGING); 
}

/* Backtrace logging exported functions */

int cadillac_initialize_backtrace_logging 
  (char *outfile, char *execfile, long limit, long interval)
{ 
  return 
    initialize_log_state 
      (outfile, execfile, limit, interval, BACKTRACE_LOGGING); 
}

/* change the signal that you are using -- 
   returns 1 for success, 0 otherwise */
int cadillac_set_log_signal (int signal)
{
  /* this only works if logging is OFF */
  if (btl_log_type != NO_LOGGING) return 0;

  if (signal < 0)
    {
#ifdef NCR486
/* Our UNIX is losing.  The SIGPROF occasionally cause system calls to fail. */
/* change default signal to SIGVTALRM */
      handler_signal_number = SIGVTALRM;
      handler_itimer_flag = ITIMER_VIRTUAL;
#else
      handler_signal_number = SIGPROF;
      handler_itimer_flag = ITIMER_PROF;
#endif
      handler_set = 1;
      return 1;
    }
  
  switch (signal)
    {
    case 0:
      handler_signal_number = 0;
      handler_itimer_flag = 0;
      handler_set = 1;
      return 1;
    case SIGPROF:
      handler_signal_number = SIGPROF;
      handler_itimer_flag = ITIMER_PROF;
      handler_set = 1;
      return 1;
    case SIGVTALRM:
      handler_signal_number = SIGVTALRM;
      handler_itimer_flag = ITIMER_VIRTUAL;
      handler_set = 1;
      return 1;
    case SIGALRM:
      handler_signal_number = SIGALRM;
      handler_itimer_flag = ITIMER_REAL;
      handler_set = 1;
      return 1;
    case SIGINT:
      handler_signal_number = SIGINT;
      handler_itimer_flag = -1;
      handler_set = 1;
      return 1;
    default:
      return 0;
    }
  return 0;
}

#ifdef EMACS_BTL

void
record_malloc_backtrace (int skip, int size, int weight, caddr_t *vector)
{
  if ((btl_log_flag == LOGGING_ON) && 
      (handler_signal_number == 0) && 
      (btl_log_type == BACKTRACE_LOGGING))
    record_backtrace_internal (skip, size, weight, 0, vector);
}

void
save_malloc_backtrace (int size, int weight, caddr_t *vector)
{
  if ((btl_log_flag == LOGGING_ON) && 
      (handler_signal_number == 0) && 
      (btl_log_type == BACKTRACE_LOGGING))
    record_backtrace_internal (0, size, weight, vector, 0);
}

#endif
