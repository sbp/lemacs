/*
 * cadillac-btl-process.c, Module CADILLAC
 *
 * ***************************************************************************
 *
 *        Copyright (C) 1990 by Lucid Inc.,  All Rights Reserved
 *
 * ***************************************************************************
 *
 * Code to turn log files into human readable form.
 *
 * Revision:	24-Jan-92 14:53:25
 *
 * Programmer: Harlan Sexton
 *
 * $Header: cadillac-btl-process.c,v 100.2 92/05/19 16:45:46 ctn Exp $
 *
 * Edit-History:
 *
 * Created:  5-Nov-90 by hbs
 *
 * End-of-Edit-History
 */
#include "cadillac-btl.h"

#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/time.h>

/* Unix System V release 4 application executables are in ELF format.LTL.*/
#ifdef NCR486
#include <elf.h>
#else
#include <a.out.h>
#endif   /* NCR486 */

#include <sys/resource.h>

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#include <alloca.h>
#endif

#define GIANT_BTL_DEPTH 10000000

typedef caddr_t btl_data_ptr;

#ifdef NCR486
struct symbol_node {
  char * sym_name;
  Elf32_Addr sym_value;
};
#endif  /* NCR486 */


struct lookup_node
{
  char *name;
  caddr_t start_addr;
  caddr_t end_addr;
  long id;
  long count;
};

struct call_tree_node
{
  struct lookup_node *funct;
  long count;
  struct call_tree_node *across;
  struct call_tree_node *down;
};

struct functions_table
{
  struct lookup_node *id_vector;
  long id_vector_length;
  struct lookup_node *ranges_vector;
  long ranges_vector_length;
};

struct btl_malloc_cell
{
  struct btl_malloc_cell *next;
};

struct btl_process_global_data_struct
{
  struct functions_table table_space;
  struct functions_table *table;

  struct btl_data_file_header data_header_space;
  struct btl_data_file_header *data_header;
  int data_fd;
  char *data_filename;

#ifdef NCR486
  Elf32_Ehdr exec_header_space;
  Elf32_Ehdr *exec_header;
#else
  struct exec exec_header_space;
  struct exec *exec_header;
#endif   /* NCR486 */

  int exec_fd;
  char *exec_filename;

  struct btl_malloc_cell *malloc_data;
  struct btl_malloc_cell *tree_data;

  struct call_tree_node root_node_space;
  struct call_tree_node *root;

  char *alternate_root_name;
  struct lookup_node *alternate_root;
  long alternate_root_count;

  double cutoff_percentage;
  int max_stack_depth;
  
  int collapse_tree;
};

struct btl_process_global_data_struct btl_gds;
struct btl_process_global_data_struct *btl_data;



/* constants that we use here and there */

#define BTL_ETEXT_NAME "_etext"
#define BTL_PC_UNDERFLOW_STRING "PC out of range -- too small"
#define BTL_PC_OVERFLOW_STRING "PC out of range -- too large"

struct lookup_node unknown_pc_node_space;
struct lookup_node *unknown_pc_node;

struct lookup_node unknown_id_node_space;
struct lookup_node *unknown_id_node;

struct lookup_node unknown_node_space;
struct lookup_node *unknown_lnode;



#define BTL_SEEK_CURRENT_POSITION -1 
#define BTL_SEEK_FILE_SIZE -2

/**********************************************************************/
/* struct definitions used only once or twice */

struct id_info_struct
{
  long max;
  long count;
};

struct lnode_info_struct
{
  struct lookup_node *base;
  struct lookup_node *limit;  
};

/**********************************************************************/

/* functions defined in this file */

static int comp_lnode (struct lookup_node *x, struct lookup_node *y);
static int comp_lnode_ptr (struct lookup_node **x, struct lookup_node **y);
static int comp_tnode_ptr 
  (struct call_tree_node **x, struct call_tree_node **y);
static void *btl_double_to_string (double arg, char *format_string);
static void btl_print 
  (int preceding_newlines, int pad, int error_flag, char *format_string, 
   void *arg1, void *arg2, void *arg3);
#ifdef NCR486
static int comp_symbol_node (struct symbol_node *x, struct symbol_node *y);
#else
static int comp_nlist (struct nlist *x, struct nlist *y);
#endif /* NCR486 */
static void btl_error (char *m, int use_perror, void *a1, void *a2, void *a3);
static void btl_warn (char *m, int use_perror, void *a1, void *a2, void *a3);
static void btl_check_write_dates (void);
static long btl_seek (int fd, long offset);
static void btl_read (int fd, char *buf, int n, char *reason_for_read);
static long btl_malloc (int size, int tree_data);
static char *btl_strcpy (char *string);
static char *btl_strcat (char *string1, char *string2);
static void btl_global_free ();
static void btl_preinit (void);
static void btl_open_data_file (char *datafile_name);
static void btl_open_exec_file (char *execfile_name);
static void btl_init_global_data (char *exec_file);
static struct lookup_node *init_r_nodes (long *r_length_ptr);

#ifdef EMACS_BTL
static int btl_get (Lisp_Object sym, struct id_info_struct *id_info);
static int btl_sym_to_lnode 
  (Lisp_Object sym, struct lnode_info_struct *lnode_info);
#endif

static struct lookup_node *init_id_nodes (long *id_length_ptr);
static struct lookup_node *btl_lookup (caddr_t pc, struct functions_table *table);
static void btl_build_call_tree (void);
static btl_data_ptr btl_add_backtrace_to_tree 
  (btl_data_ptr start, btl_data_ptr stop, int max_depth);
static btl_data_ptr btl_compute_pc_log_info 
  (btl_data_ptr start, btl_data_ptr stop);
static struct call_tree_node *btl_call_tree_node_assq 
  (struct lookup_node *lnode, struct call_tree_node *tnode);
static void btl_print_backtrace_log (void);
static int btl_sort_down_nodes_across (struct call_tree_node *parent, long cutoff);
static void btl_print_backtrace_log_internal
  (struct call_tree_node *tnode, long total_count, long cutoff, int depth);
static void btl_print_pc_log (void);


void cadillac_summarize_logging 
  (char *data_file, double cutoff, int depth, char *exec_file, 
   char *root, int collapse_tree);

/**********************************************************************/

/* compare functions */

static int comp_lnode (x, y)
     register struct lookup_node *x, *y;
{
  return (x->start_addr - y->start_addr);
}

static int comp_lnode_ptr (x, y)
     register struct lookup_node **x, **y;
{
  return ((*y)->count - (*x)->count);
}

static int comp_tnode_ptr (x, y)
     struct call_tree_node **x, **y;
{
  return ((*y)->count - (*x)->count);
}


#ifdef NCR486
static int comp_symbol_node (x, y)
     register struct symbol_node *x, *y;
{
  return (x->sym_value - y->sym_value);
}
#else
static int comp_nlist (x, y)
     register struct nlist *x, *y;
{
  return (x->n_value - y->n_value);
}
#endif /* NCR486 */


/**********************************************************************/

/* utility functions */

#define BTL_ALLOCA(pointer, size, type) \
{ \
  if (!size) \
    pointer = 0; \
  else \
    { \
      pointer = (type *) alloca (size); \
      if (!pointer) \
        btl_error ("Failure ALLOCA-ting %u bytes", 1, (void *) size, 0, 0); \
    } \
}

#define ROUND_UP(val, mod) ((1 + ((val)/(mod))) * (mod))

static void *btl_double_to_string (arg, format_string)
     double arg;
     char *format_string;
{
  static char double_buf[32];
  
  bzero (double_buf, 32);

  if (!format_string)
    format_string = "%.2f";

  sprintf (double_buf, format_string, arg);
  return (void *) double_buf;
}

#define PRINT_BUF_SIZE 1024

static void btl_print (preceding_newlines, pad, error_flag, format_string,
                       arg1, arg2, arg3)
     int preceding_newlines;
     int pad;
     int error_flag;
     char *format_string;
     void *arg1;
     void *arg2;
     void *arg3;
{
  char buf_space[PRINT_BUF_SIZE];
  char *pad_buf = 0;
  char *buf = buf_space;
  char *tmp = buf;
  
  bzero (buf, PRINT_BUF_SIZE);

  if ((pad || preceding_newlines) && !error_flag)
    {
      int i;
      int size = ROUND_UP (preceding_newlines + pad + 1, sizeof (long));
      char *tmp_pad;

      BTL_ALLOCA (pad_buf, size, char);
      bzero (pad_buf, size);

      tmp_pad = pad_buf;
    
      for (i = 0; i < preceding_newlines; i++) *tmp_pad++ = '\n';
      for (i = 0; i < pad; i++) *tmp_pad++ = ' ';
    }

  if (format_string) 
    sprintf (tmp, format_string, arg1, arg2, arg3);

  /* check to see that we haven't overrun the buffer allotted --
     if so, make the message be a short error message */
  while (*tmp++);
  if (tmp > buf + sizeof (buf_space))
    {
      char *tmp2 = buf_space;
      bzero (tmp2, PRINT_BUF_SIZE);
      sprintf (tmp2, 
               "\n\n\n     *** BTL Message %d bytes too long! ***\n\n\n", 
               (int) (tmp - buf));
      buf = tmp2;
      error_flag = 1;
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
      if (pad_buf)
        insert_before_markers (pad_buf, strlen (pad_buf));
      insert_before_markers (buf, strlen (buf));
    }
#else
  if (error_flag)
    {
      if (pad_buf) fprintf (stderr, pad_buf);
      fprintf (stderr, buf);
      fprintf (stderr, "\n");
    }
  else
    {
      if (pad_buf) fprintf (stdout, pad_buf);
      fprintf (stdout, buf);
    }
#endif

  return;
}

/* "fatal" error -- we are losing here, so quit in some appropriate way */
static void btl_error (m, use_perror, a1, a2, a3)
     char *m;
     int use_perror;
     void *a1, *a2, *a3;
{
  char buf[300];

  bzero (buf, 300);
  if (m) sprintf (buf, m, a1, a2, a3);

  if (use_perror)
    /* copy the system error message onto the buf, too */
    {
      extern int sys_nerr;
      extern char *sys_errlist[];
      extern int errno;
      char *system_error_message = 
        ((errno >=0) && (errno < sys_nerr))?
          sys_errlist[errno] : "Unknown type of system_error";
      char *buf2;

      buf2 = buf + strlen(buf);
      if (buf2 != buf) 
        {
          strcpy(buf2, " : ");
          buf2 += strlen(buf2);
        }
      strcpy(buf2, system_error_message);
    }

  /* we are giving up here, so flush everything and quit */
  if (btl_data->data_fd > 0) close (btl_data->data_fd);
  if (btl_data->exec_fd > 0) close (btl_data->exec_fd);
  btl_data->exec_fd = btl_data->data_fd = -1;

  btl_print (0, 0, 1, buf, 0, 0, 0);

#ifdef EMACS_BTL
  return;
#else
  exit(1);
#endif
}


/* non-fatal error */
static void btl_warn (m, use_perror, a1, a2, a3)
     char *m;
     int use_perror;
     void *a1, *a2, *a3;
{
  char *error_msg = "BTL warning";
  char buf[200];
  bzero (buf, 200);
  if (m) sprintf (buf, m, a1, a2, a3);

  if (use_perror) 
    /* get the system error message */
    {
      extern int sys_nerr;
      extern char *sys_errlist[];
      extern int errno;

      error_msg = 
        ((errno >=0) && (errno < sys_nerr))?
          sys_errlist[errno] : "Unknown type of system_error";
    }


  btl_print (0, 0, 0, "%.200s: %.200s\n", (void *) error_msg, (void *) buf, 0);

  return;
}

static void btl_check_write_dates ()
{
  int exec_fd = btl_data->exec_fd;
  int data_fd = btl_data->data_fd;
  int failed = 0;
  struct stat exec_buf;
  struct stat data_buf;

  if ((exec_fd > 0) && (data_fd > 0))
    {
      extern int errno;

      bzero ((char *) &exec_buf, sizeof (exec_buf));
      bzero ((char *) &data_buf, sizeof (data_buf));

      /* we probably ought to see why we failed here, if we do, but
         I'm too lazy to do this now */
      errno = 0;
      if (fstat (exec_fd, &exec_buf)) failed = 1;
      if (fstat (data_fd, &data_buf)) failed = 1;
      
      if (failed)
        {
          btl_print (0, 0, 0, "\n\n", 0, 0, 0);
          btl_warn 
            ("failed to get status info for data and/or exec files, %s and %s",
             1, 
             (void *) btl_data->data_filename, 
             (void *) btl_data->exec_filename,
             0);
          btl_print (0, 0, 0, "\n", 0, 0, 0);
        }
      else
        {
          if ((unsigned long) exec_buf.st_mtime >
              (unsigned long) data_buf.st_mtime)
            {
              btl_print (0, 0, 0, "\n\n", 0, 0, 0);
              btl_warn ("exec file %s is newer than data file %s",
                        0, 
                        (void *) btl_data->exec_filename,
                        (void *) btl_data->data_filename, 
                        0);
              btl_print (0, 0, 0, "\n", 0, 0, 0);
            }
        }
    }
        
}

static long btl_seek (fd, offset)
     int fd;
     long offset;
{
  long return_value;

  if (offset == BTL_SEEK_CURRENT_POSITION)
    return_value = (long) lseek (fd, 0L, L_INCR);
  else if (offset == BTL_SEEK_FILE_SIZE)
    {
      long current_value = (long) lseek (fd, 0L, L_INCR);
      return_value = (long) lseek (fd, 0L, L_XTND);
      lseek (fd, current_value, L_SET);
    }
  else
    return_value = (long) lseek (fd, offset, L_SET);
  if (return_value < 0)
    {
      char *file;
      if (fd == btl_data->exec_fd)
        file = btl_data->exec_filename;
      else if (fd == btl_data->data_fd)
        file = btl_data->data_filename;
      else
        file = "unknown file";
      btl_error ("Failed to do a seek to 0x%x in %.200s", 1,
                 (void *) offset, (void *) file, 0);
      return -1;
    }
  else
    return return_value;
}

static void btl_read (fd, buf, n, reason_for_read)
     int fd;
     char *buf;
     int n;
     char *reason_for_read;       
{
  int n_read;
  int remains = n;

  while (remains)
    {
      n_read = read (fd, buf, remains);
      if (n_read <= 0) btl_error (reason_for_read, 1, 0, 0, 0);
      buf += n_read;
      remains -= n_read;
    }
  return;
}

static long btl_malloc (size, tree_data)
     int size;
     int tree_data;
{
  register long result;

  if (!size) return 0;

  size += sizeof (struct btl_malloc_cell);

  result = (long) malloc (size);

  if (!result)
    btl_error ("virtual memory exhausted by request for %u bytes", 0,
               (void *) size, 0, 0);
  else
    bzero((char *) result, size);

  {
    /* push this cell on the linked list of global btl_data -- we
       will later cdr down this list freeing the elements */
    struct btl_malloc_cell *cell = (struct btl_malloc_cell *) result;
    result += sizeof (struct btl_malloc_cell);
    if (tree_data)
      {
        cell->next = btl_data->tree_data;
        btl_data->tree_data = cell;
      }
    else
      {
        cell->next = btl_data->malloc_data;
        btl_data->malloc_data = cell;
      }
  }
  /* result has been adjusted to point past the header we made */
  return result;
}

static char *btl_strcpy (string)
     char *string;
{
  char *new_string;
  
  if (!string) return 0;

  new_string = (char *) btl_malloc (strlen (string) + 1, 0);
  strcpy(new_string, string);
  return new_string;
}

static char *btl_strcat (string1, string2)
     char *string1, *string2;
{
  char *new_string;
  char *tmp_string;
  int len1;

  if (!(string1 && string2)) return 0;

  len1 = strlen (string1);
  new_string = (char *) btl_malloc (len1 + strlen (string2) + 1, 0);
  tmp_string = new_string;
  strcpy(tmp_string, string1);
  tmp_string += len1;
  strcpy(tmp_string, string2);

  return new_string;
}

static void btl_global_free ()
{
  struct btl_malloc_cell *cell;
  struct btl_malloc_cell *next;

  cell = btl_data->tree_data;
  while (cell)
    {
      next = cell->next;
      free ((void *) cell);
      cell = next;
    }
  btl_data->tree_data = 0;

  cell = btl_data->malloc_data;
  while (cell)
    {
      next = cell->next;
      free ((void *) cell);
      cell = next;
    }

  btl_data->malloc_data = 0;
  return;
}


/**********************************************************************/



/* "small" initialization functions */

/* this function wouldn't need to be called if C could initialize
   things right and Emacs didn't put all initialized data in read-only
   space during unexec */

static void btl_preinit ()
{
  /* if these things are still hanging around, close them now */
  if (btl_data && btl_data->data_fd > 0) close (btl_data->data_fd);
  if (btl_data && btl_data->exec_fd > 0) close (btl_data->exec_fd);

  /* set the data pointer entries to point at space reserved for them */
  bzero (&btl_gds, sizeof (btl_gds));
  btl_data = &btl_gds;
  btl_data->table = &(btl_data->table_space);
  btl_data->exec_header = &(btl_data->exec_header_space);
  btl_data->data_header = &(btl_data->data_header_space);
  btl_data->root = &(btl_data->root_node_space);

  {
    struct lookup_node tmp = {" <unknown PC> ", 0, 0, 0, 0};
    unknown_pc_node_space = tmp;
  }
  unknown_pc_node = &unknown_pc_node_space;

  {
    struct lookup_node tmp = {" <unknown ID> ", 0, 0, 0, 0};
    unknown_id_node_space = tmp;
  }
  unknown_id_node = &unknown_id_node_space;

  {
    struct lookup_node tmp = 
      {" <No information about this function> ", 0, 0, 0, 0};
    unknown_node_space = tmp;
  }
  unknown_lnode = &unknown_node_space;

  return;
}



static void btl_open_data_file (datafile_name)
     char *datafile_name;
{
  struct btl_data_file_header *header = btl_data->data_header;
  int fd = open(datafile_name, O_RDONLY, 0);
  char *exec_filename;

  if (fd < 0)
    btl_error ("Failure opening btl datafile %.200s", 1,
               (void *) datafile_name, 0, 0);

  btl_read (fd, (char *) header, sizeof (struct btl_data_file_header), 
            "reading data_file header");
  
  if (header->magic_number != BTL_DATA_MAGIC_NUMBER)
    btl_error ("%.200s is not a btl data file - it has a bad magic number", 0,
               (void *) datafile_name, 0, 0);
  
  if (header->size > sizeof (struct btl_data_file_header))
    {
      int str_size = header->size - sizeof (struct btl_data_file_header);
      BTL_ALLOCA (exec_filename, str_size, char);
      btl_read (fd, exec_filename, str_size, "reading exec filename");
      btl_data->exec_filename = btl_strcpy (exec_filename);
    }

#ifndef EMACS_BTL
  if (header->entry_format == BDF_ELISP_FORMAT)
    btl_error ("The btl datafile %.200s is ELISP format, \n\
which can only be processed from inside Emacs.", 1,
               (void *) datafile_name, 0, 0);
#endif

  btl_data->data_filename = datafile_name;

  if (btl_data->data_fd > 0) close (btl_data->data_fd);
  btl_data->data_fd = fd;

  { 
    int start = header->size;
    int end = btl_seek (fd, BTL_SEEK_FILE_SIZE);
    /* make sure that (end - start) is a multiple of the data elt size */
    if ((end - start) % CHARS_PER_BTL_DATA_ENTRY)
      btl_warn ("Got end = 0x%x, start = 0x%x, (end - start)%ENTRY_SIZE = %d", 
                0, (void *) end, (void *) start, 
                (void *) ((end - start) % CHARS_PER_BTL_DATA_ENTRY));
    end = start + (((end - start)/CHARS_PER_BTL_DATA_ENTRY) * 
                   CHARS_PER_BTL_DATA_ENTRY);

    header->start_offset = start;
    header->end_offset = end;
  }  
  return;
}

static void btl_open_exec_file (execfile_name)
     char *execfile_name;
{
  struct btl_data_file_header *data_header = btl_data->data_header;
#ifdef NCR486
  Elf32_Ehdr *header = btl_data->exec_header;
#else
  struct exec *header = btl_data->exec_header;
#endif
  int fd = open(execfile_name, O_RDONLY, 0);

  if (fd < 0)
    btl_error ("Failure opening btl exec file %.200s", 1,
               (void *) execfile_name, 0, 0);

#ifdef NCR486
  btl_read (fd, (char *) header, sizeof (Elf32_Ehdr),
            "reading exec_file header");

  /* Is execfile_name an ELF file? */
  if (header->e_ident[EI_MAG0]!=0x7f || header->e_ident[EI_MAG1]!='E' ||
      header->e_ident[EI_MAG2]!='L' || header->e_ident[EI_MAG3]!='F')
    btl_error
      ("%.200s is not an ELF file - it has a bad magic number = 0%o", 0,
       (void *) execfile_name, (void *) header->e_ident[EI_MAG0], 0);

  /* Is execfile_name an executable file? */
  if (header->e_type != ET_EXEC)
    btl_error
      ("%.200s is not an exec file - it has a wrong object type= 0%o", 0,
       (void *) execfile_name, (void *) header->e_type, 0);

  /* Is header version number current? */
  if (header->e_ident[EI_VERSION] != EV_CURRENT)
    btl_error
      ("%.200s header version number is not current - it has version number = 0%o", 0, (void *) execfile_name, (void *) header->e_ident[EI_VERSION], 0);

  /* Is execfile_name's class 32-bit object? */
  if (header->e_ident[EI_CLASS] != ELFCLASS32)
    btl_error
      ("%.200s class is not 32-bit object - it has class number = 0%o", 0,
       (void *) execfile_name, (void *) header->e_ident[EI_CLASS], 0);

  /* Does execfile_name have a section header table? */
  if (!header->e_shoff)
    btl_error
      ("%.200s does not have a section header table- it has an e_shoff = 0%o", 0
,
       (void *) execfile_name, (void *) header->e_shoff, 0);

#else
  btl_read (fd, (char *) header, sizeof (struct exec), 
            "reading exec_file header");
  
  if (header->a_magic != OMAGIC && header->a_magic != NMAGIC
      && header->a_magic != ZMAGIC)
    btl_error
      ("%.200s is not an exec file - it has a bad magic number = 0%o", 0,
       (void *) execfile_name, (void *) header->a_magic, 0);
#endif /* NCR486 */
  
  if (execfile_name != btl_data->exec_filename)
    btl_data->exec_filename = btl_strcpy (execfile_name);
  
  if (btl_data->exec_fd > 0) close (btl_data->exec_fd);
  btl_data->exec_fd = fd;

  btl_check_write_dates ();

  return;
}

/**********************************************************************/

/* "big" initialization functions */

static void btl_init_global_data (exec_file)
     char *exec_file;
{
  struct functions_table *table = btl_data->table;
  long r_length, id_length;

  btl_open_exec_file (exec_file);

  table->id_vector = init_id_nodes (&id_length);
  table->id_vector_length = id_length;
  table->ranges_vector = init_r_nodes (&r_length);
  table->ranges_vector_length = r_length;
  
  return;
}

/* used only by the function below */
#define SYMBOL_NAME(nlist_ptr) (strings + nlist_ptr->n_un.n_strx)

/* Given a pointer to long (in which to return the length), this function 
   returns a pointer to an array of lookup_node_X structs created 
   from the symbol-table information for the a.out file. It is assumed
   that the exec file has been opened and the btl_data global properly
   initialized. */ 

#ifdef NCR486
static struct lookup_node *init_r_nodes (r_length_ptr)
     long *r_length_ptr;
{
  struct lookup_node *r_nodes;
  int str_sec_hdr_ind, counter, Elf32_Sym_count;
  Elf32_Shdr *s_hdr_tbl;                             /* section header table */
  Elf32_Sym *all_symbols;                            /* symbol table */
  char *ass_str_tbl;                                 /* associated string table
*/
  struct symbol_node *symbol_node_tbl, *int_symbols; /* symbol node table */
  long r_length;

  if (btl_data->data_header->entry_format == BDF_ELISP_FORMAT)
    /* this means that we are actually processing a file that
       has nothing but EMACS_BTL data in it, so we ignore pc's */
    {
      *r_length_ptr = 0;
      return 0;
    }

  Elf32_Ehdr *header = btl_data->exec_header;
  int fd=btl_data->exec_fd;

  { /* read section header table */
    int s_hdr_tbl_sz;

    /* move file pointer to section header table*/
    btl_seek (fd, header->e_shoff);
    s_hdr_tbl_sz = header->e_shnum*header->e_shentsize;
    BTL_ALLOCA (s_hdr_tbl, s_hdr_tbl_sz, Elf32_Shdr);
    btl_read (fd, (char *) s_hdr_tbl, s_hdr_tbl_sz,
              "reading section header table");
  }

  { /* find symbol table section header (complete symbol table)
       SHT_SYMTAB section holds a symbol table.  Currently, an object file
       may have only ONE section of this type */
    Elf32_Shdr* s_hdr_ptr = s_hdr_tbl;
    int symbol_size;

    /* read symbol table and find the associated string table index */
    for (int i=0; i<(int)header->e_shnum ; i++) {
     if (s_hdr_ptr->sh_type == SHT_SYMTAB) {
       str_sec_hdr_ind = s_hdr_ptr->sh_link;
       symbol_size = s_hdr_ptr->sh_size;
       BTL_ALLOCA (all_symbols, symbol_size, Elf32_Sym);

       /* move fp to first byte in symbol table section  */
       btl_seek(fd, s_hdr_ptr->sh_offset);
       btl_read (fd, (char *) all_symbols, symbol_size,"reading symtab");
       Elf32_Sym_count = symbol_size/sizeof(Elf32_Sym);
       break;
     }
    s_hdr_ptr ++;
    }
  }

  { /* read string table */
    Elf32_Shdr *ass_str_tbl_hdr;

    ass_str_tbl_hdr = s_hdr_tbl + str_sec_hdr_ind;
    BTL_ALLOCA (ass_str_tbl, (int) ass_str_tbl_hdr->sh_size , char);
    btl_seek(fd, ass_str_tbl_hdr->sh_offset);
    btl_read(fd, (char *) ass_str_tbl, (int) ass_str_tbl_hdr->sh_size,
             "reading string table");
  }

  { /* write interesting symbols to symbol_node table */
    Elf32_Sym *current_Elf32_Sym_ptr = all_symbols;
    Elf32_Sym *end_Elf32_Sym_ptr = all_symbols + Elf32_Sym_count;

    BTL_ALLOCA (symbol_node_tbl, (Elf32_Sym_count*sizeof(struct symbol_node)),
                struct symbol_node);
    struct symbol_node *symbol_node_ptr = symbol_node_tbl;

    counter=0;
    while (current_Elf32_Sym_ptr < end_Elf32_Sym_ptr) {
      /* We don't want any symbols except those that are associated with FUNCTIO
NS */
      /* and BTL_ETEXT_NAME to mark the end of text symbols */
      if (current_Elf32_Sym_ptr->st_name) { /* symbol table entry has name */
        char *possible_name = ass_str_tbl + current_Elf32_Sym_ptr->st_name;
        if ((!strcmp(possible_name, BTL_ETEXT_NAME)) ||
            (ELF32_ST_TYPE(current_Elf32_Sym_ptr->st_info) == STT_FUNC))
        {
          /* interesting symbol */
          symbol_node_ptr->sym_name = btl_strcpy(possible_name);
          symbol_node_ptr->sym_value = current_Elf32_Sym_ptr->st_value;
          counter++;
          symbol_node_ptr++;
        }
      }
      current_Elf32_Sym_ptr++;

    }
  }

  { /* move all valid nodes from symbol_node_tbl to int_symbols */
    BTL_ALLOCA (int_symbols, (counter*sizeof(struct symbol_node)), 
		struct symbol_node);

    struct symbol_node *int_symbols_ptr = int_symbols;
    int cntdown=counter;

    while (cntdown--) *int_symbols_ptr++= *symbol_node_tbl++;
  }

  Elf32_Sym_count = counter;
  r_length = counter+1;

  /* sort the symbol_node structures vector by value */
  qsort (int_symbols, Elf32_Sym_count, sizeof (struct symbol_node), 
  	comp_symbol_node);

  /* make the r_nodes vector */
  r_nodes = (struct lookup_node *)
      btl_malloc (r_length * sizeof (struct lookup_node), 0);
  /* initialize the r_nodes vector */
  {
    struct symbol_node *current_nlist_ptr = int_symbols;
    struct symbol_node *last_nlist_ptr = int_symbols + (Elf32_Sym_count - 1);
    struct lookup_node *c_node = r_nodes + 1;

    /* loop through all but the last function setting the
       end_addr to be that start_addr of the next one */
    while (current_nlist_ptr < last_nlist_ptr)
      {
        c_node->name = current_nlist_ptr->sym_name;
        c_node->start_addr =
          (caddr_t) current_nlist_ptr->sym_value;
        c_node->end_addr =
          (caddr_t) (current_nlist_ptr + 1)->sym_value;
        c_node->id = 0;
        c_node++;
        current_nlist_ptr++;
      }
    /* for the last r_node, the end_addr is infinity = -1 */
    c_node->name = last_nlist_ptr->sym_name;
    c_node->start_addr =
      (caddr_t) last_nlist_ptr->sym_value;
    c_node->end_addr = (caddr_t) -1;
    c_node->id = 0;

    if (!strcmp (c_node->name, BTL_ETEXT_NAME))
      c_node->name = BTL_PC_OVERFLOW_STRING;

    /* for the first r_node, the start_addr is 0 */
    r_nodes->name = BTL_PC_UNDERFLOW_STRING;
    r_nodes->start_addr = 0;
    r_nodes->end_addr = (r_nodes + 1)->start_addr;
    r_nodes->id = 0;
  }

  /* if we need to, try to find the alternate_root */
  /* alternate_root will be a shorter lookup_node array starting
     "alternate_root_name" symbol */
  if (btl_data->alternate_root_name && !btl_data->alternate_root)
    {
      char *root_name = btl_data->alternate_root_name;
      struct lookup_node *c_node = r_nodes + 1;
      struct lookup_node *stop = r_nodes + r_length - 1;

      while (c_node < stop)
        {
          if (!strcmp (c_node->name, root_name))
            {
              btl_data->alternate_root = c_node;
              break;
            }
          else
            c_node++;
        }
    }

  *r_length_ptr = r_length;

  return r_nodes;


}
#else

static struct lookup_node *init_r_nodes (r_length_ptr)
     long *r_length_ptr;
{
  struct exec *header = btl_data->exec_header;
  int fd = btl_data->exec_fd;
  struct lookup_node *r_nodes;
  long r_length;
  long symbol_size = N_STROFF (*header) - N_SYMOFF (*header);
  long nlist_count = symbol_size / sizeof (struct nlist);
  struct nlist *all_symbols;
  struct nlist *symbols;
  long string_size;
  char *strings;
  
  if (btl_data->data_header->entry_format == BDF_ELISP_FORMAT)
    /* this means that we are actually processing a file that
       has nothing but EMACS_BTL data in it, so we ignore pc's */
    {
      *r_length_ptr = 0;
      return 0; 
    }

  /* read in the symbol table */
  if (!symbol_size)
    btl_error ("Exec file %.200s has no symbol table", 0,
               (void *) btl_data->exec_filename, 0, 0);
  BTL_ALLOCA (all_symbols, symbol_size, struct nlist);
  btl_seek (fd, N_SYMOFF(*header));
  btl_read (fd, (char *) all_symbols, symbol_size, 
            "reading exec file symbol table size");

  /* read in the string table */
  btl_read (fd, (char *) &string_size, sizeof(string_size), 
            "reading exec file string table size");
  if (!string_size)
    btl_error ("Exec file %.200s has no string table", 0,
               (void *) btl_data->exec_filename, 0, 0);
  BTL_ALLOCA (strings, string_size, char);
  bzero(strings, sizeof(string_size));
  btl_read (fd, strings + sizeof(string_size), 
            string_size - sizeof(string_size), 
            "reading exec file string table");

  /* put the names in the nlist structures that we are interested in */
  {
    struct nlist *current_nlist_ptr = all_symbols;
    struct nlist *end_nlist_ptr = all_symbols + nlist_count;
    int counter = 0;

    while (current_nlist_ptr < end_nlist_ptr)
      {
        int type = current_nlist_ptr->n_type;
        if (!(type & N_STAB) && ((type & N_TYPE) == N_TEXT))
          {
            /* we've filtered out the non_text symbols -- now we 
               try to get rid of the .o filenames, library names, 
               and the messages like "gcc_compiled." */
            char *possible_name = SYMBOL_NAME (current_nlist_ptr);

            {
              char *tmp = possible_name;
              char c;
              while ((c = *tmp++))
                {
                  if (c == '.')
                    {
                      possible_name = 0;
                      break;
                    }
                  
                }
            }
            
            if (!possible_name)
              current_nlist_ptr->n_un.n_name = 0;
            else if ((possible_name[0] == '-') || (possible_name[0] == '/'))
              current_nlist_ptr->n_un.n_name = 0;
            else
              {
                current_nlist_ptr->n_un.n_name = 
                  btl_strcpy(possible_name);
                counter++;
              }
          }
        else
          current_nlist_ptr->n_un.n_name = 0;
        current_nlist_ptr++;
      }
    
    /* set up the interesting symbols */
    BTL_ALLOCA (symbols, (counter * sizeof(struct nlist)), struct nlist);
    
    {
      struct nlist *current_all_ptr = all_symbols;
      struct nlist *end_all_ptr = all_symbols + nlist_count;
      struct nlist *current_n_ptr = symbols;
      struct nlist *end_n_ptr = symbols + counter;
      while (current_all_ptr < end_all_ptr)
        {
          if (current_all_ptr->n_un.n_name)
            if (current_n_ptr >= end_n_ptr)
              btl_error ("Ran out of room copying interesting text symbols", 0,
                         0, 0, 0);          
            else
              {
                *current_n_ptr = *current_all_ptr;
                current_n_ptr++;
              }
          current_all_ptr++;
        }
    
      if (current_n_ptr != end_n_ptr)
        btl_error ("Failed to copy all interesting text symbols", 0, 0, 0, 0); 
      
      nlist_count = counter;
      r_length = counter + 1;
    }
  }
  
  /* sort the nlist structures vector */
  qsort (symbols, nlist_count, sizeof (struct nlist), comp_nlist);

  /* make the r_nodes vector */
  r_nodes = (struct lookup_node *) 
    btl_malloc (r_length * sizeof (struct lookup_node), 0);
  /* initialize the r_nodes vector */
  {
    struct nlist *current_nlist_ptr = symbols;
    struct nlist *last_nlist_ptr = symbols + (nlist_count - 1);
    struct lookup_node *c_node = r_nodes + 1;
    /* loop through all but the last function setting the
       end_addr to be that start_addr of the next one */
    while (current_nlist_ptr < last_nlist_ptr)
      {
        c_node->name = current_nlist_ptr->n_un.n_name;
        c_node->start_addr = 
          (caddr_t) current_nlist_ptr->n_value;
        c_node->end_addr = 
          (caddr_t) (current_nlist_ptr + 1)->n_value;
        c_node->id = 0;
        c_node++;
        current_nlist_ptr++;
      }
    /* for the last r_node, the end_addr is infinity = -1 */
    c_node->name = last_nlist_ptr->n_un.n_name;
    c_node->start_addr = 
      (caddr_t) last_nlist_ptr->n_value;
    c_node->end_addr = (caddr_t) -1;
    c_node->id = 0;
    if (!strcmp (c_node->name, BTL_ETEXT_NAME))
      c_node->name = BTL_PC_OVERFLOW_STRING;

    /* for the first r_node, the start_addr is 0 */
    r_nodes->name = BTL_PC_UNDERFLOW_STRING;
    r_nodes->start_addr = 0;
    r_nodes->end_addr = (r_nodes + 1)->start_addr;
    r_nodes->id = 0;
  }

  /* if we need to, try to find the alternate_root */
  if (btl_data->alternate_root_name && !btl_data->alternate_root)
    {
      char *root_name = btl_data->alternate_root_name;
      struct lookup_node *c_node = r_nodes + 1;
      struct lookup_node *stop = r_nodes + r_length - 1;

      while (c_node < stop)
        {
          if (!strcmp (c_node->name, root_name))
            {
              btl_data->alternate_root = c_node;
              break;
            }
          else
            c_node++;
        }
    }

  *r_length_ptr = r_length;
  return r_nodes;
}

#endif  /* NCR486 */

#ifdef EMACS_BTL
#include "btl-get.h"

/* use this function either to get the id of a symbol or to map over
   all symbols counting the number that have id's */
static int btl_get (sym, id_info)
     Lisp_Object sym;
     struct id_info_struct *id_info;
{
  extern Lisp_Object Qnil;
  extern Lisp_Object VBTL_id_tag;
  register Lisp_Object tag = VBTL_id_tag;
  Lisp_Object id;
  int foundp = 0;
  long id_value = 0;
  
  BTL_GET (sym, tag, id, foundp);

  if (foundp)
    {
      id_value = XINT(id);

      if ((XTYPE (id) != Lisp_Int) || (id_value <= 0))
        btl_error ("Bad id = 0x%x = %d on Elisp symbol \"%.200s\"", 0,
                   (void *) XFASTINT(id),
                   (void *) XINT(id),
                   (void *) (XSYMBOL(sym)->name)->data);
      else if (id_info)
        {
          long idmax = id_info->max;
          id_info->max = (idmax >= id_value)?idmax:id_value;
          id_info->count++;
        }
    }

  return id_value;
}

/* initialize an lnode from an Elisp symbol having an id */
static int btl_sym_to_lnode (sym, lnode_info)
     Lisp_Object sym;
     struct lnode_info_struct *lnode_info;
{
  long id_value = btl_get (sym, 0);
  
  if (id_value)
    {
      struct lookup_node *lnode = lnode_info->base + id_value;  
      struct lookup_node *bound = lnode_info->limit;  
      if (lnode < bound)
        {
          if (lnode->id)
            btl_error 
              ("Reused lnode entry %.200s for symbol \"%.200s\", id = %d", 0,
               lnode->name, (void *) (XSYMBOL(sym)->name)->data,
               (void *) id_value);
          else
            {
              char *name = (char *)(XSYMBOL(sym)->name)->data;

              lnode->name = btl_strcat ("#'", name);
              lnode->id = id_value;
              lnode->count = 0;
              lnode->start_addr = 0;
              lnode->end_addr = 0;

              /* if we need to, check to see if this is the alternate_root
                 name */
              if (btl_data->alternate_root_name && 
                  !btl_data->alternate_root &&
                  !strcmp (name, btl_data->alternate_root_name))
                btl_data->alternate_root = lnode;
            }
        }
      else
        btl_error 
          ("Ran off end of lnode array on symbol \"%.200s\", id = %d", 0,
           (void *) (XSYMBOL(sym)->name)->data,
           (void *) id_value, 0);
    }

  return 0;
}


/* Given a pointer to long (in which to return the length), this function 
   returns a pointer to an array of lookup_node_X structs created 
   from the Vobarry table maintained by Elisp. */ 
static struct lookup_node *init_id_nodes (id_length_ptr)
     long *id_length_ptr;
{
  extern void map_obarray ();
  extern Lisp_Object Vobarray;
  long id_length;
  long id_max;
  struct lookup_node *id_nodes;

  if (btl_data->data_header->entry_format != BDF_ELISP_FORMAT)
    /* this means that we are actually processing a file that
       doesn't have any EMACS_BTL data in it, just pc info */
    {
      *id_length_ptr = 0;
      return 0; 
    }

  /* find out the max_id and length of the id_vector that we need */
  {
    struct id_info_struct id_info = {0,0};
    map_obarray (Vobarray, btl_get, &id_info);
    id_length = id_info.max + 1;
  }

  /* make the id_nodes */
  id_nodes = (struct lookup_node *) 
    btl_malloc (id_length * sizeof (struct lookup_node), 0);

  /* initialize the id_nodes */
  {
    struct lnode_info_struct lnode_info = {id_nodes, id_nodes + id_length};
    /* or lnode.current = id_nodes; lnode.limit = id_nodes + id_length; */
    map_obarray (Vobarray, btl_sym_to_lnode, &lnode_info);
  }
       
  *id_length_ptr = id_length;
  return id_nodes;
}
#else
static struct lookup_node *init_id_nodes (id_length_ptr)
     long *id_length_ptr;
{
  *id_length_ptr = 0;
  return 0; 
}
#endif

/**********************************************************************/

static struct lookup_node *btl_lookup (pc, table)
     caddr_t pc;
     struct functions_table *table;
{
  long id = (long) pc;

  if (pc && table->ranges_vector)
    /* by default we use the pc */
    {
      struct lookup_node *range;
      struct lookup_node *vec = table->ranges_vector;
      long lo = 0;
      long hi = table->ranges_vector_length - 1;
      long index = hi / 2;

      /* do a binary search, starting with the "hashed" index */
      while (lo <= hi)
        {
          range = vec + index;

          if (pc < range->start_addr)
            hi = index - 1;
          else if (pc >= range->end_addr)
            lo = index + 1;
          else
            return range;

          index = (lo + hi) / 2;
        }
      return unknown_pc_node;
    }
  /* if there is no pc ranges stuff we try it as an -- id's must be > 0 */
  else if (id && table->id_vector)
    {
      if ((id < table->id_vector_length) && (id > 0))
        {
          struct lookup_node *node = table->id_vector + id;
          if (node->id == id)
            return node;
          else
            return unknown_id_node;
        }
      else
        return unknown_id_node;
    }
  else 
    return unknown_lnode;
}

static void btl_build_call_tree ()
{
  int page_size = getpagesize ();
  struct btl_data_file_header *header = btl_data->data_header;
  long size = header->end_offset;
  long map_size = ROUND_UP (size, page_size);
  caddr_t base_addr;
  btl_data_ptr stop;
  btl_data_ptr start;
  int max_depth = btl_data->max_stack_depth;
  unsigned long mmap_upper_limit;
  struct rlimit rls;

  /* we have to make sure that we aren't overshooting the data segment
     limit, and since the man page for mmap fails to mention this,
     it took me a long time to find this out */
  getrlimit (RLIMIT_DATA, &rls);
  mmap_upper_limit = rls.rlim_max;

  /* pick some reasonable place for the base_addr */
  base_addr = 
    (caddr_t)
      (page_size * 
       ((mmap_upper_limit - ((unsigned long) sbrk(0))) / (2 * page_size)));

  /* put the data fd back to 0, just because I don't know if it matters */
  btl_seek (btl_data->data_fd, 0);
  base_addr = 
    mmap (base_addr, map_size, PROT_READ, MAP_PRIVATE, btl_data->data_fd, 0);
  
  if ((-1 == (long) base_addr) || !base_addr)
    btl_error ("MMAP of log data file %.200s failed", 1,
               btl_data->data_filename, 0, 0);

  start = (btl_data_ptr)((base_addr + size)- CHARS_PER_BTL_DATA_ENTRY);
  stop = (btl_data_ptr )
    ((base_addr + header->start_offset) - CHARS_PER_BTL_DATA_ENTRY);

  switch (btl_data->data_header->log_type)
    {
    case BACKTRACE_LOGGING:
      while (start > stop) 
        { 
          start = btl_add_backtrace_to_tree (start, stop, max_depth);
        };
      btl_print_backtrace_log ();
      break;
    case PC_LOGGING:
      btl_compute_pc_log_info (start, stop);
      btl_print_pc_log ();
      break;
    default:
      break;
    }
  
  if (-1 == (long) munmap (base_addr, map_size))
    btl_warn
      ("MUNMAP of %u bytes at address 0x%x for data file %.200s failed", 1,
       (void *) map_size,
       (void *) base_addr, 
       (void *) btl_data->data_filename);
 
}

#define NEXT_FRAME(start, stop, pc) \
{ \
  start -= CHARS_PER_BTL_DATA_ENTRY; \
  if (start <= stop) return start; \
  pc = BDE_PC(start); \
}

static btl_data_ptr btl_add_backtrace_to_tree (start, stop, max_depth)
     btl_data_ptr start;
     btl_data_ptr stop;
     int max_depth;
{
  caddr_t pc = BDE_PC(start);
  struct functions_table *table = btl_data->table;
  struct call_tree_node *root = btl_data->root;
  struct call_tree_node *tnode = root;
  struct lookup_node *lnode;
  /* current depth down from "_start" (the call tree root) */
  int counter = 0;
  int backtrace_weight = 1;

  /* skip over any delimiters */
  while (!pc) NEXT_FRAME (start, stop, pc);

  if (BDE_WEIGHTED_P(start))
    {
      backtrace_weight = BDE_WEIGHT(start);
      start -= BDE_WSIZE;
      if (start <= stop) 
        return start;
      pc = BDE_PC(start);
    }

  /* increment total number of backtraces */
  root->count += backtrace_weight;

  /* if we have an alternate_root, skip up backtrace looking for it as a
     place to start */
  if (btl_data->alternate_root)
    {
      struct lookup_node *root_lnode = btl_data->alternate_root;

      while (pc)
        {
          lnode = btl_lookup (pc, table);
          if (lnode == root_lnode) 
            {
              btl_data->alternate_root_count += backtrace_weight;
              goto basic_loop;
            }
          else
            /* otherwise  go to the next frame up */
            NEXT_FRAME (start, stop, pc);
        }

      /* we didn't find the root in this backtrace, so we just return */
      /* Since start is now a delimiter frame, we "skip it" in the 
         return value */
      return start - CHARS_PER_BTL_DATA_ENTRY;
    }

 basic_loop:

  while (pc)
    {
      if (++counter <= max_depth)
        {
          lnode = btl_lookup (pc, table);
          /* find the next tnode, consing one if necessary */
          tnode = btl_call_tree_node_assq (lnode, tnode);
          tnode->count += backtrace_weight;
        }

      /* now go to the next frame up */
      NEXT_FRAME (start, stop, pc);
    }

  /* start is now the first delimiter frame, so start+1 is the last
     non-delimiter frame; that is, start+1 is the leaf node */
  pc = BDE_PC(start + CHARS_PER_BTL_DATA_ENTRY);
  lnode = btl_lookup (pc, table);
  /* increment the counter for the leaf node, to give pc_logging style
     information, too*/
  lnode->count += backtrace_weight;

  /* Since start is a now delimiter frame, we "skip it" in the return value */
  return start - CHARS_PER_BTL_DATA_ENTRY;
}

static btl_data_ptr btl_compute_pc_log_info (start, stop)
    btl_data_ptr start;
    btl_data_ptr stop;
{
  caddr_t pc = BDE_PC(start);
  struct functions_table *table = btl_data->table;
  struct call_tree_node *root = btl_data->root;
  struct lookup_node *lnode;

  while (start > stop)
    {
      lnode = btl_lookup (pc, table);
      /* increment the counter for this node */
      lnode->count++;
      /* total number of pc's collected */
      root->count++;

      NEXT_FRAME (start, stop, pc);
    }
  return start;
}


/* does an ASSQ on a tnode looking for a subordinate tnode that 
   has an lnode matching the lnode argument */
   
static struct call_tree_node *btl_call_tree_node_assq (lnode, tnode)
  struct lookup_node *lnode;
  struct call_tree_node *tnode;
{
  struct call_tree_node *subnode = tnode->down;

  while (subnode)
    {
      if (lnode == subnode->funct)
        return subnode;
      else
        subnode = subnode->across;
    }
  
  subnode = (struct call_tree_node *) 
    btl_malloc (sizeof (struct call_tree_node), 1);
  subnode->funct = lnode;
  subnode->across = tnode->down;
  tnode->down = subnode;
  return subnode;
}



static void btl_print_backtrace_log ()
{
  struct call_tree_node *tnode = btl_data->root;
  long total_count = btl_data->root->count;
  long cutoff;

  btl_print (0, 0, 0, "\n\n\n\n* Backtrace logging information for %.200s.\n",
             (void *) btl_data->exec_filename, 0, 0);
      
  if (btl_data->alternate_root)
    {
      btl_print (0, 0, 0, 
                 "* The function %.200s is being used as used as the root \
of the call tree.\n\
* Of %u total backtraces in the log file, %u contained the root.\n",
                 (void *) btl_data->alternate_root_name, 
                 (void *) total_count, 
                 (void *) btl_data->alternate_root_count);

      total_count = btl_data->alternate_root_count;
    }

  cutoff = (long) ((total_count * btl_data->cutoff_percentage)/100.0);

  btl_print (0, 0, 0,
             "* Using %u backtraces -- data from log file %.200s.\n\
* The format below is Depth: Function <pcnt>, where \n\
* <pcnt> is the percentage of all backtraces hitting Function.\n\
* Note: Subtrees of percentage less than %s ",
             (void *) total_count, 
             (void *) btl_data->data_filename,
             btl_double_to_string (btl_data->cutoff_percentage, 0));

  if (btl_data->max_stack_depth == GIANT_BTL_DEPTH)
    btl_print (0, 0, 0, "are excluded.\n\n", 0, 0, 0);
  else
    btl_print (0, 0, 0, "or depth more than %d %.200sare excluded.\n\n",  
               (void *) btl_data->max_stack_depth,
               (void *) (btl_data->alternate_root)?"from the root ":"", 
               (void *) 0);

  btl_print_backtrace_log_internal (tnode, total_count, cutoff, 1);
  btl_print_pc_log ();
  return;
}


/* used only by btl_print_backtrace_log_internal (), just below */
static int 
btl_sort_down_nodes_across (struct call_tree_node *parent, long cutoff)
{
  int count = 0;
  int return_count = 0;
  struct call_tree_node *tmp = parent->down;

  while (tmp)
    {
      count++;
      if (tmp->count >= cutoff)
        return_count++;
      tmp = tmp->across;
    }

  if (count <= 1)
    return return_count;
  else
    {
      struct call_tree_node **results_array;
      int results_array_length = count;
      int i = 0;

      BTL_ALLOCA (results_array, (results_array_length * sizeof (tmp)), 
                  struct call_tree_node *);

      tmp = parent->down;
      while (tmp)
        {
          results_array[i++] = tmp;
          tmp = tmp->across;
        }

      qsort (results_array, results_array_length, 
             sizeof (tmp), comp_tnode_ptr);

      parent->down = results_array[0];

      for (i = 0; i < results_array_length - 1; i++)
        {
          results_array[i]->across = results_array[i+1];
          results_array[i+1]->across = 0;
        }
      return return_count;
    }
}

static void btl_print_backtrace_log_internal(tnode, total_count, cutoff, depth)
     struct call_tree_node *tnode;
     long total_count;
     long cutoff;
     int depth;
{
  int child_count;
  struct call_tree_node *child;
  int spacing;

  /* I think that we may be losing because of stack_overflow, so I am
     changing the code to not recurse when there is only one down node */

 start_btl_print:

  child_count = btl_sort_down_nodes_across (tnode, cutoff);
  child = tnode->down;

  if (child_count == 0)
    return;
  else if (child_count == 1)
    {
      if (btl_data->collapse_tree)
        {
          tnode = child;
          goto start_btl_print;
        }

      if (child->count >= cutoff)
        {
          double percent = (100.0 * child->count)/total_count;
          btl_print (1, 2 * depth, 0, "%2d: %.200s     %s", 
                     (void *) depth, 
                     (void *) child->funct->name, 
                     btl_double_to_string (percent, 0));
          tnode = child;
          depth++;
          goto start_btl_print;
        }
      else
        return;
    }
  else
    {
      while (child)
        {
          if (child->count >= cutoff)
            {
              double percent = (100.0 * child->count)/total_count;
              btl_print (1, 2 * depth, 0, "%2d: %.200s     %s", 
                         (void *) depth, 
                         (void *) child->funct->name, 
                         btl_double_to_string (percent, 0));
              btl_print_backtrace_log_internal 
                (child, total_count, cutoff, depth + 1);
            }
          child = child->across;
        }
      return;
    }
}

#define CONSIDER_RESULT(ptr, bound, lnode) \
{ if (lnode->count && (ptr < bound)) *ptr++ = lnode; }

static void btl_print_pc_log ()
{
  int counter = 0;
  struct lookup_node *start;
  struct lookup_node *end;
  struct lookup_node **results_array;
      
  /* zero these guys, too */
  if (unknown_lnode->count) counter++;
  if (unknown_id_node->count) counter++;
  if (unknown_pc_node->count) counter++;

  start = btl_data->table->id_vector;
  end = start + btl_data->table->id_vector_length;
  while (start < end)
    {
      if (start->count) counter++;
      start++;
    }

  start = btl_data->table->ranges_vector;
  end = start + btl_data->table->ranges_vector_length;
  while (start < end)
    {
      if (start->count) counter++;
      start++;
    }
  
  BTL_ALLOCA (results_array, (counter * sizeof (start)), struct lookup_node *);
  {
    struct lookup_node **result = results_array;
    struct lookup_node **end_result = results_array + counter;

    CONSIDER_RESULT (result, end_result, unknown_lnode);
    CONSIDER_RESULT (result, end_result, unknown_id_node);
    CONSIDER_RESULT (result, end_result, unknown_pc_node);

    start = btl_data->table->id_vector;
    end = start + btl_data->table->id_vector_length;
    while (start < end)
      {
        CONSIDER_RESULT (result, end_result, start);
        start++;
      }

    start = btl_data->table->ranges_vector;
    end = start + btl_data->table->ranges_vector_length;
    while (start < end)
      {
        CONSIDER_RESULT (result, end_result, start);
        start++;
      }
  }

  qsort (results_array, counter, sizeof (start), comp_lnode_ptr);

  {
    long total_count = btl_data->root->count;
    long cutoff;
    int i;

    btl_print (0, 0, 0, "\n\n\n\n\n* Leaf node information for %.200s.\n",
               (void *) btl_data->exec_filename, 0, 0);

    if (btl_data->alternate_root)
      {
        btl_print (0, 0, 0, 
                   "* The function %.200s is being used as used as the root \
of the call tree.\n\
* Of %u total samples in the log file, %u contained the root.\n\
* Only leaf nodes underneath this root are represented in this data.\n",
                   (void *) btl_data->alternate_root_name, 
                   (void *) total_count, 
                   (void *) btl_data->alternate_root_count);

        total_count = btl_data->alternate_root_count;
      }

    cutoff = (long) ((btl_data->cutoff_percentage * total_count)/100.0);

    btl_print (0, 0, 0, "* Using %u samples -- data from log file %.200s.\n\
* The format below is <pcnt> Function, where \n\
* <pcnt> is the percentage of all samples having Function as a leaf.\n\
* Note: Functions of percentage less than %s are excluded.\n\n",
               (void *) total_count,
               (void *) btl_data->data_filename,
               btl_double_to_string (btl_data->cutoff_percentage, 0));

    for (i = 0; i < counter; i++)
      {
        struct lookup_node *lnode = results_array[i];
        double percent = (100.0 * lnode->count)/total_count;
        
        if (lnode->count >= cutoff)
          btl_print (0, 0, 0, " %s  %.200s\n", 
                     btl_double_to_string (percent, "%6.2f"),
                     (void *) lnode->name, 
                     0);
        else
          break;
      }

    btl_print (0, 0, 0, "\n\n", 0, 0, 0);
  }
  return;
}


/* user function */

void cadillac_summarize_logging (data_file, cutoff, depth, 
                                 exec_file, root, collapse_tree)
     char *data_file;
     double cutoff;
     int depth;
     char *exec_file;
     char *root;
     int collapse_tree;
{
  btl_preinit ();

  if (!data_file) return;
  btl_open_data_file (data_file);

  if (!exec_file) exec_file = btl_data->exec_filename;
  if (!exec_file) 
    btl_error ("For logfile %.200s, no exec file is known.", 0,
               data_file, 0, 0);

  btl_data->exec_filename = exec_file = btl_strcpy (exec_file);
  btl_data->data_filename = btl_strcpy (data_file);
  btl_data->alternate_root_name = btl_strcpy (root);
  btl_data->alternate_root = 0;
  btl_data->alternate_root_count = 0;
  btl_data->collapse_tree = collapse_tree;

  /* go off and initialize whatever needs it */
  btl_init_global_data (exec_file);
  if (depth < 0)
    btl_data->max_stack_depth = GIANT_BTL_DEPTH;
  else
    btl_data->max_stack_depth = 
      ((depth > 0) && (depth < 10000))?depth:10;
  btl_data->cutoff_percentage = 
    ((cutoff >= 0.0) && (cutoff < 100.0))?(cutoff):2.0;

  /* now we are ready to process the data_file */
  btl_build_call_tree ();

  /* if these things are still hanging around, close them now */
  if (btl_data && (btl_data->data_fd > 0))
    {
      close (btl_data->data_fd);
      btl_data->data_fd = -1;
    }
  if (btl_data && (btl_data->exec_fd > 0))
    {
      close (btl_data->exec_fd);
      btl_data->exec_fd = -1;
    }

#ifdef EMACS_BTL
  /* free any global data that might have persisted from the last
     invocation -- this only happens if this code is running inside 
     something like Emacs, not when it is a standalone process */
  btl_global_free ();
#endif

  return;
}

