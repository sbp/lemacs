/* In SunOS 4.1.1 the strcpy function references memory past the last byte of 
   the string!  This will core dump if the memory following the last byte is 
   not mapped.

   Here are correct versions by hbs@lucid.com.
*/

#define ALIGNED(x) (!(((unsigned long) (x)) & (sizeof (unsigned long) - 1)))

#define MAGIC    0x7efefeff
#define HIGH_BIT_P(c) ((c) & hi_bit)
#define HAS_ZERO(c) (((((c) + magic) ^ (c)) & not_magic) != not_magic)

char *
strcpy (char *to, const char *from)
{
  char *return_value = to;
  if (to == from)
    return to;
  else if (ALIGNED (to) && ALIGNED (from))
    {
      register unsigned long *to1 = (unsigned long *) to;
      register unsigned long *from1 = (unsigned long *) from;
      register unsigned long c;
      register unsigned long magic = MAGIC;
      register unsigned long not_magic = ~magic;
/*      register unsigned long hi_bit = 0x80000000; */

      while (c = *from1)
        {
          if (HAS_ZERO(c)) 
            {
              to = (char *) to1;
              from = (char *) from1;
              goto slow_loop;
            }
          else
            {
              *to1 = c;
              to1++; 
              from1++;
            }
        }

      to = (char *) to1;
      *to = (char) 0;
      return return_value;
    }
  else
    {
      char c;

    slow_loop:

      while (c = *from)
        {
          *to = c;
          to++;
          from++;
        }
      *to = (char) 0;
    }
  return return_value;
}


