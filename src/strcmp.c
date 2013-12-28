/* In SunOS 4.1.1 the strcmp and strncmp functions reference memory
   past the last byte of the string! This will core dump if the memory 
   following the last byte is not mapped.

   Here are correct versions by hbs@lucid.com.
*/

#include <string.h>
#define ALIGNED(x) (!(((unsigned long) (x)) & (sizeof (unsigned long) - 1)))

#define MAGIC    0x7efefeff
#define HIGH_BIT_P(c) ((c) & hi_bit)
#define HAS_ZERO(c) (((((c) + magic) ^ (c)) & not_magic) != not_magic)

int
strcmp (const char *x, const char *y)
{
  if (x == y)
    return 0;
  else if (ALIGNED (x) && ALIGNED (y))
    {
      register unsigned long *x1 = (unsigned long *) x;
      register unsigned long *y1 = (unsigned long *) y;
      register unsigned long c;
      register unsigned long magic = MAGIC;
      register unsigned long not_magic = ~magic;
      register unsigned long hi_bit = 0x80000000;

      while ((c = *x1) == *y1)
        {
          if (HAS_ZERO(c)) 
            {
              if (!HIGH_BIT_P (c))
                return 0;
              else
                {
                  x = (char *) x1;
                  y = (char *) y1;
                  goto slow_loop;
                }
            }
              
          x1++; 
          y1++;
        }

      x = (char *) x1;
      y = (char *) y1;
      goto slow_loop;
    }
  else
    {
      char c;

    slow_loop:

      while ((c = *x) == *y)
        {
          if (c == (char) 0) return 0;
          x++; 
          y++;
        }
      return (*x - *y);
    }
}


int
strncmp (const char *x, const char *y, size_t n)
{
  if ((x == y) || (n <= 0))
    return 0;
  else if (ALIGNED (x) && ALIGNED (y))
    {
      register unsigned long *x1 = (unsigned long *) x;
      register unsigned long *y1 = (unsigned long *) y;
      register unsigned long c;
      register unsigned long magic = MAGIC;
      register unsigned long not_magic = ~magic;
      register unsigned long hi_bit = 0x80000000;

      while ((c = *x1) == *y1)
        {
          n -= sizeof (unsigned long);
          if (n <= 0)
            return 0;
          
          if (HAS_ZERO(c)) 
            {
              if (!HIGH_BIT_P (c))
                return 0;
              else
                {
                  x = (char *) x1;
                  y = (char *) y1;
                  goto slow_loop;
                }
            }
              
          x1++; 
          y1++;
        }

      x = (char *) x1;
      y = (char *) y1;
      goto slow_loop;
    }
  else
    {
      char c;

    slow_loop:

      while ((c = *x) == *y)
        {
          n--;
          if (n <= 0)
            return 0;
          if (c == (char) 0) 
            return 0;
          x++; 
          y++;
        }
      return (*x - *y);
    }
}
