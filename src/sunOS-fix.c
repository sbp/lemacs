/* If you are using SunOS 4.1.1 and X11r5, then you need this patch.
   There is a stupid bug in the SunOS libc.a: two functions which X11r5
   uses, mbstowcs() and wcstombs(), are unusable when programs are
   statically linked (as Emacs must be) because the static version of
   libc.a contains the *dynamic* versions of these functions.  These
   functions don't seem to be called when Emacs is running, so it's 
   enough to define stubs for them.

   This appears to be fixed in SunOS 4.1.2.
 */

#ifdef __STDC__

#include <stdlib.h>

size_t mbstowcs (wchar_t *foo, const char *bar, size_t baz)
{
  abort ();
  return 0;
}

size_t wcstombs (char *foo, const wchar_t *bar, size_t baz)
{
  abort ();
  return 0;
}

#else

void mbstowcs ()
{
  abort ();
}

void wcstombs ()
{
  abort ();
}

#endif
