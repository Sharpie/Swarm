AC_DEFUN(md_CHECK_long_long_printf,
[AC_MSG_CHECKING(how to print long long)
AC_CACHE_VAL(swarm_cv_printf_ll_fmt,
for swarm_cv_printf_ll_fmt in l L q ll unknown; do
AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
main ()
{
  char *s = malloc (128);
  long long x = (long long) 1048576 * (long long) 1048576;

  sprintf (s,"%${swarm_cv_printf_ll_fmt}d",x);
  exit (strcmp (s, "1099511627776"));
}
],
break,,break)
done)
AC_MSG_RESULT(%${swarm_cv_printf_ll_fmt}d and %${swarm_cv_printf_ll_fmt}u)
AC_DEFINE_UNQUOTED(LLFMT,"$swarm_cv_printf_ll_fmt")
AC_CHECK_SIZEOF(long, 4)
AC_CHECK_SIZEOF(long long, 8)
])
