AC_DEFUN(md_CHECK_POINTER_FMT,
AC_MSG_CHECKING(for 0x prefix from %p)
AC_TRY_RUN([
#include <stdio.h>

main ()
{
  changequote(<,>)dnl
  char buf[16];

  sprintf (buf, "%p", &buf);

  exit (!(buf[0] == '0' && buf[1] == 'x'));
  changequote([,])dnl
}
],
AC_DEFINE(POINTER_FMT_HEX_PREFIX) AC_MSG_RESULT(yes),
AC_MSG_RESULT(no),
AC_MSG_RESULT(guessing no)
))

