AC_DEFUN([md_CHECK_POINTER_FMT],
[AC_MSG_CHECKING(for 0x prefix from %p)
AC_TRY_RUN([
#include <stdio.h>

main ()
{
  char buf[16];

  sprintf (buf, "%p", &buf);

  exit (!(*buf == '0' && *(buf + 1) == 'x'));
}
],
[fmt='%p'; AC_MSG_RESULT(yes)],
[fmt='0x%p'; AC_MSG_RESULT(no)],
[fmt='0x%p'; AC_MSG_RESULT(guessing no)])
AC_DEFINE_UNQUOTED(PTRHEXFMT,"$fmt",[define as format to use for hex pointer])
])


