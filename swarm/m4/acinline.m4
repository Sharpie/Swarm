AC_DEFUN(md_CHECK_INLINING,
[AC_MSG_CHECKING(for GCC inlining bug)
AC_TRY_RUN([inline void
fuzz (double __y)
{
  long long int __p = __y;
  if (__p == 0)
    return;
  if (__p < 0)
    __p = 0;
}
int
main ()
{
  double nodedy[2];
  fuzz (0.5);
  exit (0);
}
],AC_MSG_RESULT(no),
AC_MSG_RESULT(yes)
CFLAGS="$CFLAGS -fno-inline",AC_MSG_RESULT(guessing no))])
