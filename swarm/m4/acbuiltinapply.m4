AC_DEFUN(md_CHECK_BUILTIN_APPLY,
AC_MSG_CHECKING(for working __builtin_apply)
AC_TRY_RUN([
void *buf;
int exit_code = 0;

void callme (void)
{
  buf = __builtin_apply_args ();
  exit_code = 0;
}


main ()
{
  callme ();
  exit_code = 1;
  __builtin_apply (callme, buf, 0);
  exit (exit_code);
}
],
AC_MSG_RESULT(yes),
AC_DEFINE(BUGGY_BUILTIN_APPLY) AC_MSG_RESULT(no),
AC_MSG_RESULT(guessing yes)
))