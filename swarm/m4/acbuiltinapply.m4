AC_DEFUN([md_CHECK_BUILTIN_APPLY],
[AC_MSG_CHECKING(for working __builtin_apply)
AC_TRY_RUN([
int exit_code=0;
int arg4 = 4;
int stack_size = 32;
void *buf , *res;

void called(int arg)
{
if ( arg == arg4 ) exit_code=0;
}

int callme(int arg)
{
buf = __builtin_apply_args();
res = __builtin_apply(called,buf,stack_size);
__builtin_return(res);
}


main()
{
exit_code=1;
if (arg4 != callme(arg4)) exit_code=1;
return(exit_code);
}
],
[AC_MSG_RESULT(yes)],
[AC_DEFINE(BUGGY_BUILTIN_APPLY,1,[define if __builtin_apply is buggy]) AC_MSG_RESULT(no)],
[AC_MSG_RESULT(guessing yes)]
)])
