AC_DEFUN(md_CHECK_EXESUFFIX,
[AC_MSG_CHECKING(for executable suffix)
AC_TRY_COMPILE([#ifdef __CYGWIN32__
#error
#endif],[],
EXESUFFIX='',
EXESUFFIX=.exe)
if test -z "$EXESUFFIX" ; then
AC_MSG_RESULT(none)
else
AC_MSG_RESULT($EXESUFFIX)
fi
AC_SUBST(EXESUFFIX)
])
