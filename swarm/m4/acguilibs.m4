AC_DEFUN([md_CHECK_GUILIBS],
[AC_MSG_CHECKING(for GUI libs)
AC_TRY_COMPILE([#if defined(__CYGWIN__) || defined(__MINGW32__)
#error
#endif],[],
GUILIBS="-lX11 ${X_EXTRA_LIBS}",
GUILIBS='-lwinspool -lcomdlg32 -luser32 -lgdi32')
AC_MSG_RESULT($GUILIBS)
AC_SUBST(GUILIBS)
])
