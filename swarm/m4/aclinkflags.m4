AC_DEFUN(md_CHECK_LINKFLAGS,
[AC_MSG_CHECKING(for special linkflags)
AC_TRY_COMPILE([#ifdef __CYGWIN32__
#error
#endif
],[],
EXTRALDFLAGS="",
EXTRALDFLAGS=["-mwindows -Wl,--defsym,_WinMainCRTStartup=_mainCRTStartup"])
if test -z "$EXTRALDFLAGS" ; then
  AC_MSG_RESULT(none)
else
  AC_MSG_RESULT($EXTRALDFLAGS)
fi
AC_SUBST(EXTRALDFLAGS)
])
