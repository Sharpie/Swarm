AC_DEFUN(md_CHECK_LINKFLAGS,
[AC_MSG_CHECKING(for special linkflags)
AC_TRY_COMPILE([#ifdef __CYGWIN32__
#error
#endif
],[],
extraldflags="",
extraldflags=["-mwindows -Wl,--defsym,_WinMainCRTStartup=_mainCRTStartup"])
LDFLAGS="$LDFLAGS $extraldflags"
if test -z "$extraldflags" ; then
  AC_MSG_RESULT(none)
else
  AC_MSG_RESULT($extraldflags)
fi
])
