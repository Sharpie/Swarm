AC_DEFUN(md_CHECK_DLL,
[AC_MSG_CHECKING(for DLL options)
AC_TRY_COMPILE([#ifdef __CYGWIN__
#error
#endif
],[],
[EXPORT_EXTERN=extern
EXPORT_EXTERNDEF=
IMPORT_EXTERN=extern
USEDLL=no
AC_MSG_RESULT(using plain extern)],
[USEDLL=yes
EXPORT_EXTERN="extern __attribute__ ((dllexport))"
EXPORT_EXTERNDEF="__attribute__ ((dllexport))"
IMPORT_EXTERN="extern __attribute__ ((dllimport))"
AC_MSG_RESULT(using dllimport and dllexport)
])
AC_DEFINE_UNQUOTED(EXPORT_EXTERN,$EXPORT_EXTERN)
AC_DEFINE_UNQUOTED(EXPORT_EXTERNDEF,$EXPORT_EXTERNDEF)
AC_DEFINE_UNQUOTED(IMPORT_EXTERN,$IMPORT_EXTERN)
AC_PATH_PROG(DLLWRAP, dllwrap)
if test "$dllwrap" != missing; then
  DLLWRAP="${DLLWRAP} --add-stdcall-alias --entry __cygwin_noncygwin_dll_entry@12"
fi
AC_SUBST(USEDLL)
AM_CONDITIONAL(USEDLL, test $USEDLL = yes)
])
