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
AC_DEFINE(EXPORT_EXTERN,$EXPORT_EXTERN)
AC_DEFINE(EXPORT_EXTERNDEF,$EXPORT_EXTERNDEF)
AC_DEFINE(IMPORT_EXTERN,$IMPORT_EXTERN)
AC_PATH_PROG(DLLWRAP, dllwrap, missing)
AC_SUBST(USEDLL)
AM_CONDITIONAL(USEDLL, test $USEDLL = yes)
])
