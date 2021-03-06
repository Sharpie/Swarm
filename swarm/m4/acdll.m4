AC_DEFUN([md_CHECK_DLL],
[AC_MSG_CHECKING(for DLL options)
AC_TRY_COMPILE([#ifdef __CYGWIN__
#error
#endif
],[],
[EXPORT_EXTERN=extern
EXPORT_EXTERNDEF=
IMPORT_EXTERN=extern
USEDLL=no
SWARMLIBS=-lswarm
AC_MSG_RESULT(using plain extern)],
[USEDLL=yes
SWARMLIBS='-lswarm'
EXPORT_EXTERN="extern __attribute__ ((dllexport))"
EXPORT_EXTERNDEF="__attribute__ ((dllexport))"
IMPORT_EXTERN="extern __attribute__ ((dllimport))"
AC_MSG_RESULT(using dllimport and dllexport)
])
AC_DEFINE_UNQUOTED(EXPORT_EXTERN,$EXPORT_EXTERN,[declaration for declaring exported DLL variables])
AC_DEFINE_UNQUOTED(EXPORT_EXTERNDEF,$EXPORT_EXTERNDEF,[declaration for defining exported DLL variables])
AC_DEFINE_UNQUOTED(IMPORT_EXTERN,$IMPORT_EXTERN,[declaration for importing exported DLL variables])
AC_PATH_PROG(DLLWRAP, dllwrap)
AC_SUBST(USEDLL)
AC_SUBST(SWARMLIBS)
AM_CONDITIONAL(USEDLL, test $USEDLL = yes)
])
