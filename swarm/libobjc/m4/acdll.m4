AC_DEFUN(md_CHECK_DLL,
[AC_MSG_CHECKING(for DLL options)
AC_TRY_COMPILE([#ifdef __CYGWIN__
/* #error */
#endif
],[],
[EXPORT_EXTERN=extern
EXPORT_EXTERNDEF=
IMPORT_EXTERN=extern
USEDLL=no
AC_MSG_RESULT(using plain extern)],
[EXPORT_EXTERN="extern __attribute__ ((dllexport))"
EXPORT_EXTERNDEF="__attribute__ ((dllexport))"
IMPORT_EXTERN="extern __attribute__ ((dllimport))"
USEDLL=yes
AC_MSG_RESULT(using dllimport and dllexport)])
AC_SUBST(EXPORT_EXTERN)
AC_SUBST(EXPORT_EXTERNDEF)
AC_SUBST(IMPORT_EXTERN)
AC_PATH_PROG(DLLWRAP, dllwrap, missing)
AC_SUBST(USEDLL)
])
