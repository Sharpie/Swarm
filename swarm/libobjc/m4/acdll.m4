AC_DEFUN(md_CHECK_DLLEXTERN,
[AC_MSG_CHECKING(for DLL options)
AC_TRY_COMPILE([#ifdef __CYGWIN__
#error
#endif
],[],
[EXPORT_EXTERN=extern
IMPORT_EXTERN=extern
AC_MSG_RESULT(using plain extern)],
[EXPORT_EXTERN="__attribute__ ((dllexport))"
IMPORT_EXTERN="extern __attribute__ ((dllimport))"
AC_MSG_RESULT(using dllimport and dllexport)])
AC_SUBST(EXPORT_EXTERN)
AC_SUBST(IMPORT_EXTERN)])
