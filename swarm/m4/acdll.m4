AC_DEFUN(md_CHECK_DLLEXTERN,
[AC_MSG_CHECKING(for DLL options)
AC_TRY_COMPILE([#ifdef __CYGWIN__
#error
#endif
],[],
[AC_DEFINE(EXPORT_EXTERN,extern)
AC_DEFINE(IMPORT_EXTERN,extern)
AC_MSG_RESULT(using plain extern)],
[AC_DEFINE(EXPORT_EXTERN,__attribute__ ((dllexport)))
AC_DEFINE(IMPORT_EXTERN,extern __attribute__ ((dllimport)))
AC_MSG_RESULT(using dllimport and dllexport)])])
