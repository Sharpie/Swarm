AC_DEFUN(md_CHECK_OBJCMALLOC,
[AC_CHECK_LIB(objc,objc_malloc)
AC_CHECK_FUNCS(objc_malloc, OBJC_MALLOC='', OBJC_MALLOC=objc-malloc.lo)])
