AC_DEFUN([md_CHECK_SIZES],
[AC_CHECK_SIZEOF(int, 4)
AC_CHECK_SIZEOF(long, 4)
AC_CHECK_SIZEOF(long long, 8)
AC_CHECK_SIZEOF(void *, 4)
if test $ac_cv_sizeof_int = $ac_cv_sizeof_void_p; then
  ptrint=int
  ptruint=unsigned
  ptrintfmt='%d'
  ptruintfmt='%u'
elif test $ac_cv_sizeof_long = $ac_cv_sizeof_void_p; then
  ptrint=long
  ptruint='unsigned long'
  ptrintfmt='%ld'
  ptruintfmt='%lu'
elif test $ac_cv_sizeof_long_long = $ac_cv_sizeof_void_p; then
  ptrint='long long'
  ptruint='unsigned long long'
  ptrintfmt='%Ld'
  ptruintfmt='%Lu'
else
  AC_MSG_ERROR(Neither int nor long have the size of void *)
fi
AC_DEFINE_UNQUOTED(PTRINT,$ptrint,[integer comparable in size to a pointer])
AC_DEFINE_UNQUOTED(PTRUINT,$ptruint,[unsigned integer comparable in size to a pointer])
AC_DEFINE_UNQUOTED(PTRINTFMT,"$ptrintfmt",[format for integer comparable in size to a pointer])
AC_DEFINE_UNQUOTED(PTRUINTFMT,"$ptruintfmt",[format for unsigned integer comparable in size to a pointer])
])
