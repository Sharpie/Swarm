AC_DEFUN(md_FIND_FFI,
[USE_FFCALL=0
if test -n "$with_ffidir" ; then
  ffidir=$with_ffidir
else
  if test -n "$with_ffcalldir" ; then
    ffcalldir=$with_ffcalldir
    USE_FFCALL=1
  fi
fi

if test $USE_FFCALL = 0 ; then
  if test -z "$ffidir" ; then 
    ffidir=$prefix
  fi
  AC_MSG_CHECKING(directory of libffi library)
  if test -f $ffidir/lib/libffi.a ; then
    AC_MSG_RESULT($ffidir/lib)
  else
    AC_MSG_RESULT(no)    
    AC_MSG_ERROR(Please use --with-ffi to specify location of libffi library.)
  fi
  AC_MSG_CHECKING(directory of libffi include)
  if test -f $ffidir/include/ffi.h ; then
    AC_MSG_RESULT($ffidir/include)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR(Please use --with-ffi to specify locatin of libffi header file.) 
  fi
else
  AC_DEFINE(USE_FFCALL)
  if test -z "$ffidir" ; then 
    ffidir=$prefix
  fi
  AC_MSG_CHECKING(directory of avcall library)
  if test -f $ffidir/lib/libavcall.a ; then
    AC_MSG_RESULT($ffidir/lib)
  else
    AC_MSG_RESULT(no)    
    AC_MSG_ERROR(Please use --with-ffcalldir to specify location of avcall library.)
  fi
  AC_MSG_CHECKING(directory of avcall include)
  if test -f $ffidir/include/avcall.h ; then
    AC_MSG_RESULT($ffidir/include)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR(Please use --with-ffcalldir to specify locatin of avcall header file.) 
  fi
fi
AC_SUBST(ffidir)
])
