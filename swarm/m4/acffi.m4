AC_DEFUN(md_FIND_FFI,
[USE_FFCALL=0
if test -n "$with_ffidir" ; then
  ffidir=$with_ffidir
else
  if test -n "$with_ffcalldir" ; then
    ffidir=$with_ffcalldir
    USE_FFCALL=1
  fi
fi

if test $USE_FFCALL = 0 ; then
  if test -z "$ffidir" ; then 
    ffidir=$defaultdir
  fi
  FFILDFLAGS=''
  AC_MSG_CHECKING(directory of libffi)
  for dir in /usr $ffidir ; do
    if test -f $dir/lib/libffi.so ; then
      FFILDFLAGS="-L\${dir}/lib $RPATH\${dir}/lib"
      AC_MSG_RESULT($dir/lib/libffi.so)
      break
    else
      if test -f $dir/lib/libffi.a ; then
        FFILDFLAGS='-L${dir}/lib'
        AC_MSG_RESULT($dir/lib/libffi.a)
        break
      fi
    fi
  done
  if test -z "$FFILDFLAGS" ; then
    AC_MSG_RESULT(no)    
    AC_MSG_ERROR(Please use --with-ffidir to specify location of libffi library.)
  else
    ffidir=$dir
  fi
  AC_MSG_CHECKING(directory of libffi include)
  if test -f $ffidir/include/ffi.h ; then
    AC_MSG_RESULT($ffidir/include)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR(Please use --with-ffidir to specify locatin of libffi header file.) 
  fi
  FFILIB=-lffi
else
  AC_DEFINE(USE_AVCALL)
  FFILIB=-lavcall
  if test -z "$ffidir" ; then 
    ffidir=$defaultdir
  fi
  AC_MSG_CHECKING(directory of libavcall.a)
  if test -f $ffidir/lib/libavcall.a ; then
    FFILDFLAGS='-L${ffidir}/lib'
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
if test $ffidir = /usr; then
  FFIINCLUDES=''
else
  FFIINCLUDES=-I$ffidir/include
fi
AC_SUBST(FFIINCLUDES)
AC_SUBST(FFILDFLAGS)
AC_SUBST(FFILIB)
])
