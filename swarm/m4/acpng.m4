AC_DEFUN(md_FIND_ZLIB,
[if test -z "$zlibdir" ; then 
  zlibdir=$defaultdir
fi
AC_MSG_CHECKING(directory of zlib library)
if test -f $zlibdir/lib/libz.a ; then
  AC_MSG_RESULT($zlibdir/lib)
else
  AC_MSG_RESULT(no)    
  AC_MSG_ERROR(Please use --with-zlibdir to specify location of zlib library.)
fi
AC_MSG_CHECKING(directory of zlib include file)
if test -f $zlibdir/include/zlib.h ; then
  AC_MSG_RESULT($zlibdir/include)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Please use --with-zlibdir to specify location of zlib header file.) 
fi
ZLIBLDFLAGS='-L$(zlibdir)'
AC_SUBST(zlibdir)
AC_SUBST(ZLIBLDFLAGS)
])dnl

AC_DEFUN(md_FIND_PNG,
[if test -z "$pngdir" ; then 
  pngdir=$defaultdir
fi
AC_MSG_CHECKING(directory of libpng.so)
if test -f $pngdir/lib/libpng.so ; then
  PNGLDFLAGS="-L\$(pngdir)/lib $RPATH\$(pngdir)/lib"
  AC_MSG_RESULT($pngdir/lib)
else
  AC_MSG_RESULT(no)
  AC_MSG_CHECKING(directory of libpng.a)
  if test -f $pngdir/lib/libpng.a ; then
    PNGLDFLAGS='-L$(pngdir)/lib'
    AC_MSG_RESULT($pngdir/lib)
  else
    AC_MSG_RESULT(no)    
    AC_MSG_ERROR(Please use --with-pngdir to specify location of png library.)
  fi
fi
AC_MSG_CHECKING(directory of png include file)
if test -f $pngdir/include/png.h ; then
  AC_MSG_RESULT($pngdir/include)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Please use --with-pngdir to specify location of png header file.) 
fi
AC_SUBST(pngdir)
AC_SUBST(PNGLDFLAGS)
])dnl
