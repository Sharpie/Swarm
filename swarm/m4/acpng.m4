AC_DEFUN(md_FIND_ZLIB,
[if test -z "$zlibdir" ; then 
  zlibdir=$prefix
fi
AC_MSG_CHECKING(directory of zlib library)
if test -f $zlibdir/lib/libz.a ; then
  AC_MSG_RESULT($zlibdir/lib)
else
  AC_MSG_RESULT(no)    
  AC_MSG_ERROR(Please use --with-zlib to specify location of zlib library.)
fi
AC_MSG_CHECKING(directory of zlib include file)
if test -f $zlibdir/include/zlib.h ; then
  AC_MSG_RESULT($zlibdir/include)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Please use --with-zlib to specify location of zlib header file.) 
fi
AC_SUBST(zlibdir)
])dnl

AC_DEFUN(md_FIND_PNG,
[if test -z "$pngdir" ; then 
  pngdir=$prefix
fi
AC_MSG_CHECKING(directory of png library)
if test -f $pngdir/lib/libpng.a ; then
  AC_MSG_RESULT($pngdir/lib)
else
  AC_MSG_RESULT(no)    
  AC_MSG_ERROR(Please use --with-png to specify location of png library.)
fi
AC_MSG_CHECKING(directory of png include file)
if test -f $pngdir/include/png.h ; then
  AC_MSG_RESULT($pngdir/include)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Please use --with-png to specify location of png header file.) 
fi
AC_SUBST(pngdir)
])dnl
