AC_DEFUN([md_FIND_ZLIB],
[test -z "$zlibdir" && zlibdir=$defaultdir
md_FIND_LIB(zlib,z,,1)
if test -n "$zliblibdir"; then
  md_FIND_INCLUDE(zlib)
  ZLIBINCLUDES=$_includes
  ZLIBLDFLAGS=$_ldflags
  ZLIBLIB=-lz
fi
AC_SUBST(zliblibdir)
AC_SUBST(zlibincludedir)
AC_SUBST(ZLIBINCLUDES)
AC_SUBST(ZLIBLDFLAGS)
AC_SUBST(ZLIBLIB)
])

AC_DEFUN([md_FIND_PNG],
[test -z "$pngdir" && pngdir=$defaultdir
md_FIND_LIB(png,,,1)
if test -n "$pnglibdir"; then
  md_FIND_INCLUDE(png)
  if test "$zliblibdir"; then
    PNGINCLUDES=$_includes
    PNGLDFLAGS=$_ldflags
    PNGLIB=-lpng
    AC_DEFINE(HAVE_PNG,1,[define if PNG support is to be provided])
  fi
fi
AC_SUBST(pngincludedir)
AC_SUBST(pnglibdir)
AC_SUBST(PNGINCLUDES)
AC_SUBST(PNGLDFLAGS)
AC_SUBST(PNGLIB)
])
