AC_DEFUN(md_FIND_ZLIB,
[test -z "$zlibdir" && zlibdir=$defaultdir
md_FIND_INCLUDE(zlib)
md_FIND_LIB(zlib,z)
ZLIBINCLUDES=$_includes
ZLIBLDFLAGS=$_ldflags
AC_SUBST(zliblibdir)
AC_SUBST(zlibincludedir)
AC_SUBST(ZLIBINCLUDES)
AC_SUBST(ZLIBLDFLAGS)
])

AC_DEFUN(md_FIND_PNG,
[test -z "$pngdir" && pngdir=$defaultdir
md_FIND_INCLUDE(png)
md_FIND_LIB(png,png)
PNGINCLUDES=$_includes
PNGLDFLAGS=$_ldflags
AC_SUBST(pngincludedir)
AC_SUBST(pnglibdir)
AC_SUBST(PNGINCLUDES)
AC_SUBST(PNGLDFLAGS)
])
