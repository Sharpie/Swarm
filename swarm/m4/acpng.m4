AC_DEFUN(md_FIND_LIB,
[name=$1
libname=$2
AC_MSG_CHECKING(directory of lib${libname})
for dir in $$1dir/lib $defaultdir/lib /usr/lib /usr/local/lib; do
  for suffix in .so .a; do
    if test -r $dir/lib${libname}${suffix} ; then
      $1libdir=$dir
      break
    else
      $1libdir=''
    fi
  done
  test -z "$$1libdir" || break
done
if test -z "$$1libdir"; then
  AC_MSG_RESULT(no)    
  AC_MSG_ERROR(Please use --with-$1dir to specify location of $1.)
else
  AC_MSG_RESULT($$1libdir)
fi
if test "$suffix" = .so; then
  _ldflags="-L\$($1libdir) $RPATH\$($1libdir)"
else
  _ldflags='-L$($1libdir)'
fi
])

AC_DEFUN(md_FIND_INCLUDE,
[if test -z "$$1includedir" ; then
  $1includedir=$$1dir/include
fi
AC_MSG_CHECKING(directory of $1.h)
for dir in $$1includedir /usr/include; do
  if test -r $dir/$1.h ; then
    $1includedir=$dir
    break
  else
    $1includedir=''
  fi
done
if test -n "$$1includedir"; then
  AC_MSG_RESULT($$1includedir)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Please use --with-$1includedir to specify location of $1.h)
fi
if test "$$1includedir" = /usr/include; then
  _includes=''
else
  _includes=-I$$1includedir
fi
])

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
