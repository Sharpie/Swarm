AC_DEFUN(md_FIND_LIB,
[name=$1
 libname=$2
 AC_MSG_CHECKING(directory of lib${libname})
 for suffix in .so .a; do
  for dir in $$1dir $defaultdir /usr/lib /usr/local/lib; do
    if test -f $dir/lib/lib${libname}${suffix} ; then
      $1dir=$dir
      break
    else
      $1dir=''
    fi
  done
  test -z "$$1dir" || break
done
if test -z "$$1dir"; then
  AC_MSG_RESULT(no)    
  AC_MSG_ERROR(Please use --with-$1dir to specify location of $1 library.)
else
  AC_MSG_RESULT($$1dir/lib)
fi
if test "$suffix" = .so; then
  translit($1, `a-z', `A-Z')LDFLAGS="-L\$($1dir) -R$RPATH\$($1dir)"
else
  translit($1, `a-z', `A-Z')LDFLAGS='-L$($1dir)'
fi
AC_SUBST(translit($1, `a-z', `A-Z')LDFLAGS)
AC_SUBST($1dir)
])

AC_DEFUN(md_FIND_INCLUDE,
[if test -z "$$1includedir" ; then
  $1includedir=$$1dir/include
fi
AC_MSG_CHECKING(directory of $1.h)
if test -r $$1includedir ; then
  AC_MSG_RESULT($$1includedir)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Please use --with-$1includedir to specify location of $1.h)
fi
AC_SUBST($1includedir)
])

AC_DEFUN(md_FIND_ZLIB,
[test -z "$zlibdir" && zlibdir=$defaultdir
md_FIND_INCLUDE(zlib)
md_FIND_LIB(zlib,z)
])

AC_DEFUN(md_FIND_PNG,
[test -z "$pngdir" && pngdir=$defaultdir
md_FIND_INCLUDE(png)
md_FIND_LIB(png,png)
])
