AC_DEFUN([md_FIND_LIB],
[libname=$2
if test -z "$libname" ; then
  libname=$1
fi
AC_MSG_CHECKING(directory of lib${libname})
_libdir="$$1libdir"
if test "$$1dir" != no ; then
test -z "$_libdir" && _libdir=$$1dir/lib
for dir in $3 $_libdir $defaultdir/lib /usr/lib32 /usr/lib /usr/local/lib; do
  expand_dir=`eval echo $dir`
  for suffix in .dylib .so .sl .dll.a .a; do
    if test -r $expand_dir/lib${libname}${suffix} ; then
      $1libdir=$dir
      break
    else
      $1libdir=''
    fi
  done
  test -z "$$1libdir" || break
done
fi
if test -z "$$1libdir"; then
  AC_MSG_RESULT(no)    
  if test -z "$4"; then
    AC_MSG_ERROR(Please use --with-$1dir to specify location of $1.)
  fi
else
  AC_MSG_RESULT($$1libdir)
fi
if test -n "$$1libdir" ; then
  if test "$$1libdir" != /usr/lib; then
    if test $suffix = .so || test $suffix = .sl ; then
      _ldflags="-L\${$1libdir} -R \${$1libdir}"
    else
      _ldflags='-L${$1libdir}'
    fi
  else
    _ldflags=''
  fi
else
  _ldflags=''
fi
])

AC_DEFUN([md_FIND_INCLUDE],
[test -n "$$1includedir" || $1includedir=$$1dir/include
AC_MSG_CHECKING(directory of $1.h)
for dir in $$1includedir /usr/include /usr/local/include; do
 expand_dir=`eval echo $dir`
 if test -r $expand_dir/$1.h ; then
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
expand_dir=`eval echo $$1includedir`
if test "$expand_dir" = /usr/include; then
  _includes=''
else
  _includes='-I${$1includedir}'
fi
])
