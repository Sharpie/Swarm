AC_DEFUN(md_FIND_FFI,
[USE_FFCALL=0
if test -n "$with_ffidir" && "$with_ffidir" != no; then
  case $target_cpu in
    i?86|sparc)
      AC_MSG_WARN(Ignoring --with-ffidir: libffi is known to be less reliable than avcall on this platform.)
      with_ffidir=no
      ;;
    *)
      ffidir=$with_ffidir 
      ;;
  esac
else
  if test -n "$with_ffcalldir" ; then
    ffidir=$with_ffcalldir
    USE_FFCALL=1
  fi
fi

if test "$ffidir" ; then
  if test $USE_FFCALL = 0; then
    if test "$with_ffidir" != no ; then
      test -n "$ffidir" || ffidir=$defaultdir
      FFILDFLAGS=''
      AC_MSG_CHECKING(directory of libffi)
      for dir in $ffidir /usr ; do
        ffidir_expand=`eval echo $dir`
        if test -f $ffidir_expand/lib/libffi.so ; then
          FFILDFLAGS="-L\${ffilibdir} -R \${ffilibdir}"
          AC_MSG_RESULT($dir/lib/libffi.so)
          break
        else
          if test -f $ffidir_expand/lib/libffi.a ; then
            FFILDFLAGS='-L${ffilibdir}'
            AC_MSG_RESULT($dir/lib/libffi.a)
            break
          fi
        fi
      done
      if test -z "$FFILDFLAGS" ; then
        AC_MSG_RESULT(no)    
      else
        ffidir=$dir
        AC_MSG_CHECKING(directory of libffi include)
        if test -f $ffidir_expand/include/ffi.h ; then
          AC_MSG_RESULT($ffidir/include)
          FFILIB=-lffi
        else
          AC_MSG_RESULT(no)
        fi
      fi
    fi
  else 
    if test "$with_ffcalldir" != no; then
      AC_DEFINE(USE_AVCALL)
      FFILIB=-lavcall
      test -n "$ffidir" || ffidir=$defaultdir
      ffidir_expand=`eval echo $ffidir`
      AC_MSG_CHECKING(directory of libavcall.a)
      if test -f $ffidir_expand/lib/libavcall.a ; then
        FFILDFLAGS='-L${ffilibdir}'
        AC_MSG_RESULT($ffidir/lib)
      else
        AC_MSG_RESULT(no)    
        AC_MSG_ERROR(Please use --with-ffcalldir to specify location of avcall library.)
      fi
      AC_MSG_CHECKING(directory of avcall include)
      if test -f $ffidir_expand/include/avcall.h ; then
        AC_MSG_RESULT($ffidir/include)
      else
      AC_MSG_RESULT(no)
        AC_MSG_ERROR(Please use --with-ffcalldir to specify locatin of avcall header file.) 
      fi
    fi
  fi
fi
if test -n "$FFILIB"; then
  ffilibdir=$ffidir/lib
  if test $ffidir_expand = /usr; then
    FFIINCLUDES=''
  else
    FFIINCLUDES='-I${ffidir}/include'
  fi
  AM_CONDITIONAL(USEBUILTINAVCALL, false)
else
  AC_DEFINE(USE_AVCALL)
  ffidir=
  ffilibdir=
  FFIINCLUDES='-I$(top_builddir)/avcall'
  FFILDFLAGS=
  FFILIB="\$(top_builddir)/avcall/avcall.lo"
  if test "$host_os" = cygwin; then
    FFILIB="\$(top_builddir)/avcall/avcall-i386-msvc.lo ${FFILIB}"
  fi
  AM_CONDITIONAL(USEBUILTINAVCALL, true)
fi
AC_SUBST(ffidir)
AC_SUBST(ffilibdir)
AC_SUBST(FFIINCLUDES)
AC_SUBST(FFILDFLAGS)
AC_SUBST(FFILIB)
])
