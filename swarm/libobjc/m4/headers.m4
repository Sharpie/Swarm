AC_DEFUN(md_CREATE_LIBOBJC_HEADERS,
[configsrcdir=$srcdir/config

case "$host_cpu" in
  i486 | i586 | i686) host_cpu=i386 ;;
  hppa1.1 | hppa2.0) host_cpu=hppa ;;
  alphaev56) host_cpu=alpha ;;
esac

case "$target_os" in
  solaris2.5.1 | solaris2.7) target_os=solaris2 ;;
  linux) target_os=linux-gnu ;;
  irix6.5) target_os=irix6 ;;
  hpux9* hpux10*) target_os=hpux ;;
esac


if test -d $configsrcdir/$host_cpu -a "`echo $configsrcdir/$host_cpu/*`" != "$host_cpu/*"; then
    cpudir=$configsrcdir/$host_cpu
    cpu=$host_cpu
else
    echo '*************** Warning ****************'
    echo libobjc backend information for $host_cpu is
    echo missing for libobjc.
    echo Using information from unknown.
    echo '****************************************'
    cpudir=$configsrcdir/unknown
    cpu=unknown
fi

if test -f $cpudir/compiler-info-${target_os}.h; then
  COMPILERINFOHEADER="\${top_srcdir}/config/$cpu/compiler-info-${target_os}.h"
else
  COMPILERINFOHEADER="\${top_srcdir}/config/$cpu/compiler-info-generic.h"
  echo '*************** Warning ****************'
  echo The mframe software has not been ported to $target_cpu-$target_os.
  echo Using information from $COMPILERINFOHEADER
  echo '****************************************'
fi
AC_SUBST(COMPILERINFOHEADER)

if test -f $cpudir/mframe-${target_os}.h; then
  MFRAMEHEADER="\${top_srcdir}/config/$cpu/mframe-${target_os}.h"
else
  MFRAMEHEADER="\${top_srcdir}/config/$cpu/mframe-generic.h"
  echo '*************** Warning ****************'
  echo The mframe software has not been ported to $target_cpu-$target_os.
  echo Using information from $MFRAMEHEADER
  echo '****************************************'
fi
AC_SUBST(MFRAMEHEADER)
])
