AC_DEFUN(md_CREATE_LIBOBJC_HEADERS,
[configsrcdir=$srcdir/config

if test -d $configsrcdir/$host_cpu -a "`echo $configsrcdir/$host_cpu/*`" != "$host_cpu/*"; then
    cpudir=$configsrcdir/$host_cpu
else
    echo '*************** Warning ****************'
    echo libobjc backend information for $host_cpu is
    echo missing for libobjc.
    echo Using information from unknown.
    echo '****************************************'
    cpudir=unknown
fi

if test -f $cpudir/compiler-info-${target_os}.h; then
  COMPILERINFOHEADER="\${srcdir}/config/$host_cpu/compiler-info-${target_os}.h"
  AC_SUBST(COMPILERINFOHEADER)
else
  AC_MSG_ERROR(Could not find compiler information for $target_os)
fi

if test -f $cpudir/mframe-${target_os}.h; then
    ospath=$cpudir/mframe-${target_os}.h
    MFRAMEHEADER="\${srcdir}/config/$host_cpu/mframe-${target_os}.h"
    echo Using information from $MFRAMEHEADER
else
    ospath=$cpudir/mframe-generic.h
    echo '*************** Warning ****************'
    echo The mframe software has not been ported to $target_cpu-$target_os.
    echo Using information from $ospath.
    echo '****************************************'
    MFRAMEHEADER="\${srcdir}/$host_cpu/mframe-generic.h"
fi
AC_SUBST(MFRAMEHEADER)
])