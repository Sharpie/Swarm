AC_DEFUN(md_CHECK_PATHSYNTAX,
[if test "$host_os" = cygwin; then
  PATHSEP=";"
  PATHEXPR='$(subst /,\,$(subst //%/,%:\,$<))'
  top_dossrcdir=`cd $srcdir; pwd | sed 's,//\([A-Za-z]\),\1:,'`  
else
  PATHEXPR='$<'
  PATHSEP=":"
  top_dossrcdir=$srcdir
fi
AC_SUBST(top_dossrcdir)
AC_SUBST(PATHSEP)
AC_SUBST(PATHEXPR)
])
