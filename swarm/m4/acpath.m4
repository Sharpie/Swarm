AC_DEFUN(md_CHECK_PATH_SYNTAX,
[if test "$host_os" = cygwin; then
  PATHSEP=";"
  PATHDELIM='\'
  PATHEXPR='$(subst /,\,$(subst //%/,%:\,$<))'
  top_dossrcdir=`cd $srcdir; pwd | sed 's,//\([A-Za-z]\),\1:,'`  
else
  PATHSEP=:
  PATHDELIM=/
  PATHEXPR='$<'
  top_dossrcdir=$srcdir
fi
AC_SUBST(top_dossrcdir)
AC_SUBST(PATHSEP)
AC_SUBST(PATHDELIM)
AC_SUBST(PATHEXPR)
])
