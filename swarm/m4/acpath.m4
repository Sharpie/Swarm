AC_DEFUN([md_CHECK_PATH_SYNTAX],
[if test "$host_os" = cygwin; then
  PATHSEP=";"
  PATHDELIM='\'
  PATHEXPR='"$(shell cygpath -w $<)"'
  top_dossrcdir=`cygpath -w $srcdir`
else
  PATHSEP=:
  PATHDELIM=/
  PATHEXPR='$<'
  top_dossrcdir='$(top_srcdir)'
fi
AC_SUBST(top_dossrcdir)
AC_SUBST(PATHSEP)
AC_SUBST(PATHDELIM)
AC_SUBST(PATHEXPR)
])
