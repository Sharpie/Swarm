AC_DEFUN(md_FIND_XPM,
[test -z "$xpmlibdir" && xpmlibdir=$x_libraries
found=no
for name in $xpmlibname Xpm; do
  md_FIND_LIB(xpm,$name,,)
  if test -n "$_ldflags" ; then
    xpmlibname=$name
    found=yes
    break
  fi
done
if test $found = yes; then
  XPMLDFLAGS=$_ldflags
  XPMLIB=-l$xpmlibname
else
  XPMLDFLAGS=''
  XPMLIB==''
fi

AC_SUBST(xpmlibdir)
AC_SUBST(xpmincludedir)
AC_SUBST(XPMLDFLAGS)
AC_SUBST(XPMLIB)
test -z "$xpmincludedir" && xpmincludedir="$x_includes"
if test "$xpmincludedir" = "$x_includes"; then
  XPMINCLUDES=''
else
  XPMINCLUDES='-I${xpmincludedir}'
fi
AC_SUBST(XPMINCLUDES)
])
