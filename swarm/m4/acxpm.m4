AC_DEFUN(md_FIND_XPM,
[test -z "$xpmlibdir" && xpmlibdir=$x_libraries
found=no
for name in $xpmlibname Xpm; do
  md_FIND_LIB(xpm,$name,,1)
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
  gnuwin32=no
AC_TRY_COMPILE([#ifdef __CYGWIN__
#error
#endif],[],,gnuwin32=yes)
  if test $gnuwin32 = yes; then
    XPMLDFLAGS=''
    XPMLIB=''
  else
    AC_MSG_ERROR(Please use --with-xpmlibdir to specify location of XPM.)
  fi
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
if test -f $xpmincludedir/X11/xpm.h; then
  AC_DEFINE(HAVE_X11_XPM_H)
elif test -f $xpmincludedir/xpm.h; then
  AC_DEFINE(HAVE_XPM_H)
else
  AC_MSG_ERROR(Cannot find xpm.h)
fi

AC_SUBST(XPMINCLUDES)
])

AC_DEFUN(md_STRUCT_XPM_ALLOCPIXELS,
[last_cppflags=$CPPFLAGS
CPPFLAGS="-I$xpmincludedir $CPPFLAGS"
AC_MSG_CHECKING(for nalloc_pixels in XpmAttributes)
AC_TRY_COMPILE([#include <X11/xpm.h>], 
[XpmAttributes attr; attr.nalloc_pixels = 0;],
AC_MSG_RESULT(yes)
AC_DEFINE(HAVE_XPM_ALLOCPIXELS),
AC_MSG_RESULT(no))
CPPFLAGS=$last_cppflags
])

