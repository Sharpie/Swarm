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
gnuwin32=no
if test $found = yes; then
  XPMLDFLAGS=$_ldflags
  XPMLIB=-l$xpmlibname
else
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
AC_SUBST(XPMINCLUDES)

if test "$gnuwin32" = no; then

last_cppflags=$CPPFLAGS
test -n "$xpmincludedir" && CPPFLAGS="-I$xpmincludedir $CPPFLAGS"
AC_MSG_CHECKING(for X11/xpm.h)
AC_TRY_COMPILE([#include <X11/xpm.h>],[],have_x11_xpm_h=yes,have_x11_xpm_h=no)
if test $have_x11_xpm_h = yes; then
  AC_DEFINE(HAVE_X11_XPM_H)
  AC_MSG_RESULT(yes)
else 
  AC_MSG_RESULT(no)
  AC_MSG_CHECKING(for xpm.h)
  AC_TRY_COMPILE([#include <xpm.h>],[],have_xpm_h=yes,have_xpm_h=no)
  if test $have_xpm_h = yes; then
    AC_DEFINE(HAVE_XPM_H)
    AC_MSG_RESULT(yes)
  else
    AC_MSG_ERROR(Cannot find xpm.h)
  fi
fi
CPPFLAGS=$last_cppflags

fi
])

AC_DEFUN(md_STRUCT_XPM_ALLOCPIXELS,
[last_cppflags=$CPPFLAGS
test -n "$xpmincludedir" && CPPFLAGS="-I$xpmincludedir $CPPFLAGS"
AC_MSG_CHECKING(for nalloc_pixels in XpmAttributes)
AC_TRY_COMPILE([#ifdef HAVE_X11_XPM_H 
#include <X11/xpm.h>
#elif defined(HAVE_XPM_H)
#include <xpm.h>
#else
#error
#endif], 
[XpmAttributes attr; attr.nalloc_pixels = 0;],
AC_MSG_RESULT(yes)
AC_DEFINE(HAVE_XPM_ALLOCPIXELS),
AC_MSG_RESULT(no))
CPPFLAGS=$last_cppflags
])

