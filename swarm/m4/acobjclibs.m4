AC_DEFUN(md_CHECK_OBJC_LIBS,
[AC_MSG_CHECKING(for Objective C libraries)
linked=no
ORIG_LIBS=$LIBS
ORIG_CC=$CC
OBJCLIBS=-lobjc
LIBS="$ORIG_LIBS $OBJCLIBS"
CC="$ORIG_CC -x objective-c"
AC_TRY_LINK([#include <objc/Object.h>], 
changequote(<,>)
[[Object alloc] init]
changequote([,]),linked=yes)

if test $linked = no ; then
OBJCLIBS='-lobjc -lpthread'
LIBS="$ORIG_LIBS $OBJCLIBS"
AC_TRY_LINK([#include <objc/Object.h>],
changequote(<,>)
[[Object alloc] init]
changequote([,]),linked=yes)

if test $linked = no ; then
AC_MSG_RESULT(no)
AC_MSG_ERROR(failed to link an Objective C program)
else
AC_MSG_RESULT($OBJCLIBS)
fi
else
AC_MSG_RESULT($OBJCLIBS)
fi
CC="$ORIG_CC"
AC_CHECK_FUNCS(objc_malloc, 
OBJC_MALLOC_OBJ=''
AC_DEFINE(HAVE_OBJC_MALLOC),
OBJC_MALLOC_OBJ=objc-malloc.lo)
AC_SUBST(OBJC_MALLOC_OBJ)
LIBS=$ORIG_LIBS
AC_SUBST(OBJCLIBS)
])
