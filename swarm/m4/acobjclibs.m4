AC_DEFUN(md_CHECK_OBJC_LIBS,
[AC_MSG_CHECKING(for Objective C libraries)
linked=no
ORIG_LIBS=$LIBS
LIBS="$ORIG_LIBS -lobjc"
AC_TRY_LINK([#include <objc/Object.h>], 
changequote(<,>)
[[Object alloc] init]
changequote([,]),,linked=yes)

if test $linked = no ; then
LIBS="$ORIG_LIBS -lobjc -lpthread"
AC_TRY_LINK([#include <objc/Object.h>],
changequote(<,>)
[[Object alloc] init]
changequote([,]),,linked=yes)

if test $linked = no ; then
AC_MSG_RESULT(no)
AC_MSG_ERROR(failed to link an Objective C program)
else
AC_MSG_RESULT(-lobjc -lpthread)
fi
else
AC_MSG_RESULT(-lobjc)
fi
])