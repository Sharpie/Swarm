AC_DEFUN(md_CHECK_OBJC_LIBS,
[AC_MSG_CHECKING(for Objective C libraries)
ORIG_LIBS=$LIBS
ORIG_CC=$CC
CC="$OBJC -x objective-c"
last_ac_link=$ac_link
ac_link='${CC-cc} -o conftest${ac_exeext} $CFLAGS $CPPFLAGS $LDFLAGS conftest.$ac_ext -x none $LIBS 1>&AC_FD_CC' 
for OBJCLIBS in -lobjc '-lobjc -lpthread' '-lobjc -lposix4' '-lobjc -lpthread -lposix4' ; do
  linked=no
  LIBS="$ORIG_LIBS $OBJCLIBS"
  AC_TRY_LINK([#include <objc/Object.h>], 
  changequote(<,>)
  [[Object alloc] init]
  changequote([,]),linked=yes)
  test $linked = no || break
done

if test $linked = no ; then
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(failed to link an Objective C program)
else
  AC_MSG_RESULT($OBJCLIBS)
fi
CC=$ORIG_CC
ac_link=$last_ac_link
AC_CHECK_FUNCS(objc_malloc, 
OBJC_MALLOC_OBJ=''
AC_DEFINE(HAVE_OBJC_MALLOC),
OBJC_MALLOC_OBJ=objc-malloc.lo)
AC_SUBST(OBJC_MALLOC_OBJ)
LIBS=$ORIG_LIBS
AC_SUBST(OBJCLIBS)
])
