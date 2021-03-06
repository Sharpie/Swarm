AC_DEFUN([md_CHECK_WARNFLAGS],
[lastCFLAGS=$CFLAGS

AC_MSG_CHECKING(how to avoid implicit int return warning)
CFLAGS_NOWARN_IMPLICIT_INT=-Wno-implicit-int
CFLAGS="-Wall -Werror $CFLAGS_NOWARN_IMPLICIT_INT $CFLAGS"
AC_TRY_COMPILE([extern foobar ();],[],[],CFLAGS_NOWARN_IMPLICIT_INT=-Wno-implicit)
AC_MSG_RESULT($CFLAGS_NOWARN_IMPLICIT_INT)
AC_SUBST(CFLAGS_NOWARN_IMPLICIT_INT)

CFLAGS=$lastCFLAGS
AC_MSG_CHECKING(how to avoid unknown pragma warning)
CFLAGS_NOWARN_UNKNOWN_PRAGMAS=-Wno-unknown-pragmas
CFLAGS="-Wall -Werror $CFLAGS_NOWARN_UNKNOWN_PRAGMAS"
AC_TRY_COMPILE([#pragma ident],[],[],CFLAGS_NOWARN_UNKNOWN_PRAGMAS="")
AC_MSG_RESULT($CFLAGS_NOWARN_UNKNOWN_PRAGMAS)
AC_SUBST(CFLAGS_NOWARN_UNKNOWN_PRAGMAS)

CFLAGS=$lastCFLAGS
])
