AC_DEFUN(vj_FIND_JDK,
[AC_PATH_PROG(JAVAC, javac, missing)
AC_MSG_CHECKING(for JDK)
if test -z "$jdkdir" ; then
  if test $JAVAC != missing; then
    changequote(,)
    jdkdir=`echo $JAVAC | sed -e 's/\/javac$//' -e 's/\/[^/][^/]*$//'`
    changequote([,])
  fi
fi
if test -z "$jdkdir"; then
  AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
else
  if test "$jdkdir" = no; then
    AC_MSG_RESULT(no)
    jdkdir=
    JAVASTUBS=
  else
    AC_MSG_RESULT($jdkdir)
    JAVASTUBS=stubs
    test $JAVAC = missing && JAVAC=$jdkdir/bin/javac
  fi
fi
AC_SUBST(JAVASTUBS)
AC_SUBST(jdkdir)
])

