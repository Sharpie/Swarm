AC_DEFUN(vj_FIND_JDK,
[if test -z "$jdkdir" ; then
  AC_PATH_PROG(JAVAC, javac, missing)
  if test $JAVAC != missing; then
    changequote(,)
    jdkdir=`echo $JAVAC | sed -e 's/\/javac$//' -e 's/\/[^/][^/]*$//'`
    changequote([,])
  fi
fi
AC_MSG_CHECKING(for JDK)
test -z "$jdkdir" && jdkdir=no
if test $jdkdir = no; then
  AC_MSG_RESULT(no)
  jdkdir=
  JAVASTUBS=
else
  if test -f $jdkdir/include/jni.h; then
    JAVAINCLUDES="-I$jdkdir/include -I$jdkdir/include/solaris -I$jdkdir/include/genunix"
  elif test -f $jdkdir/include/japhar/jni.h; then
    JAVAINCLUDES="-I$jdkdir/include/japhar"
  else
    AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_MSG_RESULT($jdkdir)
  AC_DEFINE(HAVE_JDK)
  JAVASTUBS=stubs
fi 

AC_SUBST(JAVASTUBS)
AC_SUBST(JAVAINCLUDES)
AC_SUBST(jdkdir)
])

