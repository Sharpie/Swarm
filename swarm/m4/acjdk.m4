AC_DEFUN(vj_FIND_JDK,
[if test -z "$jdkdir" ; then
  AC_PATH_PROG(JAVAC, javac, missing)
  AC_PATH_PROG(JAR, jar, missing)
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
    AC_MSG_RESULT($jdkdir)
  else
    AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_DEFINE(HAVE_JDK)
  JAVASTUBS=stubs
  JAR=$jdkdir/bin/jar
  JAVAC=$jdkdir/bin/javac
  AC_SUBST(JAVAC)
  AC_SUBST(JAR)
fi 

JAVAINCLUDES="-I$jdkdir/include -I$jdkdir/include/solaris -I$jdkdir/include/genunix"
AC_SUBST(JAVASTUBS)
AC_SUBST(JAVAINCLUDES)
AC_SUBST(jdkdir)
])

