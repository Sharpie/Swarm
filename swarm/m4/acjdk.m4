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
    JAVACLASSES='${jdkdir}/lib/classes.zip'
    JAVA='${jdkdir}/bin/java'
  elif test -f $jdkdir/include/japhar/jni.h; then
    JAVAINCLUDES="-I$jdkdir/include/japhar"
    JAVACLASSES="`$jdkdir/bin/japhar-config info datadir`"
    JAVA='${jdkdir}/bin/japhar'
  elif test -f $jdkdir/include/kaffe/jni.h ; then
    JAVAINCLUDES="-I$jdkdir/include/kaffe"
    JAVACLASSES="$datadir/kaffe/Klasses.jar:$datadir/kaffe/pizza.jar"
    JAVA='${jdkdir}/bin/kaffe'
  else
    AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_MSG_RESULT($jdkdir)
  AC_DEFINE(HAVE_JDK)
  JAVASTUBS=stubs
fi 

AC_SUBST(JAVASTUBS)
AC_SUBST(JAVAINCLUDES)
AC_SUBST(JAVACLASSES)
AC_SUBST(JAVA)
AC_SUBST(jdkdir)
])

