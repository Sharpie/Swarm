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
    JAVAINCLUDES="-I$jdkdir/include"
    if test -f $jdkdir/include/solaris/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkdir/include/solaris"
    else
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkdir/include/genunix"
    fi
    JAVACLASSES='${jdkdir}/lib/classes.zip'
    JAVA='JAVA_HOME=${jdkdir} ${jdkdir}/bin/java'
    JAVASTUBS_FUNCTION=java-run-all-unicode
    JAVALIBS='${jdkdir}/lib/sparc/green_threads/lib'
    JAVAC='JAVA_HOME=${jdkdir} ${jdkdir}/bin/javac'
  elif test -f $jdkdir/include/japhar/jni.h; then
    JAVAINCLUDES="-I$jdkdir/include/japhar"
    JAVACLASSES="`$jdkdir/bin/japhar-config info datadir`"
    JAVA='${jdkdir}/bin/japhar'
    JAVASTUBS_FUNCTION=java-run-all-unicode
    JAVALIBS='${jdkdir}/lib'
    JAVAC='${jdkdir}/bin/javac'
  elif test -f $jdkdir/include/kaffe/jni.h ; then
    JAVAINCLUDES="-I$jdkdir/include/kaffe"
    JAVACLASSES="$datadir/kaffe/Klasses.jar:$datadir/kaffe/pizza.jar"
    JAVA='${jdkdir}/bin/Kaffe'
    JAVASTUBS_FUNCTION=java-run-all-literal
    JAVALIBS='${jdkdir}/lib:${jdkdir}/lib/kaffe'
    JAVAC='${jdkdir}/bin/javac'
  else
    AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_MSG_RESULT($jdkdir)
  AC_DEFINE(HAVE_JDK)
  JAVASTUBS=stubs
fi 

AC_SUBST(JAVASTUBS)
AC_SUBST(JAVASTUBS_FUNCTION)
AC_SUBST(JAVAINCLUDES)
AC_SUBST(JAVALIBS)
AC_SUBST(JAVACLASSES)
AC_SUBST(JAVA)
if test -n "$JAR"; then
  jar_home=`echo $JAR | sed 's/\/bin\/jar$//'`
  JAR="JAVA_HOME=${jar_home} $JAR"
else
  JAR='JAVA_HOME=${jdkdir} ${jdkdir}/bin/jar'
fi
AC_SUBST(JAR)
AC_SUBST(jdkdir)
])

