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
  JAVASWARMLIBS=
  JAVASWARMSCRIPTS=
else
  if test -f $jdkdir/include/jni.h; then
    JAVAINCLUDES="-I$jdkdir/include"
    if test -f $jdkdir/include/solaris/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkdir/include/solaris"
    else
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkdir/include/genunix"
    fi
    JAVACLASSES='${jdkdir}/lib/classes.zip'
    JAVACMD='${jdkdir}/bin/java'
    JAVAENV='JAVA_HOME=${jdkdir}'
    JAVA='${JAVAENV} ${JAVACMD}'
    JAVASTUBS_FUNCTION=java-run-all-unicode
    JAVALIBS='${jdkdir}/lib/sparc/green_threads/lib'
    JAVAC='JAVA_HOME=${jdkdir} ${jdkdir}/bin/javac'
  elif test -f $jdkdir/include/japhar/jni.h; then
    JAVAINCLUDES="-I$jdkdir/include/japhar"
    JAVACLASSES="`$jdkdir/bin/japhar-config info datadir`"
    JAVACMD='${jdkdir}/bin/japhar'
    JAVAENV=''
    JAVA='${JAVACMD}'
    JAVASTUBS_FUNCTION=java-run-all-unicode
    JAVALIBS='${jdkdir}/lib'
    JAVAC='${jdkdir}/bin/javac'
  elif test -f $jdkdir/include/kaffe/jni.h ; then
    JAVAINCLUDES="-I$jdkdir/include/kaffe"
    JAVACLASSES="$datadir/kaffe/Klasses.jar:$datadir/kaffe/pizza.jar"
    JAVASTUBS_FUNCTION=java-run-all-literal
    JAVALIBS='${jdkdir}/lib:${jdkdir}/lib/kaffe'
    JAVACMD='${jdkdir}/libexec/Kaffe'
    JAVAENV=''
    JAVA='LD_LIBRARY_PATH=${JAVALIBS}:${LD_LIBRARY_PATH} ${JAVACMD}'
    JAVAC='${jdkdir}/bin/javac'
  else
    AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_MSG_RESULT($jdkdir)
  AC_DEFINE(HAVE_JDK)
  JAVASTUBS=stubs
  JAVASWARMLIBS=-ljavaswarm
  JAVASWARMSCRIPTS="javaswarm javacswarm"
fi 

AC_SUBST(JAVASTUBS)
AC_SUBST(JAVASWARMLIBS)
AC_SUBST(JAVASTUBS_FUNCTION)
AC_SUBST(JAVAINCLUDES)
AC_SUBST(JAVALIBS)
AC_SUBST(JAVASWARMSCRIPTS)
AC_SUBST(JAVACLASSES)
AC_SUBST(JAVACMD)
AC_SUBST(JAVAENV)
AC_SUBST(JAVA)
if test -n "$JAR"; then
  jar_home=`echo $JAR | sed 's/\/bin\/jar$//'`
  JAR="JAVA_HOME=${jar_home} $JAR"
else
  JAR='JAVA_HOME=${jdkdir} ${jdkdir}/bin/jar'
fi
if test -n "$JAR_CLASSPATH" ; then
  JAR="CLASSPATH=$JAR_CLASSPATH $JAR"
fi
AC_SUBST(JAR)
AC_SUBST(jdkdir)
])

