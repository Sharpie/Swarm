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
  jdkdosdir=
  JAVASTUBS=
  JAVASWARMLIBS=
  JAVASWARMSCRIPTS=
else
  if test "$host_os" = cygwin; then
    jdkdosdir=`echo $jdkdir | sed -e 's,//\(.\)/,\1:/,g' -e 's,/,\\\,g'`"
  else
    jdkdosdir=$jdkdir
  fi
  if test -f $jdkdir/include/jni.h; then
    JAVAINCLUDES="-I$jdkdir/include"
    if test -f $jdkdir/include/solaris/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkdir/include/solaris"
    elif test -f $jdkdir/include/winnt/jni_md.h; then  # WebObjects
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkdir/include/winnt"
    else
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkdir/include/genunix"
    fi
    if test "$host_os" = cygwin; then
      JAVACLASSES='${jdkdosdir}\lib\classes.zip'
    else
      JAVACLASSES='${jdkdir}/lib/classes.zip'
    fi
    JAVACMD='${jdkdir}/bin/java'
    JAVAENV='JAVA_HOME=${jdkdosdir}'
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
AC_SUBST(jdkdosdir)
])

AC_DEFUN(md_CHECK_JNI_H,
[last_cppflags=$CPPFLAGS
CPPFLAGS="$JAVAINCLUDES $CPPFLAGS"
AC_TRY_COMPILE([#include <jni.h>],[],jni_h_works=yes,jni_h_works=no)
CPPFLAGS=$last_cppflags
if test $jni_h_works = no; then
  AC_CHECK_SIZEOF(int, 4)
  AC_CHECK_SIZEOF(long, 4)
  AC_CHECK_SIZEOF(long long, 8)
  if test $ac_cv_sizeof_int = 8; then
    AC_DEFINE(INT64, int)
  elif test $ac_cv_sizeof_long = 8; then
    AC_DEFINE(INT64, long)
  elif test $ac_cv_sizeof_long_long = 8; then
    AC_DEFINE(INT64, long long)
  else
    AC_MSG_ERROR(Cannot find 8 byte integer for jni.h)
  fi
  CPPFLAGS="$JAVAINCLUDES $CPPFLAGS"
  AC_TRY_COMPILE([#define __int64 INT64
#include <jni.h>],[],AC_DEFINE(JNI_H_NEEDS_INT64))
  CPPFLAGS=$last_cppflags
fi
])

