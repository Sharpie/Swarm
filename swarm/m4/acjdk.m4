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
    jdkdosdir="`echo $jdkdir | sed -e 's,//\(.\)/,\1:/,g' -e 's,/,\\\\,g'`"
  else
    jdkdosdir=$jdkdir
  fi
  if test $jdkdir = /usr; then
    jdkincludedir=$jdkdir/include/java
  else
    jdkincludedir=$jdkdir/include
  fi
  if test -f $jdkincludedir/jni.h; then
    JAVAINCLUDES="-I$jdkincludedir"
    if test -f $jdkincludedir/solaris/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/solaris"
    elif test -f $jdkincludedir/alpha/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/alpha"
    elif test -f $jdkincludedir/hp-ux/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/hp-ux"
    elif test -f $jdkincludedir/winnt/jni_md.h; then  # WebObjects
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/winnt"
    else
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/genunix"
    fi
    JAVACMD='${jdkdir}/bin/java'
    if test "$host_os" = cygwin; then
      JAVACLASSES="${jdkdosdir}\lib\classes.zip"
      JAVAENV=
      javac_default=${jdkdir}/bin/javac
    else
      JAVACLASSES="${jdkdir}/lib/classes.zip"
      JAVAENV='JAVA_HOME=${jdkdir}'
      javac_default='JAVA_HOME=${jdkdir} ${jdkdir}/bin/javac'
    fi
    JAVA='${JAVAENV} ${JAVACMD}'
    JAVASTUBS_FUNCTION=java-run-all-unicode
    JAVALIBS='${jdkdir}/lib/sparc/green_threads/lib'
    JAVALIBPREFIX=''
  elif test -f $jdkincludedir/japhar/jni.h; then
    JAVAINCLUDES="-I$jdkincludedir/japhar"
    JAVACLASSES="`$jdkdir/bin/japhar-config info datadir`"
    JAVACMD='${jdkdir}/bin/japhar'
    JAVAENV=''
    JAVA='${JAVACMD}'
    JAVASTUBS_FUNCTION=java-run-all-unicode
    JAVALIBS='${jdkdir}/lib'
    javac_default='${jdkdir}/bin/javac'
    JAVALIBPREFIX=japhar_
  elif test -f $jdkincludedir/kaffe/jni.h ; then
    JAVAINCLUDES="-I$jdkincludedir/kaffe"
    JAVACLASSES="$datadir/kaffe/Klasses.jar:$datadir/kaffe/pizza.jar"
    JAVASTUBS_FUNCTION=java-run-all-literal
    JAVALIBS='${jdkdir}/lib/kaffe' # count on -rpath for main executable
    JAVACMD='${jdkdir}/libexec/Kaffe'
    JAVAENV=''
    JAVA='KAFFELIBRARYPATH=${JAVALIBS} ${JAVACMD}'
    javac_default='${jdkdir}/bin/javac'
    JAVALIBPREFIX=
  else
    AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_MSG_RESULT($jdkdir)
  AC_DEFINE(HAVE_JDK)
  JAVASTUBS=stubs
  JAVASWARMLIBS=-ljavaswarm
  JAVASWARMSCRIPTS="javaswarm javacswarm"
  JAVAC=${JAVAC-$javac_default}
fi 

AC_SUBST(JAVASTUBS)
AC_SUBST(JAVASWARMLIBS)
AC_SUBST(JAVASTUBS_FUNCTION)
AC_SUBST(JAVAINCLUDES)
AC_SUBST(JAVALIBS)
AC_SUBST(JAVALIBPREFIX)
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

