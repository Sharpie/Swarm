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
  USEDOSCLASSPATH=no
  if test $jdkdir = /usr; then
    jdkincludedir=$jdkdir/include/java
  else
    jdkincludedir=$jdkdir/include
  fi
  if test -f $jdkincludedir/jni.h; then
    JAVAINCLUDES="-I$jdkincludedir"
    if test -f $jdkincludedir/solaris/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/solaris"
      threads=green
      proc=sparc
    elif test -f $jdkincludedir/alpha/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/alpha"
      threads=native
      proc=alpha
    elif test -f $jdkincludedir/hp-ux/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/hp-ux"
      JAVALIBS='${jdkdir}/shlib'
      threads=green
      proc=PA_RISC
      LD_LIBRARY_PATH_VARNAME=SHLIB_PATH
    elif test -f $jdkincludedir/winnt/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/winnt"
      JAVALIBS=no
      JAVACMD='${jdkdir}/bin/java'
    else
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/genunix"
      JAVACMD='${jdkdir}/bin/java'
      threads=green
      proc=$target_cpu
    fi
    if test "$JAVALIBS" = no; then
      JAVALIBS=
    else
      test -n "$JAVALIBS" || JAVALIBS="\${jdkdir}/lib/${proc}/${threads}_threads"
    fi
    test -n "$JAVACMD" || JAVACMD="\${jdkdir}/bin/${proc}/${threads}_threads/java"
    if test "$host_os" = cygwin; then
      JAVACLASSES="`cygpath -w ${jdkdir}/lib/classes.zip`"
      JAVAENV=
      javac_default=${jdkdir}/bin/javac
      USEDOSCLASSPATH=yes
    else
      JAVACLASSES="${jdkdir}/lib/classes.zip"
      test -n "$LD_LIBRARY_PATH_VARNAME" || LD_LIBRARY_PATH_VARNAME=LD_LIBRARY_PATH
      JAVAENV="$LD_LIBRARY_PATH_VARNAME=$JAVALIBS:\${$LD_LIBRARY_PATH_VARNAME}"
      javac_default='JAVA_HOME=${jdkdir} ${jdkdir}/bin/javac'
    fi
    JAVA='${JAVAENV} ${JAVACMD}'
    JAVASTUBS_FUNCTION=java-run-all-unicode
    JAVALIBPREFIX=''
    JAVALIBPATH_VAR=
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
    JAVALIBPATH_VAR=
  elif test -f $jdkincludedir/kaffe/jni.h ; then
    JAVAINCLUDES="-I$jdkincludedir/kaffe"
    jdkdatadir=`sed -n 's/: ${KAFFE_CLASSDIR="\(.*\)"}/\1/p' < $jdkdir/bin/kaffe`
    if test "$host_os" = cygwin; then
      jdkdatadir=`cygpath -w $jdkdatadir`
      USEDOSCLASSPATH=yes
      # ${jdkdir}/lib/kaffe is included so that .la file can be found
      JAVALIBS="`cygpath -w ${jdkdir}/bin`;`cygpath -w ${jdkdir}/lib/kaffe`"
    else
      JAVALIBS='${jdkdir}/lib/kaffe'
    fi
    JAVACLASSES="$jdkdatadir${PATHDELIM}Klasses.jar${PATHSEP}$jdkdatadir${PATHDELIM}pizza.jar"
    JAVASTUBS_FUNCTION=java-run-all-literal

    JAVACMD='${jdkdir}/libexec/Kaffe'
    JAVAENV=''
    JAVA='KAFFELIBRARYPATH="${JAVALIBS}" ${JAVACMD}'
    javac_default='${jdkdir}/bin/javac'
    JAVALIBPREFIX=
    JAVALIBPATH_VAR=KAFFELIBRARYPATH
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
test -n "$JAVALIBPATH_VAR" || JAVALIBPATH_VAR="_JAVASWARM_LIBPATH"
AC_SUBST(JAVALIBPATH_VAR)
AC_SUBST(JAVASWARMSCRIPTS)
AC_SUBST(JAVACLASSES)
AC_SUBST(USEDOSCLASSPATH)
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

