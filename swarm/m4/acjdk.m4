AC_DEFUN(vj_FIND_JDK,
[if test -z "$jdkdir" ; then
  test -n "$JAVAC" || AC_PATH_PROG(JAVAC, javac, missing)
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
  JAVASWARM_DLL_LOADNAME=javaswarm # a default is needed for Globals.java
  if test $jdkdir = /usr && test -d /usr/include/java; then
    jdkincludedir=$jdkdir/include/java
  else
    jdkincludedir=$jdkdir/include
  fi
  if test -f $jdkincludedir/jni.h; then
    JAVAINCLUDES="-I$jdkincludedir"
    if test -f $jdkincludedir/solaris/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/solaris"
      if test -d ${jdkdir}/bin/sparc/native_threads; then
	threads=native
      elif test -d ${jdkdir}/bin/sparc/green_threads; then
        threads=green
      else
        AC_MSG_ERROR([Can't find threads])
      fi
      proc=sparc
    elif test -f $jdkincludedir/alpha/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/alpha"
      JAVALIBS='${jdkdir}/shlib'
      threads=native
      proc=alpha
    elif test -f $jdkincludedir/hp-ux/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/hp-ux"
      if test -d ${jdkdir}/lib/PA_RISC/native_threads; then
        threads=native
      else
        threads=green
      fi
      proc=PA_RISC
      LD_LIBRARY_PATH_VARNAME=SHLIB_PATH
    elif test -f $jdkincludedir/winnt/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/winnt"
      JAVALIBS=no
      JAVACMD='${jdkdir}/bin/java'
    elif test -f $jdkincludedir/win32/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/win32"
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
    if test -f ${jdkdir}/jre/lib/rt.jar; then
      JAVACLASSES=${jdkdir}/jre/lib/rt.jar
    else
      JAVACLASSES=${jdkdir}/lib/classes.zip
    fi
    if test "$host_os" = cygwin; then
      JAVACLASSES="`cygpath -w ${JAVACLASSES}`"
      JAVAENV=
      javac_default=${jdkdir}/bin/javac
      USEDOSCLASSPATH=yes
      JAVASWARM_DLL_NAME=javaswarm
      JAVASWARM_DLL_ENTRY='__cygwin_noncygwin_dll_entry@12'
    else
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
    kaffe_prefix=`sed -n 's/^prefix="\(.*\)"/\1/p' < $jdkdir/bin/kaffe`
    kaffe_datadir=`sed -n 's/: ${KAFFE_CLASSDIR="\(.*\)"}/\1/p' < $jdkdir/bin/kaffe`
    jdkdatadir=`eval echo \`echo $kaffe_datadir | sed  's/\${prefix}/$kaffe_prefix/'\`` 
    if test "$host_os" = cygwin; then
      jdkdatadir=`cygpath -w $jdkdatadir`
      USEDOSCLASSPATH=yes
      # ${jdkdir}/lib/kaffe is included so that .la file can be found
      JAVALIBS="`cygpath -w ${jdkdir}/bin`;`cygpath -w ${jdkdir}/lib/kaffe`"
      JAVASWARM_DLL_NAME=libkaffeswarm
      JAVASWARM_DLL_LOADNAME=kaffeswarm
      JAVASWARM_DLL_ENTRY='__cygwin_dll_entry@12'
    else
      JAVALIBS='${jdkdir}/lib/kaffe'
    fi
    AC_DEFINE(HAVE_KAFFE)
    JAVACLASSES="$jdkdatadir${PATHDELIM}Klasses.jar${PATHSEP}$jdkdatadir${PATHDELIM}pizza.jar"
    JAVASTUBS_FUNCTION=java-run-all-literal
    if test -x "${jdkdir}/libexec/Kaffe"; then
      JAVACMD='${jdkdir}/libexec/Kaffe'
    elif test -x ${jdkdir}/lib/kaffe/Kaffe; then
      JAVACMD='${jdkdir}/lib/kaffe/Kaffe'
    else
      AC_MSG_ERROR([Cannot find Kaffe executable])
    fi
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
  AC_SUBST(JAVASWARM_DLL_ENTRY)
  AC_SUBST(JAVASWARM_DLL_NAME)
  AC_SUBST(JAVASWARM_DLL_LOADNAME)
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

