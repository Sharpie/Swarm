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
  JAVASWARMSCRIPTS=
else
  expand_jdkdir=`eval echo $jdkdir`
  USEDOSCLASSPATH=no
  if test $expand_jdkdir = /usr && test -d /usr/include/java; then
    jdkincludedir=$jdkdir/include/java
    expand_jdkincludedir=$expand_jdkdir/include/java
  else
    jdkincludedir=$jdkdir/include
    expand_jdkincludedir=$expand_jdkdir/include
  fi
  JAVASWARM_LIB_NAME=javaswarm
  if test -f $expand_jdkincludedir/jni.h; then
    JAVAINCLUDES="-I$jdkincludedir"
    if test -f $expand_jdkincludedir/linux/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/linux"
      if test -d ${expand_jdkdir}/bin/i386/native_threads; then
	threads=native
        proc=i386
      elif test -d ${expand_jdkdir}/bin/linux/native_threads; then # IBMJDK
	threads=native
        proc=linux
      elif test -d ${expand_jdkdir}/bin/ppc/green_threads; then
        threads=green
        proc=ppc
      elif test -d ${expand_jdkdir}/bin/i386/green_threads; then
        threads=green
        proc=i386
      else
        AC_MSG_ERROR([Can't find threads])
      fi
    elif test -f $expand_jdkincludedir/solaris/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/solaris"
      if test -d ${jdkdir}/bin/sparc/native_threads; then
	threads=native
      elif test -d ${jdkdir}/bin/sparc/green_threads; then
        threads=green
      else
        AC_MSG_ERROR([Can't find threads])
      fi
      proc=sparc
    elif test -f $expand_jdkincludedir/alpha/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/alpha"
      JAVALIBS='${jdkdir}/shlib'
      threads=native
      proc=alpha
    elif test -f $expand_jdkincludedir/hp-ux/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/hp-ux"
      if test -d ${expand_jdkdir}/lib/PA_RISC/native_threads; then
        threads=native
      else
        threads=green
      fi
      proc=PA_RISC
      LD_LIBRARY_PATH_VARNAME=SHLIB_PATH
    elif test -f $expand_jdkincludedir/winnt/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/winnt"
      JAVALIBS=no
      JAVACMD='${jdkdir}/bin/java'
    elif test -f $expand_jdkincludedir/win32/jni_md.h; then
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
      if test -z "$JAVALIBS"; then
        if test -d "${expand_jdkdir}/jre/lib/${proc}"; then
          if test -d "${expand_jdkdir}/jre/lib/${proc}/${threads}_threads"; then
            JAVALIBS="\${jdkdir}/jre/lib/${proc}/${threads}_threads"
          fi
          if test -d "${expand_jdkdir}/jre/lib/${proc}/classic"; then
            JAVALIBS="${JAVALIBS}:\${jdkdir}/jre/lib/${proc}/classic"
          fi
          JAVALIBS="${JAVALIBS}:\${jdkdir}/jre/lib/${proc}"
        elif test -d "${expand_jdkdir}/lib/${proc}/${threads}_threads"; then
          JAVALIBS="\${jdkdir}/lib/${proc}/${threads}_threads"
        else
          AC_MSG_ERROR([Cannot find JDK library])
        fi
      fi
    fi
    test -n "$JAVACMD" || JAVACMD="\${jdkdir}/bin/${proc}/${threads}_threads/java"
    if test -f ${expand_jdkdir}/jre/lib/rt.jar; then
      JAVACLASSES=${jdkdir}/jre/lib/rt.jar
    else
      JAVACLASSES=${jdkdir}/lib/classes.zip
    fi
    if test "$host_os" = cygwin; then
      JAVACLASSESARG="`cygpath -w ${JAVACLASSES}`"
      JAVAENV=
      javac_default=${jdkdir}/bin/javac
      USEDOSCLASSPATH=yes
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
  elif test -f $expand_jdkincludedir/japhar/jni.h; then
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
  elif test -f $expand_jdkincludedir/kaffe/jni.h ; then
    JAVAINCLUDES="-I$jdkincludedir/kaffe"
    JAVASWARM_LIB_NAME=kaffeswarm
    kaffe_prefix=`sed -n 's/^prefix="\(.*\)"/\1/p' < $expand_jdkdir/bin/kaffe`
    kaffe_datadir=`sed -n 's/: ${KAFFE_CLASSDIR="\(.*\)"}/\1/p' < $expand_jdkdir/bin/kaffe`
    jdkdatadir=`eval echo \`echo $kaffe_datadir | sed  's/\${prefix}/$kaffe_prefix/'\``
    JAVACLASSES="${jdkdatadir}/Klasses.jar"

    if test "$host_os" = cygwin; then
      JAVACLASSESARG="`cygpath -w ${jdkdatadir}/Klasses.jar`"
      # we can assume SWARMROOT will be set in Windows environment --
      # recover the symbolic path representation from datadir.
      jdkdatadir=`echo $jdkdatadir | sed "s,$SWARMROOT,\\${SWARMROOT},g"`
      USEDOSCLASSPATH=yes
      # ${jdkdir}/lib/kaffe is included so that .la file can be found
      JAVALIBS="${jdkdir}/bin:${jdkdir}/lib/kaffe"
      JAVALIBSARG="`cygpath -w ${expand_jdkdir}/bin`;`cygpath -w ${expand_jdkdir}/lib/kaffe`"
      JAVASWARM_LIB_NAME=libkaffeswarm
      JAVASWARM_DLL_ENTRY='__cygwin_dll_entry@12'
    else
      JAVALIBS='${jdkdir}/lib/kaffe'
      JAVALIBSARG=$JAVALIBS
    fi
    AC_DEFINE(HAVE_KAFFE)
    JAVASTUBS_FUNCTION=java-run-all-literal
    if test -x "${expand_jdkdir}/libexec/Kaffe"; then
      JAVACMD='${jdkdir}/libexec/Kaffe'
    elif test -x "${expand_jdkdir}/lib/kaffe/Kaffe"; then
      JAVACMD='${jdkdir}/lib/kaffe/Kaffe'
    elif test -x "${expand_jdkdir}/lib/kaffe/bin/Kaffe"; then
      JAVACMD='${jdkdir}/lib/kaffe/bin/Kaffe'
    else
      AC_MSG_ERROR([Cannot find Kaffe executable])
    fi
    JAVAENV=''
    JAVA='KAFFELIBRARYPATH="${JAVALIBSARG}" ${JAVACMD}'
    javac_default='${jdkdir}/bin/javac'
    JAVALIBPREFIX=
    JAVALIBPATH_VAR=KAFFELIBRARYPATH
  else
    AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_MSG_RESULT($jdkdir)
  AC_DEFINE(HAVE_JDK)
  JAVASTUBS=stubs
  JAVASWARMSCRIPTS="javaswarm javacswarm"
  AC_SUBST(JAVASWARM_DLL_ENTRY)
  AC_SUBST(JAVASWARM_LIB_NAME)
  JAVAC=${JAVAC-$javac_default}
fi 

AC_SUBST(JAVASTUBS)
AC_SUBST(JAVASTUBS_FUNCTION)
AC_SUBST(JAVAINCLUDES)
AC_SUBST(JAVALIBS)
test -n "$JAVALIBSARG" || JAVALIBSARG=$JAVALIBS
AC_SUBST(JAVALIBSARG)
AC_SUBST(JAVALIBPREFIX)
test -n "$JAVALIBPATH_VAR" || JAVALIBPATH_VAR="_JAVASWARM_LIBPATH"
AC_SUBST(JAVALIBPATH_VAR)
AC_SUBST(JAVASWARMSCRIPTS)
AC_SUBST(JAVACLASSES)
test -n "$JAVACLASSESARG" || JAVACLASSESARG=$JAVACLASSES
AC_SUBST(JAVACLASSESARG)
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

