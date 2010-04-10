AC_DEFUN([vj_FIND_JDK],
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
  KAWAJAR=
else
  MACOSJAVA=no
  expand_jdkdir=`eval echo $jdkdir`
  USEDOSCLASSPATH=no
  if test $expand_jdkdir = /System/Library/Frameworks/JavaVM.framework; then
    MACOSJAVA=yes
    jdkincludedir=$jdkdir/Headers
    expand_jdkincludedir=$expand_jdkdir/Headers
  elif test $expand_jdkdir = /usr && test -d /usr/include/java; then
    jdkincludedir=$jdkdir/include/java
    expand_jdkincludedir=$expand_jdkdir/include/java
  else
    jdkincludedir=$jdkdir/include
    expand_jdkincludedir=$expand_jdkdir/include
  fi
  JAVASWARM_LIB_NAME=javaswarm
  if test -f $expand_jdkincludedir/jni.h && test ! -f $expand_jdkincludedir/kaffe/jni.h; then
    JAVAINCLUDES="-I$jdkincludedir"
    if test -f $expand_jdkincludedir/linux/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/linux"
      if test -d ${expand_jdkdir}/bin/i386/native_threads; then
	threads=native
        proc=i386
      elif test -d ${expand_jdkdir}/bin/linux/native_threads; then # IBMJDK
	threads=native
        proc=linux
      # no more bin/i386 as of 1.4.3
      elif test -d ${jdkdir}/jre/lib/amd64/native_threads; then
        threads=native
        JAVACMD="\${jdkdir}/jre/bin/java"
      elif test -d ${jdkdir}/jre/lib/i386/native_threads; then 
        threads=native
        JAVACMD="\${jdkdir}/jre/bin/java"
      elif test -d ${expand_jdkdir}/bin/ppc/native_threads; then
        threads=native
        proc=ppc
      elif test -d ${expand_jdkdir}/bin/ppc/green_threads; then
        threads=green
        proc=ppc
        extra_JAVAENV="_JVM_THREADS_TYPE=green_threads"
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
      # no more bin/sparc as of 1.4.3
      elif test -d ${jdkdir}/jre/lib/sparc/native_threads; then 
        threads=native
        JAVACMD="\${jdkdir}/jre/bin/java"
      elif test -d ${jdkdir}/bin/sparc/green_threads; then
        threads=green
      else
        AC_MSG_ERROR([Can't find threads])
      fi
      proc=sparc
    elif test -f $expand_jdkincludedir/freebsd/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/freebsd"
      threads=green
      proc=i386
    elif test -f $expand_jdkincludedir/irix/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/irix"
      threads=green
      proc=mips
      JAVALIBS="\${jdkdir}/lib32/${proc}/${threads}_threads"
      JAVACMD="\${jdkdir}/bin32/${proc}/${threads}_threads/java"
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
    elif test -f $expand_jdkincludedir/winnt/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/winnt"
      JAVALIBS=no
      JAVACMD='${jdkdir}/bin/java'
    elif test -f $expand_jdkincludedir/win32/jni_md.h; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/win32"
      JAVALIBS=no
      JAVACMD='${jdkdir}/bin/java'
    elif test -f $expand_jdkincludedir/NSJavaConfiguration.h; then
      JAVALIBS='${jdkdir}/Libraries'
      JAVACMD='${jdkdir}/Commands/java'
    elif test -d $expand_jdkincludedir/kaffe; then
      JAVAINCLUDES="$JAVAINCLUDES -I$jdkincludedir/kaffe"
      JAVACMD='${jdkdir}/jre/bin/kaffe-bin'
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
          JAVALIBS="${JAVALIBS}:\${jdkdir}/jre/lib/${proc}"
          if test -d "${expand_jdkdir}/jre/lib/${proc}/hotspot"; then
            JAVALIBS="${JAVALIBS}:\${jdkdir}/jre/lib/${proc}/hotspot"
          elif test -d "${expand_jdkdir}/jre/lib/${proc}/classic"; then
            JAVALIBS="${JAVALIBS}:\${jdkdir}/jre/lib/${proc}/classic"
          fi
        elif test -d "${expand_jdkdir}/lib/${proc}/${threads}_threads"; then
          JAVALIBS="\${jdkdir}/lib/${proc}/${threads}_threads"
        else
          AC_MSG_ERROR([Cannot find JDK library])
        fi
      fi
    fi
    test -n "$JAVACMD" || JAVACMD="\${jdkdir}/jre/bin/${proc}/${threads}_threads/java"
    if test -f ${expand_jdkdir}/jre/lib/rt.jar; then
      JAVACLASSES=${jdkdir}/jre/lib/rt.jar
    elif test -f ${expand_jdkdir}/lib/rt.jar; then
      JAVACLASSES=${jdkdir}/lib/rt.jar
    elif test -f ${expand_jdkdir}/Classes/classes.jar; then
      JAVACLASSES=${jdkdir}/Classes/classes.jar
      javac_default='${jdkdir}/Commands/javac'
      test -n "$JAR" || JAR='${jdkdir}/Commands/jar'
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
      JAVAENV="APPHOME=${jdkdir} $SHLIBPATH_VAR=$JAVALIBS:\${$SHLIBPATH_VAR}"
      if test -z "$javac_default"; then
        javac_default='JAVA_HOME=${jdkdir} ${jdkdir}/bin/javac'
      fi
    fi
    JAVAENV="$extra_JAVAENV $JAVAENV"
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
    if test -x "$expand_jdkdir/bin/kaffe" ; then
      kaffebindir="$expand_jdkdir/bin"
      javac_default='${jdkdir}/bin/javac'
    elif test -x "$expand_jdkdir/lib/kaffe/bin/kaffe"; then
      kaffebindir="$expand_jdkdir/lib/kaffe/bin"
      javac_default='${jdkdir}/lib/kaffe/bin/javac'
    else
      AC_MSG_ERROR([Cannot find kaffe script])
    fi
    kaffe_prefix=`sed -n 's/^prefix="\(.*\)"/\1/p' < $kaffebindir/kaffe`
    kaffe_datadir=`sed -n 's/^: ${KAFFE_CLASSDIR="\(.*\)"}/\1/p' < $kaffebindir/kaffe`
    kaffe_expanded_datadir=`echo $kaffe_datadir | sed  "s,\\${prefix},${kaffe_prefix},"`
    jdkdatadir=`eval echo $kaffe_expanded_datadir`
    JAVACLASSES="${jdkdatadir}/Klasses.jar"

    if test "$host_os" = cygwin; then
      JAVACLASSESARG="`cygpath -w ${jdkdatadir}/Klasses.jar`"
      if test -n "$SWARMROOT"; then
        jdkdatadir=`echo $jdkdatadir | sed "s,$SWARMROOT,\\${SWARMROOT},g"`
      fi
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
    AC_DEFINE(HAVE_KAFFE,1,[defined if Java support using Kaffe is to be provided])
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
    JAVALIBPREFIX=
    JAVALIBPATH_VAR=KAFFELIBRARYPATH
  else
    AC_MSG_ERROR([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_MSG_RESULT($jdkdir)
  AC_DEFINE(HAVE_JDK,1,[defined if Java support is to be provided])
  JAVASTUBS=stubs
  JAVASWARMSCRIPTS="javaswarm javacswarm"
  if test "$host_os" = cygwin; then
    JAVASWARMSCRIPTS="$JAVASWARMSCRIPTS jdkswarm jdkcswarm"
  fi
  AC_SUBST(JAVASWARM_DLL_ENTRY)
  AC_SUBST(JAVASWARM_LIB_NAME)
  JAVAC=${JAVAC-$javac_default}
  KAWAJAR=kawa-1.6.70.jar
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
AC_SUBST(KAWAJAR)
])

AC_DEFUN([md_CHECK_JNI_H],
[last_cppflags=$CPPFLAGS
CPPFLAGS="$JAVAINCLUDES $CPPFLAGS"
AC_TRY_COMPILE([#include <jni.h>],[],jni_h_works=yes,jni_h_works=no)
CPPFLAGS=$last_cppflags
if test $jni_h_works = no; then
  AC_CHECK_SIZEOF(int, 4)
  AC_CHECK_SIZEOF(long, 4)
  AC_CHECK_SIZEOF(long long, 8)
  if test $ac_cv_sizeof_int = 8; then
    swarm_int64=int
  elif test $ac_cv_sizeof_long = 8; then
    swarm_int64=long
  elif test $ac_cv_sizeof_long_long = 8; then
    swarm_int64='long long'
  else
    AC_MSG_ERROR(Cannot find 8 byte integer for jni.h)
  fi
  CPPFLAGS="$JAVAINCLUDES $CPPFLAGS"
  AC_DEFINE_UNQUOTED(SWARM_INT64, $swarm_int64, [type of integer that is 64 bits])
  AC_TRY_COMPILE([#define __int64 SWARM_INT64
#include <jni.h>],[],AC_DEFINE(JNI_H_NEEDS_INT64,1,[define if __int64 needs to be defined for jni.h]))
  CPPFLAGS=$last_cppflags
fi
])

