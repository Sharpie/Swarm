
AC_DEFUN(vj_DISABLE_JAVA, [
AC_ARG_ENABLE(java, 
changequote(<<, >>)dnl
<<  --enable-java[=ARG]      build java support [default=>>vj_ENABLE_JAVA_DEFAULT],
changequote([, ])
[case "$enableval" in 
yes) enable_java=yes ;; 
no) enable_java=no ;; 
*) enable_java=no ;; 
esac], 
enable_java=no)dnl
])

AC_DEFUN(vj_FIND_JDK,
[if test -z "$jdkdir" ; then
  AC_PATH_PROG(JAVAC, javac, missing)
  if test $JAVAC != missing; then
    changequote(,)
    jdkdir=`echo $JAVAC | sed -e 's/\/javac$//' -e 's/\/[^/][^/]*$//'`
    changequote([,])
  else
    AC_MSG_RESULT([Please use --with-jdkdir to specify location of JDK.])
  fi
  AC_SUBST(jdkdir)
fi
])

