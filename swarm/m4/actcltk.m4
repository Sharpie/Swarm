dnl Locate the Tcl/Tk include files
dnl A hybrid of BLT's approach of looking in lots of places and, if
dnl that fails, just believing what "echo [info library] | tclsh" says.
dnl We play a bit of a trick here piping commands into tclsh
dnl Extra set of brackets hides the Tcl brackets from autoconf m4.
dnl
dnl First, if tclsh is around execute it to make a guess as to where Tcl
dnl is installed, and also to find out if we're using tcl > 7.3.
AC_DEFUN(md_FIND_TCL,dnl
[lastPATH=$PATH
if test -n "$with_tcldir" ; then               
  PATH=${with_tcldir}/bin:$PATH
fi
AC_CHECK_PROG(tclsh8_found, tclsh8.0, yes, no)
if test $tclsh8_found = no ; then
  AC_CHECK_PROG(tclsh_found, tclsh, yes, no)
fi
changequote(<,>)dnl
tclLibrary=''
if test $tclsh8_found = yes; then
  tclLibrary=`echo "puts [info library]" | tclsh8.0`
else
  if test $tclsh_found = yes; then
    tclLibrary=`echo "puts [info library]" | tclsh`
  fi
fi
PATH=$lastPATH
changequote([,])dnl
if test -n "$tclLibrary"; then
  tclInstalledDir=`dirname "$tclLibrary"`
  tclInstalledDir=`dirname "$tclInstalledDir"`
fi

# Second, if TCL_LIBRARY or TK_LIBRARY are set, work from there.
if test -n "$TCL_LIBRARY"; then
  USER_TCL_INCLUDE=`dirname $TCL_LIBRARY`
  USER_TCL_INCLUDE=`dirname $USER_TCL_INCLUDE`/include
  USER_TCL_LIB=`dirname $TCL_LIBRARY`
else
  USER_TCL_INCLUDE=""
  USER_TCL_LIB=""
fi
if test -n "$TK_LIBRARY"; then
  USER_TK_INCLUDE=`dirname $TK_LIBRARY`
  USER_TK_INCLUDE=`dirname $USER_TK_INCLUDE`/include
  USER_TK_LIB=`dirname $TK_LIBRARY`
else
  USER_TK_INCLUDE=""
  USER_TK_LIB=""
fi
])

AC_DEFUN(md_FIND_TCL_HEADERS,dnl
[
# define INCPLACES to be those directories where tcl.h and tk.h could be.
# We look in lots of "standard" places as well as where [info version]
# tells us to look. Note that this list is both for tcl.h and tk.h: they
# are often installed in the same directory.

INCPLACES=" \
        $defaultdir/include $defaultdir/include/tcl $defaultdir/include/tk \
        \${CYGFS}include /Cygnus/B19/include \
        /usr/local/include /usr/local/include/tcl /usr/local/include/tk \
        /usr/include /usr/include/tcl /usr/include/tk \
        $x_includes $x_includes/tcl $x_includes/tk \
        ../tcl8.1 ../tcl8.0 ../tcl7.6 ../tcl7.5 ../tcl7.4 \
        ../tk8.1 ../tk8.0 ../tk4.2 ../tk4.1 ../tk4.0"
if test -n "$tclInstalledDir"; then
  INCPLACES="$tclInstalledDir/include $INCPLACES"
fi
INCPLACES="$USER_TCL_INCLUDE $USER_TK_INCLUDE $INCPLACES"

AC_MSG_CHECKING(directory of tcl.h)
for dir in $tclincludedir "$TCL_INCLUDE_DIR" $INCPLACES; do
  expand_dir=`eval echo $dir`
  if test -r $expand_dir/tcl.h; then
    tclincludedir=$dir
    break
  fi
done

if test -n "$tclincludedir"; then
  AC_MSG_RESULT($tclincludedir)
  if test "$tclincludedir" = "/usr/include" ; then
    TCLINCLUDES=""
  else
    TCLINCLUDES='-I${tclincludedir}'
  fi
else
  AC_MSG_RESULT(no)
fi
AC_SUBST(tclincludedir)
AC_SUBST(TCLINCLUDES)
])dnl

AC_DEFUN(md_FIND_TCL_LIBRARIES,dnl
[changequote(<,>)dnl
LIBPLACES="$defaultdir/lib $defaultdir/lib/tcl $defaultdir/lib/tk \
        \${CYGFS}H-i386-cygwin32/lib /Cygnus/B19/H-i386-cygwin32/lib \
	/usr/local/lib /usr/local/lib/tcl /usr/local/lib/tk \
	/usr/lib /usr/lib/tcl /usr/lib/tk \
	$x_libraries $x_libraries/tcl $x_libraries/tk \
	../tcl8.1 ../tcl8.0 ../tcl7.6 ../tcl7.5 ../tcl7.4 \
	../tk8.1 ../tk8.0 ../tk4.2 ../tk4.1 ../tk4.0"

if test -n "$tclInstalledDir"; then
  LIBPLACES="$tclInstalledDir/lib $LIBPLACES"
fi
LIBPLACES="$USER_TCL_LIB $USER_TK_LIB $LIBPLACES"
LIBPLACES="`dirname $tclincludedir`/lib $LIBPLACES"

AC_MSG_CHECKING(directory and version of libtcl)
for dir in $tcllibdir "$TCL_LIB_DIR" $LIBPLACES; do
  tcllibdir=''
  expand_dir=`eval echo $dir`
  for suffix in .so .a; do
    if test -n "$tcllibname"; then
      if test -r $expand_dir/lib${tcllibname}${suffix} ; then
        tcllibdir=$dir
        break
      fi
    else
      for version in 81 8.1 80 8.0 76 7.6 7.5 7.4 ''; do
        if test -r $expand_dir/libtcl${version}${suffix}; then
          tcllibdir=$dir  
          tcllibname=tcl$version
          break
        fi        
      done
    fi
    test -z "$tcllibdir" || break
  done
  test -z "$tcllibdir" || break
done

if test -n "$tcllibdir" ; then
  AC_MSG_RESULT(<$tcllibdir, $tcllibname>)
  if test "$tcllibdir" = "/usr/lib" ; then
    TCLLDFLAGS=''
    TCLLIB="-l$tcllibname"
  else
    if test $suffix = .so ; then
      TCLLDFLAGS="-L\${tcllibdir} $RPATH\${tcllibdir}"
    else
      TCLLDFLAGS='-L${tcllibdir}'
    fi
    TCLLIB=-l$tcllibname
  fi
else
  AC_MSG_RESULT(no)
fi
changequote([,])dnl
AC_SUBST(TCLLIB)
AC_SUBST(TCLLDFLAGS)
AC_SUBST(tcllibdir)
])dnl

AC_DEFUN(md_FIND_TK_HEADERS,
[POTENTIALINCDIR=`dirname $tclincludedir`
POTENTIALINCDIR=`dirname $POTENTIALINCDIR`
INCPLACES="$INCPLACES \
	$tclincludedir \ 
        $POTENTIALINCDIR/tk8.1/include \
	$POTENTIALINCDIR/tk8.0/include \
	$POTENTIALINCDIR/tk4.2/include \
	$POTENTIALINCDIR/tk4.1/include \
	$POTENTIALINCDIR/tk4.0/include \
	$POTENTIALINCDIR/tk/include"
if test -n "$tclInstalledDir"; then
  INCPLACES="$tclInstalledDir/include $INCPLACES"
fi
AC_MSG_CHECKING(directory of tk.h)
for dir in $tkincludedir "$TK_INCLUDE_DIR" $INCPLACES; do
  expand_dir=`eval echo $dir`
  if test -r $expand_dir/tk.h; then
    tkincludedir=$dir
    break
  fi
done
if test -n "$tkincludedir"; then
  AC_MSG_RESULT($tkincludedir)
  if test "$tkincludedir" = "/usr/include" ; then
    TKINCLUDES=""
  else
    TKINCLUDES='-I${tkincludedir}'
  fi
else
  AC_MSG_RESULT(no)
fi
AC_SUBST(tkincludedir)
AC_SUBST(TKINCLUDES)
])

AC_DEFUN(md_FIND_TK_LIBRARIES,
[changequote(<,>)dnl
POTENTIALLIBDIR=`dirname $tcllibdir`
POTENTIALLIBDIR=`dirname $POTENTIALLIBDIR`
LIBPLACES="`dirname $tkincludedir`/lib $tcllibdir $POTENTIALLIBDIR/tk/lib \
        $POTENTIALLIBDIR/tk8.1/lib \
	$POTENTIALLIBDIR/tk8.0/lib \
	$POTENTIALLIBDIR/tk4.2/lib \
	$POTENTIALLIBDIR/tk4.1/lib \
	$POTENTIALLIBDIR/tk4.0/lib \
	$LIBPLACES"
if test -n "$tclInstalledDir"; then
  LIBPLACES="$tclInstalledDir/lib $LIBPLACES"
fi
AC_MSG_CHECKING(directory and version of libtk)
for dir in $tklibdir "$TK_LIB_DIR" $LIBPLACES; do
  tklibdir=''
  expand_dir=`eval echo $dir`
  for suffix in .so .a; do
    if test -n "$tklibname" ; then
      if test -r $expand_dir/lib${tklibname}${suffix} ; then
        tklibdir=$dir
        break
      fi
    else
      for version in 81 8.1 80 8.0 42 4.2 4.1 4.0 ''; do
        if test -r $expand_dir/libtk${version}${suffix}; then
          tklibdir=$dir
          tklibname=tk${version}
          break
        fi
      done
    fi
    test -z "$tklibdir" || break
  done
  test -z "$tklibdir" || break
done

if test -n "$tklibdir" ; then
  AC_MSG_RESULT(<$tklibdir, $tklibname>)
  if test "$tklibdir" = "/usr/lib" ; then
    TKLDFLAGS=''
    TKLIB=-l$tklibname
  else
    if test $suffix = .so ; then
      TKLDFLAGS="-L\${tklibdir} $RPATH\${tklibdir}"
    else
      TKLDFLAGS='-L${tklibdir}'
    fi
    TKLIB=-l$tklibname
  fi
else
  AC_MSG_RESULT(no)
fi
changequote([,])dnl
AC_SUBST(TKLDFLAGS)
AC_SUBST(TKLIB)
AC_SUBST(tklibdir)
])

AC_DEFUN(md_FIND_BLT,
[test -z "$bltdir" && bltdir=$defaultdir
found=no
for name in $bltlibname BLT BLT8.0 BLT80 ; do
  md_FIND_LIB(blt,$name,$bltdir/lib/shared,)
  if test -n "$_ldflags" ; then
    bltlibname=$name
    found=yes
    break
  fi
done
if test $found = no; then
  AC_MSG_ERROR(Please use --with-bltdir to specify location of BLT.)
fi

BLTLDFLAGS=$_ldflags
BLTLIB=-l$bltlibname
AC_SUBST(bltlibdir)
AC_SUBST(bltdir)
AC_SUBST(BLTLDFLAGS)
AC_SUBST(BLTLIB)
])
