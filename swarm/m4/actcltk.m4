dnl Locate the Tcl/Tk include files
dnl A hybrid of BLT's approach of looking in lots of places and, if
dnl that fails, just believing what "echo [info library] | tclsh" says.
dnl We play a bit of a trick here piping commands into tclsh
dnl Extra set of brackets hides the Tcl brackets from autoconf m4.
dnl
dnl First, if tclsh is around execute it to make a guess as to where Tcl
dnl is installed, and also to find out if we're using tcl > 7.3.
dnl
dnl (nelson 12/11/95) - removed the check for the tcl version, we just
dnl assume they have 7.4. There was a small problem when the version of
dnl tcl from tclsh in the PATH didn't agree with libtcl.a
dnl we used to invoke ["puts [expr [info tclversion] < 7.4]"]
dnl in the tclsh to guess the version number.
AC_DEFUN(md_FIND_TCL_HEADERS,dnl
[AC_CHECK_PROG(tclsh_found, tclsh, 1, 0)
changequote(<,>)dnl
if test "$tclsh_found" = "1"; then
  tclLibrary=`echo "puts [info library]" | tclsh`
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

# now define INCPLACES to be those directories where tcl.h and tk.h could be.
# We look in lots of "standard" places as well as where [info version]
# tells us to look. Note that this list is both for tcl.h and tk.h: they
# are often installed in the same directory.

INCPLACES=" \
           $defaultdir/include $defaultdir/include/tcl $defaultdir/include/tk \
           /usr/local/include /usr/local/include/tcl /usr/local/include/tk \
           /usr/include /usr/include/tcl /usr/include/tk \
           $x_includes $x_includes/tcl $x_includes/tk \
          ../tcl8.0 ../tcl7.6 ../tcl7.5 ../tcl7.4 ../tk8.0 ../tk4.2 ../tk4.1 ../tk4.0"
if test -n "$tclInstalledDir"; then
  INCPLACES="$tclInstalledDir/include $INCPLACES"
fi
INCPLACES="$USER_TCL_INCLUDE $USER_TK_INCLUDE $INCPLACES"

AC_MSG_CHECKING(directory of tcl.h)
tclincludedir=""
for dir in "$TCL_INCLUDE_DIR" $INCPLACES; do
  if test -r $dir/tcl.h; then
    tclincludedir=$dir
    break;
  fi
done

if test -n "$tclincludedir"; then
  AC_MSG_RESULT($tclincludedir)
  if test "$tclincludedir" = "/usr/include" ; then
    TCLINCLUDES=""
  else
    TCLINCLUDES="-I$tclincludedir"
  fi
else
  AC_MSG_RESULT(no)
fi
changequote([,])dnl
AC_SUBST(tclincludedir)
AC_SUBST(TCLINCLUDES)
])dnl

AC_DEFUN(md_FIND_TCL_LIBRARIES,dnl
[changequote(<,>)dnl
LIBPLACES="$defaultdir/lib $defaultdir/lib/tcl $defaultdir/lib/tk \
	/usr/local/lib /usr/local/lib/tcl /usr/local/lib/tk \
	/usr/lib /usr/lib/tcl /usr/lib/tk \
	$x_libraries $x_libraries/tcl $x_libraries/tk \
	../tcl8.0 ../tcl7.6 ../tcl7.5 ../tcl7.4 \
	../tk8.0 ../tk4.2 ../tk4.1 ../tk4.0"

if test -n "$tclInstalledDir"; then
  LIBPLACES="$tclInstalledDir/lib $LIBPLACES"
fi
LIBPLACES="$USER_TCL_LIB $USER_TK_LIB $LIBPLACES"
LIBPLACES="`dirname $tclincludedir`/lib $LIBPLACES"

tcllibdir=""
TCLLIBNAME=""
AC_MSG_CHECKING(directory and version of libtcl)
for suffix in .so .a; do
  for dir in "$TCL_LIB_DIR" $LIBPLACES; do
    for version in 80 8.0 76 7.6 7.5 7.4 ''; do
      if test -r $dir/libtcl${version}${suffix}; then
        tcllibdir=$dir  
        TCLLIBNAME=tcl$version
        break;
      fi        
    done        
    test -z "$tcllibdir" || break;
  done
  test -z "$tcllibdir" || break;
done

if test -n "$tcllibdir" ; then
  AC_MSG_RESULT(<$tcllibdir, $TCLLIBNAME>)
  if test "$tcllibdir" = "/usr/lib" ; then
    TCLLDFLAGS=''
    TCLLIB="-l$TCLLIBNAME"
  else
    if test $suffix = .so ; then
      TCLLDFLAGS="-L\$(tcllibdir) $RPATH\$(tcllibdir)"
    else
      TCLLDFLAGS='-L$(tcllibdir)'
    fi
    TCLLIB=-l$TCLLIBNAME
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
	$POTENTIALINCDIR/tk8.0/include \
	$POTENTIALINCDIR/tk4.2/include \
	$POTENTIALINCDIR/tk4.1/include \
	$POTENTIALINCDIR/tk4.0/include \
	$POTENTIALINCDIR/tk/include"
AC_MSG_CHECKING(directory of tk.h)
tkincludedir=""
for dir in "$TK_INCLUDE_DIR" $INCPLACES; do
  if test -r $dir/tk.h; then
    tkincludedir=$dir
    break;
  fi
done
if test -n "$tkincludedir"; then
  AC_MSG_RESULT($tkincludedir)
  if test "$tkincludedir" = "/usr/include" ; then
    TKINCLUDES=""
  else
    TKINCLUDES="-I$tkincludedir"
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
	$POTENTIALLIBDIR/tk8.0/lib \
	$POTENTIALLIBDIR/tk4.2/lib \
	$POTENTIALLIBDIR/tk4.1/lib \
	$POTENTIALLIBDIR/tk4.0/lib \
	$LIBPLACES"
TKLLIBDIR=""
TKLIBNAME=""
AC_MSG_CHECKING(directory and version of libtk)
for suffix in .so .a; do
  for dir in "$TK_LIB_DIR" $LIBPLACES; do
    for version in 80 8.0 42 4.2 4.1 4.0 ''; do
      if test -r $dir/libtk${version}${suffix}; then
        tklibdir=$dir
        TKLIBNAME=tk${version}
        break;
      fi
    done
    test -z "$tklibdir" || break;
   done
   test -z "$tklibdir" || break;
done

if test -n "$tklibdir" ; then
  AC_MSG_RESULT(<$tklibdir, $TKLIBNAME>)
  if test "$tklibdir" = "/usr/lib" ; then
    TKLDFLAGS=''
    TKLIB=-l$TKLIBNAME
  else
    if test $suffix = .so ; then
      TKLDFLAGS="-L\$(tklibdir) $RPATH\$(tklibdir)"
    else
      TKLDFLAGS='-L$(tklibdir)'
    fi
    TKLIB=-l$TKLIBNAME
  fi
else
  AC_MSG_RESULT(no)
fi
changequote([,])dnl
AC_SUBST(TKLDFLAGS)
AC_SUBST(TKLIB)
AC_SUBST(tklibdir)
])

AC_DEFUN(md_FIND_TCLOBJC_HEADERS,
[if test -z "$tclobjcdir" ; then 
  tclobjcdir=$defaultdir
fi

if test -z "$tclobjcincludedir" ; then
  tclobjcincludedir=$tclobjcdir/include
fi

AC_MSG_CHECKING(directory of tclObjc.h)
if test -f $tclobjcincludedir/tclObjc.h ; then
  AC_MSG_RESULT($tclobjcincludedir)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Please use --with-tclobjcincludedir or --with-tclobjcdir to specify location of tclobjc header files)
fi
AC_SUBST(tclobjcdir)
AC_SUBST(tclobjcincludedir)
])

AC_DEFUN(md_FIND_TCLOBJC_LIBRARIES,
[if test -z "$tclobjclibname" ; then
  tclobjclibname=tclobjc
fi
AC_MSG_CHECKING(directory of lib${tclobjclibname}.a)
if test -f $tclobjcdir/lib/lib${tclobjclibname}.a ; then
  AC_MSG_RESULT($tclobjcdir/lib)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Please use --with-tclobjcdir to specify location of tclobjc package)
fi
TCLOBJCLDFLAGS='-L$(tclobjcdir)/lib'
TCLOBJCLIB=-l$tclobjclibname
AC_SUBST(TCLOBJCLDFLAGS)
AC_SUBST(TCLOBJCLIB)
])

AC_DEFUN(md_FIND_BLT,
[if test -z "$bltdir" ; then
  bltdir=$defaultdir
fi
if test -z "$bltlibname" ; then
  bltlibname=BLT
fi
AC_MSG_CHECKING(directory of lib${bltlibname}.so)
if test -f $bltdir/lib/lib${bltlibname}.so ; then
  BLTLDFLAGS="-L\$(bltdir)/lib $RPATH\$(bltdir)/lib"
  AC_MSG_RESULT($bltdir/lib)
else
  AC_MSG_RESULT(no)
  AC_MSG_CHECKING(directory of lib${bltlibname}.a)
  if test -f $bltdir/lib/lib${bltlibname}.a ; then
    BLTLDFLAGS='-L$(bltdir)/lib'
    AC_MSG_RESULT($bltdir/lib)
  else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR(Please use --with-bltdir to specify location of BLT package)
  fi
fi
BLTLIB=-l${bltlibname}
AC_SUBST(bltdir)
AC_SUBST(BLTLDFLAGS)
AC_SUBST(BLTLIB)
])

