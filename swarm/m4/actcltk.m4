dnl Locate the Tcl/Tk include files
dnl A hybrid of BLT's approach of looking in lots of places and, if
dnl that fails, just believing what "echo [info library] | tclsh" says.
dnl We play a bit of a trick here piping commands into tclsh
dnl Extra set of brackets hides the Tcl brackets from autoconf m4.
dnl
dnl First, if tclsh is around execute it to make a guess as to where Tcl
dnl is installed, and also to find out if we're using tcl > 7.3.
AC_DEFUN([md_FIND_TCL],dnl
[lastPATH=$PATH
if test -n "$with_tcldir" ; then               
  PATH=${with_tcldir}/bin:$PATH
fi
AC_CHECK_PROG(tclsh84_found, tclsh8.4, yes, no)
if test $tclsh84_found = no ; then
    AC_CHECK_PROG(tclsh83_found, tclsh8.3, yes, no)
    if test $tclsh83_found = no ; then
        AC_CHECK_PROG(tclsh82_found, tclsh8.2, yes, no)
        if test $tclsh82_found = no ; then
            AC_CHECK_PROG(tclsh81_found, tclsh8.1, yes, no)
            if test $tclsh81_found = no ; then
                AC_CHECK_PROG(tclsh80_found, tclsh8.0, yes, no)
                if test $tclsh80_found = no ; then
                    AC_CHECK_PROG(tclsh_found, tclsh, yes, no)
                fi
            fi
        fi
    fi
fi
changequote(<,>)dnl
tclLibrary=''
if test $tclsh84_found = yes; then
  tclLibrary=`echo "puts [info library]" | tclsh8.4`
elif test $tclsh83_found = yes; then
  tclLibrary=`echo "puts [info library]" | tclsh8.3`
elif test $tclsh82_found = yes; then
  tclLibrary=`echo "puts [info library]" | tclsh8.2`
elif test $tclsh81_found = yes; then
  tclLibrary=`echo "puts [info library]" | tclsh8.1`
elif test $tclsh80_found = yes; then
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

AC_DEFUN([md_FIND_TCL_HEADERS],dnl
[
# define INCPLACES to be those directories where tcl.h and tk.h could be.
# We look in lots of "standard" places as well as where [info version]
# tells us to look. Note that this list is both for tcl.h and tk.h: they
# are often installed in the same directory.

INCPLACES=" \
        $defaultdir/include $defaultdir/include/tcl $defaultdir/include/tk \
        \${CYGFS}include /Cygnus/cygwin-b20/include \
        /usr/local/include /usr/local/include/tcl /usr/local/include/tk \
        /usr/include /usr/include/tcl /usr/include/tk \
        $x_includes $x_includes/tcl $x_includes/tk \
        ../tcl8.4 ../tcl8.3 ../tcl8.2 ../tcl8.1 ../tcl8.0 ../tcl7.6 ../tcl7.5 ../tcl7.4 \
        ../tk8.3 ../tk8.2 ../tk8.1 ../tk8.0 ../tk4.2 ../tk4.1 ../tk4.0"
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

AC_DEFUN([md_FIND_TCL_LIBRARIES],dnl
[LIBPLACES="$defaultdir/lib $defaultdir/lib/tcl $defaultdir/lib/tk \
        /usr/lib64 \
        \${CYGFS}H-i586-cygwin32/lib \
        /Cygnus/cygwin-b20/H-i586-cygwin32/lib \
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
  for debug in '' g; do
	  for suffix in .dylib .so .sl .a; do
		if test -n "$tcllibname"; then
		  if test -r $expand_dir/lib${tcllibname}${debug}${suffix} ; then
			tcllibdir=$dir
			break
		  fi
		else
		  for version in 84 8.4 83 8.3 82 8.2 81 8.1 80 8.0 76 7.6 7.5 7.4 ''; do
			if test -r $expand_dir/libtcl${version}${debug}${suffix}; then
			  tcllibdir=$dir  
			  tcllibname=tcl${version}${debug}
			  break
			fi        
		  done
		fi
		test -z "$tcllibdir" || break
	  done
  done
  test -z "$tcllibdir" || break
done

if test -n "$tcllibdir" ; then
  AC_MSG_RESULT([$tcllibdir, $tcllibname])
  if test "$tcllibdir" = "/usr/lib" ; then
    TCLLDFLAGS=''
    TCLLIB="-l$tcllibname"
  else
    if test $suffix = .so || test $suffix = .sl; then
      TCLLDFLAGS="-L\${tcllibdir} -R \${tcllibdir}"
    else
      TCLLDFLAGS='-L${tcllibdir}'
    fi
    TCLLIB=-l$tcllibname
  fi
else
  AC_MSG_RESULT(no)
fi
AC_SUBST(TCLLIB)
AC_SUBST(TCLLDFLAGS)
AC_SUBST(tcllibdir)
])dnl

AC_DEFUN([md_FIND_TK_HEADERS],
[POTENTIALINCDIR=`dirname $tclincludedir`
POTENTIALINCDIR=`dirname $POTENTIALINCDIR`
INCPLACES="$INCPLACES \
	$tclincludedir \
    $POTENTIALINCDIR/tk8.4/include \
    $POTENTIALINCDIR/tk8.3/include \
    $POTENTIALINCDIR/tk8.2/include \
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

AC_DEFUN([md_FIND_TK_LIBRARIES],
[POTENTIALLIBDIR=`dirname $tcllibdir`
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
  for debug in '' g; do
	  for suffix in .dylib .so .sl .a; do
		if test -n "$tklibname" ; then
		  if test -r $expand_dir/lib${tklibname}${debug}${suffix} ; then
			tklibdir=$dir
			break
		  fi
		else
		  for version in 84 8.4 83 8.3 82 8.2 81 8.1 80 8.0 42 4.2 4.1 4.0 ''; do
			if test -r $expand_dir/libtk${version}${debug}${suffix}; then
			  tklibdir=$dir
			  tklibname=tk${version}${debug}
			  break
			fi
		  done
		fi
		test -z "$tklibdir" || break
	  done
  done
  test -z "$tklibdir" || break
done

if test -n "$tklibdir" ; then
  AC_MSG_RESULT([$tklibdir, $tklibname])
  if test "$tklibdir" = "/usr/lib" ; then
    TKLDFLAGS=''
    TKLIB=-l$tklibname
  else
    if test $suffix = .so || test $suffix = .sl ; then
      TKLDFLAGS="-L\${tklibdir} -R \${tklibdir}"
    else
      TKLDFLAGS='-L${tklibdir}'
    fi
    TKLIB=-l$tklibname
  fi
else
  AC_MSG_RESULT(no)
fi
AC_SUBST(TKLDFLAGS)
AC_SUBST(TKLIB)
AC_SUBST(tklibdir)
])

AC_DEFUN([md_FIND_TCLTK_SCRIPTS],
[_configfile=`eval echo "$$1libdir/$1Config.sh"`
if test -z "$$1scriptdir" ; then
  _version=`sed -n "s/^translit($1,a-z,A-Z)_VERSION='\(.*\)'/\1/p" $_configfile 2>/dev/null`
  _prefix=`sed -n "s/^translit($1,a-z,A-Z)_PREFIX='\(.*\)'/\1/p" $_configfile 2>/dev/null`
  $1scriptdir=$_prefix/lib/$1$_version
  if test ! -d "$$1scriptdir" ; then
    $1scriptdir=$_prefix/share/$1$_version
  fi
fi
])

AC_DEFUN([md_FIND_TCL_SCRIPTS],
[md_FIND_TCLTK_SCRIPTS(tcl)
if test ! -r $tclscriptdir/init.tcl ; then
  AC_MSG_ERROR(Please use --with-tclscriptdir to specify location of init.tcl)
fi
])

AC_DEFUN([md_FIND_TK_SCRIPTS],
[md_FIND_TCLTK_SCRIPTS(tk)
if test ! -r $tkscriptdir/tk.tcl ; then
  AC_MSG_ERROR(Please use --with-tkscriptdir to specify location of tk.tcl)
fi
])


AC_DEFUN([md_FIND_BLT],
[test -z "$bltdir" && bltdir=$defaultdir
found=no
for name in $bltlibname BLTCore30 BLT.2.4 BLT24 BLT8.0 BLT80 BLT; do
   for extra_libdir in "$bltdir/lib/shared" "$bltdir/lib" "$bltdir"; do
	  md_FIND_LIB(blt,$name,$extra_libdir,1)
	  if test -n "$bltlibdir" ; then
		bltlibname=$name
		found=yes
		break
	  fi
   done
   if test $found = yes; then
     break
   fi
done
if test $found = no; then
  AC_MSG_ERROR(Please use --with-bltdir to specify location of BLT.)
fi

BLTLDFLAGS=$_ldflags
if test $bltlibname = BLTCore30; then
  BLTLIB="-lBLTX30 -l$bltlibname"
elif test $bltlibname = BLTCore30g; then
  BLTLIB="-lBLTX30g -l$bltlibname"
else
  BLTLIB=-l$bltlibname
fi
AC_SUBST(bltlibdir)
AC_SUBST(bltdir)
AC_SUBST(BLTLDFLAGS)
AC_SUBST(BLTLIB)
])

AC_DEFUN([md_CHECK_TCLTK_LIBINC_VERSION_MATCH],
[_configfile=`eval echo "$$1libdir/$1Config.sh"`
_upper=translit($1,a-z,A-Z)
_version=`sed -n "s/^${_upper}_VERSION='\(.*\)'/\1/p" $_configfile 2>/dev/null`
_patchlevel=`sed -n "s/^${_upper}_PATCH_LEVEL='\(.*\)'/\1/p" $_configfile 2>/dev/null`
_lversionpl=${_version}${_patchlevel}
path=`eval echo $$1includedir/$1.h`
_iversionpl=`sed -n "s/#define ${_upper}_PATCH_LEVEL.*\"\(.*\)\"/\1/p" < $path`
if test "$_lversionpl" != "$_iversionpl" || test -z "$_lversionpl" || test -z "$_iversionpl"; then
  AC_MSG_WARN($1 include and $1Config.sh file version mismatch \"$_lversionpl\" != \"$_iversionpl\")
fi
])
