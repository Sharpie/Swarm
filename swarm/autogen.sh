#!/bin/sh
# Run this to generate the Makefile.in's and configure script from
# a CVS checkout.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

DIE=0

# By default don't run `configure' after aclocal/autoconf/automake
NOCONFIGURE=1

# Update whenever version dependencies of developer tools change
REQUIRED_AUTOCONF_VERSION="2.52"
REQUIRED_LIBTOOL_VERSION="1.4.2"
REQUIRED_AUTOMAKE_VERSION="1.5"

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`autoconf' installed to compile Swarm."
  echo "Download the appropriate package for your distribution,"
  echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
  DIE=1
}

AUTOCONF_VERSION=`(autoconf --version) | head -1|cut -d')' -f2| cut -d'(' -f1|sed  's/ //g'`
if [ "$AUTOCONF_VERSION" != "$REQUIRED_AUTOCONF_VERSION" ]; then
    echo
    echo "**Warning**: only tested with version" $REQUIRED_AUTOCONF_VERSION
    echo "of autoconf and may not work with version" $AUTOCONF_VERSION
fi

(libtool --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "**Error**: You must have \`libtool' installed to compile Swarm."
    DIE=1
  }

LIBTOOL_VERSION=`(libtool --version) | head -1|cut -d')' -f2| cut -d'(' -f1|sed  's/ //g'`
if [ "$LIBTOOL_VERSION" != "$REQUIRED_LIBTOOL_VERSION" ]; then
    echo
    echo "**Warning**: only tested with version" $REQUIRED_LIBTOOL_VERSION
    echo "of libtool and may not work with version" $LIBTOOL_VERSION
fi

(automake --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: You must have \`automake' installed to compile Swarm."
  DIE=1
  NO_AUTOMAKE=yes
}

AUTOMAKE_VERSION=`(automake --version) | head -1|cut -d')' -f2| cut -d'(' -f1|sed  's/ //g'`
if [ "$AUTOMAKE_VERSION" != "$REQUIRED_AUTOMAKE_VERSION" ]; then
    echo
    echo "**Warning**: only tested with version" $REQUIRED_AUTOMAKE_VERSION
    echo  "of automake and may not work with version" $AUTOMAKE_VERSION
fi


# if no automake, don't bother testing for aclocal
test -n "$NO_AUTOMAKE" || (aclocal --version) < /dev/null > /dev/null 2>&1 || {
  echo
  echo "**Error**: Missing \`aclocal'.  You need to install automake"  
  DIE=1
}

if test "$DIE" -eq 1; then
  exit 1
fi

case $CC in
xlc )
  am_opt=--include-deps;;
esac

for coin in `find $srcdir -name configure.in -print`
do 
  dr=`dirname $coin`
  if test -f $dr/NO-AUTO-GEN; then
    echo skipping $dr -- flagged as no auto-gen
  else
    echo processing $dr
    #macrodirs=`sed -n -e 's,AM_ACLOCAL_INCLUDE(\(.*\)),\1,gp' < $coin`
    # check for m4 macros in "m4" subdirectory and in the current dir!...
    macrodirs="m4 ."
    ( cd $dr
      aclocalinclude="$ACLOCAL_FLAGS"
      for k in $macrodirs; do
  	if test -d $k; then
          aclocalinclude="$aclocalinclude -I $k"
  	##else 
	##  echo "**Warning**: No such directory \`$k'.  Ignored."
        fi
      done
      #if grep "^AC_PROG_LIBTOOL" configure.in >/dev/null; then
      # echo "Running libtoolize..."
      # libtoolize --force --copy
      #fi
      echo "Running aclocal $aclocalinclude ..."
      aclocal $aclocalinclude
      if grep "^AM_CONFIG_HEADER" configure.in >/dev/null; then
	echo "Running autoheader..."
	autoheader
      fi
      echo "Running autoconf ..."
      autoconf
      if test -f ./Makefile.am; then
	  echo "Running automake --gnu $am_opt ..."
	  automake --add-missing --gnu $am_opt
      else
	  echo skipping "automake" in $dr -- Makefile.am does not exist
      fi
    )
  fi
done

conf_flags="--enable-maintainer-mode --enable-compile-warnings" #--enable-iso-c

if test x$NOCONFIGURE = x; then

  if test -z "$*"; then
      echo "**Warning**: I am going to run \`configure' with no arguments."
      echo "If you wish to pass any to it, please specify them on the"
      echo \`$0\'" command line."
      echo
  fi

  echo Running $srcdir/configure $conf_flags "$@" ...
  $srcdir/configure $conf_flags "$@" \
  && echo Now type \`make\' to compile $PKG_NAME || exit 1
else
  echo Skipping configure process.
fi
