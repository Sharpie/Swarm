Summary: Toolkit for agent-based modelling.
Name: swarm
Version: 2.0.1
Release: 1.rh6
Copyright: LGPL
Group: Development/Libraries
Source: ftp://ftp.santafe.edu/pub/swarm/swarm-%{PACKAGE_VERSION}.tar.gz 
BuildRoot: /tmp/swarm-root
BuildPrereq: kaffe jikes hdf5
Icon: swarm.xpm
Packager: Paul Johnson <pauljohn@ukans.edu>
Distribution: Swarm Distribution
Vendor: Swarm Development Group
URL: http://www.santafe.edu/projects/swarm
Requires: egcs-objc 
Requires: libpng
Requires: libpng-devel
Requires: zlib
Requires: xpm
Requires: xpm-devel
Requires: blt
Requires: tcl >= 8.0.4 
Requires: tk >= 8.0.4 
Provides: swarm-base
Conflicts: swarm-hdf5

%define prefix      /usr

# this prefix_hdf5 is a hack to trick rpm into doing a second
# build/install without erasing the first build, ultimately the prefix
# should be switched back to /usr before packaging the installed files
# in the %files macro - have still got to figure out how to do this
# ;-) 
%define prefix_hdf5 /usr-hdf5

%define makebuilddir() test -d %1 || mkdir %1 && cd %1

%define swarmlibraries() %(for i in activity analysis collections defobj misc objc objectbase random simtools simtoolsgui space swarm tclobjc tkobjc; do echo %1/lib$i.*; done)

%define swarm_source_dir $RPM_BUILD_DIR/swarm-$RPM_PACKAGE_VERSION

#%define basefilelist() %(for i in "%{1}/bin/libtool-swarm" "%{1}/bin/m2h" "%{1}/bin/make-h2x" "%{1}/etc" "%{1}/include" "%{1}/info" "%swarmlibraries %{1}/lib/swarm" "%dir %{1}/share" "%doc README" "%doc AUTHORS" "%doc COPYING" "%doc ChangeLog" "%doc INSTALL" "%doc NEWS"  "%doc THANKS"; do echo $i;done)
 
%description
Swarm is a simulation toolkit for Complex Adaptive Systems. The Swarm
project is sponsored by the Santa Fe Institute.  To use this version,
you need a number of libraries and utilities, including 
tcl/tk 8.0.4 or better, and the blt library package, as well has 
hdf5 version 1.2. 

%changelog
* Sun Aug 29 1999 Paul Johnson <pauljohn@ukans.edu>

- jikes is the required java compiler.  Users have to install that or edit
  javacswarm script and change jikes to javac.

* Tue Aug 03 1999 Paul Johnson <pauljohn@ukans.edu>

-  Testing new SPEC file for swarm 2.0 preparation.

* Sat Jun 05 1999 Paul Johnson <pauljohn@ukan.edu>

-  RPM compiled with a patched egcs to eliminate some warnings
   observed by users who have the egcs as distributed with RH6.0.

- Added patches for the Argument files and strchr and strdup usages

* Wed Jan 27  1999 Paul Johnson <pauljohn@ukans.edu>

- version bump (ouch!) to 1.4

- Added xpm-devel requirement

- Removed the exact version requirement for tcl/tk

* Sun Dec 06 1998 Alex Lancaster <alex@santafe.edu>

- Added stricter dependencies for tcl/tk - exact version numbers

- disable the creation of static libraries 

* Sun Oct 04 1998 Paul Johnson <pauljohn@ukans.edu>

- Updated verison info for Swarm 1.3.1

* Fri Sep 28 1998 Alex Lancaster <alex@santafe.edu>

- Made package member of Development/Libraries group rather than X11/Libraries 

- Bumped rel to 2

- Added egcs and egcs-objc to Requires. 

* Mon Aug 24 1998 Paul E. Johnson <pauljohn@ukans.edu>

-finalized/revised for swarm 1.3 release. This binary is compiled against blt2.4e, but experimentation shows it is fine with blt8.0-unoff as well.

* Wed Aug 12 1998 Paul E. Johnson <pauljohn@ukans.edu>

- Used imlib.spec as template for swarm.

%package hdf5
Summary: Toolkit for agent-based modelling with hdf5 enabled.
Group: Development/Libraries
Requires: hdf5 > 1.2
Provides: swarm-base
Conflicts: swarm
%description hdf5
Swarm is a simulation toolkit for Complex Adaptive Systems. The Swarm
project is sponsored by the Santa Fe Institute.  To use this version,
you need a number of libraries and utilities, including 
tcl/tk 8.0.4 or better, and the blt library package.  This version enables
hdf5. 

%package kaffe
Summary: Adds Java support to base swarm package using kaffe
Group: Development/Libraries
Requires: kaffe = 1.0b4-2
Requires: jikes = 0.47
Requires: swarm-base = $RPM_PACKAGE_VERSION
%description kaffe
swarm-kaffe adds Java support to your base Swarm package (either swarm
or swarm-hdf5).  Kaffe is a freely-redistributable implementation of
the Sun JDK.

%prep 

%setup -n swarm-$RPM_PACKAGE_VERSION

%build

# first run is without hdf support
%makebuilddir =without-hdf

JAVAC=/usr/bin/jikes ../configure --srcdir=.. --with-defaultdir=/usr --disable-static --prefix=%prefix --with-jdkdir=/usr --without-hdf5dir --disable-jar 
make

# second run is with hdf support
cd %swarm_source_dir
%makebuilddir =with-hdf

JAVAC=/usr/bin/jikes ../configure --srcdir=.. --with-defaultdir=/usr --disable-static --prefix=%prefix_hdf5 --with-jdkdir=/usr --with-hdf5dir=/usr --disable-jar
make
                            
%install
rm -rf $RPM_BUILD_ROOT
mkdir $RPM_BUILD_ROOT
cd %swarm_source_dir
cd =without-hdf
make prefix=$RPM_BUILD_ROOT%{prefix} install
cd %swarm_source_dir
cd =with-hdf
make prefix=$RPM_BUILD_ROOT%{prefix_hdf5} install
cd $RPM_BUILD_ROOT%{prefix_hdf5}/lib
mkdir swarm
mv lib* swarm

%clean 
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%files 
%defattr(-,root,root)
%{prefix}/bin/libtool-swarm 
%{prefix}/bin/m2h
%{prefix}/bin/make-h2x
%{prefix}/etc
%{prefix}/include
%{prefix}/info

%swarmlibraries %{prefix}/lib/swarm

%dir %{prefix}/share

%doc README
%doc AUTHORS
%doc COPYING
%doc ChangeLog
%doc INSTALL
%doc NEWS
%doc THANKS

%files hdf5
%defattr(-,root,root)
%{prefix_hdf5}/bin/libtool-swarm 
%{prefix_hdf5}/bin/m2h
%{prefix_hdf5}/bin/make-h2x
%{prefix_hdf5}/etc
%{prefix_hdf5}/include
%{prefix_hdf5}/info

%swarmlibraries %{prefix_hdf5}/lib/swarm

%dir %{prefix_hdf5}/share

%doc README
%doc AUTHORS
%doc COPYING
%doc ChangeLog
%doc INSTALL
%doc NEWS
%doc THANKS

%files kaffe
%defattr(-,root,root)
%{prefix}/bin/javacswarm
%{prefix}/bin/javaswarm
%{prefix}/lib/swarm/libjava*
%{prefix}/share/swarm

