%define ver      2.0 
%define rel      1.rh6kaffe 	
%define prefix   /usr

Summary: Toolkit for agent-based modelling.
Name: swarm
Version: %ver
Release: %rel
Copyright: LGPL
Group: Development/Libraries
Source: ftp://ftp.santafe.edu/pub/swarm/swarm-%ver.tar.gz 
BuildRoot: /tmp/swarm-root
Icon: swarm.xpm
Packager: Paul Johnson <pauljohn@ukans.edu>
URL: http://www.santafe.edu/projects/swarm
Requires: egcs-objc 
Requires: jikes
Requires: libpng
Requires: libpng-devel
Requires: zlib
Requires: xpm
Requires: xpm-devel
Requires: blt
Requires: tcl >= 8.0.4 
Requires: tk >= 8.0.4 
Requires: kaffe = 1.0b4-2

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


%prep

%setup -n swarm-%{ver} 

%build

JAR=/usr/local/jdk1.2/bin/jar  JAVAC=jikes ./configure --with-defaultdir=/usr --disable-static --prefix=%prefix --with-jdkdir=/usr 

make
                            
%install
rm -rf $RPM_BUILD_ROOT
mkdir $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT%{prefix} install

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%files

%defattr(-,root,root)
%{prefix}

%doc README
%doc AUTHORS
%doc COPYING
%doc ChangeLog
%doc INSTALL
%doc NEWS
%doc THANKS

