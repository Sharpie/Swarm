Summary: Toolkit for agent-based simulation.
Name: swarm
%define version 2.0.1
Version: %{version}
Release: 1.rh6
Copyright: LGPL
Group: Development/Libraries
Source: ftp://ftp.santafe.edu/pub/swarm/swarm-%{version}.tar.gz 
%define prefix      /usr
Prefix: %prefix
BuildRoot: /tmp/swarm-root
%define kaffe_version 1.0.b4
BuildPrereq: kaffe = %kaffe_version
%define jikes_version 0.47
BuildPrereq: jikes = %jikes_version
%define hdf5_version 1.2
BuildPrereq: hdf5 > %hdf5_version
Icon: swarm.xpm
Packager: Red Hat Contrib|Net <rhcn-bugs@redhat.com>
Distribution: Red Hat Contrib|Net
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

# this prefix_hdf5 is a hack to trick rpm into doing a second
# build/install without erasing the first build, ultimately the prefix
# should be switched back to /usr before packaging the installed files
# in the %files macro - have still got to figure out how to do this
# ;-) 
%define prefix_hdf5 /hdf5

%define makebuilddir() test -d %1 || mkdir %1 && cd %1

%define SWARM_SRC_DIR $RPM_BUILD_DIR/swarm-$RPM_PACKAGE_VERSION

%define swarmlibraries() for i in activity analysis collections defobj misc objc objectbase random simtools simtoolsgui space swarm tclobjc tkobjc; do echo "%{1}/lib/swarm/lib$i.*" >> %2; done

%define gen_filelist() echo "%{1}/bin/libtool-swarm" > %2; echo "%{1}/bin/m2h" >> %2; echo "%{1}/bin/make-h2x" >> %2 ; echo "%{1}/etc" >> %2 ; echo "%{1}/include" >> %2 ; echo "%{1}/info" >> %2 ;  %{swarmlibraries: %1 %2}; echo '%dir %{1}/share' >> %2 ; echo '%doc README AUTHORS COPYING ChangeLog INSTALL NEWS THANKS' >> %2
 
%description
Swarm is a simulation toolkit for complex adaptive systems. The Swarm
project is sponsored by the Santa Fe Institute.  To use this version,
you need a number of libraries and utilities, including 
tcl/tk 8.0.4 or better, and the blt library package, as well has 
hdf5 version 1.2. 

%changelog
- **Remove this message if public release made** 

* Fri Sep 17 1999 Alex Lancaster <alex@santafe.edu>

- Enable relocation of package, via sed scripts in the %post section.

- Temporarily disable the hdf5 subpackage.

- Reformat spec file as per RHCN guidelines at http://developer.redhat.com

- Various miscellaneous cleanups

* Fri Sep 10 1999 Alex Lancaster <alex@santafe.edu>

- Major structural changes to spec file. Development version, only.
  Does not build the swarm-hdf5 correctly as yet.  Not for public
  release.

* Sun Aug 29 1999 Paul Johnson <pauljohn@ukans.edu>

- jikes is the required java compiler.  Users have to install that or edit
  javacswarm script and change jikes to javac.

* Tue Aug 03 1999 Paul Johnson <pauljohn@ukans.edu>

- Testing new SPEC file for swarm 2.0 preparation.

* Sat Jun 05 1999 Paul Johnson <pauljohn@ukan.edu>

- RPM compiled with a patched egcs to eliminate some warnings observed
  by users who have the egcs as distributed with RH6.0.

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

- finalized/revised for swarm 1.3 release. This binary is compiled
  against blt2.4e, but experimentation shows it is fine with
  blt8.0-unoff as well.

* Wed Aug 12 1998 Paul E. Johnson <pauljohn@ukans.edu>

- Used imlib.spec as template for swarm.

#  %package hdf5
#  Summary: Toolkit for agent-based modelling with hdf5 enabled.
#  Group: Development/Libraries
#  Requires: hdf5 > %hdf5_version
#  Provides: swarm-base
#  Conflicts: swarm
#  %description hdf5
#  Swarm is a simulation toolkit for Complex Adaptive Systems. The Swarm
#  project is sponsored by the Santa Fe Institute.  To use this version,
#  you need a number of libraries and utilities, including 
#  tcl/tk 8.0.4 or better, and the blt library package.  This version enables
#  hdf5. 

%package kaffe
Summary: Adds Java support to swarm-base package.
Group: Development/Libraries
Requires: kaffe = %kaffe_version
Requires: jikes = %jikes_version
Requires: swarm-base
%description kaffe
swarm-kaffe adds Java support to your base Swarm package (either swarm
or swarm-hdf5).  Kaffe is a freely-redistributable implementation of
the Sun JDK.

%prep 
%setup -q

%build

# first run is without hdf support
%makebuilddir =without-hdf

JAVAC=/usr/bin/jikes %{SWARM_SRC_DIR}/configure --srcdir=%{SWARM_SRC_DIR} --with-defaultdir=/usr --disable-static --prefix='/REPLACE-AT-INSTALLATION' --enable-linuxprefix --with-jdkdir=/usr --without-hdf5dir --disable-jar 
make

# second run is with hdf support
#cd %SWARM_SRC_DIR
#%makebuilddir =with-hdf

#JAVAC=/usr/bin/jikes %{SWARM_SRC_DIR}/configure --srcdir=%{SWARM_SRC_DIR} --with-defaultdir=/usr --disable-static --prefix=%prefix --enable-linuxprefix --with-jdkdir=/usr --with-hdf5dir=/usr --disable-jar
#make
                            
%install
rm -rf $RPM_BUILD_ROOT
mkdir $RPM_BUILD_ROOT

# install without-hdf5 version of build
cd %SWARM_SRC_DIR
%gen_filelist %prefix %{SWARM_SRC_DIR}/without-hdf

cd =without-hdf
make prefix=$RPM_BUILD_ROOT%{prefix} install

# install with-hdf5 version of build
#cd %SWARM_SRC_DIR
#%gen_filelist %{prefix_hdf5}%{prefix} %{SWARM_SRC_DIR}/with-hdf

#cd =with-hdf
#make prefix=$RPM_BUILD_ROOT%{prefix_hdf5}%{prefix} install

%post 
cd $RPM_INSTALL_PREFIX0
for i in etc/swarm/Makefile* etc/swarm/config.swarm lib/swarm/lib*.la
do
 eval "sed 's%/REPLACE-AT-INSTALLATION%$RPM_INSTALL_PREFIX0%g' $i > /tmp/swarmfile"
 mv /tmp/swarmfile $i
done
/sbin/ldconfig

%post kaffe
cd $RPM_INSTALL_PREFIX0
for i in bin/java*swarm lib/swarm/libkaffeswarm*.la
do
 eval "sed 's%/REPLACE-AT-INSTALLATION%$RPM_INSTALL_PREFIX0%g' $i > /tmp/swarmfile"
 mv /tmp/swarmfile $i
done
chmod +x bin/java*swarm  # make sure there executable
/sbin/ldconfig

%files -f without-hdf
%defattr(-,root,root)

#%files hdf5 -f with-hdf
#%defattr(-,root,root)

%files kaffe
%defattr(-,root,root)
%{prefix}/bin/javacswarm
%{prefix}/bin/javaswarm
%{prefix}/lib/swarm/libkaffe*
%{prefix}/share/swarm

%clean 
rm -rf $RPM_BUILD_ROOT
