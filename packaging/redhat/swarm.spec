Summary: Toolkit for agent-based simulation.
Name: swarm
Version: 2.1.141.20021019
Release: 2RH8.0 
Copyright: LGPL
Group: Development/Libraries
Source: ftp://ftp.swarm.org/pub/swarm/swarm-%version.tar.gz
%define prefix      /usr
#Patch0: ObjectSaver.patch
#Patch1: Customize.patch
#Patch2: Signs2.patch 
Prefix: %prefix
BuildRoot: /tmp/swarm-root
%ifarch ppc
BuildPrereq: libffi >= 1.20
%endif
BuildPrereq: jikes
Icon: swarm.xpm
Packager: Red Hat Contrib|Net <rhcn-bugs@redhat.com>
Distribution: Red Hat Contrib|Net
Vendor: Swarm Development Group
URL: http://www.swarm.org
Requires: gcc >= 3.2, gcc-objc, hdf5 = 1.4.4, XFree86-devel, glibc-devel, libpng, libpng-devel, zlib, zlib-devel, blt, tcl >= 8.3.0, tk >= 8.3.0
%ifarch ppc
Requires: libffi >= 1.20
%endif
Provides: swarm-base

%description
Swarm is a simulation toolkit for complex adaptive systems. The Swarm
project is sponsored by the Santa Fe Institute.  To use this version,
you need a number of libraries and utilities, including tcl/tk 8.3.0
or better, and the blt library package.


#%define gcc_path PATH=/usr/local/bin:$PATH
%define gcc_path PATH=$PATH

%define baseconfigure CC=gcc JAVAC=jikes %{SWARM_SRC_DIR}/configure --srcdir=%{SWARM_SRC_DIR} --with-defaultdir=/usr --prefix=/usr --enable-subdirs --with-jdkdir=/usr/java/j2sdk1.4.1_01

%define SWARM_SRC_DIR $RPM_BUILD_DIR/swarm-%{version} 

%define makebuilddir() test -d %1 || mkdir %1 && cd %1 

%define swarm_shared_libs() for i in activity analysis collections defobj misc objc objectbase random simtools simtoolsgui space swarm tclobjc tkobjc; do echo "%verify (not size md5 mtime) %{1}/lib/swarm/lib$i.la" >> %2 ; echo "%{1}/lib/swarm/lib$i.so*" >> %2 ; done 

%define swarm_static_libs() for i in activity analysis collections defobj misc objc objectbase random simtools simtoolsgui space swarm tclobjc tkobjc; do echo "%{1}/lib/swarm/lib$i.a" >> %2 ; done 

%define gen_shared_filelist() echo "%{1}/bin/libtool-swarm" > %2; echo "%{1}/bin/m2h" >> %2; echo "%{1}/bin/make-h2x" >> %2 ; echo "%verify (not size md5 mtime) %{1}/etc/*" >> %2 ; echo "%{1}/include" >> %2 ; echo "%{1}/info/*" >> %2 ;  %{swarm_shared_libs: %1 %2}; echo '%dir %{1}/share' >> %2 ; echo '%doc README AUTHORS COPYING ChangeLog INSTALL NEWS THANKS' >> %2 

%define gen_static_filelist() echo > %2; %{swarm_static_libs: %1 %2}

%define fix_path() cd $RPM_INSTALL_PREFIX0; for i in %1; do eval "sed 's%/REPLACE-AT-INSTALLATION%$RPM_INSTALL_PREFIX0%g' $i > /tmp/swarmfile"; chmod --reference=$i /tmp/swarmfile; mv /tmp/swarmfile $i; done

%changelog
* Thu Nov 8 2001 Paul Johnson <pauljohn@ukans.edu>

- trying to use gcc3 from Redhat 7.2

* Fri Sep 7 2001 Paul Johnson <pauljohn@ukans.edu>

- create separate swarmgcj package
- patch swarm for METHOD_FUNCTIONS usage

* Fri Jun 22 2001 Paul Johnson <pauljohn@ukans.edu>

- diddling for gcc3.0

* Mon Feb 28 2000 Marcus G. Daniels <mgd@swarm.org>

- Remove ppc patch.

- Remove versioning on kaffe and jikes BuildPrereq.

- Break out system-specific build rules in to separate file, swarm.rules.

- Add support for Sparc and SUSE/i386.

- Hardwire /packages/bin/gcc as compiler.

- Minimize use of %defines (using substitutions instead).

* Fri Oct 22 1999 Alex Lancaster <alex@santafe.edu>

- Replace $RPM_PACKAGE_VERSION with %{version} throughout.

- Add "%gcc_path" macro so that path to gcc can be overriden (by
  setting "%_gcc_path" in the ~/.rpmmacros file), so that packages can
  be easily built against non-stock Red Hat 6.{1,2} compilers (such as
  gcc-2.95.1).

* Tue Oct 19 1999 Alex Lancaster <alex@santafe.edu>

- Add XFree86-devel and egcs to Requires.

- Bump kaffe version to 1.0.5.  Remove --disable-jar option to 
  configure, jar now works in this release of kaffe.

* Thu Oct 01 1999 Alex Lancaster <alex@santafe.edu>

- Disable generation of swarm-kaffe package on ppc, kaffe broken.

* Thu Sep 30 1999 Alex Lancaster <alex@santafe.edu>

- Add "%ifarch ppc" directives to apply 2.0.1-ppc.patch;
  platform-specific OBJCFLAGS/CFLAGS to configure; slightly alternate
  versions numbers for kaffe; and Requires: tag for libffi.

* Tue Sep 28 1999 Alex Lancaster <alex@santafe.edu>

- Disable size, md5 and mtime checking using %verify for files which
  are modified by the %post-install script to avoid -V option 
  reporting a problem.

- New swarm-static add-on subpackage, installs the static libraries:
  used to build the stand-alone swarmdemos and for users wishing to
  enable profiling.

- Removed jikes Requires for installer of package (but not for builder)

* Sun Sep 19 1999 Alex Lancaster <alex@santafe.edu>

- Use macros wherever possible for common shell script code.

- Available for public release.

* Fri Sep 17 1999 Alex Lancaster <alex@santafe.edu>

- Enable relocation of package, via sed scripts in the %post section.

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


%package gcjswarm
Summary: Swarm library compiled for GNU java compiler
Group: Development/Libraries

%description gcjswarm
This package is needed if you want to use gcj to compile java swarm programs.


%package static
Summary: Static libraries for Swarm
Group: Development/Libraries
Requires: swarm-base

%description static
Contains static libraries for Swarm which are otherwise excluded from
the normal binary distribuion

%package jdk
Summary: Adds Java support to swarm-base package.
Group: Development/Libraries
Requires: swarm-base, jikes, j2sdk = 1.4.1_01


%description jdk
swarm-jdk adds Java support to your base Swarm package (either swarm
or swarm-hdf5).  

%prep 
%setup -n swarm-%{version}
#%patch0 -p1
#%patch1 -p1
#%patch2 -p1
%build

%makebuilddir =with-hdf

%define configure %baseconfigure


%ifarch sparc
CONSERVATIVE_OPTIMIZATION=yes %configure
%endif

%ifarch ppc
CFLAGS="-fdollars-in-identifiers -O2 -g" %configure --with-ffidir=/usr --disable-jar
%endif

%ifarch i386
CFLAGS="-g $RPM_OPT_FLAGS" %configure
%endif

%ifarch i586
CFLAGS="-g $RPM_OPT_FLAGS" %configure
%endif

%ifarch i686
CFLAGS="-g $RPM_OPT_FLAGS" %configure
%endif

%{gcc_path} make EXTRACPPFLAGS=-DMETHOD_FUNCTIONS EXTRALDFLAGS=-static-libgcc JAVAC=/usr/bin/jikes

# cd java
# make gcjswarm.so


%install
rm -rf $RPM_BUILD_ROOT
mkdir $RPM_BUILD_ROOT

cd %SWARM_SRC_DIR


%gen_shared_filelist %prefix %{SWARM_SRC_DIR}/with-hdf

%gen_static_filelist %prefix %{SWARM_SRC_DIR}/static

cd =with-hdf

#RPM install would not complete unless I force creating this dir
mkdir -p $RPM_BUILD_ROOT%{prefix}/include/swarm
#%{gcc_path} make prefix=$RPM_BUILD_ROOT%{prefix} install JAVAC=/usr/bin/jikes
%{gcc_path} make  DESTDIR=$RPM_BUILD_ROOT install  JAVAC=/usr/bin/jikes


cd java
make gcjswarm.so
cp gcjswarm.so* $RPM_BUILD_ROOT%{prefix}/lib/swarm

cd ..

%post
#%define pathfiles etc/swarm/* lib/swarm/lib*.la
#%fix_path %pathfiles 

/sbin/ldconfig

%post jdk
#%define jdkpathfiles bin/java*swarm lib/swarm/libjavaswarm*.la
#%fix_path %jdkpathfiles

/sbin/ldconfig

%files -f with-hdf
%{prefix}/bin/print-hdf5
%defattr(-,root,root)

%files static -f static
%defattr(-,root,root)

%files gcjswarm
%{prefix}/lib/swarm/gcjswarm.so
%defattr(-,root,root)


%files jdk
%defattr(-,root,root)
%verify(not size md5 mtime) %{prefix}/bin/javaswarm
%verify(not size md5 mtime) %{prefix}/bin/javacswarm
%verify(not size md5 mtime) %{prefix}/lib/swarm/libjavaswarm*.la
%{prefix}/lib/swarm/libjavaswarm*.so*
%{prefix}/lib/swarm/libjavaswarm.a
%{prefix}/share/swarm

%clean 
rm -rf $RPM_BUILD_ROOT

# Local variables:
# mode: shell-script
# sh-shell: rpm
# end:
