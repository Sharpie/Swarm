%define swarm_version 2.0.1
%define swarmapps_version 2.0.1
%define prefix   /usr

Summary: Swarm demonstration applications.
Name: swarmdemos
Version: %{swarm_version}
Release: 1
Copyright: GPL
Group: Applications/Engineering
Source0: ftp://ftp.santafe.edu/pub/swarm/swarmapps-%{swarmapps_version}.tar.gz
Source1: ftp://ftp.santafe.edu/pub/swarm/swarm-%{swarm_version}.tar.gz 
BuildRoot: /tmp/swarmdemos-root
Prefix: %prefix
Icon: swarm.xpm
Packager: Red Hat Contrib|Net <rhcn-bugs@redhat.com>
Distribution: Red Hat Contrib|Net
Vendor: Swarm Development Group
URL: http://www.santafe.edu/projects/swarm
BuildPrereq: blt tcl tk zlib libpng 
Requires: blt tcl tk zlib libpng 

%changelog
* Sat Sep 18 1999 Alex Lancaster <alex@santafe.edu>

- Created. 

%description 
Swarm is a simulation development toolkit for research in Complex
Adaptive Systems (CAS).  Swarmdemos is a collections of demonstration
applications for Swarm, that functions *without* a local installation
of Swarm.

%prep

%setup -T -b 0 -n swarmapps-%swarmapps_version
%setup -T -b 1 -n swarm-%swarm_version

%build

cd $RPM_BUILD_DIR/swarm-%{swarm_version}

# make the static version of Swarm
mkdir '=build'
mkdir '=inst'
cd '=build'

$RPM_BUILD_DIR/swarm-%{swarm_version}/configure --srcdir=$RPM_BUILD_DIR/swarm-%{swarm_version} --prefix=$RPM_BUILD_DIR/swarm-%{swarm_version}/=inst --with-defaultdir=/usr --enable-subdirs --without-jdkdir --without-hdf5dir --disable-shared --enable-static
make 
make install 

# compile and link all apps against the static version
cd $RPM_BUILD_DIR/swarmapps-%{swarmapps_version}

for i in heatbugs market template mousetrap 
do
	cd $i
	SWARMHOME=$RPM_BUILD_DIR/swarm-%{swarm_version}/=inst make
	cd -
done
    
%install
rm -rf $RPM_BUILD_ROOT
mkdir $RPM_BUILD_ROOT

# make the standalone swarmdemos package
mkdir $RPM_BUILD_ROOT%{prefix}
mkdir $RPM_BUILD_ROOT%{prefix}/bin
mkdir $RPM_BUILD_ROOT%{prefix}/etc
mkdir $RPM_BUILD_ROOT%{prefix}/etc/swarm
mkdir $RPM_BUILD_ROOT%{prefix}/share
mkdir $RPM_BUILD_ROOT%{prefix}/share/swarm

# need to put libtool-swarm around in the BuildRoot to be
# able to install the app binaries properly
cp $RPM_BUILD_DIR/swarm-%{swarm_version}/=inst/bin/libtool-swarm $RPM_BUILD_ROOT%{prefix}/bin

cd $RPM_BUILD_DIR/swarmapps-%{swarmapps_version}

# install all the applications in the BuildRoot
for i in heatbugs market mousetrap 
do
	cd $i
	SWARMHOME=$RPM_BUILD_DIR/swarm-%{swarm_version}/=inst make \
		prefix=$RPM_BUILD_ROOT%{prefix} \
		bindir=$RPM_BUILD_ROOT%{prefix}/bin install
	cd $RPM_BUILD_DIR/swarmapps-%{swarmapps_version}
done

# don't want to package libtool-swarm!
rm $RPM_BUILD_ROOT%{prefix}/bin/libtool-swarm

# create doc directories for each app
# and prepare documentation for packaging
mkdir $RPM_BUILD_ROOT%{prefix}/doc
mkdir $RPM_BUILD_ROOT%{prefix}/doc/$RPM_PACKAGE_NAME-%{swarmapps_version}

%define install_doc() cd %1; mkdir $RPM_BUILD_ROOT%{prefix}/doc/$RPM_PACKAGE_NAME-%{swarmapps_version}/%1; cp %2 $RPM_BUILD_ROOT%{prefix}/doc/$RPM_PACKAGE_NAME-%{swarmapps_version}/%1; cd -

%define docheatbugs README ChangeLog COPYING
%install_doc heatbugs %docheatbugs

%define docmousetrap COPYING README ChangeLog
%install_doc mousetrap %docmousetrap

%define docmarket LICENSE ChangeLog
%install_doc market %docmarket

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%files
%defattr(-,root,root)
%{prefix}
%docdir %{prefix}/doc/$RPM_PACKAGE_NAME-%{swarmapps_version}/heatbugs
%docdir %{prefix}/doc/$RPM_PACKAGE_NAME-%{swarmapps_version}/mousetrap
%docdir %{prefix}/doc/$RPM_PACKAGE_NAME-%{swarmapps_version}/market

