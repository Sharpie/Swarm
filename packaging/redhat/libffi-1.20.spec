%define ver      1.20
%define rel      2
%define prefix   /usr

Summary: Foreign function interface: library for language interoperability
Name: libffi
Version: %ver
Release: %rel
Copyright: LGPL
Group: Development/Libraries
Vendor: Swarm Development Group
Source: ftp://ftp.cygnus.com:/pub/green/libffi-1.20.tar.gz
BuildRoot: /tmp/libffi-root
Packager: Paul Johnson <pauljohn@ukans.edu>
URL: http://www.cygnus.com/~green/libffi.html


%description
Packaged for Swarm User convenience. Installs into /usr. From the
homepage, "The libffi library provides a portable, high level
programming interface to various calling conventions. This allows a
programmer to call any function specified by a call interface
description at run-time." Libffi was written by Anthony Green.

%changelog

* Tue Sep 28 1999 Alex Lancaster <alex@santafe.edu>

- Added Vendor tag, bumped Release to 2.

- Just use %prefix in %files list, RPM will automatically package all
  files in subdirectories.  Dont create symbolic link manually.

* Sat Jan 9 1999 Paul E. Johnson <pauljohn@ukans.edu>

- Updated for libffi 1.20

* Mon Aug 10 1998 Paul E. Johnson <pauljohn@ukans.edu>

- Used imlib.spec as template for libffi.

%prep

%setup

%build

./configure --prefix=%{prefix}
make
			    
%install
rm -rf $RPM_BUILD_ROOT
mkdir $RPM_BUILD_ROOT
mkdir $RPM_BUILD_ROOT%{prefix}
make prefix=$RPM_BUILD_ROOT%{prefix} install

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%files
%defattr(-, root, root)
%{prefix}

