%define ver      1.2.1
%define rel      1
%define prefix   /usr

Summary: HDF5 data storage library
Name: hdf5
Version: %ver
Release: %rel
Source: hdf5-%{ver}.tar.gz
%ifarch sparc
Patch: hdf5-1.2.1.sparc.patch
%endif
Copyright: gpl
Group: Development/Languages
Packager: Marcus G. Daniels <mgd@swarm.org>
URL: http://hdf.ncsa.uiuc.edu/HDF5/
Prefix: %{prefix}
BuildRoot: /tmp/hdf5
#Requires: sed
#Conflicts: R-contrib <= 1998.11.16
#Conflicts: R-VR <= 5.3pl027

%description 

HDF5 is portable binary storage mechanism for hierarchial data, especially
suited toward high performance access to scientific datasets.

%prep

%setup -q # -n hdf5-%{ver}

%ifarch sparc
%patch -p1
%endif

CC=gcc ./configure --prefix=%{prefix} 

%build
make 

%install

mkdir -p $RPM_BUILD_ROOT/%{prefix}


make prefix=${RPM_BUILD_ROOT}/%{prefix} install


%files 
%defattr(-, root, root)
%{prefix}

%doc COPYING INSTALL README RELEASE
%doc MANIFEST INSTALL_MAINT INSTALL_parallel 

%clean
rm -rf $RPM_BUILD_ROOT


%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%preun

