%define ver      1.4.4
%define rel      1RH80 
%define prefix   /usr

Summary: HDF5 data storage library
Name: hdf5
Version: %ver
Release: %rel
Source0: hdf5-%{ver}.tar.gz
Copyright: gpl
Group: Development/Languages
Packager: Paul Johnson <pauljohn@ukans.edu>
URL: http://hdf.ncsa.uiuc.edu/HDF5/
Prefix: %{prefix}
BuildRoot: /tmp/hdf5
#Requires: sed
#Conflicts: R-contrib <= 1998.11.16
#Conflicts: R-VR <= 5.3pl027


%description 

HDF5 is a new, experimental version of HDF that is designed to address some of the limitations of the current version of HDF (HDF4.x) and to
address current and anticipated requirements of modern systems and applications. 


%prep
%setup -n hdf5-%{ver}


%build

rm -rf builddir
mkdir builddir
cd builddir
CC=gcc ../configure --disable-hsizet --prefix=%{prefix} --host=i386-pc-linux 
make 

%install

mkdir -p $RPM_BUILD_ROOT/%{prefix}

cd builddir
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

