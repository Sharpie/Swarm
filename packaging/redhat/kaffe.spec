Summary: A free virtual machine for running Java(TM) code.
Name: kaffe
Version: 2000.02.26
Release: 1
Serial: 1
Copyright: GPL
Url: http://www.kaffe.org/
Group: Development/Languages
Source0: ftp://ftp.transvirtual.com/pub/kaffe/kaffe-%{version}.tar.gz
Obsoletes: kaffe-bissawt
Buildroot: /var/tmp/kaffe-root
AutoReqProv: no

%description
Kaffe is a cleanroom, open source implementation of a Java virtual
machine and class libraries.

%prep
%setup -q -n kaffe-%{version}
%build
PATH=/packages/bin:$PATH
export PATH
%define configure CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=/usr --libdir=/usr/lib/kaffe
%ifarch ppc
%configure --with-libffi
%else
%configure
%endif
make

%install
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT/usr libdir=$RPM_BUILD_ROOT/usr/lib/kaffe \
	libexecdir=$RPM_BUILD_ROOT/usr/libexec \
	nativedir=$RPM_BUILD_ROOT/usr/lib/kaffe \
	classdir=$RPM_BUILD_ROOT/usr/share/kaffe install
%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-,root,root)
%doc README FAQ license.terms developers
/usr/libexec/Kaffe
/usr/lib/kaffe
/usr/bin/*
/usr/man/*/*
/usr/share/kaffe
/usr/include/kaffe

%changelog
* Wed Feb 02 2000 Cristian Gafton <gafton@redhat.com>
- fix the description

* Mon Oct 18 1999 Bill Nottingham <notting@redhat.com>
- 1.0.5. Lets see what it does.

* Mon Apr 12 1999 Bill Nottingham <notting@redhat.com>
- build for alpha (it seems to work...)
- fix paths so it works

* Mon Apr 12 1999 Preston Brown <pbrown@redhat.com>
- according to the kaffe people, b4 is a "massive bugfix release"

* Sun Mar 21 1999 Cristian Gafton <gafton@redhat.com> 
- auto rebuild in the new build environment (release 4)

* Thu Mar 18 1999 Bill Nottingham <notting@redhat.com>
- strip binaries

* Tue Dec 15 1998 Bill Nottingham <notting@redhat.com>
- add an Obsoletes: for kaffe-bissawt

* Wed Dec  9 1998 Bill Nottingham <notting@redhat.com>
- update to 1.0b3
- include alpha patch, but it's still broke

* Mon Oct 05 1998 Cristian Gafton <gafton@redhat.com>
- added sparc to the list of supported architectures
- update to 1.0b2

* Tue Sep 22 1998 Bill Nottingham <notting@redhat.com>
- don't rename libraries; install them in /usr/lib/kaffe
- remove sparc arch (doesn't work)

* Thu Jul 23 1998 Jeff Johnson <jbj@redhat.com>
- update to 1.0.b1.
- add sparc arch (alpha has problems kaffe/kaffeevm/support.c:{343,518}

* Fri May 08 1998 Prospector System <bugs@redhat.com>
- translations modified for de, fr, tr

* Mon May 04 1998 Cristian Gafton <gafton@redhat.com>
- finally their ftp site is up again: updated to 0.10.0
- too bad the Biss-AWT doesn't seem to be maintained anymore... Removed the
  bissawt package
- unfortunately alpha and sparc assembler code that reference registers
  like eax, ebx, etc. makes this package ExclusiveArch: i386

* Tue Dec 09 1997 Cristian Gafton <gafton@redhat.com>
- added kaffe to the file list
- added BuildRoot; cleaned the spec file

* Tue Nov 11 1997 Michael K. Johnson <johnsonm@redhat.com>
- removed pieces with incompatible licenses

* Tue Oct 21 1997 Erik Troan <ewt@redhat.com>
- updated to 0.9.2

* Mon Aug 25 1997 Erik Troan <ewt@redhat.com>
- built against glibc

* Thu Apr 24 1997 Erik Troan <ewt@redhat.com>
- added libkaffe_vm.so symlink.

* Tue Apr 22 1997 Erik Troan <ewt@redhat.com>
- added manual provide of libkaffe_vm.so (RPM seems a bit broken).
