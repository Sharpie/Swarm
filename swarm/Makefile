SWARMHOME=.
include Makefile.conf

all: 
	(cd src; $(MAKE) all)
clean:
	(cd src; $(MAKE) clean)

realclean: clean
	-(cd $(LIBDIR); rm -rf `ls -1 | grep -v CVS`)
	-(cd $(INCDIR); rm -rf `ls -1 | grep -v CVS`)
	-(cd $(BINDIR); rm -f `ls -1 | grep -v CVS`)
	-rm -f src/tmpobj/*

binary: FORCE
	(cd src; $(MAKE) binary)

FORCE:

###
### "distributions" are tarballs that contain source/binary distributions
### for swarm, set up with their standard configuration
SWARMDIST=swarm-$(VERSION)

ifeq ($(SWARMTMPDIR),)
TMPDIR=/tmp
else
TMPDIR=$(SWARMTMPDIR)
endif
TMPDIST=$(TMPDIR)/$(SWARMDIST)
SRCDIST=$(TMPDIR)/swarmsrc.tar
BINDIST=$(TMPDIR)/swarmbin.tar

distrib: realclean
	- rm -rf $(TMPDIST)
	- mkdir $(TMPDIST)
	tar cf - . | (cd $(TMPDIST); tar xplf -)
	(cd $(TMPDIST); cp Makefile.conf Makefile.oconf; chmod +w Makefile.conf)
	(cd $(TMPDIST)/src; $(MAKE) $(MFLAGS) headers) 
	(cd $(TMPDIST); $(MAKE) $(MFLAGS) SETUP=NONE SYS=NONE LDCONFIG=archive OUT=$(SRCDIST) dist)
	(cd $(TMPDIST); $(MAKE) $(MFLAGS) SETUP=$(SETUP) SYS=$(SYS) LDCONFIG=bin OUT=$(BINDIST) bindist)
	- rm -rf $(TMPDIST)

### "dist" should only be called from "make distrib"
dist: 
ifeq ($(OUT),"")
	@ echo "make dist" should only be called from "make distrib"
else
	sed -e 's,^SYS=$$,SYS=$(SYS),' \
	    -e 's,^SETUP=[^$$].*,SETUP=$(SETUP),' \
	    -e 's,^LDCONFIG=.*,LDCONFIG=$(LDCONFIG),' Makefile.oconf > Makefile.conf
	(cd ..; tar cvf $(OUT) $(SWARMDIST))
endif

### copy selected tcl/tk/blt/tclobjc include files and
### the tcl runtime files, so binary-only
### clients will have a complete software environment
BININCDIRS=$(TCLOBJCINCDIR) $(TCLINCDIR) $(TKINCDIR) $(BLTINCDIR) 
TKTCLDIRS=$(TKLIBDIR) $(TCLLIBDIR)
bininc: FORCE
	for d in $(BININCDIRS); do \
	  cp $$d/*.h $(TMPDIST)/include/; \
	done
	test -d $(XINCDIR)/X11/xpm.h && (cd $(XINCDIR); tar cf - X11/xpm.h) | (cd $(TMPDIST)/include/; tar xf -)
	for d in $(TKTCLDIRS); do \
	  cp $$d/*.tcl $(TMPDIST)/lib/; \
	done
	if test -d $(BLTLIBRARYDIR) ; then cp -r $(BLTLIBRARYDIR) $(TMPDIST) ; else cp -r $(BLTLIBDIR)/* $(TMPDIST)/lib ; fi

### make a binary, some libs, get rid of the .o's, copy
### system header files, then make a tar file with the
### SETUP set to NONE...
bindist: binary all clean bininc 
	$(MAKE) SYS=$(SYS) SETUP=NONE LDCONFIG=$(LDCONFIG) dist
