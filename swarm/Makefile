SWARMHOME=.
include Makefile.conf

all: 
	(cd src; $(MAKE) all)
clean:
	(cd src; $(MAKE) clean)

realclean: clean
	- rm -f $(LIBDIR)/* $(BINDIR)/* MakeOut
	- rm -rf $(INCDIR)/*
	- rm -f src/tmpobj/*

binary: FORCE
	(cd src; $(MAKE) binary)

FORCE:

###
### "distributions" are tarballs that contain source/binary distributions
### for swarm, set up with their standard configuration
SWARMDIST=swarm-1.0.0

TMPDIR=/tmp
TMPDIST=$(TMPDIR)/$(SWARMDIST)
SRCDIST=/tmp/swarmsrc.tar
BINDIST=/tmp/swarmbin.tar

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
	sed -e 's/SYS=$$/SYS=$(SYS)/' \
	    -e 's/SETUP=.*/SETUP=$(SETUP)/' \
	    -e 's/LDCONFIG=.*/LDCONFIG=$(LDCONFIG)/' Makefile.oconf > Makefile.conf
	(cd ..; tar cvf $(OUT) $(SWARMDIST))
endif

### copy selected tcl/tk/blt/tclobjc include files and
### the tcl runtime files, so binary-only
### clients will have a complete software environment
BININCDIRS=$(TCLOBJCINC) $(TCLINC) $(TKINC) $(BLTINC) 
TKTCLDIRS=$(TKLIB) $(TCLLIB)
bininc: FORCE
	for d in $(BININCDIRS); do \
	  cp $$d/*.h $(TMPDIST)/include/; \
	done
	(cd $(XINC); tar cf - X11/xpm.h) | (cd $(TMPDIST)/include/; tar xf -)
	for d in $(TKTCLDIRS); do \
	  cp $$d/*.tcl $(TMPDIST)/lib/; \
	done
	cp -r $(BLTLIB)/* $(TMPDIST)/lib

### make a binary, some libs, get rid of the .o's, copy
### system header files, then make a tar file with the
### SETUP set to NONE...
bindist: binary all clean bininc 
	$(MAKE) SYS=$(SYS) SETUP=NONE LDCONFIG=$(LDCONFIG) dist
