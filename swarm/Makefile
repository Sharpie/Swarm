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
TMPDIST=/tmp/swarmdist
SRCDIST=/tmp/swarmsrc.tar
BINDIST=/tmp/swarmbin.tar

distrib: realclean
	- rm -rf $(TMPDIST)
	- mkdir $(TMPDIST)
	tar cf - . | (cd $(TMPDIST); tar xplf -)
	(cd $(TMPDIST); cp Makefile.conf Makefile.oconf; chmod +w Makefile.conf)
	(cd $(TMPDIST)/src; $(MAKE) $(MFLAGS) headers) 
	(cd $(TMPDIST); $(MAKE) $(MFLAGS) SETUP=$(SETUP) SYS=$(SYS) CONFIG=archive OUT=$(SRCDIST) dist)
	(cd $(TMPDIST); $(MAKE) $(MFLAGS) SETUP=$(SETUP) SYS=$(SYS) CONFIG=bin OUT=$(BINDIST) bindist)

### "dist" should only be called from "make distrib"
dist: 
ifeq ($(OUT),"")
	@ echo "make dist" should only be called from "make distrib"
else
	sed -e 's/SYS=.*/SYS=$(SYS)/' \
	    -e 's/SETUP=.*/SETUP=$(SETUP)/' \
	    -e 's/CONFIG=.*/CONFIG=$(CONFIG)/' Makefile.oconf > Makefile.conf
	tar cvf $(OUT) .
endif

bindist: binary clean dist
