SWARMHOME=.
include Makefile.conf

all: 
	(cd src; $(MAKE) all)
clean:
	(cd src; $(MAKE) clean)
realclean: clean
	- rm -f $(LIBDIR)/* $(BINDIR)/* MakeOut
	- rm -rf $(INCDIR)/*
