DIRS=defobj collections activity random tkobjc swarmobject simtools analysis \
     space 

all:    headers libs
clean:
	- for f in $(DIRS); do (cd $$f; $(MAKE) clean); done
headers:
	- for f in $(DIRS); do (cd $$f; $(MAKE) install-headers); done
libs:
	- for f in $(DIRS); do (cd $$f; $(MAKE) install-lib); done

include ../Makefile.conf
SWARMHOME=../..
TMPOBJDIR=tmpobj
TMPOBJSUBDIR=$(TMPOBJDIR)/obj

###
### Make the monolithic binary that can be used to link swarm applications
### for those sites that don't want to screw around with compiling from src

binary:  $(TMPOBJDIR) $(TMPOBJSUBDIR) distrib-files
	@ echo "DIST: Extracting individual objects"
	- rm -f $(TMPOBJSUBDIR)/*
	cd $(TMPOBJSUBDIR); \
	for f in ../*.a; do (echo $$f; ar x $$f); done
	@ echo " "
	@ echo "DIST: Linking $(DISTOBJ)"
	cd $(TMPOBJDIR); ld $(MINLDFLAGS) -r -o $(DISTOBJ) obj/* $(SYSLIBS)
	rm -f $(TMPOBJSUBDIR)/* $(TMPOBJDIR)/*.a

### make and unpack the libraries into our temp object space 
distrib-files: $(TMPOBJDIR)
	@ echo "DIST: Making Swarm Archives"
	for f in $(DIRS); do (cd $$f; $(MAKE) install-lib LIBDIR=../$(TMPOBJDIR)); done	
	@ echo " "

$(TMPOBJDIR):
	-mkdir $(TMPOBJDIR)

$(TMPOBJSUBDIR):
	-mkdir $(TMPOBJSUBDIR)
