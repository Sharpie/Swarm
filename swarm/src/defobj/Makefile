SWARMHOME=../..
include $(SWARMHOME)/Makefile.conf
INCLIB=$(INCDIR)/defobj

OBJECTS= defobj.o defobj.o Program.o DefObject.o DefClass.o \
         Customize.o Create.o Zone.o Symbol.o

all: install-headers install-lib

clean:
	- rm -f defobj.x[cmt] $(OBJECTS)

defobj.o: defobj.xm

install-lib: install-headers $(LIBDIR)/libdefobj.a

$(LIBDIR)/libdefobj.a: $(OBJECTS)
	-rm -f $@
	ar qv $@ $(OBJECTS)
	$(RANLIB) $@

install-headers: \
	$(BINDIR) \
	$(BINDIR)/make-h2x \
	$(INCLIB) \
	$(INCDIR)/defobj.h \
	$(INCLIB)/types.h \
	$(INCLIB)/classes.h \
	$(INCLIB)/deftype.h \
	$(INCLIB)/Program.h \
	$(INCLIB)/DefObject.h \
	$(INCLIB)/DefClass.h \
	$(INCLIB)/Customize.h \
	$(INCLIB)/Create.h \
	$(INCLIB)/Zone.h \
	$(INCLIB)/Symbol.h

$(BINDIR):
	if [ ! -x $(BINDIR) ]; then mkdir $(BINDIR); fi
$(BINDIR)/make-h2x: make-h2x
	cp $? $@
$(INCLIB):
	if [ ! -x $(INCLIB) ]; then mkdir $(INCLIB); fi
$(INCDIR)/defobj.h: defobj.h
	cp $? $@
$(INCLIB)/types.h $(INCLIB)/classes.h: defobj.h
	@ rm -f defobj.xm; $(MAKE) defobj.xm
	cp defobj.xt $(INCLIB)/types.h
	cp defobj.xc $(INCLIB)/classes.h
$(INCLIB)/deftype.h: deftype.h
	cp $? $@
$(INCLIB)/Program.h: Program.h
	cp $? $@
$(INCLIB)/DefObject.h: DefObject.h
	cp $? $@
$(INCLIB)/DefClass.h: DefClass.h
	cp $? $@
$(INCLIB)/Customize.h: Customize.h
	cp $? $@
$(INCLIB)/Create.h: Create.h
	cp $? $@
$(INCLIB)/Zone.h: Zone.h
	cp $? $@
$(INCLIB)/Symbol.h: Symbol.h
	cp $? $@
