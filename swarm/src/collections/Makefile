SWARMHOME=../..
SWARMHOME=../..
include $(SWARMHOME)/Makefile.conf
INCLIB=$(INCDIR)/collections

LISTOBJECTS = List_linked.o List_mlinks.o

OBJECTS= collections.o Collection.o Array.o List.o $(LISTOBJECTS) \
         Map.o Set.o OrderedSet.o String.o InputStream.o OutputStream.o \
         predicates.o

all: install-headers install-lib

clean:
	- rm -rf ,m2h collections.x[cmt] $(OBJECTS)

collections.o: collections.xm

$(LISTOBJECTS): List_GEN.m List_GEN.h

install-lib: install-headers $(LIBDIR)/libcollections.a

$(LIBDIR)/libcollections.a: $(OBJECTS)
	-rm -f $@
	ar qv $@ $(OBJECTS)
	$(RANLIB) $@

install-headers: \
         $(INCDIR)/collections.h \
         $(INCLIB) \
         $(INCLIB)/types.h \
         $(INCLIB)/classes.h \
         $(INCLIB)/Collection.h \
         $(INCLIB)/Array.h \
         $(INCLIB)/List.h \
         $(INCLIB)/List_GEN.h \
         $(INCLIB)/List_linked.h \
         $(INCLIB)/List_mlinks.h \
         $(INCLIB)/Map.h \
         $(INCLIB)/OrderedSet.h \
         $(INCLIB)/Set.h \
         $(INCLIB)/String.h \
	 $(INCLIB)/InputStream.h \
         $(INCLIB)/OutputStream.h \
         $(INCLIB)/predicates.h

$(INCDIR)/collections.h: collections.h
	cp $? $@
$(INCLIB):
	if [ ! -x $(INCLIB) ]; then mkdir $(INCLIB); fi
$(INCLIB)/types.h $(INCLIB)/classes.h: collections.h
	@ rm -f collections.xm; $(MAKE) collections.xm
	cp collections.xt $(INCLIB)/types.h 
	cp collections.xc $(INCLIB)/classes.h 
$(INCLIB)/Collection.h: Collection.h
	cp $? $@
$(INCLIB)/Array.h: Array.h
	cp $? $@
$(INCLIB)/List.h: List.h
	cp $? $@
$(INCLIB)/List_GEN.h: List_GEN.h
	cp $? $@
$(INCLIB)/List_linked.h: List_linked.h
	cp $? $@
$(INCLIB)/List_mlinks.h: List_mlinks.h
	cp $? $@
$(INCLIB)/Map.h: Map.h
	cp $? $@
$(INCLIB)/OrderedSet.h: OrderedSet.h
	cp $? $@
$(INCLIB)/Set.h: Set.h
	cp $? $@
$(INCLIB)/String.h: String.h
	cp $? $@
$(INCLIB)/OutputStream.h: OutputStream.h
	cp $? $@
$(INCLIB)/InputStream.h: InputStream.h
	cp $? $@
$(INCLIB)/predicates.h: predicates.h
	cp $? $@
