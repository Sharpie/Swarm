SWARMHOME=../..
include $(SWARMHOME)/Makefile.conf

OBJECTS= activity.o CompoundAction.o ActionGroup.o Schedule.o SwarmProcess.o \
         Action.o XActivity.o 

INCLIB = $(INCDIR)/activity

all: install-headers install-lib

clean:
	- rm -rf ,m2h activity.x[ctm] $(OBJECTS)

activity.o: activity.xm
ActionGroup.o: CompoundAction.m
Schedule.o: CompoundAction.m

install-lib: install-headers $(LIBDIR)/libactivity.a
$(LIBDIR)/libactivity.a: $(OBJECTS)
	-rm -f $@
	ar qv $@ $(OBJECTS)
	$(RANLIB) $@

install-headers: \
         $(INCDIR)/activity.h \
         $(INCLIB) \
         $(INCLIB)/types.h \
         $(INCLIB)/classes.h \
         $(INCLIB)/CompoundAction.h \
         $(INCLIB)/ActionGroup.h \
         $(INCLIB)/Schedule.h \
         $(INCLIB)/SwarmProcess.h \
         $(INCLIB)/Action.h \
         $(INCLIB)/XActivity.h

$(INCDIR)/activity.h: activity.h
	cp $? $@
$(INCLIB):
	if [ ! -x $(INCLIB) ]; then mkdir $(INCLIB); fi
$(INCLIB)/types.h $(INCLIB)/classes.h: activity.h
	@ rm -f activity.xm; $(MAKE) activity.xm
	cp activity.xt $(INCLIB)/types.h 
	cp activity.xc $(INCLIB)/classes.h 
$(INCLIB)/CompoundAction.h: CompoundAction.h
	cp $? $@
$(INCLIB)/ActionGroup.h: ActionGroup.h
	cp $? $@
$(INCLIB)/Schedule.h: Schedule.h
	cp $? $@
$(INCLIB)/SwarmProcess.h: SwarmProcess.h
	cp $? $@
$(INCLIB)/Action.h: Action.h
	cp $? $@
$(INCLIB)/XActivity.h: XActivity.h
	cp $? $@
