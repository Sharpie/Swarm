SWARMHOME=../..
include $(SWARMHOME)/Makefile.conf

OBJECTS= activity.o ActionPlan.o ActionGroup.o Schedule.o GenericSwarm.o \
        Action.o Activity.o 

INCLIB = $(INCDIR)/activity

all: install-headers install-lib

clean:
	- rm -f activity.x[ctm] *.o *.a core

activity.o: activity.xm
ActionGroup.o: ActionPlan.m
Schedule.o: ActionPlan.m

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
         $(INCLIB)/ActionPlan.h \
         $(INCLIB)/ActionGroup.h \
         $(INCLIB)/Schedule.h \
         $(INCLIB)/GenericSwarm.h \
         $(INCLIB)/Activity.h \
         $(INCLIB)/Action.h

$(INCDIR)/activity.h: activity.h
	cp $? $@
$(INCLIB):
	if [ ! -x $(INCLIB) ]; then mkdir $(INCLIB); fi
$(INCLIB)/types.h $(INCLIB)/classes.h: activity.h
	@ rm -f activity.xm; $(MAKE) activity.xm
	cp activity.xt $(INCLIB)/types.h 
	cp activity.xc $(INCLIB)/classes.h 
$(INCLIB)/ActionPlan.h: ActionPlan.h
	cp $? $@
$(INCLIB)/ActionGroup.h: ActionGroup.h
	cp $? $@
$(INCLIB)/Schedule.h: Schedule.h
	cp $? $@
$(INCLIB)/GenericSwarm.h: GenericSwarm.h
	cp $? $@
$(INCLIB)/Action.h: Action.h
	cp $? $@
$(INCLIB)/Activity.h: Activity.h
	cp $? $@
