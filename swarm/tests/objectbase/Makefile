LIBNAME=simtools

OTHERCLEAN=ProbeTest.o 

include $(SWARMHOME)/Makefile.conf
all: ProbeTest
ProbeTest: ProbeTest.o $(LIBRARY)
	$(CC)  $(LDFLAGS) -o ProbeTest ProbeTest.o $(LIBS)

