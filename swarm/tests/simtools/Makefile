#SWARMHOME=../../..
LIBNAME=simtools

OTHERCLEAN=ObjectLoaderTest ObjectLoaderTest.o TestObject TestObject.o AppendFileTest AppendFileTest.o

include $(SWARMHOME)/Makefile.conf
all: ObjectLoaderTest AppendFileTest QSortTest
ObjectLoaderTest: ObjectLoaderTest.o TestObject.o $(LIBRARY)
	$(CC)  $(LDFLAGS) -o ObjectLoaderTest ObjectLoaderTest.o TestObject.o $(LIBS)
ObjectLoaderTest: ObjectLoaderTest.o TestObject.o $(LIBRARY)
	$(CC)  $(LDFLAGS) -o ObjectLoaderTest ObjectLoaderTest.o TestObject.o $(LIBS)
AppendFileTest: AppendFileTest.o $(LIBRARY)
	$(CC)  $(LDFLAGS) -o AppendFileTest AppendFileTest.o $(LIBS)
QSortTest: QSortTest.o $(LIBRARY)
	$(CC)  $(LDFLAGS) -o QSortTest QSortTest.o $(LIBS)

