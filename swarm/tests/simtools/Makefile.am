include $(top_builddir)/etc/Makefile.tests

check_PROGRAMS = ObjectLoaderTest AppendFileTest QSortTest

ObjectLoaderTest_SOURCES = ObjectLoaderTest.m TestObject.h TestObject.m
AppendFileTest_SOURCES = AppendFileTest.m TestObject.h TestObject.m
QSortTest_SOURCES = QSortTest.m

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = SWARM_INHIBIT_ARCHIVER_LOAD=1

EXTRA_DIST = test.data File.test