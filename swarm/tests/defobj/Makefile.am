include $(top_builddir)/etc/Makefile.tests

check_PROGRAMS = archiver

archiver_SOURCES = archiver.m

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = SWARM_INHIBIT_ARCHIVER_LOAD=1
