include $(top_builddir)/etc/Makefile.tests

check_PROGRAMS = archiver forwarding

archiver_SOURCES = archiver.m
forwarding_SOURCES = forwarding.m

TESTS = $(check_PROGRAMS)
TESTS_ENVIRONMENT = SWARM_INHIBIT_ARCHIVER_LOAD=1
