MODULE = collections
CURRENT = 2
REVISION = 0
AGE = 0
collectionsdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I. -I.. -I$(srcdir)/.. -I$(srcdir)/../defobj -I$(srcdir)/../misc -I$(srcdir)/../objectbase -I$(srcdir)/../random

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libcollections.la

BUILT_SOURCES = $(TYPEINFO)

EXTRA_DIST = List_GEN.m

SRC = Array.m Collection.m InputStream.m List.m \
	List_linked.m List_mlinks.m ListShuffler.m \
	Map.m OrderedSet.m OutputStream.m Set.m \
	Permutation.m StringObject.m collections.m predicates.m

libcollections_la_SOURCES = $(SRC)
libcollections_la_LDFLAGS = @inst_LDFLAGS@

collections_HEADERS = Array.h Collection.h InputStream.h List.h List_GEN.h \
	List_linked.h List_mlinks.h ListShuffler.h Map.h OrderedSet.h \
	OutputStream.h Set.h Permutation.h StringObject.h collections.h \
	predicates.h

install-data-local:
	$(INSTALL_DATA) classes.h $(collectionsdir)
	$(INSTALL_DATA) types.h $(collectionsdir)


