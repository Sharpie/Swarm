MODULE = collections
CURRENT = 1
REVISION = 1
AGE = 0
collectionsdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I. -I.. -I$(srcdir)/.. -I$(srcdir)/../defobj -I$(srcdir)/../misc -I$(srcdir)/../objectbase -I$(srcdir)/../activity -I$(srcdir)/../random

lib_LTLIBRARIES = libcollections.la

module_headers = classes.h types.h collections.xm

BUILT_SOURCES = $(module_headers)

EXTRA_DIST = List_GEN.m

SRC = Array.m Collection.m InputStream.m List.m \
	List_linked.m List_mlinks.m ListShuffler.m \
	Map.m OrderedSet.m OutputStream.m Set.m \
	Permutation.m StringObject.m collections.m predicates.m

libcollections_la_SOURCES = $(SRC)
libcollections_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

collections_HEADERS = Array.h Collection.h InputStream.h List.h List_GEN.h \
	List_linked.h List_mlinks.h ListShuffler.h Map.h OrderedSet.h \
	OutputStream.h Set.h Permutation.h StringObject.h collections.h \
	predicates.h

install-data-local:
	$(INSTALL_DATA) classes.h $(collectionsdir)
	$(INSTALL_DATA) types.h $(collectionsdir)
