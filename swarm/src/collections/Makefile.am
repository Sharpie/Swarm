MODULE = collections
CURRENT = 0
REVISION = 0
AGE = 0
collectionsdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I.. -I$(srcdir)/.. -I$(srcdir)/../defobj -I$(srcdir)/../misc

lib_LTLIBRARIES = libcollections.la

module_headers = classes.h types.h

BUILT_SOURCES = $(module_headers)

EXTRA_DIST = List_GEN.m

SRC = Array.m Collection.m InputStream.m List.m \
	List_linked.m List_mlinks.m Map.m OrderedSet.m OutputStream.m Set.m \
	String.m collections.m predicates.m

libcollections_la_SOURCES = $(SRC)
libcollections_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

collections_HEADERS = Array.h Collection.h InputStream.h List.h List_GEN.h \
	List_linked.h List_mlinks.h Map.h OrderedSet.h OutputStream.h Set.h \
	String.h collections.h predicates.h

install-data-local:
	$(INSTALL_DATA) $(module_headers) $(collectionsdir)
