MODULE = collections
CURRENT = 3
REVISION = 1
AGE = 0
collectionsdir = $(includedir)/Swarm

include $(top_srcdir)/src/Makefile.rules

INCLUDES = $(LIBOBJCINCLUDES) -I$(top_builddir) -I. -I.. -I$(srcdir)/.. -I$(srcdir)/../defobj -I$(srcdir)/../misc -I$(srcdir)/../objectbase -I$(srcdir)/../random $(JAVAINCLUDES) $(GNUSTEPLIBINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libcollections.la

CLEAN_FILES = $(TYPEINFO)

BUILT_SOURCES = $(TYPEINFO)

EXTRA_DIST = List_GEN.m List_HDF5in.m List_HDF5out.m

SRC = Array.m Collection.m InputStream.m Stream.m List.m List_linked.m List_mlinks.m ListShuffler.m Map.m OrderedSet.m OutputStream.m Set.m Permutation.m StringObject.m collections.m predicates.m

libcollections_la_SOURCES = $(SRC)
libcollections_la_LDFLAGS = @inst_LDFLAGS@

collections_HEADERS = $(MODULE).h Array.h Collection.h InputStream.h Stream.h List.h List_GEN.h List_linked.h List_mlinks.h ListShuffler.h Map.h OrderedSet.h OutputStream.h Set.h Permutation.h StringObject.h predicates.h macros.h

install-data-local:
	$(INSTALL_DATA) $(MODULE)_classes.h $(DESTDIR)$(collectionsdir)
	$(INSTALL_DATA) $(MODULE)_types.h $(DESTDIR)$(collectionsdir)


