MODULE = collections
collectionsdir = $(includedir)/$(MODULE)

include $(top_srcdir)/Makefile.rules

INCLUDES = -I.. -I$(srcdir)/.. -I$(srcdir)/../defobj

lib_LIBRARIES = libcollections.a

module_headers = classes.h types.h

BUILT_SOURCES = $(module_headers)

libcollections_a_SOURCES = Array.m Collection.m InputStream.m List.m \
	List_linked.m List_mlinks.m Map.m OrderedSet.m OutputStream.m Set.m \
	String.m collections.m predicates.m

collections_HEADERS = Array.h Collection.h InputStream.h List.h List_GEN.h \
	List_linked.h List_mlinks.h Map.h OrderedSet.h OutputStream.h Set.h \
	String.h collections.h predicates.h $(module_headers)

include_HEADERS = $(MODULE).h
