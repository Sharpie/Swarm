CURRENT = 0
REVISION = 1
AGE = 0
tclobjcdir = $(includedir)/tclobjc

TCLINCLUDES = @TCLINCLUDES@
TKINCLUDES = @TKINCLUDES@
FFIINCLUDES = @FFIINCLUDES@

SUFFIXES = .m

INCLUDES = -I$(srcdir)/../misc $(TKINCLUDES) $(TCLINCLUDES) $(FFIINCLUDES)

override CFLAGS += $(X_CFLAGS)
override CPPFLAGS += -DList=TclObjcList

lib_LTLIBRARIES = libtclobjc.la

SRC = TclInterp.m TkInterp.m tclObjc.m
EXTRA_libtclobjc_la_SOURCES = HashTable.m List.m objc-malloc.m

libtclobjc_la_SOURCES = $(SRC)
libtclobjc_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)
libtclobjc_la_LIBADD = @NON_NeXT_EXTRA_OBJS@ @OBJC_MALLOC_OBJ@
libtclobjc_la_DEPENDENCIES = $(libtclobjc_la_LIBADD)

noinst_HEADERS = HashTable.h List.h objc-gnu2next.h objc-malloc.h

tclobjc_HEADERS = TclInterp.h TkInterp.h tclObjc.h

headers:
