MODULE = tclobjc
CURRENT = 1
REVISION = 3
AGE = 1
tclobjcdir = $(includedir)/Swarm/tclobjc

TCLINCLUDES = @TCLINCLUDES@
TKINCLUDES = @TKINCLUDES@
XINCLUDES = @XINCLUDES@
FFIINCLUDES = @FFIINCLUDES@

tcllibdir = @tcllibdir@
TCL_LIBRARY = @tclscriptdir@

tklibdir = @tklibdir@
TK_LIBRARY = @tkscriptdir@

SUFFIXES = .m

INCLUDES = $(LIBOBJCINCLUDES) -I$(srcdir)/../misc -I.. -I$(srcdir)/.. -I$(srcdir)/../defobj $(TKINCLUDES) $(TCLINCLUDES) $(FFIINCLUDES) $(XINCLUDES)

# Solaris 2.8 has a pragma in Xlib.h
override OBJCFLAGS += $(X_CFLAGS) @CFLAGS_NOWARN_IMPLICIT_INT@ @CFLAGS_NOWARN_UNKNOWN_PRAGMAS@ 
override CPPFLAGS += -DList=TclObjcList -DTCL_LIBRARY=\"$(TCL_LIBRARY)\" -DTK_LIBRARY=\"$(TK_LIBRARY)\" -DBUILDING_SWARM

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libtclobjc.la

SRC = TclInterp.m TkInterp.m tclObjc.m
EXTRA_libtclobjc_la_SOURCES = HashTable.m List.m objc-malloc.m

libtclobjc_la_SOURCES = $(SRC)
libtclobjc_la_LDFLAGS = @inst_LDFLAGS@
libtclobjc_la_LIBADD = @NON_NeXT_EXTRA_OBJS@ # @OBJC_MALLOC_OBJ@
libtclobjc_la_DEPENDENCIES = $(libtclobjc_la_LIBADD)

noinst_HEADERS = HashTable.h List.h objc-gnu2next.h objc-malloc.h

tclobjc_HEADERS = TclInterp.h TkInterp.h tclObjc.h

headers:
