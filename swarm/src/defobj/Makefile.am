MODULE = defobj
CURRENT = 0
REVISION = 1
AGE = 0
defobjdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

EXTRA_DIST = make-h2x m2h

INCLUDES = -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../misc

lib_LIBRARIES = libdefobj.a
lib_LTLIBRARIES = libdefobj.la

module_headers = classes.h types.h

BUILT_SOURCES = $(module_headers)

SRC = Create.m Customize.m DefClass.m DefObject.m Program.m \
	Symbol.m Zone.m defobj.m

libdefobj_a_SOURCES = $(SRC)
libdefobj_la_SOURCES = $(SRC)
libdefobj_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

noinst_SCRIPTS = m2h make-h2x

defobj_HEADERS = Create.h Customize.h DefClass.h DefObject.h Program.h \
	Symbol.h Zone.h defalloc.h deftype.h

install-data-local:
	$(INSTALL_DATA) classes.h $(defobjdir)
	$(INSTALL_DATA) types.h $(defobjdir)
