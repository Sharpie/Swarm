MODULE = defobj
defobjdir = $(includedir)/$(MODULE)

include $(top_srcdir)/Makefile.rules

INCLUDES = -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../misc

lib_LIBRARIES = libdefobj.a

module_headers = classes.h types.h

BUILT_SOURCES = $(module_headers)

libdefobj_a_SOURCES = Create.m Customize.m DefClass.m DefObject.m Program.m \
	Symbol.m Zone.m defobj.m

bin_SCRIPTS = m2h make-h2x

defobj_HEADERS = Create.h Customize.h DefClass.h DefObject.h Program.h \
	Symbol.h Zone.h defalloc.h deftype.h $(module_headers)

include_HEADERS = defobj.h