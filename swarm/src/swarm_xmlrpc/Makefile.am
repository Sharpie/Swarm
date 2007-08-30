MODULE = swarm_xmlrpc
CURRENT = 4
REVISION = 0
AGE = 0
swarm_xmlrpcdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules
#AM_OBJCFLAGS = -g -O0

INCLUDES = $(LIBOBJCINCLUDES) -I.. -I$(srcdir)/.. -I$(srcdir)/../misc -I$(top_builddir)/avcall -I$(srcdir)/../defobj -I$(srcdir)/../collections -I$(srcdir)/../activity -I$(srcdir)/../objectbase $(EXPAT_INCLUDE)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@ 

#libswarm_xmlrpc_la_LIBADD = ./expat/xmltok/libexpat_tok.la ./expat/xmlparse/libexpat_parse.la 

EXTRA_LTLIBRARIES = libswarm_xmlrpc.la
EMACS = @EMACS@

SRC = SwarmXMLRPCClient.m SwarmXMLRPCServer.m SwarmXMLRPCFProxy.m swarm_xmlrpc.m \
		base64.m encodings.m queue.m simplestring.m xml_element.m \
		xml_to_dandarpc.m xml_to_xmlrpc.m xml_to_soap.m xmlrpc.m xmlrpc_introspection.m \
		system_methods.m

libswarm_xmlrpc_la_SOURCES = $(SRC)
libswarm_xmlrpc_la_LDFLAGS = @inst_LDFLAGS@ $(EXPAT_LIB)
LIBS = -lexpat

swarm_xmlrpc_HEADERS = SwarmXMLRPCClient.h SwarmXMLRPCServer.h SwarmXMLRPCFProxy.h \
base64.h encodings.h queue.h simplestring.h xml_element.h xml_to_dandarpc.h xml_to_xmlrpc.h xmlrpc.h \
xmlrpc_introspection.h
