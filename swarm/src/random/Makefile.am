MODULE = random
CURRENT = 0
REVISION = 0
AGE = 0
randomdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

noinst_HEADERS =  include.dists.creating.m include.dists.setting.m \
        include.dists.using.m include.gens.creating.m \
        include.gens.setting.m include.gens.using.m \
        include.xgens.creating.m include.xgens.setting.m \
        include.xgens.using.m

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../misc -I$(srcdir)/../objectbase -I.. -I$(srcdir)/../activity -I$(srcdir)/..

lib_LTLIBRARIES = librandom.la

SRC = ACGgen.m BernoulliDist.m C2LCGXgen.m C2MRG3gen.m \
	C2TAUSgen.m C3MWCgen.m C4LCGXgen.m ExponentialDist.m GammaDist.m \
	LCGgen.m LogNormalDist.m MRGgen.m MT19937gen.m MWCAgen.m MWCBgen.m \
	NormalDist.m PMMLCGgen.m PSWBgen.m RWC2gen.m RWC8gen.m \
	RandomBitDist.m SCGgen.m SWBgen.m TGFSRgen.m UniformDoubleDist.m \
	UniformIntegerDist.m UniformUnsignedDist.m random.m

librandom_la_SOURCES = $(SRC)
librandom_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

random_HEADERS = ACGgen.h BernoulliDist.h C2LCGXgen.h C2MRG3gen.h C2TAUSgen.h \
	C3MWCgen.h C4LCGXgen.h ExponentialDist.h GammaDist.h LCGgen.h \
	LogNormalDist.h MRGgen.h MT19937gen.h MWCAgen.h MWCBgen.h \
	NormalDist.h PMMLCGgen.h PSWBgen.h RWC2gen.h RWC8gen.h \
	RandomBitDist.h randomdefs.h randomvars.h SCGgen.h SWBgen.h \
	TGFSRgen.h UniformDoubleDist.h UniformIntegerDist.h \
	UniformUnsignedDist.h distributions.h generators.h
