MODULE = random
randomdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

INCLUDES = -I$(srcdir)/../collections -I$(srcdir)/../defobj -I.. -I$(srcdir)/..

lib_LIBRARIES = librandom.a

librandom_a_SOURCES = ACGgen.m BernoulliDist.m C2LCGXgen.m C2MRG3gen.m \
	C2TAUSgen.m C3MWCgen.m C4LCGXgen.m ExponentialDist.m GammaDist.m \
	LCGgen.m LogNormalDist.m MRGgen.m MT19937gen.m MWCAgen.m MWCBgen.m \
	NormalDist.m PMMLCGgen.m PSWBgen.m RWC2gen.m RWC8gen.m \
	RandomBitDist.m SCGgen.m SWBgen.m TGFSRgen.m UniformDoubleDist.m \
	UniformIntegerDist.m UniformUnsignedDist.m random.m

random_HEADERS = ACGgen.h BernoulliDist.h C2LCGXgen.h C2MRG3gen.h C2TAUSgen.h \
	C3MWCgen.h C4LCGXgen.h ExponentialDist.h GammaDist.h LCGgen.h \
	LogNormalDist.h MRGgen.h MT19937gen.h MWCAgen.h MWCBgen.h \
	NormalDist.h PMMLCGgen.h PSWBgen.h RWC2gen.h RWC8gen.h \
	RandomBitDist.h RandomDefs.h RandomVars.h SCGgen.h SWBgen.h \
	TGFSRgen.h UniformDoubleDist.h UniformIntegerDist.h \
	UniformUnsignedDist.h distributions.h generators.h
