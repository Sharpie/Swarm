SWARMHOME=../..

LIBNAME=random

OBJECTS=LCGgen.o PMMLCGgen.o \
	ACGgen.o SCGgen.o SWBgen.o PSWBgen.o \
	TGFSRgen.o C2TAUSgen.o \
	MRGgen.o C2MRG3gen.o \
	C2LCGXgen.o C4LCGXgen.o \
	MT19937gen.o \
	MWCAgen.o MWCBgen.o C3MWCgen.o RWC8gen.o RWC2gen.o \
	UniformIntegerDist.o UniformUnsignedDist.o UniformDoubleDist.o \
	RandomBitDist.o BernoulliDist.o \
	NormalDist.o LogNormalDist.o \
	ExponentialDist.o GammaDist.o \
	random.o

HEADERS=LCGgen.h PMMLCGgen.h \
	ACGgen.h SCGgen.h SWBgen.h PSWBgen.h \
	TGFSRgen.h C2TAUSgen.h \
	MRGgen.h C2MRG3gen.h \
	C2LCGXgen.h C4LCGXgen.h \
	MT19937gen.h \
	MWCAgen.h MWCBgen.h C3MWCgen.h RWC8gen.h RWC2gen.h \
	UniformIntegerDist.h UniformUnsignedDist.h UniformDoubleDist.h \
	RandomBitDist.h BernoulliDist.h \
	NormalDist.h LogNormalDist.h \
	ExponentialDist.h GammaDist.h \
	RandomDefs.h RandomVars.h \
	generators.h distributions.h

include $(SWARMHOME)/Makefile.lib

LCGgen.o: LCGgen.m LCGgen.h \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

PMMLCGgen.o: PMMLCGgen.m PMMLCGgen.h \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

ACGgen.o: ACGgen.m ACGgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

SCGgen.o: SCGgen.m SCGgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

SWBgen.o: SWBgen.m SWBgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

PSWBgen.o: PSWBgen.m PSWBgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

TGFSRgen.o: TGFSRgen.m TGFSRgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

MT19937gen.o: MT19937gen.m MT19937gen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

C2TAUSgen.o: C2TAUSgen.m C2TAUSgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

MRGgen.o: MRGgen.m MRGgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

MWCAgen.o: MWCAgen.m MWCAgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

MWCBgen.o: MWCBgen.m MWCBgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

C3MWCgen.o: C3MWCgen.m C3MWCgen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

RWC8gen.o: RWC8gen.m RWC8gen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

RWC2gen.o: RWC2gen.m RWC2gen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

C2MRG3gen.o: C2MRG3gen.m C2MRG3gen.h \
	Common.gens.genSeeds.m \
	Common.gens.setparams.m Common.gens.floats.m Common.gens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

C2LCGXgen.o: C2LCGXgen.m C2LCGXgen.h \
	Common.gens.genSeeds.m \
	Common.xgens.vgens.m \
	Common.xgens.setparams.m Common.xgens.floats.m Common.xgens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h

C4LCGXgen.o: C4LCGXgen.m C4LCGXgen.h \
	Common.gens.genSeeds.m \
	Common.xgens.vgens.m \
	Common.xgens.setparams.m Common.xgens.floats.m Common.xgens.create.m \
	random.h generators.h RandomDefs.h RandomVars.h


RandomBitDist.o: RandomBitDist.m RandomBitDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

BernoulliDist.o: BernoulliDist.m BernoulliDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

UniformIntegerDist.o: UniformIntegerDist.m UniformIntegerDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

UniformUnsignedDist.o: UniformUnsignedDist.m UniformUnsignedDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

UniformDoubleDist.o: UniformDoubleDist.m UniformDoubleDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

NormalDist.o: NormalDist.m NormalDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

LogNormalDist.o: LogNormalDist.m LogNormalDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

ExponentialDist.o: ExponentialDist.m ExponentialDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

GammaDist.o: GammaDist.m GammaDist.h \
	Common.dists.m \
	random.h distributions.h RandomDefs.h RandomVars.h

random.o: random.m random.h RandomVars.h

