// By Sven Thommesen <sthomme@humsci.auburn.edu>
// "HumbugModelSwarm.h"

// Import Swarm libraries:

#import <space.h>
#import <activity.h>
#import <collections.h>
#import <swarmobject.h>

#import <random.h>
#import <swarmobject/Swarm.h>

@interface HumbugModelSwarm : Swarm {

  int debugPrint;

// Internal data objects:

  id <PMMLCG1gen>   pmmlcg1Generator;
  id <PMMLCG2gen>   pmmlcg2Generator;
  id <PMMLCG3gen>   pmmlcg3Generator;
  id <PMMLCG4gen>   pmmlcg4Generator;
  id <PMMLCG5gen>   pmmlcg5Generator;
  id <PMMLCG6gen>   pmmlcg6Generator;
  id <PMMLCG7gen>   pmmlcg7Generator;
  id <PMMLCG8gen>   pmmlcg8Generator;
  id <PMMLCG9gen>   pmmlcg9Generator;
  id <LCG1gen>      lcg1Generator;
  id <LCG2gen>      lcg2Generator;
  id <LCG3gen>      lcg3Generator;
  id <ACGgen>       acgGenerator;
  id <SCGgen>       scgGenerator;
  id <SWB1gen>      swb1Generator;
  id <SWB2gen>      swb2Generator;
  id <SWB3gen>      swb3Generator;
  id <PSWBgen>      pswbGenerator;
  id <TT403gen>     TT403Generator;
  id <TT775gen>     TT775Generator;
  id <TT800gen>     TT800Generator;
  id <MT19937gen>   MT19937Generator;
  id <MRG5gen>      mrg5Generator;
  id <MRG6gen>      mrg6Generator;
  id <MRG7gen>      mrg7Generator;
  id <C2MRG3gen>    c2mrg3Generator;
  id <C2TAUS1gen>   c2taus1Generator;
  id <C2TAUS2gen>   c2taus2Generator;
  id <C2TAUS3gen>   c2taus3Generator;
  id <MWCAgen>      mwcaGenerator;
  id <MWCBgen>      mwcbGenerator;
  id <C3MWCgen>     c3mwcGenerator;
  id <RWC2gen>      rwc2Generator;
  id <RWC8gen>      rwc8Generator;

  id <C2LCGXgen>    c2lcgxGenerator;
  id <C4LCGXgen>    c4lcgxGenerator;

  id <RandomBitDist>       randomBitDistribution;
  id <BernoulliDist>       bernoulliDistribution;
  id <UniformIntegerDist>  uniformIntegerDistribution;
  id <UniformUnsignedDist> uniformUnsignedDistribution;
  id <UniformDoubleDist>   uniformDoubleDistribution;
  id <NormalDist>          normalDistribution;
  id <LogNormalDist>       logNormalDistribution;
  id <ExponentialDist>     exponentialDistribution;
  id <GammaDist>           gammaDistribution;

  id mySimpleGen;
  id mySplitGen;

  id myGen, myDist;

// Internal scheduling objects:

  id modelPreActions, modelPostActions;
  id modelPreSchedule, modelPostSchedule;

  id myStream;	// output stream

}

// Create the object:
+createBegin: (id) aZone;
-createEnd;

// Do the usual Swarm things:
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

@end
