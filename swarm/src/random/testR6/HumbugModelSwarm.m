// By Sven Thommesen <sthomme@humsci.auburn.edu>
// "HumbugModelSwarm.m"

// Import Swarm libraries:

#import <sys/time.h>
#import <unistd.h>

#import <space.h>
#import <activity.h>
#import <collections.h>
#import <swarmobject.h>
#import <simtools.h>

#import "HumbugModelSwarm.h"

// HumbugModelSwarm.m.6
// -----------------------------
// 
// Write a number of (binary) integers
// to a disk file from each
// generator, calling -getUnsignedSample.
// 
// This creates large files
// for use with Diehard and
// other tests.
// 
// Diehard requires that we left-
// shift the output from generators
// that produce less that 32 bits
// per variate.
// 
// -----------------------------

@implementation HumbugModelSwarm

+createBegin: (id) aZone {
  HumbugModelSwarm * obj;

  obj = [super createBegin: aZone];

  obj->debugPrint = 1;

  obj->myStream = [ OutputStream create: aZone setFileStream: stdout ];

  return obj;
}

-createEnd {
  return [super createEnd];
}

- simpleGenTest6: myGenerator 
            File: (const char *)fname
         NumVars: (unsigned)nvars
           Shift: (int)shift
{
   FILE * theFile;
   unsigned bitbucket;
   int i, status;

   theFile = fopen(fname, "w");
   if (theFile == NULL) {
     printf("Could not open file %s for generator %s - aborting\n",
	fname, [myGenerator getName]);
     return nil;
   }

   for (i=0; i<nvars; i++) {
     bitbucket = [myGenerator getUnsignedSample];
     if (shift) bitbucket = (bitbucket << shift);
     // fprintf(theFile, "%12u\n", bitbucket);
     status = fwrite(&bitbucket, sizeof(unsigned), 1, theFile);
     if (status < 1) {
       printf("Error writing to file %s for generator %s -- aborting\n",
	fname, [myGenerator getName]);
       break;
     }
   }

   status = fclose(theFile);
   if (status)
     printf("Error closing file %s for generator %s!\n",
	fname, [myGenerator getName]);

   return self;
}


-splitGenTest6: myGenerator 
          File: (const char *)fname
       NumVars: (unsigned)nvars
         Shift: (int)shift
{
   FILE * theFile;
   unsigned bitbucket;
   int i, status;
   const int vGen = 8;

   theFile = fopen(fname, "w");
   if (theFile == NULL) {
     printf("Could not open file %s for generator %s - aborting\n",
	fname, [myGenerator getName]);
     return nil;
   }

   for (i=0; i<nvars; i++) {
     bitbucket = [myGenerator getUnsignedSample: vGen];
     if (shift) bitbucket = (bitbucket << shift);
     // fprintf(theFile, "%12u\n", bitbucket);
     status = fwrite(&bitbucket, sizeof(unsigned), 1, theFile);
     if (status < 1) {
       printf("Error writing to file %s for generator %s -- aborting\n",
	fname, [myGenerator getName]);
       break;
     }
   }

   status = fclose(theFile);
   if (status)
     printf("Error closing file %s for generator %s!\n",
	fname, [myGenerator getName]);

   return self;
}


-buildObjects {
 
  [super buildObjects];

  pmmlcg1Generator = [PMMLCG1gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg2Generator = [PMMLCG2gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg3Generator = [PMMLCG3gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg4Generator = [PMMLCG4gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg5Generator = [PMMLCG5gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg6Generator = [PMMLCG6gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg7Generator = [PMMLCG7gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg8Generator = [PMMLCG8gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg9Generator = [PMMLCG9gen create: [self getZone] setStateFromSeed: 1];  
  lcg1Generator    = [LCG1gen    create: [self getZone] setStateFromSeed: 1];  
  lcg2Generator    = [LCG2gen    create: [self getZone] setStateFromSeed: 1];  
  lcg3Generator    = [LCG3gen    create: [self getZone] setStateFromSeed: 1];  
  acgGenerator     = [ACGgen     create: [self getZone] setStateFromSeed: 1];  
  scgGenerator     = [SCGgen     create: [self getZone] setStateFromSeed: 1];  
  swb1Generator    = [SWB1gen    create: [self getZone] setStateFromSeed: 1];  
  swb2Generator    = [SWB2gen    create: [self getZone] setStateFromSeed: 1];  
  swb3Generator    = [SWB3gen    create: [self getZone] setStateFromSeed: 1];  
  pswbGenerator    = [PSWBgen    create: [self getZone] setStateFromSeed: 1];
  TT403Generator   = [TT403gen   create: [self getZone] setStateFromSeed: 1];
  TT775Generator   = [TT775gen   create: [self getZone] setStateFromSeed: 1];
  TT800Generator   = [TT800gen   create: [self getZone] setStateFromSeed: 1];
  MT19937Generator = [MT19937gen create: [self getZone] setStateFromSeed: 1];
  c2taus1Generator = [C2TAUS1gen create: [self getZone] setStateFromSeed: 1];
  c2taus2Generator = [C2TAUS2gen create: [self getZone] setStateFromSeed: 1];
  c2taus3Generator = [C2TAUS3gen create: [self getZone] setStateFromSeed: 1];
  mrg5Generator    = [MRG5gen    create: [self getZone] setStateFromSeed: 1];
  mrg6Generator    = [MRG6gen    create: [self getZone] setStateFromSeed: 1];
  mrg7Generator    = [MRG7gen    create: [self getZone] setStateFromSeed: 1];
  c2mrg3Generator  = [C2MRG3gen  create: [self getZone] setStateFromSeed: 1];
  mwcaGenerator    = [MWCAgen    create: [self getZone] setStateFromSeed: 1];
  mwcbGenerator    = [MWCBgen    create: [self getZone] setStateFromSeed: 1];
  c3mwcGenerator   = [C3MWCgen   create: [self getZone] setStateFromSeed: 1];
  rwc2Generator    = [RWC2gen    create: [self getZone] setStateFromSeed: 1];
  rwc8Generator    = [RWC8gen    create: [self getZone] setStateFromSeed: 1];

  c2lcgxGenerator  = [C2LCGXgen  create: [self getZone] 
			setA: 32 setv: 20 setw: 30
			setStateFromSeed: 1];
  c4lcgxGenerator  = [C4LCGXgen  create: [self getZone] 
			setA: 128 setv: 31 setw: 41 
			setStateFromSeed: 1];

/* 

// We don't test distributions yet

randomBitDistribution = [RandomBitDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone] 
			setStateFromSeed: 1] ];
bernoulliDistribution = [BernoulliDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone]
			setStateFromSeed: 1] 
			setProbability: 0.67 ];
uniformIntegerDistribution  = 
   [UniformIntegerDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone]
			setStateFromSeed: 1]
			setIntegerMin: -49 setMax: 50 ];
uniformUnsignedDistribution = 
   [UniformUnsignedDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone]
			setStateFromSeed: 1]
			setUnsignedMin: 200 setMax: 299 ];
uniformDoubleDistribution = 
   [UniformDoubleDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone]
			setStateFromSeed: 1]
			setDoubleMin: 3.000 setMax: 4.000 ];
normalDistribution = [NormalDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone]
			setStateFromSeed: 1]
			setMean: 3.000 setVariance: 1.500 ];
logNormalDistribution = [LogNormalDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone]
			setStateFromSeed: 1]
			setMean: 3.000 setVariance: 1.500 ];
exponentialDistribution = [ExponentialDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone]
			setStateFromSeed: 1]
			setMean: 3.000 ];
gammaDistribution = [GammaDist create: [self getZone]
			setGenerator: [PMMLCG9gen create: [self getZone]
			setStateFromSeed: 1]
			setAlpha: 3.000 setBeta: 1.500 ];
*/

// NOTE: # variates = 2.5M = 2.5*1024*1024 = 2,621,440

// Test simple generators:

  myGen = pmmlcg1Generator;
  // [self simpleGenTest6: myGen File: "PMMLCG1gen.bin" NumVars: 2621440 Shift: 1];

  myGen = pmmlcg2Generator;
  // [self simpleGenTest6: myGen File: "PMMLCG2gen.bin" NumVars: 2621440 Shift: 1];

  myGen = pmmlcg3Generator;
  // [self simpleGenTest6: myGen File: "PMMLCG3gen.bin" NumVars: 2621440 Shift: 1];

  myGen = pmmlcg4Generator;
  // [self simpleGenTest6: myGen File: "PMMLCG4gen.bin" NumVars: 2621440 Shift: 1];

  myGen = pmmlcg5Generator;
  // [self simpleGenTest6: myGen File: "PMMLCG5gen.bin" NumVars: 2621440 Shift: 1];

  myGen = pmmlcg6Generator;
  // [self simpleGenTest6: myGen File: "PMMLCG6gen.bin" NumVars: 2621440 Shift: 1];

  myGen = pmmlcg7Generator;
  // [self simpleGenTest6: myGen File: "PMMLCG7gen.bin" NumVars: 2621440 Shift: 1];

  myGen = pmmlcg8Generator;
  // [self simpleGenTest6: myGen File: "PMMLCG8gen.bin" NumVars: 2621440 Shift: 1];

  myGen = pmmlcg9Generator;
  [self simpleGenTest6: myGen File: "PMMLCG9gen.bin" NumVars: 2621440 Shift: 1];

  myGen = lcg1Generator;
  // [self simpleGenTest6: myGen File: "LCG1gen.bin" NumVars: 2621440 Shift: 0];

  myGen = lcg2Generator;
  // [self simpleGenTest6: myGen File: "LCG2gen.bin" NumVars: 2621440 Shift: 0];

  myGen = lcg3Generator;
  // [self simpleGenTest6: myGen File: "LCG3gen.bin" NumVars: 2621440 Shift: 0];

  myGen = acgGenerator;
  // [self simpleGenTest6: myGen File: "ACGgen.bin" NumVars: 2621440 Shift: 0];

  myGen = scgGenerator;
  // [self simpleGenTest6: myGen File: "SCGgen.bin" NumVars: 2621440 Shift: 2];

  myGen = swb1Generator;
  // [self simpleGenTest6: myGen File: "SWB1gen.bin" NumVars: 2621440 Shift: 0];

  myGen = swb2Generator;
  // [self simpleGenTest6: myGen File: "SWB2gen.bin" NumVars: 2621440 Shift: 0];

  myGen = swb3Generator;
  // [self simpleGenTest6: myGen File: "SWB3gen.bin" NumVars: 2621440 Shift: 0];

  myGen = pswbGenerator;
  // [self simpleGenTest6: myGen File: "PSWBgen.bin" NumVars: 2621440 Shift: 0];

  myGen = MT19937Generator;
  // [self simpleGenTest6: myGen File: "MT19937gen.bin" NumVars: 2621440 Shift: 0];

  myGen = TT800Generator;
  // [self simpleGenTest6: myGen File: "TT800gen.bin" NumVars: 2621440 Shift: 0];

  myGen = TT775Generator;
  // [self simpleGenTest6: myGen File: "TT775gen.bin" NumVars: 2621440 Shift: 1];

  myGen = TT403Generator;
  // [self simpleGenTest6: myGen File: "TT403gen.bin" NumVars: 2621440 Shift: 1];

  myGen = mrg5Generator;
  // [self simpleGenTest6: myGen File: "MRG5gen.bin" NumVars: 2621440 Shift: 1];

  myGen = mrg6Generator;
  // [self simpleGenTest6: myGen File: "MRG6gen.bin" NumVars: 2621440 Shift: 1];

  myGen = mrg7Generator;
  // [self simpleGenTest6: myGen File: "MRG7gen.bin" NumVars: 2621440 Shift: 1];

  myGen = c2mrg3Generator;
  // [self simpleGenTest6: myGen File: "C2MRG3gen.bin" NumVars: 2621440 Shift: 1];

  myGen = c2taus1Generator;
  // [self simpleGenTest6: myGen File: "C2TAUS1gen.bin" NumVars: 2621440 Shift: 1];

  myGen = c2taus2Generator;
  // [self simpleGenTest6: myGen File: "C2TAUS2gen.bin" NumVars: 2621440 Shift: 1];

  myGen = c2taus3Generator;
  // [self simpleGenTest6: myGen File: "C2TAUS3gen.bin" NumVars: 2621440 Shift: 1];

  myGen = mwcaGenerator;
  // [self simpleGenTest6: myGen File: "MWCAgen.bin" NumVars: 2621440 Shift: 0];

  myGen = mwcbGenerator;
  // [self simpleGenTest6: myGen File: "MWCBgen.bin" NumVars: 2621440 Shift: 0];

  myGen = c3mwcGenerator;
  // [self simpleGenTest6: myGen File: "C3MWCgen.bin" NumVars: 2621440 Shift: 0];

  myGen = rwc2Generator;
  // [self simpleGenTest6: myGen File: "RWC2gen.bin" NumVars: 2621440 Shift: 0];

  myGen = rwc8Generator;
  // [self simpleGenTest6: myGen File: "RWC8gen.bin" NumVars: 2621440 Shift: 0];


// Test split generators:

  myGen = c2lcgxGenerator;
  // [self splitGenTest6: myGen File: "C2LCGXgen.bin" NumVars: 2621440 Shift: 1];

  myGen = c4lcgxGenerator;
  // [self splitGenTest6: myGen File: "C4LCGXgen.bin" NumVars: 2621440 Shift: 1];


/*

// We do not test distributions for the time being

// Test distributions:

  myDist = randomBitDistribution;

  myDist = bernoulliDistribution;

  myDist = uniformIntegerDistribution;

  myDist = uniformUnsignedDistribution;

  myDist = uniformDoubleDistribution;

  myDist = normalDistribution;

  myDist = logNormalDistribution;

  myDist = exponentialDistribution;

  myDist = gammaDistribution;

*/

  return self;
}

-buildActions {

  [super buildActions];

  return self;
}

-activateIn: (id) swarmContext {

  [super activateIn: swarmContext];

  return [self getSwarmActivity];
}

@end
