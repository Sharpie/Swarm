// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:		 C4LCGXgen.m
Description:	 Combined random generator using 4 (PMM)LCG generators
Library:	 random
Original Author: Sven Thommesen
Date:		 1997-09-01   (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <limits.h>		// for setA:setv:setw: (LONG_MAX)

#import <collections.h>		// for outStream in -describe

#import <random/C4LCGXgen.h>

@implementation C4LCGXgen


// Import common snippets of code:

#import "Common.xgens.create.m"

#import "Common.gens.genSeeds.m"

#import "Common.xgens.setparams.m"

#import "Common.xgens.vgens.m"

#import "Common.xgens.floats.m"


// And now code particular to this generator:


// This struct is used by setStateFrom / putStateInto
// to pass our state data to/from other objects:

typedef struct {
// Generator identification:
  unsigned int genMagic;
  unsigned int stateSize;
// Fixed parameters:
  unsigned int numGenerators;
  unsigned int numSegments;
  unsigned int segmentLength;
  BOOL antiThetic;
// state variables:
  BOOL singleInitialSeed;
  unsigned int initialSeed;
  unsigned int initialSeeds[SEEDS];
  struct vGenStruct stateVec;
} state_struct_t;



-setState {
  int i,j,g;

// Assuming initialSeeds contains valid seed values,
// set the state of virtual generator #0 from initialSeed:

   for (i = 0; i < lengthOfSeedVector; i++) 
     vGenArr[0].Ig[i] = initialSeeds[i];	// initialSeeds[] are never 0

  [self initGenerator: 0];		// InitGenerator(0, InitialSeed);

// Set the state of the rest of the virtual generators,
// offset equal intervals from the state of vGen #0:

  for (g=1; g<numGenerators; g++)
    {
      for (j=0; j<COMPONENTS; j++)
      vGenArr[g].Ig[j] = [self MultModMs: avw[j] t: vGenArr[g-1].Ig[j] M: m[j]];
      [self initGenerator: g];	// InitGenerator(g, InitialSeed);
    }

  return self;
}


-initState {
   int i,j;
   int arrSize;

// This method is called from create after setA:setv:setw: .

// Set the 'personality' of this generator:
   strncpy(genName,"C4LCGX",sizeof(genName));
   genMagic = C4LCGXMAGIC + GENSUBMASK*1 + C4LCGXREVISION;  // see RandomDefs.h

// Set the parameters for the first component generator:

   a[0] = 45991;
   m[0] = 2147483647;		// 2^31-1

// Set the parameters for the second component generator:

   a[1] = 207707;
   m[1] = 2147483543;		// 2^31-105

// Set the parameters for the third component generator:

   a[2] = 138556;
   m[2] = 2147483423;		// 2^31-225

// Set the parameters for the fourth component generator:

   a[3] = 49689;
   m[3] = 2147483323;		// 2^31-325

// Set derived constants for each component generator:

for (j=0; j<COMPONENTS; j++)
{
   q[j] = m[j] / a[j];		// Schrage quotient
   r[j] = m[j] % a[j];		// Schrage remainder
}

// For size of state and seed vectors:
   lengthOfSeedVector = SEEDS;

// Since the number of virtual generator is not known until runtime,
// we allocate the state vector dynamically:

   [ self allocVectors ];

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i = 0; i < lengthOfSeedVector; i++)
     maxSeedValues[i] = m[i]-1;

// State size for getState and setState:
   arrSize = numGenerators * sizeof(struct vGenStruct);
   stateSize = sizeof(state_struct_t) + arrSize;

// For the whole generator, the total cycle length is
// countMax = (m[0]-1) * (m[1]-1) / 2 ~= 2^61;
// so we limit single segments to be <= 2^60
// and set countMax = segmentLength.
// (See method -setA:setv:setw: above.)

// ----------

// For the combined generator, output is in [0,m[0]-1]:
// (rather than [0,m[0]-2], from the way components are aggregated)
   unsignedMax = m[0] - 1;
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);
   invModMult2 = invModMult * invModMult;

// Initialize multipliers for jump-ahead:
   for (j=0; j<COMPONENTS; j++)
   {
     aw [j] = a [j];
     for (i=1; i<=segmentLength; i++)
       aw [j] = [self MultModMs: aw[j] t: aw[j] M: m[j] ];
     avw [j] = aw [j];
     for (i=1; i<=numSegments; i++)
       avw [j] = [self MultModMs: avw[j] t: avw[j] M: m[j] ];
   }

  return self;
}


-setA: (unsigned) A setv: (unsigned) v setw: (unsigned) w {
   unsigned log2A;
   int i;
   unsigned maxGen;

   // LIMITS:

   // 1. If we try to allocate more than LONG_MAX bytes then
   //    memset in allocVectors fails.
   // On Intel, LONG_MAX = 2^31 = 2,147,483,648 = 2GB
   // Currently, vGenStruct is 64 bytes => maxGen = 2^25 = 33,554,432

   maxGen = LONG_MAX / sizeof(struct vGenStruct) - 1;

   // Add this test, in case someone has 64-bit memory:
   if (maxGen > MAXVGEN) maxGen = MAXVGEN;

   // 2. If (v>63) or (w>63) then testing currentCount and 
   //    currentSegment for rollover becomes difficult.

   if ((A > maxGen) || (v > 63) || (w > 63)) {
   printf("Generator parameters: A = %d  v = %d  w = %d
    maxA = %d  maxv = 63  maxw = 63\n\n", 
      A,v,w,maxGen);
   [InvalidCombination raiseEvent:
     "%s Bad initialization parameters: A, v or w too large\n",genName];
   }

   numGenerators = A;
   numSegments   = v;
   segmentLength = w;
   segmentMax    = (1ull << numSegments);	// V = 2^v
   countMax      = (1ull << segmentLength);	// W = 2^w

   // Calculate log2(A) and round up to nearest integer:

   log2A = A;
   i = 0;
   while ( log2A > 0 ) {
      log2A /= 2;
      i++;
   }
   i--;

   log2A = (1 << i);
   if (log2A < A) i++;
   log2A = i;

   // 3. Test that we are not exceeding the generator's cycle length:

   if ((log2A+v+w) > 120) {
   printf("Generator parameters: log2(A) = %d  v = %d  w = %d
    maxv = 63  maxw = 63 maxSum = 120\n\n", log2A,v,w);
   printf("Generator parameters: A = %d  V = %lld  W = %lld  maxA = %d\n\n", 
       A,segmentMax,countMax,maxGen);
   [InvalidCombination raiseEvent:
     "%s Bad initialization parameters: a+v+w too large\n",genName];
   }

   return self;
}

+createWithDefaults: (id) aZone {
  C4LCGXgen * aGenerator;

// Allocate space for the object:

  aGenerator = [C4LCGXgen createBegin: aZone];
 
// set characteristic parameters:
 
  [aGenerator setA: 128 setv: 31 setw: 41];
  [aGenerator initState];
 
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}



// ----- Published creation methods: -----

+createBegin: aZone {
  C4LCGXgen * aGenerator;

// Allocate space for the object:

  aGenerator = [super createBegin: aZone];

// initialize instance variables:

  aGenerator->numSegments = 0;

  return aGenerator;
}

+create: (id) aZone 
	setA: (unsigned) A setv: (unsigned) v setw: (unsigned) w
	setStateFromSeed: (unsigned) seed {
  C4LCGXgen * aGenerator;

// Allocate space for the object:

  aGenerator = [C4LCGXgen createBegin: aZone];
  
// Set characteristic parameters:

  [aGenerator setA: A setv: v setw: w];
  [aGenerator initState];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone
	setA: (unsigned) A setv: (unsigned) v setw: (unsigned) w
	setStateFromSeeds: (unsigned *) seeds {

  C4LCGXgen * aGenerator;

// Allocate space for the object:

  aGenerator = [C4LCGXgen createBegin: aZone];

// Set characteristic parameters:

  [aGenerator setA: A setv: v setw: w];
  [aGenerator initState];

// Initialize seed dependent part of the state:

  [aGenerator setStateFromSeeds: seeds];

   return [aGenerator createEnd];
}

// This method implements the alternative way to aggregate the
// four component generators, yielding an unsigned int as result.
// For speed, all constants are inline.

-(unsigned) getUnsignedSample: (unsigned) vGen {
  int k,s,ss;
  unsigned ret;

  if (vGen > numGenerators)
  [InvalidCombination raiseEvent:
  "%s getUnsignedSample: vGen too large %u\n", genName, vGen];

// Update count of variates delivered:

  vGenArr[vGen].currentCount++;

// Test if the virtual generator has exceeded its segment:

  if (vGenArr[vGen].currentCount >= countMax) {
    printf("\n*** \n");
    printf("*** WARNING %s: virtual generator %u has exhausted a segment!\n", 
	genName,vGen);
    printf("*** You need to use larger segments.\n");
    printf("*** (Advancing generator to next segment)\n");
    printf("*** \n\n");
    [self advanceGenerator: vGen];
  }

// Generate the next 'random' value from the state.
// The split of the math into q and r ensures that we don't 
// overflow 32 bits.  (Following Schrage.)

  ss = 0;

  s = vGenArr[vGen].Cg [0];  
  k = s / 46693;
  s = 45991 * (s - k * 46693) - k * 25884;
  if (s < 0) s = s + 2147483647;  			// s in [1,m1-1]
  vGenArr[vGen].Cg [0] = s;

  ss += s;						// ss in [1,m1-1]
 
  s = vGenArr[vGen].Cg [1];  
  k = s / 10339;
  s = 207707 * (s - k * 10339) - k * 870;
  if (s < 0) s = s + 2147483543;  			// s in [1,m2-1]
  vGenArr[vGen].Cg [1] = s;

  ss -= s;
  if (ss < 0) ss += 2147483647;				// ss in [0,m1-1]

  s = vGenArr[vGen].Cg [2];  
  k = s / 15499;
  s = 138556 * (s - k * 15499) - k * 3979;
  if (s < 0) s = s + 2147483423;  			// s in [1,m3-1]
  vGenArr[vGen].Cg [2] = s;

  ss = (ss + s) % 2147483647;				// ss in [0,m1-1]
  
  s = vGenArr[vGen].Cg [3];  
  k = s / 43218;
  s = 49689 * (s - k * 43218) - k * 24121;
  if (s < 0) s = s + 2147483323;  			// s in [1,m4-1]
  vGenArr[vGen].Cg [3] = s;

  ss -= s;
  if (ss < 0) ss += 2147483647;				// ss in [0,m1-1]

  ret = ss;						// ret in [0,m1-1]

  if (antiThetic) 
    return (unsignedMax - ret);
  else 
    return ret;

}


// ----- protocol InternalState: -----


-(void) putStateInto: (void *) buffer {
   state_struct_t * stateBuf;
   int i;
   struct vGenStruct * structArr;

// Recast the caller's pointer:
stateBuf = (state_struct_t *) (buffer) ;

// Fill the external buffer with current values:

  // Generator identification:
  stateBuf->genMagic     = genMagic;
  stateBuf->stateSize    = stateSize;

  // Instance parameters:
  stateBuf->numGenerators = numGenerators;
  stateBuf->numSegments   = numSegments;
  stateBuf->segmentLength = segmentLength;

  stateBuf->antiThetic    = antiThetic;
  stateBuf->singleInitialSeed = singleInitialSeed;
  stateBuf->initialSeed = initialSeed;

  for (i=0; i < lengthOfSeedVector; i++)
    stateBuf->initialSeeds[i] = initialSeeds[i];

  structArr = &(stateBuf->stateVec);

  for (i=0; i<numGenerators; i++) {
    structArr[i] = vGenArr[i];
  }

  // nothing returned from a (void) function
}

-(void) setStateFrom: (void *) buffer {
   state_struct_t * stateBuf;
   int i,j;
   struct vGenStruct * structArr;

// Recast the caller's pointer:
stateBuf = (state_struct_t *) (buffer) ;

// TEST the integrity of the external data:
if (     (stateBuf->genMagic  != genMagic)
      || (stateBuf->stateSize != stateSize)
   )
[InvalidCombination raiseEvent:
 "%u %s generator: your are passing bad data to setState!\n %u %u\n",
  genMagic, genName,
  stateBuf->genMagic,
  stateBuf->stateSize ];

// TEST the size of the space needed:
if (stateBuf->numGenerators != numGenerators)
  //   [InvalidCombination raiseEvent:
  //   "%u %s generator: wrong number of generators passed to setState!\n",
  //   genMagic, genName ];
// ALTERNATIVE: de-allocate current arrays and allocate new ones ...
   [self allocVectors];

  // Instance parameters:
  numGenerators = stateBuf->numGenerators;
  numSegments   = stateBuf->numSegments;
  segmentLength = stateBuf->segmentLength;

  antiThetic = stateBuf->antiThetic;
  singleInitialSeed = stateBuf->singleInitialSeed;
  initialSeed = stateBuf->initialSeed;

  for (i=0; i<lengthOfSeedVector; i++)
    initialSeeds[i] = stateBuf->initialSeeds[i];

  // Initialize working variables:
  segmentMax = (1ull << numSegments);
  countMax   = (1ull << segmentLength);

  structArr = &(stateBuf->stateVec);

  for (i=0; i<numGenerators; i++) {
    vGenArr[i] = structArr[i];
  }

  // Initialize multipliers for jump-ahead:
   for (j=0; j<COMPONENTS; j++)
   {
     aw [j] = a [j];
     for (i=1; i<=segmentLength; i++)
       aw [j] = [self MultModMs: aw[j] t: aw[j] M: m[j] ];
     avw [j] = aw [j];
     for (i=1; i<=numSegments; i++)
       avw [j] = [self MultModMs: avw[j] t: avw[j] M: m[j] ];
   }

  // nothing returned from a (void) function
}


- (void) describe: outStream {
  char buffer[128];
  int i,j;
  unsigned int *qInt;

  (void)sprintf(buffer,"%s Describe: \n",genName);
  [outStream catC: buffer];

  (void)sprintf(buffer,"       genName = %24s\n", genName);
  [outStream catC: buffer];
  (void)sprintf(buffer,"     stateSize = %24u\n", stateSize);
  [outStream catC: buffer];
  (void)sprintf(buffer,"      genMagic = %24u\n", genMagic);
  [outStream catC: buffer];

  (void)sprintf(buffer," numGenerators = %24u\n", numGenerators);
  [outStream catC: buffer];
  (void)sprintf(buffer,"   numSegments = %24u\n", numSegments);
  [outStream catC: buffer];
  (void)sprintf(buffer,"    segmentMax = %24llu\n", segmentMax);
  [outStream catC: buffer];
  (void)sprintf(buffer," segmentLength = %24u\n", segmentLength);
  [outStream catC: buffer];
  (void)sprintf(buffer,"      countMax = %24llu\n", countMax);
  [outStream catC: buffer];
  (void)sprintf(buffer,"   unsignedMax = %24u\n", unsignedMax);
  [outStream catC: buffer];
  (void)sprintf(buffer,"    invModMult = %24.16e\n", invModMult);
  [outStream catC: buffer];
  (void)sprintf(buffer,"   invModMult2 = %24.16e\n", invModMult2);
  [outStream catC: buffer];
  (void)sprintf(buffer,"    antiThetic = %24u\n", antiThetic);
  [outStream catC: buffer];
  (void)sprintf(buffer," singleInitialSeed = %20u\n", singleInitialSeed);
  [outStream catC: buffer];
  (void)sprintf(buffer,"   initialSeed = %24u\n", initialSeed);
  [outStream catC: buffer];
  (void)sprintf(buffer," lengthOfSeedVector = %19u\n", lengthOfSeedVector);
  [outStream catC: buffer];

  for (j=0; j<lengthOfSeedVector; j++) {
    (void) sprintf(buffer,"     maxSeeds[%02d] = %21u\n", j, maxSeedValues[j]);
    [outStream catC: buffer];
  }
  [outStream catC: "\n"];

  for (j=0; j<lengthOfSeedVector; j++) {
    (void) sprintf(buffer," initialSeeds[%02d] = %21u\n", j, initialSeeds[j]);
    [outStream catC: buffer];
  }
  [outStream catC: "\n"];

  for (j=0; j<COMPONENTS; j++) {
    (void)sprintf(buffer,
    "lcg#%d m = %10d a = %10d q = %9d r = %9d\n",
     j, m[j], a[j], q[j], r[j]);
    [outStream catC: buffer];
  }
  [outStream catC: "\n"];

  for (j=0; j<COMPONENTS; j++) {
    (void)sprintf(buffer,
    "aw=%10d avw=%10d maxSeed=%10u\n",
     aw[j], avw[j], maxSeedValues[j]);
    [outStream catC: buffer];
  }
  [outStream catC: "\n"];

  for (i=0; i<numGenerators; i++) {
      (void)sprintf(buffer,
      "vGen # %03d: Ig = %12u %12u %12u %12u\n", 
      i,vGenArr[i].Ig[0], vGenArr[i].Ig[1], vGenArr[i].Ig[2], vGenArr[i].Ig[3]);
      [outStream catC: buffer];
  }
  [outStream catC: "\n"];

  for (i=0; i<numGenerators; i++) {
      (void)sprintf(buffer,
      "vGen # %03d: Lg = %12u %12u %12u %12u\n", 
      i,vGenArr[i].Lg[0], vGenArr[i].Lg[1], vGenArr[i].Lg[2], vGenArr[i].Lg[3]);
      [outStream catC: buffer];
  }
  [outStream catC: "\n"];

  for (i=0; i<numGenerators; i++) {
      (void)sprintf(buffer,
      "vGen # %03d: Cg = %12u %12u %12u %12u\n", 
      i,vGenArr[i].Cg[0], vGenArr[i].Cg[1], vGenArr[i].Cg[2], vGenArr[i].Cg[3]);
      [outStream catC: buffer];
  }
  [outStream catC: "\n"];

  for (i=0; i<numGenerators; i++) {
      (void)sprintf(buffer,
      "vGen # %03d: currSeg=%20llu currCount=%20llu\n", 
      i,vGenArr[i].currentSegment, vGenArr[i].currentCount);
      [outStream catC: buffer];
  }
  [outStream catC: "\n\n"];

  qInt = [self getInitialSeeds: 0];
  (void)sprintf(buffer,"Ig0 = %12u %12u %12u %12u\n", 
   // (*qInt)[0],(*qInt)[1],(*qInt)[2],(*qInt)[3]);
   qInt[0], qInt[1], qInt[2], qInt[3]);
  [outStream catC: buffer];

  qInt = [self getLastSeeds: 0];
  (void)sprintf(buffer,"Lg0 = %12u %12u %12u %12u\n", 
   // (*qInt)[0],(*qInt)[1],(*qInt)[2],(*qInt)[3]);
   qInt[0], qInt[1], qInt[2], qInt[3]);
  [outStream catC: buffer];

  qInt = [self getCurrentSeeds: 0];
  (void)sprintf(buffer,"Cg0 = %12u %12u %12u %12u\n", 
   // (*qInt)[0],(*qInt)[1],(*qInt)[2],(*qInt)[3]);
   qInt[0], qInt[1], qInt[2], qInt[3]);
  [outStream catC: buffer];

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end


