// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            C2MRG3gen.m
Description:     Combined Multiple Recursive [LCG] Generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01   (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/C2MRG3gen.h>

@implementation C2MRG3gen


// Import common snippets of code:

#import "Common.gens.create.m"

#import "Common.gens.genSeeds.m"

#import "Common.gens.setparams.m"

#import "Common.gens.floats.m"


// And now code particular to this generator:


// This struct is used by setStateFrom / putStateInto
// to pass our state data to/from other objects:

typedef struct {
// Generator identification:
  unsigned int genMagic;
  unsigned int stateSize;
// Fixed parameters:
  BOOL antiThetic;
// State variables:
  BOOL singleInitialSeed;
  unsigned int initialSeed;
  unsigned long long int currentCount;
  unsigned int initialSeeds[SEEDS];
  unsigned int state[SEEDS];
} state_struct_t;


-setState {

// Fill state variables from initialSeeds vector,
// assuming initialSeeds contains valid seeds:

   x10 = initialSeeds[0];		// initialSeeds[] are never zero
   x11 = initialSeeds[1];
   x12 = initialSeeds[2];
   x20 = initialSeeds[3];
   x21 = initialSeeds[4];
   x22 = initialSeeds[5];

// If needed, draw a number of variates
// to escape rho-sequences:
   // [self runup: 5*r];

// The components of C2MRG are full-cycle generators which do not need runup

   currentCount = 0;

   return self;
}


-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"C2MRG3",sizeof(genName));
   genMagic = C2MRGMAGIC + GENSUBMASK*1 + C2MRGREVISION;  // see RandomDefs.h

// Set the parameters:

   k   =           3;	// maxlag

   m1  =  2147483647;	// 2^31-1
					// a11 = 0
   a12 =       63308;
   q12 =       33921;	// m1 / a12
   r12 =       12979;	// m1 % a12

   a13 =     -183326;
   q13 =       11714;	// m1 / a13
   r13 =        2883;	// m1 % a13

   m2  =  2145483479;	// 2^31-2000169

   a21 =       86098;
   q21 =       24919;	// m2 / a21
   r21 =        7417;	// m2 % a21
					// a22 = 0
   a23 =     -539608;
   q23 =        3976;	// m2 / a23
   r23 =        2071;	// m2 % a23

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 2*k;			// COMPONENTS*MAXLAG
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i < k; i++)
     maxSeedValues[i] = m1 - 1;
   for (i=k; i < 2*k; i++)
     maxSeedValues[i] = m2 - 1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = (m1^k -1)*(m2^k -1)/2 or ~= 2^185
// Since this vastly exceeds the counter variable, instead we set:
   countMax = (1ull << 63);			// 2^63

// Math is modulo m1, so max output value is m1-1:
   unsignedMax = m1 - 1;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

   return self;
}


// ----- Published creation methods: -----

+createBegin: aZone {
  C2MRG3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [super createBegin: aZone];

// initialize instance variables:

  aGenerator->currentCount = TESTCASE;

// initialize fixed parts of state:

  [aGenerator initState];	// must be called before setStateFromSeed

  return aGenerator;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  C2MRG3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2MRG3gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  C2MRG3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2MRG3gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  C2MRG3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2MRG3gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


// ----- Get unsigned random value: -----

-(unsigned) getUnsignedSample {
   int h, p21, p23, p12, p13;
   int new;

// Update count of variates delivered:
// (cycle is > 2^63, so report that counter is exhausted)

   currentCount++;

// Give diagnostic warning that we're exceeding the counter:

   if (currentCount >= countMax) {
   printf("\n*** \n");
   printf("*** NOTICE %s: generator has created 2^63 variates\n", genName);
   printf("*** out of a cycle of length at least 2^185\n");
   printf("*** (resetting counter)\n");
   printf("*** \n\n");
   currentCount = 0;
   }

// -----------

// Generate the next 'random' value from the state.

// Component #1:
   h = x10 / q13;
   p13 = -a13 * (x10 - h * q13) - h * r13;
   h = x11 / q12;
   p12 =  a12 * (x11 - h * q12) - h * r12;
   if (p13 < 0) p13 = p13 + m1;
   if (p12 < 0) p12 = p12 + m1;
   x10 = x11;
   x11 = x12;
   x12 = p12 - p13;
   if (x12 < 0) x12 = x12 + m1;

// Component #2:
   h = x20 / q23;
   p23 = -a23 * (x20 - h * q23) - h * r23;
   h = x22 / q21;
   p21 = a21 * (x22 - h * q21) - h * r21;
   if (p23 < 0) p23 = p23 + m2;
   if (p21 < 0) p21 = p21 + m2;
   x20 = x21;
   x21 = x22;
   x22 = p21 - p23;
   if (x22 < 0) x22 = x22 + m2;

// Combine them:

   if (x12 < x22) new = (x12 - x22 + m1);
   else new = (x12 - x22);

// -----------

   if (antiThetic) return (unsignedMax - new);
   else return new;
}


// ----- protocol InternalState: -----


-(void) putStateInto: (void *) buffer {
   state_struct_t * stateBuf;
   int i;

// Recast the caller's pointer:
stateBuf = (state_struct_t *) (buffer) ;

// Fill the external buffer with current values:

  // Generator identification:
  stateBuf->genMagic     = genMagic;
  stateBuf->stateSize    = stateSize;

  // Fixed parameters:
  stateBuf->antiThetic   = antiThetic;

  // State variables:
  stateBuf->singleInitialSeed = singleInitialSeed;
  stateBuf->initialSeed  = initialSeed;
  stateBuf->currentCount = currentCount;

  stateBuf->state[0] = x10;
  stateBuf->state[1] = x11;
  stateBuf->state[2] = x12;
  stateBuf->state[3] = x20;
  stateBuf->state[4] = x21;
  stateBuf->state[5] = x22;

  for (i=0; i < lengthOfSeedVector; i++)
    stateBuf->initialSeeds[i] = initialSeeds[i];

  // nothing returned from a (void) function
}

-(void) setStateFrom: (void *) buffer {
   state_struct_t * stateBuf;
   int i;

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


// Place external data into internal state variables:

  antiThetic   = stateBuf->antiThetic;
  singleInitialSeed = stateBuf->singleInitialSeed;
  initialSeed  = stateBuf->initialSeed;
  currentCount = stateBuf->currentCount;

  x10 = stateBuf->state[0];
  x11 = stateBuf->state[1];
  x12 = stateBuf->state[2];
  x20 = stateBuf->state[3];
  x21 = stateBuf->state[4];
  x22 = stateBuf->state[5];

  for (i=0; i<lengthOfSeedVector; i++)
    initialSeeds[i] = stateBuf->initialSeeds[i];

  // nothing returned from a (void) function
}


- (void) describe: outStream {
  char buffer[128];
  int i;

  (void)sprintf(buffer,"%s Describe: \n",genName);
  [outStream catC: buffer];

  (void)sprintf(buffer,"      genName = %24s\n", genName);
  [outStream catC: buffer];
  (void)sprintf(buffer,"    stateSize = %24u\n", stateSize);
  [outStream catC: buffer];
  (void)sprintf(buffer,"     genMagic = %24u\n", genMagic);
  [outStream catC: buffer];


  (void)sprintf(buffer,"            k = %24d\n", k);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            m = %24d %16d\n", m1, m2);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           a1 = %24d %16d\n",   0, a21);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           a2 = %24d %16d\n", a12,   0);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           a3 = %24d %16d\n", a13, a23);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           q1 = %24d %16d\n",   0, q21);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           q2 = %24d %16d\n", q12,   0);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           q3 = %24d %16d\n", q13, q23);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           r1 = %24d %16d\n",   0, r21);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           r2 = %24d %16d\n", r12,   0);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           r3 = %24d %16d\n", r13, r23);
  [outStream catC: buffer];
  (void)sprintf(buffer,"   antiThetic = %24d\n", antiThetic);
  [outStream catC: buffer];

  (void)sprintf(buffer,"  unsignedMax = %24u\n", unsignedMax);
  [outStream catC: buffer];
  (void)sprintf(buffer,"   invModMult = %24.16e\n", invModMult);
  [outStream catC: buffer];
  (void)sprintf(buffer,"  invModMult2 = %24.16e\n", invModMult2);
  [outStream catC: buffer];

  (void)sprintf(buffer,"  initialSeed = %24u\n", initialSeed);
  [outStream catC: buffer];
  (void)sprintf(buffer," singleInitialSeed = %19d\n", singleInitialSeed);
  [outStream catC: buffer];
  (void)sprintf(buffer," currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer," initialSeeds[%02d] = %24u\n", i, initialSeeds[i]);
    [outStream catC: buffer];
  }

  (void)sprintf(buffer,"      state1 = %24u %16u\n", x10, x20);
  [outStream catC: buffer];
  (void)sprintf(buffer,"      state2 = %24u %16u\n", x11, x21);
  [outStream catC: buffer];
  (void)sprintf(buffer,"      state3 = %24u %16u\n", x12, x22);
  [outStream catC: buffer];

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end


