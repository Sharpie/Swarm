// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            MRGgen.m
Description:     Multiple Recursive [LCG] Generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01   (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/MRGgen.h>

@implementation MRGgen


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
  unsigned int initialSeeds[MAXLAG];
  unsigned int state[MAXLAG];
} state_struct_t;


-setState {
   int i;

// Fill state variables from initialSeeds vector,
// assuming initialSeeds contains valid seeds.
// Perform any necessary transformations:

   for (i = 0; i < lengthOfSeedVector; i++) 
     state[i] = initialSeeds[i];	// initialSeeds[] is never zero

// If needed, draw a number of variates
// to escape rho-sequences:
   // [self runup: 5*r];

// MRGx are full-cycle generators which do not need runup

   currentCount = 0;

   return self;
}


-initState {

// Override this method in each subclassed generator

   [InvalidCombination raiseEvent:
   "MRG initState: superclass method invoked - Yell at Sven!\n"];

   return self;
}


// ----- Published creation methods: -----


+createBegin: aZone {
  MRGgen * aGenerator;

// Allocate space for the object:

  aGenerator = [super createBegin: aZone];

// initialize instance variables:

  aGenerator->currentCount = TESTCASE;

// initialize fixed parts of state:

  [aGenerator initState];	// must be called before setStateFromSeed

  return aGenerator;
}

// NOTE: creation methods are located in the subclasses below



// ----- Get unsigned random value: -----

-(unsigned) getUnsignedSample {
   int h1, hk, p1, pk;
   int x1;
   unsigned int xk;
   int i;

// Update count of variates delivered:
// (cycle is > 2^63, so report that counter is exhausted)

   currentCount++;

// Give diagnostic warning that we're exceeding the counter:

   if (currentCount >= countMax) {
   printf("\n*** \n");
   printf("*** NOTICE %s: generator has created 2^63 variates\n", genName);
   printf("*** out of a cycle of length at least 2^155\n");
   printf("*** (resetting counter)\n");
   printf("*** \n\n");
   currentCount = 0;
   }

// -----------

// Generate the next 'random' value from the state.

// General formula: next = SUMi{ (-1)^i * ((ai*xi) mod mi) }
// though in our case, only a1 and ak are nonzero, and m1=mk=m.

// The state vector contains the k past values of the generator.

// Comment: depending on the subclass executing this code,
// the index 'k' is either 5, 6, or 7.

// Calculate next step from lag #k (k=5,6, or 7):
   xk = state[k-1];
   hk = xk / qk;
   pk = ak * (xk - hk * qk) - hk * rk;
   if (pk > 0) pk -= m;			// ensures the result is negative

// Shift the other lagged results:
   for (i=k-1; i>0; i--)
     state[i] = state[i-1];

// Calculate next step from lag #1:
   x1 = state[0];
   h1 = x1 / q1;
   p1 = a1 * (x1 - h1 * q1) - h1 * r1;
   if (p1 < 0) p1 += m;			// ensures the result is positive

// Combine the results into a new term:
   x1 = p1 + pk;			// actually, p1 - pk since pk < 0
   if (x1 < 0) x1 += m;			// make sure result is positive

// Store the new term:
   state[0] = x1;

// -----------

   if (antiThetic) return (unsignedMax - x1);
   else return x1;
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

  for (i=0; i<lengthOfSeedVector; i++)
    stateBuf->state[i] = state[i];

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

  for (i=0; i<lengthOfSeedVector; i++)
    state[i] = stateBuf->state[i];

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
  (void)sprintf(buffer,"            m = %24d %16d\n", m, m);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            a = %24d %16d\n", a1, ak);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            q = %24d %16d\n", q1, qk);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            r = %24d %16d\n", r1, rk);
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
    (void)sprintf(buffer,"     maxSeeds[%02d] = %24u\n", i, maxSeedValues[i]);
    [outStream catC: buffer];
  }

  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer," initialSeeds[%02d] = %24u\n", i, initialSeeds[i]);
    [outStream catC: buffer];
  }

  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer,"        state[%02d] = %24u\n", i, state[i]);
    [outStream catC: buffer];
  }

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end

@implementation MRG5gen

-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"MRG5",sizeof(genName));
   genMagic = MRGMAGIC + GENSUBMASK*1 + MRGREVISION;  // see RandomDefs.h

// Set the parameters:

   k  =           5;
   m  =  2147483647;	// 2^31-1
   a1 =   107374182;
   ak =      104480;	// a5

   q1 = m / a1;
   r1 = m % a1;

   qk = m / ak;		// q5
   rk = m % ak;		// r5

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = k;			// state vector
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i < lengthOfSeedVector; i++)
     maxSeedValues[i] = m - 1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = (m-1)^k -1 or ~= 2^155
// Since this vastly exceeds the counter variable, instead we set:
   countMax = (1ull << 63);			// 2^63

// Math is modulo m, so max output value is m-1:
   unsignedMax = m - 1;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

   return self;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  MRG5gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG5gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  MRG5gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG5gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  MRG5gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG5gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation MRG6gen

-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"MRG6",sizeof(genName));
   genMagic = MRGMAGIC + GENSUBMASK*2 + MRGREVISION;  // see RandomDefs.h

// Set the parameters:

   k  =           6;
   m  =  2147483647;	// 2^31-1
   a1 =      177786;
   ak =       64654;	// a6

   q1 = m / a1;
   r1 = m % a1;

   qk = m / ak;		// q6
   rk = m % ak;		// r6

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = k;			// state vector
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i < lengthOfSeedVector; i++)
     maxSeedValues[i] = m - 1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = (m-1)^k -1 or ~= 2^183
// Since this vastly exceeds the counter variable, instead we set:
   countMax = (1ull << 63);			// 2^63

// Math is modulo m, so max output value is m-1:
   unsignedMax = m - 1;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  MRG6gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG6gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  MRG6gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG6gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  MRG6gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG6gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation MRG7gen

-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"MRG7",sizeof(genName));
   genMagic = MRGMAGIC + GENSUBMASK*3 + MRGREVISION;  // see RandomDefs.h

// Set the parameters:

   k  =           7;
   m  =  2147483629;	// 2^31-19
   a1 =     1071064;
   ak =     2113664;	// a7

   q1 = m / a1;
   r1 = m % a1;

   qk = m / ak;		// q7
   rk = m % ak;		// r7

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = k;			// state vector
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i < lengthOfSeedVector; i++)
     maxSeedValues[i] = m - 1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = (m-1)^k -1 or ~= 2^217
// Since this vastly exceeds the counter variable, instead we set:
   countMax = (1ull << 63);			// 2^63

// Math is modulo m, so max output value is m-1:
   unsignedMax = m - 1;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  MRG7gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG7gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  MRG7gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG7gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  MRG7gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MRG7gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


