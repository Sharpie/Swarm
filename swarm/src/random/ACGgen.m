// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            ACGgen.m
Description:     Additive Congruential Generator
Library:         random
Original Author: Nelson Minar
Date:            1996-09-09
Modified by:     Sven Thommesen
Date:            1997-01-15   (v. 0.6)
Modified by:     Sven Thommesen
Date:            1997-09-01   (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/ACGgen.h>

@implementation ACGgen


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
  unsigned int index;
  unsigned int stateVec;   // start of vector to hold state and initialSeeds
} state_struct_t;


-setState {
   int i;

// Fill state variables from initialSeeds vector,
// assuming initialSeeds contains valid seeds:

   for (i = 0; i < r; i++) 
     state[i] = initialSeeds[i];

// Point to the beginning of the state vector:

   index = 0;

// If needed, draw a number of variates
// to escape rho-sequences:
   // [self runup: r];

// Comment: ACG is single-cycle and does not need runup.
// Eliminated in v. 0.7.

   currentCount = 0;

   return self;
}


-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"ACG",sizeof(genName));
   genMagic = ACGMAGIC + GENSUBMASK*1 + ACGREVISION;	// see RandomDefs.h

// Set the parameters:

   r = 55;
   s = 24;

// Make sure generator was initialized with good parameters:

   if ( (r > s) && (s > 0) ) {
   // all is fine
   } else {
   [InvalidCombination raiseEvent: 
    "%s: Initialization error: need 0 < s < r\n", genName];
   }

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = r;			// state vector
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i<r; i++)
     maxSeedValues[i] = 0xfffffffe;		// to avoid degenerate state

// State size for getState and setState:
   stateSize = sizeof(state_struct_t) + 2*lengthOfSeedVector*sizeof(int);

// Actual countMax = 2^r =2^55 here (single full cycle). Thus,
   countMax = (1ull << r);

// Math is modulo m = 2^32, so max output value is m-1:
   unsignedMax = 0xffffffff;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

   return self;
}



// ----- Published creation methods: -----

+createBegin: aZone {
  ACGgen * aGenerator;

// Allocate space for the object:

  aGenerator = [super createBegin: aZone];

// initialize instance variables:

  aGenerator->currentCount = TESTCASE;

// initialize fixed parts of state:

  [aGenerator initState];	// must be called before setStateFromSeed

  return aGenerator;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  ACGgen * aGenerator;

// Allocate space for the object:

  aGenerator = [ACGgen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  ACGgen * aGenerator;

// Allocate space for the object:

  aGenerator = [ACGgen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  ACGgen * aGenerator;

// Allocate space for the object:

  aGenerator = [ACGgen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


// ----- Get unsigned random value: -----

-(unsigned) getUnsignedSample {
   unsigned int rth, sth, new;

// Update count of variates delivered:
// (cycle is > 2^63, so report that counter is exhausted)

   currentCount++;

// Give diagnostic warning that we're exceeding the counter:

   if (currentCount >= countMax) {
   printf("\n*** \n");
   printf("*** WARNING %s: generator has exhausted its cycle of\n", genName);
   printf("*** %llu variates! You need to use a better generator!\n", countMax);
   printf("*** (resetting counter)\n");
   printf("*** \n\n");
   currentCount = 0;
   }

// Generate the next 'random' value from the state.

   rth = state[index];
   if (s <= index)
     sth = state[index-s];
   else
     sth = state[index+r-s];

// -----------
   new = sth + rth;			// implicitly modulo 2^32
// -----------

   state[index] = new;

// Move index to next slot in state vector:

   index++;
   if (index >= r) index = 0;

// If needed, transform state to yield output in [0,unsignedMax]:

   if (antiThetic) return (unsignedMax - new);
   else return new;
}


// ----- protocol InternalState: -----


-(void) putStateInto: (void *) buffer {
   state_struct_t * stateBuf;
   int i;
   unsigned int *bufArr;

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
  stateBuf->index        = index;

  bufArr = &(stateBuf->stateVec);

  for (i=0; i<lengthOfSeedVector; i++)
    bufArr[i]   = state[i];

  for (i=0; i < lengthOfSeedVector; i++)
    bufArr[lengthOfSeedVector+i] = initialSeeds[i];

  // nothing returned from a (void) function
}

-(void) setStateFrom: (void *) buffer {
   state_struct_t * stateBuf;
   int i;
   unsigned int *bufArr;

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
  index        = stateBuf->index;

  bufArr = &(stateBuf->stateVec);
  
  for (i=0; i<lengthOfSeedVector; i++)
    state[i]   = bufArr[i];

  for (i=0; i<lengthOfSeedVector; i++)
    initialSeeds[i] = bufArr[lengthOfSeedVector+i];

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

  (void)sprintf(buffer,"            m =               4294967296\n");
  [outStream catC: buffer];
  (void)sprintf(buffer,"            r = %24d\n", r);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            s = %24d\n", s);
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
  (void)sprintf(buffer," singleInitialSeed = %19u\n", singleInitialSeed);
  [outStream catC: buffer];
  (void)sprintf(buffer,"        index = %24u\n", index);
  [outStream catC: buffer];
  (void)sprintf(buffer," currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer,"     maxSeeds[%02d] = %20u\n", i, maxSeedValues[i]);
    [outStream catC: buffer];
  }

  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer," initialSeeds[%02d] = %20u\n", i, initialSeeds[i]);
    [outStream catC: buffer];
  }

  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer,"        state[%02d] = %20u\n", i, state[i]);
    [outStream catC: buffer];
  }

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end
