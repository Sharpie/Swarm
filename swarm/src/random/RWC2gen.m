// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            RWC2gen.m
Description:     Multiply-With-Carry generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01   (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/RWC2gen.h>

@implementation RWC2gen


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
  unsigned int x1, x2, carry;
} state_struct_t;


-setState {

// Fill state variables from initialSeeds vector,
// assuming initialSeeds contains valid seeds.
// Perform any necessary transformations:

   carry = initialSeeds[0];
   x1    = initialSeeds[1];
   x2    = initialSeeds[2];

// If needed, draw a number of variates
// to escape rho-sequences:
   [self runup: 2];

// RWC generators may have rho sequences of at most length r

   currentCount = 0;

   return self;
}


-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"RWC2",sizeof(genName));
   genMagic = RWC2MAGIC + GENSUBMASK*1 + RWC2REVISION;  // see RandomDefs.h

// Set the parameters:

   a  = 1111111464;

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = SEEDS;			// state vector
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   maxSeedValues[0] = a+a-1;			// max carry
   maxSeedValues[1] = 0xfffffffe;		// x1
   maxSeedValues[2] = 0xfffffffe;		// x2

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual period ~= 2^92, so we set:
   countMax = (1ull << 63);

// Math is modulo m, so max output value is m-1:
   unsignedMax = 0xffffffff;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}


// ----- Published creation methods: -----


+createBegin: aZone {
  RWC2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [super createBegin: aZone];

// initialize instance variables:

  aGenerator->currentCount = TESTCASE;

// initialize fixed parts of state:

  [aGenerator initState];	// must be called before setStateFromSeed

  return aGenerator;
}


+create: (id) aZone setStateFromSeed: (unsigned) seed {
  RWC2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [RWC2gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  RWC2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [RWC2gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  RWC2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [RWC2gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}



// ----- Get unsigned random value: -----

-(unsigned) getUnsignedSample {

// Update count of variates delivered:
// (cycle is > 2^63, so report that counter is exhausted)

   currentCount++;

// Give diagnostic warning that we're exceeding the counter:

   if (currentCount >= countMax) {
   printf("\n*** \n");
   printf("*** NOTICE %s: generator has created 2^63 variates\n", genName);
   printf("*** out of a cycle of length at least 2^92\n");
   printf("*** (resetting counter)\n");
   printf("*** \n\n");
   currentCount = 0;
   }

// -----------

// Generate the next 'random' value from the state.

// For each period, we have 
//    x     = a*(x1+x2) + carry % m  
//    carry = a*(x1+x2) + carry / m
//    x2 = x1; x1 = x
// We have m = 2^32, and conveniently store x as the low 32 bits and the
// carry as the high 32 bits in an unsigned long long int (64 bits).

  L = ( (unsigned long long) x1 + x2) * a + carry;	// 64 bits
  x2    = x1;						// age the x's
  carry = (unsigned) (L >> 32);				// top 32 bits
  x1    = (unsigned) (L & 0xffffffff);			// bottom 32 bits

  lastX = x1;

// -----------

   if (antiThetic) return (unsignedMax - lastX);
   else return lastX;
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

  stateBuf->x1 = x1;
  stateBuf->x2 = x2;
  stateBuf->carry = carry;

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

  x1 = stateBuf->x1;
  x2 = stateBuf->x2;
  carry = stateBuf->carry;

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


  (void)sprintf(buffer,"            a = %24d\n", a);
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
    (void)sprintf(buffer,"     maxSeeds[%02d] = %20u\n", i, maxSeedValues[i]);
    [outStream catC: buffer];
  }
  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer," initialSeeds[%02d] = %20u\n", i, initialSeeds[i]);
    [outStream catC: buffer];
  }

  (void)sprintf(buffer,"               x1 = %20u\n", x1);
  [outStream catC: buffer];
  (void)sprintf(buffer,"               x2 = %20u\n", x2);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            carry = %20u\n", carry);
  [outStream catC: buffer];
  (void)sprintf(buffer,"                L = %20llu\n", L);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            lastX = %20u\n", lastX);
  [outStream catC: buffer];

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end

