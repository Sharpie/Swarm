// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            C2TAUSgen.m
Description:     Combined Tausworthe Generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01   (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/C2TAUSgen.h>

@implementation C2TAUSgen


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
  unsigned int initialSeeds[SEEDS];
  unsigned int state[SEEDS];
} state_struct_t;


-setState {
   int i;

// Fill state variables from initialSeeds vector,
// assuming initialSeeds contains valid seeds:

   for (i = 0; i < lengthOfSeedVector; i++) 
     state[i] = initialSeeds[i];	// initialSeeds[] is never zero

// If needed, draw a number of variates
// to escape rho-sequences:
   // [self runup: 5*r];

// These Tausworthe generators all have a single full cycle,
// and do not need runup.

   currentCount = 0;

   return self;
}


-initState {

// Override this method in each subclassed generator

   [InvalidCombination raiseEvent:
   "C2TAUS initState: superclass method invoked - Yell at Sven!\n"];

   return self;
}



// ----- Published creation methods: -----

+createBegin: aZone {
  C2TAUSgen * aGenerator;

// Allocate space for the object:

  aGenerator = [super createBegin: aZone];

// initialize instance variables:

  aGenerator->currentCount = TESTCASE;

// initialize fixed parts of state:

  [aGenerator initState];	// must be called before setStateFromSeed

  return aGenerator;
}

// NOTE: the creation methods are found in the subclasses below



// ----- Get unsigned random value: -----

-(unsigned) getUnsignedSample {
   unsigned int b;

// Update count of variates delivered:

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

// -----------

// Generate the next 'random' value from the state.

   // Component number 1:

          b = ((state[0] << Q[0]) ^      state[0]) & Mask[0];
   state[0] = ((state[0] << S[0]) ^ (b >> PmS[0])) & Mask[0];

   // Component number 2:

          b = ((state[1] << Q[1]) ^      state[1]) & Mask[1];
   state[1] = ((state[1] << S[1]) ^ (b >> PmS[1])) & Mask[1];

   // Combine them:

    b = (state[0] ^ (state[1] << P1mP2) );

// -----------

   if (antiThetic) return (unsignedMax - b);
   else return b;
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

  for (i=0; i<lengthOfSeedVector; i++)
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

  (void)sprintf(buffer,"            P = %24u %16u\n", P[0], P[1]);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            Q = %24u %16u\n", Q[0], Q[1]);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            S = %24u %16u\n", S[0], S[1]);
  [outStream catC: buffer];
  (void)sprintf(buffer,"          P-S = %24u %16u\n", PmS[0], PmS[1]);
  [outStream catC: buffer];
  (void)sprintf(buffer,"         Mask = %24u %16u\n", Mask[0], Mask[1]);
  [outStream catC: buffer];
  (void)sprintf(buffer,"        P1-P2 = %24u\n", P1mP2);
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

  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer,"    state[%02d] = %24u\n", i, state[i]);
    [outStream catC: buffer];
  }

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end

@implementation C2TAUS1gen

-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"C2TAUS1",sizeof(genName));
   genMagic = C2TAUSMAGIC + GENSUBMASK*1 + C2TAUSREVISION;  // see RandomDefs.h

// Set the parameters:

   P[0] = 31;
   Mask[0] = (1u << P[0]) - 1;
   S[0] = 12;
   PmS[0] = P[0] - S[0];
   Q[0] = 13;

   P[1] = 29;
   Mask[1] = (1u << P[1]) - 1;
   S[1] = 17;
   PmS[1] = P[1] - S[1];
   Q[1] =  2;

   P1mP2 = P[0] - P[1];

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 2;
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i<lengthOfSeedVector; i++)
     maxSeedValues[i] = Mask[i];		// check!

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax < 2^63:
   countMax = Mask[0] * Mask[1];		// 2^60

// Math is modulo m, so max output value is m-1:
   unsignedMax = Mask[0];			// 2^31-1

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  C2TAUS1gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS1gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  C2TAUS1gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS1gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  C2TAUS1gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS1gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation C2TAUS2gen

-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"C2TAUS2",sizeof(genName));
   genMagic = C2TAUSMAGIC + GENSUBMASK*2 + C2TAUSREVISION;  // see RandomDefs.h

// Set the parameters:

   P[0] = 31;
   Mask[0] = (1u << P[0]) - 1;
   S[0] = 21;
   PmS[0] = P[0] - S[0];
   Q[0] =  3;

   P[1] = 29;
   Mask[1] = (1u << P[1]) - 1;
   S[1] = 17;
   PmS[1] = P[1] - S[1];
   Q[1] =  2;

   P1mP2 = P[0] - P[1];

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 2;
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i<lengthOfSeedVector; i++)
     maxSeedValues[i] = Mask[i];		// check!

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax < 2^63:
   countMax = Mask[0] * Mask[1];		// 2^60

// Math is modulo m, so max output value is m-1:
   unsignedMax = Mask[0];			// 2^31-1

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

   return self;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  C2TAUS2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS2gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  C2TAUS2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS2gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  C2TAUS2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS2gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation C2TAUS3gen

-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"C2TAUS3",sizeof(genName));
   genMagic = C2TAUSMAGIC + GENSUBMASK*3 + C2TAUSREVISION;  // see RandomDefs.h

// Set the parameters:

   P[0] = 31;
   Mask[0] = (1u << P[0]) - 1;
   S[0] = 13;
   PmS[0] = P[0] - S[0];
   Q[0] = 13;

   P[1] = 29;
   Mask[1] = (1u << P[1]) - 1;
   S[1] = 20;
   PmS[1] = P[1] - S[1];
   Q[1] =  2;

   P1mP2 = P[0] - P[1];

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 2;
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i<lengthOfSeedVector; i++)
     maxSeedValues[i] = Mask[i];		// check!

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax < 2^63:
   countMax = Mask[0] * Mask[1];		// 2^60

// Math is modulo m, so max output value is m-1:
   unsignedMax = Mask[0];			// 2^31-1

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

   return self;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  C2TAUS3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS3gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  C2TAUS3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS3gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  C2TAUS3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [C2TAUS3gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


