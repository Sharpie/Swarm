// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            MWCAgen.m
Description:     Multiply-With-Carry generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01   (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/MWCAgen.h>

@implementation MWCAgen


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
// assuming initialSeeds contains valid seeds.
// Perform any necessary transformations:

   K = initialSeeds[0];
   J = initialSeeds[1];

// If needed, draw a number of variates
// to escape rho-sequences:
   // [self runup: 5*r];

// MWC are full-cycle generators which do not need runup

   currentCount = 0;

   return self;
}


-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"MWCA",sizeof(genName));
   genMagic = MWCMAGIC + GENSUBMASK*1 + MWCREVISION;  // see RandomDefs.h

// Set the parameters:

   a  = 30903;
   b  = 18000;

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = SEEDS;			// state vector
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   maxSeedValues[0] = ((a-1) << 16) + 0xffff;
   maxSeedValues[1] = ((b-1) << 16) + 0xffff;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual period ~= 2^59, so we set:
   countMax = (1ull << 59);

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
  MWCAgen * aGenerator;

// Allocate space for the object:

  aGenerator = [super createBegin: aZone];

// initialize instance variables:

  aGenerator->currentCount = TESTCASE;

// initialize fixed parts of state:

  [aGenerator initState];	// must be called before setStateFromSeed

  return aGenerator;
}


+create: (id) aZone setStateFromSeed: (unsigned) seed {
  MWCAgen * aGenerator;

// Allocate space for the object:

  aGenerator = [MWCAgen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  MWCAgen * aGenerator;

// Allocate space for the object:

  aGenerator = [MWCAgen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  MWCAgen * aGenerator;

// Allocate space for the object:

  aGenerator = [MWCAgen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}



// ----- Get unsigned random value: -----

-(unsigned) getUnsignedSample {
   // unsigned int new;

// Update count of variates delivered:
// (cycle is < 2^63, so report that cycle is exhausted)

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

// For each 16-bit component, we have 
//    x     = a*x + carry % m  
//    carry = a*x + carry / m
// We have m = 2^16, and conveniently store 
// x as the low 16 bits and the carry as the high 16 bits in an unsigned int.
// We concatenate output from the two components for a 32-bit random number.

  K = a * (K & 65535) + (K >> 16); // K = a*K-lo + K-hi = a*k+carry mod 2^16
  J = b * (J & 65535) + (J >> 16); // J = b*J-lo + J-hi = b*j+carry mod 2^16
  lastX = (K << 16) + J; 	// Marsaglia's code

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

  stateBuf->state[0] = K;
  stateBuf->state[1] = J;

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

  K = stateBuf->state[0];
  J = stateBuf->state[1];

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
  (void)sprintf(buffer,"            b = %24d\n", b);
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

  (void)sprintf(buffer,"        state[00] = %20u\n", K);
  [outStream catC: buffer];
  (void)sprintf(buffer,"        state[01] = %20u\n", J);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            lastX = %20u\n", lastX);
  [outStream catC: buffer];

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end

