// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            MT19937gen.m
Description:     Twisted GFSR generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01   (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/MT19937gen.h>

@implementation MT19937gen


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
  unsigned int state[SEEDS];
  unsigned int initialSeeds[SEEDS];
} state_struct_t;


-setState {
   int i;

// Fill state variables from initialSeeds vector,
// assuming initialSeeds contains valid seeds:

   for (i = 0; i < lengthOfSeedVector; i++) 
     state[i] = initialSeeds[i];		// initialSeeds[] is never zero

// Point to the beginning of the state vector:

   index = 1;		// That's what Matsumoto does, rather than = 0

// If needed, draw a number of variates
// to escape rho-sequences:
   // [self runup: 5*r];

// These generators are full-cycle and do not need runup

   currentCount = 0;

   return self;
}


-initState {
   int i;

// This method is called from createBegin.

// Set the 'personality' of this generator:
strncpy(genName,"MT19937",sizeof(genName));
genMagic = MT19937MAGIC + GENSUBMASK*1 + MT19937REVISION; // see RandomDefs.h

// Set the parameters:

   N    = 624;
   M    = 397;
   a[0]              = 0;			// mag01[0]
   a[1]              = 0x9908b0df;		// mag01[1] = MATRIX_A
   UPPER_MASK        = 0x80000000;
   LOWER_MASK        = 0x7fffffff;
   TEMPERING_MASK_B  = 0x9d2c5680;
   TEMPERING_MASK_C  = 0xefc60000;

   TEMPERING_SHIFT_U = 11;
   TEMPERING_SHIFT_S =  7;
   TEMPERING_SHIFT_T = 15;
   TEMPERING_SHIFT_L = 18;

// Make sure generator was initialized with good parameters:

   if ( (N > M) && (M > 0) ) {
   // all is fine
   } else {
   [InvalidCombination raiseEvent: 
    "%s: Initialization error: need 0 < M < N\n", genName];
   }

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = SEEDS;			// = N
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   for (i=0; i < lengthOfSeedVector; i++)
     maxSeedValues[i] = 0xffffffff;		// 2^32-1

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = 2^19937
// Since this vastly exceeds the counter variable, instead we set:
   countMax = (1ull << 63);			// 2^63

// Math is modulo m, so max output value is m-1:
   unsignedMax = 0xffffffff;	// 2^32-1

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

   return self;
}

// ----- Published creation methods: -----

+createBegin: aZone {
  MT19937gen * aGenerator;

// Allocate space for the object:

  aGenerator = [super createBegin: aZone];

// initialize instance variables:

  aGenerator->currentCount = TESTCASE;

// initialize fixed parts of state:

  [aGenerator initState];	// must be called before setStateFromSeed

  return aGenerator;
}

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  MT19937gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MT19937gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}

+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  MT19937gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MT19937gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  MT19937gen * aGenerator;

// Allocate space for the object:

  aGenerator = [MT19937gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}

// ----- Get unsigned random value: -----

-(unsigned) getUnsignedSample {
  unsigned int y;

// Update count of variates delivered:
// (cycle is > 2^63, so report that counter is exhausted)

   currentCount++;

// Give diagnostic warning that we're exceeding the counter:

   if (currentCount >= countMax) {
   printf("\n*** \n");
   printf("*** NOTICE %s: generator has created 2^63 variates\n", genName);
   printf("*** out of a cycle of length 2^19937\n");
   printf("*** (resetting counter)\n");
   printf("*** \n\n");
   currentCount = 0;
   }

// -----------

// Generate the next 'random' value from the state.

  if (index == N)		{ /* generate N words at one time */
	int kk;

	for (kk=0;kk<N-M;kk++) {
	  y = (state[kk]&UPPER_MASK)|(state[kk+1]&LOWER_MASK);
	  state[kk] = state[kk+M] ^ (y >> 1) ^ a[y & 0x1];
	}

	for (;kk<N-1;kk++) {
	  y = (state[kk]&UPPER_MASK)|(state[kk+1]&LOWER_MASK);
	  state[kk] = state[kk+(M-N)] ^ (y >> 1) ^ a[y & 0x1];
	}

	y = (state[N-1]&UPPER_MASK)|(state[0]&LOWER_MASK);
	state[N-1] = state[M-1] ^ (y >> 1) ^ a[y & 0x1];
	
	index = 0;		// on subsequent rounds we start at 0
  }
  
  y = state[index++];

//  y ^= TEMPERING_SHIFT_U(y);
//  y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
//  y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
//  // y &= 0xffffffff; /* you may delete this line if word size = 32 */
//  y ^= TEMPERING_SHIFT_L(y);

  y ^= ( y >> TEMPERING_SHIFT_U );
  y ^= ( ( y << TEMPERING_SHIFT_S ) & TEMPERING_MASK_B );
  y ^= ( ( y << TEMPERING_SHIFT_T ) & TEMPERING_MASK_C );
  y &= 0xffffffff; /* you may delete this line if word size = 32 */
  y ^= ( y >> TEMPERING_SHIFT_L );

// -----------

   if (antiThetic) return (unsignedMax - y);
   else return y;
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
  stateBuf->index        = index;

  for (i=0; i < lengthOfSeedVector; i++)
    stateBuf->state[i]   = state[i];

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
  index        = stateBuf->index;

  for (i=0; i<lengthOfSeedVector; i++)
    state[i]   = stateBuf->state[i];

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

  (void)sprintf(buffer,"            w = %24u\n", 32);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            N = %24u\n", N);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            M = %24u\n", M);
  [outStream catC: buffer];
  (void)sprintf(buffer,"      shift u = right %18u\n", 11);
  [outStream catC: buffer];
  (void)sprintf(buffer,"      shift s = left  %18u\n", 7);
  [outStream catC: buffer];
  (void)sprintf(buffer,"      shift t = left  %18u\n", 15);
  [outStream catC: buffer];
  (void)sprintf(buffer,"      shift l = right %18u\n", 18);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            b = %24u\n", TEMPERING_MASK_B);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            c = %24u\n", TEMPERING_MASK_C);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            a = %24u\n", a[1]);
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
  (void)sprintf(buffer,"        index = %24u\n", index);
  [outStream catC: buffer];
  (void)sprintf(buffer," currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  [outStream catC: "\n"];
  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer," initialSeeds[%03d] = %19u\n", i, initialSeeds[i]);
    [outStream catC: buffer];
  }

  [outStream catC: "\n"];
  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer,"     maxSeeds[%03d] = %19u\n", i, maxSeedValues[i]);
    [outStream catC: buffer];
  }

  [outStream catC: "\n"];
  for (i=0; i<lengthOfSeedVector; i++) {
    (void)sprintf(buffer,"        state[%03d] = %19u\n", i, state[i]);
    [outStream catC: buffer];
  }

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}


