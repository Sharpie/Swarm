// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            PMMLCGgen.m
Description:     Prime Modulus Multiplicative Linear Congruential Generator
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

#import <random/PMMLCGgen.h>

@implementation PMMLCGgen


// Import common snippets of code:

#import "Common.gens.create.m"

// #import "Common.gens.genSeeds.m"	// use local copy

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
  unsigned int state;
  unsigned int initialSeeds[1];
} state_struct_t;


-setState {

// Fill state variables from initialSeeds vector,
// assuming initialSeeds contains valid seeds.
// Perform any needed transformations:

   state = initialSeeds[0];	// initialSeeds[] is never zero

   currentCount = 0;

   return self;
}


-initState {

// Override this method in each subclassed generator

   [InvalidCombination raiseEvent:
   "PMMLCG initState: superclass method invoked - Yell at Sven!\n"];

   return self;
}

-generateSeeds {

   // PMMLCG only needs one seed, so we just copy initialSeed. But:
   // Since initialSeed may be greater than modulus for the PMMLCG,
   // we need to reduce it to a valid value:

   initialSeeds[0] = initialSeed % (maxSeedValues[0] + 1);
   if (initialSeeds[0] == 0)		 	// cannot have that
      initialSeeds[0] = 0x01555555;		// arbitrary value

   return self;
}



// ----- Published creation methods: -----

+createBegin: aZone {
  PMMLCGgen * aGenerator;

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
   int high, low, test;
   unsigned int new;

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

// Calculate the new state value:

   high = state / q;
   low = state % q;
   test = a*low - r*high;
   if (test > 0)
     state = test;
   else
     state = test + m;

// If needed, transform state to yield output in [0, unsignedMax]:

   new = state - 1;

   if (antiThetic) return (unsignedMax - new);
   else return new;
}


/*
-(unsigned) getUnsignedSample {
// This version uses 64-bit math (long long int), which is easier to
// read because it mimics the actual algorithm. Unfortunately, on the
// Intel x86 at least, it is about 35% slower than the Schrage method.

  unsigned long long int workLongInt; // working variable
  // unsigned long long int la, lm;   // parameters

// Update count of variates delivered:

  currentCount++;

// Test if generator has exceeded its cycle:
// (countMax is exact)

   if (currentCount >= countMax) {
   printf("\n*** \n");
   printf("*** WARNING %s: generator has exhausted its cycle of\n", genName);
   printf("*** %llu variates! You need to use a better generator!\n", countMax);
   printf("*** (resetting counter)\n");
   printf("*** \n\n");
   currentCount = 0;
   }

// Generate the next 'random' value from the state.
// We use 64-bit a working variable to avoid the Schrage a,m,q,r workaround.

  workLongInt = (la * state) % lm;	// 64 bits
  state = workLongInt;			// back to 32 bits

// If needed, transform state to yield output in [0, unsignedMax]:

  new = state - 1;

  if (antiThetic) return (unsignedMax - new);
  else return new;
}
*/


// ----- protocol InternalState: -----


-(void) putStateInto: (void *) buffer {
   state_struct_t * stateBuf;

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

  stateBuf->state = state;
  stateBuf->initialSeeds[0] = initialSeeds[0];

  // nothing returned from a (void) function
}

-(void) setStateFrom: (void *) buffer {
   state_struct_t * stateBuf;

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

  state = stateBuf->state;
  initialSeeds[0] = stateBuf->initialSeeds[0];

  // nothing returned from a (void) function
}


-(void) describe: outStream {
  char buffer[128];

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
  (void)sprintf(buffer,"            m = %24d\n", m);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            q = %24d\n", q);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            r = %24d\n", r);
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
  (void)sprintf(buffer,"     maxSeeds[0] = %21u\n", maxSeedValues[0]);
  [outStream catC: buffer];
  (void)sprintf(buffer," initialSeeds[0] = %21u\n", initialSeeds[0]);
  [outStream catC: buffer];

  (void)sprintf(buffer,"        state = %24u\n", state);
  [outStream catC: buffer];

  (void)sprintf(buffer," currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end

@implementation PMMLCG1gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG1",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*1 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 16807;
   m = 0x7fffffff;		// 2^31-1

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG1gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG1gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG1gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG1gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG1gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG1gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation PMMLCG2gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG2",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*2 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 48271;
   m = 0x7fffffff;		// 2^31-1

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG2gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG2gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG2gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG2gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation PMMLCG3gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG3",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*3 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 69621;
   m = 0x7fffffff;		// 2^31-1

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG3gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG3gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG3gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG3gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation PMMLCG4gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG4",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*4 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 45991;
   m = 0x7fffffff;		// 2^31-1

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG4gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG4gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG4gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG4gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG4gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG4gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation PMMLCG5gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG5",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*5 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 207707;
   m = 2147483543;		// 2^31-105

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG5gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG5gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG5gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG5gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG5gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG5gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation PMMLCG6gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG6",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*6 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 138556;
   m = 2147483423;		// 2^31-225

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG6gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG6gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG6gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG6gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG6gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG6gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation PMMLCG7gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG7",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*7 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 49689;
   m = 2147483323;		// 2^31-325

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG7gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG7gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG7gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG7gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG7gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG7gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation PMMLCG8gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG8",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*8 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 40014;
   m = 2147483563;		// 2^31-85

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG8gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG8gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG8gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG8gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG8gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG8gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


@implementation PMMLCG9gen

-initState {

// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy(genName,"PMMLCG9",sizeof(genName));
   genMagic = PMMLCGMAGIC + GENSUBMASK*9 + PMMLCGREVISION;  // see RandomDefs.h

// Set the parameters:

   a = 40692;
   m = 2147483399;		// 2^31-249

   q = m / a;			// Schrage quotient
   r = m % a;			// Schrage remainder

// For single-seed startup:
   initialSeed = 0;

// For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = m-1;

// State size for getState and setState:
   stateSize = sizeof(state_struct_t);

// Actual countMax = m-1 << 2^63 here, so:
   countMax = m-1;

// Math is modulo m, so max state value is m-1,
// from which we subtract 1, so:
   unsignedMax = m-2;

// We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;

  return self;
}

// ----- Published creation methods: -----

+create: (id) aZone setStateFromSeed: (unsigned) seed {
  PMMLCG9gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG9gen createBegin: aZone];

// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: seed];

  return [ aGenerator createEnd ];

}


+create: (id) aZone setStateFromSeeds: (unsigned *) seeds {
  PMMLCG9gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG9gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];

  return [ aGenerator createEnd ];

}

+createWithDefaults: (id) aZone {
  PMMLCG9gen * aGenerator;

// Allocate space for the object:

  aGenerator = [PMMLCG9gen createBegin: aZone];
  
// initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h

  return [ aGenerator createEnd ];

}


@end


