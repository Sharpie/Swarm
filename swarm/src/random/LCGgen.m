// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            LCGgen.m
Description:     Linear Congruential Generator
Library:         random
Original Author: Nelson Minar
Date:            1996-09-09
Modified by:     Sven Thommesen
Date:            1997-01-15   (v. 0.6)
Modified by:     Sven Thommesen
Date:            1997-09-01   (v. 0.7)
Modified by:	 Sven Thommesen
Date:		 1998-10-08   (v. 0.8)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/LCGgen.h>

@implementation LCGgen


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
  unsigned int state;
  unsigned int initialSeeds[1];
  unsigned long long int currentCount;
} state_struct_t;



PHASE(Creating)

#import "include.gens.creating.m"

- initState
{
// Override this method in each subclassed generator
  
  [InvalidCombination
    raiseEvent:
      "LCG initState: superclass method invoked - Yell at Sven!\n"];
  
  return self;
}

+ create: aZone setStateFromSeed: (unsigned) seed
{
  // this is an empty method, to be overridden by the subclasses
  return self;
}

+ create: aZone setStateFromSeeds: (unsigned *) seeds
{
  // this is an empty method, to be overridden by the subclasses
  return self;
}

+ createWithDefaults: aZone
{
  // this is an empty method, to be overridden by the subclasses
  return self;
}

+ createBegin: aZone
{
  LCGgen * aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [super createBegin: aZone];
  
  // initialize instance variables:
  
  aGenerator->currentCount = TESTCASE;
  
  // initialize fixed parts of state:
  
  [aGenerator initState];	// must be called before setStateFromSeed
  
  return aGenerator;
}


PHASE(Setting)
     
#import "include.gens.setting.m"

- localGenerateSeeds
{
  // LCG uses only one seed, which may take on any unsigned value.
  // So we just copy the single initialSeed:
  
  initialSeeds[0] = initialSeed;
  
  return self;
}


- generateSeeds
{
  [self localGenerateSeeds];	// different version
  return self;
}

- setState
{
  // Fill state variables from initialSeeds vector,
  // assuming initialSeeds contains valid seeds.
  // Perform any needed transformations:
  
  state = initialSeeds[0];	// initialSeeds[] is never zero
  
  currentCount = 0;
  
  return self;
}



PHASE(Using)
     
#import "include.gens.using.m"
     
- reset
{
  // Reset generator to the point of the last use of -setStateFromSeed(s).
  // Also reset counters.
  [self setState];
  return self;
}

- (unsigned)getUnsignedSample
{
  // Update count of variates delivered:
  // (cycle is > 2^63, so report that counter is exhausted)
  
  currentCount++;
  
  // Give diagnostic warning that we're exceeding the counter:
  
  if (currentCount >= countMax)
    {
      printf("\n*** \n");
      printf("*** WARNING %s: generator has exhausted its cycle of\n",
             genName);
      printf("*** %llu variates! You need to use a better generator!\n",
             countMax);
      printf("*** (resetting counter)\n");
      printf("*** \n\n");
      currentCount = 0;
    }
  
  // Calculate the new state value:
  
  state = (a * state) + c;	// implicitly modulo 2^32
  
  // If needed, transform state to yield output in [0, unsignedMax]:
  
  if (antiThetic)
    return (unsignedMax - state);
  else
    return state;
}

- (void)putStateInto: (void *)buffer
{
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

- (void)setStateFrom: (void *)buffer
{
  state_struct_t * stateBuf;
  
  // Recast the caller's pointer:
  stateBuf = (state_struct_t *) (buffer) ;
  
  // TEST the integrity of the external data:
  if ((stateBuf->genMagic  != genMagic)
      || (stateBuf->stateSize != stateSize))
    
    [InvalidCombination
      raiseEvent:
        "%u %s generator: your are passing bad data to setState!\n %u %u\n",
      genMagic, genName,
      stateBuf->genMagic,
      stateBuf->stateSize];
  
  // Place external data into internal state variables:
  
  antiThetic   = stateBuf->antiThetic;
  singleInitialSeed = stateBuf->singleInitialSeed;
  initialSeed  = stateBuf->initialSeed;
  currentCount = stateBuf->currentCount;
  
  state = stateBuf->state;
  initialSeeds[0] = stateBuf->initialSeeds[0];
  
  // nothing returned from a (void) function
}


- (void)describe: outStream
{
  char buffer[128];
  
  (void)sprintf (buffer,"%s Describe: \n",genName);
  [outStream catC: buffer];

  (void)sprintf (buffer,"      genName = %24s\n", genName);
  [outStream catC: buffer];
  (void)sprintf (buffer,"    stateSize = %24u\n", stateSize);
  [outStream catC: buffer];
  (void)sprintf (buffer,"     genMagic = %24u\n", genMagic);
  [outStream catC: buffer];

  (void)sprintf (buffer,"            m = %24d\n", a);
  (void)sprintf (buffer,"            m =               4294967296\n");
  [outStream catC: buffer];
  (void)sprintf (buffer,"            a = %24d\n", a);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            c = %24d\n", c);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   antiThetic = %24d\n", antiThetic);
  [outStream catC: buffer];

  (void)sprintf (buffer,"  unsignedMax = %24u\n", unsignedMax);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   invModMult = %24.16e\n", invModMult);
  [outStream catC: buffer];
  (void)sprintf (buffer,"  invModMult2 = %24.16e\n", invModMult2);
  [outStream catC: buffer];

  (void)sprintf (buffer,"  initialSeed = %24u\n", initialSeed);
  [outStream catC: buffer];
  (void)sprintf (buffer," singleInitialSeed = %19d\n", singleInitialSeed);
  [outStream catC: buffer];
  (void)sprintf (buffer,"     maxSeeds[0] = %21u\n", maxSeedValues[0]);
  [outStream catC: buffer];
  (void)sprintf (buffer," initialSeeds[0] = %21u\n", initialSeeds[0]);
  [outStream catC: buffer];

  (void)sprintf (buffer,"        state = %24u\n", state);
  [outStream catC: buffer];

  (void)sprintf (buffer," currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.

}

@end

@implementation LCG1gen

PHASE(Creating)

- initState
{
  // This method is called from createBegin.
  
// Set the 'personality' of this generator:
  strncpy (genName,"LCG1",sizeof (genName));
  genMagic = LCGMAGIC + GENSUBMASK * 1 + LCGREVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  a =    1664525U;
  c = 1013904223U;
  
  // For single-seed startup:
  initialSeed = 0;
  
  // For multi-seed startup:
  lengthOfSeedVector = 1;
  initialSeeds[0] = 0;
  maxSeedValues[0] = 0xffffffff;		// 2^32-1
  
  // State size for getState and setState:
  stateSize = sizeof(state_struct_t);
  
  // Actual countMax = 2^32 here, so:
  countMax = (1ull << 32);
  
  // Math is modulo m, so max output value is m-1:
  unsignedMax = 0xffffffff;			// 2^32-1
  
  // We pre-compute the divisor for converting to floating point:
  invModMult = (double) unsignedMax;
  invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
  invModMult2 = invModMult * invModMult;
  
  return self;
}

+ create: aZone setStateFromSeed: (unsigned)seed
{
  LCG1gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG1gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];
  
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  LCG1gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG1gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];

}

+ createWithDefaults: aZone
{
  LCG1gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG1gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
}

PHASE(Setting)

PHASE(Using)
     
     
@end

@implementation LCG2gen

PHASE(Creating)

- initState
{
// This method is called from createBegin.

// Set the 'personality' of this generator:
   strncpy (genName,"LCG2",sizeof (genName));
   genMagic = LCGMAGIC + GENSUBMASK * 2 + LCGREVISION;  // see RandomDefs.h
   
   // Set the parameters:
   
   a =      69069U;
   c = 1013904223U;
   
   // For single-seed startup:
   initialSeed = 0;
   
   // For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = 0xffffffff;		// 2^32-1
   
   // State size for getState and setState:
   stateSize = sizeof(state_struct_t);
   
   // Actual countMax = 2^32 here, so:
   countMax = (1ull << 32);
   
   // Math is modulo m, so max output value is m-1:
   unsignedMax = 0xffffffff;			// 2^32-1
   
   // We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;
   
   return self;
}

+ create: aZone setStateFromSeed: (unsigned)seed
{
  LCG2gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG2gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];  
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  LCG2gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG2gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
  
}

+ createWithDefaults: aZone
{
  LCG2gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG2gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
  
}

PHASE(Setting)
     
PHASE(Using)

@end

@implementation LCG3gen

PHASE(Creating)

- initState
{
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy (genName, "LCG3", sizeof (genName));
  genMagic = LCGMAGIC + GENSUBMASK * 3 + LCGREVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  a =   1664525U;
   c = 152193325U;
   
   // For single-seed startup:
   initialSeed = 0;
   
   // For multi-seed startup:
   lengthOfSeedVector = 1;
   initialSeeds[0] = 0;
   maxSeedValues[0] = 0xffffffff;		// 2^32-1
   
   // State size for getState and setState:
   stateSize = sizeof(state_struct_t);
   
   // Actual countMax = 2^32 here, so:
   countMax = (1ull << 32);
   
   // Math is modulo m, so max output value is m-1:
   unsignedMax = 0xffffffff;			// 2^32-1
   
   // We pre-compute the divisor for converting to floating point:
   invModMult = (double) unsignedMax;
   invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
   invModMult2 = invModMult * invModMult;
   
   return self;
}

+ create: aZone setStateFromSeed: (unsigned)seed
{
  LCG3gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG3gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];
  
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  LCG3gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG3gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
  
}

+ createWithDefaults: aZone
{
  LCG3gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [LCG3gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];  
}

PHASE(Setting)

PHASE(Using)

@end

