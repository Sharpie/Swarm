// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            TGFSRgen.m
Description:     Twisted GFSR generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01   (v. 0.7)
Modified by:	 Sven Thommesen
Date:		 1998-10-08   (v. 0.8)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/TGFSRgen.h>

@implementation TGFSRgen


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



PHASE(Creating)

#import "include.gens.creating.m"

- initState
{
  // Override this method in each subclassed generator
  
  [InvalidCombination
    raiseEvent:
      "TGFSR initState: superclass method invoked - Yell at Sven!\n"];
  
  return self;
}


+ create: aZone setStateFromSeed: (unsigned)seed
{
  // this is an empty method, to be overridden by the subclasses
  return self;
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
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
  TGFSRgen *aGenerator;
  
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

- generateSeeds
{
  [self generateSeedVector];
  return self;
}

- setState
{
  unsigned i;
  
  // Fill state variables from initialSeeds vector,
  // assuming initialSeeds contains valid seeds:
  
  for (i = 0; i < lengthOfSeedVector; i++) 
    state[i] = initialSeeds[i];		// initialSeeds[] is never zero
  
  // Point to the beginning of the state vector:
  
  index = 0;

  // If needed, draw a number of variates
  // to escape rho-sequences:
  // [self runup: 5*r];
  
  // These generators are full-cycle and do not need runup
  
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

- (void)drop
{
  [[self getZone] free: state];
  [[self getZone] free: initialSeeds];
  [[self getZone] free: maxSeedValues];
  [super drop];
}

- (unsigned)getUnsignedSample
{
  unsigned y;
  
  // Update count of variates delivered:
  // (cycle is > 2^63, so report that counter is exhausted)
  
  currentCount++;

  // Give diagnostic warning that we're exceeding the counter:
  
  if (currentCount >= countMax)
    {
      printf("\n*** \n");
      printf("*** NOTICE %s: generator has created 2^63 variates\n", genName);
      printf("*** out of a cycle of length at least 2^403\n");
      printf("*** (resetting counter)\n");
      printf("*** \n\n");
      currentCount = 0;
    }
  
  // -----------
  
  // Generate the next 'random' value from the state.
  
  if (index == N)
    {
      /* generate N words at one time */
      unsigned kk;
      for (kk = 0; kk <N - M; kk++)
        state[kk] = state[kk+M] ^ (state[kk] >> 1) ^ a[state[kk] % 2];
      for (; kk < N; kk++)
	state[kk] = state[kk+(M-N)] ^ (state[kk] >> 1) ^ a[state[kk] % 2];
      index=0;
    }
  
  y = state[index];
  
  y ^= (y << s) & b;			 /* s and b, magic vectors */
  
  y ^= (y << t) & c;			 /* t and c, magic vectors */
  
  // y &= 0xffffffff; /*  may delete this line if word size = 32 */
  
  y ^= (y >> 16); /* added to the 1994 version by M. Matsumoto, 1996 */

  // Move index to next slot in state vector:
  
  index++;
  
  // -----------
  
  if (antiThetic)
    return (unsignedMax - y);
  else
    return y;
}

- (void)putStateInto: (void *)buffer
{
  state_struct_t * stateBuf;
   unsigned i;
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
   
  for (i = 0; i < lengthOfSeedVector; i++)
    bufArr[i] = state[i];
  
  for (i = 0; i < lengthOfSeedVector; i++)
    bufArr[lengthOfSeedVector + i] = initialSeeds[i];
  
  // nothing returned from a (void) function
}

- (void)setStateFrom: (void *)buffer
{
  state_struct_t * stateBuf;
  unsigned i;
  unsigned int *bufArr;
  
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
  index        = stateBuf->index;
  
  bufArr = &(stateBuf->stateVec);
  
  for (i = 0; i < lengthOfSeedVector; i++)
    state[i] = bufArr[i];
  
  for (i = 0; i < lengthOfSeedVector; i++)
    initialSeeds[i] = bufArr[lengthOfSeedVector+i];
  
  // nothing returned from a (void) function
}


- (void)describe: outStream
{
  char buffer[128];
  unsigned i;
  
  (void)sprintf (buffer,"%s Describe: \n",genName);
  [outStream catC: buffer];
  
  (void)sprintf (buffer,"      genName = %24s\n", genName);
  [outStream catC: buffer];
  (void)sprintf (buffer,"    stateSize = %24u\n", stateSize);
  [outStream catC: buffer];
  (void)sprintf (buffer,"     genMagic = %24u\n", genMagic);
  [outStream catC: buffer];
  
  (void)sprintf (buffer,"            w = %24u\n", w);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            N = %24u\n", N);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            M = %24u\n", M);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            s = %24u\n", s);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            t = %24u\n", t);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            b = %24u\n", b);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            c = %24u\n", c);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            a = %24u\n", a[1]);
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
  (void)sprintf (buffer,"        index = %24u\n", index);
  [outStream catC: buffer];
  (void)sprintf (buffer," currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  for (i = 0; i < lengthOfSeedVector; i++)
    {
      (void)sprintf (buffer,"     maxSeeds[%02u] = %24u\n",
                     i, maxSeedValues[i]);
      [outStream catC: buffer];
    }

  for (i = 0; i < lengthOfSeedVector; i++)
    {
      (void)sprintf (buffer," initialSeeds[%02u] = %24u\n",
                     i, initialSeeds[i]);
      [outStream catC: buffer];
    }
  
  for (i=0; i < lengthOfSeedVector; i++)
    {
      (void)sprintf (buffer,"        state[%02u] = %24u\n", i, state[i]);
      [outStream catC: buffer];
    }
  
  [outStream catC: "\n\n"];
  
  //  nothing returned from a (void) procedure.
}

@end

@implementation TT403gen

PHASE(Creating)

- initState
{
  unsigned i;
  
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy (genName, "TT403", sizeof (genName));
  genMagic = TGFSRMAGIC + GENSUBMASK * 1 + TGFSRREVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  w    = 31;
  N    = 13;
  M    =  2;
  s    =  8;
  t    = 14;
  b    = 0x102d1200;
  c    = 0x66e50000;
  a[1] = 0x6b5eccf6;
  a[0] =  0;
  
  // Make sure generator was initialized with good parameters:
  
  if ((N > M) && (M > 0))
    {
      // all is fine
    }
  else
    [InvalidCombination
      raiseEvent: 
        "%s: Initialization error: need 0 < M < N\n", genName];

  // Now allocate data vectors whose length depends on r:
  // (Dynamic allocation needed only if subclasses require
  // different vector sizes!
  // If not, constant SEEDS is used to size arrays.)
  
  state = [[self getZone] alloc: N*sizeof(int)];
  if (state == NULL)
    [InvalidCombination raiseEvent:
                          "%s: Error allocating state vector!\n", genName];
  memset (state, 0, N * sizeof (int));		// zero it out
  
  initialSeeds = [[self getZone] alloc: N * sizeof (int)];
  if (initialSeeds == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating initialSeeds vector!\n", genName];
  memset (initialSeeds, 0, N * sizeof (int));		// zero it out
  
  maxSeedValues = [[self getZone] alloc: N*sizeof(int)];
  if (maxSeedValues == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating maxSeedValues vector!\n", genName];
  memset (maxSeedValues, 0, N * sizeof (int));		// zero it out
  
  
  // For single-seed startup:
  initialSeed = 0;
  
  // For multi-seed startup:
  
  lengthOfSeedVector = N;			// state vector
  
  for (i = 0; i < lengthOfSeedVector; i++)
    initialSeeds[i] = 0;
  
  for (i = 0; i < lengthOfSeedVector; i++)
    maxSeedValues[i] = 0x7fffffff;		// 2^31-1
  
  // State size for getState and setState:
  stateSize = sizeof (state_struct_t) + 2 * lengthOfSeedVector * sizeof (int);
  
  // Actual countMax = 2^(N*w) - 1 = 2^403
  // Since this vastly exceeds the counter variable, instead we set:
  countMax = (1ull << 63);			// 2^63
  
  // Math is modulo m, so max output value is m-1:
  unsignedMax = 0x7fffffff;	// 2^31-1
  
  // We pre-compute the divisor for converting to floating point:
  invModMult = (double) unsignedMax;
  invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
  invModMult2 = invModMult * invModMult;
  
  return self;
}

+ create: aZone setStateFromSeed: (unsigned)seed
{
  TT403gen * aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT403gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];
  
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  TT403gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT403gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
}

+ createWithDefaults: aZone
{
  TT403gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT403gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
}

PHASE(Setting)
     
PHASE(Using)


@end


@implementation TT775gen

PHASE(Creating)

- initState
{
  unsigned i;
  
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy (genName, "TT775", sizeof (genName));
  genMagic = TGFSRMAGIC + GENSUBMASK * 2 + TGFSRREVISION;  // see RandomDefs.h
   
  // Set the parameters:
  
  w    = 31;
  N    = 25;
  M    =  8;
  s    =  6;
  t    = 14;
  b    = 0x1abd5900;
  c    = 0x776a0000;
  a[1] = 0x6c6cb38c;
  a[0] =  0;
  
  // Make sure generator was initialized with good parameters:
  
  if ((N > M) && (M > 0))
    {
      // all is fine
    }
  else
    [InvalidCombination
      raiseEvent: 
        "%s: Initialization error: need 0 < M < N\n", genName];
  
  // Now allocate data vectors whose length depends on r:
  // (Dynamic allocation needed only if subclasses require
  // different vector sizes!
  // If not, constant SEEDS is used to size arrays.)
  
  state = [[self getZone] alloc: N*sizeof(int)];
  if (state == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating state vector!\n", genName];
  memset (state, 0, N * sizeof (int));		// zero it out
  
  initialSeeds = [[self getZone] alloc: N*sizeof(int)];
  if (initialSeeds == NULL)
    [InvalidCombination 
      raiseEvent:
        "%s: Error allocating initialSeeds vector!\n", genName];
  memset (initialSeeds, 0, N * sizeof (int));		// zero it out
  
  maxSeedValues = [[self getZone] alloc: N * sizeof (int)];
  if (maxSeedValues == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating maxSeedValues vector!\n", genName];
  memset (maxSeedValues, 0, N * sizeof (int));		// zero it out
  
  
  // For single-seed startup:
  initialSeed = 0;
  
  // For multi-seed startup:
  
  lengthOfSeedVector = N;			// state vector
  
  for (i = 0; i < lengthOfSeedVector; i++)
    initialSeeds[i] = 0;
  
  for (i = 0; i < lengthOfSeedVector; i++)
    maxSeedValues[i] = 0x7fffffff;		// 2^31-1
  
  // State size for getState and setState:
  stateSize = sizeof (state_struct_t) + 2 * lengthOfSeedVector * sizeof (int);
  
  // Actual countMax = 2^(N*w) - 1 = 2^403
  // Since this vastly exceeds the counter variable, instead we set:
  countMax = (1ull << 63);			// 2^63
  
  // Math is modulo m, so max output value is m-1:
  unsignedMax = 0x7fffffff;	// 2^31-1
  
  // We pre-compute the divisor for converting to floating point:
  invModMult = (double) unsignedMax;
  invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
  invModMult2 = invModMult * invModMult;
  
  return self;
}

+ create: aZone setStateFromSeed: (unsigned)seed
{
  TT775gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT775gen createBegin: aZone];
  
// initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  TT775gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT775gen createBegin: aZone];
  
  // initialize seed dependent part of state:

  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
}

+ createWithDefaults: aZone
{
  TT775gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT775gen createBegin: aZone];
  
  // initialize seed dependent part of state:

  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
}

PHASE(Setting)

PHASE(Using)

@end


@implementation TT800gen

PHASE(Creating)

- initState
{
  unsigned i;
  
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy (genName, "TT800", sizeof (genName));
  genMagic = TGFSRMAGIC + GENSUBMASK * 3 + TGFSRREVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  w    = 32;
  N    = 25;
  M    =  7;
  s    =  7;
  t    = 15;
  b    = 0x2b5b2500;
  c    = 0xdb8b0000;
  a[1] = 0x8ebfd028;
  a[0] =  0;
  
  // Make sure generator was initialized with good parameters:
  
  if ((N > M) && (M > 0))
    {
      // all is fine
    }
  else
    {
      [InvalidCombination
        raiseEvent: 
          "%s: Initialization error: need 0 < M < N\n", genName];
    }
  
  // Now allocate data vectors whose length depends on r:
  // (Dynamic allocation needed only if subclasses require
  // different vector sizes!
  // If not, constant SEEDS is used to size arrays.)
  
  state = [[self getZone] alloc: N*sizeof(int)];
  if (state == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating state vector!\n", genName];
  memset (state, 0, N * sizeof (int));		// zero it out
  
  initialSeeds = [[self getZone] alloc: N*sizeof(int)];
  if (initialSeeds == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating initialSeeds vector!\n", genName];
  memset (initialSeeds, 0, N * sizeof (int));		// zero it out
  
  maxSeedValues = [[self getZone] alloc: N*sizeof(int)];
  if (maxSeedValues == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating maxSeedValues vector!\n", genName];
  memset (maxSeedValues, 0, N * sizeof (int));		// zero it out
  
  // For single-seed startup:
  initialSeed = 0;
  
  // For multi-seed startup:
  
   lengthOfSeedVector = N;			// state vector
   
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   
   for (i=0; i < lengthOfSeedVector; i++)
     maxSeedValues[i] = 0xffffffff;		// 2^32-1
   
// State size for getState and setState:
   stateSize = sizeof (state_struct_t) + 2 * lengthOfSeedVector * sizeof(int);
   
   // Actual countMax = 2^(N*w) - 1 = 2^403
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

+ create: aZone setStateFromSeed: (unsigned)seed
{
  TT800gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT800gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];
  
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  TT800gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT800gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
}

+ createWithDefaults: aZone
{
  TT800gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [TT800gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
}

PHASE(Setting)
     
PHASE(Using)

@end
