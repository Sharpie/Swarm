// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            SWBgen.m
Description:     Subtract-with-Borrow Congruential Generator
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

#import <random/SWBgen.h>

@implementation SWBgen


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
  // Override this method in subclassed generators
  
  [InvalidCombination
    raiseEvent:
      "SWB initState: superclass method invoked - Yell at Sven!\n"];
  
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
  SWBgen *aGenerator;
  
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
  // assuming initialSeeds contains valid seeds.
  // Perform any needed transformations:
  
  for (i = 0; i < r; i++) 
    state[i] = initialSeeds[i];
  
  carry = initialSeeds[r];		// carry bit
  
  // Point to the beginning of the state vector:
  
  index = 0;
  
  // If needed, draw a number of variates
  // to escape rho-sequences:
  [self runup: r];	// A rho sequence is at most r long
  
  // Comment: SWB is NOT a full-cycle generator, and hence it DOES need runup.
  
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
  // printf("SWBi drop: free state\n");
  [[self getZone] free: state];
  // printf("SWBi drop: free initialSeeds\n");
  [[self getZone] free: initialSeeds];
  // printf("SWBi drop: free maxSeedValues\n");
  [[self getZone] free: maxSeedValues];
  // printf("SWBi drop: drop super\n");
  [super drop];
  // printf("SWBi drop: speaking to you from beyond the grave ...\n\n");
}

- (unsigned) getUnsignedSample
{
  unsigned int rth, sth, new;
  
  // Update count of variates delivered:
  // (cycle is > 2^63, so report that counter is exhausted)
  
  currentCount++;
  
  // Give diagnostic warning that we're exceeding the counter:
  
  if (currentCount >= countMax)
    {
      printf("\n*** \n");
      printf("*** NOTICE %s: generator has created 2^63 variates\n", genName);
      printf("*** out of a cycle of length at least 10^200\n");
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
  
  // Calculate the new state value:
  
  new = sth - rth - carry;		// implicitly modulo 2^32
  
  // Update the carry flag: 
  // (Slightly complicated to handle messes like sth==rth && carry==1)
  
  if (carry == 0)
    carry = (sth >= rth) ? 0 : 1;
  else
    carry = (sth > rth)  ? 0 : 1;
  
  // -----------
  
  state[index] = new;
  
  // Move index to next slot in state vector:
  
  index++;
  if (index >= r)
    index = 0;
  
  // If needed, transform state to yield output in [0,unsignedMax]:
  
  if (antiThetic)
    return (unsignedMax - new);
  else
    return new;
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

  for (i = 0; i < r; i++)
    bufArr[i]   = state[i];
  
  bufArr[r] = carry;
  
  for (i = 0; i < lengthOfSeedVector; i++)
    bufArr[lengthOfSeedVector+i] = initialSeeds[i];
  
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
  
  for (i = 0; i < r; i++)
    state[i]   = bufArr[i];
  
  carry = bufArr[r];
  
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

  (void)sprintf (buffer,"            m =               4294967296\n");
  [outStream catC: buffer];
  (void)sprintf (buffer,"            r = %24d\n", r);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            s = %24d\n", s);
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
      (void)sprintf (buffer,"     maxSeeds[%02u] = %20u\n",
                     i, maxSeedValues[i]);
      [outStream catC: buffer];
    }
  
  for (i = 0; i < lengthOfSeedVector; i++)
    {
      (void)sprintf (buffer," initialSeeds[%02u] = %20u\n",
                     i, initialSeeds[i]);
      [outStream catC: buffer];
    }
  
  for (i = 0; i < r; i++)
    {
      (void)sprintf (buffer,"    state[%02u] = %24u\n", i, state[i]);
      [outStream catC: buffer];
    }
  (void)sprintf (buffer,"        carry = %24d\n", carry);
  [outStream catC: buffer];
  
  [outStream catC: "\n\n"];
  
  //  nothing returned from a (void) procedure. 
}

@end



@implementation SWB1gen

PHASE(Creating)

- initState
{
  unsigned i;
  
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy (genName, "SWB1", sizeof (genName));
  genMagic = SWBMAGIC + GENSUBMASK * 1 + SWBREVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  r = 37;
  s = 24;
  
// Make sure generator was initialized with good parameters:
  
  if ((r > s) && (s > 0))
    {
      // all is fine
    }
  else 
    [InvalidCombination
      raiseEvent: 
        "%s: Initialization error: need 0 < s < r\n", genName];
  
  // Now allocate data vectors whose length depends on r:
  // (Dynamic allocation needed only if subclasses require
  // different vector sizes!
  // If not, constant SEEDS is used to size arrays.)
  
   state = [[self getZone] alloc: r * sizeof(int)];
   if (state == NULL)
     [InvalidCombination raiseEvent:
                           "%s: Error allocating state vector!\n", genName];
   memset (state,0,r*sizeof(int));		// zero it out
   
   initialSeeds = [[self getZone] alloc: (r+1)*sizeof(int)];
   if (initialSeeds == NULL)
     [InvalidCombination
       raiseEvent:
         "%s: Error allocating initialSeeds vector!\n", genName];
   memset (initialSeeds, 0, (r + 1) * sizeof (int));		// zero it out
   
   maxSeedValues = [[self getZone] alloc: (r+1)*sizeof(int)];
   if (maxSeedValues == NULL)
     [InvalidCombination
       raiseEvent:
         "%s: Error allocating maxSeedValues vector!\n", genName];
   memset (maxSeedValues, 0, (r + 1) * sizeof (int));		// zero it out
   
   
   // For single-seed startup:
   initialSeed = 0;
   
   // For multi-seed startup:
   
   lengthOfSeedVector = r + 1;			// state vector + carry
   
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   
   for (i = 0; i < r; i++)
     maxSeedValues[i] = 0xfffffffe;		// to avoid degenerate state
   
   maxSeedValues[r] = 0x1;			// carry bit
   
   // State size for getState and setState:
   stateSize = sizeof (state_struct_t) + 2 * lengthOfSeedVector * sizeof(int);
   
   // Actual countMax = 2^1178 - 2^762 ~= 10^354 (64 cycles)
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
  SWB1gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB1gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];  
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  SWB1gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB1gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];

  return [aGenerator createEnd];
}

+ createWithDefaults: aZone
{
  SWB1gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB1gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
  
}

PHASE(Setting)

PHASE(Using)

@end

@implementation SWB2gen

PHASE(Creating)

- initState
{
  unsigned i;
  
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy (genName, "SWB2", sizeof (genName));
  genMagic = SWBMAGIC + GENSUBMASK * 2 + SWBREVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  r = 24;
  s = 19;
  
  // Make sure generator was initialized with good parameters:
  
  if ((r > s) && (s > 0))
    {
      // all is fine
    }
  else
    [InvalidCombination
      raiseEvent: 
        "%s: Initialization error: need 0 < s < r\n", genName];

  // Now allocate data vectors whose length depends on r:
  // (Dynamic allocation needed only if subclasses require
  // different vector sizes!
  // If not, constant SEEDS is used to size arrays.)
  
  state = [[self getZone] alloc: r*sizeof(int)];
   if (state == NULL)
     [InvalidCombination raiseEvent:
                           "%s: Error allocating state vector!\n", genName];
   memset (state, 0, r * sizeof (int));		// zero it out
   
   initialSeeds = [[self getZone] alloc: (r+1)*sizeof(int)];
   if (initialSeeds == NULL)
     [InvalidCombination
       raiseEvent:
         "%s: Error allocating initialSeeds vector!\n", genName];
   memset (initialSeeds, 0, (r + 1) * sizeof (int));		// zero it out
   
   maxSeedValues = [[self getZone] alloc: (r +1 ) * sizeof (int)];
   if (maxSeedValues == NULL)
     [InvalidCombination
       raiseEvent:
         "%s: Error allocating maxSeedValues vector!\n", genName];
   memset (maxSeedValues, 0, (r +1 ) * sizeof (int));		// zero it out
   
   // For single-seed startup:
   initialSeed = 0;
   
   // For multi-seed startup:
   
   lengthOfSeedVector = r + 1;			// state vector + carry
   
   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = 0;
   
   for (i=0; i < r; i++)
     maxSeedValues[i] = 0xfffffffe;		// to avoid degenerate states
   
   maxSeedValues[r] = 0x1;			// carry bit
   
   // State size for getState and setState:
   stateSize = sizeof (state_struct_t) + 2 * lengthOfSeedVector * sizeof(int);
   
   // Actual countMax = (2^759 - 2^599) / 3 ~= 10^228 (1536 cycles)
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
  SWB2gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB2gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  SWB2gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB2gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
}

+ createWithDefaults: aZone
{
  SWB2gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB2gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
}


PHASE(Setting)

PHASE(Using)

@end

@implementation SWB3gen

PHASE(Creating)

- initState
{
  unsigned i;
  
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy (genName, "SWB3", sizeof (genName));
  genMagic = SWBMAGIC + GENSUBMASK * 3 + SWBREVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  r = 21;
  s =  6;
  
  // Make sure generator was initialized with good parameters:
  
  if ((r > s) && (s > 0))
    {
      // all is fine
    }
  else
    [InvalidCombination
      raiseEvent: 
        "%s: Initialization error: need 0 < s < r\n", genName];
  
  // Now allocate data vectors whose length depends on r:
  // (Dynamic allocation needed only if subclasses require
  // different vector sizes!
  // If not, constant SEEDS is used to size arrays.)
  
  state = [[self getZone] alloc: r*sizeof(int)];
  if (state == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating state vector!\n", genName];
  memset (state, 0, r * sizeof (int));		// zero it out
  
  initialSeeds = [[self getZone] alloc: (r + 1) * sizeof(int)];
  if (initialSeeds == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating initialSeeds vector!\n", genName];
  memset(initialSeeds, 0, (r + 1) * sizeof (int));		// zero it out
  
  maxSeedValues = [[self getZone] alloc: (r + 1) * sizeof(int)];
  if (maxSeedValues == NULL)
    [InvalidCombination
      raiseEvent:
        "%s: Error allocating maxSeedValues vector!\n", genName];
  memset (maxSeedValues, 0, (r + 1) * sizeof (int));		// zero it out
  
  
  // For single-seed startup:
  initialSeed = 0;
  
  // For multi-seed startup:
  
  lengthOfSeedVector = r + 1;			// state vector + carry
  
  for (i = 0; i < lengthOfSeedVector; i++)
    initialSeeds[i] = 0;
  
  for (i=0; i < r; i++)
    maxSeedValues[i] = 0xfffffffe;		// to avoid degenerate state
  
  maxSeedValues[r] = 0x1;			// carry bit
  
  // State size for getState and setState:
  stateSize = sizeof (state_struct_t) + 2 * lengthOfSeedVector * sizeof(int);
  
  // Actual countMax = (2^666 - 2^186) / 3 ~= 10^200 (192 cycles)
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
  SWB3gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB3gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];
  
}

+ create: aZone setStateFromSeeds: (unsigned *)seeds
{
  SWB3gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB3gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
}

+ createWithDefaults: aZone
{
  SWB3gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [SWB3gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
}

PHASE(Setting)

PHASE(Using)

@end

