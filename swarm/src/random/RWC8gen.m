// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:            RWC8gen.m
Description:     8-lag Recursion-With-Carry generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01   (v. 0.7)
Changed by:	 Sven Thommesen
Date:		 1998-10-08   (v. 0.8)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>		// for outStream in -describe

#import <random/RWC8gen.h>

@implementation RWC8gen


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
  unsigned short initialSeeds[SEEDS];
  unsigned short m1[STATESIZE];
  unsigned short m2[STATESIZE];
} state_struct_t;



PHASE(Creating)

#import "include.gens.creating.m"

- initState
{
  unsigned i;
  
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy (genName, "RWC8", sizeof (genName));
  genMagic = RWC8MAGIC + GENSUBMASK * 1 + RWC8REVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  a1 = 1941;
  a2 = 1860;
  a3 = 1812;
  a4 = 1776;
  a5 = 1492;
  a6 = 1215;
  a7 = 1066;
  a8 = 12013;
  
  b1 = 1111;
  b2 = 2222;
  b3 = 3333;
  b4 = 4444;
  b5 = 5555;
  b6 = 6666;
  b7 = 7777;
  b8 = 9272;
  
  // For single-seed startup:
  initialSeed = 0;
  
  // For multi-seed startup:
  
  lengthOfSeedVector = SEEDS;			// state vector
  
  for (i = 0; i < lengthOfSeedVector; i++)
    initialSeeds[i] = 0;
  
  for (i=0; i < lengthOfSeedVector; i++)
    maxSeedValues[i] = 0xffff;			// any 16-bit value ok
  
  // Limit the initial carry values:
  maxSeedValues[0] = a1+a2+a3+a4+a5+a6+a7+a8-1;
  maxSeedValues[SEEDSIZE] = b1+b2+b3+b4+b5+b6+b7+b8-1;
  
  // State size for getState and setState:
  stateSize = sizeof(state_struct_t);
  
  // Actual period ~= 2^250, so we set:
  countMax = (1ull << 63);
  
  // Math is modulo m, so max output value is m-1:
  unsignedMax = 0xffffffff;
  
  // We pre-compute the divisor for converting to floating point:
  invModMult = (double) unsignedMax;
  invModMult = 1.0 / (invModMult + 1.0);	// to avoid returning 1.0
  invModMult2 = invModMult * invModMult;
  
  return self;
}

+ createBegin: (id <Zone>)aZone
{
  RWC8gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [super createBegin: aZone];
  
  // initialize instance variables:
  
  aGenerator->currentCount = TESTCASE;
  
  // initialize fixed parts of state:
  
  [aGenerator initState];	// must be called before setStateFromSeed
  
  return aGenerator;
}


+ create: (id <Zone>)aZone setStateFromSeed: (unsigned)seed
{
  RWC8gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [RWC8gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];
  
  return [aGenerator createEnd];
  
}

+ create: (id <Zone>)aZone setStateFromSeeds: (unsigned *)seeds
{
  RWC8gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [RWC8gen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
  
}

+ createWithDefaults: (id <Zone>)aZone
{
  RWC8gen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [RWC8gen createBegin: aZone];
  
// initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: STARTSEED];		// See RandomDefs.h
  
  return [aGenerator createEnd];
  
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
  // Perform any necessary transformations:
  
  for (i = 0; i < SEEDSIZE; i++)
    // SEEDSIZE = lengthOfSeedVector/2
    m1[i] = initialSeeds[i];
  
  for (i=0; i < SEEDSIZE; i++)
    m2[i] = initialSeeds[SEEDSIZE+i];
  
  // Note: the last elements in vectors m1 and m2 were not filled
  
  // If needed, draw a number of variates
  // to escape rho-sequences:
  [self runup: MAXLAG];
  
  // (RWC generators may have rho sequences of at most length r)
  
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
  // unsigned int new;
  unsigned long number1, number2;
  
  // Update count of variates delivered:
  // (cycle is > 2^63, so report that counter is exhausted)
  
  currentCount++;
  
  // Give diagnostic warning that we're exceeding the counter:
  
  if (currentCount >= countMax)
    {
      printf("\n*** \n");
      printf("*** NOTICE %s: generator has created 2^63 variates\n", genName);
      printf("*** out of a cycle of length at least 2^250\n");
      printf("*** (resetting counter)\n");
      printf("*** \n\n");
      currentCount = 0;
    }
  
  // -----------
  
  // Generate the next 'random' value from the state.
  
  /* Move elements 1 - 8 down to 2 - 9 */
  memmove (m1 + 2, m1 + 1,8 * sizeof (short));
  memmove (m2 + 2, m2 + 1,8 * sizeof (short));
  
  /* Put the carry values in numberi */
  number1 = m1[0];
  number2 = m2[0];
  
  /* Form the linear combinations */
  number1 += (a1 * m1[2] + a2 * m1[3] + a3 * m1[4] + a4 * m1[5] +
              a5 * m1[6] + a6 * m1[7] + a7 * m1[8] + a8 * m1[9]);
  
  number2 += (b1 * m2[2] + b2 * m2[3] + b3 * m2[4] + b4 * m2[5] +
              b5 * m2[6] + b6 * m2[7] + b7 * m2[8] + b8 * m2[9]);
  
  /* Save the high bits of numberi as the new carry */
  m1[0] = (number1 >> 16);
  m2[0] = (number2 >> 16);
  
  /* Put the low bits of numberi into mi[1] */
  m1[1] = (number1 & 65535);
  m2[1] = (number2 & 65535);
  
  /* Combine the two 16 bit random numbers into one 32 bit */
  lastX = ( ((long) m1[1]) << 16) + (long) m2[1];
  
  // -----------
  
  if (antiThetic)
    return (unsignedMax - lastX);
  else
    return lastX;
}

- (void)putStateInto: (void *)buffer
{
  state_struct_t * stateBuf;
  unsigned i;
  
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
  
  for (i = 0; i < STATESIZE; i++)
    {
      stateBuf->m1[i] = m1[i];
      stateBuf->m2[i] = m2[i];
    }
  
  for (i = 0; i < lengthOfSeedVector; i++)
    stateBuf->initialSeeds[i] = initialSeeds[i];
  
  // nothing returned from a (void) function
}

- (void)setStateFrom: (void *)buffer
{
  state_struct_t * stateBuf;
  unsigned i;
  
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
      stateBuf->stateSize ];
  
  // Place external data into internal state variables:

  antiThetic   = stateBuf->antiThetic;
  singleInitialSeed = stateBuf->singleInitialSeed;
  initialSeed  = stateBuf->initialSeed;
  currentCount = stateBuf->currentCount;

  for (i = 0; i < STATESIZE; i++)
    {
      m1[i] = stateBuf->m1[i];
      m2[i] = stateBuf->m2[i];
    }
  
  for (i = 0; i < lengthOfSeedVector; i++)
    initialSeeds[i] = stateBuf->initialSeeds[i];
  
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

  (void)sprintf (buffer,"   a[%d] b[%d] = %12u %12u\n", 1, 1, a1, b1);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   a[%d] b[%d] = %12u %12u\n", 2, 2, a2, b2);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   a[%d] b[%d] = %12u %12u\n", 3, 3, a3, b3);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   a[%d] b[%d] = %12u %12u\n", 4, 4, a4, b4);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   a[%d] b[%d] = %12u %12u\n", 5, 5, a5, b5);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   a[%d] b[%d] = %12u %12u\n", 6, 6, a6, b6);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   a[%d] b[%d] = %12u %12u\n", 7, 7, a7, b7);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   a[%d] b[%d] = %12u %12u\n", 8, 8, a8, b8);
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
  (void)sprintf (buffer," currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  for (i=0; i<lengthOfSeedVector; i++)
    {
      (void)sprintf (buffer,"     maxSeeds[%02u] = %20u\n",
                     i, maxSeedValues[i]);
      [outStream catC: buffer];
    }
  for (i=0; i<lengthOfSeedVector; i++)
    {
      (void)sprintf (buffer," initialSeeds[%02u] = %20u\n",
                     i, initialSeeds[i]);
      [outStream catC: buffer];
    }
  
  for (i=0; i < STATESIZE; i++)
    {
      (void)sprintf (buffer," m1[%u] m2[%u] = %12u %12u\n",
                     i, i, m1[i], m2[i]);
      [outStream catC: buffer];
    }
  
  (void)sprintf (buffer,"        lastX = %24u\n", lastX);
  [outStream catC: buffer];
  
  [outStream catC: "\n\n"];
  
  //  nothing returned from a (void) procedure.
}

@end
