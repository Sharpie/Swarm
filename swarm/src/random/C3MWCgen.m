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
Name:            C3MWCgen.m
Description:     Combined Multiply-With-Carry generator
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

#import <random/C3MWCgen.h>

@implementation C3MWCgen


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



PHASE(Creating)

#import "include.gens.creating.m"

- initState
{
  unsigned i;
  
  // This method is called from createBegin.
  
  // Set the 'personality' of this generator:
  strncpy(genName,"C3MWC",sizeof(genName));
  genMagic = C3MWCMAGIC + GENSUBMASK*1 + C3MWCREVISION;  // see RandomDefs.h
  
  // Set the parameters:
  
  a  = 30903;
  b  = 18000;
  c  = 29013;
  d  = 30345;
  e  = 30903;
  f  = 31083;
  
  // For single-seed startup:
  initialSeed = 0;
  
  // For multi-seed startup:
  lengthOfSeedVector = SEEDS;			// state vector
  for (i = 0; i < lengthOfSeedVector; i++)
    initialSeeds[i] = 0;
  maxSeedValues[0] = ( (a-1) << 16) + 0xffff;
  maxSeedValues[1] = ( (b-1) << 16) + 0xffff;
  maxSeedValues[2] = ( (c-1) << 16) + 0xffff;
  maxSeedValues[3] = ( (d-1) << 16) + 0xffff;
  maxSeedValues[4] = ( (e-1) << 16) + 0xffff;
  maxSeedValues[5] = ( (f-1) << 16) + 0xffff;
  
  // State size for getState and setState:
  stateSize = sizeof(state_struct_t);
  
  // Actual period ~= 2^118 > 2^63, so we set:
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
  C3MWCgen *aGenerator;
  
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
  C3MWCgen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [C3MWCgen createBegin: aZone];
  
// initialize seed dependent part of state:
  
  [aGenerator setStateFromSeed: seed];

  return [aGenerator createEnd];

}

+ create: (id <Zone>)aZone setStateFromSeeds: (unsigned *)seeds
{
  C3MWCgen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [C3MWCgen createBegin: aZone];
  
  // initialize seed dependent part of state:
  
  [aGenerator setStateFromSeeds: seeds];
  
  return [aGenerator createEnd];
  
}

+ createWithDefaults: (id <Zone>)aZone
{
  C3MWCgen *aGenerator;
  
  // Allocate space for the object:
  
  aGenerator = [C3MWCgen createBegin: aZone];
  
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
  // Fill state variables from initialSeeds vector,
  // assuming initialSeeds contains valid seeds.
  // Perform any necessary transformations:
  
   K = initialSeeds[0];
   J = initialSeeds[1];
   I = initialSeeds[2];
   L = initialSeeds[3];
   M = initialSeeds[4];
   N = initialSeeds[5];
   
   // If needed, draw a number of variates
   // to escape rho-sequences:
   // [self runup: 5*r];
   
   // C3MWC are full-cycle generators which do not need runup
   
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
      printf("*** NOTICE %s: generator has created 2^63 variates\n", genName);
      printf("*** out of a cycle of length at least 2^118\n");
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
  // We further concatenate the 32-bit output from 3 component MWC generators.
  
  // With the given multipliers, KJILMN is each < 2^31 at all times
  
  K = a * (K & 65535) + (K >> 16); // K = a*K-lo + K-hi = a*k+carry mod 2^16
  J = b * (J & 65535) + (J >> 16);
  I = c * (I & 65535) + (I >> 16);
  L = d * (L & 65535) + (L >> 16);
  M = e * (M & 65535) + (M >> 16);
  N = f * (N & 65535) + (N >> 16);
  
  // Now combine the components:
  
  /*
    // Marsaglia's code. It fails almost all the tests :-(
    // NOTE: each of the addition operations may overflow 32 bits !
    lastX = ((K+I+M) >> 16) + (J+L+N);	// Marsaglia's code
  */
  
  // This code follows theory, and passes all tests :-)
  // Alternate code: Conjoin the two components
  lastX = (((K+I+M) & 65535) << 16) + ((J+L+N) & 65535);
  
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

  stateBuf->state[0] = K;
  stateBuf->state[1] = J;
  stateBuf->state[2] = I;
  stateBuf->state[3] = L;
  stateBuf->state[4] = M;
  stateBuf->state[5] = N;

  for (i=0; i < lengthOfSeedVector; i++)
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
      stateBuf->stateSize];
  
  
  // Place external data into internal state variables:
  
  antiThetic   = stateBuf->antiThetic;
  singleInitialSeed = stateBuf->singleInitialSeed;
  initialSeed  = stateBuf->initialSeed;
  currentCount = stateBuf->currentCount;
  
  K = stateBuf->state[0];
  J = stateBuf->state[1];
  I = stateBuf->state[2];
  L = stateBuf->state[3];
  M = stateBuf->state[4];
  N = stateBuf->state[5];

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
  
  (void)sprintf (buffer,"            a = %24d\n", a);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            b = %24d\n", b);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            c = %24d\n", c);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            d = %24d\n", d);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            e = %24d\n", e);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            f = %24d\n", f);
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

  for (i = 0; i< lengthOfSeedVector; i++)
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
  
  (void)sprintf (buffer,"        state[00] = %20u\n", K);
  [outStream catC: buffer];
  (void)sprintf (buffer,"        state[01] = %20u\n", J);
  [outStream catC: buffer];
  (void)sprintf (buffer,"        state[02] = %20u\n", I);
  [outStream catC: buffer];
  (void)sprintf (buffer,"        state[03] = %20u\n", L);
  [outStream catC: buffer];
  (void)sprintf (buffer,"        state[04] = %20u\n", M);
  [outStream catC: buffer];
  (void)sprintf (buffer,"        state[05] = %20u\n", N);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            lastX = %20u\n", N);
  [outStream catC: buffer];
  
  [outStream catC: "\n\n"];

  //  nothing returned from a (void) procedure.
}

@end

