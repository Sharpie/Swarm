// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

/*
Name:            C3MWCgen.h
Description:     Combined Multiply With Carry generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01 (v. 0.7)

Modified by:	 Sven Thommesen
Date:		 1998-10-08 (v. 0.8)
Changes:	 Code cleanup related to signed/unsigned comparisons.
		 Code rearranged for create-phase compatibility.
		 Added -reset method.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| C3MWC

Description:	| Combined Multiply-With-Carry generator

Reference:	| Article posted to Usenet in 1994 by George Marsaglia,
		| "The Mother of all Random Number Generators."
		| Source found at <ftp://ftp.taygeta.com/pub/c/mother.c>
		| Also document /papers/mwc1.ps on his Diehard CD-ROM,
		| found at <http://www.hku.hk/internet/randomCD.html>.

Algorithm:	| K = a * (K & 65535) + (K>>16);
		| J = b * (J & 65535) + (J>>16);
		| I = c * (I & 65535) + (I>>16);
		| L = d * (L & 65535) + (L>>16);
		| M = e * (M & 65535) + (M>>16);
		| N = f * (N & 65535) + (N>>16);
		| return ((K+I+M)>>16) + (J+L+N);

Comments:	| This generator is a combination of 3 MWC generators,
		| each of which is a combination of 2 16-bit 
		| Multiply-With-Carry generators. 

Parameters:	| unsigned int a = 30903	// multiplier
		| unsigned int b = 18000
		| unsigned int c = 29013
		| unsigned int d = 30345
		| unsigned int e = 30903
		| unsigned int f = 31083

State:		| unsigned int K,J,I,L,M,N		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,unsignedMax] = [0,2^32-1].

Valid seeds:	| Using create:setStateFromSeed:  [1, 2^32-1]
		| Using create:setStateFromSeeds: each component may have 
		| a 'state' <= 0xffff and a carry <= (multiplier-1), so that 
		| for the first component the largest seed allowed is 
		| (a-1)<<16 + 65535.

Cycles:		| This generator is claimed to be strictly periodic, with a 
		| period > 2^118. There may be more than one cycle.

Output Quality:	| Good.

Reference for	| Marsaglia, op. cit.
output quality:	| 

C3MWCgen	| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                        6.437 uS
		|   -getFloatSample, getDoubleSample:          8.200 uS
		|   -getFatDoubleSample, getLongDoubleSample: 15.562 uS

Relative speed:	| Speed 0.574 (time 1.741) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <random.h>
#import <objectbase/SwarmObject.h>

#define COMPONENTS 3
#define SEEDS      6

@interface C3MWCgen: SwarmObject <SimpleRandomGenerator, CREATABLE>

{

// Generator personality:

   unsigned int stateSize;
   unsigned int genMagic;
   char genName[GENNAMESIZE];

// Characteristic constants:

   unsigned long long int countMax;	// largest value for currentCount
   unsigned int unsignedMax;		// largest integral value returned
   double invModMult;			// (1.0/unsignedMax+1)
   double invModMult2;			// (1.0/unsignedMax+1)^2

   unsigned int lengthOfSeedVector;	// if multiple seeds
   unsigned int maxSeedValues[SEEDS];	// if multiple seeds

// Parameters:

   BOOL antiThetic;			// see -getUnsignedSample

// Working variables:

   BOOL singleInitialSeed;		// created with 1 seed ?
   unsigned int initialSeed;		// starting seed used (if 1)
   unsigned int initialSeeds[SEEDS];	// starting seeds used (if > 1)

// Count of variates generated:

   unsigned long long int currentCount;	// variates generated so far

// -- 

// Generator parameters:

   unsigned int a,b,c,d,e,f;		// multipliers

// Fixed value working variables:


// Working variables:

   unsigned int lastX;			// most recent output

// State variables:

   unsigned int K, J, I, L, M, N;	// state vector 


}


CREATING

// Unpublished (private) methods:
- runup: (unsigned)streak;
- initState;
+ createBegin: (id <Zone>)aZone;
- createEnd;

// @protocol Simple
+ createWithDefaults: (id <Zone>)aZone;

// @protocol SingleSeed
+ create: (id <Zone>)aZone setStateFromSeed: (unsigned)seed;

// @protocol MultiSeed
+ create: (id <Zone>)aZone setStateFromSeeds: (unsigned *)seeds;

SETTING

// Unpublished (private) methods:
- setState;
- generateSeeds;
- generateSeedVector;

// @protocol Simple
- setAntithetic: (BOOL) antiT;

// @protocol SingleSeed
- setStateFromSeed: (unsigned)seed;

// @protocol MultiSeed
- setStateFromSeeds: (unsigned *)seeds;

USING

// Unpublished (private) methods:

// @protocol InternalState
- (unsigned)getStateSize;		// size of buffer needed
- (void)putStateInto: (void *)buffer;	// save state data for later use
- (void)setStateFrom: (void *)buffer;	// set state from saved data
- (void)describe: outStream;	        // prints ascii data to stream
- (const char *)getName;		// returns name of object
- (unsigned)getMagic;			// object's 'magic number'

// @protocol SimpleOut
- (unsigned)getUnsignedMax;

- (unsigned)getUnsignedSample;
- (float)getFloatSample;
- (double)getThinDoubleSample;
- (double)getDoubleSample;
- (long double)getLongDoubleSample;

// @protocol Simple
- (BOOL)getAntithetic;
- (unsigned long long int)getCurrentCount;
- reset;

// @protocol SingleSeed
- (unsigned)getMaxSeedValue;		// min is 1
- (unsigned)getInitialSeed;

// @protocol MultiSeed
- (unsigned)lengthOfSeedVector;
- (unsigned *)getMaxSeedValues;		// min is 1
- (unsigned *)getInitialSeeds;

@end


