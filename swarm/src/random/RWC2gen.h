// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            RWC2gen.h
Description:     2-lag Recursion With Carry generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01 (v. 0.7)

Modified by:	Sven Thommesen
Date:		1998-10-08 (v. 0.8)
Changes:	Code cleanup related to signed/unsigned comparisons.
		Code rearranged for create-phase compatibility.
		Added -reset method.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| RWC2

:escription:	| 2-lag Recursion-With-Carry generator

Reference:	| Article posted to Usenet in 1994 by George Marsaglia,
		| "The Mother of all Random Number Generators."
		| Source found at <ftp://ftp.taygeta.com/pub/c/mother.c>
		| Also document /papers/rwc1.ps on his Diehard CD-ROM,
		| found at <http://www.hku.hk/internet/randomCD.html>.

Algorithm:	| x[n]  = a*(x[n-1]+x[n-2]) + carry % 2^32
		| carry = a*(x[n-1]+x[n-2]) + carry / 2^32

Comments:	| This generator is a 2-lag MWC generator

Implementation:	| This generator is implemented using 64-bit math

Parameters:	| unsigned int a = 1,111,111,464	// multiplier

State:		| unsigned int carry,x1,x2	// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,unsignedMax] = [0,2^32-1].

Valid seeds:	| Using create:setStateFromSeed:  [1, 2^32-1]
		| Using create:setStateFromSeeds: x1 and x2: [1, 2^32-1]
		| The carry can only be [1,a-1].

Cycles:		| This generator is claimed to have a period > 2^92.
		| It may have rho sequences up to length r=2.
		| It may have multiple cycles of the stated period length.

Output Quality:	| Good.

Reference for	| Marsaglia, op. cit.
output quality:	| 

RWC2gen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                        7.445 uS
		|   -getFloatSample, getDoubleSample:          8.845 uS
		|   -getFatDoubleSample, getLongDoubleSample: 17.338 uS

Relative speed:	| Speed 0.497 (time 2.013) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <random.h>
#import <objectbase/SwarmObject.h>

#define COMPONENTS 1
#define SEEDS      3

@interface RWC2gen: SwarmObject <SimpleRandomGenerator, CREATABLE>

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

   unsigned int a;			// multipliers

// Fixed value working variables:


// Working variables:

   unsigned long long int L;
   unsigned int lastX;			// last output

// State variables:

   unsigned int x1, x2;			// state vector
   unsigned int carry;			//   "     "

}


CREATING

// Unpublished (private) methods:
- runup: (unsigned)streak;
- initState;
+ createBegin: aZone;
- createEnd;

// @protocol Simple
+ createWithDefaults: aZone;

// @protocol SingleSeed
+ create: aZone setStateFromSeed: (unsigned)seed;

// @protocol MultiSeed
+ create: aZone setStateFromSeeds: (unsigned *)seeds;

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


