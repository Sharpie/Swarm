// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            ACGgen.h
Description:     Additive Congruential Generator
Library:         random
Original Author: Nelson Minar
Date:            1996-09-09

Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)
Changes:	 Cast the "old style" objective-C objects to Swarm objects,
		 using create phase protocols.

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Change: eliminated 'runup' (see setState).
			--> alters output compared to v. 0.6.
		 Changed the inline LCG to a PMMLCG (for seed generation).
			--> alters output compared to v. 0.6.
		 Standardized the informational comments.
		 Added floating point output (float, double, long double).
		 Revised the 'magic number' numbering scheme.

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

Generator name:	| ACG

Description:	| Additive Congruential Generator

Reference:	| ACG is an often used algorithm, mentioned in Knuth,
		| Numerical Recipes, etc.

		| state vector = f(seed)
Algorithm:	| state_n = (state_(n-s) + state_(n-r)) mod 2^32; r > s
		| output = state_n

		| ACG is in the Lagged Fibonacci class of generators.
		| These generators use a basic algorithm of the form
		| X_n = f(X_(n-r),X_(n-s)) mod m; r>s
		| The function f is typically xor, addition, subtraction,
		| multiplication or subtraction with carry. It uses 
		| simpler math than a basic LCG, but keeps a larger state.

Comments:	| This generator relies on controlled overflow at 32 bits.
		| This requires that unsigned be a 32bit value that follows
		| ANSI C rules.

Implementation:	| The state vector contains the generator's r most recent
		| outputs (X_n .. X_(n-r)). This vector is initialized from
		| an LCG generator using one 32-bit seed value. After 
		| initialization we throw away r*5 numbers to 'prime' the
		| generator (i.e. make sure we have entered a closed cycle.)

Parameters:	| unsigned int r		// long lag
		| unsigned int s		// short lag

State:		| unsigned int state[r]		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,2^32-1] may be returned
		| (i.e. [0,4294967295].)

Valid seeds:	| Using create:setStateFromSeed:  [1,2^32-1]
		| Using create:setStateFromSeeds: [1,2^32-2]

Cycles:		| With properly chosen parameters, these generators
		| have a cycle of length about 2^r.

Output Quality:	| Low order bits are poor, and there are other problems.
		| SWB is a better generator in the same class, and it is
		| only slightly slower.

Reference for	| Knuth, op.cit.
output quality:	| 

ACGgen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       2.702 uS
		|   -getFloatSample, getDoubleSample:         4.482 uS
		|   -getFatDoubleSample, getLongDoubleSample: 8.299 uS

Relative speed:	| Speed 1.369 (time 0.731) relative to MT19937 getUnsignedSample
--------------- | --------------------------------------------------------------
*/

/*
Parameters:	| r = 55
		| s = 24

Reference for	| These values recommended by Knuth.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length ~ 2^r.

Output Quality:	| Low-order bits are poor.

Reference for	| Knuth, op.cit.
output quality:	| 
*/

#import <random.h>
#import <objectbase/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      55

@interface ACGgen: SwarmObject <SimpleRandomGenerator, CREATABLE>

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

   unsigned int r;			// long lag
   unsigned int s;			// short lag

// Fixed value working variables (all functions of m):

   // (none)

// Working variables:

   unsigned index;			// pointer to current state value

// State variables:

   unsigned int state[SEEDS];		// state vector

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

