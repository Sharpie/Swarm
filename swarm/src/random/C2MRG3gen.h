// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            C2MRG3gen.h
Description:     Combined Multiple Recursive Generator
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

Generator name:	| C2MRG3

Description:	| Combination of 2 Multiple Recursive LCG Generators

Reference:	| Pierre L'Ecuyer,
		| "Combined Multiple Recursive Random Number Generators."
		| See <http://www.iro.umontreal.ca/~lecuyer>.

		| For a discussion of MRG generators, see MRGgen.h.

Algorithm:	| For each generator:
		| x_n = (a_1*x_n-1 + a_2*x_n-2 ... + a_k*x_n-k) mod m
		| Combining them:
		| x = (x1 - x2) mod m1

Comments:	| Combinations of like generators are shown to have better
		| statistical properties than single generators.

Implementation:	| The components of this generator each has two nonzero 
		| multipliers (and one that's zero). They use different
		| moduli (2^31-1, 2145483479.)

Parameters:	| unsigned int m[2]		// modulus
		| unsigned int a1[2], ak[2]	// multipliers
		| unsigned int k[2]		// the order of the generator

State:		| unsigned int x[k][2]		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,unsignedMax] = [0,m1-2].

Valid seeds:	| This generator requires 3 seeds for each component:
		| within [1,m1-1] for component 1;
		| within [1,m2-1] for component 2.
		| Using create:setStateFromSeed: in [1,2^32-1]

Cycles:		| With properly chosen parameters, this generator
		| has a single cycle of length (m1^3-1)(m2^3-1)/2
		| or 2^184 < cycle < 2^185 (4.9e55).

Output Quality:	| Good.

Reference for	| L'Ecuyer et al, op. cit.
output quality:	| 

C2MRG3gen	| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       13.459 uS
		|   -getFloatSample, getDoubleSample:         15.203 uS
		|   -getFatDoubleSample, getLongDoubleSample: 29.699 uS

Relative speed:	| Speed 0.275 (time 3.640) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/

/*
Parameters:	| unsigned int k = 3		// lag order

		| unsigned int m1 = 2^31 - 1	// modulus
		| unsigned int a12 = 63308	// multiplier
		| unsigned int a13 = -183326	// multiplier

		| unsigned int m2 = 2145483479	// modulus
		| unsigned int a21 = 86098	// multiplier
		| unsigned int a23 = -539608	// multiplier

Reference for	| L'Ecuyer et al, op. cit., Table 3.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length 
		| ((2^31-1)^3 - 1) * (2145483479^3 - 1) / 2 ~= 4.89e55,
		| i.e. 2^184 < cycle < 2^185. 
		| (The paper, in apparent error, claims 
		| that the cycle is close to 2^205.)

Output Quality:	| Good. Parameters chosen on the basis of theory 
		| and empirical testing.

Reference for	| L'Ecuyer et al, op. cit.
output quality:	| 
*/

#import <random.h>
#import <objectbase/SwarmObject.h>

#define MAXLAG     3U
#define COMPONENTS 2U
#define SEEDS      (COMPONENTS * MAXLAG)

@interface C2MRG3gen: SwarmObject <SimpleRandomGenerator, CREATABLE>

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

   unsigned k;			// lag order (same for both components)
   int m1, m2;			// moduli
   int a12, a13, a21, a23;	// multipliers

// Fixed value working variables:

   int q12, q13, q21, q23;	// Schrage quotients
   int r12, r13, r21, r23;	// Schrage remainders

// Working variables:


// State variables:

   int x10, x11, x12, x20, x21, x22;	// state vector

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

