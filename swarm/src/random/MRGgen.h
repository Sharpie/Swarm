// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            MRGgen.h
Description:     Multiple Recursive [LCG] Generator
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

Generator name:	| MRG

Description:	| Multiple Recursive LCG Generator

Reference:	| Pierre L'Ecuyer, Francois Blouin and Raymond Couture,
		| "A Search for Good Multiple Recursive Random Generators."
		| See <http://www.iro.umontreal.ca/~lecuyer>.

Algorithm:	| x_n = (a_1*x_n-1 + a_2*x_n-2 ... + a_k*x_n-k) mod m

Comments:	| These generators require k multipliers and k past values
		| to be kept. In their paper, the authors investigate MRG's
		| of order k from 1 to 7. They provide several sets of 
		| parameters which they recommend out of a large number that
		| were tested. Generally, the quality of the generators
		| increases with k.

Implementation:	| The authors discovered that most of the benefit of these
		| generators can be had if only a_1 and a_k are nonzero and the
		| rest zero; this yields faster execution since only 2 terms
		| need be calculated. We implement 4 such generators here,
		| all with modulus m close to 2^31.


Parameters:	| unsigned int m		// modulus
		| unsigned int a1, ak		// multipliers
		| unsigned int k		// the order of the generator

State:		| unsigned int x[k]		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,unsignedMax] = [0,m-2].

Valid seeds:	| Using create:setStateFromSeed:  [1, 2^32-1]
		| Using create:setStateFromSeeds: [1, unsignedMax]
		| (unsignedMax differs between MRG5, MRG6, MRG7)

Cycles:		| With properly chosen parameters, these generators
		| have a single cycle of length m^k - 1. 
		| Thus, for m ~= 2^31 we have:
		| k=5 => cycle ~= 2^155, 
		| k=6 => cycle ~= 2^186,
		| k=7 => cycle ~= 2^217.

Output Quality:	| Good.

Reference for	| L'Ecuyer et al, op. cit.
output quality:	| 

MRG5gen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                        9.674 uS
		|   -getFloatSample, getDoubleSample:         11.524 uS
		|   -getFatDoubleSample, getLongDoubleSample: 22.135 uS

Relative speed:	| Speed 0.382 (time 2.616) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 

MRG6gen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       10.449 uS
		|   -getFloatSample, getDoubleSample:         12.337 uS
		|   -getFatDoubleSample, getLongDoubleSample: 27.735 uS

Relative speed:	| Speed 0.354 (time 2.826) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 

MRG7gen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       10.913 uS
		|   -getFloatSample, getDoubleSample:         12.764 uS
		|   -getFatDoubleSample, getLongDoubleSample: 24.627 uS

Relative speed:	| Speed 0.339 (time 2.951) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/

#import <random.h>
#import <objectbase/SwarmObject.h>

// NOTE: 
// we allocate static vectors of length 7 even though
// the 3 generators use lags of 5,6,7 respectively:
#define MAXLAG 7

#define COMPONENTS 1
#define SEEDS      0

@interface MRGgen: SwarmObject <SimpleRandomGenerator>

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
   unsigned int maxSeedValues[MAXLAG];	// if multiple seeds

// Parameters:

   BOOL antiThetic;			// see -getUnsignedSample

// Working variables:

   BOOL singleInitialSeed;		// created with 1 seed ?
   unsigned int initialSeed;		// starting seed used (if 1)
   unsigned int initialSeeds[MAXLAG];	// starting seeds used (if > 1)

// Count of variates generated:

   unsigned long long int currentCount;	// variates generated so far

// -- 

// Generator parameters:

   unsigned int k;			// lag order
   unsigned int m;			// modulus
   unsigned int a1, ak;			// multipliers

// Fixed value working variables:

   unsigned int q1, qk;			// Schrage quotients
   unsigned int r1, rk;			// Schrage remainders

// Working variables:


// State variables:

   unsigned int state[MAXLAG];		// state vector [=x]


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


@interface  MRG5gen: MRGgen <SimpleRandomGenerator, CREATABLE>

{

/*
Parameters:	| unsigned int k = 5		// lag order
		| unsigned int m = 2^31 - 1	// modulus
		| unsigned int a1 = 107374182	// multiplier
		| unsigned int a5 = 104480	// multiplier

Reference for	| L'Ecuyer et al, op. cit., Table 3.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (2^31-1)^5 - 1,
		| i.e. 2^154 < cycle < 2^155.

Output Quality:	| Good. Parameters chosen on the basis of theory 
		| and empirical testing.

Reference for	| L'Ecuyer et al, op. cit.
output quality:	| 
*/

}

CREATING

- initState;
+ create: aZone setStateFromSeed:  (unsigned)   seed;
+ create: aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: aZone;

SETTING

USING

@end


@interface  MRG6gen: MRGgen <SimpleRandomGenerator, CREATABLE>

{

/*
Parameters:	| unsigned int k = 6		// lag order
		| unsigned int m = 2^31 - 1	// modulus
		| unsigned int a1 = 177786	// multiplier
		| unsigned int a6 = 64654	// multiplier

Reference for	| L'Ecuyer et al, op. cit., Table 3.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (2^31-1)^6 - 1,
		| i.e. 2^185 < cycle < 2^186.

Output Quality:	| Good. Parameters chosen on the basis of theory 
		| and empirical testing.

Reference for	| L'Ecuyer et al, op. cit.
output quality:	| 
*/

}

CREATING

- initState;
+ create: aZone setStateFromSeed:  (unsigned)   seed;
+ create: aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: aZone;

SETTING

USING

@end


@interface  MRG7gen: MRGgen <SimpleRandomGenerator, CREATABLE>

{

/*
Parameters:	| unsigned int k = 7		// lag order
		| unsigned int m = 2^31 - 19	// modulus
		| unsigned int a1 = 1071064	// multiplier
		| unsigned int a7 = 2113664	// multiplier

Reference for	| L'Ecuyer et al, op. cit., Table 3.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (2^31-1)^7 - 1,
		| i.e. 2^216 < cycle < 2^217.

Output Quality:	| Good. Parameters chosen on the basis of theory 
		| and empirical testing.

Reference for	| L'Ecuyer et al, op. cit.
output quality:	| 
*/

}

CREATING

- initState;
+ create: aZone setStateFromSeed:  (unsigned)   seed;
+ create: aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: aZone;

SETTING

USING

@end


