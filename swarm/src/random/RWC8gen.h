// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            RWC8gen.h
Description:     Multiply With Carry generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01 (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| RWC8

Description:	| 8-lag Recursion-With-Carry generator
		| ("The Mother of all RNG's")

Reference:	| Article posted to Usenet in 1994 by George Marsaglia,
		| "The Mother of all Random Number Generators."
		| Source found at <ftp://ftp.taygeta.com/pub/c/mother.c>
		| Also document /papers/mwc1.ps on his Diehard CD-ROM,
		| found at <http://www.hku.hk/internet/randomCD.html>.

Algorithm:	| x = sum(ai*xi) + carry mod 2^16
		| y = sum(bi*yi) + carry mod 2^16
		| return ((x<<16)+y)

Comments:	| This generator is a combination of 2 16-bit 
		| 8-lag Recursion-With-Carry generators. 

Parameters:	| unsigned int a[8] = 
		| { 1941, 1860, 1812, 1776, 1492, 1215, 1066, 12013 }

		| unsigned int b[8] = 
		| { 1111, 2222, 3333, 4444, 5555, 6666, 7777,  9272 }

State:		| unsigned short m1[10], m2[10] // state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,unsignedMax] = [0,2^32-1].

Valid seeds:	| Using create:setStateFromSeed:  [1, 2^32-1]
		| Using create:setStateFromSeeds: 
		|   a total of 18 16-bit seeds are needed;
		|   each xi may be in [1, 2^31-1].
		|   each carry may be in [1, sum(multipliers)-1].

Cycles:		| This generator is claimed to have a period > 2^250.
		| It may have rho sequences up to length r = 8.
		| It may have multiple cycles of the stated period length.

Output Quality:	| Claimed to be good. 

Reference for	| Marsaglia, op. cit.
output quality:	| 

RWC8gen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       14.649 uS
		|   -getFloatSample, getDoubleSample:         16.400 uS
		|   -getFatDoubleSample, getLongDoubleSample: 31.915 uS

Relative speed:	| Speed 0.252 (time 3.961) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <string.h>
#import <random.h>
#import <objectbase/SwarmObject.h>

#define COMPONENTS 2

#define MAXLAG     8
#define SEEDSIZE   (MAXLAG+1)
#define STATESIZE  (MAXLAG+2)
#define SEEDS      ( COMPONENTS * SEEDSIZE )

@interface RWC8gen: SwarmObject

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

   unsigned int a1,a2,a3,a4,a5,a6,a7,a8;	// multipliers
   unsigned int b1,b2,b3,b4,b5,b6,b7,b8;	// multipliers

// Fixed value working variables:

// Working variables:

   unsigned int lastX;				// last output

// State variables:

   unsigned short m1[STATESIZE];		// state vector
   unsigned short m2[STATESIZE];

}

//                                                                      simple.h

// ----- Private methods: -----

-		runup: (unsigned) streak;
-		generateSeeds;
-		setState;

-		initState;
+		createBegin: (id) aZone;
-		setStateFromSeed:  (unsigned)   seed;
-		setStateFromSeeds: (unsigned *) seeds;
-		createEnd;

// NOTE: creation methods are found in the subclasses below

// ----- Single-seed creation: -----

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;

// Limits on seed value supplied (minimum = 0):
- (unsigned)	getMaxSeedValue;

// Return generator starting value:
- (unsigned) 	getInitialSeed;

// ----- Multi-seed creation: -----

+		create: aZone setStateFromSeeds: (unsigned *) seeds;

// Number of seeds required (size of array) (minimum = 1):
- (unsigned) 	lengthOfSeedVector;

// Limits on seed values supplied (minimum = 0):
- (unsigned *)	getMaxSeedValues;

// Return generator starting values:
- (unsigned *) 	getInitialSeeds;

// ----- Other create methods: -----

// Create with a default set of seeds and parameters:
+		createWithDefaults: aZone;

-		setAntithetic: (BOOL) antiT;

// ----- Return values of parameters: -----
- (BOOL)	getAntithetic;

// ----- Return state values: -----

// Return count of variates generated:
- (unsigned long long int)	getCurrentCount;

// ----- Generator output: -----

// The maximum value returned by getUnsignedSample is:
- (unsigned)    getUnsignedMax;

// Return a 'random' integer uniformly distributed over [0,unsignedMax]:
- (unsigned)	getUnsignedSample;

// Return a 'random' floating-point number uniformly distributed in [0.0,1.0):

- (float)       getFloatSample;			// using 1 unsigned
- (double)      getThinDoubleSample;		// using 1 unsigned
- (double)      getDoubleSample;		// using 2 unsigneds
- (long double) getLongDoubleSample;		// using 2 unsigneds

// Warning: use of the last method is not portable between architectures.

// ----- Object state management: -----

- (unsigned)	getStateSize;		
- (void)	putStateInto: (void *) buffer;
- (void)	setStateFrom: (void *) buffer;
- (void)	describe: (id) outStream;
- (char *)      getName;		
- (unsigned)	getMagic;	

@end


