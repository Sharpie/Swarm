// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            MT19937gen.h
Description:     'Mersenne Twister' Twisted GFSR generator
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

Generator name:	| MT19937 

Description:	| Twisted GFSR generator

Reference:	| Makoto Matsumoto and ??? Nishimura,
		| code posted to the pLab web site, xx/xx/97.
		| Site: <http://random.mat.sbg.ac.at>
		| Code: <ftp://random.mat.sbg.ac.at/pub/data/mt19937b-int.c>
		| Article reportedly submitted to ACM TOMACS.

		| 
Algorithm:	| 
		| 

Comments:	| 

Implementation:	| 

Parameters:	| unsigned int N		// size of state array
		| unsigned int M		// lag
		| unsigned int a		// "Magic numbers"
		| unsigned int b,c		//    "     "
		| unsigned int u,s,t,l		//    "     "

State:		| unsigned int state[N]		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,2^32] may be returned

Valid seeds:	| Using create:setStateFromSeed:  in [1,2^32-1]
		| Using create:setStateFromSeeds: in [1,2^32-1].

Cycles:		| This generator has a single cycle of length 2^19937-1.

Output Quality:	| Excellent equidistribution properties in all dimensions
		| up to 623 are claimed.

Reference for	| Matsumoto and Nishimura, op. cit.
output quality:	| See also pLab Report #07 on the pLab web site.

MT19937gen	| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                        3.698 uS
		|   -getFloatSample, getDoubleSample:          5.502 uS
		|   -getFatDoubleSample, getLongDoubleSample: 10.159 uS

Relative speed:	| Speed 1.0 (time 1.0) relative to itself (it's the benchmark)
--------------- . ------------------------------------------------------------- 
*/

/*
Parameters:	| unsigned int w=32		// word length
		| unsigned int N=624		// size of state array
		| unsigned int M=397		// lag
		| unsigned int s=7		// 'magic vector'
		| unsigned int t=15		//    "     "
		| unsigned int u=11		//    "     "
		| unsigned int l=18		//    "     "
		| unsigned int b=0x102d1200	//    "     "
		| unsigned int c=0x66e50000	//    "     "
		| unsigned int a=0x6b5eccf6	//    "     "

Reference for	| Matsumoto and Nishimura, op. cit.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length 2^19937 -1,
		| or 10^6001 !!! (The longest period yet.)

Output Quality:	| Good. Parameters chosen on the basis of theory 
		| and empirical testing.

Reference for	| Matsumoto and Nishimura, op. cit.
output quality:	| 
*/

#import <random.h>
#import <objectbase/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      624

@interface MT19937gen: SwarmObject

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

   unsigned int N;				// size of state array
   unsigned int M;				// lag
   unsigned int a[2];				// "Magic numbers"
   unsigned int TEMPERING_MASK_B;		//    "     "
   unsigned int TEMPERING_MASK_C;		//    "     "
   unsigned int UPPER_MASK;
   unsigned int LOWER_MASK;

// Shift definitions:

/*
#define TEMPERING_SHIFT_U(y)   (y >> 11)
#define TEMPERING_SHIFT_S(y)   (y <<  7)
#define TEMPERING_SHIFT_T(y)   (y << 15)
#define TEMPERING_SHIFT_L(y)   (y >> 18)
*/
   unsigned int TEMPERING_SHIFT_U;
   unsigned int TEMPERING_SHIFT_S;
   unsigned int TEMPERING_SHIFT_T;
   unsigned int TEMPERING_SHIFT_L;

// Fixed value working variables:


// Working variables:

   unsigned int index;		// pointer to current state value [=k]

// State variables:

   unsigned int state[SEEDS];	// state vector "ptgfsr[]"
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
- (const char *)getName;		
- (unsigned)	getMagic;	

@end
