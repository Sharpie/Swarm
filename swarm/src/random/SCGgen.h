// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            SCGgen.h
Description:     Subtractive Congruential Generator
Library:         random
Original Author: Nelson Minar
Date:            1996-09-09

Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)
Changes:	 Cast the "old style" objective-C objects to Swarm objects,
		 using create phase protocols.

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Fixed bug: starting seeds were not being generated properly.
			--> alters output compared to v. 0.6.
		 Change: eliminated 'runup' (see setState).
			--> alters output compared to v. 0.6.
		 Changed the inline LCG to a PMMLCG (for seed generation).
			--> alters output compared to v. 0.6.
		 Standardized the informational comments.
		 Added floating point output (float, double, long double).
		 Revised the 'magic number' numbering scheme.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| SCG

Description:	| Subtractive Congruential Generator

Reference:	| SCG is an often used algorithm, mentioned in Knuth,
		| Numerical Recipes, etc.

		| state vector = f(seed)
Algorithm:	| state_n = (state_(n-r) - state_(n-s)) mod 10^9; r > s
		| output = state_n

		| SCG is in the Lagged Fibonacci class of generators.
		| These generators use a basic algorithm of the form
		| X_n = f(X_(n-r),X_(n-s)) mod m; r>s
		| The function f is typically xor, addition, subtraction,
		| multiplication or subtraction with carry. It uses 
		| simpler math than a basic LCG, but keeps a larger state.

Implementation:	| The state vector contains the generator's r most recent
		| outputs (X_n .. X_(n-r)). This vector is initialized from
		| an LCG generator using one 32-bit seed value. After 
		| initialization we throw away r*5 numbers to 'prime' the
		| generator (i.e. make sure we have entered a closed cycle.)

Parameters:	| unsigned int r		// long lag
		| unsigned int s		// short lag
		| unsigned int m		// modulus

State:		| unsigned int state[r]		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,10^9-1] may be returned
		| (i.e. [0,999999999].)

Valid seeds:	| Using create:setStateFromSeed:  [1,2^32-1]
		| Using create:setStateFromSeeds: [1,999999998]

Cycles:		| With properly chosen parameters, these generators
		| have a cycle of length about 2^r.

Output Quality:	| Low order bits are poor, and there are other problems.
		| SWB is a better generator in the same class, and it is
		| only slightly slower.

Reference for	| Knuth, op.cit.
output quality:	| 

SCGgen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       3.328 uS
		|   -getFloatSample, getDoubleSample:         5.058 uS
		|   -getFatDoubleSample, getLongDoubleSample: 9.274 uS

Relative speed:	| Speed 1.111 (time 0.900) relative to MT19937 getUnsignedSample
--------------- | --------------------------------------------------------------
*/

/*
Parameters:	| r = 55
		| s = 24
		| m = 10^9

Reference for	|
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length ~ 2^r.

Output Quality:	| Low-order bits are poor.

Reference for	| Knuth, op.cit.
output quality:	| 
*/

#import <string.h>
#import <random.h>
#import <objectbase/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      55

@interface SCGgen: SwarmObject

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
   unsigned int m;			// modulus

// Fixed value working variables (all functions of m):

   // (none)

// Working variables:

   unsigned index;			// pointer to current state value

// State variables:

   unsigned int state[SEEDS];		// state vector

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

