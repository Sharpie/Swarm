// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            LCGgen.h
Description:     Linear Congruential Generator
Library:         random
Original Author: Nelson Minar
Date:            1996-09-09

Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)
Changes:	 Cast the "old style" objective-C objects to Swarm objects,
		 using create phase protocols.

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Standardized the informational comments.
		 Sub-classed LCG1,LCG2,LCG3.
		 Added floating point output (float, double, long double).
		 Revised the 'magic number' numbering scheme.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| LCG

Description:	| Linear Congruential Generator

Reference:	| This is a classic generator.
		| See for example Donald Knuth, 
		| The Art of Computer Programming,
		| vol. II, "Seminumerical Algorithms."

		| state = seed
Algorithm:	| newstate = ((a * oldstate) + c ) mod 2^32
		| output = state

Comments:	| This generator relies on controlled overflow at 32 bits.
		| This requires that unsigned be a 32bit value that follows
		| ANSI C rules.
		| Knuth claims that the adder c does not matter much, as
		| long as it has no factors in common with the modulus 2^32.


Parameters:	| unsigned int c	// adder
		| unsigned int a	// multiplier

State:		| unsigned int state	// a "state vector" of length 1

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,2^32-1] may be returned
		| (i.e. [0,4294967295].)

Valid seeds:	| Using create:setStateFromSeed:  [1,2^32-1]
		| Using create:setStateFromSeeds: [1,2^32-1]

Cycles:		| With properly chosen parameters, these generators
		| have a single full cycle of length (2^32).

Output Quality:	| Low order bits are poor.

Reference for	| Knuth, op.cit.
output quality:	| 

LCGgen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       2.564 uS
		|   -getFloatSample, getDoubleSample:         4.295 uS
		|   -getFatDoubleSample, getLongDoubleSample: 7.734 uS

Relative speed:	| Speed 1.442 (time 0.693) relative to MT19937 getUnsignedSample
--------------- | --------------------------------------------------------------
*/

#import <string.h>
#import <random.h>
#import <objectbase/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      1

@interface LCGgen: SwarmObject

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

   unsigned int a;		// multiplier
   unsigned int c;		// adder

// Fixed value working variables:

   // (none)

// Working variables:

   // (none)

// State variables:

   unsigned int state;			// (state vector of length 1)

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

// NOTE: create methods are in the subclass below

// ----- Single-seed creation: -----

// + 		create: aZone setStateFromSeed:  (unsigned)   seed;

// Limits on seed value supplied (minimum = 0):
- (unsigned)	getMaxSeedValue;

// Return generator starting value:
- (unsigned) 	getInitialSeed;

// ----- Multi-seed creation: -----

// +		create: aZone setStateFromSeeds: (unsigned *) seeds;

// Number of seeds required (size of array) (minimum = 1):
- (unsigned) 	lengthOfSeedVector;

// Limits on seed values supplied (minimum = 0):
- (unsigned *)	getMaxSeedValues;

// Return generator starting values:
- (unsigned *) 	getInitialSeeds;

// ----- Other create methods: -----

// Create with a default set of seeds and parameters:
// +		createWithDefaults: aZone;

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


@interface LCG1gen: LCGgen

{

/*
Parameters:	| a =     1,664,525
		| c = 1,013,904,223

Reference for	| The multiplier is from Knuth vol II (line 26 of table).
parameters:	| Adder is from Numerical Recipes, in turn from H.W.Lewis

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length m.

Output Quality:	| Low-order bits are poor.

Reference for	| Knuth, op.cit. and Numerical Recipes.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

@interface LCG2gen: LCGgen

{

/*
Parameters:	| a =        69,069
		| c = 1,013,904,223

Reference for	| The multiplier is from Knuth vol II (line 25 of table).
parameters:	| Adder is from Numerical Recipes, in turn from H.W.Lewis

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length m.

Output Quality:	| Low-order bits are poor.

Reference for	| Knuth, op.cit. and Numerical Recipes.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

@interface LCG3gen: LCGgen

{

/*
Parameters:	| a =   1,664,525
		| c = 152,193,325

Reference for	| The multiplier is from Knuth vol II (line 26 of table).
parameters:	| Adder is from Numerical Recipes, in turn from H.W.Lewis

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length m.

Output Quality:	| Low-order bits are poor.

Reference for	| Knuth, op.cit. and Numerical Recipes.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

