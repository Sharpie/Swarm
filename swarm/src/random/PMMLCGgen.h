// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.


/*
Name:            PMMLCGgen.h
Description:     Prime Modulus Multiplicative Linear Congruential Generator
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
		 Sub-classed PMMLCG1,PMMLCG2,PMMLCG3.
		 Added PMMLCG4, PMMLCG5, PMMLCG6, PMMLCG7, PMMLCG8, PMMLCG9
		 from L'Ecuyer.
		 Added floating point output (float, double, long double).
		 Revised the 'magic number' numbering scheme.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| PMMLCG

Description:	| Prime Modulus Multiplicative Linear Congruential Generator

Reference:	| Stephen K. Park and Keith W. Miller 
		| (CACM 31:10, October 1988, 1192-1201),
		| "Random Number Generators: Good Ones Are Hard To Find."
		| The authors refer to this generator as a "minimally
		| acceptable" or "minimally sufficient" generator.

		| state = seed
Algorithm:	| newstate = (a * oldstate) mod m
		| output = state-1

Comments:	| The modulus m must be a prime for this class of generator
		| (as opposed to the more common power of two modulus).
		| For these generators we use m=2^31-1 (2,147,483,647).
		| The choice of a good multiplier a is crucial.

Parameters:	| unsigned int m	// modulus
		| unsigned int a	// multiplier

State:		| unsigned int state	// a "state vector" of length 1

Output types:	| unsigned int (we return the current state-1).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| The basic algorithm yields output in the range [1,m-1].
		| We subtract 1 to return output in the range [0,m-2]
		| (i.e. [0,2147483645].)

Valid seeds:	| create:setStateFromSeed:  [1, 2^32-1]
		| create:setStateFromSeeds: [1, m-1]

Cycles:		| With properly chosen parameters, these generators
		| have a single full cycle of length (m-1).
		| (State=0 is an absorbing state which will not normally
		| occur; we avoid setting the starting state to 0.)

Output Quality:	| All bits should be safe.
		| (I.e. low order bits should not have small cycles.)
		| With m prime, this generator is better than standard LCG.

Reference for	| Park and Miller, op. cit.
output quality:	| 

Implementation  | The code for -getUnsignedMax uses a trick devised by
comment:        | Schrage to avoid 32-bit overflow when calculating the
		| next state (i.e. using q=m/a and r=m%a as auxiliary
		| values). Using 64-bit math (long long integers) is more
		| straightforward. Unfortunagely, on x86 at least, there
		| is a 35% speed penalty for doing so. The output, however,
		| is identical. YMMV.

PMMLCGgen	| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                        4.715 uS
		|   -getFloatSample, getDoubleSample:          6.500 uS
		|   -getFatDoubleSample, getLongDoubleSample: 12.152 uS

Relative speed:	| Speed 0.792 (time 1.263) relative to MT19937 getUnsignedSample
--------------- | --------------------------------------------------------------
*/

#import <string.h>
#import <random.h>
#import <objectbase/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      1

@interface PMMLCGgen: SwarmObject

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

   unsigned int a;			// multiplier
   unsigned int m;			// modulus

// Fixed value working variables (all functions of m):

   unsigned int q;			// quotient m/a
   unsigned int r;			// remainder m%a

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

// NOTE: create methods are in the subclasses below

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
- (char *)      getName;		
- (unsigned)	getMagic;	

@end


@interface PMMLCG1gen: PMMLCGgen

{


/*
Parameters:	| a =        16,807
		| m = 2,147,483,647 (2^31-1)

Reference for	| Park and Miller, op. cit.
parameters:	| 

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.
		| These parameters have been extensively tested by the authors.

Reference for	| Park and Miller, op. cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end


@interface PMMLCG2gen: PMMLCGgen

{ 

/*
Parameters:	| a =        48,271
		| m = 2,147,483,647 (2^31-1)

Reference for	| Park and Miller, op. cit.
parameters:	| 

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.
		| These parameters are not as well tested as PMMLCG1,
		| but there's reason to believe they may be better.

Reference for	| Park and Miller, op. cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end


@interface PMMLCG3gen: PMMLCGgen

{ 

/*
Parameters:	| a =        69,621
		| m = 2,147,483,647 (2^31-1)

Reference for	| Park and Miller, op. cit.
choice of 	| 
parameters:	| 

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.
		| These parameters are not as well tested as PMMLCG1,
		| but there's reason to believe they may be better.

Reference for	| Park and Miller, op. cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

@interface PMMLCG4gen: PMMLCGgen

{ 

/*
Parameters:	| a =        45,991
		| m = 2,147,483,647 (2^31-1)

Reference for	| Pierre L'Ecuyer and Terry H. Andres,
choice of 	| "A Random Number Generator Based on the Combination
parameters:	| of Four LCGs", unpubl.

		| This is one of the component generators of their CLCG4.

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.

Reference for	| L'Ecuyer and Andres, op.cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

@interface PMMLCG5gen: PMMLCGgen

{ 

/*
Parameters:	| a =       207,707
		| m = 2,147,483,543 (2^31-105)

Reference for	| Pierre L'Ecuyer and Terry H. Andres,
choice of 	| "A Random Number Generator Based on the Combination
parameters:	| of Four LCGs", unpubl.

		| This is one of the component generators of their CLCG4.

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.

Reference for	| L'Ecuyer and Andres, op.cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

@interface PMMLCG6gen: PMMLCGgen

{ 

/*
Parameters:	| a =       138,556
		| m = 2,147,483,423 (2^31-225)

Reference for	| Pierre L'Ecuyer and Terry H. Andres,
choice of 	| "A Random Number Generator Based on the Combination
parameters:	| of Four LCGs", unpubl.

		| This is one of the component generators of their CLCG4.

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.

Reference for	| L'Ecuyer and Andres, op.cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

@interface PMMLCG7gen: PMMLCGgen

{ 

/*
Parameters:	| a =        49,689
		| m = 2,147,483,323 (2^31-325)

Reference for	| Pierre L'Ecuyer and Terry H. Andres,
choice of 	| "A Random Number Generator Based on the Combination
parameters:	| of Four LCGs", unpubl.

		| This is one of the component generators of their CLCG4.

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.

Reference for	| L'Ecuyer and Andres, op.cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end


@interface PMMLCG8gen: PMMLCGgen

{ 

/*
Parameters:	| a =        40,014
		| m = 2,147,483,563 (2^31-85)

Reference for	| Pierre L'Ecuyer and Serge Cote,
choice of 	| "Implementing a Random Number Package with Splitting
parameters:	| Facilities," ACM Trans. Math. Softw. 17 no 1 (March 1991).

		| This is one of the component generators of their generator.

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.

Reference for	| L'Ecuyer and Cote, op.cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

@interface PMMLCG9gen: PMMLCGgen

{ 

/*
Parameters:	| a =        40,692
		| m = 2,147,483,399 (2^31-249)

Reference for	| Pierre L'Ecuyer and Serge Cote,
choice of 	| "Implementing a Random Number Package with Splitting
parameters:	| Facilities," ACM Trans. Math. Softw. 17 no 1 (March 1991).

		| This is one of the component generators of their generator.

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length (m-1).

Output Quality:	| All bits should be safe.

Reference for	| L'Ecuyer and Cote, op.cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

