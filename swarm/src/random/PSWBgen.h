// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            PSWBgen.h
Description:     Subtract-with-borrow Congruential Generator with prime modulus
Library:         random
Original Author: Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| PSWB

Description:	| Subtract-with-Borrow Congruential Generator
		|    with Prime Modulus

Reference:	| George Marsaglia and Arif Zaman, 
		| "A New Class of Random Number Generators",
		| Annals of Applied Probability vol 1 no 3 (1991), 462-480.
		| SWB is an improvement on the Lagged Fibonacci generators.

		| PSWB is an improvement on SWB in that the use of a prime
		| modulus guarantees a single full cycle. 
		| It's slower, of course.

		| state vector = f(seed)
Algorithm:	| state_n = (state_(n-s) - state_(n-r) - carry) mod m; r > s
		| output = state_n

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
		| int carry			// carry bit

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,m-1] may be returned.

Valid seeds:	| Using create:setStateFromSeed:  [1,2^32-1]
		| Using create:setStateFromSeeds: r seeds in [1,2^32-7]
		| and one seed in [0,1] (the carry)

Cycles:		| With properly chosen parameters, this generator
		| has a single cycle of length close to 10^414.

		| Unfortunately, there are no 'jump-ahead' or 'splitting'
		| facilities that would allow us to jump ahead in the cycle,
		| other than calling the generator a number of times.

Output Quality:	| Much better than other Lagged Fibonacci generators, such as
		| ACG and SCG.

Reference for	| Marsaglia and Zaman, op.cit.
output quality:	| 

PSWBgen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       3.452 uS
		|   -getFloatSample, getDoubleSample:         5.166 uS
		|   -getFatDoubleSample, getLongDoubleSample: 9.544 uS

Relative speed:	| Speed 1.070 (time 0.933) relative to MT19937 getUnsignedSample
--------------- | --------------------------------------------------------------
*/

/*
Parameters:	| r = 43			// long lag
		| s = 22			// short lag
		| m = 2^32 - 5			// modulus (prime)

Reference for	| Marsaglia, op. cit.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single cycle of length close to 10^414.

Output Quality:	| Good.

Reference for	| Marsaglia, op.cit., p. 479
output quality:	| 
*/


#import <random.h>
#import <objectbase/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      44

@interface PSWBgen: SwarmObject

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

   unsigned int m;			// modulus
   unsigned int r;			// long lag
   unsigned int s;			// short lag

// Fixed value working variables:

   // (none)

// Working variables:

   unsigned index;			// index into state vector

// State variables:

   unsigned int carry;			// carry bit (part of state)
   unsigned int state[SEEDS-1];		// state vector

}

//                                                                      simple.h

// ----- Private methods: -----

-		runup: (unsigned) streak;
-		setState;
-		generateSeeds;

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


