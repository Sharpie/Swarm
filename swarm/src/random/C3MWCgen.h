// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            C3MWCgen.h
Description:     Combined Multiply With Carry generator
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

Generator name:	| C3MWC

Description:	| Combined Multiply-With-Carry generator

Reference:	| Article posted to Usenet in 1994 by George Marsaglia,
		| "The Mother of all Random Number Generators."
		| Source found at <ftp://ftp.taygeta.com/pub/c/mother.c>
		| Also document /papers/mwc1.ps on his Diehard CD-ROM,
		| found at <http://www.hku.hk/internet/randomCD.html>.

Algorithm:	| K = a * (K & 65535) + (K>>16);
		| J = b * (J & 65535) + (J>>16);
		| I = c * (I & 65535) + (I>>16);
		| L = d * (L & 65535) + (L>>16);
		| M = e * (M & 65535) + (M>>16);
		| N = f * (N & 65535) + (N>>16);
		| return ((K+I+M)>>16) + (J+L+N);

Comments:	| This generator is a combination of 3 MWC generators,
		| each of which is a combination of 2 16-bit 
		| Multiply-With-Carry generators. 

Parameters:	| unsigned int a = 30903	// multiplier
		| unsigned int b = 18000
		| unsigned int c = 29013
		| unsigned int d = 30345
		| unsigned int e = 30903
		| unsigned int f = 31083

State:		| unsigned int K,J,I,L,M,N		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,unsignedMax] = [0,2^32-1].

Valid seeds:	| Using create:setStateFromSeed:  [1, 2^32-1]
		| Using create:setStateFromSeeds: each component may have 
		| a 'state' <= 0xffff and a carry <= (multiplier-1), so that 
		| for the first component the largest seed allowed is 
		| (a-1)<<16 + 65535.

Cycles:		| This generator is claimed to be strictly periodic, with a 
		| period > 2^118. There may be more than one cycle.

Output Quality:	| Good.

Reference for	| Marsaglia, op. cit.
output quality:	| 

C3MWCgen	| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                        6.437 uS
		|   -getFloatSample, getDoubleSample:          8.200 uS
		|   -getFatDoubleSample, getLongDoubleSample: 15.562 uS

Relative speed:	| Speed 0.574 (time 1.741) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <random.h>
#import <objectbase/SwarmObject.h>

#define COMPONENTS 3
#define SEEDS      6

@interface C3MWCgen: SwarmObject

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

   unsigned int a,b,c,d,e,f;		// multipliers

// Fixed value working variables:


// Working variables:

   unsigned int lastX;			// most recent output

// State variables:

   unsigned int K, J, I, L, M, N;	// state vector 


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


