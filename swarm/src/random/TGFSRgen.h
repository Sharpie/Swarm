// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            TGFSRgen.h
Description:     Twisted GFSR generator
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

Generator name:	| TGFSR

Description:	| Twisted GFSR generator

Reference:	| Makoto Matsumoto and Yoshiharu Kurita,
		| "Twisted GFSR Generators II". 
		| ACM TOMACS, vol 4 no 3 July 1994 pp. 254-266.
		| Amended by M. Matsumoto, July 8 1996.

		| 
Algorithm:	| 
		| 

Comments:	| 

Implementation:	| 

Parameters:	| unsigned int w		// word length
		| unsigned int N		// size of state array
		| unsigned int M		// lag
		| unsigned int s		// 'magic vector'
		| unsigned int t		//    "     "
		| unsigned int b		//    "     "
		| unsigned int c		//    "     "
		| unsigned int a		//    "     "

State:		| unsigned int state[N]		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,2^w-1] may be returned

Valid seeds:	| Using create:setStateFromSeed:  in [1,2^32-1]
		| Using create:setStateFromSeeds: in [1,2^w-1].

Cycles:		| With properly chosen parameters, these generators
		| have a single cycle of length 2^(w*N) -1.

Output Quality:	| Good.

Reference for	| Matsumoto and Kurita, op. cit.
output quality:	| Also recommended by P. L'Ecuyer (private communication).

TT403gen,	| On a 486/66, the following approximate measures were obtained:
TT775gen,	|   -getUnsignedSample:                        4.654 uS
TT800gen	|   -getFloatSample, getDoubleSample:          6.430 uS
speed:		|   -getFatDoubleSample, getLongDoubleSample: 11.953 uS

Relative speed:	| Speed 0.795 (time 1.259) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/

#import <random.h>
#import <objectbase/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      0

@interface TGFSRgen: SwarmObject

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
   unsigned int *maxSeedValues;		// if multiple seeds

// Parameters:

   BOOL antiThetic;			// see -getUnsignedSample

// Working variables:

   BOOL singleInitialSeed;		// created with 1 seed ?
   unsigned int initialSeed;		// starting seed used (if 1)
   unsigned int *initialSeeds;		// starting seeds used (if > 1)

// Count of variates generated:

   unsigned long long int currentCount;	// variates generated so far

// -- 

// Generator parameters:

   unsigned int w;		// word length
   unsigned int N;		// size of state array
   unsigned int M;		// lag
   unsigned int s;		// 'magic vector'
   unsigned int t;		//    "     "
   unsigned int b;		//    "     "
   unsigned int c;		//    "     "
   unsigned int a[2];		//    "     "

// Fixed value working variables:


// Working variables:

   unsigned index;		// pointer to current state value [=k]

// State variables:

   unsigned int *state;			// pointer to state vector [=x]

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


@interface  TT403gen: TGFSRgen

{

/*
Parameters:	| unsigned int w=31		// word length
		| unsigned int N=13		// size of state array
		| unsigned int M=2		// lag
		| unsigned int s=8		// 'magic vector'
		| unsigned int t=14		//    "     "
		| unsigned int b=0x102d1200	//    "     "
		| unsigned int c=0x66e50000	//    "     "
		| unsigned int a=0x6b5eccf6	//    "     "

Reference for	| Matsumoto and Kurita, op. cit.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length 2^403 -1.

Output Quality:	| Good. Parameters chosen on the basis of theory 
		| and empirical testing.

Reference for	| Matsumoto and Kurita, op. cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end


@interface  TT775gen: TGFSRgen

{

/*
Parameters:	| unsigned int w=31		// word length
		| unsigned int N=25		// size of state array
		| unsigned int M=8		// lag
		| unsigned int s=6		// 'magic vector'
		| unsigned int t=14		//    "     "
		| unsigned int b=0x1abd5900	//    "     "
		| unsigned int c=0x776a0000	//    "     "
		| unsigned int a=0x6c6cb38c	//    "     "

Reference for	| Matsumoto and Kurita, op. cit.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length 2^775 -1.

Output Quality:	| Good. Parameters chosen on the basis of theory 
		| and empirical testing.

Reference for	| Matsumoto and Kurita, op. cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end


@interface  TT800gen: TGFSRgen

{

/*
Parameters:	| unsigned int w=32		// word length
		| unsigned int N=25		// size of state array
		| unsigned int M=7		// lag
		| unsigned int s=7		// 'magic vector'
		| unsigned int t=15		//    "     "
		| unsigned int b=0x2b5b2500	//    "     "
		| unsigned int c=0xdb8b0000	//    "     "
		| unsigned int a=0x8ebfd028	//    "     "

Reference for	| Matsumoto and Kurita, op. cit.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length 2^800 -1.

Output Quality:	| Good. Parameters chosen on the basis of theory 
		| and empirical testing.

Reference for	| Matsumoto and Kurita, op. cit.
output quality:	| 
*/

}

-initState;

+ 		create: aZone setStateFromSeed:  (unsigned)   seed;
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
+		createWithDefaults: aZone;

@end

