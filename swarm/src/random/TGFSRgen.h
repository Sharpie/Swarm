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

Modified by:	Sven Thommesen
Date:		1998-10-08 (v. 0.8)
Changes:	Code cleanup related to signed/unsigned comparisons.
		Code rearranged for create-phase compatibility.
		Added -drop method.
		Added -reset method.
		Fixed memset bug in -initState in subclasses.
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

@interface TGFSRgen: SwarmObject  <SimpleRandomGenerator>

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

// Methods for this class only:
- (void)drop;

@end


@interface  TT403gen: TGFSRgen <SimpleRandomGenerator, CREATABLE>

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

CREATING

- initState;
+ create: aZone setStateFromSeed:  (unsigned)   seed;
+ create: aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: aZone;

SETTING

USING

@end


@interface  TT775gen: TGFSRgen <SimpleRandomGenerator, CREATABLE>

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

CREATING

- initState;
+ create: aZone setStateFromSeed:  (unsigned)   seed;
+ create: aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: aZone;

SETTING

USING

@end


@interface  TT800gen: TGFSRgen <SimpleRandomGenerator, CREATABLE>

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

CREATING

- initState;
+ create: aZone setStateFromSeed:  (unsigned)   seed;
+ create: aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: aZone;

SETTING

USING

@end

