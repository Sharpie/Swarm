// Swarm library. Copyright � 1996-1999 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            MWCBgen.h
Description:     Multiply With Carry generator
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01 (v. 0.7)

Modified by:	Sven Thommesen
Date:		1998-10-08 (v. 0.8)
Changes:	Code cleanup related to signed/unsigned comparisons.
		Code rearranged for create-phase compatibility.
		Added -reset method.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| MWCB

Description:	| Multiply-With-Carry generator

Reference:	| Article posted to Usenet in 1994 by George Marsaglia,
		| "The Mother of all Random Number Generators."
		| Source found at <ftp://ftp.taygeta.com/pub/c/mother.c>
		| Also document /papers/mwc1.ps on his Diehard CD-ROM,
		| found at <http://www.hku.hk/internet/randomCD.html>.

Algorithm:	| K = a * (K & 65535) + (K>>16);
		| J = b * (J & 65535) + (J>>16);
		| return (K<<16) + (J&65535);

Comments:	| This generator is a combination of 2 16-bit 
		| Multiply-With-Carry generators. 

Implementation:	| This generator implements an alternate manner of
		| conjoining the two components (differs from MWCA).
		| It also passes all tests and is fast.

Parameters:	| unsigned int a = 30903	// multiplier
		| unsigned int b = 18000	// multiplier

State:		| unsigned int K,J		// state vector

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,unsignedMax] = [0,2^32-1].

Valid seeds:	| Using create:setStateFromSeed:  [1, 2^32-1]
		| Using create:setStateFromSeeds: each component may have
		| a 'state' <= 65535 and a 'carry' <= (multiplier-1). Thus
		| the first component has a maximum seed of
		| ( (a-1) << 16) + 65535.

Cycles:		| This generator is claimed to be strictly periodic, with a 
		| period > 2^59. (There's possibly two such cycles.)

Output Quality:	| Good.

Reference for	| Marsaglia, op. cit.
output quality:	| 

MWCgen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       3.399 uS
		|   -getFloatSample, getDoubleSample:         5.131 uS
		|   -getFatDoubleSample, getLongDoubleSample: 9.447 uS

Relative speed:	| Speed 1.088 (time 0.919) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <random.h>
#import <objectbase/SwarmObject.h>

#define COMPONENTS 1
#define SEEDS      2

@interface MWCBgen: SwarmObject <SimpleRandomGenerator, CREATABLE>

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

   unsigned int a,b;			// multipliers

// Fixed value working variables:


// Working variables:

   unsigned int lastX;			// most recent output

// State variables:

   unsigned int K, J;			// state vector [=x]


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


