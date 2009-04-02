// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:            MT19937gen.h
Description:     'Mersenne Twister' Twisted GFSR generator
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

#import <Swarm/random.h>
#import <Swarm/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      624

@interface MT19937gen: SwarmObject <SimpleRandomGenerator, CREATABLE>

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

   unsigned (*getUnsignedSample) (id, SEL);
}


CREATING

// Unpublished (private) methods:
- runup: (unsigned)streak;
- initState;
+ createBegin: (id <Zone>)aZone;
- createEnd;

// @protocol Simple
+ createWithDefaults: (id <Zone>)aZone;

// @protocol SingleSeed
+ create: (id <Zone>)aZone setStateFromSeed: (unsigned)seed;

// @protocol MultiSeed
+ create: (id <Zone>)aZone setStateFromSeeds: (unsigned *)seeds;

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
