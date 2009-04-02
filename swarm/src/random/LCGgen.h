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

#import <Swarm/random.h>
#import <Swarm/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      1

@interface LCGgen: SwarmObject <SimpleRandomGenerator>

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
- localGenerateSeeds;			// this class only

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


@interface LCG1gen: LCGgen <SimpleRandomGenerator, CREATABLE>

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

CREATING

- initState;
+ create: (id <Zone>)aZone setStateFromSeed:  (unsigned)   seed;
+ create: (id <Zone>)aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: (id <Zone>)aZone;

SETTING

USING

@end

@interface LCG2gen: LCGgen <SimpleRandomGenerator, CREATABLE>

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

CREATING

- initState;
+ create: (id <Zone>)aZone setStateFromSeed:  (unsigned)   seed;
+ create: (id <Zone>)aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: (id <Zone>)aZone;

SETTING

USING

@end

@interface LCG3gen: LCGgen <SimpleRandomGenerator, CREATABLE>

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

CREATING

- initState;
+ create: (id <Zone>)aZone setStateFromSeed:  (unsigned)   seed;
+ create: (id <Zone>)aZone setStateFromSeeds: (unsigned *) seeds;
+ createWithDefaults: (id <Zone>)aZone;

SETTING

USING

@end

