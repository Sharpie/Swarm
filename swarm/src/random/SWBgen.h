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
Name:            SWBgen.h
Description:     Subtract-with-borrow Congruential Generator
Library:         random
Original Author: Nelson Minar
Date:            1996-09-09

Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)
Changes:	 Cast the "old style" objective-C objects to Swarm objects,
		 using create phase protocols.

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Fixed bug: carry was not being set properly between calls.
			--> alters output compared to v. 0.6.
		 Changed the inline LCG to a PMMLCG (for seed generation).
			--> alters output compared to v. 0.6.
		 Changed the initial 'runup' length from 5*r to r (max. needed.)
			--> alters output compared to v. 0.6.
		 Standardized the informational comments.
		 Sub-classed SWB1,SWB2,SWB3.
		 Added floating point output (float, double, long double).
		 Revised the 'magic number' numbering scheme.

Modified by:	Sven Thommesen
Date:		1998-10-08 (v. 0.8)
Changes:	Code cleanup related to signed/unsigned comparisons.
		Code rearranged for create-phase compatibility.
		Added -drop method.
		Added -reset method.
		Fixed memset bug in SWBi: -initState
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| SWB

Description:	| Subtract-with-Borrow Congruential Generator

Reference:	| George Marsaglia and Arif Zaman, 
		| "A New Class of Random Number Generators",
		| Annals of Applied Probability vol 1 no 3 (1991), 462-480.
		| SWB is an improvement on the Lagged Fibonacci generators.

		| state vector = f(seed)
Algorithm:	| state_n = (state_(n-s) - state_(n-r) - carry) mod 2^32; r > s
		| output = state_n

		| These generators use a basic algorithm of the form
		| X_n = f(X_(n-r),X_(n-s)) mod m; r>s
		| The function f is typically xor, addition, subtraction,
		| multiplication or subtraction with carry. It uses 
		| simpler math than a basic LCG, but keeps a larger state.

Note:		| This generator relies on controlled overflow at 32 bits.
		| This requires that unsigned be a 32bit value that follows
		| ANSI C rules.

Implementation:	| The state vector contains the generator's r most recent
		| outputs (X_n .. X_(n-r)). This vector is initialized from
		| an LCG generator using one 32-bit seed value. After 
		| initialization we throw away r*5 numbers to 'prime' the
		| generator (i.e. make sure we have entered a closed cycle.)

Parameters:	| unsigned int r		// long lag
		| unsigned int s		// short lag

State:		| unsigned int state[r]		// state vector
		| int carry			// carry bit

Output types:	| unsigned int (we return the current state).
		| float in [0.0,1.0) using 1 iteration of the generator.
		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
		| long double in [0.0,1.0) using 2 iterations of the generator.
		| (See the implementation file for more details.)

Output range:	| Any number in the range [0,2^32-1] may be returned
		| (i.e. [0,4294967295].)

Valid seeds:	| Using create:setStateFromSeed:  [1,2^32-1]
		| Using create:setStateFromSeeds: r seeds in [1,2^32-2]
		| and one seed in [0,1] (the carry)

Cycles:		| With properly chosen parameters, these generators
		| have a number of long cycles, up to 10^354 in length.
		| UNFORTUNATELY there is no way to pick different 
		| cycles, nor any easy way to 'jump ahead' in the
		| chosen cycle (other than calling -getUnsignedSample
		| a number of times). 

Output Quality:	| Much better than other Lagged Fibonacci generators, such as
		| ACG and SCG.

Reference for	| Marsaglia and Zaman, op.cit.
output quality:	| 

SWBgen		| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       3.285 uS
		|   -getFloatSample, getDoubleSample:         5.057 uS
		|   -getFatDoubleSample, getLongDoubleSample: 9.209 uS

Relative speed:	| Speed 1.126 (time 0.888) relative to MT19937 getUnsignedSample
--------------- | --------------------------------------------------------------
*/

#import <Swarm/random.h>
#import <Swarm/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      0

@interface SWBgen: SwarmObject <SimpleRandomGenerator>

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

   unsigned int r;			// long lag
   unsigned int s;			// short lag

// Fixed value working variables:

   // (none)

// Working variables:

   unsigned index;			// pointer to current state value

// State variables:

   unsigned int *state;			// pointer to state vector
   int carry;
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

// Methods for this class only:
- (void)drop;

@end


@interface SWB1gen: SWBgen <SimpleRandomGenerator, CREATABLE>

{

/*
Parameters:	| r = 37
		| s = 24

Reference for	| Marsaglia, op. cit.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has 64 cycles of length 10^354

Output Quality:	| Good.

Reference for	| Marsaglia, op.cit., p. 479
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


@interface SWB2gen: SWBgen <SimpleRandomGenerator, CREATABLE>

{

/*
Parameters:	| r = 24
		| s = 19

Reference for	| Marsaglia, op. cit.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has 1536 cycles of length 10^228

Output Quality:	| Good.

Reference for	| Marsaglia, op.cit., p. 479
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


@interface SWB3gen: SWBgen <SimpleRandomGenerator, CREATABLE>

{

/*
Parameters:	| r = 21
		| s =  6

Reference for	| Marsaglia, op. cit.
parameters:	|

Cycles:		| With the chosen parameters, this generator
		| has 192 cycles of length 10^200

Output Quality:	| Good.

Reference for	| Marsaglia, op.cit., p. 479
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

