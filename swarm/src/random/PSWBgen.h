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
Name:            PSWBgen.h
Description:     Subtract-with-borrow Congruential Generator with prime modulus
Library:         random
Original Author: Sven Thommesen
Date:		 1997-09-01 (v. 0.7)

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


#import <Swarm/random.h>
#import <Swarm/SwarmObject.h>


#define COMPONENTS 1
#define SEEDS      44

@interface PSWBgen: SwarmObject <SimpleRandomGenerator, CREATABLE>

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


