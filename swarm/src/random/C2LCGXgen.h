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
Name:            C2LCGXgen.h
Description:     Combined random generator using 2 (PMM)LGC generators
Library:         random
Original Author: Sven Thommesen
Date:            1997-09-01 (v. 0.7)

Modified by:	Sven Thommesen
Date:		1998-10-08 (v. 0.8)
Changes:	Code cleanup related to signed/unsigned comparisons.
		Code rearranged for create-phase compatibility.
		Added -drop method.
		Added -reset method.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Generator Documentation:
		| ------------------------

Generator name:	| C2LCGX

Description:	| Combined random generator using 2 (PMM)LGC generators

Reference:	| Pierre L'Ecuyer and Serge Cote,
		| "Implementing a Random Number Package with Splitting 
		| Facilities." ACM Transactions on Mathematical Software 
		| vol. 17 no. 1, March 1991, pp. 98-111.
		| Their name for the generator: 'split.c'.
		| See also <http://www.iro.umontreal.ca/~lecuyer>.

Comments:	| This portable generator is based on a backbone generator
		| which is a combination of 2 (PMM)LCG generators. It has a
		| period length of almost 2^61 (2.3e18).

		| The backbone generator's period can be split up into a
		| number of 'virtual generators' (A), each of which can be set
		| to access a number of 'segments' (V) of length W, subject to
		| the constraint that A * V * W <= 2^60.

		| This is possible because the backbone generator has 
		| 'jump-ahead facilities' -- i.e. a quick method of calculating
		| the state of the generator at a given point in its cycle
		| without having to actually draw a large number of variates.

Algorithm	| state(j) = seed(j)
for each	| newstate(j) = (a(j) * oldstate(j)) mod m(j)
generator:	| output(j) = state(j)

Algorithm	| output = (state(1) - state(2)) mod m(0)
for combination:| 

Comments:	| For a discussion of PMMLCG generators, see the source file
		| PMMLCGgen.h.

Configuration	| unsigned int numGenerators;		// # virtual generators
parameters:	| unsigned int numSegments;		// log2(#segments/vGen)
		| unsigned int segmentLength;		// log2(segment length)

State:		| unsigned int state [numGenerators] [2];
		| (each virtual generator has a 4-integer state vector)

Output types	| unsigned int in [0,unsignedMax].
(for each	| float in [0.0,1.0) using 1 iteration of the generator.
virtual		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
generator):	| long double in [0.0,1.0) using 2 iterations of the generator.

Output range:	| The combined generator yields output in the range [0,m0-1]
		| (i.e. [0,2147483562].)

Limits on	| Number of generators A <= 2^25  (33,554,432)
parameters:	| (You'll likely run out of memory before then ...)
		| Number of segments / generator <= 2^60 (i.e. v <= 60)
		| Length of each segment <= 2^60 (i.e. w <= 60)

Defaults:	| A=32, v=20, w=30

Valid seeds:	| Using create:setStateFromSeed: in [1, 2^32-1]
		| Using create:setStateFromSeeds:
		| The generator must be seeded with 2 starting seeds:
		| 	Seed[0]: [1, 2147483562] = [1,m1-1]
		| 	Seed[1]: [1, 2147483398] = [1,m2-1]

Cycles:		| With properly chosen parameters, the component generators
		| have a single full cycle of length (m-2).
		| The combined generator has a cycle length of
		| (m1-1)*(m2-1)/2 = 2147483562*2147483398/2 ~= 2^61.

Output Quality:	| All bits should be safe.
		| The combined generator has much better statistical
		| properties than either of its component PMMLCG's.

Reference for	| L'Ecuyer and Cote, op. cit.
output quality:	| 

Implementation  | The code for -getUnsignedMax uses a trick devised by
comment:        | Schrage to avoid 32-bit overflow when calculating the
		| next state (i.e. using q=m/a and r=m%a as auxiliary
		| values). Using 64-bit math (long long integers) is more
		| straightforward. Unfortunately, on x86 at least, there is 
		| a significant speed penalty for doing so.
		| The output, however, would be identical. YMMV.

C2LCGXgen	| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                        7.029 uS
		|   -getFloatSample, getDoubleSample:          9.049 uS
		|   -getFatDoubleSample, getLongDoubleSample: 16.725 uS

Relative speed:	| Speed 0.526 (time 1.900) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/

/*
Parameters:	| a[COMPONENTS] = {      40014,      40692 }
		| m[COMPONENTS] = { 2147483563, 2147483399 }

Reference for	| L'Ecuyer and Cote, op. cit.
parameters:	| 

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length close to 2^60 (2.3e18).

Output Quality:	| The authors performed an exhaustive search for component
		| generators that would yield the best statistical quality.

Reference for	| L'Ecuyer and Cote, op. cit.
output quality:	| 
*/


#import <Swarm/random.h>
#import <Swarm/SwarmObject.h>


#define COMPONENTS 2U
#define SEEDS      2U
#define VGENS      0

struct vGenStruct {
   unsigned long long int currentCount;
   unsigned long long int currentSegment;
   unsigned int Ig [SEEDS];
   unsigned int Lg [SEEDS];
   unsigned int Cg [SEEDS];
} ;


@interface C2LCGXgen: SwarmObject <SplitRandomGenerator, CREATABLE>

{

// 1. Data for the whole backbone generator:

// Generator personality:

   unsigned int stateSize;
   unsigned int genMagic;
   char genName[12];	

// Characteristic constants:

   unsigned int unsignedMax;		// largest integer value returned
   double invModMult;			// (1.0/unsignedMax+1)
   double invModMult2;			// invModMult^2

   unsigned int lengthOfSeedVector;	// if multiple seeds
   unsigned int maxSeedValues[SEEDS];	// if multiple seeds

// Parameters defining the configuration:

   unsigned int numGenerators;		// A
   unsigned int numSegments;		// v
   unsigned long long int segmentMax;	// V = 2^v
   unsigned int segmentLength;		// w
   unsigned long long int countMax;	// W = 2^w

// Other parameters:

   BOOL antiThetic;			// see -getUnsignedSample

// Working variables:

   BOOL singleInitialSeed;		// created with 1 seed ?
   unsigned int initialSeed;		// starting seed used (if 1)
   unsigned int initialSeeds[SEEDS];	// starting seeds used (if > 1)

// 2. Data for each of the virtual generators:

   struct vGenStruct * vGenArr;		// vGenArr[A], allocated dynamically

// --

// 3. Data for each of the component generators of the backbone:

// Generator parameters:

   unsigned int a   [COMPONENTS];	// multiplier
   unsigned int m   [COMPONENTS];	// modulus

// Fixed value working variables (all functions of m):

   unsigned int q   [COMPONENTS];	// quotient m/a
   unsigned int r   [COMPONENTS];	// remainder m%a

   unsigned int aw  [COMPONENTS];	// segment multiplier
   unsigned int avw [COMPONENTS];	// vGen multiplier

}

CREATING

// Unpublished (private) methods:
- initState;
- allocVectors;
+ createBegin: (id <Zone>)aZone;
- createEnd;
- setA: (unsigned)A setV: (unsigned)v setW: (unsigned)w;

// @protocol Split
+ createWithDefaults: (id <Zone>)aZone;

// @protocol SplitSingleSeed
+ create          : (id <Zone>)aZone
              setA: (unsigned)A 	// # of virtual generators
              setV: (unsigned)v 	// log2(#segments/generator)
              setW: (unsigned)w		// log2(segment length)
  setStateFromSeed: (unsigned)seed;

// @protocol SplitMultiSeed
+ create          : (id <Zone>)aZone
              setA: (unsigned)A         // # of virtual generators
              setV: (unsigned)v         // log2(#segments/generator)
              setW: (unsigned)w	        // log2(segment length)
 setStateFromSeeds: (unsigned *)seeds;

SETTING

// Unpublished (private) methods:
- (unsigned)MultModMs: (unsigned)s t: (unsigned)t M: (unsigned)M;
- setState;
- generateSeedVector;
- generateSeeds;

// @protocol Split
- setAntithetic: (BOOL)antiT;
- initGenerator: (unsigned)vGen;
- initAll; 

// @protocol SplitSingleSeed
- setStateFromSeed: (unsigned)seed;

// @protocol SplitMultiSeed
- setStateFromSeeds: (unsigned *)seeds;



USING

// Unpublished (private) methods:
- (void)drop;

// @protocol InternalState
- (unsigned)getStateSize;		// size of buffer needed
- (void)putStateInto: (void *)buffer;	// save state data for later use
- (void)setStateFrom: (void *)buffer;	// set state from saved data
- (void)describe: outStream;	        // prints ascii data to stream
- (const char *)getName;		// returns name of object
- (unsigned)getMagic;			// object's 'magic number'

// @protocol SplitOut
- (unsigned)getUnsignedMax;		// min is 0

- (unsigned)getUnsignedSample:      (unsigned)vGen;
- (float)getFloatSample:            (unsigned)vGen;
- (double)getThinDoubleSample:      (unsigned)vGen;
- (double)getDoubleSample:          (unsigned)vGen;
- (long double)getLongDoubleSample: (unsigned)vGen;	// non-portable

// @protocol Split
- (unsigned)getNumGenerators;		// A
- (unsigned)getNumSegments;		// v
- (unsigned)getSegmentLength;		// w
- (BOOL)getAntithetic;
- reset;

- restartGenerator: (unsigned)vGen;
- advanceGenerator: (unsigned)vGen;
- jumpGenerator:    (unsigned)vGen  toSegment: (unsigned long long int)seg;

- restartAll;
- advanceAll;
- jumpAllToSegment: (unsigned long long int)seg;

- (unsigned long long int)getCurrentCount:   (unsigned)vGen;
- (unsigned long long int)getCurrentSegment: (unsigned)vGen;

// @protocol SplitSingleSeed
- (unsigned)getMaxSeedValue;		// min is 1
- (unsigned)getInitialSeed;

// @protocol SplitMultiSeed
- (unsigned)lengthOfSeedVector;
- (unsigned *)getMaxSeedValues;		// min is 1
- (unsigned *)getInitialSeeds;     	// = getInitialSeeds: 0

@end

