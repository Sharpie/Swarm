// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            C4LCGXgen.h
Description:     Combined random generator using 4 (PMM)LGC generators
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

Generator name:	| C4LCGX

Description:	| Combined random generator using 4 (PMM)LGC generators

Reference:	| Pierre L'Ecuyer and Terry H. Andres,
		| "A Random Number Generator Based on the Combination
		|  of Four LCGs", 17 December 1996.
		| To appear in Mathematics and Computers in Simulation. See:
		| <http://www.iro.umontreal.ca/~lecuyer/myftp/papers/clcg4.ps>.
		| Their name for the generator: 'clcg4'.

Comments:	| This portable generator is based on a backbone generator
		| which is a combination of 4 (PMM)LCG generators. It has a
		| period length of (m1-1)(m2-1)(m3-1)(m4-1) / 8, or almost 
		| 2^121 (2.6e36).

		| The backbone generator's period can be split up into a
		| number of 'virtual generators' (A), each of which can be set
		| to access a number of 'segments' (V) of length W, subject to
		| the constraint that A * V * W <= 2^120.

		| This is possible because the backbone generator has 
		| 'jump-ahead facilities' -- i.e. a quick method of calculating
		| the state of the generator at a given point in its cycle
		| without having to actually draw a large number of variates.

Algorithm	| state(j) = seed(j)
for each	| newstate(j) = (a(j) * oldstate(j)) mod m(j)
generator:	| output(j) = state(j)

Algorithm	| output = (state(1) - state(2) + state(3) - state(4)) mod m(0)
for combination:| 

Comments:	| For a discussion of PMMLCG generators, see the source file
		| PMMLCGgen.h.

Configuration	| unsigned int numGenerators;		// # virtual generators
parameters:	| unsigned int numSegments;		// log2(#segments/vGen)
		| unsigned int segmentLength;		// log2(segment length)

State:		| unsigned int state [numGenerators] [4];
		| (each virtual generator has a 4-integer state vector)

Output types	| unsigned int in [0,unsignedMax].
(for each	| float in [0.0,1.0) using 1 iteration of the generator.
virtual		| double in [0.0,1.0) using 1 or 2 iterations of the generator.
generator):	| long double in [0.0,1.0) using 2 iterations of the generator.

Output range:	| The combined generator yields output in the range [0,m0-2]
		| (i.e. [0,2147483645].)

Limits on	| Number of generators A <= 2^25  (33,554,432)
parameters:	| (You'll likely run out of memory before then ...)
		| Number of segments / generator <= 2^63 (i.e. v<=63)
		| Length of each segment <= 2^63 (i.e. w<=63)

Defaults:	| A=128, v=31, w=41

Valid seeds:	| Using create:setStateFromSeed: in [1, 2^32-1]
		| Using create:setStateFromSeeds:
		| The generator must be seeded with 4 starting seeds:
		| 	Seed[0]: [1, 2147483646] = [1,m1-1]
		| 	Seed[1]: [1, 2147483542] = [1,m2-1]
		| 	Seed[2]: [1, 2147483422] = [1,m3-1]
		| 	Seed[3]: [1, 2147483322] = [1,m4-1]

Cycles:		| With properly chosen parameters, the component generators
		| have a single full cycle of length (m-1).
		| The combined generator has a cycle length of
		| 2147483646*2147483542*2147483422*2147483322/8 > 2^120.

Output Quality:	| All bits should be safe.
		| The combined generator has much better statistical
		| properties than either of its component PMMLCG's.

Reference for	| L'Ecuyer and Andres, op. cit.
output quality:	| 

Implementation  | The code for -getUnsignedMax uses a trick devised by
comment:        | Schrage to avoid 32-bit overflow when calculating the
		| next state (i.e. using q=m/a and r=m%a as auxiliary
		| values). Using 64-bit math (long long integers) is more
		| straightforward. Unfortunately, on x86 at least, there is 
		| a significant speed penalty for doing so.
		| The output, however, would be identical. YMMV.

C4LCGXgen	| On a 486/66, the following approximate measures were obtained:
speed:		|   -getUnsignedSample:                       11.688 uS
		|   -getFloatSample, getDoubleSample:         13.644 uS
		|   -getFatDoubleSample, getLongDoubleSample: 26.154 uS

Relative speed:	| Speed 0.316 (time 3.160) relative to MT19937 getUnsignedSample
--------------- | --------------------------------------------------------------
*/

/*
Parameters:	| a[COMPONENTS] = {      45991,     207707,
		|			     138556,      49689 }
		| m[COMPONENTS] = { 2147483647, 2147483543,
		|			 2147483423, 2147483323 }

Reference for	| L'Ecuyer and Andres, op. cit.
parameters:	| 

Cycles:		| With the chosen parameters, this generator
		| has a single full cycle of length close to 2^121 (2.6e36).

Output Quality:	| The authors performed an exhaustive search for component
		| generators that would yield the best statistical quality.

Reference for	| L'Ecuyer and Andres, op. cit.
output quality:	| 
*/


#import <string.h>
#import <random.h>
#import <objectbase/SwarmObject.h>

#define COMPONENTS 4
#define SEEDS      4

struct vGenStruct {
   unsigned long long int currentCount;
   unsigned long long int currentSegment;
   unsigned int Ig [SEEDS];
   unsigned int Lg [SEEDS];
   unsigned int Cg [SEEDS];
} ;


@interface C4LCGXgen: SwarmObject

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


//                                                                       split.h

// ----- Private methods: -----

-		generateSeeds;
-		setState;
-		allocVectors;

-		initState;
+		createBegin: (id) aZone;
-		setA: (unsigned) A setv: (unsigned) v setw: (unsigned) w;
-		setStateFromSeed: (unsigned) seed;
-		setStateFromSeeds: (unsigned *) seeds;
-		createEnd;

// ----- Single-seed creation: -----

+		create: aZone
		   setA: (unsigned) A 		// # of virtual generators
		   setv: (unsigned) v 		// log2(#segments/generator)
		   setw: (unsigned) w		// log2(segment length)
		setStateFromSeed:  (unsigned)   seed;

// Limits on seed value supplied (minimum = 0):
- (unsigned)	getMaxSeedValue;

// Return generator starting value:
- (unsigned)	getInitialSeed;	

// ----- Multi-seed creation: -----

+		create: aZone
		   setA: (unsigned) A 		// # of virtual generators
		   setv: (unsigned) v 		// log2(#segments/generator)
		   setw: (unsigned) w		// log2(segment length)
		setStateFromSeeds: (unsigned *) seeds;

// Number of seeds required (size of array) (minimum = 1):
- (unsigned)	lengthOfSeedVector;

// Limits on seed values supplied (minimum = 0):
- (unsigned *)	getMaxSeedValues;

// Return generator starting values:
- (unsigned *)	getInitialSeeds;

// ----- Other create methods: -----

// Create with a default set of seeds and parameters:
+		createWithDefaults: aZone;

-		setAntithetic: (BOOL) antiT;

// ----- Return values of parameters: -----

- (unsigned)    getNumGenerators;		// returns A
- (unsigned)    getNumSegments;			// returns v
- (unsigned)    getSegmentLength;		// returns w
- (BOOL)	getAntithetic;

// ----- Virtual generator management: -----

// Reset the state of a virtual generator to the start of a specified segment:

-initGenerator:    (unsigned) vGen;		// to segment #0
-restartGenerator: (unsigned) vGen;		// to start of current segment
-advanceGenerator: (unsigned) vGen;		// to next segment
-jumpGenerator:    (unsigned) vGen  toSegment: (unsigned long long int) seg;

// Reset the state of all virtual generators to the start of a given segment:

-initAll;					// to segment #0
-restartAll;					// to start of current segment
-advanceAll;					// to next segment
-jumpAllToSegment: (unsigned long long int) seg;

// ----- Return state values: -----

// Return generator seeds for a given virtual generator:
//   (the single-seed version of these methods would never be applicable.)

- (unsigned *)   getInitialSeeds:  (unsigned) vGen;	// start of segment #0
- (unsigned *)   getLastSeeds:     (unsigned) vGen;	// start of current seg
- (unsigned *)   getCurrentSeeds:  (unsigned) vGen;	// current state

// Return current segment and number of variates generated from that segment:

- (unsigned long long int) getCurrentSegment: (unsigned) vGen;
- (unsigned long long int) getCurrentCount:   (unsigned) vGen;

// ----- Generator output: -----

// The maximum value returned by getUnsignedSample is:

- (unsigned)    getUnsignedMax;

// Return a 'random' integer uniformly distributed over [0,unsignedMax]
//   from the current segment of virtual generator vGen:

- (unsigned)	getUnsignedSample:   (unsigned) vGen;

// Return a 'random' floating-point number uniformly distributed in [0.0,1.0):

- (float)       getFloatSample:      (unsigned) vGen;	// using 1 unsigned
- (double)      getThinDoubleSample: (unsigned) vGen;	// using 1 unsigned
- (double)      getDoubleSample:     (unsigned) vGen;	// using 2 unsigneds
- (long double) getLongDoubleSample: (unsigned) vGen;	// using 2 unsigneds

// Warning: use of the last method is not portable between architectures.

// ----- Object state management: -----

- (unsigned)	getStateSize;		
- (void)	putStateInto: (void *) buffer;
- (void)	setStateFrom: (void *) buffer;
- (void)	describe: (id) outStream;
- (const char *)getName;		
- (unsigned)	getMagic;	

@end

