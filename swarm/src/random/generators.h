//
// <random/generators.h>
//
//     1997-09-01 (v. 0.7)
//

// 
// See the file docs/README.Generators.v07 for guide to usage
// 

//
// Every implemented random number generator is --
// 
//   A predefined algorithm for generating non-negative integral
//   values uniformly distributed across an algorithm-specific
//   closed interval [0,unsignedMax], as well as floating point
//   values uniformly distributed over the half-open interval 
//   [0.0,1.0).
//

//
// The random number generators may be classified three different ways
//   for implementation purposes: 
// 
// (a) whether the generator is 'short' or 'long': if it is short,
//   it has a small state (1 integer) and a short cycle (less than 2^32).
//   If it is long, it has a larger state (several integers) and a
//   longer cycle (cycle >> 2^32).
// 
// (b) whether the generator is 'single' or 'combined': if it is 
//   combined, it consists of a number of (short or long) components,
//   whose output is combined in such a way that we get longer 
//   cycles and better statistical properties.
// 
// (c) whether the generator is 'simple' or 'split': if it is split, then the
//   generator's cycle is divided into a number (A) of 'virtual generators',
//   each of which can randomly access a number (2^v) of segments (streams)
//   of length 2^w. The particular configuration (A,v,w) is user selectable.
// 
//   These substreams (virtual generators) provide non-overlapping and
//   statistically independent sequences of pseudo-random numbers.
// 
//   Individual virtual generators may be reset to the start of the current
//   segment, jumped ahead to the next segment, or positioned to a chosen
//   segment number; also all virtual generators may be moved in unison.
//

// The present implementation is an attempt to unify the methods available
//   for these different types of generators.
// 


// 
// ---------------------------------------------------------------------
// Protocol components:
//

@deftype SingleSeed
//S: Internal
CREATING
+ create: aZone setStateFromSeed: (unsigned)seed;

USING
//M: The setStateFromSeeds method initializes the seed dependent part of the 
//M: state.
- setStateFromSeed: (unsigned)seed;

//M: The getMaxSeedValue method returns the upper limit on the seed value.
- (unsigned)getMaxSeedValue;	// minvalue is 1

//M: The getInitialSeed method returns the generator's starting values.
- (unsigned)getInitialSeed;
@end


@deftype MultiSeed
//S: Internal
CREATING
+ create: aZone setStateFromSeeds: (unsigned *)seeds;

USING
//M: The setStateFromSeeds method initializes the seed dependent part of the 
//M: state.
- setStateFromSeeds: (unsigned *)seeds;

//M: The lengthOfSeedVector method returns the number of seeds required
//M: (the size of the array).
- (unsigned)lengthOfSeedVector;

//M: The getMaxSeedValue method returns the upper limit on the seed value
//M: that can be supplied.
- (unsigned *)getMaxSeedValues;		// minvalue is 1

//M: The getInitialSeed method returns the generator's starting values.
- (unsigned *)getInitialSeeds;
@end


@deftype Simple
//S: Internal
CREATING
+ createWithDefaults: aZone;

USING
- setAntithetic: (BOOL) antiT;

//M: The getAntithetic method returns the current values of generator
//M: parameters.
- (BOOL)getAntithetic;

//M: The getCurrentCount method returns the count of variates generated.
- (unsigned long long int)getCurrentCount;
@end


@deftype SimpleOut
//S: Internal
USING
//M: The getUnsignedMax method returns the maximum value returned by
//M: getUnsignedSample.
- (unsigned)getUnsignedMax;

//M: The getUnsignedSample method returns a random integer uniformly
//M: distributed over [0,unsignedMax].
- (unsigned)getUnsignedSample;

//M: The getFloatSample method returns a random floating point number
//M: uniformly distributed in the range [0.0, 1.0].
- (float)getFloatSample;		// using 1 unsigned

- (double)getThinDoubleSample;		// using 1 unsigned
- (double)getDoubleSample;		// using 2 unsigneds
//M: use of this method is not portable between architectures.
- (long double)getLongDoubleSample;	// using 2 unsigneds
@end

@deftype SplitSingleSeed
//S: Internal
CREATING
+ create          : aZone
              setA: (unsigned)A 	// # of virtual generators
              setv: (unsigned)v 	// log2(#segments/generator)
              setw: (unsigned)w		// log2(segment length)
  setStateFromSeed: (unsigned)seed;

USING
//M: The setStateFromSeeds method initializes the seed dependent part of the 
//M: state.
- setStateFromSeed: (unsigned)seed;

//M: The getMaxSeedValue method returns the upper limit on the seed value.
- (unsigned)getMaxSeedValue;	// min is 1

//M: The getInitialSeed method returns the generator's starting value.
- (unsigned)getInitialSeed;
@end

@deftype SplitMultiSeed
//S: Internal
CREATING
+ create          : aZone
              setA: (unsigned)A         // # of virtual generators
              setv: (unsigned)v         // log2(#segments/generator)
              setw: (unsigned)w	        // log2(segment length)
 setStateFromSeeds: (unsigned *)seeds;

USING
//M: The setStateFromSeeds method initializes the seed dependent part of the 
//M: state.
- setStateFromSeeds: (unsigned *)seeds;

//M: The lengthOfSeedVector method returns the number of seeds required.
- (unsigned)lengthOfSeedVector;

//M: The getMaxSeedValue method returns the upper limit on the seed value.
- (unsigned *)getMaxSeedValues;		// min is 1

//M: The getInitialSeed method returns the generator's starting value.
- (unsigned *)getInitialSeeds;     	// = getInitialSeeds: 0
@end


@deftype Split
//S: Internal
CREATING
+ createWithDefaults: aZone;

USING
- setAntithetic: (BOOL)antiT;

// Note: Valid values for vGen are [0,getNumGenerators-1]

//M: The getNumGenerators method returns the current number of generators.
- (unsigned)getNumGenerators;

//M: The getNumSegments method returns the current number of segments.
- (unsigned)getNumSegments;

//M: The getSegmentLength method returns the current segment length.
- (unsigned)getSegmentLength;

//M: The getAntithetic method returns the current values of generator
//M: parameters.
- (BOOL)getAntithetic;

//M: The initGenerator method resets the state of a virtual generator to 
//M: segment #0.
- initGenerator: (unsigned)vGen;

//M: The restartGenerator method resets the state of a virtual generator to
//M: the start of the current segment.
- restartGenerator: (unsigned)vGen;

//M: The advanceGenerator method resets the state of a virtual generator to 
//M: the next segment.
- advanceGenerator: (unsigned)vGen;

//M: The jumpGenerator:toSegment: method resets the state of a virtual 
//M: generator to start at the specified segment.
- jumpGenerator: (unsigned)vGen  toSegment: (unsigned long long int)seg;

//M: The initAll method resets the state of all the virtual generators to 
//M: segment #0.
- initAll; 

//M: The restartAll method resets the state of all the virtual generators to
//M: the start of the current segment.
- restartAll;					// start of current segment

//M: The advanceAll method resets the state of all the virtual generators to 
//M: the next segment.
- advanceAll;					// to next segment

//M: The jumpAlltoSegment: method resets the state of all the virtual 
//M: generators to start at the specified segment.
- jumpAllToSegment: (unsigned long long int)seg;

//M: The getCurrentCount method returns the current count of the virtual 
//M: generator.
- (unsigned long long int)getCurrentCount: (unsigned)vGen;

//M: The getCurrentSegment method returns the current segment of the virtual
//M: generator.
- (unsigned long long int)getCurrentSegment: (unsigned)vGen;
@end

@deftype SplitOut
//S: Internal
USING
// Note: Valid values for vGen are [0,getNumGenerators-1]

//M: The getUnsignedMax method returns the maximum value returned by
//M: getUnsignedSample.
- (unsigned)getUnsignedMax;

//M: The getUnsignedSample method returns a 'random' integer uniformly 
//M: distributed over [0,unsignedMax] from the 'virtual generator' 
//M: (data stream) vGen.
- (unsigned)getUnsignedSample: (unsigned)vGen;

//M: The getFloatSample method returns a 'random' floating-point number 
//M: uniformly distributed in [0.0,1.0).
- (float)getFloatSample: (unsigned)vGen; // using 1 unsigned

- (double)getThinDoubleSample: (unsigned)vGen;      // using 1 unsigned
- (double)getDoubleSample: (unsigned)vGen;          // using 2 unsigneds

//M: Warning: use of this method is not portable between architectures.
- (long double)getLongDoubleSample: (unsigned)vGen; // using 2 unsigneds
@end

// 
// ------------------------------------------------------------------------
// Generator types:
//

@deftype SingleShortGenerator <Create, Drop, InternalState, SimpleOut, Simple, SingleSeed>
//S: A Single Short generator.

//D: It has a single seed, a small state (1 integer) and a short cycle 
//D: (less than 2^32).
@end

@deftype SingleLongGenerator <Create, Drop, InternalState, SimpleOut, Simple, SingleSeed, MultiSeed>
//S: A Single Long generator.

//D: It has a single seed, a larger state (several integers) and a longer cycle
//D: (cycle >> 2^32).
@end

@deftype CombinedShortGenerator <Create, Drop, InternalState, SimpleOut, Simple, SingleSeed, MultiSeed>
//S: A Combined Short generator.

//D: A "combined short" generator is a combination of a number of single
//D: short generators.
@end

@deftype CombinedLongGenerator <Create, Drop, InternalState, SimpleOut, Simple, SingleSeed, MultiSeed>
//S: A Combined Long generator

//D: A "combined long" generator is a combination of a number of single 
//D: long generators.
@end

@deftype SingleShortSplitGenerator <Create, Drop, InternalState, SplitOut, Split, SplitSingleSeed>
//S: A Single Short Split generator

//D: A "single short split" generator is a single short generator with 
//D: splitting facilities. 
@end

@deftype SingleLongSplitGenerator <Create, Drop, InternalState, SplitOut, Split, SplitSingleSeed, SplitMultiSeed>
//S: A Single Long Split generator

//D: A "single long split" generator is a single long generator with splitting
//D: facilities. 
@end

@deftype CombinedShortSplitGenerator <Create, Drop, InternalState, SplitOut, Split, SplitSingleSeed, SplitMultiSeed>
//S: A Combined Short Split generator.

//D: A "combined short split" generator is a combination of a number of single
//D: short generators, with splitting facilities.
@end

@deftype CombinedLongSplitGenerator <Create, Drop, InternalState, SplitOut, Split, SplitSingleSeed, SplitMultiSeed>
//S: A Combined Long Split generator.

//D: A "combined long split" generator is a combination of a number of single 
//D: long generators, with splitting facilities.
@end

// 
// NOTE: for the time being, all the split generators implement the
//   *same* protocols, and the non-split (simple) generators likewise.
//   The files methods.simplegenerators.h and methods.splitgenerators.h 
//   in the /docs directory show the resulting combined interfaces for 
//   the two different types of generator.
//


// 
// -----------------------------------------------------------------
// @deftypes for individual generator classes:
//

// LCG[1-3] -- single short random number generators, 
@deftype LCGgen <SingleShortGenerator>
//S: Linear Congruential Generator

//D: This classic generator relies on controlled overflow at 32 bits.
//D: This requires that unsigned be a 32bit value that follows ANSI C rules.
//D: Knuth claims that the adder c does not matter much, as long as it has 
//D: no factors in common with the modulus 2^32.

//D: NOT recommended for serious use; these are included for
//D: historical reasons (compatibility with earlier releases).
@end

@deftype LCG1gen <LCGgen, CREATABLE>
//S: Linear Congruential Generator 1

//D: With the parameters: a = 1,664,525 and c = 1,013,904,223 this generator
//D: has a single full cycle of length m.
@end

@deftype LCG2gen <LCGgen, CREATABLE>
//S: Linear Congruential Generator 2

//D: With the parameters: a = 69,069 and c = 1,013,904,223 this generator
//D: has a single full cycle of length m.
@end

@deftype LCG3gen <LCGgen, CREATABLE>
//S: Linear Congruential Generator 3

//D: With the parameters: a = 1,664,525 and c = 152,193,325 this generator
//D: has a single full cycle of length m.
@end


//
// PMMLCG1, PMMLCG2, PMMLCG3 --
//   single short random number generators recommended for use.
//
// PMMLCG4, PMMLCG5, PMMLCG6, PMMLCG7 --
//   component generators of C4LCGX
//
// PMMLCG8, PMMLCG9 --
//   component generators of C2LCGX

@deftype PMMLCGgen <SingleShortGenerator>
//S: Prime Modulus Multiplicative Linear Congruential Generator

//D: These generator have single full cycle of length (m-1).
@end

@deftype PMMLCG1gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 1

//D: With parameters a = 16,807 and m = 2,147,483,647, this generator has a
//D: single full cycle of length (m-1).
@end

@deftype PMMLCG2gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 2

//D: With parameters a = 48,271 and m = 2,147,483,647, this generator has a
//D: single full cycle of length (m-1).
@end

@deftype PMMLCG3gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 3

//D: With parameters a = 69,621 and m = 2,147,483,647, this generator has a
//D: single full cycle of length (m-1).
@end

@deftype PMMLCG4gen  <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 4

//D: With parameters a = 45,991 and m = 2,147,483,647, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the CLOG4.
@end

@deftype PMMLCG5gen  <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 5

//D: With parameters a = 207,707 and m = 2,147,483,543, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the CLOG4.
@end

@deftype PMMLCG6gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 6

//D: With parameters a = 138,556 and m = 2,147,483,423, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the CLOG4.
@end

@deftype PMMLCG7gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 7

//D: With parameters a = 49,689 and m = 2,147,483,323, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the CLOG4.
@end

@deftype PMMLCG8gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 8

//D: With parameters a = 40,014 and m = 2,147,483,563, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the C2LOGX.
@end

@deftype PMMLCG9gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 9

//D: With parameters a = 40,692 and m = 2,147,483,399, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the C2LOGX.
@end

// ACG, SCG -- single long random number generators

@deftype ACGgen <SingleLongGenerator, CREATABLE>
//S: Additive Congruential Generator

//D: ACG is in the Lagged Fibonacci class of generators. These generators 
//D: use a basic algorithm of the form X_n = f(X_(n-r),X_(n-s)) mod m; r>s
//D: The function f is typically xor, addition, subtraction, multiplication 
//D: or subtraction with carry. It uses simpler math than a basic LCG, but 
//D: keeps a larger state.

//D: NOT recommended for serious use; these are included for
//D: historical reasons (compatibility with earlier releases).
@end

@deftype SCGgen <SingleLongGenerator, CREATABLE>
//S: Subtractive Congruential Generator

//D: SCG is in the Lagged Fibonacci class of generators. These generators use 
//D: a basic algorithm of the form X_n = f(X_(n-r),X_(n-s)) mod m; r>s
//D: The function f is typically xor, addition, subtraction, multiplication 
//D: or subtraction with carry. It uses simpler math than a basic LCG, but 
//D: keeps a larger state.

//D: NOT recommended for serious use; these are included for
//D: historical reasons (compatibility with earlier releases).
@end

// SWB1, SWB2, SWB3 -- single long generators recommended for use.

@deftype SWBgen <SingleLongGenerator>
//S: Subtract-with-borrow Congruential Generator

//D: These generators use a basic algorithm of the form 
//D: X_n = f(X_(n-r),X_(n-s)) mod m; r>s
//D: The function f is typically xor, addition, subtraction, multiplication 
//D: or subtraction with carry. It uses simpler math than a basic LCG, but 
//D: keeps a larger state.
@end

@deftype SWB1gen <SWBgen, CREATABLE>
//S: Subtract-with-borrow Congruential Generator 1

//D: With the parameters r = 37 and s = 24, this generator has 64 cycles of 
//D: length 10^354.
@end

@deftype SWB2gen <SWBgen, CREATABLE>
//S: Subtract-with-borrow Congruential Generator 2

//D: With the parameters r = 24 and s = 19, this generator has 1536 cycles of 
//D: length 10^228.
@end

@deftype SWB3gen <SWBgen, CREATABLE>
//S: Subtract-with-borrow Congruential Generator 3

//D: With the parameters r = 21 and s = 6, this generator has 192 cycles of 
//D: length 10^200.
@end

// PSWB -- single long generator recommended for use.
@deftype PSWBgen   <SingleLongGenerator, CREATABLE>
//S: Subtract-with-borrow Congruential Generator with prime modulus

//D: PSWB is an improvement on SWB in that the use of a prime modulus 
//D: guarantees a single full cycle. It's slower, of course.
@end

// MWC -- two long generators recommended for use.

@deftype MWCAgen <SingleLongGenerator, CREATABLE>
//S: Multiply-With-Carry generator

//D: This generator is claimed to be strictly periodic, with a period > 2^59. 
//D: (There's possibly two such cycles.)
@end

@deftype MWCBgen <SingleLongGenerator, CREATABLE>
//S: Multiply-With-Carry generator

//D: This generator implements an alternate manner of conjoining the two 
//D: components (differs from MWCA). This generator is claimed to be strictly 
//D: periodic, with a period > 2^59. (There's possibly two such cycles.)
@end

// RWC2 -- single long generator recommended for use.
@deftype RWC2gen <SingleLongGenerator, CREATABLE>
//S: 2-lag Recursion With Carry generator

//D: This generator is a 2-lag MWC generator implemented using 64-bit math.
@end

// RWC8 ("Mother") -- single long generator recommended for use.
@deftype RWC8gen <CombinedLongGenerator, CREATABLE>
//S: Multiply With Carry generator ("The Mother of all RNG's")

//D: This generator is a combination of 2 16-bit 8-lag Recursion-With-Carry 
//D: generators. 
@end

// TT403, TT775, TT800 -- single long generators recommended for use.
@deftype TGFSRgen <SingleLongGenerator>
//S: Twisted GFSR generator

//D: With properly chosen parameters, these generators have a single cycle 
//D: of length 2^(w*N) -1.
@end

@deftype TT403gen <TGFSRgen, CREATABLE>
//S: A single long generator recommended for use.

//D: A single long generator recommended for use.
@end

@deftype TT775gen <TGFSRgen, CREATABLE>
//S: A single long generator recommended for use.

//D: A single long generator recommended for use.
@end

@deftype TT800gen <TGFSRgen, CREATABLE>
//S: A single long generator recommended for use.

//D: A single long generator recommended for use.
@end

// MT19937 -- single *very* long generator recommended for use.
@deftype MT19937gen <SingleLongGenerator, CREATABLE>
//S: 'Mersenne Twister' Twisted GFSR generator

//D:  This generator has a single cycle of length 2^19937-1.
@end

// MRG5, MRG6, MRG7 -- single long generators recommended for use.
@deftype MRGgen <SingleLongGenerator>
//S: Multiple Recursive [LCG] Generator

//D: These generators require k multipliers and k past values to be kept. 
//D: In their paper, the authors investigate MRG's of order k from 1 to 7. 
//D: They provide several sets of parameters which they recommend out of a 
//D: large number that were tested. Generally, the quality of the generators
//D: increases with k.
@end

@deftype MRG5gen <MRGgen, CREATABLE>
//S: Multiple Recursive [LCG] Generator 5

//D: This generator has a single full cycle of length (2^31-1)^5 - 1,
//D: i.e. 2^154 < cycle < 2^155.
@end

@deftype MRG6gen <MRGgen, CREATABLE>
//S: Multiple Recursive [LCG] Generator 6

//D: This generator has a single full cycle of length (2^31-1)^6 - 1,
//D: i.e. 2^185 < cycle < 2^186.
@end

@deftype MRG7gen <MRGgen, CREATABLE>
//S: Multiple Recursive [LCG] Generator 7

//D: This generator has a single full cycle of length (2^31-1)^7 - 1,
//D: i.e. 2^216 < cycle < 2^217.
@end

// C2TAUS[1-3]: short component based generator recommended for use.
@deftype C2TAUSgen <CombinedShortGenerator>
//S: Combined Tausworthe generator 

//D: This generator is based on 2 component generators of periods 2^31-1 and 
//D: 2^29-1.
@end

@deftype C2TAUS1gen <C2TAUSgen, CREATABLE>
//S: Combined Tausworthe generator 1

//D: Component 1 parameters: P = 31, S = 12, Q = 13
//D: Component 2 parameters: P = 29, S = 17, Q =  2
//D: With these parameters, this generator has a single full cycle of 
//D: length ~ 2^60.
@end

@deftype C2TAUS2gen <C2TAUSgen, CREATABLE>
//S: Combined Tausworthe generator 2

//D: Component 1 parameters: P = 31, S = 21, Q =  3
//D: Component 2 parameters: P = 29, S = 17, Q =  2
//D: With these parameters, this generator has a single full cycle of 
//D: length ~ 2^60.
@end

@deftype C2TAUS3gen <C2TAUSgen, CREATABLE>
//S: Combined Tausworthe generator 3

//D: Component 1 parameters: P = 31, S = 13, Q = 13
//D: Component 2 parameters: P = 29, S = 20, Q =  2
//D: With these parameters, this generator has a single full cycle of 
//D: length ~ 2^60.
@end

// C2MRG3 -- long component based generator recommended for use.
@deftype C2MRG3gen <CombinedLongGenerator, CREATABLE>
//S: Combined Multiple Recursive Generator.  A combination of 2 multiple
//S: recursive LCG generators.

//D: Combinations of like generators are shown to have better statistical 
//D: properties than single generators. The components of this generator 
//D: each has two nonzero multipliers (and one that's zero). They use 
//D: different moduli (2^31-1, 2145483479.)
@end

// C3MWC -- long component based generator recommended for use.
@deftype C3MWCgen <CombinedLongGenerator, CREATABLE>
//S: Combined Multiply With Carry generator

//D: This generator is a combination of 3 MWC generators, each of which is 
//D: a combination of 2 16-bit Multiply-With-Carry generators. 
@end

// 
// XXX --
//   single short generator with splitting facilities.
//
//   (no generators in this class implemented.)
//
// @deftype XXXgen <SingleShortSplitGenerator, CREATABLE> @end

// 
// XXX --
//   single long generator with splitting facilities.
//
//   (no generators in this class implemented.)
//
// @deftype XXXgen <SingleLongSplitGenerator, CREATABLE> @end

@deftype C2LCGXgen <CombinedShortSplitGenerator, CREATABLE>
//S: A short component based generator with splitting facilities. Recommended.
//S: This combined random generator uses 2 (PMM)LGC generators.

//D: This portable generator is based on a backbone generator which is a 
//D: combination of 2 (PMM)LCG generators. It has a period length of almost 
//D: 2^61 (2.3e18). The backbone generator's period can be split up into a
//D: number of 'virtual generators' (A), each of which can be set to access 
//D: a number of 'segments' (V) of length W, subject to the constraint that 
//D: A * V * W <= 2^60.
@end

// C4LCGX -- recommended short component based generator with splitting
@deftype C4LCGXgen  <CombinedShortSplitGenerator, CREATABLE>
//S: Combined random generator using 4 (PMM)LGC generators.

//D:  This portable generator is based on a backbone generator which is a 
//D: combination of 4 (PMM)LCG generators. It has a period length of 
//D: (m1-1)(m2-1)(m3-1)(m4-1) / 8, or almost 2^121 (2.6e36).
@end

// 
// XXX --
//   long component based generator with splitting facilities.
//
//   (no generators in this class implemented.)
//
// @deftype XXXgen <CombinedLongSplitGenerator, CREATABLE> @end

// 
// ---------------------------------------------------------------
// @class definitions for implemented generators:
// 
//   ( Note: the @class names must be the same as the 
//     @deftype names given to generators specified as
//     CREATABLE. Otherwise the magic may not work!    )
//

// <SingleShortGenerator>

@class LCG1gen;
@class LCG2gen;
@class LCG3gen;
@class PMMLCG1gen;
@class PMMLCG2gen;
@class PMMLCG3gen;
@class PMMLCG4gen;
@class PMMLCG5gen;
@class PMMLCG6gen;
@class PMMLCG7gen;
@class PMMLCG8gen;
@class PMMLCG9gen;

// <SingleLongGenerator>

@class ACGgen;
@class SCGgen;
@class SWB1gen;
@class SWB2gen;
@class SWB3gen;
@class PSWBgen;
@class TT403gen;
@class TT775gen;
@class TT800gen;
@class MT19937gen;
@class MRG5gen;
@class MRG6gen;
@class MRG7gen;
@class MWCAgen;
@class MWCBgen;
@class RWC2gen;
@class RWC8gen;

// <CombinedShortGenerator>

@class C2TAUS1gen;
@class C2TAUS2gen;
@class C2TAUS3gen;

// <CombinedLongGenerator>

@class C2MRG3gen;

@class C3MWCgen;

// <SingleShortSplitGenerator>

//   (none)

// <SingleLongSplitGenerator>

//   (none)

// <CombinedShortSplitGenerator>

@class C2LCGXgen;
@class C4LCGXgen;

// <CombinedLongSplitGenerator>

//   (none)

