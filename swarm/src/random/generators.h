//
// <random/generators.h>
//
//     1997-09-01 (v. 0.7)
//     1998-10-08 (v. 0.8)
//     2000-02-21 (v. 0.81)
//

// 
// See the file docs/README.Generators for guide to usage
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
// ---------------------------------------------------------------------
// Protocol components:
//

@protocol CommonGenerator
//S: Internal
CREATING
+ createWithDefaults: (id <Zone>)aZone;

SETTING
//M: The setStateFromSeed method initializes the seed dependent part of the 
//M: state from a single seed value.
- setStateFromSeed: (unsigned)seed;

//M: The setStateFromSeeds method initializes the seed dependent part of the 
//M: state from a vector of seed values.
- setStateFromSeeds: (unsigned *)seeds;

//M: The setAntithetic method turns on or off antithetic output (default=off).
//M: Antithetic output is (unsignedMax - u) or (1.0 - d).
- setAntithetic: (BOOL) antiT;

USING
//M: The getAntithetic method returns the current values of the parameter.
- (BOOL)getAntithetic;

//M: The getMaxSeedValue method returns the upper limit on the seed value
//M: that can be supplied.
- (unsigned)getMaxSeedValue;	// minvalue is 1

//M: The getMaxSeedValues method returns a vector of upper limits on the 
//M: seed values that can be supplied.
- (unsigned *)getMaxSeedValues;		// minvalue is 1

//M: The getInitialSeed method returns the value of the generator's 
//M: starting seed.
- (unsigned)getInitialSeed;

//M: The getInitialSeeds method returns a vector of the generator's 
//M: starting seed values.
- (unsigned *)getInitialSeeds;

//M: The lengthOfSeedVector method returns the number of seeds required
//M: if you wish to set the state directly.
- (unsigned)lengthOfSeedVector;

//M: The -reset method sets the generator back to the state it had at start
//M: or at the last use of -setStateFromSeed(s). CurrentCount is zeroed.
- reset;

//M: The getUnsignedMax method returns the highest value that will ever 
//M: be returned by -getUnsignedSample (the lowest is 0).
- (unsigned)getUnsignedMax;
@end

@protocol SimpleGenerator
//S: Internal
CREATING
+ create: (id <Zone>)aZone setStateFromSeed: (unsigned)seed;

+ create: (id <Zone>)aZone setStateFromSeeds: (unsigned *)seeds;

USING
//M: The getUnsignedSample method returns a random unsigned integer 
//M: uniformly distributed over [0,unsignedMax].
- (unsigned)getUnsignedSample;

//M: The getFloatSample method returns a random floating point number
//M: of size float, uniformly distributed in the range [0.0, 1.0).
//M: It uses 1 call to -getUnsignedSample to fill the mantissa.
- (float)getFloatSample;

//M: The getThinDoubleSample method returns a random floating point number
//M: of size double, uniformly distributed in the range [0.0, 1.0).
//M: It uses 1 call to -getUnsignedSample to fill the mantissa.
- (double)getThinDoubleSample;

//M: The getDoubleSample method returns a random floating point number
//M: of size double, uniformly distributed in the range [0.0, 1.0).
//M: It uses 2 calls to -getUnsignedSample to fill the mantissa.
- (double)getDoubleSample;

//M: The getLongDoubleSample method returns a random floating point number
//M: of size long double, uniformly distributed in the range [0.0, 1.0).
//M: It uses 2 calls to -getUnsignedSample to fill the mantissa.
//M: Note: use of this method is not portable between architectures.
- (long double)getLongDoubleSample;

//M: The getCurrentCount method returns the count of variates generated.
- (unsigned long long int)getCurrentCount;
@end

@protocol SplitGenerator
//S: Internal
CREATING
+ create          : (id <Zone>)aZone
              setA: (unsigned)A 	// # of virtual generators
              setV: (unsigned)v 	// log2(#segments/generator)
              setW: (unsigned)w		// log2(segment length)
  setStateFromSeed: (unsigned)seed;

+ create          : (id <Zone>)aZone
              setA: (unsigned)A         // # of virtual generators
              setV: (unsigned)v         // log2(#segments/generator)
              setW: (unsigned)w	        // log2(segment length)
 setStateFromSeeds: (unsigned *)seeds;

SETTING
//M: The initGenerator method resets the state of a virtual generator to 
//M: the start of segment #0.
- initGenerator: (unsigned)vGen;

//M: The initAll method resets the state of all the virtual generators to 
//M: the start of segment #0.
- initAll; 

USING
// Note: Valid values for vGen are [0,getNumGenerators-1]

//M: The getNumGenerators method returns the current number of 
//M: virtual generators (A).
- (unsigned)getNumGenerators;

//M: The getNumSegments method returns log2(the current number of 
//M: segments) = v.
- (unsigned)getNumSegments;

//M: The getSegmentLength method returns log2(the current segment 
//M: length) = w.
- (unsigned)getSegmentLength;

//M: The restartGenerator method resets the state of a virtual generator to
//M: the start of the current segment.
- restartGenerator: (unsigned)vGen;

//M: The advanceGenerator method resets the state of a virtual generator to 
//M: the start of the next segment.
- advanceGenerator: (unsigned)vGen;

//M: The jumpGenerator:toSegment: method resets the state of a virtual 
//M: generator to the start of the specified segment.
- jumpGenerator: (unsigned)vGen  toSegment: (unsigned long long int)seg;

//M: The restartAll method resets the state of all the virtual generators to
//M: the start of their current segment.
- restartAll;					// start of current segment

//M: The advanceAll method resets the state of all the virtual generators to 
//M: the start of their next segment.
- advanceAll;					// to next segment

//M: The jumpAlltoSegment: method resets the state of all the virtual 
//M: generators to the start of the specified segment.
- jumpAllToSegment: (unsigned long long int)seg;

//M: The getUnsignedSample method returns a random unsigned integer 
//M: uniformly distributed over the interval [0,unsignedMax] 
//M: from virtual generator (data stream) vGen.
- (unsigned)getUnsignedSample: (unsigned)vGen;

//M: The getFloatSample method returns a random floating-point number 
//M: of size float, uniformly distributed in the range [0.0,1.0),
//M: from virtual generator (data stream) vGen.
//M: This method uses 1 call to -getUnsignedSample to fill the mantissa.
- (float)getFloatSample: (unsigned)vGen; // using 1 unsigned

//M: The getThinDoubleSample method returns a random floating-point number 
//M: of size double, uniformly distributed in the range [0.0,1.0),
//M: from virtual generator (data stream) vGen.
//M: This method uses 1 call to -getUnsignedSample to fill the mantissa.
- (double)getThinDoubleSample: (unsigned)vGen;      // using 1 unsigned

//M: The getDoubleSample method returns a random floating-point number 
//M: of size double, uniformly distributed in the range [0.0,1.0),
//M: from virtual generator (data stream) vGen.
//M: This method uses 2 calls to -getUnsignedSample to fill the mantissa.
- (double)getDoubleSample: (unsigned)vGen;          // using 2 unsigneds

//M: The getLongDoubleSample method returns a random floating-point number 
//M: of size long double, uniformly distributed in the range [0.0,1.0),
//M: from virtual generator (data stream) vGen.
//M: This method uses 2 calls to -getUnsignedSample to fill the mantissa.
//M: Warning: use of this method is not portable between architectures.
- (long double)getLongDoubleSample: (unsigned)vGen; // using 2 unsigneds

//M: The getCurrentSegment method returns the number of the current segment 
//M: of the specified virtual generator.
- (unsigned long long int)getCurrentSegment: (unsigned)vGen;

//M: The getCurrentCount method returns the current count of the specified
//M: virtual generator (i.e. the number of variates delivered).
- (unsigned long long int)getCurrentCount: (unsigned)vGen;
@end

// 
// ------------------------------------------------------------------------
// Implemented generator types:
//

// 
// NOTE: all the split generators implement the same protocols,
//   and likewise for the non-split (simple) generators.
//

@protocol BasicRandomGenerator <SwarmObject, InternalState, CommonGenerator>
//S: The common functionality of simple and split generators.

//D: This protocol covers methods common to simple and split generators.
@end

@protocol SimpleRandomGenerator <BasicRandomGenerator, SimpleGenerator>
// <SwarmObject, InternalState, SimpleOut, Simple, SingleSeed, MultiSeed>
//S: A Simple (non-split) generator.

//D: This protocol covers all implemented non-split generators.
@end

@protocol SplitRandomGenerator <BasicRandomGenerator, SplitGenerator>
// <SwarmObject, InternalState, SplitOut, Split, 
// SplitSingleSeed, SplitMultiSeed>
//S: A split generator.

//D: This protocol covers the implemented split generators
//D: (C2LCGX and C4LCGX.)
@end

// 
// -----------------------------------------------------------------
// @protocols for individual generator classes:
//

// NOTE: these protocols are included for backward compatibility only.
// Use protocols <SimpleRandomGenerator> and <SplitRandomGenerator>.

// LCG[1-3] -- single short random number generators, 
@protocol LCGgen <SimpleRandomGenerator>
//S: Linear Congruential Generator

//D: This classic generator relies on controlled overflow at 32 bits.
//D: This requires that unsigned be a 32bit value that follows ANSI C rules.
//D: Knuth claims that the adder c does not matter much, as long as it has 
//D: no factors in common with the modulus 2^32.

//D: NOT recommended for serious use; these are included for
//D: historical reasons (compatibility with earlier releases).
@end

@protocol LCG1gen <LCGgen, CREATABLE>
//S: Linear Congruential Generator 1

//D: With the parameters: a = 1,664,525 and c = 1,013,904,223 this generator
//D: has a single full cycle of length m.
@end

@protocol LCG2gen <LCGgen, CREATABLE>
//S: Linear Congruential Generator 2

//D: With the parameters: a = 69,069 and c = 1,013,904,223 this generator
//D: has a single full cycle of length m.
@end

@protocol LCG3gen <LCGgen, CREATABLE>
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

@protocol PMMLCGgen <SimpleRandomGenerator>
//S: Prime Modulus Multiplicative Linear Congruential Generator

//D: These generator have single full cycle of length (m-1).
@end

@protocol PMMLCG1gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 1

//D: With parameters a = 16,807 and m = 2,147,483,647, this generator has a
//D: single full cycle of length (m-1).
@end

@protocol PMMLCG2gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 2

//D: With parameters a = 48,271 and m = 2,147,483,647, this generator has a
//D: single full cycle of length (m-1).
@end

@protocol PMMLCG3gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 3

//D: With parameters a = 69,621 and m = 2,147,483,647, this generator has a
//D: single full cycle of length (m-1).
@end

@protocol PMMLCG4gen  <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 4

//D: With parameters a = 45,991 and m = 2,147,483,647, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the CLOG4.
@end

@protocol PMMLCG5gen  <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 5

//D: With parameters a = 207,707 and m = 2,147,483,543, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the CLOG4.
@end

@protocol PMMLCG6gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 6

//D: With parameters a = 138,556 and m = 2,147,483,423, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the CLOG4.
@end

@protocol PMMLCG7gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 7

//D: With parameters a = 49,689 and m = 2,147,483,323, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the CLOG4.
@end

@protocol PMMLCG8gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 8

//D: With parameters a = 40,014 and m = 2,147,483,563, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the C2LOGX.
@end

@protocol PMMLCG9gen <PMMLCGgen, CREATABLE>
//S: Prime Modulus Multiplicative Linear Congruential Generator 9

//D: With parameters a = 40,692 and m = 2,147,483,399, this generator has a
//D: single full cycle of length (m-1). This is one of the component generators
//D: of the C2LOGX.
@end

// ACG, SCG -- single long random number generators

@protocol ACGgen <SimpleRandomGenerator, CREATABLE>
//S: Additive Congruential Generator

//D: ACG is in the Lagged Fibonacci class of generators. These generators 
//D: use a basic algorithm of the form X_n = f(X_(n-r),X_(n-s)) mod m; r>s
//D: The function f is typically xor, addition, subtraction, multiplication 
//D: or subtraction with carry. It uses simpler math than a basic LCG, but 
//D: keeps a larger state.

//D: NOT recommended for serious use; these are included for
//D: historical reasons (compatibility with earlier releases).
@end

@protocol SCGgen <SimpleRandomGenerator, CREATABLE>
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

@protocol SWBgen <SimpleRandomGenerator>
//S: Subtract-with-borrow Congruential Generator

//D: These generators use a basic algorithm of the form 
//D: X_n = f(X_(n-r),X_(n-s)) mod m; r>s
//D: The function f is typically xor, addition, subtraction, multiplication 
//D: or subtraction with carry. It uses simpler math than a basic LCG, but 
//D: keeps a larger state.
@end

@protocol SWB1gen <SWBgen, CREATABLE>
//S: Subtract-with-borrow Congruential Generator 1

//D: With the parameters r = 37 and s = 24, this generator has 64 cycles of 
//D: length 10^354.
@end

@protocol SWB2gen <SWBgen, CREATABLE>
//S: Subtract-with-borrow Congruential Generator 2

//D: With the parameters r = 24 and s = 19, this generator has 1536 cycles of 
//D: length 10^228.
@end

@protocol SWB3gen <SWBgen, CREATABLE>
//S: Subtract-with-borrow Congruential Generator 3

//D: With the parameters r = 21 and s = 6, this generator has 192 cycles of 
//D: length 10^200.
@end

// PSWB -- single long generator recommended for use.
@protocol PSWBgen   <SimpleRandomGenerator, CREATABLE>
//S: Subtract-with-borrow Congruential Generator with prime modulus

//D: PSWB is an improvement on SWB in that the use of a prime modulus 
//D: guarantees a single full cycle. It's slower, of course.
@end

// MWC -- two long generators recommended for use.

@protocol MWCAgen <SimpleRandomGenerator, CREATABLE>
//S: Multiply-With-Carry generator

//D: This generator is claimed to be strictly periodic, with a period > 2^59. 
//D: (There's possibly two such cycles.)
@end

@protocol MWCBgen <SimpleRandomGenerator, CREATABLE>
//S: Multiply-With-Carry generator

//D: This generator implements an alternate manner of conjoining the two 
//D: components (differs from MWCA). This generator is claimed to be strictly 
//D: periodic, with a period > 2^59. (There's possibly two such cycles.)
@end

// RWC2 -- single long generator recommended for use.
@protocol RWC2gen <SimpleRandomGenerator, CREATABLE>
//S: 2-lag Recursion With Carry generator

//D: This generator is a 2-lag MWC generator implemented using 64-bit math.
@end

// RWC8 ("Mother") -- single long generator recommended for use.
@protocol RWC8gen <SimpleRandomGenerator, CREATABLE>
//S: Multiply With Carry generator ("The Mother of all RNG's")

//D: This generator is a combination of 2 16-bit 8-lag Recursion-With-Carry 
//D: generators. 
@end

// TT403, TT775, TT800 -- single long generators recommended for use.
@protocol TGFSRgen <SimpleRandomGenerator>
//S: Twisted GFSR generator

//D: With properly chosen parameters, these generators have a single cycle 
//D: of length 2^(w*N) -1.
@end

@protocol TT403gen <TGFSRgen, CREATABLE>
//S: A single long generator recommended for use.

//D: A single long generator recommended for use.
@end

@protocol TT775gen <TGFSRgen, CREATABLE>
//S: A single long generator recommended for use.

//D: A single long generator recommended for use.
@end

@protocol TT800gen <TGFSRgen, CREATABLE>
//S: A single long generator recommended for use.

//D: A single long generator recommended for use.
@end

// MT19937 -- single *very* long generator recommended for use.
@protocol MT19937gen <SimpleRandomGenerator, CREATABLE>
//S: 'Mersenne Twister' Twisted GFSR generator

//D:  This generator has a single cycle of length 2^19937-1.
@end

// MRG5, MRG6, MRG7 -- single long generators recommended for use.
@protocol MRGgen <SimpleRandomGenerator>
//S: Multiple Recursive [LCG] Generator

//D: These generators require k multipliers and k past values to be kept. 
//D: In their paper, the authors investigate MRG's of order k from 1 to 7. 
//D: They provide several sets of parameters which they recommend out of a 
//D: large number that were tested. Generally, the quality of the generators
//D: increases with k.
@end

@protocol MRG5gen <MRGgen, CREATABLE>
//S: Multiple Recursive [LCG] Generator 5

//D: This generator has a single full cycle of length (2^31-1)^5 - 1,
//D: i.e. 2^154 < cycle < 2^155.
@end

@protocol MRG6gen <MRGgen, CREATABLE>
//S: Multiple Recursive [LCG] Generator 6

//D: This generator has a single full cycle of length (2^31-1)^6 - 1,
//D: i.e. 2^185 < cycle < 2^186.
@end

@protocol MRG7gen <MRGgen, CREATABLE>
//S: Multiple Recursive [LCG] Generator 7

//D: This generator has a single full cycle of length (2^31-1)^7 - 1,
//D: i.e. 2^216 < cycle < 2^217.
@end

// C2TAUS[1-3]: short component based generator recommended for use.
@protocol C2TAUSgen <SimpleRandomGenerator>
//S: Combined Tausworthe generator 

//D: This generator is based on 2 component generators of periods 2^31-1 and 
//D: 2^29-1.
@end

@protocol C2TAUS1gen <C2TAUSgen, CREATABLE>
//S: Combined Tausworthe generator 1

//D: Component 1 parameters: P = 31, S = 12, Q = 13
//D: Component 2 parameters: P = 29, S = 17, Q =  2
//D: With these parameters, this generator has a single full cycle of 
//D: length ~ 2^60.
@end

@protocol C2TAUS2gen <C2TAUSgen, CREATABLE>
//S: Combined Tausworthe generator 2

//D: Component 1 parameters: P = 31, S = 21, Q =  3
//D: Component 2 parameters: P = 29, S = 17, Q =  2
//D: With these parameters, this generator has a single full cycle of 
//D: length ~ 2^60.
@end

@protocol C2TAUS3gen <C2TAUSgen, CREATABLE>
//S: Combined Tausworthe generator 3

//D: Component 1 parameters: P = 31, S = 13, Q = 13
//D: Component 2 parameters: P = 29, S = 20, Q =  2
//D: With these parameters, this generator has a single full cycle of 
//D: length ~ 2^60.
@end

// C2MRG3 -- long component based generator recommended for use.
@protocol C2MRG3gen <SimpleRandomGenerator, CREATABLE>
//S: Combined Multiple Recursive Generator.  A combination of 2 multiple
//S: recursive LCG generators.

//D: Combinations of like generators are shown to have better statistical 
//D: properties than single generators. The components of this generator 
//D: each has two nonzero multipliers (and one that's zero). They use 
//D: different moduli (2^31-1, 2145483479.)
@end

// C3MWC -- long component based generator recommended for use.
@protocol C3MWCgen <SimpleRandomGenerator, CREATABLE>
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
// @protocol XXXgen <SingleShortSplitGenerator, CREATABLE> @end

// 
// YYY --
//   single long generator with splitting facilities.
//
//   (no generators in this class implemented.)
//
// @protocol YYYgen <SingleLongSplitGenerator, CREATABLE> @end

@protocol C2LCGXgen <SplitRandomGenerator, CREATABLE>
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
@protocol C4LCGXgen  <SplitRandomGenerator, CREATABLE>
//S: Combined random generator using 4 (PMM)LGC generators.

//D:  This portable generator is based on a backbone generator which is a 
//D: combination of 4 (PMM)LCG generators. It has a period length of 
//D: (m1-1)(m2-1)(m3-1)(m4-1) / 8, or almost 2^121 (2.6e36).
@end

// 
// ZZZ --
//   long component based generator with splitting facilities.
//
//   (no generators in this class implemented.)
//
// @protocol ZZZgen <CombinedLongSplitGenerator, CREATABLE> @end

// 
// ---------------------------------------------------------------
// @class definitions for implemented generators:
// 

// ----- <SimpleRandomGenerator> -----

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

// ----- <SplitRandomGenerator> -----

// <SingleShortSplitGenerator>

//   (none)

// <SingleLongSplitGenerator>

//   (none)

// <CombinedShortSplitGenerator>

@class C2LCGXgen;
@class C4LCGXgen;

// <CombinedLongSplitGenerator>

//   (none)
