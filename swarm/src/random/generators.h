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
CREATING
+ 		create: aZone setStateFromSeed: (unsigned) seed;
USING
-		setStateFromSeed:  (unsigned)   seed;

// Limits on the seed value supplied:

- (unsigned)	getMaxSeedValue;	// minvalue is 1

// Return generator starting value(s):

- (unsigned) 	getInitialSeed;

@end


@deftype MultiSeed
CREATING
+		create: aZone setStateFromSeeds: (unsigned *) seeds;
USING
-		setStateFromSeeds: (unsigned *) seeds;

// Number of seeds required (size of array):

- (unsigned) 	lengthOfSeedVector;

// Limits on seed values that can be supplied:

- (unsigned *)	getMaxSeedValues;		// minvalue is 1

// Return generator starting values:

- (unsigned *) 	getInitialSeeds;

@end


@deftype Simple
CREATING
+		createWithDefaults: aZone;
USING
-		setAntithetic: (BOOL) antiT;

// Return current values of generator parameters:

- (BOOL)	getAntithetic;

// Return generator state value:

- (unsigned long long int)	getCurrentCount;

@end


@deftype SimpleOut
USING

// The maximum value returned by getUnsignedSample is:

- (unsigned)    getUnsignedMax;

// Return a 'random' integer uniformly distributed over [0,unsignedMax]:

- (unsigned)	getUnsignedSample;

// Return a 'random' floating-point number uniformly distributed in [0.0,1.0):

- (float)       getFloatSample;			// using 1 unsigned
- (double)      getThinDoubleSample;		// using 1 unsigned
- (double)      getDoubleSample;		// using 2 unsigneds
- (long double) getLongDoubleSample;		// using 2 unsigneds

// Warning: use of the last method is not portable between architectures.

@end

// 
// =====
// 

@deftype SplitSingleSeed
CREATING
+		create: aZone
		   setA: (unsigned) A 		// # of virtual generators
		   setv: (unsigned) v 		// log2(#segments/generator)
		   setw: (unsigned) w		// log2(segment length)
		setStateFromSeed: (unsigned) seed;
USING
-		setStateFromSeed: (unsigned) seed;

// Limits on the seed value supplied:

- (unsigned)	getMaxSeedValue;	// min is 1

// Return generator starting value(s):

- (unsigned)	getInitialSeed;

@end

@deftype SplitMultiSeed
CREATING
+		create: aZone
		   setA: (unsigned) A 		// # of virtual generators
		   setv: (unsigned) v 		// log2(#segments/generator)
		   setw: (unsigned) w		// log2(segment length)
		setStateFromSeeds: (unsigned *) seeds;
USING
-		setStateFromSeeds: (unsigned *) seeds;

// Number of seeds required (size of array):

- (unsigned) 	lengthOfSeedVector;

// Limits on seed values that can be supplied:

- (unsigned *)	getMaxSeedValues;			// min is 1

// Return generator starting value(s):

- (unsigned *)	getInitialSeeds;			// = getInitialSeeds: 0

@end


@deftype Split
CREATING
+		createWithDefaults: aZone;
USING
-		setAntithetic: (BOOL) antiT;

// Note: Valid values for vGen are [0,getNumGenerators-1]

// Return current values of generator parameters:

- (unsigned)    getNumGenerators;		// returns A
- (unsigned)    getNumSegments;			// returns v
- (unsigned)    getSegmentLength;		// returns w

- (BOOL)	getAntithetic;

// Reset the state of a virtual generator to the start of a specified segment:

-initGenerator:    (unsigned) vGen;		// to segment #0
-restartGenerator: (unsigned) vGen;		// start of current segment
-advanceGenerator: (unsigned) vGen;		// to next segment
-jumpGenerator:    (unsigned) vGen  toSegment: (unsigned long long int) seg;

// Reset the state of all virtual generators to the start of a given segment:

-initAll;					// to segment #0
-restartAll;					// start of current segment
-advanceAll;					// to next segment
-jumpAllToSegment: (unsigned long long int) seg;

// Return virtual generator state value:

- (unsigned long long int) getCurrentCount:   (unsigned) vGen;
- (unsigned long long int) getCurrentSegment: (unsigned) vGen;

@end

@deftype SplitOut
USING
// Note: Valid values for vGen are [0,getNumGenerators-1]

// The maximum value returned by getUnsignedSample is:

- (unsigned)    getUnsignedMax;

// Return a 'random' integer uniformly distributed over [0,unsignedMax]
//   from 'virtual generator' (data stream) vGen:

- (unsigned)	getUnsignedSample:   (unsigned) vGen;

// Return a 'random' floating-point number uniformly distributed in [0.0,1.0):

- (float)       getFloatSample:      (unsigned) vGen;	// using 1 unsigned
- (double)      getThinDoubleSample: (unsigned) vGen;	// using 1 unsigned
- (double)      getDoubleSample:     (unsigned) vGen;	// using 2 unsigneds
- (long double) getLongDoubleSample: (unsigned) vGen;	// using 2 unsigneds

// Warning: use of the last method is not portable between architectures.

@end


// 
// ------------------------------------------------------------------------
// Generator types:
//

// A "single short" generator is just that.
// 
@deftype SingleShortGenerator
<Create, Drop, InternalState, SimpleOut, Simple, SingleSeed>
@end

// A "single long" generator is just that.
// 
@deftype SingleLongGenerator 
<Create, Drop, InternalState, SimpleOut, Simple, SingleSeed, MultiSeed>
@end

// A "combined short" generator is a combination of a number of single
//   short generators.
//
@deftype CombinedShortGenerator 
<Create, Drop, InternalState, SimpleOut, Simple, SingleSeed, MultiSeed>
@end

// A "combined long" generator is a combination of a number of single 
//   long generators.
// 
@deftype CombinedLongGenerator 
<Create, Drop, InternalState, SimpleOut, Simple, SingleSeed, MultiSeed>
@end

// A "single short split" generator is a single short generator with splitting
//   facilities. Such a generator would not be very useful, and none has been
//   implemented. The category is there for completeness only.
// 
@deftype SingleShortSplitGenerator 
<Create, Drop, InternalState, SplitOut, Split, SplitSingleSeed>
@end

// A "single long split" generator is a single long generator with splitting
//   facilities. 
//
@deftype SingleLongSplitGenerator 
<Create, Drop, InternalState, SplitOut, Split, SplitSingleSeed, SplitMultiSeed>
@end

// A "combined short split" generator is a combination of a number of single
//   short generators, with splitting facilities.
// 
@deftype CombinedShortSplitGenerator 
<Create, Drop, InternalState, SplitOut, Split, SplitSingleSeed, SplitMultiSeed>
@end

// A "combined long split" generator is a combination of a number of single 
//   long generators, with splitting facilities.
// 
@deftype CombinedLongSplitGenerator  
<Create, Drop, InternalState, SplitOut, Split, SplitSingleSeed, SplitMultiSeed>
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

//
// LCG1, LCG2, LCG3 --
//   single short random number generators, 
//   NOT recommended for serious use; these are included for
//   historical reasons (compatibility with earlier releases).
// 
@deftype LCGgen      <SingleShortGenerator> @end

@deftype LCG1gen        <LCGgen, CREATABLE> @end
@deftype LCG2gen        <LCGgen, CREATABLE> @end
@deftype LCG3gen        <LCGgen, CREATABLE> @end


//
// PMMLCG1, PMMLCG2, PMMLCG3 --
//   single short random number generators recommended for use.
//
// PMMLCG4, PMMLCG5, PMMLCG6, PMMLCG7 --
//   component generators of C4LCGX
//
// PMMLCG8, PMMLCG9 --
//   component generators of C2LCGX
//
@deftype PMMLCGgen   <SingleShortGenerator> @end

@deftype PMMLCG1gen  <PMMLCGgen, CREATABLE> @end
@deftype PMMLCG2gen  <PMMLCGgen, CREATABLE> @end
@deftype PMMLCG3gen  <PMMLCGgen, CREATABLE> @end
@deftype PMMLCG4gen  <PMMLCGgen, CREATABLE> @end
@deftype PMMLCG5gen  <PMMLCGgen, CREATABLE> @end
@deftype PMMLCG6gen  <PMMLCGgen, CREATABLE> @end
@deftype PMMLCG7gen  <PMMLCGgen, CREATABLE> @end
@deftype PMMLCG8gen  <PMMLCGgen, CREATABLE> @end
@deftype PMMLCG9gen  <PMMLCGgen, CREATABLE> @end

//
// ACG, SCG --
//   single long random number generators, 
//   NOT recommended for serious use; these are included for
//   historical reasons (compatibility with earlier releases).
// 
@deftype ACGgen      <SingleLongGenerator, CREATABLE> @end
@deftype SCGgen      <SingleLongGenerator, CREATABLE> @end

//
// SWB1, SWB2, SWB3 --
//   single long generators recommended for use.
//
@deftype SWBgen   <SingleLongGenerator> @end

@deftype SWB1gen    <SWBgen, CREATABLE> @end
@deftype SWB2gen    <SWBgen, CREATABLE> @end
@deftype SWB3gen    <SWBgen, CREATABLE> @end

// 
// PSWB --
//   single long generator recommended for use.
//
@deftype PSWBgen   <SingleLongGenerator, CREATABLE> @end

// 
// MWC --
//   two long generators recommended for use.
// 
@deftype MWCAgen    <SingleLongGenerator, CREATABLE> @end
@deftype MWCBgen    <SingleLongGenerator, CREATABLE> @end

// 
// RWC2 --
//   single long generator recommended for use.
//
@deftype RWC2gen   <SingleLongGenerator, CREATABLE> @end

// 
// RWC8 ("Mother") --
//   single long generator recommended for use.
// 
@deftype RWC8gen   <CombinedLongGenerator, CREATABLE> @end

// 
// TT403, TT775, TT800 --
//   single long generators recommended for use.
//
@deftype TGFSRgen    <SingleLongGenerator>  @end

@deftype TT403gen     <TGFSRgen, CREATABLE> @end
@deftype TT775gen     <TGFSRgen, CREATABLE> @end
@deftype TT800gen     <TGFSRgen, CREATABLE> @end

// 
// MT19937 --
//   single *very* long generator recommended for use.
// 
@deftype MT19937gen  <SingleLongGenerator, CREATABLE> @end

//
// MRG5, MRG6, MRG7 --
//   single long generators recommended for use.
//
@deftype MRGgen      <SingleLongGenerator>  @end

@deftype MRG5gen      <MRGgen, CREATABLE> @end
@deftype MRG6gen      <MRGgen, CREATABLE> @end
@deftype MRG7gen      <MRGgen, CREATABLE> @end

//
// C2TAUS1, C2TAUS2, C2TAUS3 --
//   short component based generator recommended for use.
//
@deftype C2TAUSgen    <CombinedShortGenerator> @end

@deftype C2TAUS1gen   <C2TAUSgen, CREATABLE> @end
@deftype C2TAUS2gen   <C2TAUSgen, CREATABLE> @end
@deftype C2TAUS3gen   <C2TAUSgen, CREATABLE> @end

// 
// C2MRG3 --
//   long component based generator recommended for use.
//
@deftype C2MRG3gen <CombinedLongGenerator, CREATABLE> @end

// 
// C3MWC --
//   long component based generator recommended for use.
// 
@deftype C3MWCgen   <CombinedLongGenerator, CREATABLE> @end

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

//
// C2LCGX --
//   short component based generator with splitting facilities. Recommended.
//
@deftype C2LCGXgen  <CombinedShortSplitGenerator, CREATABLE> @end

//
// C4LCGX --
//   short component based generator with splitting facilities. Recommended.
//
@deftype C4LCGXgen  <CombinedShortSplitGenerator, CREATABLE> @end

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

  // (none)

// <SingleLongSplitGenerator>

  // (none)

// <CombinedShortSplitGenerator>

@class C2LCGXgen;
@class C4LCGXgen;

// <CombinedLongSplitGenerator>

  // (none)

