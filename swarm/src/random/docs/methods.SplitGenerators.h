//                                                                       split.h

// ----- Private (unpublished) methods: -----

-		runup: (unsigned) streak;
-		generateSeeds;
-		setState;
-		allocVectors;

-		initState;
+		createBegin: (id) aZone;
-		setStateFromSeed: (unsigned) seed;
-		setStateFromSeeds: (unsigned *) seeds;
-		createEnd;


// ----- Zero-seed creation: -----
// (Create with a default set of seeds and parameters:)

+		createWithDefaults: aZone;

// ----- Single-seed creation: -----

+		create: aZone
		   setA: (unsigned) A 		// # of virtual generators
		   setv: (unsigned) v 		// log2(#segments/generator)
		   setw: (unsigned) w		// log2(segment length)
		setStateFromSeed:  (unsigned)   seed;

// Limits on seed value supplied (minimum = 1):
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

// Limits on seed values supplied (minimum = 1):
- (unsigned *)	getMaxSeedValues;

// Return generator starting values:
- (unsigned *)	getInitialSeeds;

// ----- Other create methods: -----
-		setAntithetic: (BOOL) antiT;

// ----- Return values of parameters: -----
- (BOOL)	getAntithetic;

- (unsigned)    getNumGenerators;		// returns A
- (unsigned)    getNumSegments;			// returns v
- (unsigned)    getSegmentLength;		// returns w

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
- (char *)      getName;		
- (unsigned)	getMagic;	

