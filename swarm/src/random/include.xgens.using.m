// include.xgens.using.m
// 

// 
// Common code for split generators
// Random version 0.8
// 

// USING

- (void)drop
{
  [[self getZone] free: vGenArr];
  [super drop];
}

-restartGenerator: (unsigned) vGen {
   unsigned j;
// Reset chosen virtual generator to beginning of current segment
   for (j=0; j<COMPONENTS; j++)
   {
     vGenArr[vGen].Cg[j] = vGenArr[vGen].Lg[j];	// reset state = segmentSeed
   }
   vGenArr[vGen].currentCount = 0;		// restart counter
   return self;
}

-advanceGenerator: (unsigned) vGen {
   unsigned j;
// Advance chosen virtual generator to beginning of next segment
   for (j=0; j<COMPONENTS; j++)
   {
   vGenArr[vGen].Lg[j] = [self MultModMs: aw[j] t: vGenArr[vGen].Lg[j] M: m[j]];
   vGenArr[vGen].Cg[j] = vGenArr[vGen].Lg[j];
   }
   vGenArr[vGen].currentCount = 0;		// restart counter
   vGenArr[vGen].currentSegment++;		// going to next segment

   if (vGenArr[vGen].currentSegment > segmentMax)
   [InvalidCombination raiseEvent:
   "%s advanceGenerator: %u exceeding limit on #segments %24qu\n", 
   genName, vGen, vGenArr[vGen].currentSegment];

   return self;
}

-jumpGenerator: (unsigned) vGen toSegment: (unsigned long long int) seg {
   unsigned long long int i;

   if (seg > segmentMax)
   [InvalidCombination raiseEvent:
   "%s jumpGenerator: segment chosen is too big! %24qu\n", genName,seg];

// Position chosen virtual generator to start of specified segment 
   [self initGenerator: vGen];
   for (i=0; i<seg; i++)
     [self advanceGenerator: vGen];
   return self;
}


-restartAll {
   unsigned int i;

     for (i=0; i<numGenerators; i++) 
     [self restartGenerator: i];

   return self;
}

-advanceAll {
   unsigned int i;

     for (i=0; i<numGenerators; i++) 
     [self advanceGenerator: i];

   return self;
}

-jumpAllToSegment: (unsigned long long int) seg {
   unsigned int i;

     for (i=0; i<numGenerators; i++) 
     [self jumpGenerator: i toSegment: seg];

   return self;
}


-(BOOL) getAntithetic {
   return antiThetic;
}

-(unsigned) getUnsignedMax {
   return unsignedMax;
}

-(unsigned) lengthOfSeedVector {
   return lengthOfSeedVector;
}

-(unsigned) getMaxSeedValue {
   return 0xffffffff;
}

-(unsigned *) getMaxSeedValues {
   return &(maxSeedValues[0]);
}

-(unsigned) getInitialSeed {
   return initialSeed;
}

-(unsigned *) getInitialSeeds {
   return &(initialSeeds[0]);
}

-(const char *) getName {
   return genName;
}

-(unsigned) getMagic {
   return genMagic;
}

-(unsigned) getStateSize {
   return stateSize;
}

-(unsigned) getNumGenerators {
   return numGenerators;
}

-(unsigned) getNumSegments {
   return numSegments;
}

-(unsigned) getSegmentLength {
   return segmentLength;
}


-(unsigned long long int) getCurrentCount: (unsigned) vGen {
   return vGenArr[vGen].currentCount;
}

-(unsigned long long int) getCurrentSegment: (unsigned) vGen {
   return vGenArr[vGen].currentSegment;
}

// These 3 methods are implemented following L'Ecuyer,
// for debug purposes only. Not published.

-(unsigned *) getInitialSeeds: (unsigned) vGen {
   return &(vGenArr[vGen].Ig[0]);
}

-(unsigned *) getLastSeeds: (unsigned) vGen {
   return &(vGenArr[vGen].Lg[0]);
}

-(unsigned *) getCurrentSeeds: (unsigned) vGen {
   return &(vGenArr[vGen].Cg[0]);
}


// Change in version 0.75:
// -getDoubleSample no longer calls -getLongDoubleSample,
// since doing so made the method non-portable.

// ----- Producing generator output: -----

-(float) getFloatSample: (unsigned) vGen {
   double dd;
   // One 32-bit unsigned random number is used
   // to fill the 24-bit mantissa of a float

   dd = invModMult * [self getUnsignedSample: vGen];
   return (float) dd;
}

-(double) getThinDoubleSample: (unsigned) vGen {
   double dd;
   // *One single* 32-bit unsigned random number is used
   // to fill the 53-bit mantissa of a double

   dd = invModMult * [self getUnsignedSample: vGen];
   return dd;
}

-(double) getDoubleSample: (unsigned) vGen {
   double dd;
   // *Two* 32-bit unsigned random numbers are used
   // to fill the 53-bit mantissa of a double.

   dd = invModMult  * [self getUnsignedSample: vGen]
      + invModMult2 * [self getUnsignedSample: vGen];
   return dd;
}

// NOTE: since the size of a long double is machine dependent,
// using this method may render the simulation non-portable!
-(long double) getLongDoubleSample: (unsigned) vGen {
   long double ld;
   // Two 32-bit unsigned random numbers are used
   // to fill the mantissa of a long double.

   ld = (long double) invModMult  * [self getUnsignedSample: vGen]
      + (long double) invModMult2 * [self getUnsignedSample: vGen];
   return ld;
}

// include.xgens.setting.m
