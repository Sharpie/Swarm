// include.gens.using.m
//

// 
// Common code for simple generators
// Random version 0.8
// 

// USING

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

-(unsigned long long int) getCurrentCount {
   return currentCount;
}

-(unsigned) getStateSize {
   return stateSize;
}

// ----- Get floating-point random values: -----

// Change in version 0.75:
// -getDoubleSample no longer calls -getLongDoubleSample,
// since doing so made the method non-portable.

-(float) getFloatSample {
   double dd;
   // One 32-bit unsigned random number is used
   // to fill the 24-bit mantissa of a float

   dd = invModMult * [self getUnsignedSample];
   return (float) dd;
}

-(double) getThinDoubleSample {
  double dd;
  // *One single* 32-bit unsigned random number is used
  // to fill the 53-bit mantissa of a double

   dd = invModMult * [self getUnsignedSample];
   return dd;
}

-(double) getDoubleSample {
   double dd;
   // Two 32-bit unsigned random numbers are used
   // to fill the 53-bit mantissa of a double.

   dd = invModMult  * [self getUnsignedSample]
      + invModMult2 * [self getUnsignedSample];
   return dd;
}

// NOTE: since the size of a long double is machine dependent,
// using this method may render the simulation non-portable!
-(long double) getLongDoubleSample {
   long double ld;
   // Two 32-bit unsigned random numbers are used
   // to fill the mantissa of a long double.

   ld = (long double) invModMult  * [self getUnsignedSample]
      + (long double) invModMult2 * [self getUnsignedSample];

   return ld;
}

//
// include.gens.using.m
