// Common.gens.floats.m

// 
// Common code for simple generators
// Random version 0.75
// 

// Change since version 0.7:
// -getDoubleSample no longer calls -getLongDoubleSample,
// since doing so made the method non-portable.

// ----- Get floating-point random values: -----

-(float) getFloatSample {
   double dd;
   // One 32-bit unsigned random number is used
   // to fill the 24-bit mantissa of a float

   dd = invModMult * [self getUnsignedSample];
   return (float) dd;

   // return (float) [self getThinDoubleSample];
}

-(double) getThinDoubleSample {
  double dd;
  // *One single* 32-bit unsigned random number is used
  // to fill the 53-bit mantissa of a double

   dd = invModMult * [self getUnsignedSample];
   return dd;

   // return (double) ( invModMult * [self getUnsignedSample] );
}

-(double) getDoubleSample {
   double dd;
   // Two 32-bit unsigned random numbers are used
   // to fill the 53-bit mantissa of a double.
   // Since everyone uses standard 8-byte doubles with 53-bit mantissa,
   // truncating a long double to a double should be portable.

   dd = invModMult  * [self getUnsignedSample]
      + invModMult2 * [self getUnsignedSample];
   return dd;

   // return (double) [self getLongDoubleSample];
}

-(long double) getLongDoubleSample {
   long double ld;
   // Two 32-bit unsigned random numbers are used
   // to fill the mantissa of a long double.
   // NOTE: since the size of a long double is machine dependent,
   // using this method may render the simulation non-portable!

   ld = (long double) invModMult  * [self getUnsignedSample]
      + (long double) invModMult2 * [self getUnsignedSample];

   return ld;
}
