// Common.xgens.setparams.m

// 
// Common code for split generators
// Random version 0.7
// 

// ----- Getting / setting parameters: -----


-setAntithetic: (BOOL) antiT {
   antiThetic = antiT;
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

// These methods are implemented following L'Ecuyer,
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

