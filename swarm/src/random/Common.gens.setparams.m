// Common.gens.setparams.m

// 
// Common code for simple generators
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

-(char *) getName {
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

