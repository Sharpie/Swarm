// include.xgens.setting.m
// 

// 
// Common code for split generators
// Random version 0.8
// 

// SETTING

// This method called from -initState, -setState, -setStateFrom:
-(unsigned) MultModMs: (unsigned) s t: (unsigned) t M: (unsigned) M
/* Returns (s*t) MOD M.  Assumes that s < M and t < M.              */
/* See L'Ecuyer and Cote (1991).                                    */

/* This version uses 64-bit math instead, with no speed penalty. ST */

{
unsigned long long int ls, lt, lM, lWork;
unsigned ret;

   ls = s; lt = t; lM = M;		// to 64 bits
   lWork = (ls * lt) % lM;		// 64 bit math
   ret = lWork;				// back to 32 bits

   return ret;

}


-setStateFromSeed: (unsigned) seed {		// called from +create*

   if (seed == 0)
   [InvalidCombination raiseEvent:
   "%s setStateFromSeed: seed %u cannot be zero\n", genName, seed ];

   singleInitialSeed = YES;

   initialSeed = seed;

   [self generateSeeds];		// fill initialSeeds[] using PMMLCG

   [self setState];			// fill state from initialSeeds[]

   return self;
}

-setStateFromSeeds: (unsigned *) seeds {	// called from +create*
   unsigned i;

   for (i = 0; i < lengthOfSeedVector; i++)
     if ( (seeds[i] > maxSeedValues[i]) || (seeds[i] == 0) )
       [InvalidCombination raiseEvent:
       "%s setStateFromSeeds: seeds[%u]=%u is too large (max %u) or zero\n",
       genName, i, seeds[i], maxSeedValues[i] ];

   singleInitialSeed = NO;

   initialSeed = 0;			// this value not used 

   for (i = 0; i < lengthOfSeedVector; i++) {
     initialSeeds[i] = seeds[i];
     vGenArr[0].Ig[i] = initialSeeds[i] + 1;
   }

   [self setState];			// fill state from initialSeeds[]

   return self;
}


// Use an inline generator to create the full number of seeds required,
// using the single seed initialSeed to seed this inline generator.
// The specific generator used is a PMMLCG with a=69069 and m=2^32
// (see Knuth vol. 2, table 1 on p. 102, line 25). Cf. MT19937 source.
// This generator creates random nonzero 32-bit values, as long as
// initialSeed is not zero.

// Note: this method is identical to that for simple generators.

-generateSeedVector {
   unsigned ki;
   unsigned z;

   z = initialSeed;

   // Mix the bits a bit:
   for (ki=0; ki < 16; ki++)
      z = (69069 * z);		// modulo 2^32

   // Generate seeds, as many as needed:

   for (ki=0; ki < lengthOfSeedVector; ki++) {

     z = (69069 * z);		// modulo 2^32
     initialSeeds[ki] = z;
      
   }

   // Make sure we don't supply invalid seed values for this generator:

   for (ki=0; ki < lengthOfSeedVector; ki++) {

      if (maxSeedValues[ki] < 0xffffffff) {	// avoid seeds > maxSeedValue

        initialSeeds[ki] = initialSeeds[ki] % (maxSeedValues[ki] + 1);
        if (initialSeeds[ki] == 0) 		// avoid seed = 0
          initialSeeds[ki] = 0xecec;		// arbitrary value < 2^16
      } 
   }

   return self;
}


-setAntithetic: (BOOL) antiT {
   antiThetic = antiT;
   return self;
}


-initGenerator: (unsigned) vGen {
   unsigned j;
// Reset chosen virtual generator to segment #0 (initial seed)
   for (j=0; j<COMPONENTS; j++)
   {
     vGenArr[vGen].Lg[j] = vGenArr[vGen].Ig[j];	// segmentSeed = initialSeed
     vGenArr[vGen].Cg[j] = vGenArr[vGen].Lg[j];	// state = segmentSeed
   }
   vGenArr[vGen].currentCount = 0;		// restart counter
   vGenArr[vGen].currentSegment = 0;		// start in segment #0
   return self;
}

-initAll {
   unsigned int i;

     for (i=0; i<numGenerators; i++) 
     [self initGenerator: i];

   return self;
}

// include.xgens.setting.m
