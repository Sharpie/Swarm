// Common.xgens.create.m

// 
// Common code for split generators
// Random version 0.7
//

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


-allocVectors {
   int arrSize;

   if (vGenArr != NULL) {
   // de-allocate first
   [[self getZone] free: vGenArr];
   }

   arrSize = numGenerators*sizeof(struct vGenStruct);

   vGenArr = [[self getZone] alloc: arrSize];
   if (vGenArr == NULL)
     [InvalidCombination raiseEvent:
     "%s: Error allocating state vectors!\n", genName];

   memset(vGenArr,0,arrSize);	// zero the memory
   // NOTE: if arrSize > LONG_MAX we dump core here!


  return self;
}



-setStateFromSeed: (unsigned) seed {

   if (seed == 0)
   [InvalidCombination raiseEvent:
   "%s setStateFromSeed: seed %u cannot be zero\n", genName, seed ];

   singleInitialSeed = YES;

   initialSeed = seed;

   [self generateSeeds];		// fill initialSeeds[] using PMMLCG

   [self setState];			// fill state from initialSeeds[]

   return self;
}

-setStateFromSeeds: (unsigned *) seeds {
   int i;

   for (i = 0; i < lengthOfSeedVector; i++)
     if ( (seeds[i] > maxSeedValues[i]) || (seeds[i] == 0) )
       [InvalidCombination raiseEvent:
       "%s setStateFromSeeds: seeds[%d]=%u is too large (max %u) or zero\n",
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


-createEnd {

// This test will disallow the use of [aGenerator create: aZone]
// (user must call createBegin, setStateFromSeed, createEnd):

  if (numGenerators == 0)
  [InvalidCombination raiseEvent:
  "%s not Initialized with A,v,w parameters!\n", genName];
 
  if (vGenArr[0].Ig[0] == 0)
  [InvalidCombination raiseEvent:
  "%s not Initialized with Seeds!\n", genName];

  return [super createEnd];
}

