// Common.gens.create.m

// 
// Common code for simple generators
// Random version 0.7
// 

-runup: (unsigned) streak {
   unsigned i;
   unsigned bitbucket;

// Draw a number of variates from the generator:

   for (i=0; i < streak; i++)
     bitbucket = [self getUnsignedSample];

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

   for (i = 0; i < lengthOfSeedVector; i++)
     initialSeeds[i] = seeds[i];	// fill initialSeeds[]

   [self setState];			// fill state from initialSeeds[]

   return self;
}

-createEnd {

// This test will disallow the use of [aGenerator create: aZone]
// (user must call createBegin, setStateFromSeed, createEnd):

  if (currentCount == TESTCASE) // no instantiation chosen:
  [InvalidCombination raiseEvent:
  "%s not Initialized with a Seed!\n", genName];
 
  return [super createEnd];
}

