// include.gens.creating.m
//

// 
// Common code for simple generators
// Random version 0.8
// 

// CREATING

-runup: (unsigned) streak {
   unsigned i;
   unsigned bitbucket;

// Draw a number of variates from the generator:

   for (i=0; i < streak; i++)
     bitbucket = [self getUnsignedSample];

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

// 
// include.gens.creating.m
