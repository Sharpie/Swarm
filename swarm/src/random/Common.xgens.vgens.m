// Common.xgens.vgens.m

// 
// Common code for split generators
// Random version 0.7
// 

// ----- Managing virtual generators: -----

-initGenerator: (unsigned) vGen {
   int j;
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

-restartGenerator: (unsigned) vGen {
   int j;
// Reset chosen virtual generator to beginning of current segment
   for (j=0; j<COMPONENTS; j++)
   {
     vGenArr[vGen].Cg[j] = vGenArr[vGen].Lg[j];	// reset state = segmentSeed
   }
   vGenArr[vGen].currentCount = 0;		// restart counter
   return self;
}

-advanceGenerator: (unsigned) vGen {
   int j;
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


-initAll {
   unsigned int i;

     for (i=0; i<numGenerators; i++) 
     [self initGenerator: i];

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


