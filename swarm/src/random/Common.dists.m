// Common.dists.m

// Code common to all distributions
// Random version 0.7
// 

-setGenerator: (id) generator {

   if (randomGenerator) 
   [InvalidCombination raiseEvent:
   "%s: setting the generator more than once not allowed\n",distName];

   randomGenerator = generator;
   generatorMax = [randomGenerator getUnsignedMax];

   if ([randomGenerator respondsTo: M(advanceAll)])               { // split

     useSplitGenerator = YES;
	// printf("%s setGenerator: using split generator %s\n",
	// distName, [randomGenerator getName]);

   } else if ([randomGenerator respondsTo: M(getUnsignedSample)]) { // simple

     useSplitGenerator = NO;
     virtualGenerator = 0;
	// printf("%s setGenerator: using simple generator %s\n",
	// distName, [randomGenerator getName]);

   } else {							     // neither

     [InvalidCombination raiseEvent:
     "%s setGenerator: not a generator !!!\n",distName];

   }

/*
// This code does not work, for unknown reasons:
   if ( [randomGenerator conformsTo: @protocol(SimpleOut)]) {
   } else if ( [randomGenerator conformsTo: @protocol(SplitOut)]) {
   } else {
   }
*/

   if (useSplitGenerator) 
   [InvalidCombination raiseEvent:
   "%s: wrong version of create: split generator %s detected\n",
   distName, [randomGenerator getName]];

   [self resetState];

   return self;
}

-setGenerator: (id) generator setVirtualGenerator: (unsigned) vGen {
   unsigned k;

   if (randomGenerator) 
   [InvalidCombination raiseEvent:
   "%s: setting the generator more than once not allowed\n",distName];

   randomGenerator = generator;
   generatorMax = [randomGenerator getUnsignedMax];

   if ([randomGenerator respondsTo: M(advanceAll)])               { // split

     useSplitGenerator = YES;
	// printf("%s setGenerator: using split generator %s\n",
	// distName, [randomGenerator getName]);

   } else if ([randomGenerator respondsTo: M(getUnsignedSample)]) { // simple

     useSplitGenerator = NO;
     virtualGenerator = 0;
	// printf("%s setGenerator: using simple generator %s\n",
	// distName, [randomGenerator getName]);

   } else {							     // neither

     [InvalidCombination raiseEvent:
     "%s setGenerator: not a generator !!!\n",distName];

   }

/*
// This code does not work, for unknown reasons:
   if ( [randomGenerator conformsTo: @protocol(SimpleOut)]) {
   } else if ( [randomGenerator conformsTo: @protocol(SplitOut)]) {
   } else {
   }
*/

   if (!useSplitGenerator) 
      [InvalidCombination raiseEvent:
      "%s: wrong version of create: non-split generator %s detected\n",
      distName, [randomGenerator getName]];

//   [self resetState];

   if (virtualGenerator != MAXVGEN) 
      [InvalidCombination raiseEvent:
      "%s setVirtualGenerator: already set\n", distName];

   k = [randomGenerator getNumGenerators];

   if (vGen >= k)
      [InvalidCombination raiseEvent:
      "%s setVirtualGenerator: vGen=%d > generator allows (%d)\n", 
      vGen, k-1];

   virtualGenerator = vGen;

   [self resetState];

   return self;
}


-createEnd {

// If no generator has been allocated, abort:

   if (!randomGenerator)	
      [InvalidCombination raiseEvent:
      "%s initialized without a generator!\n",
      distName];

   return [super createEnd];
}


-reset {
   return [self resetState];
}

// ----- Return data values: -----


-(id) getGenerator {
   return randomGenerator;
}


-(unsigned) getVirtualGenerator {
   return virtualGenerator;
}


-(BOOL) getOptionsInitialized {
   return optionsInitialized;
}


-(unsigned long long int) getCurrentCount {
   return currentCount;
}

-(unsigned) getStateSize {
   return stateSize;
}

-(char *) getName {
   return distName;
}

-(unsigned) getMagic {
   return distMagic;
}

