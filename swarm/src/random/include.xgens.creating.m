// include.xgens.creating.m
// 

// 
// Common code for split generators
// Random version 0.8
// 

// CREATING

-allocVectors {			// called from -initState
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

// include.xgens.creating.m
