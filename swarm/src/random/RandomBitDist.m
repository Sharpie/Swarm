// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            RandomBitDist.m
Description:     Distribution returning a fair coin toss
Library:         random
Original Author: Sven Thommesen
Date:            1997-01-15

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)

*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/


#import <string.h>

#import <collections.h>
#import <random/RandomBitDist.h>


@implementation RandomBitDist


// Import common code snippets:

#import "Common.dists.m"


// And now code particular to this distribution:


// data struct used by setStateFrom / putStateInto:
//
typedef struct {
   // Object identification:
   unsigned int distMagic;
   unsigned int stateSize;
   // Generator data:
   unsigned int genMagic;
   BOOL useSplitGenerator;
   unsigned int virtualGenerator;
   // Fixed parameters:
   BOOL optionsInitialized;
   unsigned int theMask;
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;


-initState {

// Distribution personality:

   stateSize = sizeof(state_struct_t);
   strncpy(distName,"RandomBitDist",sizeof(distName));
   distMagic = RANDOMBITMAGIC + RANDOMBITREVISION;

// Parameters:

   optionsInitialized = NO;
   useSplitGenerator  = NO;
   virtualGenerator   = MAXVGEN;

   theMask = (1U<<14);				  // take middle bit

   return self;
}


-resetState {

// Called by setGenerator in the superclass

    currentCount = 0;

   return self;
}


+createBegin: aZone {
   RandomBitDist * aDistribution;

// Allocate space for the object:

   aDistribution = [super createBegin: aZone];

// Initialize instance variables:

   aDistribution->randomGenerator = NULL;

// Initialize parameters:

   [aDistribution initState];

   return aDistribution;
}


+createWithDefaults: (id) aZone {
   RandomBitDist * aDistribution;

// Allocate space for the object:

   aDistribution = [RandomBitDist createBegin: aZone];

// Connect a default random generator:

   [aDistribution setGenerator: [C2TAUS1gen createWithDefaults: aZone] ];

   return [ aDistribution createEnd ];

}


+create: (id) aZone setGenerator: (id) generator {
   RandomBitDist * aDistribution;

// Allocate space for the object:

   aDistribution = [RandomBitDist createBegin: aZone];

// Connect the supplied random generator:

   [aDistribution setGenerator: generator];

   return [ aDistribution createEnd ];

}


+create: (id) aZone setGenerator: (id) generator 
	setVirtualGenerator: (unsigned) vGen      {
   RandomBitDist * aDistribution;

// Allocate space for the object:

   aDistribution = [RandomBitDist createBegin: aZone];

// Connect the supplied random generator:

   [aDistribution setGenerator: generator
	setVirtualGenerator: vGen];

   return [ aDistribution createEnd ];

}


// ----- There are no settable parameters for this distribution,
// ----- hence no further 'create' methods.

// ----- (no parameters to return / we keep theMask to ourselves)


// ----- Generate random values:

// The methods below all pick out a middle bit from an unsigned
// random number. The bit is set in parameter theMask (see initState).

// ----- protocol BooleanDistribution -----

-(BOOL) getBooleanSample {
  unsigned rValue;

   currentCount++ ;

   if (useSplitGenerator)
     rValue = [randomGenerator getUnsignedSample: virtualGenerator];
   else
     rValue = [randomGenerator getUnsignedSample];

   return (rValue & theMask) ? YES : NO ; 
}


-(int) getIntegerSample {
   unsigned rValue;

   currentCount++ ;

   if (useSplitGenerator)
     rValue = [randomGenerator getUnsignedSample: virtualGenerator];
   else
     rValue = [randomGenerator getUnsignedSample];

  
   return (rValue & theMask) ? 1 : 0 ;
}

// ----- protocol RandomBitDistribution -----

// This method does exactly the same thing as getBooleanSample;
// it's here just for historical reasons (it used to be here!)

-(BOOL) getCoinToss {
   unsigned rValue;

   currentCount++ ;

   if (useSplitGenerator)
     rValue = [randomGenerator getUnsignedSample: virtualGenerator];
   else
     rValue = [randomGenerator getUnsignedSample];

 
   return (rValue & theMask) ? YES : NO ; 
}

// ----- protocol InternalState -----

-(void) putStateInto: (void *) buffer {
   state_struct_t * internalState;

  // recast the caller's pointer:
  internalState = (state_struct_t *) buffer;

  // fill the caller's buffer with state data:

  // object identification:
  internalState->distMagic = distMagic;
  internalState->stateSize = stateSize;

  // generator data:
  internalState->genMagic = (unsigned) [randomGenerator getMagic];
  internalState->useSplitGenerator = useSplitGenerator;
  internalState->virtualGenerator = virtualGenerator;

  // fixed parameters:
  internalState->optionsInitialized = optionsInitialized;
  internalState->theMask = theMask;

  // state variables:
  internalState->currentCount = currentCount;

  // nothing is returned from a (void) function

}

-(void) setStateFrom: (void *) buffer {
   state_struct_t * internalState;

  // recast the caller's pointer:
  internalState = (state_struct_t *) buffer;

  // TEST the integrity of the external data:
  if (    ( internalState->distMagic != distMagic )
       || ( internalState->stateSize != stateSize )
     )
  [InvalidCombination raiseEvent:
  "%u %s: you are passing bad data to setState!\n %u %u\n",
   distMagic, distName,
   internalState->distMagic, internalState->stateSize];

  // set internal state from data in caller's buffer:

  // Fixed parameters:
  optionsInitialized = internalState->optionsInitialized;
  theMask      = internalState->theMask;

  // State variables:
  currentCount = internalState->currentCount;

  // Test generator data:

  if (
          ( (unsigned) [randomGenerator getMagic] != internalState->genMagic )
       || ( useSplitGenerator != internalState->useSplitGenerator )
       || ( virtualGenerator  != internalState->virtualGenerator  )
     )
  printf("%s setState: Warning! Not using the same generator!\n", distName);

  // nothing is returned from a (void) function

}

- (void) describe: outStream {
  char buffer[200];

  (void)sprintf(buffer," %s describe: outStream: \n", distName);
  [outStream catC: buffer];

  (void)sprintf(buffer,"          distMagic = %24u\n", distMagic);
  [outStream catC: buffer];
  (void)sprintf(buffer,"           distName = %24s\n", distName);
  [outStream catC: buffer];
  (void)sprintf(buffer,"          stateSize = %24u\n", stateSize);
  [outStream catC: buffer];
  (void)sprintf(buffer,"         *Generator = %24p\n", randomGenerator);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            genName = %24s\n", 
	[randomGenerator getName]);
  [outStream catC: buffer];
  (void)sprintf(buffer,"       generatorMax = %24u\n", 
	[randomGenerator getUnsignedMax]);
  [outStream catC: buffer];
  (void)sprintf(buffer,"  useSplitGenerator = %24d\n", useSplitGenerator);
  [outStream catC: buffer];
  (void)sprintf(buffer,"   virtualGenerator = %24u\n", virtualGenerator);
  [outStream catC: buffer];
  (void)sprintf(buffer," optionsInitialized = %24d\n", optionsInitialized);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            theMask = %24u\n", theMask);
  [outStream catC: buffer];
  (void)sprintf(buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  [outStream catC: "\n"];

  //  return self;
}

@end
