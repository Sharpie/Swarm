// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            BernoulliDist.m
Description:     Distribution that returns YES with a given probability
Library:         random
Original Author: Sven Thommesen
Date:		 1997-09-01 (v. 0.7)

*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <string.h>
#import <math.h>

#import <collections.h>
#import <random/BernoulliDist.h>


@implementation BernoulliDist


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
   double theP;
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;


-initState {

// Distribution personality:

   stateSize = sizeof(state_struct_t);
   strncpy(distName,"BernoulliDist",sizeof(distName));
   distMagic = BERNOULLIDISTMAGIC + BERNOULLIDISTREVISION;

// Parameters:

   optionsInitialized = NO;
   useSplitGenerator  = NO;
   virtualGenerator   = MAXVGEN;

   theP               = 0.0;

   return self;
}


-resetState {

   currentCount = 0;

   return self;
}


+createBegin: aZone {
   BernoulliDist * aDistribution;

// Allocate space for the object:

   aDistribution = [super createBegin: aZone];

// Initialize instance variables:

   aDistribution->randomGenerator = NULL;

// Initialize parameters:

   [aDistribution initState];

   return aDistribution;
}


+create: (id) aZone setGenerator: (id) generator {
   BernoulliDist * aDistribution;

// Allocate space for the object:

   aDistribution = [BernoulliDist createBegin: aZone];

// Connect the supplied random generator:

   [aDistribution setGenerator: generator];

   return [ aDistribution createEnd ];

}


+createWithDefaults: (id) aZone {
   BernoulliDist * aDistribution;

// Allocate space for the object:

   aDistribution = [BernoulliDist createBegin: aZone];

// Connect a default random generator:

   [aDistribution setGenerator: [C2TAUS2gen createWithDefaults: aZone] ];

   return [ aDistribution createEnd ];

}


+create: (id) aZone setGenerator: (id) generator 
	setVirtualGenerator: (unsigned) vGen      {
   BernoulliDist * aDistribution;

// Allocate space for the object:

   aDistribution = [BernoulliDist createBegin: aZone];

// Connect the supplied random generator:

   [aDistribution setGenerator: generator
	setVirtualGenerator: vGen];

   return [ aDistribution createEnd ];

}


// ----- protocol Bernoulli -----

-(double) getProbability {
   return theP;
}

-setProbability: (double) p {

/*
// Relax this restriction, too.

   if (optionsInitialized)
   [InvalidCombination raiseEvent:
   "%s: setting parameters more than once not allowed\n", distName];
*/

   if ( (p < 0.0) || (p > 1.0) )
   [InvalidCombination raiseEvent:
   "%s: probability must be in [0,1]! \n", distName];

   theP = p;

   // This object is now fixed:

   optionsInitialized = YES;

   [self resetState];

   return self;
}

+create: (id) aZone setGenerator: (id) generator
	setProbability: (double) p {
   BernoulliDist * aDistribution;

   aDistribution = [ BernoulliDist create: aZone 
			setGenerator: generator ];

   [aDistribution setProbability: p ];

   return aDistribution;
}

+create: (id) aZone setGenerator: (id) generator
	setVirtualGenerator: (unsigned) vGen
	setProbability: (double) p            {
   BernoulliDist * aDistribution;

   aDistribution = [ BernoulliDist create: aZone 
			setGenerator: generator 
			setVirtualGenerator: vGen ];

   [aDistribution setProbability: p ];

   return aDistribution;
}


// ----- Generate random values: -----


-(BOOL) getSampleWithProbability: (double) p {

/*
// Allow this call even if parameters are set!

   if (optionsInitialized)
   [InvalidCombination raiseEvent:
   "%s: getSampleWithProbability: parameters are frozen\n", distName];
*/

   if ( (p < 0.0) || (p > 1.0) )
   [InvalidCombination raiseEvent:
   "%s: probability must be in [0,1]! \n", distName];

   currentCount++ ;

#ifdef USETHINDOUBLES
   if (useSplitGenerator)
     return ( [randomGenerator getThinDoubleSample: virtualGenerator] < p );
   else
     return ( [randomGenerator getThinDoubleSample] < p );
#else
   if (useSplitGenerator)
     return ( [randomGenerator getDoubleSample: virtualGenerator] < p );
   else
     return ( [randomGenerator getDoubleSample] < p );
#endif

}

-(BOOL) getBooleanSample {

if (!optionsInitialized)
[InvalidCombination raiseEvent:
"%s: getBooleanSample: parameters  have not been set\n", distName];

   currentCount++ ;

#ifdef USETHINDOUBLES
   if (useSplitGenerator)
     return ( [randomGenerator getThinDoubleSample: virtualGenerator] < theP );
   else
     return ( [randomGenerator getThinDoubleSample] < theP );
#else
   if (useSplitGenerator)
     return ( [randomGenerator getDoubleSample: virtualGenerator] < theP );
   else
     return ( [randomGenerator getDoubleSample] < theP );
#endif

}

-(int) getIntegerSample {

if (!optionsInitialized)
[InvalidCombination raiseEvent:
"%s: getIntegerSample: parameters  have not been set\n", distName];

   currentCount++ ;

#ifdef USETHINDOUBLES
   if (useSplitGenerator)
return ( [randomGenerator getThinDoubleSample: virtualGenerator] < theP) ? 1:0;
   else
return ( [randomGenerator getThinDoubleSample] < theP ) ? 1:0;
#else
   if (useSplitGenerator)
return ( [randomGenerator getDoubleSample: virtualGenerator] < theP) ? 1:0;
   else
return ( [randomGenerator getDoubleSample] < theP ) ? 1:0;
#endif

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
  internalState->theP = theP;

  // state variables:
  internalState->currentCount = currentCount;

  // nothing is returned from a (void) function

}

-(void) setStateFrom: (void *) buffer {
   state_struct_t * internalState;

  // recast the caller's pointer:
  internalState = (state_struct_t *) buffer;

  // TEST the integrity of the external data:
  if (    (internalState->distMagic != distMagic)
       || (internalState->stateSize != stateSize)
     )
  [InvalidCombination raiseEvent:
  "%u %s: you are passing bad data to setState!\n %u %u\n",
   distMagic, distName,
   internalState->distMagic, internalState->stateSize];

  // set internal state from data in caller's buffer:

  // Fixed parameters:
  optionsInitialized = internalState->optionsInitialized;
  theP       = internalState->theP;

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
  (void)sprintf(buffer,"               theP = %24.16e\n", theP);
  [outStream catC: buffer];
  (void)sprintf(buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  [outStream catC: "\n"];

  //  return self;
}

@end
