// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            ExponentialDist.m
Description:     Exponential distribution returning doubles
Library:         random
Original Author: Sven Thommesen
Date:            1997-01-15
Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Modified by:	 Sven Thommesen
Date:		 1998-10-08 (v. 0.8)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <math.h>

#import <collections.h>
#import <random/ExponentialDist.h>


@implementation ExponentialDist

// data struct used by setStateFrom / putStateInto:
//
typedef struct {
   // Object identification:
   unsigned int distMagic;
   unsigned stateSize;
   // Generator data:
   unsigned int genMagic;
   BOOL useSplitGenerator;
   unsigned int virtualGenerator;
   // Fixed parameters:
   BOOL optionsInitialized;
   double theMean;
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;



PHASE(Creating)

#include "include.dists.creating.m"

- initState
{
  // Distribution personality:
  
  stateSize = sizeof(state_struct_t);
  strncpy(distName,"ExponentialDist",sizeof(distName));
  distMagic = EXPONENTIALDISTMAGIC + EXPONENTIALDISTREVISION;
  
  // Parameters:
  
  optionsInitialized = NO;
  useSplitGenerator  = NO;
  virtualGenerator   = MAXVGEN;
  
  theMean   = 0.0;
  
#ifdef USETHINDOUBLES
  printf("NOTE! %s: created to use THIN doubles\n",distName);
#endif
  
  return self;
}

+ createBegin: aZone
{
  ExponentialDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [super createBegin: aZone];
  
  // Initialize instance variables:
  
  aDistribution->randomGenerator = NULL;
  
  // Initialize parameters:
  
  [aDistribution initState];
  
  return aDistribution;
}


+ create: aZone setGenerator: generator
{
  ExponentialDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [ExponentialDist createBegin: aZone];
  
  // Connect the supplied random generator:
  
  [aDistribution setGenerator: generator];
  
  return [aDistribution createEnd];
  
}

+ createWithDefaults: aZone
{
  ExponentialDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [ExponentialDist createBegin: aZone];
  
// Connect a default random generator:
  
  [aDistribution setGenerator: [C2TAUS3gen createWithDefaults: aZone]];
  
  return [aDistribution createEnd];
}


+ create: aZone setGenerator: generator  setVirtualGenerator: (unsigned)vGen
{
  ExponentialDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [ExponentialDist createBegin: aZone];

  // Connect the supplied random generator:
  
  [aDistribution setGenerator: generator
                 setVirtualGenerator: vGen];
  
  return [aDistribution createEnd];
}

+ create: aZone setGenerator: generator setMean: (double)mean
{
  ExponentialDist *aDistribution;
  
  aDistribution = [ExponentialDist create: aZone 
                                   setGenerator: generator];
  
  [aDistribution setMean: mean];
  
  return aDistribution;
}

+ create             : aZone
         setGenerator: generator 
  setVirtualGenerator: (unsigned)vGen
              setMean: (double)mean
{
  ExponentialDist *aDistribution;
  
  aDistribution = [ExponentialDist create: aZone 
                                   setGenerator: generator
                                   setVirtualGenerator: vGen];
  
  [aDistribution setMean: mean];
  
  return aDistribution;
}


PHASE(Setting)

#include "include.dists.setting.m"

- resetState
{
  currentCount = 0;
  
  return self;
}

- setMean: (double)mean
{
  /*
    // Relax this restriction, too.
    
    if (optionsInitialized)
    [InvalidCombination raiseEvent:
    "%s: setting parameters more than once not allowed\n", distName];
  */
  
  theMean = mean;
  
  // This object is now fixed:
  
  optionsInitialized = YES;
  
  [self resetState];
  
  return self;
}


PHASE(Using)
     
#include "include.dists.using.m"
     
     
- (double)getMean
{
  return theMean;
}


- (double)getSampleWithMean: (double)mean
{
  double xpon;
  double rdValue;
  
  /*
    // Allow this call even if parameters are set!
    
    if (optionsInitialized)
    [InvalidCombination raiseEvent:
    "%s: getSampleWithMean: options already initialized\n", distName];
  */
  
  currentCount++;
  
  do {
#ifdef USETHINDOUBLES
    if (useSplitGenerator)
      rdValue = [randomGenerator getThinDoubleSample: virtualGenerator];
    else
      rdValue = [randomGenerator getThinDoubleSample];
#else
    if (useSplitGenerator)
      rdValue = [randomGenerator getDoubleSample: virtualGenerator];
    else
      rdValue = [randomGenerator getDoubleSample];
#endif
  } while (rdValue == 0.0);		// cannot take log of zero
  
  // Transform the uniform value:
  
  xpon = -mean * log (rdValue);		// need rdValue > 0 !!!
  
  return xpon;
}


- (double)getDoubleSample
{
  double xpon;
  double rdValue;
  
  if (!optionsInitialized)
    [InvalidCombination
      raiseEvent:
        "%s: getDoubleSample: parameters have not been set\n", distName];
  
  currentCount++;
  
  do {
#ifdef USETHINDOUBLES
    if (useSplitGenerator)
      rdValue = [randomGenerator getThinDoubleSample: virtualGenerator];
    else
      rdValue = [randomGenerator getThinDoubleSample];
#else
    if (useSplitGenerator)
      rdValue = [randomGenerator getDoubleSample: virtualGenerator];
    else
      rdValue = [randomGenerator getDoubleSample];
#endif
  } while (rdValue == 0.0);		// cannot take log of zero
  
  // Transform the uniform value:

   xpon = -theMean * log (rdValue);	// need rdValue > 0 !!!
   
   return xpon;
}


- (void)putStateInto: (void *)buffer
{
  state_struct_t *internalState;
  
  // recast the caller's pointer:
  internalState = (state_struct_t *)buffer;
  
  // fill the caller's buffer with state data:
  // object identification:
  internalState->distMagic = distMagic;
  internalState->stateSize = stateSize;
  // generator data:
  internalState->genMagic = (unsigned) [randomGenerator getMagic];
  internalState->useSplitGenerator = useSplitGenerator;
  internalState->virtualGenerator = virtualGenerator;
  // Fixed parameters:
  internalState->optionsInitialized = optionsInitialized;
  internalState->theMean = theMean;
  // State variables:
  internalState->currentCount = currentCount;
  
  // nothing is returned from a (void) function
}

- (void)setStateFrom: (void *)buffer
{
  state_struct_t *internalState;
  
  // recast the caller's pointer:
  internalState = (state_struct_t *)buffer;
  
  // TEST the integrity of the external data:
  if ((internalState->distMagic != distMagic)
      || (internalState->stateSize != stateSize))
    [InvalidCombination
      raiseEvent:
        "%u %s: you are passing bad data to setState!\n %u %u\n",
      distMagic, distName,
      internalState->distMagic, internalState->stateSize];
  
  // set internal state from data in caller's buffer:
  
  // Fixed parameters:
  optionsInitialized = internalState->optionsInitialized;
  theMean = internalState->theMean;
  
  // State variables:
  currentCount = internalState->currentCount;

  // Test generator data:
  
  if (((unsigned) [randomGenerator getMagic] != internalState->genMagic)
      || (useSplitGenerator != internalState->useSplitGenerator)
      || (virtualGenerator  != internalState->virtualGenerator))
    printf("%s setState: Warning! Not using the same generator!\n", distName);
  
  // nothing is returned from a (void) function
}


- (void)describe: outStream
{
  char buffer[200];
  
  (void)sprintf (buffer," %s describe: outstream: \n", distName);
  [outStream catC: buffer];
  (void)sprintf (buffer,"          distMagic = %24u\n", distMagic);
  [outStream catC: buffer];
  (void)sprintf (buffer,"           distName = %24s\n", distName);
  [outStream catC: buffer];
  (void)sprintf (buffer,"          stateSize = %24u\n", stateSize);
  [outStream catC: buffer];
  (void)sprintf (buffer,"         *Generator = %24p\n", randomGenerator);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            genName = %24s\n", 
                 [randomGenerator getName]);
  [outStream catC: buffer];
  (void)sprintf (buffer,"       generatorMax = %24u\n", 
                 [randomGenerator getUnsignedMax]);
  [outStream catC: buffer];
  (void)sprintf (buffer,"  useSplitGenerator = %24d\n", useSplitGenerator);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   virtualGenerator = %24u\n", virtualGenerator);
  [outStream catC: buffer];
  (void)sprintf (buffer," optionsInitialized = %24d\n", optionsInitialized);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            theMean = %24.16e\n", theMean);
  [outStream catC: buffer];
  (void)sprintf (buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];


  [outStream catC: "\n"];

  //  return self;
}

@end
