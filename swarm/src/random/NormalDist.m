
// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            NormalDist.m
Description:     Normal (Gaussian) distribution returning double values
Library:         random
Original Author: Manor Askenazi
Date:		 1996-09-09
Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)
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
#import <random/NormalDist.h>

@implementation NormalDist

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
   double theVariance;
   // State variables:
   BOOL stored;
   double stored_double;
   unsigned long long int currentCount;
} state_struct_t;


PHASE(Creating)

#include "include.dists.creating.m"

- initState
{
  // Called from createBegin
  // Distribution personality:
  
  stateSize = sizeof(state_struct_t);
  strncpy (distName, "NormalDist", sizeof (distName));
  distMagic = NORMALDISTMAGIC + NORMALDISTREVISION;
  
  // Parameters:
  
  optionsInitialized = NO;
  useSplitGenerator = NO;
  virtualGenerator = MAXVGEN;
  
  theMean       = 0.0;
  theVariance   = 0.0;
  theStdDev     = 0.0;
  
#ifdef USETHINDOUBLES
  printf("NOTE! %s: created to use THIN doubles\n",distName);
#endif
  
   return self;
}

+ createBegin: aZone
{
  NormalDist *aDistribution;
  
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
  NormalDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [NormalDist createBegin: aZone];
  
  // Connect the supplied random generator:
  
  [aDistribution setGenerator: generator];
  
  return [aDistribution createEnd];
}


+ createWithDefaults: aZone
{
  NormalDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [NormalDist createBegin: aZone];
  
// Connect a default random generator:
  
  [aDistribution setGenerator: [MWCAgen createWithDefaults: aZone]];
  
  return [aDistribution createEnd];
}


+ create            : aZone 
        setGenerator: generator 
 setVirtualGenerator: (unsigned)vGen
{
  NormalDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [NormalDist createBegin: aZone];
  
  // Connect the supplied random generator:
  
  [aDistribution setGenerator: generator
                 setVirtualGenerator: vGen];
  
  return [aDistribution createEnd];
}

+ create        : aZone
    setGenerator: generator
         setMean: (double)mean
     setVariance: (double)variance
{
  NormalDist *aDistribution;
  
  aDistribution = [NormalDist create: aZone setGenerator: generator];
  
  [aDistribution setMean: mean setVariance: variance];
  
  return aDistribution;
}

+ create              : aZone
          setGenerator: generator
   setVirtualGenerator: (unsigned) vGen
               setMean: (double) mean
           setVariance: (double) variance
{
  NormalDist * aDistribution;
  
  aDistribution = [NormalDist create: aZone
                              setGenerator: generator 
                              setVirtualGenerator: vGen];
  
  [aDistribution setMean: mean setVariance: variance];
  
  return aDistribution;
}


PHASE(Setting)

#include "include.dists.setting.m"

- resetState
{
  // called by setGenerator and setMean:variance
  
  stored = NO;
  stored_double = 0.0;
  currentCount = 0;
  
  return self;
}

- setMean: (double)mean setVariance: (double)variance
{
  /*
    // Relax this restriction, too.
    
    if (optionsInitialized)
    [InvalidCombination raiseEvent:
    "%s: re-setting parameters not allowed\n",distName];
  */
  
  if (variance < 0.0)
    [InvalidCombination raiseEvent:
                          "%s: Variance cannot be negative !\n", distName];
  
  theMean     = mean;
  theVariance = variance;
  theStdDev   = sqrt (variance);
  
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

- (double)getVariance
{
  return theVariance;
}

- (double)getStdDev
{
  return theStdDev;
}


- (double)getSampleWithMean: (double)mean withVariance: (double)variance
{
  double fac,radius,v1,v2;
  double rd1Value, rd2Value;
  double stdDev;
  
  /* 
     // Allow this call even if parameters are set!
     
     if (optionsInitialized)
     [InvalidCombination raiseEvent:
     "%s: getSampleWithMean:withVariance: parameters are frozen\n",distName];
  */
  
  if (variance < 0.0)
    [InvalidCombination 
      raiseEvent:
        "%s: getSampleWithMean:withVariance: Variance cannot be negative !\n",
      distName];
  
  currentCount++;
  
  if (variance == 0.0)
    return mean;		// no need to exercise the machinery ...
  
  stdDev = sqrt (variance);
  
  if (stored) 
    {
      stored = NO;
      // Return stored value:
      return stored_double*stdDev + mean;  // use parameters of current call
    }
  else 
    {
      stored = YES;
      // Generate 2 new values & store 1:
      do {
#ifdef USETHINDOUBLES
        if (useSplitGenerator) 
          {
            rd1Value = [randomGenerator getThinDoubleSample: virtualGenerator]; 
            rd2Value = [randomGenerator getThinDoubleSample: virtualGenerator];
          } 
        else
          {
            rd1Value = [randomGenerator getThinDoubleSample]; 
            rd2Value = [randomGenerator getThinDoubleSample];
          } 
#else
        if (useSplitGenerator) 
          {
            rd1Value = [randomGenerator getDoubleSample: virtualGenerator]; 
            rd2Value = [randomGenerator getDoubleSample: virtualGenerator];
          } 
        else
          {
            rd1Value = [randomGenerator getDoubleSample]; 
            rd2Value = [randomGenerator getDoubleSample];
          } 
#endif
        v1 = (2.0 * rd1Value) - 1.0;
        v2 = (2.0 * rd2Value) - 1.0;
        radius = v1*v1 + v2*v2; 
      } while (radius >= 1.0);
      fac = sqrt (-2.0 * log (radius) / radius);
      stored_double = v1 * fac;
      return v2 * fac * stdDev + mean;	// use parameters of current call
    }
}


- (double)getDoubleSample
{
  double fac, radius, v1, v2;
  double rd1Value, rd2Value;
  
  if (!optionsInitialized)
    [InvalidCombination
      raiseEvent:
        "%s: getDoubleSample: parameters have not been set\n",distName];
  
  currentCount++;
  
  if (theStdDev == 0.0)
    return theMean;		// no need to exercise the machinery ...
  
  if (stored) 
    {
      stored = NO;
      // Return stored value:
      return stored_double * theStdDev + theMean;	// use fixed params
    } 
  else
    {
      stored = YES;
      // Generate 2 new values & store 1:
      do {
#ifdef USETHINDOUBLES
        if (useSplitGenerator)
          {
            rd1Value = [randomGenerator getThinDoubleSample: virtualGenerator]; 
            rd2Value = [randomGenerator getThinDoubleSample: virtualGenerator];
          }
        else 
          {
            rd1Value = [randomGenerator getThinDoubleSample]; 
            rd2Value = [randomGenerator getThinDoubleSample];
          } 
#else
        if (useSplitGenerator)
          {
            rd1Value = [randomGenerator getDoubleSample: virtualGenerator]; 
            rd2Value = [randomGenerator getDoubleSample: virtualGenerator];
          } 
        else
          {
            rd1Value = [randomGenerator getDoubleSample]; 
            rd2Value = [randomGenerator getDoubleSample];
          } 
#endif
        v1 = (2.0 * rd1Value) - 1.0;
        v2 = (2.0 * rd2Value) - 1.0;
        radius = v1*v1 + v2*v2; 
      } while (radius >= 1.0);
      fac = sqrt (-2.0 * log (radius) / radius);
      stored_double = v1 * fac;
      return v2 * fac * theStdDev + theMean;	// use fixed params
    }
}


- (void)putStateInto: (void *)buffer
{
  state_struct_t *internalState;
  
  // recast the caller's pointer:
  internalState = (state_struct_t *)buffer;
  
  // Fill the caller's buffer with state data:
  // Object identification:
  internalState->distMagic = distMagic;
  internalState->stateSize = stateSize;
  // Generator data:
  internalState->genMagic = (unsigned) [randomGenerator getMagic];
  internalState->useSplitGenerator = useSplitGenerator;
  internalState->virtualGenerator = virtualGenerator;
  // Fixed parameters:
  internalState->optionsInitialized = optionsInitialized;
  internalState->theMean = theMean;
  internalState->theVariance = theVariance;
  // State variables:
  internalState->stored = stored;
  internalState->stored_double = stored_double;
  internalState->currentCount = currentCount;
  
  // nothing is returned from a (void) function
}


- (void)setStateFrom: (void *)buffer
{
  state_struct_t *internalState;
  
  // recast the caller's pointer:
  internalState = (state_struct_t *) buffer;
  
  // TEST the external data (object identification):
  
  if ((internalState->distMagic != distMagic)
      || (internalState->stateSize != stateSize))
    [InvalidCombination
      raiseEvent:
        "%u %s : you are passing bad data to setState!\n %u %u\n",
      distMagic, distName,
      internalState->distMagic, internalState->stateSize];
  
  // set internal state from data in caller's buffer:
  
  // Fixed parameters:
  optionsInitialized = internalState->optionsInitialized;
  theMean       = internalState->theMean;
  theVariance   = internalState->theVariance;
  theStdDev     = sqrt(theVariance);
  // State variables:
  stored        = internalState->stored;
  stored_double = internalState->stored_double;
  currentCount  = internalState->currentCount;

  // Test generator data:

  if (((unsigned) [randomGenerator getMagic] != internalState->genMagic )
      || (useSplitGenerator != internalState->useSplitGenerator)
      || (virtualGenerator  != internalState->virtualGenerator))
    printf("%s setState: Warning! Not using the same generator!\n", distName);
  
  // nothing is returned from a (void) function
}


- (void)describe: outStream
{
  char buffer[200];
  
  (void)sprintf (buffer," %s describe: outStream: \n", distName);
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
  (void)sprintf (buffer,"        theVariance = %24.16e\n", theVariance);
  [outStream catC: buffer];
  (void)sprintf (buffer,"          theStdDev = %24.16e\n", theStdDev);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            stored? = %24d\n", stored);
  [outStream catC: buffer];
  (void)sprintf (buffer,"      stored_double = %24f\n", stored_double);
  [outStream catC: buffer];
  (void)sprintf (buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];
  
  [outStream catC: "\n"];
  
  //  return self;
}

@end
