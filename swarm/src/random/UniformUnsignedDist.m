// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            UniformUnsignedDist.m
Description:     Uniform distribution returning unsigned integers
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
#import <random/UniformUnsignedDist.h>


@implementation UniformUnsignedDist

// data struct used by setStateFrom / putStateInto:
//
typedef struct {
   // Object identification:
   unsigned distMagic;
   unsigned stateSize;
   // Generator data:
   unsigned int genMagic;
   BOOL useSplitGenerator;
   unsigned int virtualGenerator;
   // Fixed parameters:
   BOOL optionsInitialized;
   unsigned uMin;
   unsigned uMax;
   unsigned uRange;
   unsigned uCutoff;
   BOOL     bSingular;
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;


PHASE(Creating)

#include "include.dists.creating.m"

- initState
{
// Distribution personality:

   stateSize = sizeof(state_struct_t);
   strncpy (distName, "UniformUnsignedDist", sizeof (distName));
   distMagic = UNIFORMUNSIGNEDMAGIC + UNIFORMUNSIGNEDREVISION;

// Parameters:

   optionsInitialized = NO;
   useSplitGenerator  = NO;
   virtualGenerator   = MAXVGEN;

   uMin     = 0;
   uMax     = 0;
   uRange   = 0;
   uCutoff  = 0;
   bSingular = YES;

   return self;
}

+ createBegin: aZone
{
  UniformUnsignedDist *aDistribution;
  
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
  UniformUnsignedDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [UniformUnsignedDist createBegin: aZone];
  
  // Connect the supplied random generator:

  [aDistribution setGenerator: generator];
  
  return [aDistribution createEnd];
}


+ createWithDefaults: aZone
{
  UniformUnsignedDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [UniformUnsignedDist createBegin: aZone];
  
  // Connect a default random generator:
  
  [aDistribution setGenerator: [TT775gen createWithDefaults: aZone]];
  
  return [aDistribution createEnd];
}


+ create             : aZone
         setGenerator: generator 
  setVirtualGenerator: (unsigned)vGen
{
  UniformUnsignedDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [UniformUnsignedDist createBegin: aZone];
  
  // Connect the supplied random generator:
  
  [aDistribution setGenerator: generator
                 setVirtualGenerator: vGen];
  
  return [aDistribution createEnd];
}

+ create        : aZone
    setGenerator: generator
  setUnsignedMin: (unsigned) minValue
          setMax: (unsigned) maxValue
{
  UniformUnsignedDist *aDistribution;
  
  aDistribution = [UniformUnsignedDist create: aZone setGenerator: generator];
  
  [aDistribution setUnsignedMin: minValue setMax: maxValue];
  
  return aDistribution;
}

+ create            : aZone
        setGenerator: generator
 setVirtualGenerator: (unsigned)vGen
      setUnsignedMin: (unsigned)minValue
              setMax: (unsigned)maxValue
{
  UniformUnsignedDist *aDistribution;
  
   aDistribution = [UniformUnsignedDist create: aZone setGenerator: generator 
                                        setVirtualGenerator: vGen];
   
   [aDistribution setUnsignedMin: minValue setMax: maxValue];
   
   return aDistribution;
}


PHASE(Setting)

#include "include.dists.setting.m"

- resetState
{
  currentCount = 0;
  
  return self;
}

- setUnsignedMin: (unsigned)minValue setMax: (unsigned)maxValue
{
  /*
    // Relax this restriction, too.
    
    if (optionsInitialized)
    [InvalidCombination raiseEvent:
    "%s: setting parameters more than once not allowed\n", distName];
  */
  
  if (minValue == maxValue)
    {
      bSingular = YES;
      uMax = uMin = minValue;
      uRange = 0;
      uCutoff = 0;
    }
  else
    {
      bSingular = NO;
      
      // Ensure that uMax > uMin:
      
      if (maxValue > minValue)
        {
          uMax = maxValue;
          uMin = minValue;
        }
      else
        {
          uMax = minValue;
          uMin = maxValue;
        }
      uRange = uMax - uMin;
      
      if (uRange > generatorMax-1)
        [InvalidCombination
          raiseEvent:
            "%s: Requested random number with range %u, \nbut your generator only supports a range of %u \n", 
          distName, uRange, generatorMax - 1];

      uRange = uRange + 1;
      uCutoff = generatorMax - (generatorMax % uRange);
      // uCutoff = (generatorMax / uRange) * uRange;
    }
  
  // This object is now fixed:
  
  optionsInitialized = YES;
  
  [self resetState];
  
  return self;
}


PHASE(Using)

#include "include.dists.using.m"


- (unsigned)getUnsignedMin
{
  return uMin;
}

- (unsigned)getUnsignedMax
{
  return uMax;
}


// Return unsigned integer value in range [min,max] (inclusive).

- (unsigned)getUnsignedWithMin: (unsigned)minValue
                       withMax: (unsigned)maxValue
{
  unsigned tmpMax, tmpMin;
  unsigned uValue;
  unsigned tmpRange, tmpCut;
  
  /*
    // Allow this call even if parameters are set!
    
    if (optionsInitialized)
    [InvalidCombination raiseEvent:
    "%s: getUnsignedWithMin:withMax: options already initialized\n", distName];
  */
  
  currentCount++;
  
  if (minValue == maxValue)
    return minValue;
  
  
  // Ensure tmpMax > tmpMin:
  
  if (maxValue > minValue)
    {
      tmpMax = maxValue;
      tmpMin = minValue;
    } 
  else
    {
      tmpMax = minValue;
      tmpMin = maxValue;
    }
  tmpRange = tmpMax - tmpMin;
  
  if (tmpRange > generatorMax-1)
    [InvalidCombination 
      raiseEvent:
        "%s: Requested random number with range %u, \nbut your generator only supports a range of %u \n", 
      distName, tmpRange, generatorMax - 1];
  
  tmpRange = tmpRange + 1;
  tmpCut = generatorMax - (generatorMax % tmpRange);
  // tmpCut = (generatorMax / tmpRange) * tmpRange;
  
  // uCutoff is set to be a multiple of uRange
  // to ensure an unbiased uniform variable.
  // Suggested by Barry McMullin.
  
  if (useSplitGenerator)
    {
      do {
        uValue = [randomGenerator getUnsignedSample: virtualGenerator];
      } while (uValue >= tmpCut);
    } 
  else
    {
      do {
        uValue = [randomGenerator getUnsignedSample];
      } while (uValue >= tmpCut);
    }
  
  uValue = (uValue % tmpRange) + tmpMin;
  
  return uValue;
}

// Return unsigned integer value in range [min,max] (inclusive).

- (unsigned)getUnsignedSample
{
  unsigned uValue;
  
  if (!optionsInitialized)
    [InvalidCombination
      raiseEvent:
        "%s: getUnsignedSample: parameters have not been set\n", distName];
  
  currentCount++;
  
  if (bSingular)
    return uMin;
  
  
  // uCutoff is set to be a multiple of uRange
  // to ensure an unbiased uniform variable.
   // Suggested by Barry McMullin.

   if (useSplitGenerator)
     {
       do {
         uValue = [randomGenerator getUnsignedSample: virtualGenerator];
       } while (uValue >= uCutoff);
     } 
   else
     {
       do {
         uValue = [randomGenerator getUnsignedSample];
       } while (uValue >= uCutoff);
     }
   
   uValue = (uValue % uRange) + uMin;
   
   return uValue;
}


- (void)putStateInto: (void *)buffer
{
  state_struct_t *internalState;
  
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
  internalState->uMin = uMin;
  internalState->uMax = uMax;
  internalState->uRange = uRange;
  internalState->uCutoff = uCutoff;
  internalState->bSingular = bSingular;
  // state variables:
  internalState->currentCount = currentCount;

  // nothing is returned from a (void) function

}

- (void)setStateFrom: (void *)buffer
{
  state_struct_t *internalState;
  
  // recast the caller's pointer:
  internalState = (state_struct_t *) buffer;
  
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
  uMin   = internalState->uMin;
  uMax   = internalState->uMax;
  uRange   = internalState->uRange;
  uCutoff = internalState->uCutoff;
  bSingular = internalState->bSingular;

  // State variables:
  currentCount = internalState->currentCount;

  // Test generator data:

  if (((unsigned) [randomGenerator getMagic] != internalState->genMagic)
      || ( useSplitGenerator != internalState->useSplitGenerator)
      || ( virtualGenerator  != internalState->virtualGenerator))
    {
      printf ("%s setState: Warning! Not using the same generator\n",
              distName);
      uCutoff = generatorMax - (generatorMax % uRange); // recalc for new gen
    }
  
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
  (void)sprintf (buffer,"       generatorMax = %24u\n", generatorMax);
  [outStream catC: buffer];
  (void)sprintf (buffer,"  useSplitGenerator = %24d\n", useSplitGenerator);
  [outStream catC: buffer];
  (void)sprintf (buffer,"   virtualGenerator = %24u\n", virtualGenerator);
  [outStream catC: buffer];
  (void)sprintf (buffer," optionsInitialized = %24d\n", optionsInitialized);
  [outStream catC: buffer];
  (void)sprintf (buffer,"               uMin = %24u\n", uMin);
  [outStream catC: buffer];
  (void)sprintf (buffer,"               uMax = %24u\n", uMax);
  [outStream catC: buffer];
  (void)sprintf (buffer,"             uRange = %24u\n", uRange);
  [outStream catC: buffer];
  (void)sprintf (buffer,"            uCutoff = %24u\n", uCutoff);
  [outStream catC: buffer];
  (void)sprintf (buffer,"          bSingular = %24d\n", bSingular);
  [outStream catC: buffer];
  (void)sprintf (buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];
  
  
  [outStream catC: "\n"];

  //  return self;
}

@end
