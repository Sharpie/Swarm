// Swarm library. Copyright � 1996-1999 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            UniformDoubleDist.m
Description:     Uniform distribution returning double values
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


#import <collections.h>
#import <random/UniformDoubleDist.h>


@implementation UniformDoubleDist

// data struct used by setStateFrom / putStateInto:
//
typedef struct {
   // Object identification:
   unsigned distMagic;
   unsigned stateSize;
   // Generator data:
   unsigned int genMagic;
   BOOL useSplitGenerator;
   unsigned virtualGenerator;
   // Fixed parameters:
   BOOL optionsInitialized;
   BOOL   bSingular;
   double doubleRange;
   double doubleMin;
   double doubleMax;
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;


PHASE(Creating)

#include "include.dists.creating.m"

- initState
{
  // Distribution personality:

  stateSize = sizeof(state_struct_t);
  strncpy (distName, "UniformDoubleDist", sizeof (distName));
  distMagic = UNIFORMDOUBLEMAGIC + UNIFORMDOUBLEREVISION;
  
  // Parameters:
  
  optionsInitialized = NO;
  useSplitGenerator  = NO;
  virtualGenerator   = MAXVGEN;
  
  doubleMin   = 0.0;
  doubleMax   = 0.0;
  doubleRange = 0.0;
  bSingular   = YES;
  
#ifdef USETHINDOUBLES
  printf ("NOTE! %s: created to use THIN doubles\n",distName);
#endif
  
  return self;
}

+ createBegin: aZone
{
  UniformDoubleDist *aDistribution;
  
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
  UniformDoubleDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [UniformDoubleDist createBegin: aZone];

  // Connect the supplied random generator:
  
  [aDistribution setGenerator: generator];
  
  return [aDistribution createEnd];
}


+ createWithDefaults: aZone
{
  UniformDoubleDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [UniformDoubleDist createBegin: aZone];
  
  // Connect a default random generator:

  [aDistribution setGenerator: [TT800gen createWithDefaults: aZone]];
  
  return [aDistribution createEnd];
}


+ create             : aZone
         setGenerator: generator 
  setVirtualGenerator: (unsigned)vGen
{
  UniformDoubleDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [UniformDoubleDist createBegin: aZone];
  
  // Connect the supplied random generator:
  
  [aDistribution setGenerator: generator
                 setVirtualGenerator: vGen];
  
  return [aDistribution createEnd];
}

+ create        : aZone
    setGenerator: generator
    setDoubleMin: (double)minValue
          setMax: (double)maxValue
{
  UniformDoubleDist *aDistribution;
  
  aDistribution = [UniformDoubleDist create: aZone setGenerator: generator];
  
  [aDistribution setDoubleMin: minValue setMax: maxValue];
  
  return aDistribution;
}

+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
         setDoubleMin: (double)minValue
               setMax: (double)maxValue
{
  UniformDoubleDist *aDistribution;
  
  aDistribution = [UniformDoubleDist create: aZone
                                     setGenerator: generator 
                                     setVirtualGenerator: vGen];
  
  [aDistribution setDoubleMin: minValue setMax: maxValue];
  
  return aDistribution;
}


PHASE(Setting)

#include "include.dists.setting.m"

- resetState
{
  currentCount = 0;
  
  return self;
}

- setDoubleMin: (double)minValue setMax: (double)maxValue
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
      doubleMin = doubleMax = minValue;
      doubleRange = 0.0;
    }
  else
    {
      bSingular = NO;
      
      // Ensure that doubleMax > doubleMin:
      
      if (maxValue > minValue)
        {
          doubleMax = maxValue;
          doubleMin = minValue;
        }
      else
        {
          doubleMax = minValue;
          doubleMin = maxValue;
        }
      doubleRange = doubleMax - doubleMin;
    }
  
   // This object is now fixed:
  
  optionsInitialized = YES;
  
  [self resetState];
  
  return self;
}


PHASE(Using)

#include "include.dists.using.m"


- (double)getDoubleMin
{
  return doubleMin;
}

- (double)getDoubleMax
{
  return doubleMax;
}


- (double)getDoubleWithMin: (double)minValue withMax: (double)maxValue
{
  double tmpMin, tmpMax, tmpRange;
  
  /*
    // Allow this call even if parameters are set!
    
    if (optionsInitialized)
    [InvalidCombination raiseEvent:
    "%s: getDoubleWithMin:withMax: options already initialized\n", distName];
  */
  
  currentCount++;
  
  if (minValue == maxValue) 
    return minValue;
  else
    {
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
      
      // Generate a value in [tmpMin, tmpMax)
      // (using values supplied for this call only)
      
#ifdef USETHINDOUBLES
      if (useSplitGenerator)
        return (tmpMin + tmpRange * 
                ([randomGenerator getThinDoubleSample: virtualGenerator]));
      else
        return (tmpMin + tmpRange * ([randomGenerator getThinDoubleSample]));
#else
      if (useSplitGenerator)
        return (tmpMin + tmpRange * 
                ([randomGenerator getDoubleSample: virtualGenerator]));
      else
        return (tmpMin + tmpRange * ([randomGenerator getDoubleSample]));
#endif
    }
}


- (double)getDoubleSample
{
  
  if (!optionsInitialized)
    [InvalidCombination 
      raiseEvent:
        "%s: getDoubleSample: parameters have not been set\n", distName];
  
  currentCount++;
  
  if (bSingular)
    return doubleMin;
  else
    {
      // Generate a value in [doubleMin, doubleMax)
      // (using fixed values set at creation)
      
#ifdef USETHINDOUBLES
      if (useSplitGenerator)
        return (doubleMin + doubleRange * 
                ([randomGenerator getThinDoubleSample:virtualGenerator]));
    else
      return (doubleMin + doubleRange * 
              ([randomGenerator getThinDoubleSample]));
#else
      if (useSplitGenerator)
        return (doubleMin + doubleRange * 
                ([randomGenerator getDoubleSample:virtualGenerator]));
      else
        return (doubleMin + doubleRange * ([randomGenerator getDoubleSample]));
#endif
    }
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
  // fixed parameters:
  internalState->optionsInitialized = optionsInitialized;
  internalState->doubleMin = doubleMin;
  internalState->doubleMax = doubleMax;
  internalState->doubleRange = doubleRange;
  internalState->bSingular = bSingular;
  // state variables:
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
  doubleMin   = internalState->doubleMin;
  doubleMax   = internalState->doubleMax;
  doubleRange = internalState->doubleRange;
  bSingular   = internalState->bSingular;
  
  // State variables:
  currentCount = internalState->currentCount;
  
  // Test generator data:
  
  if (((unsigned) [randomGenerator getMagic] != internalState->genMagic)
      || ( useSplitGenerator != internalState->useSplitGenerator)
      || ( virtualGenerator  != internalState->virtualGenerator))
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
  (void)sprintf (buffer,"          doubleMin = %24.16e\n", doubleMin);
  [outStream catC: buffer];
  (void)sprintf (buffer,"          doubleMax = %24.16e\n", doubleMax);
  [outStream catC: buffer];
  (void)sprintf (buffer,"        doubleRange = %24.16e\n", doubleRange);
  [outStream catC: buffer];
  (void)sprintf (buffer,"          bSingular = %24d\n", bSingular);
  [outStream catC: buffer];
  (void)sprintf (buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  [outStream catC: "\n"];

  //  return self;
}

@end
