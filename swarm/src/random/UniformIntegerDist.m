// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:            UniformIntegerDist.m
Description:     Uniform distribution returning integers
Library:         random
Original Author: Sven Thommesen
Date:            1997-01-15
Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Modified by:	 Sven Thommesen
Date:		 1998-10-08 (v. 0.8)
Modified by:	 Sven Thommesen
Date:		 2000-02-21 (v. 0.81)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <math.h>

#import <collections.h>
#import <random/UniformIntegerDist.h>


@implementation UniformIntegerDist

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
   int iMin;
   int iMax;
   unsigned uRange;
   unsigned uCutoff;
   BOOL bSingular;
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;


PHASE(Creating)

#include "include.dists.creating.m"

- initState
{
// Distribution personality:
  
  stateSize = sizeof (state_struct_t);
  strncpy (distName, "UniformIntegerDist", sizeof (distName));
  distMagic = UNIFORMINTEGERMAGIC + UNIFORMINTEGERREVISION;

  // Parameters:
  
  optionsInitialized = NO;
  useSplitGenerator  = NO;
  virtualGenerator   = MAXVGEN;
  
  iMin     = 0;
  iMax     = 0;
  uRange   = 0;
  uCutoff  = 0;
  bSingular = YES;
  
  return self;
}

+ createBegin: (id <Zone>)aZone
{
  UniformIntegerDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [super createBegin: aZone];
  
  // Initialize instance variables:
  
  aDistribution->randomGenerator = NULL;
  
  // Initialize parameters:
  
  [aDistribution initState];

  return aDistribution;
}


+ create: (id <Zone>)aZone setGenerator: (id <SimpleRandomGenerator>)generator
{
  UniformIntegerDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [UniformIntegerDist createBegin: aZone];
  
  // Connect the supplied random generator:
  
  [aDistribution setGenerator: generator];
  
  return [aDistribution createEnd];
}


+ createWithDefaults: (id <Zone>)aZone
{
   UniformIntegerDist *aDistribution;
   
   // Allocate space for the object:
   
   aDistribution = [UniformIntegerDist createBegin: aZone];
   
   // Connect a default random generator:
   
   [aDistribution setGenerator: [TT403gen createWithDefaults: aZone] ];
   
   return [aDistribution createEnd];
}


+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator 
  setVirtualGenerator: (unsigned)vGen
{
  UniformIntegerDist *aDistribution;
  
  // Allocate space for the object:
  
  aDistribution = [UniformIntegerDist createBegin: aZone];
  
// Connect the supplied random generator:
  
  [aDistribution setGenerator: generator
                 setVirtualGenerator: vGen];
  
  return [aDistribution createEnd];
}

+ create         : (id <Zone>)aZone
     setGenerator: (id <SimpleRandomGenerator>)generator
    setIntegerMin: (int)minValue
           setMax: (int)maxValue
{
  UniformIntegerDist *aDistribution;
  
  aDistribution = [UniformIntegerDist create: aZone setGenerator: generator];
  
  [aDistribution setIntegerMin: minValue setMax: maxValue];
  
  return aDistribution;
}

+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator
  setVirtualGenerator: (unsigned)vGen
        setIntegerMin: (int)minValue
               setMax: (int)maxValue
{
  UniformIntegerDist *aDistribution;
  
  aDistribution = [UniformIntegerDist create: aZone setGenerator: generator 
                                      setVirtualGenerator: vGen];
  
  [aDistribution setIntegerMin: minValue setMax: maxValue];
  
  return aDistribution;
}


PHASE(Setting)

#include "include.dists.setting.m"

- resetState
{
  currentCount = 0;
  
  return self;
}

- setIntegerMin: (int)minValue setMax: (int)maxValue
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
      iMax = iMin = minValue;
      uRange = 0;
      uCutoff = 0;
    }
  else
    {
      bSingular = NO;
      
      // Ensure that iMax > iMin:
      
      if (maxValue > minValue)
        {
          iMax = maxValue;
          iMin = minValue;
        } 
      else
        {
          iMax = minValue;
          iMin = maxValue;
        }
      uRange = iMax - iMin;

      if (uRange > generatorMax - 1)
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


- (int)getIntegerMin
{
  return iMin;
}

- (int)getIntegerMax
{
  return iMax;
}

// ----- Generate random numbers: -----


- (int)getIntegerWithMin: (int)minValue withMax: (int)maxValue
{
  int tmpMax, tmpMin;
  unsigned uValue;
  unsigned tmpRange, tmpCut;
  int iValue;
  
  /*
    // Allow this call even if parameters are set!
    
    if (optionsInitialized)
    [InvalidCombination raiseEvent:
    "%s: getIntegerWithMin:withMax: options already initialized\n", distName];
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
      distName, tmpRange, generatorMax-1];
  
  tmpRange = tmpRange + 1;
  tmpCut = generatorMax - (generatorMax % tmpRange);
  // tmpCut = (generatorMax / tmpRange) * tmpRange;
  
  // uCutoff is set to be a multiple of uRange,
  // to ensure a bias free uniform variable.
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
   
   iValue = (int) (uValue % tmpRange);
   
   return (iValue + tmpMin);

}

// Return integer value in range [min,max] (inclusive).

- (int)getIntegerSample
{
  unsigned uValue;
  int iValue;
  
  if (!optionsInitialized)
    [InvalidCombination
      raiseEvent:
        "%s: getIntegerSample: parameters have not been set\n", distName];
  
  currentCount++;
  
   if (bSingular)
     return iMin;
   
   // uCutoff is set to be a multiple of uRange,
   // to ensure a bias free uniform variable.
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
   
   iValue = (int) (uValue % uRange);

   return (iValue + iMin);
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
  internalState->iMin = iMin;
  internalState->iMax = iMax;
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
  iMin      = internalState->iMin;
  iMax      = internalState->iMax;
  uRange    = internalState->uRange;
  bSingular = internalState->bSingular;
  uCutoff   = internalState->uCutoff;
  
  // State variables:
  currentCount = internalState->currentCount;
  
  // Test generator data:
  
  if (((unsigned) [randomGenerator getMagic] != internalState->genMagic)
      || (useSplitGenerator != internalState->useSplitGenerator)
      || (virtualGenerator  != internalState->virtualGenerator))
    {
      printf ("%s setState: Warning! Not using the same generator!\n",
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
  (void)sprintf (buffer,"               iMin = %24d\n", iMin);
  [outStream catC: buffer];
  (void)sprintf (buffer,"               iMax = %24d\n", iMax);
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
