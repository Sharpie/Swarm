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
Name:            PoissonDist.m
Description:     Poisson distribution returning integers
Library:         random
Original Author: Steve Jackson
Date:            2001-01-08 
Modified by:	 Sven Thommesen
Date:		 2001-07-17
Changes:	 Minor bug fixes
		 Changed the size of state_struct_t so POISSONDISTREVISION++
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>
#import <random/PoissonDist.h>

#include <misc.h> // log, pow, sqrt, exp
#include <math.h> 

#ifndef PI
  #define PI 3.141592654
#endif


@implementation PoissonDist

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
   double occurRate;
   double interval;
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;


PHASE(Creating)

#include "include.dists.creating.m"

- initState
{
  // Distribution personality:
  stateSize = sizeof (state_struct_t);
  strncpy (distName, "PoissonDist", sizeof (distName));
  distMagic = POISSONDISTMAGIC + POISSONDISTREVISION;

  // Parameters:
  optionsInitialized = NO;
  useSplitGenerator  = NO;
  virtualGenerator   = MAXVGEN;

  occurRate = -1;
  interval = -1;
  
  // Working Variables
  //
  // The following array is used in getGammaLn
  // 
  cof[0] =  76.18009173;
  cof[1] = -86.50532033;
  cof[2] =  24.01409822;
  cof[3] = -1.231739516;
  cof[4] =  0.120858003e-2;
  cof[5] = -0.536382e-5;

  
#ifdef USETHINDOUBLES
  fprintf(stderr, "NOTE! %s: created to use THIN doubles\n",distName);
  fflush(stderr);
#endif
  
  return self;
}


+ createBegin: (id <Zone>)aZone
{
  PoissonDist *aDistribution;
  
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
  PoissonDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [PoissonDist createBegin: aZone];

  // Connect the supplied random generator:
  [aDistribution setGenerator: generator];
  
  return [aDistribution createEnd];
}


+ createWithDefaults: (id <Zone>)aZone
{
  PoissonDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [PoissonDist createBegin: aZone];
  
  // Connect a default random generator:
  [aDistribution setGenerator: [PSWBgen createWithDefaults: aZone] ];

  return [aDistribution createEnd];
}


+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator 
  setVirtualGenerator: (unsigned)vGen
{
  PoissonDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [PoissonDist createBegin: aZone];


  // Connect the supplied random generator:
  [aDistribution setGenerator: generator
                 setVirtualGenerator: vGen];
  
  return [aDistribution createEnd];
}

+ create: (id <Zone>)aZone 
     setGenerator: (id <SimpleRandomGenerator>)generator
     setOccurRate: (double) anOccurRate
      setInterval: (double) anInterval
{    
  PoissonDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [PoissonDist create: aZone setGenerator: generator];

  [aDistribution setOccurRate: anOccurRate
                  setInterval: anInterval];

  return aDistribution;
}

+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator 
  setVirtualGenerator: (unsigned)vGen
         setOccurRate: (double) anOccurRate
           setInterval: (double) anInterval
{
  PoissonDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [PoissonDist create: aZone
			setGenerator: generator setVirtualGenerator: vGen];
  
  [aDistribution setOccurRate: anOccurRate
                  setInterval: anInterval];

  return aDistribution;
}


PHASE(Setting)

#include "include.dists.setting.m"

- setInterval: (double) anInterval
{
   interval = anInterval;
   if (occurRate >= 0) optionsInitialized = YES;

   return self;
}

- setOccurRate: (double) anOccurRate 
{
  occurRate = anOccurRate;
  if (interval >= 0) optionsInitialized = YES;

  return self;
}

- setOccurRate: (double) anOccurRate 
   setInterval: (double) anInterval
{
  occurRate = anOccurRate;
  interval = anInterval;
  optionsInitialized = YES;

  return self;
}


- resetState
{
  currentCount = 0;
  
  return self;
}


PHASE(Using)

#include "include.dists.using.m"

- (double) getOccurRate
{
    return occurRate;
}

- (double) getInterval
{
   return interval;
}


////////////////////////////////////////////
//
// getGammaLn is for use only with 
// getUnsignedSampleWithOccurRate:withInterval: 
// i.e it has not been generalized
//
///////////////////////////////////////////
- (double) getGammaLn: (double) arg
{
  double gammaLn = 0;
  int j;
  double x, tmp, ser;
                           
  if(arg < 1.0) 
  {
      [InvalidCombination raiseEvent:
         "%s: getGammaLn: is being passed a value less than 1\n", distName];
     
  }

  x = arg - 1.0;

  tmp = x + 5.5;
  gammaLn = (x + 0.5)*log(tmp) - tmp;
 
  ser = 1.0;

  for(j=0; j <= 5; j++) 
  {
      x += 1.0;
      ser += cof[j]/x;
  }

  gammaLn = gammaLn + log(2.50662827465 * ser);

  return gammaLn;

}


- (unsigned) getUnsignedSampleWithOccurRate: (double) anOccurRate
                         withInterval: (double) anInterval
{
  double sq=0; 
  double alxm=0;
  double g=0;
  double em=0;
  double t=0;
  double y=0;
  double myOccurRate;

  unsigned returnVal;

  if((anOccurRate < 0) || (anInterval < 0))
  {
      [InvalidCombination raiseEvent:
 "%s: getUnsignedSampleWithOccurRate:andInterval: passing a negative value \n", 
	distName];
  }

  currentCount++ ;

  myOccurRate = anOccurRate*anInterval;

  if(myOccurRate < 12)
  {
     g = exp(-myOccurRate);
     
     em = -1;
     t = 1.0;

     do
     {
        em += 1.0;
        t *= [self getMyDoubleSample];

     } while (t > g);
  }
  else
  {
      sq = sqrt(2.0 * myOccurRate);
      alxm = log(myOccurRate);

      //
      // (myOccurRate + 1) should be >= 1.0
      //
      g = myOccurRate*alxm - [self getGammaLn: (myOccurRate + 1.0)];
     
      do 
      {
         do
         {
            y = tan(PI * [self getMyDoubleSample]); 
            em = sq*y + myOccurRate;

         } while(em < 0.0);
    
         em = floor(em);

         //
         // (em + 1) should be >= 1.0
         //
         t = 0.9*(1.0 + y*y)*exp(em*alxm - [self getGammaLn: (em + 1.0)] - g);

      } while ([self getMyDoubleSample] > t);
  }
  
  returnVal = (unsigned int) em;

  // Test for returnVal overflowing, or em being negative:

  if( (double) returnVal != em )
  {
     [InvalidCombination raiseEvent:
"%s:%f:%u: getUnsignedSampleWithOccurRate:andInterval: ERROR in calculation\n", 
	distName, em, returnVal];
  }

  return returnVal;
}


/////////////////////////////////////////////////////////
//
//  getMyDoubleSample method is just a convenience to
//  to keep the code clean
//
/////////////////////////////////////////////////////////
- (double) getMyDoubleSample 
{

#ifdef USETHINDOUBLES
  if (useSplitGenerator)
    return [randomGenerator getThinDoubleSample: virtualGenerator];
  else
    return [randomGenerator getThinDoubleSample];
#else
  if (useSplitGenerator)
    return [randomGenerator getDoubleSample: virtualGenerator];
  else
    return [randomGenerator getDoubleSample];
#endif 
 
[InvalidCombination raiseEvent: "ERROR: PoissonDist >>>> getMyDoubleSample\n"];
   
} 


- (unsigned) getUnsignedSample
{
  if((occurRate < 0) || (interval < 0))
  {
      optionsInitialized = NO;
  }

  if (!optionsInitialized)
    [InvalidCombination
      raiseEvent:
        "%s: getUnsignedSample: parameters have not been set\n", distName];
  
  // currentCount++; // taken care of in the method called below

  return [self getUnsignedSampleWithOccurRate: occurRate
                                withInterval: interval];

}


- (unsigned) getUnsignedSampleWithInterval: (double) anInterval
{
  if (!optionsInitialized)
    [InvalidCombination raiseEvent:
       "%s: getUnsignedSample: parameters have not been set\n", distName];
  
  // currentCount++; // taken care of in the method called below

  return [self getUnsignedSampleWithOccurRate: occurRate
                                withInterval: anInterval];
}


- (void)putStateInto: (void *)buffer
{
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
  internalState->occurRate = occurRate;
  internalState->interval = interval;
  // state variables:
  internalState->currentCount = currentCount;
  
  // nothing is returned from a (void) function
}

// Note: before a PoissonDist can set its own state here,
// it must have been created normally, so it is assumed that
// the cof[] array has been properly initialized!
// 
- (void)setStateFrom: (void *)buffer
{
  state_struct_t * internalState;
  
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
  occurRate = internalState->occurRate;
  interval = internalState->interval;

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
  (void)sprintf (buffer,"          occurRate = %24f\n", occurRate);
  [outStream catC: buffer];
  (void)sprintf (buffer,"           interval = %24f\n", interval);
  [outStream catC: buffer];
  (void)sprintf (buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];
  
  [outStream catC: "\n"];
  
  //  return self;
}

@end
