// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

/*
Name:            BinomialDist.m
Description:     Binomial distribution returning unsigned integers
Library:         random
Original Author: Steve Jackson
Date:            2004-04-13 
Modified by:	 
Date:		 
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <collections.h>
//#import <random/BinomialDist.h>
#import "BinomialDist.h"

#include <misc.h> // log, pow, sqrt, exp
#include <math.h> 

@implementation BinomialDist

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
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;


PHASE(Creating)

#include "include.dists.creating.m"

- initState
{
  // Distribution personality:
  
  stateSize = sizeof (state_struct_t);
  strncpy (distName, "BinomialDist", sizeof (distName));
  distMagic = BINOMIALDISTMAGIC + BINOMIALDISTREVISION;

  numTrials = -1;
  probability = -1.0;

  
  // Parameters:
  
  optionsInitialized = NO;
  useSplitGenerator  = NO;
  virtualGenerator   = MAXVGEN;


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
  BinomialDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [super createBegin: aZone];
  
  // Initialize instance variables:
  aDistribution->randomGenerator = nil;

  // Initialize parameters:
  [aDistribution initState];

  return aDistribution;
}


+ create: (id <Zone>)aZone setGenerator: (id <SimpleRandomGenerator>)generator
{
  BinomialDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [BinomialDist createBegin: aZone];

  // Connect the supplied random generator:
  [aDistribution setGenerator: generator];
  
  return [aDistribution createEnd];
}


+ createWithDefaults: (id <Zone>)aZone
{
  BinomialDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [BinomialDist createBegin: aZone];
  
  // Connect a default random generator:
  [aDistribution setGenerator: [PSWBgen createWithDefaults: aZone] ];

  return [aDistribution createEnd];
}


+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator 
  setVirtualGenerator: (unsigned)vGen
{
  BinomialDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [BinomialDist createBegin: aZone];


  // Connect the supplied random generator:
  [aDistribution setGenerator: generator
                 setVirtualGenerator: vGen];
  
  return [aDistribution createEnd];
}


+ create: (id <Zone>)aZone 
     setGenerator: (id <SimpleRandomGenerator>)generator
     setNumTrials: (unsigned) aNumTrials
      setProbability: (double) aProbability
{    
  BinomialDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [BinomialDist createBegin: aZone];

  // Connect the supplied random generator:
  [aDistribution setGenerator: generator];
  
  [aDistribution setNumTrials: aNumTrials
                  setProbability: aProbability];

  return [aDistribution createEnd];
}

+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator 
  setVirtualGenerator: (unsigned)vGen
         setNumTrials: (unsigned) aNumTrials
       setProbability: (double) aProbability
{
  BinomialDist *aDistribution;
  
  // Allocate space for the object:
  aDistribution = [BinomialDist createBegin: aZone];


  // Connect the supplied random generator:
  [aDistribution setGenerator: generator
                 setVirtualGenerator: vGen];
  
  [aDistribution setNumTrials: aNumTrials
               setProbability: aProbability];

  return [aDistribution createEnd];
}


PHASE(Setting)

#include "include.dists.setting.m"

- setNumTrials: (unsigned) aNumTrials 
{

  numTrials = aNumTrials;
  optionsInitialized = YES;

  return self;

}

- setNumTrials  : (unsigned) aNumTrials 
  setProbability: (double) aProbability
{

  numTrials = aNumTrials;
  probability = aProbability;
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

- (unsigned) getNumTrials
{
    return numTrials;
}

- (double) getProbability
{
   return probability;
}



////////////////////////////////////////////
//
// getGammaLn is for use only with 
// getUnsignedSampleWithNumTrials:withInterval: 
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
  



/////////////////////////////////////////////////////////////////
//
// getUnsignedSampleWithNumTrials:withProbability
//
/////////////////////////////////////////////////////////////////
- (unsigned) getUnsignedSampleWithNumTrials: (unsigned) aNumTrials
                         withProbability: (double) aProbability
{
   int j;
   double am = -1.0, em = -1.0, g = -1.0, angle = -1.0, p = -1.0; 
   double bnl = 0.0, sq = -1.0, t = -1.0, y = -1.0;  
   double pc = -1.0, plog = -1.0, pclog = -1.0, en = -1.0, oldg = -1.0;

   if((aProbability < 0.0) || (aProbability > 1.0))
   {
      [InvalidCombination raiseEvent:
              "%s: getUnsignedSampleWithNumTrials:withProbability is being passed a probability that is less than 0\n", distName];
   }
 
   p = (aProbability <= 0.5) ? aProbability : 1.0 - aProbability;

   //
   // The binomial distribution is invariant under changing aProbability to 1.0 - aProbability
   // if we also change the answer to n minus itself, we'll remember to do this below..
   //
  
   am = aNumTrials * p;
 
   if(aNumTrials < 25)
   {

        //
        // This is the mean of the deviate to be produced.
        // Use the direct method while n is not too large.
        // This can require up to 25 calls to getMyDoubleSample.
        //

        bnl = 0.0;
        for(j = 1; j <= aNumTrials; j++)
        {
              if([self getMyDoubleSample] < p)
              {
                  bnl += 1.0;
              }
        }
   }
   else if(am < 1.0)
   {
       //
       // If fewer than one event is expected out of 25 or more trials,
       // then the distribution is quite accurately Poisson. Use direct
       // Poisson method.

       g = exp(-am);
       t = 1.0;
       for(j = 0; j <= aNumTrials; j++)
       {
           t *= [self getMyDoubleSample];
           if(t < g)
           {
                break;
           }
       }
       bnl = (j <= aNumTrials ? j : aNumTrials);
   }  
   else
   {  
       //
       // Use rejection method with a Lorentzian comparison
       // function
  
       //
       // compute some useful quantaties

       en = aNumTrials;
       oldg = [self getGammaLn: (en + 1.0)];

       pc = 1.0 - p;
       plog = log(p);
       pclog = log(pc);

       sq = sqrt(2.0 * am * pc);

       do
       { 
          do
          {
             angle = PI * [self getMyDoubleSample];
             y = tan(angle);
             em = sq * y + am;

          } while((em < 0.0) || (em >= (en + 1.0)));  // Reject

          em = floor(em);    //Trick for integer-valued distribution
          t =   1.2 * sq * (1.0 + y*y) 
              * exp(oldg - [self getGammaLn: (em + 1.0)]
              - [self getGammaLn: (en - em + 1.0)] + em*plog + (en - em)*pclog);          
    
       } while([self getMyDoubleSample] > t);   //Reject. This happens about
                                                //1.5 times per deviate, on average.
  
      bnl = em;

   } //else

   if(p != aProbability)
   {
      //
      // Remember to undo the symmetry transformation
      //
         
      bnl = aNumTrials - bnl;
   }

   return bnl;
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
 
   [InvalidCombination raiseEvent: "ERROR: BinomialDist >>>> getMyDoubleSample\n"];
   
} 





- (unsigned) getUnsignedSample
{
  
  if((numTrials < 0) || (probability < 0))
  {
      optionsInitialized = NO;
  }

  if (!optionsInitialized)
    [InvalidCombination
      raiseEvent:
        "%s: getUnsignedSample: parameters have not been set\n", distName];
  
  currentCount++;


  return [self getUnsignedSampleWithNumTrials: numTrials
                                withProbability: probability];

}


- (unsigned) getUnsignedSampleWithProbability: (double) aProbability
{
  
  if (!optionsInitialized)
    [InvalidCombination
      raiseEvent:
        "%s: getUnsignedSample: parameters have not been set\n", distName];
  
  currentCount++;


  return [self getUnsignedSampleWithNumTrials: numTrials
                                withProbability: aProbability];

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
  // state variables:
  internalState->currentCount = currentCount;
  
  // nothing is returned from a (void) function
}

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
  (void)sprintf (buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];
  
  [outStream catC: "\n"];
  
  //  return self;
}

@end
