// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            GammaDist.m
Description:     Gamma distribution returning doubles
Library:         random
Original Author: Sven Thommesen
Date:            1997-01-15 (v. 0.6)

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)

*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <math.h>

#import <collections.h>
#import <random/GammaDist.h>


@implementation GammaDist


// Import common code snippets:

#import "Common.dists.m"


// And now code particular to this distribution:


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
   double theAlpha;
   double theBeta;
   // State variables:
   unsigned long long int currentCount;
} state_struct_t;


-initState {

// Distribution personality:

   stateSize = sizeof(state_struct_t);
   strncpy(distName,"GammaDist",sizeof(distName));
   distMagic = GAMMADISTMAGIC + GAMMADISTREVISION;

// Parameters:

   optionsInitialized = NO;
   useSplitGenerator  = NO;
   virtualGenerator   = MAXVGEN;

   theAlpha      = 0.0;
   theBeta       = 0.0;

#ifdef USETHINDOUBLES
   printf("NOTE! %s: created to use THIN doubles\n",distName);
#endif

   return self;
}

-resetState {

   currentCount = 0;

   return self;
}


+createBegin: aZone {
   GammaDist * aDistribution;

// Allocate space for the object:

   aDistribution = [super createBegin: aZone];

// Initialize instance variables:

   aDistribution->randomGenerator = NULL;

// Initialize parameters:

   [aDistribution initState];

   return aDistribution;
}


+create: (id) aZone setGenerator: (id) generator {
   GammaDist * aDistribution;

// Allocate space for the object:

   aDistribution = [GammaDist createBegin: aZone];

// Connect the supplied random generator:

   [aDistribution setGenerator: generator];

   return [ aDistribution createEnd ];

}


+createWithDefaults: (id) aZone {
   GammaDist * aDistribution;

// Allocate space for the object:

   aDistribution = [GammaDist createBegin: aZone];

// Connect a default random generator:

   [aDistribution setGenerator: [PSWBgen createWithDefaults: aZone] ];

   return [ aDistribution createEnd ];

}


+create: (id) aZone setGenerator: (id) generator 
	setVirtualGenerator: (unsigned) vGen {
   GammaDist * aDistribution;

// Allocate space for the object:

   aDistribution = [GammaDist createBegin: aZone];

// Connect the supplied random generator:

   [aDistribution setGenerator: generator
	setVirtualGenerator: vGen];

   return [ aDistribution createEnd ];

}


// ----- protocol Gamma -----

-(double) getAlpha {
   return theAlpha;
}

-(double) getBeta {
   return theBeta;
}

-setAlpha: (double) alpha setBeta: (double) beta {

/*
// Relax this restriction, too.

   if (optionsInitialized)
   [InvalidCombination raiseEvent:
   "%s: setting parameters more than once not allowed\n", distName];
*/

   if (alpha <= 0.0)
   [InvalidCombination raiseEvent:
   "%s: setting alpha <= 0.0 not supported\n", distName];

   if (beta <= 0.0)
   [InvalidCombination raiseEvent:
   "%s: setting beta <= 0.0 not supported\n", distName];

   theAlpha = alpha;
   theBeta  = beta;

   // This object is now fixed:

   optionsInitialized = YES;

   [self resetState];

   return self;
}

+create: (id) aZone setGenerator: (id) generator
	setAlpha: (double) alpha setBeta: (double) beta {
   GammaDist * aDistribution;

   aDistribution = [ GammaDist create: aZone setGenerator: generator ];

   [aDistribution setAlpha: alpha setBeta: beta];

   return aDistribution;
}

+create: (id) aZone setGenerator: (id) generator
	setVirtualGenerator: (unsigned) vGen
	setAlpha: (double) alpha setBeta: (double) beta {
   GammaDist * aDistribution;

   aDistribution = [ GammaDist create: aZone 
		setGenerator: generator 
		setVirtualGenerator: vGen ];

   [aDistribution setAlpha: alpha setBeta: beta];

   return aDistribution;
}


// ----- Generate random numbers: -----


// Auxiliary method that returns an Exponential:

-(double) getExponentialWithMean: (double) theMean 
{
#ifdef USETHINDOUBLES
  if (useSplitGenerator) {
return theMean*(-log([randomGenerator getThinDoubleSample: virtualGenerator]));
  } else {
return theMean * (-log([randomGenerator getThinDoubleSample]));
  }
#else
  if (useSplitGenerator) {
return theMean * (-log([randomGenerator getDoubleSample: virtualGenerator]));
  } else {
return theMean * (-log([randomGenerator getDoubleSample]));
  }
#endif
}


-(double) getSampleWithAlpha: (double) alpha withBeta: (double) beta {
   double  x, avg, am, e, s, v1, v2, y;
   double z;

/*
// Allow this call even if parameters are set!

   if (optionsInitialized)
   [InvalidCombination raiseEvent:
   "%s: getSampleWithAlpha:withBeta: options already initialized\n", distName];
*/

   if (alpha <= 0.0)
   [InvalidCombination raiseEvent:
   "%s: setting alpha <= 0.0 not supported\n", distName];

   if (beta <= 0.0)
   [InvalidCombination raiseEvent:
   "%s: setting beta <= 0.0 not supported\n", distName];

   currentCount++ ;

  // Now transform Uniform to Gamma:
  // code from  Watkins (1994)

  // For alpha == 1, just use exponential:

  if (alpha == 1.0) 
  {
    return ([self getExponentialWithMean: 1.0] / beta);
  }

  // For alpha < 1, rejection method does not work,
  // so use alternative:
 
  if (alpha < 1.0) 
  {
    do 
    {
#ifdef USETHINDOUBLES
 if (useSplitGenerator) {
  x = pow([randomGenerator getThinDoubleSample: virtualGenerator], 1.0/alpha);
  y = pow([randomGenerator getThinDoubleSample: virtualGenerator], 1.0/(1.0-alpha));
 } else {
  x = pow([randomGenerator getThinDoubleSample], 1.0/alpha);
  y = pow([randomGenerator getThinDoubleSample], 1.0/(1.0-alpha));
 }
#else
 if (useSplitGenerator) {
  x = pow([randomGenerator getDoubleSample: virtualGenerator], 1.0/alpha);
  y = pow([randomGenerator getDoubleSample: virtualGenerator], 1.0/(1.0-alpha));
 } else {
  x = pow([randomGenerator getDoubleSample], 1.0/alpha);
  y = pow([randomGenerator getDoubleSample], 1.0/(1.0-alpha));
 }
#endif
    } while (x+y > 1.0);

    x = x/(x+y);
    y = [self getExponentialWithMean: 1.0];

    return (x*y/beta);
  }

  // For alpha > 1.0, use rejection method:
   
  do 
  {
    do 
    {
      do 
      {
#ifdef USETHINDOUBLES
  if (useSplitGenerator) {
    v1 = 2.0 * [randomGenerator getThinDoubleSample: virtualGenerator] - 1.0;
    v2 = 2.0 * [randomGenerator getThinDoubleSample: virtualGenerator] - 1.0;
  } else {
    v1 = 2.0 * [randomGenerator getThinDoubleSample] - 1.0;
    v2 = 2.0 * [randomGenerator getThinDoubleSample] - 1.0;
  }
#else
  if (useSplitGenerator) {
    v1 = 2.0 * [randomGenerator getDoubleSample: virtualGenerator] - 1.0;
    v2 = 2.0 * [randomGenerator getDoubleSample: virtualGenerator] - 1.0;
  } else {
    v1 = 2.0 * [randomGenerator getDoubleSample] - 1.0;
    v2 = 2.0 * [randomGenerator getDoubleSample] - 1.0;
  }
#endif
      } while (v1 * v1 + v2 * v2 > 1.0);
      y = v2 / v1;
      am = alpha - 1.0;
      s = sqrt(2.0 * am + 1.0);
      avg = s * y + am;
    } while (avg <= 0.0);
    e = (1.0 + y * y) * exp(am * log(avg / am) - s * y);
#ifdef USETHINDOUBLES
  if (useSplitGenerator) {
    z = [randomGenerator getThinDoubleSample: virtualGenerator];
  } else {
    z = [randomGenerator getThinDoubleSample];
  }
#else
  if (useSplitGenerator) {
    z = [randomGenerator getDoubleSample: virtualGenerator];
  } else {
    z = [randomGenerator getDoubleSample];
  }
#endif
  } while (z > e);

  return (avg / beta) ;

}



-(double) getDoubleSample {
   double  x, avg, am, e, s, v1, v2, y;
   double z;

   if (!optionsInitialized)
   [InvalidCombination raiseEvent:
   "%s: getDoubleSample: parameters have not been set\n", distName];

   currentCount++ ;

  // Now transform Uniform to Gamma:
  // code from Watkins (1994)

  // For alpha == 1, just use exponential:

  if (theAlpha == 1.0) 
  {
    return ([self getExponentialWithMean: 1.0] / theBeta);
  }

  // For alpha < 1, rejection method does not work,
  // so use alternative:
 
  if (theAlpha < 1.0) 
  {
    do 
    {
#ifdef USETHINDOUBLES
if (useSplitGenerator) {
  x = pow([randomGenerator getThinDoubleSample: virtualGenerator], 1.0 / theAlpha);
  y = pow([randomGenerator getThinDoubleSample: virtualGenerator], 1.0 / (1.0-theAlpha));
} else {
  x = pow([randomGenerator getThinDoubleSample], 1.0 / theAlpha);
  y = pow([randomGenerator getThinDoubleSample], 1.0 / (1.0-theAlpha));
}
#else
if (useSplitGenerator) {
    x = pow([randomGenerator getDoubleSample: virtualGenerator], 1.0 / theAlpha);
    y = pow([randomGenerator getDoubleSample: virtualGenerator], 1.0 / (1.0-theAlpha));
} else {
    x = pow([randomGenerator getDoubleSample], 1.0 / theAlpha);
    y = pow([randomGenerator getDoubleSample], 1.0 / (1.0-theAlpha));
}
#endif
    } while (x+y > 1.0);

    x = x / (x+y);
    y = [self getExponentialWithMean: 1.0];

    return (x*y / theBeta);
  }

  // For alpha > 1.0, use rejection method:
   
  do 
  {
    do 
    {
      do 
      {
#ifdef USETHINDOUBLES
  if (useSplitGenerator) {
    v1 = 2.0 * [randomGenerator getThinDoubleSample: virtualGenerator] - 1.0;
    v2 = 2.0 * [randomGenerator getThinDoubleSample: virtualGenerator] - 1.0;
  } else {
    v1 = 2.0 * [randomGenerator getThinDoubleSample] - 1.0;
    v2 = 2.0 * [randomGenerator getThinDoubleSample] - 1.0;
  }
#else
  if (useSplitGenerator) {
    v1 = 2.0 * [randomGenerator getDoubleSample: virtualGenerator] - 1.0;
    v2 = 2.0 * [randomGenerator getDoubleSample: virtualGenerator] - 1.0;
  } else {
    v1 = 2.0 * [randomGenerator getDoubleSample] - 1.0;
    v2 = 2.0 * [randomGenerator getDoubleSample] - 1.0;
  }
#endif
      } while (v1 * v1 + v2 * v2 > 1.0);
      y = v2 / v1;
      am = theAlpha - 1.0;
      s = sqrt(2.0 * am + 1.0);
      avg = s * y + am;
    } while (avg <= 0.0);
    e = (1.0 + y * y) * exp(am * log(avg / am) - s * y);
#ifdef USETHINDOUBLES
  if (useSplitGenerator) {
    z = [randomGenerator getThinDoubleSample: virtualGenerator];
  } else {
    z = [randomGenerator getThinDoubleSample];
  }
#else
  if (useSplitGenerator) {
    z = [randomGenerator getDoubleSample: virtualGenerator];
  } else {
    z = [randomGenerator getDoubleSample];
  }
#endif
  } while (z > e);

  return (avg / theBeta) ;

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
  internalState->theAlpha = theAlpha;
  internalState->theBeta  = theBeta;
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
  theAlpha           = internalState->theAlpha;
  theBeta            = internalState->theBeta;

  // State variables:
  currentCount       = internalState->currentCount;

  // Test generator data:

  if (
          ( (unsigned) [randomGenerator getMagic] != internalState->genMagic )
       || ( useSplitGenerator != internalState->useSplitGenerator )
       || ( virtualGenerator  != internalState->virtualGenerator  )
     )
  printf("%s setState: Warning! Not using the same generator!\n", distName);

  // nothing is returned from a (void) function
}

// ----- temporary methods -----

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
  (void)sprintf(buffer,"           theAlpha = %24.16e\n", theAlpha);
  [outStream catC: buffer];
  (void)sprintf(buffer,"            theBeta = %24.16e\n", theBeta);
  [outStream catC: buffer];
  (void)sprintf(buffer,"       currentCount = %24llu\n", currentCount);
  [outStream catC: buffer];

  [outStream catC: "\n"];

  //  return self;
}

@end
