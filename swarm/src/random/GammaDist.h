// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            GammaDist.h
Description:     Gamma distribution returning doubles
Library:         random
Original Author: Sven Thommesen
Date:            1997-01-15

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Standardized the informational comments.
		 Eliminated code to generate uniform doubles
		 in favor of calling the generator for this service.
		 Added code to deal with split generators.
		 Added method -createWithDefaults.
		 Removed distinction between frozen and non-frozen state.

Modified by:	Sven Thommesen
Date:		1998-10-08 (v. 0.8)
Changes:	Rearranged code for create-phase compatibility.

*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Distribution Documentation:
		| ---------------------------

Name:		| GammaDistribution

Description:	| Gamma distribution returning double values

Algorithm:	| Transformation, rejection and xxx methods.

Reference:	| Watkins 1994.

Parameters:	| double Alpha
		| double Beta

State:		| unsigned long long int currentCount

GammaDist	| On a 486/66, the following measures were obtained,
speed:		| using MT19937 as the underlying generator:

		|  -getDoubleSample:                          119.424 uS
		|  -average # of generator calls per variate:   6.224 (D)
		|  -distribution net time:                     56.194 uS

Relative speed:	| 0.00310 (time 32.294) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <objectbase/SwarmObject.h>
#import <random.h>


@interface GammaDist: SwarmObject <GammaDist>

{

// Distribution personality:

   unsigned 	stateSize;
   unsigned	distMagic;
   char		distName[DISTNAMESIZE];

// Data objects and fixed variables:

   id 		randomGenerator;
   unsigned	generatorMax;

   BOOL		useSplitGenerator;
   unsigned	virtualGenerator;
   BOOL 	optionsInitialized;

// Count of variates generated:

   unsigned long long int currentCount;

// --

// Parameters:

   double theAlpha;
   double theBeta;

// Working variables:

   // (none)

// State variables:

   // (none)

}

CREATING

// @protocol GammaDist <DoubleDistribution, CREATABLE>

- initState;		// unpublished

+ create     : aZone
 setGenerator: generator
     setAlpha: (double)alpha
      setBeta: (double)beta;

+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
             setAlpha: (double)alpha
              setBeta: (double)beta;

// @protocol DoubleDistribution <ProbabilityDistribution>

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 

+ createWithDefaults: aZone;

+ create: aZone setGenerator: generator;

+ create             : aZone 
         setGenerator: generator
  setVirtualGenerator: (unsigned) vGen;

+ createBegin: aZone;
- createEnd;

// @protocol InternalState

SETTING

// @protocol GammaDist <DoubleDistribution, CREATABLE>
- resetState;		// unpublished
- setAlpha: (double)alpha setBeta: (double)beta;

// @protocol BooleanDistribution <ProbabilityDistribution> 

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: generator;
- setGenerator       : generator 
  setVirtualGenerator: (unsigned)vGen;
- reset;

// @protocol InternalState

USING

// @protocol GammaDist <DoubleDistribution, CREATABLE>
- (double)getAlpha;
- (double)getBeta;
- (double)getSampleWithAlpha: (double)alpha 
                    withBeta: (double)beta;

// @protocol DoubleDistribution <ProbabilityDistribution>
- (double)getDoubleSample;

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- getGenerator;
- (unsigned)getVirtualGenerator;
- (BOOL)getOptionsInitialized;
- (unsigned long long int)getCurrentCount;

// @protocol InternalState
- (unsigned)getStateSize;		// size of buffer needed
- (void)putStateInto: (void *)buffer;	// save state data for later use
- (void)setStateFrom: (void *)buffer;	// set state from saved data
- (void)describe: outStream;	        // prints ascii data to stream
- (const char *)getName;		// returns name of object
- (unsigned)getMagic;			// object's 'magic number'

@end

