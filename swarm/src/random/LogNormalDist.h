// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            LogNormalDist.h
Description:     Log-Normal distribution returning double values
Library:         random
Original Author: Sven Thommesen
Date:            1997-01-15

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Fixed bug in method -getDoubleSample (missing exp()).
		 Standardized the informational comments.
		 Eliminated code to create uniform doubles
		 in favor of calling the generator for this service.
		 Added code to deal with split generators.
		 Added method -createWithDefaults.
		 Change: allow variance=0.0 (returns exp(mean)).
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

Name:		| LogNormalDistribution

Description:	| Log-Normal distribution returning double values

Reference:	| W.H. Press et al, "Numerical Recipes in C" (2nd. ed.).
		| Cambridge University Press, 1992.

Algorithm:	| The Box-Muller method 

Parameters:	| double Mean
		| double Variance

State:		| double stored_value
		| BOOL stored
		| unsigned long long int currentCount

Output types:	| double 

Output Quality:	| All bits should be safe.

LogNormalDist	| On a 486/66, the following measures were obtained,
speed:		| using MT19937 as the underlying generator:

		|  -getDoubleSample:                          33.430 uS
		|  -average # of generator calls per variate:  1.273 (D)
		|  -distribution net time:                    20.498 uS

Relative speed:	| Speed 0.111 (time 9.040) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <objectbase/SwarmObject.h>
#import <random.h>


@interface LogNormalDist: SwarmObject <LogNormalDist>

{

// Distribution personality:

   unsigned 	stateSize;
   unsigned 	distMagic;
   char 	distName[DISTNAMESIZE];

// Data objects and fixed variables:

   id 		randomGenerator;
   unsigned	generatorMax;

   BOOL 	useSplitGenerator;
   unsigned	virtualGenerator;
   BOOL 	optionsInitialized;

// Count of variates generated:

   unsigned long long int currentCount;

// --

// Parameters:

   double 	theMean;
   double 	theVariance;

// Working variables:

   double 	theStdDev;

// State variables:

   BOOL 	stored;
   double 	stored_double;

}


CREATING

// @protocol LogNormalDist <Normal, CREATABLE> 

- initState;		// unpublished

+ create        : aZone
    setGenerator: generator
         setMean: (double)mean
     setVariance: (double)variance;

+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
              setMean: (double)mean
          setVariance: (double)variance;

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

// @protocol LogNormalDist <Normal, CREATABLE> 
- resetState;		// unpublished
- setMean: (double)mean setVariance: (double)variance;

// @protocol BooleanDistribution <ProbabilityDistribution> 

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: generator;
- setGenerator       : generator 
  setVirtualGenerator: (unsigned)vGen;
- reset;

// @protocol InternalState

USING

// @protocol LogNormalDist <Normal, CREATABLE> 
- (double)getMean;
- (double)getVariance;
- (double)getStdDev;
- (double)getSampleWithMean: (double)mean 
               withVariance: (double)variance;

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

