// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            BernoulliDist.h
Description:     Distribution returning YES with a given probability
Library:         random
Original Author: Sven Thommesen
Date:		 1997-09-01 (v. 0.7)

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

Name:		| BernoulliDistribution

Description:	| Distribution returning binary value YES with probability p

Algorithm:	| Rejection method

Reference:	| Code contributed by Barry McMullin

Parameters:	| double Probability

State:		| unsigned long long int currentCount

BernoulliDist	| On a 486/66, the following measures were obtained,
speed:		| using MT19937 as the underlying generator:

		|  -getBooleanSample:                         8.156 uS
		|  -average # of generator calls per variate: 1.000 (tD)
		|  -distribution net time:                    2.654 uS

Relative speed:	| Speed 0.453 (time 2.206) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <objectbase/SwarmObject.h>
#import <random.h>

// NOTE: this distribution draws doubles from its generator.
// There is no point in using 'fat' doubles for this purpose here.
// So, regardless of what's done in distributions.h, we:
#define USETHINDOUBLES 1

@interface BernoulliDist: SwarmObject <BernoulliDist>

{

// Distribution personality:

   unsigned 	stateSize;	
   unsigned	distMagic;
   char		distName[DISTNAMESIZE];

// Data objects and fixed variables:

   id		randomGenerator;
   unsigned	generatorMax;

   BOOL		useSplitGenerator;
   unsigned	virtualGenerator;
   BOOL 	optionsInitialized;

// Count of variates generated:

   unsigned long long int currentCount;

// --

// Parameters:

   double 	theP;

// Working variables:

   // (none)

// State variables:

  // (none)

}

CREATING

// @protocol BernoulliDist <BooleanDistribution, CREATABLE>

- initState;	// unpublished

+ create        : aZone
    setGenerator: generator
  setProbability: (double)p;

+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
       setProbability: (double)p;

// @protocol BooleanDistribution <ProbabilityDistribution> 

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

// @protocol BernoulliDist <BooleanDistribution, CREATABLE>
- resetState;	// unpublished
- setProbability: (double)p;

// @protocol BooleanDistribution <ProbabilityDistribution> 

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: generator;
- setGenerator       : generator 
  setVirtualGenerator: (unsigned)vGen;
- reset;

// @protocol InternalState

USING

// @protocol BernoulliDist <BooleanDistribution, CREATABLE>
- (double)getProbability;
- (BOOL)getSampleWithProbability: (double)p;

// @protocol BooleanDistribution <ProbabilityDistribution> 
- (BOOL)getBooleanSample;
- (int)getIntegerSample;	// for convenience

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
