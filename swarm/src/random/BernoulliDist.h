// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            BernoulliDist.h
Description:     Distribution returning YES with a given probability
Library:         random
Original Author: Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
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


#import <swarmobject/SwarmObject.h>
#import <random.h>

// NOTE: this distribution draws doubles from its generator.
// There is no point in using 'fat' doubles for this purpose here.
// So, regardless of what's done in distributions.h, we:
#define USETHINDOUBLES 1

@interface BernoulliDist: SwarmObject 

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

// --- Private methods: -----

-initState;
-resetState;

// ----- Generator creation: -----

+createBegin: (id) aZone;
-setGenerator: (id) generator;
-setGenerator: (id) generator setVirtualGenerator: (unsigned) vGen;
-createEnd;


+createWithDefaults: aZone;

+create: (id) aZone setGenerator: (id) generator;

+create: (id) aZone setGenerator: (id) generator 
	setVirtualGenerator: (unsigned) vGen;

-setProbability: (double) p;

+create: (id) aZone setGenerator: (id) generator
	setProbability: (double) p;

+create: (id) aZone setGenerator: (id) generator
	setVirtualGenerator: (unsigned) vGen
	setProbability: (double) p;

-reset;		// reset currentCount and other state data;

// ----- Return values of parameters: -----

-(id)		getGenerator;
-(unsigned)	getVirtualGenerator;
-(BOOL)		getOptionsInitialized;

-(double) 	getProbability;


// ----- Return state values: -----

// Return count of variates generated:

-(unsigned  long long int) getCurrentCount;

 
// ----- Distribution output: -----

// FIXED parameters:

-(BOOL) 	getBooleanSample;
-(int)  	getIntegerSample;

// FREE parameters:

-(BOOL) getSampleWithProbability: (double) p;


// ----- Object state management: -----

-(unsigned)  getStateSize;
-(void)      putStateInto:  (void *) buffer;
-(void)      setStateFrom:  (void *) buffer;
-(void)      describe:      (id) outStream;
-(const char *)getName;
-(unsigned)  getMagic;

@end
