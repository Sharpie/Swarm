// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
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


#import <swarmobject/SwarmObject.h>
#import <random.h>


@interface GammaDist: SwarmObject 

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

-setAlpha: (double) alpha setBeta: (double) beta;

+create: (id) aZone setGenerator: (id) generator
	setAlpha: (double) alpha setBeta: (double) beta;

+create: (id) aZone setGenerator: (id) generator
	setVirtualGenerator: (unsigned) vGen
	setAlpha: (double) alpha setBeta: (double) beta;

-reset;		// reset currentCount and other state data

// ----- Return values of parameters: -----

-(id)		getGenerator;
-(unsigned)	getVirtualGenerator;
-(BOOL)		getOptionsInitialized;

-(double) 	getAlpha;
-(double) 	getBeta;


// ----- Return state values: -----

// Return count of variates generated:

-(unsigned  long long int) getCurrentCount;

 
// ----- Distribution output: -----

// FIXED parameters:

-(double)      getDoubleSample;

// FREE parameters:

-(double) getSampleWithAlpha: (double) alpha withBeta: (double) beta;


// ----- Object state management: -----

-(unsigned)  getStateSize;
-(void)      putStateInto:  (void *) buffer;
-(void)      setStateFrom:  (void *) buffer;
-(void)      describe:      (id) outStream;
-(char *)    getName;
-(unsigned)  getMagic;

@end

