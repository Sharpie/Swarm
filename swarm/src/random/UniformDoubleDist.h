// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            UniformDoubleDist.h
Description:     Uniform distribution returning double values
Library:         random
Original Author: Nelson Minar

Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Allow minValue == maxValue (returns minValue).
		 Standardized the informational comments.
		 Eliminated code to generate uniform doubles
		 in favor of calling the generator for this service.
		 Added code to deal with split generators.
		 Added method createWithDefaults.
		 Removed distinction between frozen and non-frozen state.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Distribution Documentation:
		| ---------------------------

Name:		| UniformDouble 

Description:	| Distribution returning doubles over an interval [min,max)

Algorithm:	| Transformation of uniform double [0.0,1.0)

Reference:	| (none)

Parameters:	| double minValue
		| double maxValue

State:		| unsigned long long int currentCount

Implementation	| Setting minValue=maxValue is allowed.
comment:	| Then minValue is returned "with probability 1".

UniformDoubleDist	| On a 486/66, the following measures were obtained,
speed:			| using MT19937 as the underlying generator:

		|  -getDoubleSample:                          13.777 uS
		|  -average # of generator calls per variate:  1.000 (D)
		|  -distribution net time:                     3.618 uS

Relative speed:	| Speed 0.268 (time 3.726) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <swarmobject/SwarmObject.h>
#import <random.h>


@interface UniformDoubleDist: SwarmObject 

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

   double 	doubleMin;
   double 	doubleMax;

// Working variables:

   BOOL		bSingular;
   double 	doubleRange;

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

-setDoubleMin: (double) minValue setMax: (double) maxValue;

+create: (id) aZone setGenerator: (id) generator
	setDoubleMin: (double) minValue setMax: (double) maxValue;

+create: (id) aZone setGenerator: (id) generator
	setVirtualGenerator: (unsigned) vGen
	setDoubleMin: (double) minValue setMax: (double) maxValue;

-reset;		// reset currentCount and other state data

// ----- Return values of parameters: -----

-(id)		getGenerator;
-(unsigned)	getVirtualGenerator;
-(BOOL)		getOptionsInitialized;

-(double) 	getDoubleMin;
-(double) 	getDoubleMax;


// ----- Return state values: -----

// Return count of variates generated:
-(unsigned  long long int) getCurrentCount;

 
// ----- Distribution output: -----

// FIXED parameters:

-(double)      getDoubleSample;

// FREE parameters:

-(double) getDoubleWithMin: (double) minValue withMax: (double) maxValue;



// ----- Object state management: -----

-(unsigned)  getStateSize;
-(void)      putStateInto:  (void *) buffer;
-(void)      setStateFrom:  (void *) buffer;
-(void)      describe:      (id) outStream;
-(char *)    getName;
-(unsigned)  getMagic;

@end
