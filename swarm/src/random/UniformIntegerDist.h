// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            UniformIntegerDist.h
Description:     Uniform distribution returning integer values
Library:         random
Original Author: Nelson Minar

Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Allow minValue == maxValue (returns minValue).
		 Standardized the informational comments.
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

Name:		| UniformInteger

Description:	| Distribution returning integers over an interval [min,max]

Algorithm:	| Transformation of uniform unsigneds [0,unsignedMax]

Reference:	| (none)

Parameters:	| integer minValue
		| integer maxValue

State:		| unsigned long long int currentCount

Implementation  | Setting minValue=maxValue is allowed.
comment:        | Then minValue is retured "with probability 1".

UniformIntegerDist	| On a 486/66, the following measures were obtained,
speed:			| using MT19937 as the underlying generator:

		|  -getIntegerSample:                         7.534 uS
		|  -average # of generator calls per variate: 1.000 (U)
		|  -distribution net time:                    3.836 uS

Relative speed:	| Speed 0.491 (time 2.037) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <swarmobject/SwarmObject.h>
#import <random.h>


@interface UniformIntegerDist: SwarmObject 

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

   int 		iMin;
   int 		iMax;

// Working variables:

   BOOL 	bSingular;
   unsigned 	uRange;
   unsigned 	uCutoff;

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

-setIntegerMin: (int) minValue setMax: (int) maxValue;

+create: (id) aZone setGenerator: (id) generator
	setIntegerMin: (int) minValue setMax: (int) maxValue;

+create: (id) aZone setGenerator: (id) generator
	setVirtualGenerator: (unsigned) vGen
	setIntegerMin: (int) minValue setMax: (int) maxValue;

-reset;		// reset currentCount and other state data

// ----- Return values of parameters: -----

-(id)		getGenerator;
-(unsigned)	getVirtualGenerator;
-(BOOL)		getOptionsInitialized;

-(int) 		getIntegerMin;
-(int) 		getIntegerMax;


// ----- Return state values: -----

// Return count of variates generated:

-(unsigned  long long int) getCurrentCount;

 
// ----- Distribution output: -----

// FIXED parameters:

-(int) getIntegerSample;

// FREE parameters:

-(int) getIntegerWithMin: (int) minValue withMax: (int) maxValue;


// ----- Object state management: -----

-(unsigned)  getStateSize;
-(void)      putStateInto:  (void *) buffer;
-(void)      setStateFrom:  (void *) buffer;
-(void)      describe:      (id) outStream;
-(const char *)getName;
-(unsigned)  getMagic;

@end
