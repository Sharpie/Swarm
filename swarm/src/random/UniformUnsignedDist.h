// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            UniformUnsignedDist.h
Description:     Uniform distribution returning unsigned integer values
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

Name:		| UniformUnsigned

Description:	| Distribution returning unsigneds over an interval [min,max]

Algorithm:	| Transformation of uniform unsigned [0,unsignedMax]

Reference:	| (none)

Parameters:	| unsigned minValue
		| unsigned maxValue

State:		| unsigned long long int currentCount

Implementation  | Setting minValue=maxValue is allowed.
comment:        | Then minValue is returned "with probability 1".

UniformUnsignedDist	| On a 486/66, the following measures were obtained,
speed:			| using MT19937 as the underlying generator:

		|  -getUnsignedSample:                        7.632 uS
		|  -average # of generator calls per variate: 1.000 (U)
		|  -distribution net time:                    3.934 uS

Relative speed:	| Speed 0.485 (time 2.064) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <swarmobject/SwarmObject.h>
#import <random.h>


@interface UniformUnsignedDist: SwarmObject 

{

// Distribution personality:

   unsigned 	stateSize;
   unsigned	distMagic;
   char		distName[DISTNAMESIZE];

// Data objects and fixed variables:

   id		randomGenerator;
   unsigned 	generatorMax;

   BOOL		useSplitGenerator;
   unsigned	virtualGenerator;
   BOOL 	optionsInitialized;

// Count of variates generated:

   unsigned long long int currentCount;

// --

// Parameters:

   unsigned 	uMin;
   unsigned 	uMax;

// Working variables:

   BOOL		bSingular;
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

-setUnsignedMin: (unsigned) minValue setMax: (unsigned) maxValue;

+create: (id) aZone setGenerator: (id) generator
	setUnsignedMin: (unsigned) minValue setMax: (unsigned) maxValue;

+create: (id) aZone setGenerator: (id) generator
	setVirtualGenerator: (unsigned) vGen
	setUnsignedMin: (unsigned) minValue setMax: (unsigned) maxValue;

-reset;		// reset currentCount and other state data;

// ----- Return values of parameters: -----

-(id)		getGenerator;
-(unsigned)	getVirtualGenerator;
-(BOOL)		getOptionsInitialized;

-(unsigned) 	getUnsignedMin;
-(unsigned) 	getUnsignedMax;


// ----- Return state values: -----

// Return count of variates generated:

-(unsigned  long long int) getCurrentCount;

 
// ----- Distribution output: -----

// FIXED parameters:

-(unsigned) getUnsignedSample;

// FREE parameters:

-(unsigned) getUnsignedWithMin: (unsigned) minValue 
			withMax: (unsigned) maxValue;



// ----- Object state management: -----

-(unsigned)  getStateSize;
-(void)      putStateInto:  (void *) buffer;
-(void)      setStateFrom:  (void *) buffer;
-(void)      describe:      (id) outStream;
-(char *)    getName;
-(unsigned)  getMagic;

@end
