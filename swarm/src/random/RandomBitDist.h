// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            RandomBitDist.h
Description:     Distribution object returning fair (?) coin tosses
Library:         random
Original Author: Nelson Minar

Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Standardized the informational comments.
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

Name:		| RandomBitDistribution

Description:	| Distribution returning YES and NO with equal probability

Algorithm:	| Extract a middle bit from an unsigned random number

Reference:	| (none)

Parameters:	| (none)

State:		| unsigned long long int currentCount

RandomBitDist	| On a 486/66, the following measures were obtained,
speed:		| using MT19937 as the underlying generator:

		|  -getBooleanSample:                         5.656 uS
		|  -average # of generator calls per variate: 1.000 (U)
		|  -distribution net time:                    1.958 uS

Relative speed:	| Speed 0.654 (time 1.529) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <objectbase/SwarmObject.h>
#import <random.h>


@interface RandomBitDist: SwarmObject 

{

// Distribution personality:

   unsigned 	stateSize;
   unsigned 	distMagic;
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

   unsigned theMask;

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

+create: (id) aZone setGenerator: (id) generator;

+create: (id) aZone setGenerator: (id) generator 
	setVirtualGenerator: (unsigned) vGen;

+createWithDefaults: aZone;

-reset;		// reset currentCount and other state data

// ----- Return values of parameters: -----

-(id)		getGenerator;
-(unsigned)	getVirtualGenerator;
-(BOOL)		getOptionsInitialized;


// ----- Return state values: -----

// Return count of variates generated:

-(unsigned  long long int) getCurrentCount;

 
// ----- Distribution output: -----

// Methods that conform to the BooleanDistribution protocol:

-(BOOL)		getBooleanSample;
-(int) 		getIntegerSample;

// Methods that conform to the RandomBits protocol:

-(BOOL) 	getCoinToss;


// ----- Object state management: -----

-(unsigned)  getStateSize;
-(void)      putStateInto:  (void *) buffer;
-(void)      setStateFrom:  (void *) buffer;
-(void)      describe:      (id) outStream;
-(const char *)getName;
-(unsigned)  getMagic;

@end
