// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            NormalDist.h
Description:     Normal (Gaussian) distribution returning double values
Library:         random
Original Author: Manor Askenazi
Date:		 1996-09-09

Modified by:     Sven Thommesen
Date:            1997-01-15 (v. 0.6)
Changes:	 Cast the "old style" objective-C objects to Swarm objects,
		 using create phase protocols.
		 Added code to fill doubles from 2 random numbers.

Modified by:	 Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Changes:	 Standardized the informational comments.
		 Eliminated code to create uniform doubles
		 in favor of calling the generator for this service.
		 Added code to deal with split generators.
		 Added method -createWithDefaults.
		 Change: allow variance=0.0 (returns mean).
		 Removed distinction between frozen and non-frozen state.
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Distribution Documentation:
		| ---------------------------

Name:		| NormalDistribution

Description:	| Normal (Gaussian) distribution returning double values

Algorithm:	| The Box-Muller method

Reference:	| W.H. Press et al, "Numerical Recipes in C" (2nd ed.).
		| Cambridge University Press, 1992.

Parameters:	| double Mean
		| double Variance

State:		| double stored_value
		| BOOL stored
		| unsigned long long int currentCount

NormalDist	| On a 486/66, the following measures were obtained,
speed:		| using MT19937 as the underlying generator:

		|  -getDoubleSample:                          24.729 uS
		|  -average # of generator calls per variate:  1.273 (D)
		|  -distribution net time:                    11.797 uS

Relative speed:	| Speed 0.150 (time 6.687) relative to MT19937 getUnsignedSample
--------------- . ------------------------------------------------------------- 
*/


#import <objectbase/SwarmObject.h>
#import <random.h>


@interface NormalDist: SwarmObject 

{

// Distribution personality:

   unsigned 	stateSize;	
   unsigned 	distMagic;
   char     	distName [DISTNAMESIZE];

// Data objects and fixed variables:

   id       	randomGenerator;
   unsigned	generatorMax;

   BOOL     	useSplitGenerator;
   unsigned 	virtualGenerator;
   BOOL     	optionsInitialized;

// Count of variates generated:

   unsigned long long int currentCount;

// --

// Parameters:

   double theMean;
   double theVariance;

// Working variables:

   double theStdDev;

// State variables:

   BOOL stored;
   double stored_double;

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

-setMean: (double) mean setVariance: (double) variance;

+create: aZone setGenerator: generator
	setMean: (double) mean setVariance: (double) variance;

+create: aZone setGenerator: generator
	setVirtualGenerator: (unsigned) vGen
	setMean: (double) mean setVariance: (double) variance;

-reset;		// reset currentCount and other state data

// ----- Return values of parameters: -----

-(id)		getGenerator;
-(unsigned)	getVirtualGenerator;
-(BOOL)		getOptionsInitialized;

-(double) 	getMean;
-(double) 	getVariance;
-(double) 	getStdDev;


// ----- Return state values: -----

// Return count of variates generated:

-(unsigned  long long int) getCurrentCount;
 
// ----- Distribution output: -----

// FIXED parameters:

-(double)      getDoubleSample;

// FREE parameters:

-(double) getSampleWithMean: (double) mean withVariance: (double) variance;


// ----- Object state management: -----

-(unsigned)  getStateSize;
-(void)      putStateInto:  (void *) buffer;
-(void)      setStateFrom:  (void *) buffer;
-(void)      describe:      (id) outStream;
-(const char *)getName;
-(unsigned)  getMagic;

@end
