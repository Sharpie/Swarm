// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

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

Modified by:	Sven Thommesen
Date:		2000-02-19 (v. 0.81)
Changes:	Added methods to permit the use of StdDev instead of Variance

Modified by:	Sven Thommesen
Date:		2000-02-21 (v. 0.81)
Changes:	Added (id <GeneratorType>) to method definitions.

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


#import <Swarm/SwarmObject.h>
#import <Swarm/random.h>


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

+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>) generator
         setMean: (double)mean
     setVariance: (double)variance;

+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>) generator
         setMean: (double)mean
       setStdDev: (double)sdev;

+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>) generator
  setVirtualGenerator: (unsigned)vGen
              setMean: (double)mean
          setVariance: (double)variance;

+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>) generator
  setVirtualGenerator: (unsigned)vGen
              setMean: (double)mean
            setStdDev: (double)sdev;

// @protocol DoubleDistribution <ProbabilityDistribution>

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 

+ createWithDefaults: (id <Zone>)aZone;

+ create: (id <Zone>)aZone setGenerator: (id <SimpleRandomGenerator>) generator;

+ create             : (id <Zone>)aZone 
         setGenerator: (id <SplitRandomGenerator>) generator
  setVirtualGenerator: (unsigned) vGen;

+ createBegin: (id <Zone>)aZone;
- createEnd;

// @protocol InternalState

SETTING

// @protocol LogNormalDist <Normal, CREATABLE> 
- resetState;		// unpublished
- setMean: (double)mean setVariance: (double)variance;
- setMean: (double)mean setStdDev: (double)sdev;

// @protocol BooleanDistribution <ProbabilityDistribution> 

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: (id <SimpleRandomGenerator>) generator;
- setGenerator       : (id <SplitRandomGenerator>) generator 
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
- (double)getSampleWithMean: (double)mean 
                 withStdDev: (double)sdev;

// @protocol DoubleDistribution <ProbabilityDistribution>
- (double)getDoubleSample;

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- (id <BasicRandomGenerator>)getGenerator;
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

