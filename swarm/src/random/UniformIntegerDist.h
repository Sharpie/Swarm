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

Modified by:	Sven Thommesen
Date:		1998-10-08 (v. 0.8)
Changes:	Rearranged code for create-phase compatibility.

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


#import <Swarm/SwarmObject.h>
#import <Swarm/random.h>


@interface UniformIntegerDist: SwarmObject <UniformIntegerDist>

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


CREATING

// @protocol UniformIntegerDist <IntegerDistribution, CREATABLE>

- initState;		// unpublished

+ create      : (id <Zone>)aZone
  setGenerator: (id <SimpleRandomGenerator>) generator
 setIntegerMin: (int)minValue
        setMax: (int)maxValue;

+ create           : (id <Zone>)aZone
       setGenerator: (id <SplitRandomGenerator>) generator
setVirtualGenerator: (unsigned) vGen
      setIntegerMin: (int)minValue
             setMax: (int)maxValue;

// @protocol IntegerDistribution <ProbabilityDistribution> 

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

// @protocol UniformIntegerDist <IntegerDistribution, CREATABLE>
- resetState;		// unpublished
- setIntegerMin: (int)minValue setMax: (int)maxValue;

// @protocol BooleanDistribution <ProbabilityDistribution> 

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: (id <SimpleRandomGenerator>) generator;
- setGenerator       : (id <SplitRandomGenerator>) generator 
  setVirtualGenerator: (unsigned)vGen;
- reset;

// @protocol InternalState

USING

// @protocol UniformIntegerDist <IntegerDistribution, CREATABLE>
- (int)getIntegerMin;
- (int)getIntegerMax;
- (int)getIntegerWithMin: (int)minValue withMax: (int)maxValue;

// @protocol IntegerDistribution <ProbabilityDistribution> 
- (int)getIntegerSample;

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
