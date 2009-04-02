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


#import <Swarm/SwarmObject.h>
#import <Swarm/random.h>


@interface RandomBitDist: SwarmObject <RandomBitDist>

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


CREATING

// @protocol RandomBitDist <BooleanDistribution, CREATABLE>

- initState;		// unpublished

// @protocol BooleanDistribution <ProbabilityDistribution> 

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

// @protocol RandomBitDist <BooleanDistribution, CREATABLE>
- resetState;		// unpublished

// @protocol BooleanDistribution <ProbabilityDistribution> 

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: (id <SimpleRandomGenerator>) generator;
- setGenerator       : (id <SplitRandomGenerator>) generator 
  setVirtualGenerator: (unsigned)vGen;
- reset;

// @protocol InternalState

USING

// @protocol RandomBitDist <BooleanDistribution, CREATABLE>
- (BOOL)getCoinToss;

// @protocol BooleanDistribution <ProbabilityDistribution> 
- (BOOL)getBooleanSample;
- (int)getIntegerSample;	// for convenience

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
