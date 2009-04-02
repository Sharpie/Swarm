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
Name:            BernoulliDist.h
Description:     Distribution returning YES with a given probability
Library:         random
Original Author: Sven Thommesen
Date:		 1997-09-01 (v. 0.7)

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


#import <Swarm/SwarmObject.h>
#import <Swarm/random.h>

// NOTE: this distribution draws doubles from its generator.
// There is no point in using 'fat' doubles for this purpose here.
// So, regardless of what's done in distributions.h, we:
#define USETHINDOUBLES 1

@interface BernoulliDist: SwarmObject <BernoulliDist>

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

CREATING

// @protocol BernoulliDist <BooleanDistribution, CREATABLE>

- initState;	// unpublished

+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>) generator
  setProbability: (double)p;

+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>) generator
  setVirtualGenerator: (unsigned)vGen
       setProbability: (double)p;

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

// @protocol BernoulliDist <BooleanDistribution, CREATABLE>
- resetState;	// unpublished
- setProbability: (double)p;

// @protocol BooleanDistribution <ProbabilityDistribution> 

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: (id <SimpleRandomGenerator>) generator;
- setGenerator       : (id <SplitRandomGenerator>) generator 
  setVirtualGenerator: (unsigned)vGen;
- reset;

// @protocol InternalState

USING

// @protocol BernoulliDist <BooleanDistribution, CREATABLE>
- (double)getProbability;
- (BOOL)getSampleWithProbability: (double)p;

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
