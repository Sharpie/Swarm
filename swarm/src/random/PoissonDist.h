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
Name:            PoissonDist.h
Description:     Poisson distribution returning unsigned integers
Library:         random
Original Author: Steve Jackson
Date:            2001-01-08

Modified by:	 Sven Thommesen
Date:		 2001-07-16
Changes:	 Minor changes for protocol compliance
		
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Distribution Documentation:
		| ---------------------------

Name:		| PoissonDistribution

Description:	| Poisson distribution returning unsigned integer values

Algorithm:	| rejection method.

Reference:	| Press, Flannery, Teukolsky, and Vetterlin        
  		| "Numerical Recipes in C"
		| 1st Edition
		| ch 7 pp 221-223, ch 6 pp 167- 168
              

Parameters:	| occurRate, interval

State:		| unsigned long long int currentCount

PoissonDist	| 
speed:		| 

		|  
		|  
		|  

Relative speed:	| 
--------------- . ------------------------------------------------------------- 
*/


#import <Swarm/SwarmObject.h>
#import <Swarm/random.h>


@interface PoissonDist: SwarmObject <PoissonDist>
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

   double occurRate;
   double interval;

// Working variables:

   double cof[6];

// State variables:

   // (none)

}

CREATING

- initState;		// unpublished

// @protocol PoissonDist <UnsignedDistribution, CREATABLE>

+ create: (id <Zone>)aZone 
    setGenerator: (id <SimpleRandomGenerator>) generator
    setOccurRate: (double) anOccurRate
     setInterval: (double) anInterval;

+ create             : (id <Zone>)aZone 
         setGenerator: (id <SplitRandomGenerator>) generator
  setVirtualGenerator: (unsigned) vGen
    setOccurRate: (double) anOccurRate
     setInterval: (double) anInterval;

// @protocol UnsignedDistribution <ProbabilityDistribution>

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

- resetState;		// unpublished

// @protocol PoissonDist <UnsignedDistribution, CREATABLE>
- setInterval:  (double) anInterval;
- setOccurRate: (double) aOccurRate;
- setOccurRate: (double) aOccurRate
   setInterval: (double) anInterval;

// @protocol UnsignedDistribution <ProbabilityDistribution>
// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: (id <SimpleRandomGenerator>) generator;
- setGenerator       : (id <SplitRandomGenerator>) generator 
  setVirtualGenerator: (unsigned)vGen;
- reset;

// @protocol InternalState

USING

- (double) getGammaLn: (double) arg;	// unpublished
- (double) getMyDoubleSample;		// unpublished

// @protocol PoissonDist <UnsignedDistribution, CREATABLE>
- (double) getOccurRate;
- (double) getInterval;
- (unsigned) getUnsignedSampleWithInterval: (double) anInterval;
- (unsigned) getUnsignedSampleWithOccurRate: (double) anOccurRate
                         withInterval: (double) anInterval;

// @protocol UnsignedDistribution <ProbabilityDistribution>
- (unsigned) getUnsignedSample;

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

