// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

/*
Name:            BinomialDist.h
Description:     Binomial distribution returning the integer number of occurrences, given a probability per trial and number of trials.
Library:         random
Original Author: Steve Jackson
Date:            2004-04-13

Modified by:	 
Date:		 
Changes:	 
		
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

/*
--------------- | Distribution Documentation:
		| ---------------------------

Name:		| BinomialDist

Description:	| Binomial distribution returning the integer number of occurrences, 
                | given a probability per trial and number of trials.

Algorithm:	| rejection method.

Reference:	| Press, Flannery, Teukolsky, and Vetterlin        
  		| "Numerical Recipes in C"
		| ch 7 pp 223-224
              

Parameters:	| probability, numTrials

State:		| unsigned long long int currentCount

BinomialDist	| 
speed:		| 

		|  
		|  
		|  

Relative speed:	| 
--------------- . ------------------------------------------------------------- 
*/


#import <Swarm/SwarmObject.h>
#import <Swarm/random.h>

#define BINOMIALDISTMAGIC       2420400U  // put in randomdefs.h
#define BINOMIALDISTREVISION          1U  //

#ifndef PI
  #define PI 3.141592654
#endif


@interface BinomialDist: SwarmObject <BinomialDist>

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
   BOOL 	workingVarsInitialized;

// Count of variates generated:

   unsigned long long int currentCount;

// --

// Parameters:

   double probability;
   unsigned numTrials;

// Working variables:

   double cof[6];

// State variables:

   // (none)

}

CREATING

// @protocol BinomialDist <IntegerDistribution, CREATABLE>

- initState;		// unpublished

+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>) generator
  setVirtualGenerator: (unsigned)vGen;

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 

+ createWithDefaults: (id <Zone>)aZone;

+ create: (id <Zone>)aZone setGenerator: (id <SimpleRandomGenerator>) generator;

+ create             : (id <Zone>)aZone 
         setGenerator: (id <SplitRandomGenerator>) generator
  setVirtualGenerator: (unsigned) vGen;

+ create        : (id <Zone>)aZone 
    setGenerator: (id <SimpleRandomGenerator>) generator
    setNumTrials: (unsigned) aNumTrials
  setProbability: (double) aProbability;

+ create             : (id <Zone>)aZone 
         setGenerator: (id <SplitRandomGenerator>) generator
  setVirtualGenerator: (unsigned) vGen
         setNumTrials: (unsigned) aNumTrials
       setProbability: (double) aProbability;

+ createBegin: (id <Zone>)aZone;
- createEnd;

// @protocol InternalState

SETTING

// @protocol BinomialDist <IntegerDistribution, CREATABLE>
- setNumTrials: (unsigned) aNumTrials;

-   setNumTrials: (unsigned) aNumTrials
  setProbability: (double) aProbability;

- resetState;		// unpublished

// @protocol ProbabilityDistribution <SwarmObject, InternalState> 
- setGenerator: (id <SimpleRandomGenerator>) generator;
- setGenerator       : (id <SplitRandomGenerator>) generator 
  setVirtualGenerator: (unsigned)vGen;
- reset;

// @protocol InternalState

USING


// Not published
// for internal use
- (double) getGammaLn: (double) arg;
- (double) getMyDoubleSample;

// @protocol UnsignedDistribution <ProbabilityDistribution>
// @protocol BinomialDist <IntegerDistribution, CREATABLE>

- (unsigned) getUnsignedSample;
- (unsigned) getNumTrials;
- (double) getProbability;
- (unsigned) getUnsignedSampleWithProbability: (double) anInterval;
- (unsigned) getUnsignedSampleWithNumTrials: (unsigned) aNumTrials
                            withProbability: (double) aProbability;

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

