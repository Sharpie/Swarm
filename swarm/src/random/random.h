// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:            random.h
Description:     Protocol header file for random number generation  
Library:         random
Authors:         Nelson Minar, Roger Burkhart, Manor Askenazi
Date:            1996-12-11
Modified:        1997-01-15 (v. 0.6)  by Sven Thommesen
Modified:	 1997-09-01 (v. 0.7)  by Sven Thommesen
Modified:	 1997-12-08 (v. 0.75) by Sven Thommesen
*/

#import <defobj.h>

// InternalState --
//   Methods to save the internal state of an object (generator, distribution)
//   to a memory buffer allocated by the calling program, and to set the state
//   of an object from previously saved state data, provided in a memory buffer.
//   Method -getStateSize specifies the minimum buffer size needed.
//
// NOTE: the putStateInto/setStateFrom methods are NOT portable across 
//   architectures, since they store integers and doubles using different
//   byte orders. A portable storage method may be provided in the next
//   release.
// 
@deftype InternalState
- (unsigned)	getStateSize;			// size of buffer needed
- (void)	putStateInto: (void *) buffer;	// save state data for later use
- (void)	setStateFrom: (void *) buffer;	// set state from saved data
- (void)	describe: (id) outStream;	// prints ascii data to stream
- (const char *)getName;			// returns name of object
- (unsigned)	getMagic;			// object's 'magic number'
@end

// -----------------------------

#import <random/generators.h>

#import <random/distributions.h>

#import <random/RandomVars.h>

#import <random/RandomDefs.h>

// -----------------------------

