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
Name:            random.h
Description:     Protocol header file for random number generation  
Library:         random
Authors:         Nelson Minar, Roger Burkhart, Manor Askenazi
Date:            1996-12-11
Modified:        1997-01-15 (v. 0.6)  by Sven Thommesen
Modified:	 1997-09-01 (v. 0.7)  by Sven Thommesen
Modified:	 1997-12-08 (v. 0.75) by Sven Thommesen
Modified:	 1998-10-08 (v. 0.8)  by Sven Thommesen
*/

//S: Module for random number generation

//D: This module consists of a set of random number generation classes
//D: and a set of distribution classes for transforming random number
//D: sequences into various simulated probability distributions.

#import <Swarm/objectbase.h>		// for protocol SwarmObject


@protocol InternalState
//S: Archiving routines for internal generator and distribution state.

//D: Methods to save the internal state of an object (generator, distribution)
//D: to a memory buffer allocated by the calling program, and to set the state
//D: of an object from previously saved state data, provided in a memory
//D: buffer.

//D: NOTE: the putStateInto/setStateFrom methods are NOT portable across 
//D: architectures, since they store integers and doubles using different
//D: byte orders. A portable storage method may be provided in the next
//D: release.

USING
//M: Specifies the minimum buffer size needed.
- (unsigned)getStateSize;		// size of buffer needed
- (void)putStateInto: (void *)buffer;	// save state data for later use
- (void)setStateFrom: (void *)buffer;	// set state from saved data
- (unsigned)getMagic;			// object's 'magic number'
@end

#import <Swarm/generators.h>
#import <Swarm/distributions.h>
#import <Swarm/randomvars.h>
#import <Swarm/randomdefs.h>

