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
Name:            RandomVars.h
Description:     Commonly used random-wide variables
Library:         random
Original Author: Glen E. Ropella
Date:            1997-09-02 (v. 0.7)
Modified by:	 Sven Thommesen
Date:		 1997-12-08 (v. 0.75)
Modified by:	 Sven Thommesen
Date:		 1998-10-08 (v. 0.8)
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#import <Swarm/externvar.h>

// Make utility objects globally accessible:

externvar id <MT19937gen> randomGenerator;
// externvar id <SimpleRandomGenerator> randomGenerator;

externvar id <UniformIntegerDist> uniformIntRand;
externvar id <UniformUnsignedDist> uniformUnsRand;
externvar id <UniformDoubleDist> uniformDblRand;

// Make variables in random.m globally accessible:
// (Code to initialize these variables is in random.m)

externvar BOOL _useFixedSeed;
externvar unsigned int _firstSeed;

  // extern unsigned int		 _randomSeed;
  // extern unsigned int		 _timeThen;
  // extern unsigned int		 _timeNow;

// Make functions in random.m globally available:

extern void initRandom (id arguments);
extern unsigned int tempusFugit (void);
extern unsigned int nextSeed (void);

