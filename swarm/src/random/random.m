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
Name:		 random.m
Description:	 startup functions for the random library
Library:	 random
Original Author: Glen E. Ropella
Date:		 1997-09-01 (v. 0.7)
Modified by:	 Sven Thommesen
Date:		 1997-12-08 (v. 0.75)
Modified by:	 Sven Thommesen
Date:		 1998-10-08 (v. 0.8)
*/

#import <defobj.h>
#import <random.h>
#import <objectbase.h>

#include <misc.h> // gettimeofday

#ifdef __MINGW32__
#define sleep usleep
#endif


// Utility random objects:

// id <MT19937gen>          randomGenerator;
externvardef id <SimpleRandomGenerator>  randomGenerator;

externvardef id <UniformIntegerDist>     uniformIntRand;
externvardef id <UniformUnsignedDist>    uniformUnsRand;
externvardef id <UniformDoubleDist>      uniformDblRand;

// Local variables:

externvardef BOOL _useFixedSeed;       // globally accessible
externvardef unsigned int _firstSeed;  // globally accessible

unsigned _randomSeed;
unsigned _timeThen;
unsigned _timeNow;


// This function is called from simtools/simtools.m at startup:

void
initRandom (id arguments)
{
  int i;
  struct timeval then;

// Initialize time base (for Solaris):

   i = clock();
   i = sleep(1);
   i = clock();
 
// Default mode is that starting seeds are fixed (as in 0.6):
   _useFixedSeed = YES;

  if ([arguments getVarySeedFlag])
    {
      _useFixedSeed = NO;
      _firstSeed = PIDTIMESEED;
    }
  else if ([arguments getFixedSeedFlag])
    _firstSeed = [arguments getFixedSeed];
  else
    _firstSeed = DEFAULTSEED;
    

// Initialize the inline RNG:

   _randomSeed = _firstSeed;

// Save the time-of-day in microseconds for use later in computing RANDOMSEED:

   gettimeofday (&then, NULL);
   _timeThen = then.tv_usec + 1000000 * ( then.tv_sec % 2048 );


// Create the utility objects here:

   randomGenerator = [ MT19937gen create: globalZone 
			setStateFromSeed: _firstSeed ];

   uniformIntRand  = [ UniformIntegerDist create: globalZone
			setGenerator: randomGenerator ];
   uniformUnsRand  = [ UniformUnsignedDist create: globalZone
			setGenerator: randomGenerator ];
   uniformDblRand  = [ UniformDoubleDist create: globalZone
			setGenerator: randomGenerator ];
// Comments: 
// 
//  1. the MT19937 generator is fast and has a *long* period.
//     It is initialized with either a fixed or a randomized seed, 
//     depending on the use of the 'varySeed' command line flag.
// 
//  2. the distribution objects are all connected to this generator,
//     getting their random numbers from it in an interleaved fashion.
// 
}

// These two functions are used by the macros in RandomDefs.h
// to generate 'random' seed values:

unsigned int
tempusFugit (void)
{
  struct timeval now;
  
  gettimeofday(&now, NULL);
  _timeNow = now.tv_usec + 1000000 * ( now.tv_sec % 2048 );
  
  if ( _timeNow > _timeThen ) 
    return ( _timeNow - _timeThen );
  else if ( _timeNow < _timeThen )
    return ( _timeThen - _timeNow );
  else 
    // do *not* return 0:
    return DEFAULTSEED2;
}

unsigned int
nextSeed(void)
{
  _randomSeed = ( _randomSeed * 39039 );	// implicitly, mod 2^32
  return _randomSeed;
}

