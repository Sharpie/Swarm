// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

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

// Make utility objects globally accessible:

// externvar id <MT19937gen> randomGenerator;
externvar id <SimpleRandomGenerator> randomGenerator;
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

