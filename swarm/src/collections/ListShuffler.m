// Swarm library. Copyright © 1996-2000 Swarm Development Group. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections/ListShuffler.h>

@protocol _MinimalRandom
- (unsigned)getUnsignedWithMin: (unsigned)minVal withMax: (unsigned)maxVal;
@end

extern id uniformUnsRand;

@implementation ListShuffler_c

PHASE(Creating)

- setUniformRandom: rnd 
{
  if (uniformRandom)
    raiseEvent (InvalidArgument, 
                "ListShuffler: You can only set the UniformUnsigned object once\n");
  
  uniformRandom = rnd;

  if ( (!uniformRandom) || 
      (![uniformRandom respondsTo: M(getUnsignedWithMin:withMax:)]))
    raiseEvent (InvalidArgument,
                "ListShuffler: need a UniformUnsigned distribution object!\n");
  return self;
}

- createEnd 
{
  if (!uniformRandom) 
    {
      // (a) Use the default system object:
      uniformRandom = uniformUnsRand;	// defined in <random/random.m> 
      
      // (b) Complain and quit:
      // raiseEvent (InvalidArgument,
      // "ListShuffler was created without a random number generator.\n");
    }
  setNextPhase(self);
  return self;
}

+ create: aZone setUniformRandom: dist
{
  ListShuffler_c *shuffler = [self createBegin: aZone];
  [shuffler setUniformRandom: dist];
  shuffler = [shuffler createEnd];
  
  return shuffler;
}

PHASE(Using)

- shufflePartialList: list Num: (unsigned)num 
{
  unsigned j, k, m;

  j = num;

  m = [list getCount];
  if (m < 2)
    return list;

  if (j > m)
    j = m; // silent -- no error message

  // Shuffle the lowest num elements of list:
  
  while (j > 1) 
    {
      // Get a k that's uniform over [0,j-1]:
      k = [uniformRandom getUnsignedWithMin: 0 withMax: j-1];
      
      j--;
      
      [list atOffset: j put: [list atOffset: k put: [list atOffset: j]]];
    }
  
  return list;
}

- shuffleWholeList: list 
{
  unsigned j, k;

  j = [list getCount];

  if (j < 2)
    return list;
  
  while (j > 1)
    {
      // Get a k that's uniform over [0,j-1]:
      k = [uniformRandom getUnsignedWithMin: 0 withMax: j - 1];
      
      j--;
      
      [list atOffset: j put: [list atOffset: k put: [list atOffset: j]]];
    }
  return list;
}

@end
