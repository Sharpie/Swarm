// Swarm library. Copyright (C) 1996 Santa Fe Institute. This library is
//   distributed without any warranty; without even the implied warranty
//   of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections/ListShuffler.h>
#import <random.h>

@implementation ListShuffler_c

PHASE(Creating)

- (void)setUniformRandom: rnd 
{
  if (uniformRandom) 
    [InvalidArgument
      raiseEvent: 
        "ListShuffler: You can only set the UniformUnsigned object once\n"];
  
  uniformRandom = rnd;

  if ( (!uniformRandom) || 
      (![uniformRandom respondsTo: M(getUnsignedWithMin:withMax:)]))
    [InvalidArgument
      raiseEvent:
        "ListShuffler: need a UniformUnsigned distribution object!\n"];
}

- createEnd 
{
  if (!uniformRandom) 
    {
      // (a) Use the default system object:
      uniformRandom = uniformUnsRand;	// defined in <random/random.m> 
      
      // (b) Complain and abort:
      // [InvalidCombination raiseEvent: 
      // "ListShuffler was created without a random number generator.\n"];
    }
  
  return self;
}

+ create: aZone setUniformRandom: dist
{
  ListShuffler_c *shuffler;
  
  shuffler = [ListShuffler_c createBegin: aZone];
  [shuffler setUniformRandom: dist];
  shuffler = [shuffler createEnd];
  
  return shuffler;
}

PHASE(Using)

- shufflePartialList: list Num: (int) num 
{
  int j, k, m;

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
  int j,k;

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
