// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h>
#import <simtools/NSelect.h>
#import <collections.h> // getCount, addLast
#import <defobj.h> // InvalidArgument
#import <random.h>

@implementation NSelect

+ (void)select: (int)n from: aCollection into: bCollection
{
  id a;
  int N; // total number of items in aCollection
  int t; // items seen
  int m; // items selected
  float r;

  if (!n)
    return;

  t = m = 0;

  N = [aCollection getCount];

  if (N < n)
    {
      raiseEvent (InvalidArgument,
                  "NSelect: attempted to select %d elements from a collection containing only %d elements.\n",
                  n, N);
    }
  
  a = [aCollection begin: scratchZone];
  
  while (m < n)
    {
      r = (float)[uniformDblRand getDoubleWithMin:0 withMax: 1.0];    
      
      if ((((float)(N - t)) * r) >= ((float)(n - m)))
        [a next];
      else
        {
          m++;
          [bCollection addLast: [a next]];
        }
      t++;
    }
  
  [a drop];
}

@end
