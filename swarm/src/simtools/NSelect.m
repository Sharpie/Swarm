// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         NSelect.m
Description:  Selection Routine
Library:      simtools
*/

#import <simtools.h>
#import <simtools/NSelect.h>
#import <random.h>

//S: A class to select exactly N elements at random from a collection.
//D: NSelect selects exactly N elements from a collection without repetition.
//D: A target collection must be provided.
@implementation NSelect

//M: The select:from:into: method selects exactly N elements from a collection
//M: without repetition into another collection.  The selection algorithm was 
//M: written by Donald Knuth.
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
      id <Error> NSelectTooMany;

      deferror (NSelectTooMany, NULL);

      [NSelectTooMany
        raiseEvent: 
          "NSelect: attempted to select %d elements from a collection containing only %d elements.\n", n, N];
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
