// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <analysis/Averager.h>
#import <defobj/defalloc.h> // getZone
#include <misc.h> // sqrt

@implementation Averager

PHASE(Creating)

- setCollection: obj
{
  target = obj;
  return self;
}

- setWidth: (unsigned)width
{
  maWidth = width;
  return self;
}

- createEnd
{
  isList = [target respondsTo: M(getFirst)];

  total = 0.0;
  totalSquared = 0.0;

  if (target == nil)
    raiseEvent (InvalidCombination, "Averager created without a target\n");
  
  if (maWidth == 0 && !isList)
    raiseEvent (InvalidCombination,
                "Averages of non-collections must having a interval width\n");
  
  if (maWidth > 0)
    {
      unsigned i;

      if (isList)
        maWidth *= [target getCount];

      maData = [getZone (self) allocBlock: maWidth * sizeof (double)];

      for (i = 0; i < maWidth; i++)
        maData[i] = 0.0;

      maTotal = 0.0;
      maTotalSquared = 0.0;
    }
  setMappedAlloc (self);
  return [super createEnd];
}

PHASE(Setting)

PHASE(Using)

- (void)addValueToAverage: (double)v
{
  total += v;
  totalSquared += v * v;

  if (count == 0)
    max = min = v;
  else
    {
      if (v > max)
        max = v;
      else if (v < min)
        min = v;
    }
  if (maWidth > 0)
    {
      if (count < maWidth)
        {
          maTotal += v;
          maTotalSquared += v * v;
          maData[count] = v;
        }
      else
        {
          unsigned maPos = count % maWidth;
          double obsolete = maData[maPos];
      
          maTotal = maTotal - obsolete + v; 
          maTotalSquared = (maTotalSquared - 
                            obsolete * obsolete +
                            v * v);
          maData[maPos] = v;
        }
    }
  count++;
}

- (void)update
{
  if (isList)
    {
      if (maWidth == 0)
        {
          count = 0;
          total = 0.0;
          totalSquared = 0.0;
        }
      
      // special case empty collection.
      if ([target getCount] == 0)
        {
          min = 0;
          max = 0;
          return;
        }
      
      {
        id <Index> iter;
        id obj;
        
        iter = [target begin: scratchZone];
        for (obj = [iter next]; [iter getLoc] == Member; obj = [iter next])
          [self addValueToAverage: [self doubleDynamicCallOn: obj]];
        [iter drop];
      }
    }
  else
    [self addValueToAverage: [self doubleDynamicCallOn: target]];
}

- (double)getAverage
{
  if (count)
    return total / (double) count;
  else 
    return 0.0;
} 

- (double)getMovingAverage
{
  if (count  == 0)
    return 0.0;

  return maTotal / ((count < maWidth) ? count : maWidth);
}


- (double)getVariance
{
  double mean = total / (double) count;

  return (((double) count / ((double) (count - 1))) * 
          (totalSquared / (double) count - mean * mean));
}

- (double)getMovingVariance
{
  double movingMean = [self getMovingAverage];
  double actualCount = (count < maWidth) ? count : maWidth;

  return (((double) actualCount / ((double) (actualCount - 1))) * 
          (maTotalSquared / (double) count - movingMean * movingMean));
}

- (double)getStdDev
{
  return sqrt ([self getVariance]);
}

- (double)getMovingStdDev
{
  return sqrt ([self getMovingVariance]);
}

- (double)getTotal
{
  return total;
}

- (double)getMax
{
  return max;
}

- (double)getMin
{
  return min;
}

- (unsigned)getCount 
{
  return count;
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
  mapalloc->size = maWidth * sizeof (double);
  mapAlloc (mapalloc, maData);
}

@end
