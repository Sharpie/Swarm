// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <analysis/Averager.h>
#import <defobj/defalloc.h> // getZone

@implementation Averager

PHASE(Creating)

- setCollection: l
{
  collection = l;
  return self;
}

- createEnd
{
  if (collection == nil)
    [InvalidCombination raiseEvent: "Averager created without a collection\n"];

  return [super createEnd];
}

PHASE(Setting)

PHASE(Using)

- update
{
  id obj;

  total = 0.0;
  count = 0;

  // special case empty collection.
  if ([collection getCount] == 0)
    {
      min = 0;
      max = 0;
      return self;
    }
  
  obj = [collection getFirst];
  
  max = [self doubleDynamicCallOn: obj];
  min = max;
  
  {
    id <Index> iter;
    
    iter = [collection begin: getZone (self)];
    for (obj = [iter next]; [iter getLoc] == Member; obj = [iter next])
      {
	double v = [self doubleDynamicCallOn: obj];
	
	total += v;
	if (v > max)
	  max = v;
	if (v < min)
	  min = v;
	count++;
      }
    [iter drop];
  }
  
  return self;
}

- (double)getAverage
{
  if (count)
    return total / (double)count;
  else 
    return 0.0;
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

@end
