// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <analysis/Averager.h>

// Implemenation of data collection facilities. Code like this
// eventually belongs in the Swarm libraries.

// Averager: averages together data, gives the data to whomever asks.
@implementation Averager

-setCollection: (id) l {
  collection = l;
  return self;
}

-createEnd {
  if (collection == nil)
    [InvalidCombination raiseEvent: "Averager created without a collection\n"];

  return [super createEnd];
}

// Update: run through the collection calling the selector on each object,
// average the results. Could easily be extended to collect other
// statistics: variance, max, min, etc.
-update {
  id iter, obj;

  total = 0.0;
  count = 0;

  // special case empty collection.
  if ([collection getCount] == 0) {
    min = 0;
    max = 0;
    return self;
  }
  
  obj = [collection getFirst];
  [self updateMethodCache: obj];

  max = [self doubleDynamicCallOn: obj];
  min = max;
  
  // Ok, we have cached our function to call on each object - do it.
  // note that we don't do lookup for each step: this code only works
  // if the collection is homogeneous.
  iter = [collection begin: [self getZone]];
  while ((obj = [iter next]) != nil) {
    double v = [self doubleDynamicCallOn: obj];
    
    total += v;
    if (v > max)
      max = v;
    if (v < min)
      min = v;
    count++;
  }

  [iter drop] ;

  return self;
}

-(double) getAverage {
  if(count)
    return total/count;
  else 
    return 0 ;
} 

-(double) getTotal {
  return total;
}

-(double) getMax {
  return max;
}

-(double) getMin {
  return min;
}

-(int) getCount {
  return count;
}

@end
