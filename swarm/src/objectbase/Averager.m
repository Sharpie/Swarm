// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <swarmobject/Averager.h>

// Implemenation of data collection facilities. Code like this
// eventually belongs in the Swarm libraries.

// Averager: averages together data, gives the data to whomever asks.
@implementation Averager

-setList: (id) l {
  list = l;
  return self;
}

-createEnd {
  if (list == nil)
    [InvalidCombination raiseEvent: "Averager created without a list\n"];

  return [super createEnd];
}

// Update: run through the list calling the selector on each object,
// average the results. Could easily be extended to collect other
// statistics: variance, max, min, etc.
-update {
  id iter, obj;

  total = 0.0;
  count = 0;

  // special case empty list.
  if ([list getCount] == 0) {
    min = 0;
    max = 0;
    return self;
  }
  
  obj = [list first];
  [self updateMethodCache: obj];

  max = [self doubleDynamicCallOn: obj];
  min = max;
  
  // Ok, we have cached our function to call on each object - do it.
  // note that we don't do lookup for each step: this code only works
  // if the list is homogeneous.
  iter = [list begin: zone];
  while ((obj = [iter next]) != nil) {
    double v = [self doubleDynamicCallOn: obj];
    
    total += v;
    if (v > max)
      max = v;
    if (v < min)
      min = v;
    count++;
  }
  return self;
}

-(double) getAverage {
  return total/count;
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
