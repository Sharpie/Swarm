// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <math.h>
#import <collections.h>
#import <analysis.h>

@implementation Entropy

//* The setCollection method sets the collection of objects that will be 
//* probed.
- setCollection: aCollection
{
  collection = aCollection;
  return self;
}

- createEnd 
{
  if (collection == nil)
    [InvalidCombination raiseEvent: "Entropy created without a collection\n"];

  return [super createEnd];
}

//* The update method polls the collection and updates the entropy. This method
//* should be scheduled prior to collecting the data using getEntropy.
- update
{
  id iter, obj;
  double maximum;
  int count;

  entropy = 0.0;

  count = [collection getCount];   

  if(!count)
    return self;
  
  maximum = log( 1.0 / ((double) count));

  obj = [collection getFirst];
  // [self updateMethodCache: obj];
  
  // Ok, we have cached our function to call on each object - do it.
  // note that we don't do lookup for each step: this code only works
  // if the collection is homogeneous.
  iter = [collection begin: [self getZone]];
  while ((obj = [iter next]) != nil)
    {
      double v = [self doubleDynamicCallOn: obj];
      
      if(v > 0.0)
        entropy += v * log(v);
    }
  [iter drop];
  
  entropy /= maximum;
  
  return self;
}

//* The getEntropy method returns the calculated Entropy. The entropy value
//* is read out of the object, not computed everytime it is requested.
- (double)getEntropy
{
  return entropy;
} 

@end
