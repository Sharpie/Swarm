// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/Entropy.h>
#import <collections.h>
#import <defobj/defalloc.h> // getZone
#include <misc.h> // log

@implementation Entropy

PHASE(Creating)
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

PHASE(Setting)
PHASE(Using)

- (void)update
{
  id iter, obj;
  double maximum;
  int count;

  entropy = 0.0;

  count = [collection getCount];   

  if (!count)
    return;
  
  maximum = log (1.0 / ((double) count));

  obj = [collection getFirst];
  
  iter = [collection begin: getZone (self)];
  while ((obj = [iter next]) != nil)
    {
      double v = [self doubleDynamicCallOn: obj];
      
      if(v > 0.0)
        entropy += v * log(v);
    }
  [iter drop];
  
  entropy /= maximum;
}

- (double)getEntropy
{
  return entropy;
} 

@end
