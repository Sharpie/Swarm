// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/MessageProbe.h>

// Entropy object: calculates the entropy based on the probabilities returned
// by a collection of objects responding to a specified selector.

@interface Entropy: MessageProbe
{
  double entropy;
  id collection;
}

- setCollection: aCollection;
- createEnd;	

- update;		
- (double)getEntropy;
@end
