// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h> // Entropy
#import <objectbase/MessageProbe.h>

// Entropy object: calculates the entropy based on the probabilities returned
// by a collection of objects responding to a specified selector.

@interface Entropy: MessageProbe <Entropy>
{
  double entropy;
  id collection;
}

- setCollection: aCollection;
- createEnd;	

- (void)update;		
- (double)getEntropy;
@end
