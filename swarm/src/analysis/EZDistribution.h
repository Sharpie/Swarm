// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h> // EZBin
#import <analysis/EZBin.h>

// EZDistribution object: used to generate distributions.

@interface EZDistribution: EZBin <EZDistribution>
{
  double *probabilities;
  double entropy, maximumEntropy;
}

- (double *)getProbabilities;
- (double)getEntropy;

@end



