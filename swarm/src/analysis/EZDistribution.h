// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h>

// EZDistribution object: used to generate distributions.

@interface EZDistribution : EZBin {
  double *probabilities ;
  double entropy, maximumEntropy ;
}

-(double *)getProbabilities ;
-(double) getEntropy ;

@end



