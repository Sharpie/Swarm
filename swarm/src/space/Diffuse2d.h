// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Approximation to 2d diffusion via CA.

#import <space/Ca2d.h>

@interface Diffuse2d: Ca2d {
  double diffusionConstant;
  double evaporationRate;
}

-setDiffusionConstant: (double) d;
-setEvaporationRate: (double) e;

@end

