// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Approximation to 2d diffusion via CA.

#import <space.h> // Diffuse2d
#import <space/Ca2d.h>

@interface Diffuse2d: Ca2d <Diffuse2d>
{
  double diffusionConstant;
  double evaporationRate;
}

+ createBegin: aZone;
- initializeLattice;
- setDiffusionConstant: (double)d;
- setEvaporationRate: (double)e;
- stepRule;
@end

