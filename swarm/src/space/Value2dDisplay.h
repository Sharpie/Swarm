// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Discrete2d.h>
#import <space.h>
#import <gui.h>

// generic object to handle display 2d values

@interface Value2dDisplay: SwarmObject
{
  id <Raster> displayWidget;
  IMP drawPointImp;
  id <Colormap> colormap;
  id <Discrete2d> discrete2d;			  // read only
  int numCaStates;
  int modFactor;
  int colorConstant;
}

- setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c;
- setDiscrete2dToDisplay: c;
- setDisplayMappingM: (int)m C: (int)c;	  // linear mapping
- display;
@end

