// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space.h> // Value2dDisplay, Discrete2d
#import <space/Discrete2d.h>
#import <gui.h> // Raster, Colormap

// generic object to handle display 2d values

@interface Value2dDisplay: SwarmObject <Value2dDisplay>
{
  id <Raster> displayWidget;
  IMP drawPointImp;
  id <Colormap> colormap;
  id <GridData> discrete2d;  // read only
  int numCaStates;
  int modFactor;
  int colorConstant;
}
+ create: aZone setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c setDiscrete2dToDisplay: (id <GridData>)d;
- setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c;
- setDiscrete2dToDisplay: (id <GridData>)c;
- setDisplayMappingM: (int)m C: (int)c;	  // linear mapping
- display;
@end

