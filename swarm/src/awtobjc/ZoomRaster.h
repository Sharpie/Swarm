// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C interface for a Raster with a zoom factor.

#import <awtobjc/Raster.h>

@interface ZoomRaster: Raster
{
  unsigned zoomFactor;
  unsigned logicalWidth, logicalHeight;
}

- increaseZoom;
- decreaseZoom;
- (unsigned)getZoomFactor;
- setZoomFactor: (unsigned)z;

@end
