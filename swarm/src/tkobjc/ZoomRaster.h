// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C interface for a Raster with a zoom factor.

#import <tkobjc/Raster.h>
#import <tkobjc/Frame.h>
#import <gui.h>

@interface ZoomRaster: Raster <_ZoomRaster>
{
  unsigned zoomFactor;
  unsigned logicalWidth, logicalHeight;
}

- increaseZoom;
- decreaseZoom;
- (unsigned)getZoomFactor;
- setZoomFactor: (unsigned)z;
- handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight;
- handleExposeWidth: (unsigned)newWidth Height: (unsigned)newHeight;

- ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Width: (unsigned)penWidth Color: (Color)c;
- lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Width: (unsigned)penWidth Color: (Color)c;
- rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Width: (unsigned)penWidth Color: (Color)c;

- draw: (id <Drawer>)drawer X: (int)x Y: (int)y;
@end
