// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C interface for a Raster with a zoom factor.

#import <tkobjc/Raster.h>
#import <tkobjc/Frame.h>
#import <gui.h>

@interface ZoomRaster: Raster <ZoomRaster>
{
  unsigned zoomFactor;
  unsigned logicalWidth, logicalHeight;
  BOOL configureFlag;
}
- _setConfigureInfo_: (const char *)eventName;
- createEnd;
- (void)pack;
- (unsigned)getWidth;
- (unsigned)getHeight;
- (unsigned)getZoomFactor;
- setZoomFactor: (unsigned)z;
- (void)handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight;
- (void)handleExposeWidth: (unsigned)newWidth Height: (unsigned)newHeight;
- (void)setWidth: (unsigned)newWidth Height: (unsigned)newHeight;
- (void)drawPointX: (int)x Y: (int)y Color: (Color)c;
- (void)fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c;
- (void)ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
            Width: (unsigned)penWidth Color: (Color)c;
- (void)lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
         Width: (unsigned)penWidth Color: (Color)c;
- (void)rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
              Width: (unsigned)penWidth Color: (Color)c;
- (void)draw: (id <Drawer>)drawer X:(int)x Y: (int)y;
- (void)increaseZoom;
- (void)decreaseZoom;
- (void)handleButton: (int)n X: (int)x Y: (int)y;


@end
