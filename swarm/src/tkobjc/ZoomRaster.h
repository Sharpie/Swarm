// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

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
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight;
- (void)drawPointX: (int)x Y: (int)y Color: (Color)c;
- (void)fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c;
- (void)fillCenteredRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c;
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
