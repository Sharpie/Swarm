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

// Objective C interface to Raster, for use with tclobjc.

#import <tkobjc/ArchivedGeometryWidget.h>
#import <gui.h>

// This could include a list of environments (graphics contexts).
@interface Raster: ArchivedGeometryWidget <Raster>
{
  id button1Client, button2Client, button3Client;
  SEL button1Sel, button2Sel, button3Sel;
  PixelValue *map;
@public
  id <Colormap> colormap;
  void *private;
  int eraseColor;
  unsigned width, height;
}
+ createBegin: aZone;
- createEnd;
- setWidth: (unsigned)width;
- setHeight: (unsigned)height;
- setWidth: (unsigned)width Height: (unsigned)height;
- (id <Colormap>)getColormap;
- setColormap: (id <Colormap>)colormap;
- (void)drawPointX: (int)x Y: (int)y Color: (Color)c;
- (void)fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c;
- (void)ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Width: (unsigned)width Color: (Color)c;
- (void)lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Width: (unsigned)width Color: (Color)c;
- (void)rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Width: (unsigned)width Color: (Color)c;
- (void)draw: (id <Drawer>)drawer X: (int)x Y: (int)y;
- (void)drawSelf;
- (void)erase;
- (void)handleButton: (int)n X: (int)x Y: (int)y;
- (void)setButton: (int)n Client: c Message: (SEL)s;
@end
