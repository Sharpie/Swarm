// Swarm library. Copyright © 1996-2000 Swarm Development Group.  This
// program is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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

#include "internal.h"

#import <tkobjc/global.h>
#import <gui.h>
#import <tkobjc/ZoomRaster.h>

#define USE_GRID

@implementation ZoomRaster

PHASE(Creating)

- _setConfigureInfo_: (const char *)eventName
{
  [globalTkInterp 
    eval: 
      "bind %s <%s> { %s handle%sWidth: %s Height: %s }",
    [parent getWidgetName], 
    eventName,
    [self getObjectName],
    eventName,
    "%w", "%h"];
  return self;
}

- createEnd
{
  [super createEnd];
  // We do things to the parent widget that are really only allowed
  // on toplevels.  This check is at least friendly.
  if (!([parent isKindOfClassNamed: "Frame"]) && ([parent getParent] == 0))
    raiseEvent (WindowCreation,
                "Warning: ZoomRaster created as child of non toplevel.\n"
                "Resize code probably\nwill not work.\n");
  
  logicalWidth = width;
  logicalHeight = height;
  zoomFactor = 1U;

  [self _setConfigureInfo_: "Configure"];
  [self _setConfigureInfo_: "Expose"];
  return self;
}

PHASE(Using)

- (void)pack
{
  while (GUI_EVENT_ASYNC ()) { };
  configureFlag = YES;
  [super pack];
}

- (unsigned)getWidth
{
  return logicalWidth;
}

- (unsigned)getHeight
{
  return logicalHeight;
}

- (unsigned)getZoomFactor
{
  return zoomFactor;
}

// the REDRAWONZOOM code handles redrawing the window when zooming.
// Unfortunately, it's a bit buggy and only works well when there are no
// pixmaps drawn to the Raster, just squares.
- setZoomFactor: (unsigned)z
{
  unsigned oldZoom;
#ifdef REDRAWONZOOM
  XImage *oldImage;
  XGCValues oldgcv;
  unsigned x, y;
#endif

  // save necessary state: old image, old zoom.
  oldZoom = zoomFactor;
#ifdef REDRAWONZOOM
  oldImage = XGetImage (display, pm, 0, 0, width, height, AllPlanes, XYPixmap);
#endif

  // zoom ourselves
  zoomFactor = z;
#ifdef USE_GRID
  [self setWidth: logicalWidth Height: logicalHeight];
#else
  if ([super getWidth] != logicalWidth * zoomFactor
      || [super getHeight] != logicalHeight * zoomFactor)
    {
      char buf[40];
      
      [self setWidth: logicalWidth Height: logicalHeight];
      sprintf (buf,
               "%ux%u+%u+%u",
               logicalWidth * zoomFactor, logicalHeight * zoomFactor,
               [self getX], [self getY]);
      [self setWindowGeometry: buf];
    }
#endif

#ifdef REDRAWONZOOM
  // now build a new image from the data in the old one.
  // I hope this use of oldPixel is portable: why is image support so lousy?
  XGetGCValues (display, gc, GCForeground, &oldgcv);   // save old colour
  for (x = 0; x < logicalWidth; x++)
    for (y = 0; y < logicalHeight; y++) {
      unsigned long oldPixel;
      oldPixel = XGetPixel (oldImage, x*oldZoom, y*oldZoom);
      XSetForeground (display, gc, oldPixel);
      XFillRectangle (display, pm, gc, x*zoomFactor, y*zoomFactor,
		     zoomFactor, zoomFactor);
    }
  XChangeGC (display, gc, GCForeground, &oldgcv);  // now restore colour
  
  XDestroyImage (oldImage);
#endif
  
  return self;
}

// handler for tk4.0 <Configure> events - width and height is passed to us.
// note that they are passed to us in absolute values, not gridded.
- (void)handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  if (configureFlag)
    {
      unsigned newZoomH = newHeight / logicalHeight;
      unsigned newZoomW = newWidth / logicalWidth;
      unsigned newZoom = newZoomH > newZoomW ? newZoomH : newZoomW;
      
      if (newZoom == 0)
        newZoom = 1;
      
      [self setZoomFactor: newZoom];
    }
}

- (void)handleExposeWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  if (newWidth > width || newHeight > height)
    tkobjc_raster_clear (self, width, height);
}
  
// override setWidth to set it for them according to zoom factor.
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  logicalWidth = newWidth;
  logicalHeight = newHeight;

  [super setWidth: newWidth * zoomFactor Height: newHeight * zoomFactor];

#ifdef USE_GRID
  // Set up gridded geometry so this is resizeable. Only works if
  // the parent is a toplevel.
  [globalTkInterp eval: "wm grid %s %u %u %u %u; wm aspect %s %u %u %u %u",
		  [parent getWidgetName],  
                  zoomFactor, zoomFactor,
		  logicalWidth, logicalHeight,
                  [parent getWidgetName],
                  logicalWidth, logicalHeight,
                  logicalWidth, logicalHeight];
#endif
  return self;
}

// The point's coordinates are (x,y), but when the zoomFactor is 1, it
// is drawn from (x,y) to (x+1,y+1).  Such a point is not really
// centered at (x,y), but we have no alternative.  If the zoomFactor
// is increased, the coordinates over which the point is stretched
// will expand.  This method uses a centering algorthm so that
// zoomFactor*(x,y) is at the "center" of the rectangle that
// represents the points. For example, a point at (10,10) in a
// zoomFactor 1 window will be centered at (100,100) in a zoomFactor
// 10 window, and the width of the point will be represented by 10x10
// square.  The new square stretches from (95,95) (on the top left) to
// (105,105) (on the bottom left).
- (void)drawPointX: (int)x Y: (int)y Color: (Color)c
{
  [self fillCenteredRectangleX0: x  Y0: y 
         X1: (x + 1)  Y1: (y + 1) 
	 Color: c];
}

- (void)fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c
{
  [super fillRectangleX0: x0 * zoomFactor Y0: y0 * zoomFactor
         X1: x1 * zoomFactor Y1: y1 * zoomFactor
	 Color: c];
}


// When the zoom factor is increased, the previous method
// "fillRectangle" stretches all pixels down and to the right.  On the
// other hand, when X resizes lines, it expands the line in both
// directions.  Thus, zooming can make a line that is originally
// inside a rectangle "walk" out of it.  Suppose instead we want to be
// sure that, if a point located at (x0,y0) is fully inside the
// rectangle, then when the raster is zoomed up or down, the point is
// still inside the rectangle.  Then use this to draw your rectangle!
// We can't tweak "fillRectangle" to do the same thing because that
// method fills other roles, like drawing the background, and so the
// whole class breaks if we fiddle with that. Note this method is
// used by draw point because we want centered points.

- (void)fillCenteredRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c
{
  int newX0 = x0 * zoomFactor - floor(0.5 * zoomFactor);
  int newY0 = y0 * zoomFactor - floor(0.5 * zoomFactor);
  int diffX = zoomFactor * (x1 - x0);
  int diffY = zoomFactor * (y1 - y0);
  tkobjc_raster_fillRectangle (self, newX0, newY0, diffX, diffY, c);
}


- (void)ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
            Width: (unsigned)penWidth Color: (Color)c
{
  tkobjc_raster_ellipse (self,
                         x0 * zoomFactor, y0 * zoomFactor,
                         (x1 - x0) * zoomFactor, (y1 - y0) * zoomFactor,
                         penWidth * zoomFactor, c);
}

- (void)lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
         Width: (unsigned)penWidth Color: (Color)c
{
  tkobjc_raster_line (self,
                      x0 * zoomFactor, y0 * zoomFactor,
                      x1 * zoomFactor, y1 * zoomFactor,
                      penWidth * zoomFactor, c);
}

// This is a rectangle with position designed so that zoom resizing
// does not alter its logical positions. It is intended to be invariant,
// so that a point zoomFactor*(x0,y0) should either be in or out of
// the rectangle for all zoomFactors. 
// It should overlap on the edges of a "fillCenteredRectangle" drawing 

- (void)rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
              Width: (unsigned)penWidth Color: (Color)c
{
  int diffX = zoomFactor * (x1 - x0);
  int diffY = zoomFactor * (y1 - y0);
  int newX0 = x0 * zoomFactor - floor (0.5 * zoomFactor);
  int newY0 = y0 * zoomFactor - floor (0.5 * zoomFactor);

  tkobjc_raster_rectangle (self,
                           newX0, newY0,
			   diffX, diffY,
                           penWidth * zoomFactor, c);
}

- (void)draw: (id <Drawer>)drawer X:(int)x Y: (int)y
{
  [drawer drawX: x * zoomFactor Y: y * zoomFactor];
}

- (void)increaseZoom
{
  [self setZoomFactor: zoomFactor + 1U];
}

- (void)decreaseZoom
{
  if (zoomFactor > 1U)
    [self setZoomFactor: zoomFactor - 1U];
}

// scale by zoom factor
- (void)handleButton: (int)n X: (int)x Y: (int)y
{
  x += floor (.5 * zoomFactor);
  y += floor (.5 * zoomFactor);
  [super handleButton: n
         X: (x / (int) zoomFactor)
         Y: (y / (int) zoomFactor)];
}

@end
