// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

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

// drawing is just like before, only magnified.
- (void)drawPointX: (int)x Y: (int)y Color: (Color)c
{
  [super fillRectangleX0: x * zoomFactor Y0: y * zoomFactor
         X1: (x + 1) * zoomFactor Y1: (y + 1) * zoomFactor
	 Color: c];
}

- (void)fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c
{
  [super fillRectangleX0: x0 * zoomFactor Y0: y0 * zoomFactor
         X1: x1 * zoomFactor Y1: y1 * zoomFactor
	 Color: c];
}

- (void)ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
            Width: (unsigned)penWidth Color: (Color)c
{
  tkobjc_raster_ellipse (self,
                         x0 * zoomFactor, y0 * zoomFactor,
                         (x1 - x0) * zoomFactor, (y1 - y0) * zoomFactor,
                         penWidth, c);
}

- (void)lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
         Width: (unsigned)penWidth Color: (Color)c
{
  tkobjc_raster_line (self,
                      x0 * zoomFactor, y0 * zoomFactor,
                      x1 * zoomFactor, y1 * zoomFactor,
                      penWidth, c);
}

- (void)rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
              Width: (unsigned)penWidth Color: (Color)c
{
  tkobjc_raster_rectangle (self,
                           x0 * zoomFactor, y0 * zoomFactor,
                           (x1 - x0) * zoomFactor, (y1 - y0) * zoomFactor,
                           penWidth, c);
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
  [super handleButton: n
         X: (x / (int) zoomFactor)
         Y: (y / (int) zoomFactor)];
}

@end
