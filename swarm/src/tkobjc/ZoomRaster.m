// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include "internal.h"

#import <tkobjc/global.h>
#import <tkobjc/ZoomRaster.h>
#import <tkobjc/Drawer.h>

@implementation ZoomRaster

- (unsigned)getWidth
{
  return logicalWidth;
}

- (unsigned)getHeight
{
  return logicalHeight;
}

- _setConfigureInfo_ : (const char *)eventName
{
  [globalTkInterp 
    eval: 
      "bind %s <%s> { %s handle%sWidth: %s Height: %s }",
    [parent getWidgetName], 
    eventName,
    [self getObjcName],
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
    [WindowCreation 
      raiseEvent: 
        "Warning: ZoomRaster created as child of non toplevel.\n"
      "Resize code probably\nwill not work.\n"];
  
  logicalWidth = width;
  logicalHeight = height;
  zoomFactor = 1U;

  [self _setConfigureInfo_: "Configure"];
  [self _setConfigureInfo_: "Expose"];
  return self;
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
  [self setWidth: logicalWidth Height: logicalHeight];

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
- handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  unsigned newZoom = newHeight / logicalHeight;

  while (newZoom != newWidth / logicalWidth)
    {
      // In this case, assume that Windows refused to make the window
      // that narrow.  Retry.
      if (newWidth > newHeight)
        newWidth = newHeight;  
      else
        [WindowUsage
          raiseEvent:
            "nonsquare zoom given (nz:%u nh:%lu nw:%u lh: %u lw:%u).\n",
          newZoom, newHeight, newWidth, logicalHeight, logicalWidth];
    }
  
#ifdef DEBUG
  printf("Handling configure for %s\noldZoom: %u newZoom: %u, newWidth = %u newHeight = %u\n",
	 [self getObjcName], zoomFactor, newZoom, newWidth, newHeight);
#endif
  
  // This check isn't just an optimization, it prevents an infinite
  // recursion: [self setZoomFactor] reconfigures the widget, which in turn
  // generates a configure event.
  if (newZoom != zoomFactor)
    [self setZoomFactor: newZoom];
  return self;
}

- handleExposeWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  if (newWidth > width || newHeight > height)
    tkobjc_raster_clear (self, width, height);
  return self;
}
  
// override setWidth to set it for them according to zoom factor.
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  logicalWidth = newWidth;
  logicalHeight = newHeight;

  [super setWidth: newWidth * zoomFactor Height: newHeight * zoomFactor];

  // Set up gridded geometry so this is resizeable. Only works if
  // the parent is a toplevel.
  [globalTkInterp eval: "wm grid %s %u %u %u %u; wm aspect %s 1 1 1 1",
		  [parent getWidgetName],  zoomFactor, zoomFactor,
		  logicalWidth, logicalHeight, [parent getWidgetName]];
  return self;
}

// drawing is just like before, only magnified.
- drawPointX: (int)x Y: (int)y Color: (Color)c
{
  [super fillRectangleX0: x * zoomFactor Y0: y * zoomFactor
         X1: (x + 1) * zoomFactor Y1: (y + 1) * zoomFactor
	 Color: c];

  return self;
}

- fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c
{
  [super fillRectangleX0: x0 * zoomFactor Y0: y0 * zoomFactor
         X1: x1 * zoomFactor Y1: y1 * zoomFactor
	 Color: c];

  return self;
}

- draw: (id <Drawer>)xd X: (int)x Y: (int)y
{
  return [super draw: xd X: x * zoomFactor Y: y * zoomFactor];
}

- increaseZoom
{
  return [self setZoomFactor: zoomFactor + 1U];
}

- decreaseZoom
{
  if (zoomFactor > 1U)
    return [self setZoomFactor: zoomFactor - 1U];
  else
    return self;
}

// scale by zoom factor
- handleButton: (int)n X: (int)x Y: (int)y
{
  return [super handleButton: n
                X: (x / (int)zoomFactor)
                Y: (y / (int)zoomFactor)];
}

@end
