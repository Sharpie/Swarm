// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tclObjc.h>
#import <tkobjc/ZoomRaster.h>
#import <tkobjc/TkExtra.h>
#include <X11/Xutil.h>

@implementation ZoomRaster

-createEnd {
  [super createEnd];
  // we do things to the parent widget that are really only allowed
  // on toplevels. This check is at least friendly.
  if (!([parent isKindOfClassNamed: "Frame"]) && ([parent getParent] == 0))
    [WindowCreation raiseEvent: "Warning: ZoomRaster created as child of non toplevel. Resize code probably\nwill not work.\n"];

  logicalWidth = width;
  logicalHeight = height;
  zoomFactor = 1;

  [globalTkInterp eval: "bind %s <Configure> { %s handleConfigureWidth: %s Height: %s }",
		  [parent getWidgetName], [self getObjcName], "%w", "%h"];
  return self;
}


-(int) getZoomFactor {
  return zoomFactor;
}

// the REDRAWONZOOM code handles redrawing the window when zooming.
// Unfortunately, it's a bit buggy and only works well when there are no
// pixmaps drawn to the Raster, just squares.
-setZoomFactor: (int) z {
  int oldZoom;
#ifdef REDRAWONZOOM
  XImage *oldImage;
  XGCValues oldgcv;
  int x, y;
#endif

  // save necessary state: old image, old zoom.
  oldZoom = zoomFactor;
#ifdef REDRAWONZOOM
  oldImage = XGetImage(display, pm, 0, 0, width, height, AllPlanes, XYPixmap);
#endif
  
  // zoom ourselves
  zoomFactor = z;
  [self setWidth: logicalWidth Height: logicalHeight];

#ifdef REDRAWONZOOM
  // now build a new image from the data in the old one.
  // I hope this use of oldPixel is portable: why is image support so lousy?
  XGetGCValues(display, gc, GCForeground, &oldgcv);   // save old colour
  for (x = 0; x < logicalWidth; x++)
    for (y = 0; y < logicalHeight; y++) {
      unsigned long oldPixel;
      oldPixel = XGetPixel(oldImage, x*oldZoom, y*oldZoom);
      XSetForeground(display, gc, oldPixel);
      XFillRectangle(display, pm, gc, x*zoomFactor, y*zoomFactor,
		     zoomFactor, zoomFactor);
    }
  XChangeGC(display, gc, GCForeground, &oldgcv);  // now restore colour
  
  XDestroyImage(oldImage);
#endif
  
  return self;
}

// handler for tk4.0 <Configure> events - width and height is passed to us.
// note that they are passed to us in absolute values, not gridded.
-handleConfigureWidth: (int) newWidth Height: (int) newHeight {
  int newZoom;
  newZoom = newWidth / logicalWidth;
  if (newZoom != newHeight / logicalHeight)
    [WindowUsage raiseEvent: "nonsquare zoom given.\n"];

#ifdef DEBUG
  printf("Handling configure for %s\noldZoom: %d newZoom: %d, newWidth = %d newHeight = %d\n",
	 [self getObjcName], zoomFactor, newZoom, newWidth, newHeight);
#endif
  
  // this check isn't just an optimization, it prevents an infinite
  // recursion: [self setZoomFactor] reconfigures the widget, which in turn
  // generates a configure event.
  if (newZoom != zoomFactor)
    [self setZoomFactor: newZoom];
  return self;
}
  
// override setWidth to set it for them according to zoom factor.
-setWidth: (int) newWidth Height: (int) newHeight {
  logicalWidth = newWidth;
  logicalHeight = newHeight;

  [super setWidth: newWidth*zoomFactor Height:newHeight*zoomFactor];

  // set up gridded geometry so this is resizeable. Only works if
  // the parent is a toplevel.
  [globalTkInterp eval: "wm grid %s %d %d %d %d; wm aspect %s 1 1 1 1",
		  [parent getWidgetName], logicalWidth, logicalHeight, 
		  zoomFactor, zoomFactor, [parent getWidgetName]];

  return self;
}

// drawing is just like before, only magnified.
-drawPointX: (int) x Y: (int) y Color: (Color) c {
  [super fillRectangleX0: x * zoomFactor Y0: y * zoomFactor
         X1: (x+1) * zoomFactor Y1: (y+1) * zoomFactor
	 Color: c];

  return self;
}

-fillRectangleX0: (int) x0 Y0: (int) y0 X1: (int) x1 Y1: (int) y1 Color: (Color) c{
  [super fillRectangleX0: x0 * zoomFactor Y0: y0 * zoomFactor
         X1: (x1) * zoomFactor Y1: (y1) * zoomFactor
	 Color: c];

  return self;
}

-draw: (id <XDrawer>) xd X: (int) x Y: (int) y {
  return [super draw: xd X: x * zoomFactor Y: y * zoomFactor];
}

-increaseZoom {
  return [self setZoomFactor: zoomFactor + 1];
}

-decreaseZoom {
  if (zoomFactor > 1)
    return [self setZoomFactor: zoomFactor - 1];
  else
    return self;
}

// scale by zoom factor
-handleButton: (int) n X: (int) x Y: (int) y {
  return [super handleButton: n X: x / zoomFactor Y: y / zoomFactor];
}

@end
