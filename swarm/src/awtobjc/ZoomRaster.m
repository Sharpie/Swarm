// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/ZoomRaster.h>
#import <javaobjc/global.h>

@implementation ZoomRaster

- (unsigned)getWidth
{
  return logicalWidth;
}

- (unsigned)getHeight
{
  return logicalHeight;
}

- createEnd
{
  [self setClassIdFromSwarmName: "Raster"];
  [super createEnd];
#if 0
  // We do things to the parent widget that are really only allowed
  // on toplevels. This check is at least friendly.
  if (!([parent isKindOfClassNamed: "Frame"]) && ([parent getParent] == 0))
    [WindowCreation 
      raiseEvent: 
        "Warning: ZoomRaster created as child of non toplevel.\n"
      "Resize code probably\nwill not work.\n"];
#endif
  logicalWidth = width;
  logicalHeight = height;
  zoomFactor = 1U;

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

  // save necessary state: old image, old zoom.
  oldZoom = zoomFactor;
  
  // zoom ourselves
  zoomFactor = z;
  [self setWidth: logicalWidth Height: logicalHeight];

  return self;
}

// Handler for tk4.0 <Configure> events - width and height is passed to us.
// note that they are passed to us in absolute values, not gridded.
- handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  unsigned newZoom = newWidth / logicalWidth;

  if (newZoom != newHeight / logicalHeight)
    [WindowUsage raiseEvent: "nonsquare zoom given.\n"];

  // This check isn't just an optimization, it prevents an infinite
  // recursion: [self setZoomFactor] reconfigures the widget, which in turn
  // generates a configure event.
  if (newZoom != zoomFactor)
    [self setZoomFactor: newZoom];
  return self;
}
  
// override setWidth to set it for them according to zoom factor.
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  logicalWidth = newWidth;
  logicalHeight = newHeight;

  [super setWidth: newWidth * zoomFactor Height: newHeight * zoomFactor];

  return self;
}

// drawing is just like before, only magnified.
- drawPointX: (int)x Y: (int)y Color: (Color)c
{
  [super fillRectangleX0: x * zoomFactor Y0: y * zoomFactor
         X1: (x+1) * zoomFactor Y1: (y+1) * zoomFactor
	 Color: c];

  return self;
}

- fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c
{
  [super fillRectangleX0: x0 * zoomFactor Y0: y0 * zoomFactor
         X1: (x1) * zoomFactor Y1: (y1) * zoomFactor
	 Color: c];

  return self;
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

@end
