// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Our very own magic raster drawing widget. The idea here is to have
// as *little* interaction with Tcl when drawing as possible, and yet
// still get all the other Tk benefits: management, events, etc.
// To that end, a "raster widget" is really just a frame. Nothing
// else, at least from Tcl's point of view. But this C code here goes
// in and gets the X Window ID and scribbles on it. We bind to <Expose>
// to get most of the work done.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tclObjc.h>
#import <TkInterp.h>
#import <tkobjc/Raster.h>

@implementation Raster

- createEnd
{
  XGCValues gcv;

  // initialize the superclass.
  [super createEnd];

  // create a frame widget, set its height
  width = 100U; height = 100U;
  // BMcM - 12-FEB-97: force widget to have *no* background;
  //   fixes problem with newer tcl/tk (7.5/4.1 up?) whereby raster
  //   was being erased, *after* our -drawself, on all <Expose>
  //   events...
  [globalTkInterp eval: "frame %s -background \"\" -width %u -height %u",
  	  widgetName, width, height];

  tkwin = Tk_NameToWindow ([globalTkInterp interp],
                           (char *)widgetName,
                           [globalTkInterp mainWindow]);
  if (tkwin == NULL)
    {
      [WindowCreation raiseEvent: "Error creating tkin!\n%s",
                      [globalTkInterp result]];
      return nil;
    }
  
  Tk_MakeWindowExist(tkwin);
  
  display = Tk_Display (tkwin);
  xwin = Tk_WindowId (tkwin);
  
  // now arrange for expose events to call our redraw procedure.
  [globalTkInterp eval: "bind %s <Expose> {%s drawSelf}",
		  widgetName, [self getObjcName]];
  // and our own callback for button clicks
  [globalTkInterp eval: 
    "bind %s <ButtonPress> { %s handleButton: %s X: %s Y: %s }",
		  widgetName, [self getObjcName], "%b", "%x", "%y"];

  // now create a GC
  gcv.background = BlackPixel(display, DefaultScreen(display));
  gcv.graphics_exposures = False;
  gc = XCreateGC (display, xwin, GCBackground | GCGraphicsExposures, &gcv);
  XSetFillStyle (display, gc, FillSolid);
  XSetClipOrigin (display, gc, 0, 0);

  // and create a local Pixmap for drawing
  pm = XCreatePixmap (display, xwin, width, height, Tk_Depth(tkwin));
  [self erase];
  
  return self;
}

- (Display *)getDisplay
{
  return display;
}

- (id <Colormap>)getColormap
{
  return colormap;
}

// This widget won't work without this initialized.
- setColormap: (id <Colormap>)c
{
  XGCValues gcv;

  colormap = c;
  map = [colormap map];				  // cache this, fast access.
  gcv.background = [colormap black];
  XChangeGC (display, gc, GCBackground, &gcv);
  return self;
}

// set the width for the widget. We maintain our own values of this, to
// avoid problems with Tk's caching.
// For pixmap handling, we create a new pixmap, erase it, copy the old
// one to it and redraw ourselves.
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  unsigned minWidth, minHeight;
  Pixmap oldpm;

  oldpm = pm;
  minWidth = (newWidth < width ? newWidth : width);
  minHeight = (newHeight < height ? newHeight : height);
  
  pm = XCreatePixmap (display, xwin, newWidth, newHeight, Tk_Depth (tkwin));
  width = newWidth;
  height = newHeight;
  
  [super setWidth: width Height: height];

  [self erase];
  XCopyArea (display, oldpm, pm, gc, 0, 0, minWidth, minHeight, 0, 0);
  XFreePixmap (display, oldpm);
  return [self drawSelf];
}

// new methods

// erase the pixmap - can't use XClearArea, sadly.
- erase
{
  XGCValues oldgcv;
  XGetGCValues (display, gc, GCForeground, &oldgcv);   // save old colour

  XSetForeground (display, gc, BlackPixel(display, DefaultScreen(display)));
  XFillRectangle (display, pm, gc, 0, 0, width, height);

  XChangeGC (display, gc, GCForeground, &oldgcv);  // now restore colour
  return self;
}

// draw a point on the window.
- drawPointX: (int) x Y: (int) y Color: (Color) c
{
  XSetForeground (display, gc, map[c]);		  // no checking on map.

  XDrawPoint (display, pm, gc, x, y);
  return self;
}

// draw an arbitrary object: we just call the "drawOn" method in
// the object we're given.
- draw: (id <XDrawer>)xd X: (int)x Y: (int)y
{
  [xd drawOn: pm X: x Y: y GC: gc Caller: self];
  return self;
}

// draw a rectangle.
- fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c
{
  XSetForeground (display, gc, map[c]);		  // no checking on map.
  XFillRectangle (display, pm, gc, x0, y0, x1-x0, y1-y0);
  return self;
}

// copy the pixmap onto the X window.
- drawSelf
{
#ifdef DEBUG
  printf("Redrawing %s\nPixmap: %x Window: %x Width: %d Height: %d\n",
	 [self getObjcName], pm, xwin, width, height);
#endif
  XCopyArea (display, pm, xwin, gc, 0, 0, width, height, 0, 0);
  XFlush (display);
  return self;
}

// if a client is registered, then the specified selector is called with
// the x and y coordinates of a button press.
- setButton: (int)n Client: c Message: (SEL)sel
{
  switch (n)
    {
    case 1: button1Client = c; button1Sel = sel; break;
    case 2: button2Client = c; button2Sel = sel; break;
    case 3: button3Client = c; button3Sel = sel; break;
    default:
      raiseEvent (WarningMessage,
                  "Don't know how to handle button %d, ignoring.\n", n);
    }
  
  return self;
}

- handleButton: (int)n X: (int)x Y: (int)y
{
  id c = 0;
  SEL sel = 0;
  switch (n)
    {
    case 1: c = button1Client; sel = button1Sel; break;
    case 2: c = button2Client; sel = button2Sel; break;
    case 3: c = button3Client; sel = button3Sel; break;
    default: raiseEvent(SourceMessage, "Button %d out of range.\n");
    }
  
  if (c && sel)
    [c perform: sel with: (void *)x with: (void *)y];
  return self;
}

@end
