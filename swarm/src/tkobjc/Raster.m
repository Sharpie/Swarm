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

#import <tkobjc/global.h>
#import <tkobjc/Raster.h>
#include "internal.h"

@implementation Raster

- createEnd
{
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

  tkwin = tkobjc_nameToWindow (widgetName);
  if (tkwin == NULL)
    {
      [WindowCreation raiseEvent: "Error creating tkwin!\n%s",
                      [globalTkInterp result]];
      return nil;
    }

  Tk_MakeWindowExist (tkwin);
  
  // now arrange for expose events to call our redraw procedure.
  [globalTkInterp eval: "bind %s <Expose> {%s drawSelf}",
		  widgetName, [self getObjcName]];
  // and our own callback for button clicks
  [globalTkInterp eval: 
    "bind %s <ButtonPress> { %s handleButton: %s X: %s Y: %s }",
		  widgetName, [self getObjcName], "%b", "%x", "%y"];
  
  tkobjc_raster_createContext (self);
  tkobjc_raster_createPixmap (self);
  [self erase];
  
  return self;
}

- (id <Colormap>)getColormap
{
  return colormap;
}

// This widget won't work without this initialized.
- setColormap: (id <Colormap>)c
{
  colormap = c;
  map = [colormap map];				  // cache this, fast access.
  tkobjc_raster_setBackground (self, [colormap black]);
  tkobjc_raster_setColormap (self);
  return self;
}

// set the width for the widget. We maintain our own values of this, to
// avoid problems with Tk's caching.
// For pixmap handling, we create a new pixmap, erase it, copy the old
// one to it and redraw ourselves.
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  Pixmap oldpm = pm;
  int oldWidth = width;
  int oldHeight = height;

  width = newWidth;
  height = newHeight;
  tkobjc_raster_createPixmap (self);

  [super setWidth: width Height: height];
  [self erase];

  tkobjc_raster_copy (self, oldpm, oldWidth, oldHeight);

  return [self drawSelf];
}

// new methods

- erase
{
  tkobjc_raster_erase (self);
  return self;
}

// draw a point on the window.
- drawPointX: (int)x Y: (int)y Color: (Color)c
{
  tkobjc_raster_drawPoint (self, x, y, map[c]);
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
  tkobjc_raster_fillRectangle (self, x0, y0, x1 - x0, y1 - y0, c);
  return self;
}

// copy the pixmap onto the X window.
- drawSelf
{
  tkobjc_raster_flush (self);
  return self;
}

// if a client is registered, then the specified selector is called with
// the x and y coordinates of a button press.
- setButton: (int)n Client: c Message: (SEL)sel
{
  switch (n)
    {
    case ButtonLeft: button1Client = c; button1Sel = sel; break;
    case ButtonMiddle: button2Client = c; button2Sel = sel; break;
    case ButtonRight: button3Client = c; button3Sel = sel; break;
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
    case ButtonLeft: c = button1Client; sel = button1Sel; break;
    case ButtonMiddle: c = button2Client; sel = button2Sel; break;
    case ButtonRight: c = button3Client; sel = button3Sel; break;
    default: raiseEvent(SourceMessage, "Button %d out of range.\n");
    }
  
  if (c && sel)
    [c perform: sel with: (void *)x with: (void *)y];
  return self;
}

@end
