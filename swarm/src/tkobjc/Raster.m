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
#include <swarmconfig.h> // PTRINT

@implementation Raster

PHASE(Creating)

+ createBegin: aZone
{
  Raster *obj = [super createBegin: aZone];
  
  obj->width = 100U;
  obj->height = 100U;
  obj->colormap = nil;
  obj->eraseColor = -1;
  
  return obj;
}

- createEnd
{
  // initialize the superclass.
  [super createEnd];

  // BMcM - 12-FEB-97: force widget to have *no* background;
  //   fixes problem with newer tcl/tk (7.5/4.1 up?) whereby raster
  //   was being erased, *after* our -drawself, on all <Expose>
  //   events...
  [globalTkInterp eval: "frame %s -background {} -width %u -height %u",
  	  widgetName, width, height];

  tkobjc_raster_create (self);
  
  // now arrange for expose events to call our redraw procedure.
  [globalTkInterp eval: "bind %s <Expose> {%s drawSelf}",
		  widgetName, [self getObjectName]];
  // and our own callback for button clicks
  [globalTkInterp eval: 
    "bind %s <ButtonPress> { %s handleButton: %s X: %s Y: %s }",
		  widgetName, [self getObjectName], "%b", "%x", "%y"];
  
  tkobjc_raster_createContext (self);
  tkobjc_raster_createPixmap (self);

  [self updateSize];
  return self;
}

PHASE(Using)

// This widget won't work without this initialized.
- setColormap: (id <Colormap>)c
{
  colormap = c;
  map = [colormap map];				  // cache this, fast access.
  tkobjc_raster_setColormap (self);

  return self;
}

- (id <Colormap>)getColormap
{
  return colormap;
}

- setWidth: (unsigned)newWidth
{
  return [self setWidth: newWidth Height: height];
}

- setHeight: (unsigned)newHeight
{
  return [self setWidth: width Height: newHeight];
}

// Set the width for the widget. We maintain our own values of this, to
// avoid problems with Tk's caching.
// For pixmap handling, we create a new pixmap, erase it, copy the old
// one to it and redraw ourselves.
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight
{
  int oldWidth = (newWidth < width) ? newWidth : width;
  int oldHeight = (newHeight < height) ? newHeight : height;

  // This hack is for Windows.  Without it Configure events of the
  // last known size are sent after the new size has been set.
  if (newWidth < width || newHeight < height)
    {
      Tk_Window tkwin =
	tkobjc_nameToWindow ([[self getTopLevel] getWidgetName]);

      Tk_GeometryRequest (tkwin, newWidth, newHeight);
    }

  tkobjc_raster_savePixmap (self);
  width = newWidth;
  height = newHeight;
  [super setWidth: width Height: height];
  tkobjc_raster_createPixmap (self);
  [self erase];
  tkobjc_raster_copy (self, oldWidth, oldHeight);
  tkobjc_raster_dropOldPixmap (self);

  [self drawSelf];

  return self;
}

// new methods

- (void)erase
{
  tkobjc_raster_erase (self);
}

// draw a point on the window.
- (void)drawPointX: (int)x Y: (int)y Color: (Color)c
{
  tkobjc_raster_drawPoint (self, x, y, c);
}

// draw an arbitrary object: we just call the "drawOn" method in
// the object we're given.
- (void)draw: (id <Drawer>)drawer X: (int)x Y: (int)y
{
  [drawer drawX: x Y: y];
}

// draw a rectangle.
- (void)fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)c
{
  tkobjc_raster_fillRectangle (self, x0, y0, x1 - x0, y1 - y0, c);
}

- (void)ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
            Width: (unsigned)penWidth Color: (Color)c
{
  tkobjc_raster_ellipse (self, x0, y0, x1 - x0, y1 - y0, penWidth, c);
}

- (void)lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
         Width: (unsigned)penWidth Color: (Color)c
{
  tkobjc_raster_line (self, x0, y0, x1, y1, penWidth, c);
}

- (void)rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
              Width: (unsigned)penWidth Color: (Color)c
{
  tkobjc_raster_rectangle (self, x0, y0, x1 - x0, y1 - y0, penWidth, c);
}

// copy the pixmap onto the X window.
- (void)drawSelf
{
  tkobjc_raster_flush (self);
}

// if a client is registered, then the specified selector is called with
// the x and y coordinates of a button press.
- (void)setButton: (int)n Client: c Message: (SEL)sel
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
}

- (void)handleButton: (int)n X: (int)x Y: (int)y
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
    [c perform: sel with: (void *) (PTRINT) x with: (void *) (PTRINT) y];
}

@end


