// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// basic Colormap object for X windows programming.
// used for now only in Raster, may come in handy later for other
// Xlib programming. It'd be great if we could lever private colormaps
// into this object.

// Terminology:
//   Color - a number in [0,MAXCOLORS]. The swarm programmer uses these.
//   PixelValue - what value Xlib expects. Colors are mapped to this.
// main Swarm programmer interface:
//   setColor:To: declare that Color c looks like the provide colorname.

#import "internal.h"
#import <defobj/Create.h>
#import <gui.h>

#define MAXCOLORS 256

@interface Colormap: CreateDrop <Colormap>
{
  // state for the Object
  PixelValue white, black;
  
  // data necessary for Xlib.
  Tk_Window tkwin;
  Display *display;
  Window xwin;
@public
  PixelValue map[MAXCOLORS];
  BOOL isSet[MAXCOLORS];
  X11Colormap cmap;
}

- (PixelValue *)map;
- (PixelValue)pixelValue: (Color)c;
- (BOOL)setColor: (Color)c ToName: (const char *)colorName;
- (BOOL)setColor: (Color)c ToGrey: (double)g;
- (BOOL)setColor: (Color)c ToRed: (double)r Green: (double)g Blue: (double)b;
- (PixelValue)white;
- (PixelValue)black;
- (BOOL)colorIsSet: (Color)c;

@end
