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

#import <tk.h>
#import <X11/Xlib.h>
#import <defobj/Create.h>
#import <gui.h>

#define MAXCOLORS 256

typedef unsigned long PixelValue;
typedef unsigned char Color;

@interface XColormap: CreateDrop <Colormap>
{
  // state for the Object
  PixelValue white, black;
  PixelValue map[MAXCOLORS];
  BOOL isSet[MAXCOLORS];
  
  // data necessary for Xlib.
  Tk_Window tkwin;
  Display *display;
  Window xwin;
  Colormap cmap;
}

- (GUI_PixelValue *)map;
- (GUI_PixelValue)pixelValue: (GUI_Color)c;
- (BOOL)setColor: (GUI_Color)c ToName: (const char *)colorName;
- (BOOL)setColor: (GUI_Color)c ToGrey: (double)g;
- (BOOL)setColor: (GUI_Color)c ToRed: (double)r Green: (double)g Blue: (double)b;
- (GUI_PixelValue)white;
- (GUI_PixelValue)black;
- (BOOL)colorIsSet: (GUI_Color)c;

@end
