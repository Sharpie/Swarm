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

// basic Colormap object for X windows programming.
// used for now only in Raster, may come in handy later for other
// Xlib programming. It'd be great if we could lever private colormaps
// into this object.

// Terminology:
//   Color - a number in [0,MAXCOLORS]. The swarm programmer uses these.
//   PixelValue - what value Xlib expects. Colors are mapped to this.
// main Swarm programmer interface:
//   setColor:To: declare that Color c looks like the provide colorname.

#include "internal.h"
#import <defobj/Create.h>
#import <gui.h>

#define MAXCOLORS 256

@interface Colormap: CreateDrop <Colormap>
{
  PixelValue white, black;
@public
  PixelValue map[MAXCOLORS];
  BOOL isSet[MAXCOLORS];
  X11Colormap cmap;
  Tk_Window tkwin;
}

- (PixelValue *)map;
- (PixelValue)pixelValue: (Color)c;
- (BOOL)setColor: (Color)c ToName: (const char *)colorName;
- (BOOL)setColor: (Color)c ToGrey: (double)g;
- (BOOL)setColor: (Color)c ToRed: (double)r Green: (double)g Blue: (double)b;
- (PixelValue)white;
- (PixelValue)black;
- (BOOL)colorIsSet: (Color)c;
- (Color)nextFreeColor;
- (void)unsetColor: (Color)c;
@end
