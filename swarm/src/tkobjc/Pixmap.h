// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include "internal.h"
#import <defobj/Create.h>
#import <gui.h>

@interface Pixmap: CreateDrop <Pixmap, Drawer>
{
  id <Raster> raster;
  const char *directory;
  const char *filename;
  id <Widget> widget;
  BOOL decorationsFlag;

  @public
#ifndef _WIN32
  Display *display;
  X11Pixmap pixmap;				  // the map
  X11Pixmap mask;				  // clipping mask
  XpmImage xpmimage;
  XpmAttributes xpmattrs;
#else
  void *pixmap;
#endif
  unsigned palette_size;
  png_colorp palette;

  unsigned width, height;
}

+ createBegin: aZone;
- setDirectory: (const char *)directory;
- setFile: (const char *)filename;
- setWidget: (id <Widget>)widget;
- setDecorationsFlag: (BOOL)decorationsFlag;
- createEnd;

- setRaster: raster;
- (unsigned)getWidth;
- (unsigned)getHeight;
- drawX: (int)x Y: (int)y;
- save: (const char *)filename;
@end
