// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include "internal.h"
#import <defobj/Create.h>
#import <gui.h>

@interface Pixmap: CreateDrop <_Pixmap, Drawer>
{
  const char *filename;
  id <Raster> raster;
  @public
#ifndef _WIN32
  X11Pixmap pixmap;				  // the map
  X11Pixmap mask;				  // clipping mask
#else
  void *pixmap;
#endif
  unsigned width, height;
}

- setFile: (const char *)filename;
- createEnd;
- (unsigned)getWidth;
- (unsigned)getHeight;
- setRaster: raster;
- drawX: (int)x Y: (int)y;
@end
