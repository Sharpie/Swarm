// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include "internal.h"

// protocol for a "Drawer", something that draws itself with Xlib
// calls on a Drawable at a given location. Raster calls this method
// on objects it is given by the draw:X:Y: method.

@protocol Drawer
- drawX: (int)x Y: (int)y raster: (Raster *)raster;
@end
