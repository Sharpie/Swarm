// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// protocol for a "Drawer", something that draws itself with
// calls on a Drawable at a given location. Raster calls this method
// on objects it is given by the draw:X:Y: method.
// Note that the GC is given to you just for convenience: you should
// not permanently alter it, as other code might be using that same GC.
typedef void * Drawable;
typedef void * GC;

@protocol Drawer
-drawOn: (Drawable) w X: (int) x Y: (int) y GC: (GC) gc Caller: Caller;
@end
