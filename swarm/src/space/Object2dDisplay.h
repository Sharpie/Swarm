// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc.h>
#import <objectbase.h>
#import <space.h>

// generic object to handle display 2d objects
// hand it a 2d raster widget, tell it what message to send, and it sends it.
// also knows how to construct probes.

@interface Object2dDisplay: SwarmObject {
  Raster * displayWidget;
  Discrete2d * discrete2d;
  SEL displayMessage;
  id objectCollection;
}

-setDisplayWidget: (Raster *) r;
-setDiscrete2dToDisplay: (Discrete2d *) c;
-setDisplayMessage: (SEL) s;
-setObjectCollection: objects;			  // optional collection
-createEnd;
-display;
-makeProbeAtX: (int) x Y: (int) y;
@end
