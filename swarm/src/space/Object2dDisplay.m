// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Loop through a Discrete2d, sending the displayMessage message to
// all objects found in there. One argument is passed on the message,
// the display widget.

#import <space/Object2dDisplay.h>
#import <simtools.h>

#import <tkobjc/control.h>

@implementation Object2dDisplay

- createEnd
{
  [super createEnd];
  if (displayWidget == nil || discrete2d == nil || displayMessage == (SEL) nil)
    [InvalidCombination raiseEvent: "Object display improperly initialized\n"];
  return self;
}

- setDisplayWidget: (Raster *)r
{
  displayWidget = r;
  return self;
}

- setDiscrete2dToDisplay: (Discrete2d *)c
{
  discrete2d = c;
  return self;
}

- setDisplayMessage: (SEL)s
{
  displayMessage = s;
  return self;
}

// an optional collection of objects to display. If you give us one, then
// on display we'll just forEach through the objects. Otherwise we have to
// scan the whole array.
- setObjectCollection: objects
{
  objectCollection = objects;
  return self;
}

- display
{
  int x, y;
  id *lattice;
  long *offsets;
  int xsize, ysize;

  lattice = [discrete2d getLattice];
  offsets = [discrete2d getOffsets];
  xsize = [discrete2d getSizeX];
  ysize = [discrete2d getSizeY];

  // if we have a collection to display, just use that. Otherwise scan
  // the entire 2d grid.
  if (objectCollection)
    [objectCollection forEach: displayMessage : displayWidget];
  else
    {
      for (y = 0; y < ysize; y++)
        for (x = 0; x < xsize; x++) {
          id potentialObject;
          potentialObject = *discrete2dSiteAt(lattice, offsets, x, y);
          if (potentialObject)
            [potentialObject perform: displayMessage with: displayWidget];
        }
    }
  
  return self;
}

// code to make a probe for an object at a specific point. This is
// good to make as a button client for Raster widgets
- makeProbeAtX: (int)x Y: (int)y
{
  id obj;
  
  if (x >= 0 && x < [discrete2d getSizeX]
      && y >= 0 && y < [discrete2d getSizeY])
    {
      obj = [discrete2d getObjectAtX: x Y: y];
      if (obj)
        CREATE_PROBE_DISPLAY (obj);
      else
        tkobjc_ringBell();
    }
    else
      [WarningMessage
        raiseEvent: 
          "Object2dDisplay: invalid coordinates to make probe (%d,%d)\n",
        x, y];
  return self;
}


@end
