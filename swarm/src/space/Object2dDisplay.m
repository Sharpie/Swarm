// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Loop through a Discrete2d, sending the displayMessage message to
// all objects found in there.  One argument is passed on the message,
// the display widget.

#import <space/Object2dDisplay.h>
#import <space/Discrete2d.h> // discrete2dSiteAt
#import <gui.h> // GUI_BEEP
#import <simtoolsgui.h> // CREATE_PROBE_DISPLAY
#import <defobj.h> // ProtocolViolation

@implementation Object2dDisplay

PHASE(Creating)

- setDisplayWidget: (id <Raster>)r
{
  displayWidget = r;

  return self;
}

- setDiscrete2dToDisplay: c
{
  if (![c conformsTo: @protocol (_Discrete2d)])
    [ProtocolViolation
      raiseEvent:
        "Argument `%s' to Object2dDisplay setDiscrete2dDisplay: does not\n"
      "conform to Discrete2d protocol\n",
      [c name]];
  
  discrete2d = c;

  return self;
}

- setDisplayMessage: (SEL)s
{
  displayMessage = s;

  return self;
}

- createEnd
{
  [super createEnd];
  if (displayWidget == nil || discrete2d == nil || displayMessage == (SEL)nil)
    [InvalidCombination raiseEvent: "Object display improperly initialized\n"];

  return self;
}

PHASE(Using)

// An optional collection of objects to display. If you give us one, then
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
    [objectCollection forEach: displayMessage: displayWidget];
  else
    {
      for (y = 0; y < ysize; y++)
        for (x = 0; x < xsize; x++)
          {
            id potentialObject = *discrete2dSiteAt (lattice, offsets, x, y);

            if (potentialObject)
              [potentialObject perform: displayMessage with: displayWidget];
          }
    }
  
  return self;
}

// code to make a probe for an object at a specific point. This is
// good to make as a button client for Raster widgets
- makeProbeAtX: (unsigned)x Y: (unsigned)y
{
  id obj;
  
  if (x >= 0
      && x < [discrete2d getSizeX]
      && y >= 0
      && y < [discrete2d getSizeY])
    {
      obj = [discrete2d getObjectAtX: x Y: y];
      if (obj)
        CREATE_PROBE_DISPLAY (obj);
      else
        GUI_BEEP ();
    }
    else
      [WarningMessage
        raiseEvent: 
          "Object2dDisplay: invalid coordinates to make probe (%d,%d)\n",
        x, y];
  return self;
}


@end
