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

+ create: aZone setDisplayWidget: (id <Raster>)r setDiscrete2dToDisplay: (id <GridData>)c setDisplayMessage: (SEL)s
{
  Object2dDisplay *obj = [self createBegin: aZone];

  [obj setDisplayWidget: r];
  [obj setDiscrete2dToDisplay: c];
  [obj setDisplayMessage: s];
    
  return [obj createEnd];
}

- setDisplayWidget: (id <Raster>)r
{
  displayWidget = r;

  return self;
}

- setDiscrete2dToDisplay: (id <GridData>)c
{
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
  if (displayWidget == nil || discrete2d == nil || displayMessage == (SEL) nil)
    raiseEvent (InvalidCombination, "Object display improperly initialized\n");

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
#ifndef GNUSTEP
      else
        GUI_BEEP ();
#endif
    }
  else
    raiseEvent (WarningMessage, 
                "Object2dDisplay: invalid coordinates to make probe (%d,%d)\n",
                x, y);
  return self;
}


@end
