// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Loop through a Discrete2d, sending the displayMessage message to
// all objects found in there.  One argument is passed on the message,
// the display widget.

#import <space/Object2dDisplay.h>
#import <gui.h> // GUI_BEEP
#import <simtoolsgui.h> // CREATE_PROBE_DISPLAY

//S: Object2dDisplay displays 2d arrays of objects.
//D: Object2dDisplay helps display 2d arrays of objects. 
//D: Create a Object2dDisplay, give it a Raster widget to draw on,
//D: a Discrete2d, a message to call on each object, and (optionally)
//D: a collection of objects and it will dispatch the message to all
//D: objects with the Raster widget as an argument. In addition,
//D: Object2dDisplay can help you make probees. 

@implementation Object2dDisplay

- createEnd
{
  [super createEnd];
  if (displayWidget == nil || discrete2d == nil || displayMessage == (SEL)nil)
    [InvalidCombination raiseEvent: "Object display improperly initialized\n"];
  return self;
}

//M: Set the display widget to use for drawing.
- setDisplayWidget: (id <Raster>)r
{
  displayWidget = r;
  return self;
}

//M: Set the 2d array to draw.
- setDiscrete2dToDisplay: (Discrete2d *)c
{
  discrete2d = c;
  return self;
}

//M: Set the message to be sent to each object in the grid to make it
//M: draw itself. 
- setDisplayMessage: (SEL)s
{
  displayMessage = s;
  return self;
}

// An optional collection of objects to display. If you give us one, then
// on display we'll just forEach through the objects. Otherwise we have to
// scan the whole array.
//M: Set a collection of objects to be displayed. 
//M: If this is not given, then Object2dDisplay loops through the 2d
//M: grid sending draw messages to all objects it finds there. 
//M: Giving an explicit collection of objects to draw is more efficient
//M: if your grid is sparsely populated. 
- setObjectCollection: objects
{
  objectCollection = objects;
  return self;
}

//M: Draw all objects in the array (or optionally, the collection)
//M: on the raster widget. All that happens here is the display message
//M: is sent to each object - it is the object's responsibility to
//M: render itself. 
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
- makeProbeAtX: (int)x Y: (int)y
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
