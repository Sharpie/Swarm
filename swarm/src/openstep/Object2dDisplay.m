// Swarm library. Copyright � 1996-2000 Swarm Development Group.
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

#import "Object2dDisplay.h"
#import <space/Discrete2d.h> // discrete2dSiteAt
#ifndef SWARM_OSX
#import <gui.h> // GUI_BEEP
#import <simtoolsgui.h> // CREATE_PROBE_DISPLAY
#endif
#import <defobj.h> // ProtocolViolation

@implementation Object2dDisplay

PHASE(Creating)

#ifndef SWARM_OSX
+ create: aZone setDisplayWidget: (id <Raster>)r setDiscrete2dToDisplay: (id <GridData>)c setDisplayMessage: (SEL)s
#else
+ create: aZone setDisplayWidget: (id)r setDiscrete2dToDisplay: (id <GridData>)c setDisplayMessage: (SEL)s
#endif
{
  Object2dDisplay *obj = [self createBegin: aZone];

  [obj setDisplayWidget: r];
  [obj setDiscrete2dToDisplay: c];
  [obj setDisplayMessage: s];
    
  return [obj createEnd];
}

#ifndef SWARM_OSX
- setDisplayWidget: (id <Raster>)r
#else
- setDisplayWidget: (id)r
#endif
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

  if (displayInvocation)
    [displayInvocation release];

  displayInvocation = [[NSInvocation alloc] initWithSelector: displayMessage];
  [displayInvocation setSelector: displayMessage];

  return self;
}

- createEnd
{
  [super createEnd];
#ifndef SWARM_OSX
  if (displayWidget == nil || discrete2d == nil || displayMessage == (SEL) nil)
    raiseEvent (InvalidCombination, "Object display improperly initialized\n");
#endif

  return self;
}

PHASE(Using)

- (NSImage *)image
{
    return image;
}

- (void)releaseImage
{
  if (image)
    {
      [image release];
      image = nil;
    }
  if (imageRep)
    {
      [imageRep release];
      imageRep = nil;
    }
}

- (void)createImage
{
  //NSRect aRect = [self bounds];

  [self releaseImage];

	float worldXsize = (float)[discrete2d getSizeX];
	float worldYsize = (float)[discrete2d getSizeY];
	NSRect aRect = NSMakeRect(0, 0, worldXsize, worldYsize);

#if 1
  image = [[NSImage alloc] initWithSize:aRect.size];
  imageRep = [[NSBitmapImageRep alloc]
	  initWithBitmapDataPlanes:NULL
	  pixelsWide:aRect.size.width
	  pixelsHigh:aRect.size.height
	  bitsPerSample:8
	  samplesPerPixel:3
	  hasAlpha:NO
	  isPlanar:NO
	  colorSpaceName:NSDeviceRGBColorSpace
	  bytesPerRow:aRect.size.width*3
	  bitsPerPixel:24];
  [image addRepresentation:imageRep];
#endif
}

- (id <GridData>)discrete2d
{
  return discrete2d;
}

// An optional collection of objects to display. If you give us one, then
// on display we'll just forEach through the objects. Otherwise we have to
// scan the whole array.
- setObjectCollection: objects
{
  objectCollection = objects;

  return self;
}

- (void)displayOn: (NSImage *)anImage
{
  if (objectCollection)
    [objectCollection forEach: displayMessage : anImage];
}

- displayX:(int)xPos Y:(int)yPos inRect:(NSRect)aRect;
{
  id obj = [discrete2d getObjectAtX: xPos Y: yPos];
  if (obj)
    {
      if (displayMessage) {
#if 0
	NSInvocation *i = [[NSInvocation alloc] initWithSelector: displayMessage];
	[i setSelector: displayMessage];
#endif
	[displayInvocation setArgument: &aRect atIndex: 2];
	[displayInvocation invokeWithTarget: obj];
      } else
	[obj drawRect: aRect];
    }
  else
    return nil;

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
#ifndef SWARM_OSX
      if (obj)
        CREATE_PROBE_DISPLAY (obj);
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

- (void)update
{
  if (!image) [self createImage];

  if (image) {
    NSSize aSize = [image size];
    //printf("Object2dDisplay display: %f %f\n", aSize.width, aSize.height);
    NSRect aRect = NSMakeRect(0, 0, aSize.width, aSize.height);
  
    [image lockFocus];
    
    [[NSColor clearColor] set];
    NSRectFill(aRect);

    int x, y;
    for (x = 0; x < aSize.width; ++x)
      for (y = 0; y < aSize.height; ++y)
	    {
        aRect = NSMakeRect(x, y, 1, 1);
        id obj = [discrete2d getObjectAtX: x Y: y];
        if (obj) {
          if (displayMessage) {
            [displayInvocation setArgument: &aRect atIndex: 2];
            [displayInvocation invokeWithTarget: obj];
          } else
            [obj drawRect: aRect];
        }
	    }
    [image unlockFocus];
  }
}

@end
