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

#import "Value2dDisplay.h"
#import <space.h>
#ifndef SWARM_OSX
#import <gui.h>
#else
#import <Cocoa/Cocoa.h>
#endif
#import <defobj.h> // raiseEvent

// This should be subclassed to fill in the colormap for your CA.
// for now, we expect your colormap to be allocated already.

@implementation Value2dDisplay

PHASE(Creating)

#ifndef SWARM_OSX
+ create: aZone setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c setDiscrete2dToDisplay: (id <GridData>)d
#else
+ create: aZone setDisplayWidget: (id)r colormap: (id)c setDiscrete2dToDisplay: (id <GridData>)d
#endif
{
  Value2dDisplay *obj = [self createBegin: aZone];

  [obj setDisplayWidget: r colormap: c];
  [obj setDiscrete2dToDisplay: d];

  return [obj createEnd];
}

#ifndef SWARM_OSX
- setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c
#else
- setDisplayWidget: (id)r colormap: (id)c
#endif
{
  displayWidget = r;
  colormap = c;
  
  drawPointImp = [(Object *)r methodFor: @selector (drawPointX:Y:Color:)];
  return self;
}

- setDiscrete2dToDisplay: (id <GridData>)c
{
  discrete2d = c;
  return self;
}

- createEnd
{
  [super createEnd];

#if 0
  if (displayWidget == nil || discrete2d == nil)
    raiseEvent (InvalidCombination, "Value display improperly initialized\n");
#endif
  
  if (modFactor == 0)
    modFactor = 1;

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
     
// linear transform between values and colours. Good enough?
- setDisplayMappingM: (int)m C: (int)c
{
  modFactor = m;
  colorConstant = c;
  return self;
}

#if 0
- displayX:(int)xPos Y:(int)yPos inRect:(NSRect)aRect;
{
  long color;
  float redValue;
  NSColor *aColor;
  id *lattice;
  long *offsets;

#if 0
  lattice = [discrete2d getLattice];
  offsets = [discrete2d getOffsets];
  color = (long) *(discrete2dSiteAt(lattice, offsets, xPos, yPos));
#else
  color = [discrete2d getValueAtX: xPos Y: yPos];
#endif
  redValue = (float)color / (float)modFactor;
  aColor = [NSColor colorWithDeviceRed: redValue green: 0.0
		    blue: 0.0 alpha: 1.0];
  [aColor set];
  NSRectFill(aRect);

  return self;
}
#endif

#if 0
- (void)displayOn: (NSImage *)anImage
{
  int x, y;
  id *lattice;
  long *offsets;
  int xsize, ysize;

  lattice = [discrete2d getLattice];
  offsets = [discrete2d getOffsets];
  xsize = [discrete2d getSizeX];
  ysize = [discrete2d getSizeY];

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      {
	long color;
	float redValue;
	NSColor *aColor;

        color = (long) *(discrete2dSiteAt(lattice, offsets, x, y));
        redValue = (float)color / (float)modFactor;
	aColor = [NSColor colorWithDeviceRed: redValue green: 0.0
			  blue: 0.0 alpha: 1.0];
	[aColor set];
        NSRect aRect = NSMakeRect(3*x, 3*y, 3, 3);
        NSRectFill(aRect);

#if 0
        long color;

        color = (long) *(discrete2dSiteAt(lattice, offsets, x, y));
        color = color / modFactor + colorConstant;
        if (color < 0 || color > 255)
          raiseEvent (WarningMessage,
                      "Value2dDisplay: found colour %d not in [0,255].\n",
                      color);
        
        if (drawPointImp)
          // cache method lookup.
          (void) *drawPointImp (displayWidget,
                                @selector (drawPointX:Y:Color:),
                                x, y, color);
        else
          [displayWidget drawPointX: x Y: y Color: (unsigned char)color];
#endif
      }
}
#endif

#if 0
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

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      {
        long color;

        color = (long) *(discrete2dSiteAt(lattice, offsets, x, y));
        color = color / modFactor + colorConstant;
        if (color < 0 || color > 255)
          raiseEvent (WarningMessage,
                      "Value2dDisplay: found colour %d not in [0,255].\n",
                      color);
        
        if (drawPointImp)
          // cache method lookup.
          (void) *drawPointImp (displayWidget,
                                @selector (drawPointX:Y:Color:),
                                x, y, color);
        else
          [displayWidget drawPointX: x Y: y Color: (unsigned char)color];
      }
  return self;
}
#endif

- (void)update
{
  if (!image) [self createImage];

  if (image) {
    NSAutoreleasePool *pool = [NSAutoreleasePool new];
    NSSize aSize = [image size];
    printf("Value2dDisplay display: %f %f\n", aSize.width, aSize.height);
    NSRect aRect = NSMakeRect(0, 0, aSize.width, aSize.height);
  
    [image lockFocus];
    
    [[NSColor clearColor] set];
    NSRectFill(aRect);

    int x, y;
    // get max value
    modFactor = 0;
    for (x = 0; x < aSize.width; ++x)
      for (y = 0; y < aSize.height; ++y) {
        float color = (float)[discrete2d getValueAtX: x Y: y];
        if (color > modFactor) modFactor = color;
      }

    for (x = 0; x < aSize.width; ++x)
      for (y = 0; y < aSize.height; ++y)
	    {
        aRect = NSMakeRect(x, y, 1, 1);
        long color = [discrete2d getValueAtX: x Y: y];
        float redValue = (float)color / (float)modFactor;
        NSColor *aColor = [NSColor colorWithDeviceRed: redValue green: 0.0
          blue: 0.0 alpha: 1.0];
        [aColor set];
        NSRectFill(aRect);
	    }
    [image unlockFocus];
    [pool release];
  }
}

@end



