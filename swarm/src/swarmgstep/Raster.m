// Swarm library. Copyright © 2003 Swarm Development Group.
//
// Author: Scott Christley
//
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

#include <AppKit/AppKit.h>
#include "Raster.h"
#include <space/space.h>

@implementation Raster

- initWithFrame:(NSRect)aRect pointSize:(NSSize)aSize
{
  fprintf(stderr, "Raster initWithFrame:pointSize:\n");
  [super initWithFrame:aRect];

  image = nil;
  imageRep = nil;
  rasterOrigin.x = 0;
  rasterOrigin.y = 0;
  pointSize = aSize;
  displayList = [NSMutableArray new];
  backgroundColor = [[NSColor blackColor] retain];

  fprintf(stderr, "done Raster initWithFrame:pointSize:\n");
  return self;
}

- initWithFrame:(NSRect)aRect
{
  // default size of points are 3 by 3, i.e. 1/4 inch square
  return [self initWithFrame:aRect pointSize:NSMakeSize(3, 3)];
}

- (void)determineWindowMinMaxSize
{
#if 1
  // We need to have displays attached
  if ([displayList count] == 0)
    return;

  // If we are in a window and the content view for that window
  // Then set the min and max size of the window.
  // If we are not the content view then there isn't much we can do.
  id w = [self window];
  if (w)
    {
      id v = [w contentView];
      if (v == self)
	{
	  // Just pick first one for now, currently assume same size
	  // grids under each display
	  id aDisplay = [displayList objectAtIndex: 0];
	  id g = [aDisplay discrete2d];
	  unsigned worldXsize = [g getSizeX];
	  unsigned worldYsize = [g getSizeY];
	  NSRect aRect = NSMakeRect(0, 0, worldXsize * pointSize.width,
				    worldYsize * pointSize.height);
	  NSRect r = [NSWindow frameRectForContentRect: aRect
			       styleMask: [w styleMask]];
	  // add 2 just to give a little border
	  r.size.width += 2;
	  r.size.height += 2;
	  [w setMaxSize: r.size];

	  // If the current size is larger, then resize the window
	  aRect = [w frame];
	  if ((aRect.size.width > r.size.width)
	      || (aRect.size.height > r.size.height))
	    [w setFrame: aRect display: NO];
	}
    }
#endif
}

- (void)setRasterOrigin:(NSPoint)aPoint
{
  rasterOrigin = aPoint;

  [self determineWindowMinMaxSize];

  // redisplay
}

- (void)setPointSize:(NSSize)aSize
{
  pointSize = aSize;

  [self determineWindowMinMaxSize];

  // redisplay
}

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
  NSRect aRect = [self bounds];

  [self releaseImage];

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

#if 0
  image = [[NSImage alloc] initWithSize:aRect.size];
  imageRep = [[NSBitmapImageRep alloc]
	  initWithBitmapDataPlanes:NULL
	  pixelsWide:240
	  pixelsHigh:240
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

- (void) resizeWithOldSuperviewSize: (NSSize)oldSize
{
  fprintf(stderr, "RasterView got resized\n");
  [super resizeWithOldSuperviewSize: oldSize];

  [self determineWindowMinMaxSize];

  // release the image if we get resized
  [self releaseImage];
}

- (void)addDisplay: aDisplay
{
  if (aDisplay)
    {
      fprintf(stderr, "addDisplay:\n");
      [displayList addObject: aDisplay];

      [self determineWindowMinMaxSize];
    }
}

- (void)removeDisplay: aDisplay
{
  if (aDisplay)
    {
      fprintf(stderr, "removeDisplay:\n");
      [displayList removeObject: aDisplay];

      [self determineWindowMinMaxSize];
    }
}

- (void)mouseDown:(NSEvent *)theEvent
{
  fprintf(stderr, "mouseDown:\n");
}

- (void)mouseUp:(NSEvent *)theEvent
{
  fprintf(stderr, "mouseUp:\n");
}

- (void)rightMouseDown:(NSEvent *)theEvent
{
  fprintf(stderr, "rightMouseDown:\n");
}

- (void)rightMouseUp:(NSEvent *)theEvent
{
  NSPoint p = [theEvent locationInWindow];
  fprintf(stderr, "rightMouseUp: %f %f\n", p.x, p.y);
}

- (void)drawRect:(NSRect)aRect
{
#if 0
  if (!backgroundColor)
    backgroundColor = [[NSColor blackColor] retain];
#endif

  [backgroundColor set];
  PSrectfill(aRect.origin.x, aRect.origin.y,
	     aRect.size.width, aRect.size.height);

  if (!image)
    [self createImage];

#if 1
  if (image)
    {
      NSEnumerator *e = [displayList objectEnumerator];
      id aDisplay;

      [image lockFocus];

      [backgroundColor set];
      PSrectfill(aRect.origin.x, aRect.origin.y,
		 aRect.size.width, aRect.size.height);

      while ((aDisplay = [e nextObject]))
	{
	  id g = [aDisplay discrete2d];
	  unsigned worldXsize = [g getSizeX];
	  unsigned worldYsize = [g getSizeY];
	  int x, y;

	  // Calculate the size and position of each discrete dot
	  // on the raster.  Deal in integer units instead of float.
	  NSRect b = [self bounds];
	  int gridXsize, gridYsize;
	  int szw = b.size.width;
	  int szh = b.size.height;
	  int rw = b.size.width / pointSize.width;
	  int rh = b.size.height / pointSize.height;
	  int xfo, yfo;
	  int newX, newY;

	  // Pick the smaller grid, either the world or the raster
	  if (worldXsize > rw)
	    gridXsize = rw;
	  else
	    gridXsize = worldXsize;
	  if (worldYsize > rh)
	    gridYsize = rh;
	  else
	    gridYsize = worldYsize;

	  // Get x, y offsets to center the grid in the view
	  xfo = szw % (gridXsize * (int)pointSize.width);
	  if (xfo != 0)
	    szw -= xfo;
	  yfo = szh % (gridYsize * (int)pointSize.height);
	  if (yfo != 0)
	    szh -= yfo;
	  xfo = xfo / 2;
	  yfo = yfo / 2;

#if 1
	  for (x = 0; x < gridXsize; ++x)
	    for (y = 0; y < gridYsize; ++y)
	      {
		NSRect aRect;
		aRect.origin.x = x * pointSize.width + xfo;
		aRect.origin.y = y * pointSize.height + yfo;
		aRect.size.width = pointSize.width;
		aRect.size.height = pointSize.width;
		[aDisplay displayX:x Y:y inRect:aRect];
	      }
#endif
#if 0
	  [aDisplay displayOn: image];
#endif
	}

      [image unlockFocus];
    }
#endif

#if 1
    [image compositeToPoint:aRect.origin 
	   fromRect:aRect
	   operation:NSCompositeCopy];
#endif
}

@end

#if 0
#define CALCULATE_FACTORS NSSize sz = [anImage size]; \
  int szw = sz.width; \
  int szh = sz.height; \
  int xfo, yfo; \
  int xf4, yf4, xf1, yf1, xf3, yf3; \
  int newX, newY; \
  xfo = szw % (worldXSize * 4); \
  if (xfo != 0) \
    szw -= xfo; \
  yfo = szh % (worldYSize * 4); \
  if (yfo != 0) \
    szh -= yfo; \
  xfo = xfo / 2; \
  yfo = yfo / 2; \
  xf4 = szw / worldXSize; \
  yf4 = szh / worldYSize; \
  xf1 = xf4 / 4; \
  yf1 = yf4 / 4; \
  xf3 = xf1 * 3; \
  yf3 = yf1 * 3; \
  newX = (xf4 * x) + xfo; \
  newY = (yf4 * y) + yfo;
#endif
