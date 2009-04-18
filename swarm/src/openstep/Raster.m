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

#if 0
- initWithFrame:(NSRect)aRect pointSize:(NSSize)aSize
{
  fprintf(stderr, "Raster initWithFrame:pointSize:\n");

  //image = nil;
  //imageRep = nil;
  rasterOrigin.x = 0;
  rasterOrigin.y = 0;
  pointSize = aSize;
  displayList = [NSMutableArray new];
  backgroundColor = [[NSColor whiteColor] retain];

#if 0
  [[NSNotificationCenter defaultCenter] addObserver:self
    selector:@selector(frameRectDidChange:)
    name:NSViewFrameDidChangeNotification object:self];
#endif

  fprintf(stderr, "done Raster initWithFrame:pointSize:\n");
  return self;
}
#endif

- initWithFrame:(NSRect)aRect
{
  [super initWithFrame:aRect];

  displayList = [NSMutableArray new];
  backgroundColor = [[NSColor whiteColor] retain];

  return self;
}

#if 0
- (void)setRasterOrigin:(NSPoint)aPoint
{
  rasterOrigin = aPoint;

  //[self determineWindowMinMaxSize];

  // redisplay
}

- (void)setPointSize:(NSSize)aSize
{
  pointSize = aSize;

  //[self determineWindowMinMaxSize];

  // redisplay
}

- (void)setBounds:(NSRect)boundsRect
{
  [super setBounds: boundsRect];

  printf("new bounds: %f %f %f %f\n", boundsRect.origin.x, boundsRect.origin.y,
    boundsRect.size.width, boundsRect.size.height);
  
  //[self createImage];
}
#endif

- (void)addDisplay: aDisplay
{
  if (aDisplay)
    {
      fprintf(stderr, "addDisplay:\n");
      [displayList addObject: aDisplay];

      //[self determineBoundsFromDisplays];
    }
}

- (void)removeDisplay: aDisplay
{
  if (aDisplay)
    {
      fprintf(stderr, "removeDisplay:\n");
      [displayList removeObject: aDisplay];

      //[self determineBoundsFromDisplays];
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
  NSRectFill(aRect);

 // printf("drawRect: %f %f %f %f\n", aRect.origin.x, aRect.origin.y,
 //   aRect.size.width, aRect.size.height);

#if 1
    NSEnumerator *e = [displayList objectEnumerator];
    id aDisplay;
    
    while ((aDisplay = [e nextObject])) {
      NSImage *image = [aDisplay image];
      [image drawInRect: aRect fromRect: NSZeroRect operation: NSCompositeSourceAtop fraction: 1.0];
    }
#endif

#if 0
  if (!image)
    [self createImage];

  if (image) {
    NSEnumerator *e = [displayList objectEnumerator];
    id aDisplay;
    

    NSSize aSize = [image size];
    printf("drawRect image: %f %f\n", aSize.width, aSize.height);
  
    while ((aDisplay = [e nextObject])) {
      int x, y;
      
      [image lockFocus];
    
      [[NSColor clearColor] set];
      NSRectFill(aRect);
      for (x = 0; x < aSize.width; ++x)
        for (y = 0; y < aSize.height; ++y)
	      {
          NSRect aRect = NSMakeRect(x, y, 1, 1);
          [aDisplay displayX:x Y:y inRect:aRect];
	      }
      [image unlockFocus];

      [image drawInRect: aRect fromRect: NSZeroRect operation: NSCompositeSourceAtop fraction: 1.0];

    }
  }
#endif

#if 0
    NSEnumerator *e = [displayList objectEnumerator];
    id aDisplay;

    while ((aDisplay = [e nextObject])) {
      int x, y;
      
      for (x = (int)aRect.origin.x; x < aRect.size.width; ++x)
        for (y = (int)aRect.origin.y; y < aRect.size.height; ++y)
	      {
          float xx = (float)x;
          float yy = (float)y;
          NSRect aRect = NSMakeRect(xx, yy, 1.0, 1.0);
          [aDisplay displayX:x Y:y inRect:aRect];
	      }
    }
#endif

#if 0
  if (image)
    {
      NSEnumerator *e = [displayList objectEnumerator];
      id aDisplay;

      [image lockFocus];

      [backgroundColor set];
      NSRectFill(aRect);

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

#if 0
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

#if 0
  if (image)
    [image drawInRect: aRect fromRect: NSZeroRect operation: NSCompositeCopy fraction: 1.0];
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
