// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Value2dDisplay.h>
#import <gui.h>

// this should be subclassed to fill in the colormap for your CA.
// for now, we expect your colormap to be allocated already.

@implementation Value2dDisplay

- createEnd
{
  [super createEnd];

  if (displayWidget == nil || discrete2d == nil)
    [InvalidCombination raiseEvent: "Value display improperly initialized\n"];
  
  if (modFactor == 0)
    modFactor = 1;

  return self;
}

- setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c
{
  displayWidget = r;
  colormap = c;
  drawPointImp = getMethodFor ([r getClass], @selector (drawPointX:Y:Color:));
  return self;
}

- setDiscrete2dToDisplay: (Discrete2d *)c
{
  discrete2d = c;
  return self;
}

// linear transform between values and colours. Good enough?
- setDisplayMappingM: (int)m C: (int)c
{
  modFactor = m;
  colorConstant = c;
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

  for (y = 0; y < ysize; y++)
    for (x = 0; x < xsize; x++)
      {
        long color;
        color = (long) *(discrete2dSiteAt(lattice, offsets, x, y));
        color = color / modFactor + colorConstant;
        if (color < 0 || color > 255)
          {
            [WarningMessage 
              raiseEvent: 
                "Value2dDisplay: found colour %d not in [0,255].\n", color];
          }
        
#ifdef METHODS
        [displayWidget drawPointX: x Y: y Color: (unsigned char)color];
#else
        // cache method lookup.
        (void) *drawPointImp (displayWidget,
                              @selector(drawPointX:Y:Color:),
                              x, y, color);
#endif
      }
  return self;
}

@end
