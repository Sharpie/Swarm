// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <space/Value2dDisplay.h>
#import <gui.h>

// This should be subclassed to fill in the colormap for your CA.
// for now, we expect your colormap to be allocated already.

//S: Value2dDisplay displays 2d arrays of values.
//D: Value2dDisplay helps display 2d arrays of values.
//D: Value2dDisplay goes through a given Discrete2d array,
//D: turn states into colours, and draws them into a Raster widget. 
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

//M: Set the display widget and the colourmap to use to draw the value array. 
- setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c
{
  displayWidget = r;
  colormap = c;

#ifndef USE_JAVA
  drawPointImp = getMethodFor ([r getClass], @selector (drawPointX:Y:Color:));
#else
  drawPointImp = [(Object *)r methodFor: @selector (drawPointX:Y:Color:)];
#endif
  return self;
}

//M: Set which array to draw. 
- setDiscrete2dToDisplay: (Discrete2d *)c
{
  discrete2d = c;
  return self;
}

// linear transform between values and colours. Good enough?
//M: Linear transform of states to colours for drawing. 
//M: color = state / m + c 
//M: If not set, assume m == 1 and c == 0. 
- setDisplayMappingM: (int)m C: (int)c
{
  modFactor = m;
  colorConstant = c;
  return self;
}

//M: Draw the array on the given widget.
//M: Note that you still have to tell the widget to draw itself afterwards.
//M: The code for display uses the fast macro access in Discrete2d on
//M: the cached return value from getLattice.
//M: It also caches the drawPointX:Y: method lookup on the display widget -
//M: this is a nice trick that you might want to look at. 
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
