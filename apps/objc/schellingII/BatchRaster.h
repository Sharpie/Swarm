//Paul Johnson, copyright 2000-2004

// The code is made available under the provisions of the GNU Greater
// Public License (version 2 or newer, at the user's discretion).  

// Class that can save a sequence of images corresponding
// to raster on disk when the GUI isn't running. Modified
// from code by Nelson: Benedikt Stefansson <benedikt@ucla.edu>
// First version 7/23/97

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "BatchColormap.h"

@interface BatchRaster: Swarm {
  unsigned logicalWidth;
  unsigned logicalHeight;
  int * buffer;
  BatchColormap * colormap;
  
}

- (unsigned)getWidth;
- (unsigned)getHeight;
- pack;
- createEnd;
- (BatchColormap *)getColormap;
- setColormap: (BatchColormap *)c;
- setWidth: (int)newWidth Height: (int)newHeight;
- erase;
- drawPointX: (int)x Y: (int)y Color: (int)c;
- fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (int)c;
- writeSelfToFile: (char *)f;
- (void)drop;

@end



