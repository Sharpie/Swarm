// Paul Johnson, copyright 2000-2004

// The code is made available under the provisions of the GNU Greater
// Public License (version 2 or newer, at the user's discretion). 

// Class that can save a sequence of images corresponding
// to raster on disk when the GUI isn't running. Modified
// from code by Nelson: Benedikt Stefansson <benedikt@ucla.edu>

#include "BatchRaster.h"

#define bufferAt(x,y) (buffer[(y)*logicalWidth + (x)])

@implementation BatchRaster

- (unsigned)getWidth 
{
  return (logicalWidth);
}

- (unsigned)getHeight 
{
  return (logicalHeight);
}

- pack 
{
  return self;
}


- createEnd 
{
  // create a frame 
  
  logicalWidth = 100U;
  logicalHeight = 100U;
  
  // no buffer to start
  buffer = 0;

  return self;
}


- (BatchColormap *)getColormap 
{
  return colormap;
}

// this widget won't work without this initialized.
- setColormap: (BatchColormap *)c 
{

  colormap = c;
  
  return self;
}

- setWidth: (int)newWidth Height: (int)newHeight 
{

  logicalWidth = newWidth;
  logicalHeight = newHeight;
  
  // allocate the buffer (free the old one if necessary)
  if (buffer)
    free(buffer);
  buffer = calloc(logicalWidth * logicalHeight, sizeof(*buffer));
  
  return self;
}

// Caution: I'm using color 21 as "erase"
- erase 
{
  // erase the buffer (set it to color 0, since there's no default bg)
  if (buffer != NULL)
    memset(buffer, 0, logicalWidth*logicalHeight*sizeof(*buffer));
  return self;
}

// drawing is just like before, only magnified.


- drawPointX: (int)x Y: (int)y Color: (int)c 
{

  bufferAt(x,y) = c;

  return self;
}

- fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (int)c{
  int x, y;
  
  // CHECK -  is this consistent with X's convention for rectangle width?
  for (x = x0; x < x1; x++)
    for (y = y0; y < y1; y++)
      bufferAt(x, y) = c;

  return self;
}

- writeSelfToFile: (char *)f 
{
  FILE * fp;
  unsigned int x, y;
  
  fp = fopen(f, "w");
  if (fp == 0) 
    {
      char s[1024];
      sprintf(s, "Warning: couldn't open file %s for writing.", f);
      raiseEvent(WarningMessage, s);
      return self;
    }

  fprintf(fp, "P6\n%d %d\n255\n", logicalWidth, logicalHeight);
  for (y = 0; y < logicalHeight; y++)
    for (x = 0; x < logicalWidth; x++) 
      {
	RGB * value = [colormap rgbValue: bufferAt(x, y)];
	fputc([value getRed], fp);
	fputc([value getGreen], fp);
	fputc([value getBlue], fp);
      }
  fclose(fp);
  return self;
}

- (void)drop 
{
  free(buffer);
  [super drop];
}

@end



