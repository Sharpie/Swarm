// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "internal.h"
#import <tkobjc/global.h>
#import <tkobjc/Pixmap.h>

#include <png.h>
#include <misc.h> // xmalloc

@implementation Pixmap

PHASE(Creating)

- setFile: (const char *)s
{
  if (filename)
    {
      [InvalidCombination
        raiseEvent:
          "It is an error to reset the filename\n"];
      return nil;
    }
  else
    filename = s;
  return self;
}

- setRaster: theRaster
{
  raster = theRaster;

  return self;
}

- createEnd
{
  FILE *fp;
  char header[8];
  png_structp read_ptr;
  png_infop read_info_ptr;

  [super createEnd];

  if ((fp = fopen (filename, "rb")) == NULL)
    [MissingFiles raiseEvent: "Cannot open %s", filename];
  
  if (fread (header, sizeof (header), 1, fp) != 1)
    {
      fclose (fp);
      [MissingFiles raiseEvent: "Short read of %s", filename];
    }
  
  if (png_check_sig (header, sizeof (header)) == 0)
    {
      fclose (fp);
      [MissingFiles raiseEvent: "%s is not a PNG file", filename];
    }
  if ((read_ptr = png_create_read_struct (PNG_LIBPNG_VER_STRING,
                                          (png_voidp)NULL,
                                          (png_error_ptr)NULL,
                                          (png_error_ptr)NULL)) == NULL)
    {
      fclose (fp);
      [InternalError raiseEvent: "Could not create PNG read_struct"];
    }
  if ((read_info_ptr = png_create_info_struct (read_ptr)) == NULL)
    {
      fclose (fp);
      png_destroy_read_struct (&read_ptr, NULL, NULL);
      [InternalError raiseEvent: "Could not create PNG info struct"];
    }
  if (setjmp (read_ptr->jmpbuf))
    {
      png_destroy_read_struct (&read_ptr, &read_info_ptr, (png_infopp)NULL);
      fclose (fp);
      [InternalError raiseEvent: "Problem reading PNG file `%s'", filename];
    }

  png_init_io (read_ptr, fp);
  png_set_sig_bytes (read_ptr, sizeof (header));
  
  png_read_info (read_ptr, read_info_ptr);
  
  {
    int bit_depth, color_type;
    int interlace_type, compression_type, filter_type;
    png_colorp palette;
    png_uint_32 pngwidth, pngheight;
    int num_palette;

    png_get_IHDR (read_ptr, read_info_ptr, &pngwidth, &pngheight, 
                  &bit_depth, &color_type, &interlace_type,
                  &compression_type, &filter_type);
    width = pngwidth;
    height = pngheight;
    if (!png_get_PLTE (read_ptr, read_info_ptr, &palette, &num_palette))
      [WindowCreation raiseEvent: "Cannot get palette from PNG file: %s\n",
                      filename];
    {
      png_bytep row_pointers[height];
      int ri;
      unsigned rc = png_get_rowbytes (read_ptr, read_info_ptr);

      for (ri = 0; ri < height; ri++)
        row_pointers[ri] = xmalloc (rc);
      
      png_read_image (read_ptr, row_pointers);

      fclose (fp);
      tkobjc_pixmap_create (self, row_pointers, bit_depth,
                            palette, num_palette, raster);
    }
  }

  return self;
}             

PHASE(Using)               
                                         
- (unsigned)getWidth
{
  return width;
}

- (unsigned)getHeight
{
  return height;
}

- drawX: (int)x Y: (int)y
{
  tkobjc_pixmap_draw (self, x, y, raster);
  
  return self;
}

@end
