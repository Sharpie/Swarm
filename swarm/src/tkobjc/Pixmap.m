// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Pixmap.h>
#import "internal.h"
#import <tkobjc/global.h>
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

static int
compareRGB (id aobj, id bobj)
{
  png_byte a_red = ((png_bytep)aobj)[0];
  png_byte b_red = ((png_bytep)bobj)[0];
  
  int red_diff = a_red - b_red;
  
  if (red_diff == 0)
    {
      png_byte a_green = ((png_bytep)aobj)[1];
      png_byte b_green = ((png_bytep)bobj)[1];
      int green_diff = a_green - b_green;
      
      if (green_diff == 0)
        {
          png_byte a_blue = ((png_bytep)aobj)[2];
          png_byte b_blue = ((png_bytep)bobj)[2];
          int blue_diff = a_blue - b_blue;
          
          return blue_diff;
        }
      else
        return green_diff;
    }
  else
    return red_diff;
}

- createEnd
{
  FILE *fp;
  char header[8];
  png_structp read_ptr;
  png_infop read_info_ptr;
  unsigned row_bytes, row_columns;

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

    if (color_type == PNG_COLOR_TYPE_RGB)
      {
        if (bit_depth == 16)
          png_set_strip_16 (read_ptr);
        else if (bit_depth < 8)
          png_set_expand (read_ptr);
        bit_depth = 8;
        row_bytes = png_get_rowbytes (read_ptr, read_info_ptr);
        row_columns = row_bytes / 3;
      }
    else if (color_type == PNG_COLOR_TYPE_PALETTE)
      {
        row_bytes = png_get_rowbytes (read_ptr, read_info_ptr);
        row_columns = row_bytes;
        if (!png_get_PLTE (read_ptr, read_info_ptr, &palette, &num_palette))
          [PaletteError raiseEvent: "Cannot get palette from PNG file: %s\n",
                        filename];
      }
    else
      [PaletteError raiseEvent: "Wrong color type: %d\n", color_type];
    
    {
      unsigned ri;
      png_bytep row_pointers_buffer[height];
      png_bytep new_row_pointers_buffer[height];
      png_bytep *row_pointers = row_pointers_buffer;
      
      for (ri = 0; ri < height; ri++)
        row_pointers[ri] = xmalloc (row_bytes);
      
      png_read_image (read_ptr, row_pointers);
      
      fclose (fp);

      if (color_type == PNG_COLOR_TYPE_RGB)
        {
          unsigned ri;
          id cMap = [Map createBegin: [self getZone]];
          unsigned colorCount = 0;
          
          [cMap setCompareFunction: compareRGB];
          cMap = [cMap createEnd];
          
          for (ri = 0; ri < height; ri++)
            {
              unsigned ci;
              
              for (ci = 0; ci < row_columns; ci++)
                {
                  void *ptr = &row_pointers[ri][ci * 3];
                  
                  if (![cMap at: ptr])
                    [cMap at: ptr insert: (id)++colorCount];
                }
            }
          {
            id <MapIndex> mi = [cMap begin: [self getZone]];
            png_bytep rgb;
            id indexObj;

            num_palette = [cMap getCount];
            palette = xmalloc (sizeof (png_color) * num_palette);
            
            while ((indexObj = [mi next: (id *)&rgb]) != nil)
              {
                unsigned index = (unsigned)indexObj - 1;

                palette[index].red = rgb[0];
                palette[index].green = rgb[1];
                palette[index].blue = rgb[2];
              }
            [mi drop];
          }
          {
            for (ri = 0; ri < height; ri++)
              {
                unsigned ci;
                
                new_row_pointers_buffer[ri] = xmalloc (row_columns);
                for (ci = 0; ci < row_columns; ci++)
                  new_row_pointers_buffer[ri][ci] =
                    (png_byte)(unsigned)
                    [cMap at: (id)&row_pointers[ri][ci * 3]] - 1;
                xfree (row_pointers[ri]);
              }
            row_pointers = new_row_pointers_buffer;
            [cMap drop];
          }
        }
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
