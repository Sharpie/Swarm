// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Pixmap.h>
#import "internal.h"
#import <tkobjc/global.h>
#include <png.h>
#include <misc.h> // xmalloc, XFREE

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

@implementation Pixmap

PHASE(Creating)

- _loadPNG_
{
  FILE *fp;
  char header[8];
  png_structp read_ptr;
  png_infop read_info_ptr;
  unsigned row_bytes = 0;
  int dirlen = strlen (directory);
  char path[dirlen + 1 + strlen (filename) + 1], *p;
  
  p = stpcpy (path, directory);
  if (directory[dirlen - 1] != '/')
    p = stpcpy (p, "/");
  p = stpcpy (p, filename);
  
  if ((fp = fopen (path, "rb")) == NULL)
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
      [PixmapError raiseEvent: "Could not create PNG read_struct"];
    }
  if ((read_info_ptr = png_create_info_struct (read_ptr)) == NULL)
    {
      fclose (fp);
      png_destroy_read_struct (&read_ptr, NULL, NULL);
      [PixmapError raiseEvent: "Could not create PNG info struct"];
    }
  if (setjmp (read_ptr->jmpbuf))
    {
      png_destroy_read_struct (&read_ptr, &read_info_ptr, (png_infopp)NULL);
      fclose (fp);
      [PixmapError raiseEvent: "Problem reading PNG file `%s'", filename];
    }

  png_init_io (read_ptr, fp);
  png_set_sig_bytes (read_ptr, sizeof (header));
  
  png_read_info (read_ptr, read_info_ptr);
  
  {
    int bit_depth, color_type;
    int interlace_type, compression_type, filter_type;
    png_uint_32 pngwidth, pngheight;

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
      }
    else if (color_type == PNG_COLOR_TYPE_PALETTE)
      {
        row_bytes = png_get_rowbytes (read_ptr, read_info_ptr);
        if (!png_get_PLTE (read_ptr, read_info_ptr, &palette, &palette_size))
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
      unsigned row_columns = row_bytes / 3;
      
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

            palette_size = [cMap getCount];
            palette = xmalloc (sizeof (png_color) * palette_size);
            
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
                  {
                    png_bytep rgb = &row_pointers[ri][ci * 3];
                    id indexObj = [cMap at: (id)rgb];

                    if (indexObj == nil)
                      [PaletteError raiseEvent:
                                      "No index for R:%d G:%d B:%d\n",
                                    rgb[0], rgb[1], rgb[2]];
                    new_row_pointers_buffer[ri][ci] = 
                      (png_byte)(unsigned)indexObj - 1;
                  }
              }
            [cMap drop];
            for (ri = 0; ri < height; ri++)
              XFREE (row_pointers[ri]);
            row_pointers = new_row_pointers_buffer;
          }
        }
      tkobjc_pixmap_create (self, row_pointers, bit_depth);
    }
  }
  return self;
}

- setDirectory: (const char *)theDirectory
{
  if (theDirectory)
    directory = theDirectory;
  
  return self;
}

- setFile: (const char *)theFilename
{
  filename = theFilename;
  
  return self;
}

- setWidget: (id <Widget>)theWidget
{
  widget = theWidget;
  
  return self;
}

- setParentFlag: (BOOL)theParentFlag
{
  parentFlag = theParentFlag;

  return self;
}

+ createBegin: aZone
{
  Pixmap *obj = [super createBegin: aZone];

  obj->widget = nil;
  obj->directory = "./";
  obj->filename = NULL;
  obj->parentFlag = NO;

  return obj;
}
  
- createEnd
{
  if (filename)
    [self _loadPNG_];
  else
    tkobjc_pixmap_create_from_widget (self, widget, parentFlag);
  
  return self;
}

PHASE(Using)               

- setRaster: theRaster
{
  raster = theRaster;
  tkobjc_pixmap_update_raster (self, raster);

  return self;
}

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

- save: (const char *)path
{
  tkobjc_pixmap_save (self, path);

  return self;
}

- (void)drop
{
  tkobjc_pixmap_drop (self);
  [super drop];
}

@end
