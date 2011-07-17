// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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

#import <tkobjc/Pixmap.h>
#import <defobj.h> // zones
#include "internal.h"
#import <tkobjc/global.h>
#import <defobj/defalloc.h> // getZone
#include <swarmconfig.h> // PTRUINT

#ifdef HAVE_PNG
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
#endif

@implementation Pixmap

PHASE(Creating)

- _loadPNG_
{
#ifdef HAVE_PNG
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
    raiseEvent (MissingFiles, "Cannot open %s", filename);
  
  if (fread (header, sizeof (header), 1, fp) != 1)
    {
      fclose (fp);
      raiseEvent (MissingFiles, "Short read of %s", filename);
    }
  
  if (png_check_sig ((png_bytep)header, sizeof (header)) == 0)
    {
      fclose (fp);
      raiseEvent (MissingFiles, "%s is not a PNG file", filename);
    }
  if ((read_ptr = png_create_read_struct (PNG_LIBPNG_VER_STRING,
                                          (png_voidp)NULL,
                                          (png_error_ptr)NULL,
                                          (png_error_ptr)NULL)) == NULL)
    {
      fclose (fp);
      raiseEvent (PixmapError, "Could not create PNG read_struct");
    }
  if ((read_info_ptr = png_create_info_struct (read_ptr)) == NULL)
    {
      fclose (fp);
      png_destroy_read_struct (&read_ptr, NULL, NULL);
      raiseEvent (PixmapError, "Could not create PNG info struct");
    }
  if (setjmp (png_jmpbuf(read_ptr)))
    {
      png_destroy_read_struct (&read_ptr, &read_info_ptr, (png_infopp)NULL);
      fclose (fp);
      raiseEvent (PixmapError, "Problem reading PNG file `%s'", filename);
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
    if (color_type != PNG_COLOR_TYPE_PALETTE)
      {
	if (color_type & PNG_COLOR_MASK_ALPHA)
	  png_set_strip_alpha (read_ptr);
        if (bit_depth == 16)
          png_set_strip_16 (read_ptr);
        else if (bit_depth < 8)
          png_set_expand (read_ptr);
        bit_depth = 8;
        row_bytes = png_get_rowbytes (read_ptr, read_info_ptr);
      }
    else
      {
        row_bytes = png_get_rowbytes (read_ptr, read_info_ptr);
        if (!png_get_PLTE (read_ptr, read_info_ptr, &palette, (int *)&palette_size))
          raiseEvent (PaletteError,
                      "Cannot get palette from PNG file: %s\n",
                       filename);
      }
    
    {
      unsigned ri;
      png_bytep row_pointers_buffer[height];
      png_bytep new_row_pointers_buffer[height];
      png_bytep *row_pointers = row_pointers_buffer;
      id aZone = getZone (self);

      for (ri = 0; ri < height; ri++)
        row_pointers[ri] = [aZone alloc: row_bytes];
      
      png_read_image (read_ptr, row_pointers);
      
      fclose (fp);

      if (color_type != PNG_COLOR_TYPE_PALETTE)
        {
          unsigned ri;
          id cMap = [Map createBegin: getZone (self)];
          unsigned colorCount = 0;
	  unsigned row_columns = row_bytes / 3;
          
          [cMap setCompareFunction: compareRGB];
          cMap = [cMap createEnd];

          for (ri = 0; ri < height; ri++)
            {
              unsigned ci;
              
              for (ci = 0; ci < row_columns; ci++)
                {
                  void *ptr = &row_pointers[ri][ci * 3];
                  
                  if (![cMap at: ptr])
                    [cMap at: ptr insert: (id) (PTRUINT) ++colorCount];
                }
            }
          {
            id mi = [cMap begin: getZone (self)];
            png_bytep rgb;
            id indexObj;

            palette_size = [cMap getCount];
            palette = xmalloc (sizeof (png_color) * palette_size);
            
            while ((indexObj = [mi next: (id *)&rgb]) != nil)
              {
                unsigned index = (PTRUINT) indexObj - 1;

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
                      raiseEvent (PaletteError,
                                  "No index for R:%d G:%d B:%d\n",
                                  rgb[0], rgb[1], rgb[2]);
                    new_row_pointers_buffer[ri][ci] = 
                      (png_byte) (PTRUINT) indexObj - 1;
                  }
              }
            [cMap drop];
            for (ri = 0; ri < height; ri++)
              [aZone free: row_pointers[ri]];
            row_pointers = new_row_pointers_buffer;
          }
        }
      tkobjc_pixmap_create (self, row_pointers, bit_depth);
    }
  }
#else
  raiseEvent (NotImplemented,
              "PNG inoput not available on this configuration");
#endif
  return self;
}

- setDirectory: (const char *)theDirectory
{
  if (directory)
    FREEBLOCK (directory);
 
 if (theDirectory)
    directory = STRDUP (theDirectory);
  else
    directory = NULL;
  
  return self;
}

- setFile: (const char *)theFilename
{
  if (filename)
    FREEBLOCK (filename);

  if (theFilename)
    filename = STRDUP (theFilename);
  else
    filename = NULL;
  
  return self;
}

- setWidget: (id <Widget>)theWidget
{
  widget = theWidget;
  
  return self;
}

- setDecorationsFlag: (BOOL)theDecorationsFlag
{
  decorationsFlag = theDecorationsFlag;

  return self;
}

+ createBegin: aZone
{
  Pixmap *obj = [super createBegin: aZone];

  obj->widget = nil;
  obj->directory = ZSTRDUP (aZone, "./");
  obj->filename = NULL;
  obj->decorationsFlag = NO;
#ifndef _WIN32
  obj->pixmap = 0;
  obj->mask = 0;
  obj->display = NULL;
#endif

  return obj;
}
  
- createEnd
{
  if (filename)
    [self _loadPNG_];
  else
    tkobjc_pixmap_create_from_widget (self, widget, decorationsFlag);
  
  return self;
}

PHASE(Using)               

- (void)setRaster: theRaster
{
  raster = theRaster;
  tkobjc_pixmap_update_raster (self, (Raster *)raster);
}

- (unsigned)getWidth
{
  return width;
}

- (unsigned)getHeight
{
  return height;
}

- (void)drawX: (int)x Y: (int)y
{
  tkobjc_pixmap_draw (self, x, y, (Raster *)raster);
}

- (void)save: (const char *)path
{
  tkobjc_pixmap_save (self, path);
}

- (void)drop
{
  tkobjc_pixmap_drop (self);
  if (directory)
    FREEBLOCK (directory);
  if (filename)
    FREEBLOCK (filename);

  [super drop];
}

@end
