#include "win32dib.h"
#include <misc.h>
#include <windows.h>

dib_t *
dib_create (void)
{
  dib_t *dib = xmalloc (sizeof (dib_t));
  
  dib->window = NULL;
  dib->sourceDC = NULL;
  dib->destDC = NULL;
  dib->dibInfo = xmalloc (sizeof (CDIB_BITMAP));
  dib->bitmap = NULL;
  dib->oldBitmap = NULL;
  dib->bits = NULL;
  dib->palette = NULL;
  dib->oldPalette = NULL;
  dib->xBitmapOffset = 0;
  dib->yBitmapOffset = 0;

  return dib;
}

void
dib_destroy (dib_t *dib)
{
  xfree (dib->dibInfo);
  dib->dibInfo = NULL;
  if (dib->bitmap)
    {
      HDC hdc = GetDC(dib->window);
      SelectObject (hdc, dib->oldBitmap);
      DeleteObject(dib->bitmap);
      ReleaseDC (dib->window, hdc);
    }
  dib->bitmap = NULL;
  dib->oldBitmap = NULL;
  dib->bits = NULL;

  if (dib->palette)
    {
      DeleteObject (dib->palette);
      dib->palette = NULL;
    }
  dib->oldPalette = NULL;
  dib->window = NULL;
  if (dib->sourceDC)
    {
      DeleteDC (dib->sourceDC);
      dib->sourceDC = NULL;
    }
  if (dib->destDC)
    {
      /* Shouldn't this be before the dib->window is NULLed? */
      ReleaseDC (dib->window, dib->destDC);
      dib->destDC = NULL;
    }
  dib->xBitmapOffset = 0;
  dib->yBitmapOffset = 0;
}

void
dib_createBitmap (dib_t *dib, HWND window, int width, int height)
{
  HDC hdc = GetDC (window);
  dib->window = window;

  if (dib->bitmap)
    {
      xfree (dib->dibInfo);
      SelectObject (hdc, dib->oldBitmap);
      DeleteObject (dib->bitmap);
      ReleaseDC (window, hdc);
      dib->bitmap = NULL;
      dib->oldBitmap = NULL;
      dib->bits = NULL;
    }

  dib->dibInfo->bmiHead.biSize = sizeof (BITMAPINFOHEADER);
  dib->dibInfo->bmiHead.biWidth = width;
  if (TOP_DOWN_DIB)
    dib->dibInfo->bmiHead.biHeight = -height;
  else
    dib->dibInfo->bmiHead.biHeight = height;
  dib->dibInfo->bmiHead.biPlanes = 1;
  dib->dibInfo->bmiHead.biBitCount = 8;
  dib->dibInfo->bmiHead.biCompression = BI_RGB;
  dib->dibInfo->bmiHead.biSizeImage = 0;
  dib->dibInfo->bmiHead.biClrUsed = 0;
  dib->dibInfo->bmiHead.biClrImportant = 0;
  dib->bitmap = CreateDIBSection (hdc,
				  (BITMAPINFO *)dib->dibInfo,
				  DIB_PAL_COLORS,
				  &dib->bits,
				  NULL,
				  0 /* ignored if above is NULL */ );
  ReleaseDC (window, hdc);
}

void
dib_setPalette (dib_t *dib, HPALETTE handle, unsigned long *map)
{
  if (dib->bitmap == NULL)
    return;
  
  if (dib->palette != NULL)
    {
      DeleteObject (dib->palette);
      dib->palette = NULL;
    }
  dib->palette = handle;
  
  if (map)
    {
      int i;
      HDC shdc = CreateCompatibleDC (NULL);
      
      for (i = 0; i < 256; i++)
	{
	  dib->dibInfo->rgb[i].rgbRed = map[i] & 0xff;
	  dib->dibInfo->rgb[i].rgbGreen = map[i] >> 8 & 0xff;
	  dib->dibInfo->rgb[i].rgbBlue = map[i] >> 16;
	}
      dib->oldBitmap = SelectObject (shdc, dib->bitmap);
      SetDIBColorTable (shdc, 0, 256, dib->dibInfo->rgb);
      SelectObject (shdc, dib->oldBitmap);
      DeleteDC (shdc);
    }
}

void
dib_realizePalette (dib_t *dib)
{
  if (dib->palette != NULL)
    {
      HDC hdc = GetDC (dib->window);
      
      SelectPalette (hdc, dib->palette, FALSE);
      RealizePalette (hdc);
      ReleaseDC (dib->window, hdc);
      
      InvalidateRect (dib->window, NULL, TRUE);
    }
}

BOOL
dib_blit (dib_t *dib,
	  int destX, int destY,
	  int sourceX, int sourceY,
	  int sourceWidth, int sourceHeight)
{
  if (dib->bitmap)
    {
      BOOL result;
      
      SelectPalette (dib->destDC, dib->palette, FALSE);
      RealizePalette (dib->destDC);
      result = BitBlt (dib->destDC,
		       destX, destY,
		       sourceWidth, sourceHeight,
		       dib->sourceDC,
		       sourceX, sourceY,
		       SRCCOPY);

      return result;
    }
  return FALSE;
}

void
dib_fill (dib_t *dib,
	  int x, int y,
	  int width, int height,
	  unsigned char color)
{
  int frameWidth = dib->dibInfo->bmiHead.biWidth;
  BYTE *base;
  int yoff;

  /* GDI raster lines are even length. */
  if (frameWidth & 1)
    frameWidth++;

  base = (BYTE *)dib->bits + (y * frameWidth);
  for (yoff = 0; yoff < height; yoff++)
    {
      int xoff;
      BYTE *ybase = &base[yoff * frameWidth + x];
      
      for (xoff = 0; xoff < width; xoff++)
	ybase[xoff] = color;
    }
}

BOOL
dib_paintBlit (dib_t *dib,
	       HDC destDC,
	       int destX, int destY,
	       int sourceX, int sourceY,
	       int sourceWidth, int sourceHeight)
{
  if (dib->bitmap == NULL)
    return FALSE;
  
  GdiFlush ();

  {
    /* Lock */
    HDC sourceDC = CreateCompatibleDC (destDC);

    dib->oldBitmap = SelectObject (sourceDC, dib->bitmap);
    
    /* Blit */
    {
      BOOL result;
      
      SelectPalette (destDC, dib->palette, FALSE);
      RealizePalette (destDC);
      result = BitBlt (destDC, 
		       destX, destY,
		       sourceWidth, sourceHeight,
		       sourceDC, 
		       sourceX + dib->xBitmapOffset,
		       sourceY + dib->yBitmapOffset,
		       SRCCOPY);
      
      /* Unlock */
      SelectObject (sourceDC, dib->oldBitmap);
      DeleteDC (sourceDC);
      
      return result;
    }
  }
}

BOOL
dib_copy (dib_t *source, dib_t *dest, int width, int height)
{
  BOOL result;

  if (dib_lock (dest) == NULL)
    return FALSE;
  if (dib_lock (source) == NULL)
    return FALSE;
  result = BitBlt (dest->sourceDC, 0, 0,
		   width, height,
		   source->sourceDC, 0, 0,
		   SRCCOPY);
  dib_unlock (source);
  dib_unlock (dest);
  return result;
}

BYTE *
dib_lock (dib_t *dib)
{
  if (dib->bitmap == NULL)
    return NULL;
  
  GdiFlush ();
  
  if (!dib->destDC)
    dib->destDC = GetDC (dib->window);
  
  if (!dib->sourceDC)
    dib->sourceDC = CreateCompatibleDC (dib->destDC);

  dib->oldBitmap = SelectObject (dib->sourceDC, dib->bitmap);
  
  return dib->bits;
}

void
dib_unlock (dib_t *dib)
{
  if (dib->bitmap)
    {
      SelectObject (dib->sourceDC, dib->oldBitmap);
      
      DeleteDC (dib->sourceDC);
      dib->sourceDC = NULL;
      ReleaseDC (dib->window, dib->destDC);
      dib->destDC = NULL;
    }
}

#if 0
void
dib_setPaintOffset (dib_t *dib, int xOffset, int yOffset)
{
  dib->xBitmapOffset = xOffset;
  dib->yBitmapOffset = yOffset;
}

BYTE *
dib_getSurface (dib_t *dib)
{
  if (dib->bitmap == NULL)
    return NULL;

  return (BYTE *)dib->bits;
}

int
dib_getWidth (dib_t *dib)
{
  if (dib->bitmap == NULL)
    return -1;
  
  return dib->dibInfo->bmiHead.biWidth;
}


int
dib_getHeight (dib_t *dib)
{
  if (dib->bitmap == NULL)
    return -1;

  {
    int height = dib->dibInfo->bmiHead.biHeight;

    return height < 0 ? -height : height;
  }
}
#endif
