#ifndef _DIB_H
#define _DIB_H
#include <windows.h>
#include <windowsx.h>

/* 1 = Top-Down   0 = Bottom-Up */
#define TOP_DOWN_DIB 1

/* BITMAPINFO */
typedef struct CDIB_BITMAP {
  BITMAPINFOHEADER bmiHead;
  RGBQUAD rgb[256];
} CDIB_BITMAP;

typedef struct CDIBPALETTE { 
  WORD palVersion; 
  WORD palNumEntries; 
  PALETTEENTRY aEntries[256]; 
} CDIBPALETTE; 	

typedef struct dib {
  /* Controlling window */
  HWND window;
  HDC sourceDC;
  HDC destDC;

  CDIB_BITMAP *dibInfo;
  
  /* Bitmap */
  HBITMAP bitmap;
  HBITMAP oldBitmap;
  void /* far */ *bits;

  /* Palette */
  HPALETTE *palette;
  HPALETTE *oldPalette;

  /* Offset coordinates */
  int xBitmapOffset;
  int yBitmapOffset;
} dib_t;

/* Constructor */
dib_t *dib_create (void);
void dib_destroy (dib_t *dib);

void dib_createBitmap (dib_t *dib, HWND window, int width, int height);
void dib_deleteBitmap (dib_t *dib);

/* Palette Manipulation */
void dib_setPalette (dib_t *dib, HPALETTE handle, unsigned long *map);
void dib_realizePalette (dib_t *dib);

/* Blitting interface  */
/* Make sure to: 1) Lock surface, 2) Blit, 3) Unlock surface */

BOOL dib_blit (dib_t *dib,
	       int destX, int destY,
	       int sourceX, int sourceY,
	       int sourceWidth, int sourceHeight);

/* Called from a WM_PAINT command -- No locking required. */
BOOL dib_paintBlit (dib_t *dib,
		    HDC destHdc,
		    int destX, int destY,
		    int sourceX, int sourceY,
		    int sourceWidth, int sourceHeight);

void dib_fill (dib_t *dib, int x, int y, unsigned width, unsigned height, unsigned char color);
void dib_ellipse (dib_t *dib, int x, int y, unsigned width, unsigned height, unsigned pixels, unsigned char color);

BOOL dib_copy (dib_t *source, dib_t *dest, int width, int height);

BYTE* dib_lock (dib_t *dib);
void dib_unlock (dib_t *dib);

#if 0
/* Used to control what part of the bitmap gets shown to the screen */
/* Helps with scrolling / oversized senarios */
void dib_setPaintOffset (dib_t *dib, int xOffset, int yOffset);

/* Surface info */
BYTE *dib_getSurface (dib_t *dib);

int dib_getWidth (dib_t *dib);
int dib_getHeight (dib_t *dib);

#endif
#endif
