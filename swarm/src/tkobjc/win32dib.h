#ifndef _DIB_H
#define _DIB_H

#ifdef __CYGWIN__
#define _WIN32
#endif

#ifdef _WIN32
#include <windows.h>
#include <windowsx.h>
#define BOOL OBJC_BOOL
#include <misc.h>
#undef BOOL

/* 1 = Top-Down   0 = Bottom-Up */
#define TOP_DOWN_DIB 1

/* BITMAPINFO */
typedef struct CDIB_BITMAP {
  BITMAPINFOHEADER bmiHead;
  RGBQUAD rgb[256]; // Not used for BI_RGB with more than 256 colors.
} CDIB_BITMAP;

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

  unsigned colormapOffsets[256];
  void *colormapObjects[256];
  unsigned colormapBlocks;
  unsigned colormapSize;
  unsigned long *colormap; // only used for when depth > 8
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

void dib_createBitmap (dib_t *dib, HWND window, unsigned width, unsigned height);
void dib_deleteBitmap (dib_t *dib);

/* Palette Manipulation */
int dib_paletteIndexForObject (dib_t *dib, void *object);
void dib_get_color (dib_t *dib, void *object, unsigned color, BYTE *red, BYTE *green, BYTE *blue);

void dib_augmentPalette (dib_t *dib, void *object,
			 unsigned mapSize, unsigned long *map);

/* Called from a WM_PAINT command -- No locking required. */
BOOL dib_paintBlit (dib_t *dib,
		    HDC destHdc,
		    int destX, int destY,
		    int sourceX, int sourceY,
		    unsigned sourceWidth, unsigned sourceHeight);

void dib_fill (dib_t *dib, int x, int y, unsigned width, unsigned height, void *object, unsigned color);
void dib_ellipse (dib_t *dib, int x, int y, unsigned width, unsigned height, unsigned pixels, void *object, unsigned color);
void dib_line (dib_t *dib, int x0, int y0, int x1, int y1, unsigned pixels, void *object, unsigned color);
void dib_rectangle (dib_t *dib, int x, int y, unsigned width, unsigned height, unsigned pixels, void *object, unsigned color);

BOOL dib_copy (dib_t *source, dib_t *dest, int destx, int desty, unsigned width, unsigned height);
void dib_copy_metadata (dib_t *source, dib_t *dest);

LPBYTE dib_lock (dib_t *dib);
void dib_unlock (dib_t *dib);

void dib_snapshot(dib_t *dib, BOOL windowDCFlag);

#if 0
/* Used to control what part of the bitmap gets shown to the screen */
/* Helps with scrolling / oversized senarios */
void dib_setPaintOffset (dib_t *dib, int xOffset, int yOffset);

/* Surface info */
LPBYTE dib_getSurface (dib_t *dib);

int dib_getWidth (dib_t *dib);
int dib_getHeight (dib_t *dib);

/* Blitting interface  */
/* Make sure to: 1) Lock surface, 2) Blit, 3) Unlock surface */

BOOL dib_blit (dib_t *dib,
	       int destX, int destY,
	       int sourceX, int sourceY,
	       unsigned sourceWidth, unsigned sourceHeight);


#endif
#endif
#endif
