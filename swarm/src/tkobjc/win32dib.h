#ifndef _DIB_H
#define _DIB_H
#ifdef _WIN32
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

  unsigned colorMapOffsets[256];
  void *colorMapObjects[256];
  unsigned colorMapBlocks;
  unsigned colorMapSize;
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
void dib_augmentPalette (dib_t *dib, void *object,
			 unsigned mapSize, unsigned long *map);

/* Called from a WM_PAINT command -- No locking required. */
BOOL dib_paintBlit (dib_t *dib,
		    HDC destHdc,
		    int destX, int destY,
		    int sourceX, int sourceY,
		    unsigned sourceWidth, unsigned sourceHeight);

void dib_fill (dib_t *dib, int x, int y, unsigned width, unsigned height, unsigned char color);
void dib_ellipse (dib_t *dib, int x, int y, unsigned width, unsigned height, unsigned pixels, unsigned char color);
void dib_line (dib_t *dib, int x0, int y0, int x1, int y1, unsigned pixels, unsigned char color);
void dib_rectangle (dib_t *dib, int x, int y, unsigned width, unsigned height, unsigned pixels, unsigned char color);

BOOL dib_copy (dib_t *source, dib_t *dest, int destx, int desty, unsigned width, unsigned height);

BYTE* dib_lock (dib_t *dib);
void dib_unlock (dib_t *dib);

void dib_snapshot(dib_t *dib);

#if 0
/* Used to control what part of the bitmap gets shown to the screen */
/* Helps with scrolling / oversized senarios */
void dib_setPaintOffset (dib_t *dib, int xOffset, int yOffset);

/* Surface info */
BYTE *dib_getSurface (dib_t *dib);

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
