// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "internal.h"
#import "global.h"

#include <tk.h>

#ifdef _WIN32
// Get a definition of timeval, but avoid redefinition of timezone.
#define timezone timezone__
#include <sys/time.h>
#undef timezone
// Undo X11 definition
#undef Status
#include "win32dib.h"
#define BOOL BOOL_
#include "tkInt.h"
#undef BOOL

#define Arguments Win32Arguments
#define Colormap X11Colormap
#include "tkWinInt.h"
#undef Colormap
#undef Arguments
#endif

#import <tkobjc/TkExtra.h>
#import <objectbase/Arguments.h>
#include <misc.h>

extern TkExtra *globalTkInterp;

Tk_Window 
tkobjc_nameToWindow (const char *widgetName)
{
  return Tk_NameToWindow ([globalTkInterp interp],
                          (char *)widgetName,
                          [globalTkInterp mainWindow]);
}

void
tkobjc_unlinkVar (const char *variableName)
{
  Tcl_UnlinkVar ([globalTkInterp interp], (char *)variableName); 
}

void
tkobjc_linkVar (const char *variableName, void *p, int type)
{
  Tcl_LinkVar ([globalTkInterp interp], (char *)variableName, (char *)p, type);
}

static void
registerInterp (void)
{
  [globalTkInterp registerObject: globalTkInterp withName: "globalTkInterp"];
}

static void
setSecondaryPath (id arguments)
{
  const char *swarmHome = [arguments getSwarmHome];
  
  if (swarmHome)
    {
      char *libraryPath = xmalloc (strlen (swarmHome) + 5);
      char *p;
      
      p = stpcpy (libraryPath, swarmHome);
      strcpy (p, "/lib");
      [globalTkInterp setSecondaryLibraryPath: libraryPath];
    }
}

void
tkobjc_initTclInterp (id arguments)
{
  globalTkInterp = [TclInterp alloc];  // misnomer
  setSecondaryPath (arguments);
  [globalTkInterp initWithArgc: [arguments getArgc]
                  argv: [arguments getArgv]];
  registerInterp ();
}

void
tkobjc_initTkInterp (id arguments)
{
  globalTkInterp = [TkExtra alloc];
  setSecondaryPath (arguments);

  [globalTkInterp initWithArgc: [arguments getArgc]
                  argv: [arguments getArgv]];
  registerInterp ();
}

void
tkobjc_createEventHandler (id widget, Tk_EventProc proc)
{
  Tk_Window tkwin = tkobjc_nameToWindow ([widget getWidgetName]);

  Tk_CreateEventHandler (tkwin, StructureNotifyMask, proc, widget);
}

void
tkobjc_deleteEventHandler (id widget, Tk_EventProc proc)
{
  Tk_Window tkwin = tkobjc_nameToWindow ([widget getWidgetName]);

  Tk_DeleteEventHandler (tkwin, StructureNotifyMask, proc, widget);
}


static void
Xfill (Display *display, GC gc, X11Pixmap pm,
       int x, int y,
       unsigned width, unsigned height,
       PixelValue pixel)
{
#if 0
  XGCValues oldgcv;
  
  XGetGCValues (display, gc, GCForeground, &oldgcv);  // save old colour
#endif
  XSetForeground (display, gc, pixel); // no checking on map.
  XFillRectangle (display, pm, gc, x, y, width, height);
  
#if 0
  XChangeGC (display, gc, GCForeground, &oldgcv);
#endif
}

void
tkobjc_raster_erase (Raster *raster)
{
#ifndef _WIN32
  Display *display = Tk_Display (raster->tkwin);
  
  Xfill (display, raster->gc, raster->pm,
         0, 0, raster->width, raster->height,
         BlackPixel (display, DefaultScreen (display)));
#else
  Colormap *colormap = raster->colormap;

  if (colormap)
    {
      if (raster->eraseColor == -1)
	{
	  raster->eraseColor = [colormap nextFreeColor];

	  [colormap setColor: raster->eraseColor ToName: "black"];
	}
      [raster fillRectangleX0: 0 Y0: 0
	      X1: [raster getWidth] Y1: [raster getHeight]
	      Color: raster->eraseColor];
    }
#endif
}

void
tkobjc_raster_fillRectangle (Raster *raster,
                             int x, int y,
                             unsigned width, unsigned height,
			     Color color)
{
#ifndef _WIN32
  PixelValue *map = ((Colormap *)raster->colormap)->map;
  
  Xfill (Tk_Display (raster->tkwin), raster->gc, raster->pm,
         x, y, width, height,
         map[color]);
#else
  dib_fill ((dib_t *)raster->pm, x, y, width, height, color);
#endif
}

void
tkobjc_raster_ellipse (Raster *raster,
		       int x, int y,
		       unsigned width, unsigned height,
		       unsigned pixels,
		       Color color)
{
#ifndef _WIN32
  PixelValue *map = ((Colormap *)raster->colormap)->map;
  Display *display = Tk_Display (raster->tkwin);
  GC gc = raster->gc;

  XSetForeground (display, gc, map[color]);
  XDrawArc (display, raster->pm, gc, x, y, width, height, 0, 23040);
#else
  dib_ellipse ((dib_t *)raster->pm, x, y, width, height, pixels, color);
#endif
}

void
tkobjc_raster_line (Raster *raster,
                    int x0, int y0,
                    int x1, int y1,
                    unsigned pixels,
                    Color color)
{
#ifndef _WIN32
  PixelValue *map = ((Colormap *)raster->colormap)->map;
  Display *display = Tk_Display (raster->tkwin);
  GC gc = raster->gc;

  XSetForeground (display, gc, map[color]);
  XSetLineAttributes (display, gc, pixels, LineSolid, CapButt, JoinRound);
  XDrawLine (display, raster->pm, gc, x0, y0, x1, y1);
#else
  dib_line ((dib_t *)raster->pm, x0, y0, x1, y1, pixels, color);
#endif
}

void
tkobjc_raster_drawPoint (Raster *raster, int x, int y, Color c)
{
  PixelValue *map = ((Colormap *)raster->colormap)->map;
#ifndef _WIN32
  X11Pixmap pm = raster->pm;
  Display *display = Tk_Display (raster->tkwin);
  GC gc = raster->gc;

  XSetForeground (display, gc, map[c]);    // no checking on map.

  XDrawPoint (display, pm, gc, x, y);
#else
  dib_t *dib = (dib_t *)raster->pm;
  int frameWidth = dib->dibInfo->bmiHead.biWidth;

  ((BYTE *)dib->bits)[x + y * frameWidth] = map[c];
#endif
}

void
tkobjc_raster_createContext (Raster *raster)
{
  XGCValues gcv;
  Display *display = Tk_Display (raster->tkwin);
  Window xwin = Tk_WindowId (raster->tkwin);
  GC gc;

  // now create a GC
  gcv.background = BlackPixel (display, DefaultScreen (display));
  gcv.graphics_exposures = False;
  gc = XCreateGC (display, xwin, GCBackground | GCGraphicsExposures, &gcv);
  XSetFillStyle (display, gc, FillSolid);
  XSetClipOrigin (display, gc, 0, 0);
  raster->gc = gc;
}

void
tkobjc_raster_setColormap (Raster *raster)
{
#ifdef _WIN32
  Colormap *colormap = raster->colormap;
  dib_t *dib = (dib_t *)raster->pm;
  
  if (colormap)
    dib_augmentPalette (dib,
			raster,
			[colormap nextFreeColor],
			colormap->map);
#endif
}

void
tkobjc_raster_createPixmap (Raster *raster)
{
  Tk_Window tkwin = raster->tkwin;
#ifndef _WIN32
  // and create a local Pixmap for drawing
  raster->pm = XCreatePixmap (Tk_Display (tkwin),
                              Tk_WindowId (tkwin),
                              raster->width,
                              raster->height,
                              Tk_Depth (tkwin));
#else
  dib_t *dib = dib_create ();
  
  raster->pm = dib;

  dib_createBitmap (dib, TkWinGetHWND (Tk_WindowId (tkwin)),
		    raster->width, raster->height);
  tkobjc_raster_setColormap (raster);
#endif
}

void
tkobjc_raster_setBackground (Raster *raster, PixelValue color)
{
  XGCValues gcv;

  gcv.background = color;
  XChangeGC (Tk_Display (raster->tkwin), raster->gc, GCBackground, &gcv);
}

void
tkobjc_raster_flush (Raster *raster)
{
  Tk_Window tkwin = raster->tkwin;
#ifndef _WIN32
  Display *display = Tk_Display (tkwin);

  XCopyArea (display, raster->pm, Tk_WindowId (tkwin), raster->gc,
             0, 0, raster->width, raster->height, 0, 0);
  XFlush (display); 
#else
  TkWinDrawable *twdPtr = (TkWinDrawable *)Tk_WindowId (tkwin);
  dib_t *dib = (dib_t *)raster->pm;
  
  dib_paintBlit (dib, GetDC (twdPtr->window.handle),
		 0, 0, 0, 0, raster->width, raster->height);
#endif
}

void
tkobjc_raster_clear (Raster *raster, unsigned oldWidth, unsigned oldHeight)
{
#ifdef _WIN32
  Tk_Window tkwin = raster->tkwin;
  Display *display = Tk_Display (tkwin);
  Window w = Tk_WindowId (tkwin);
  HPALETTE oldPalette, palette;
  HBRUSH brush;
  HWND hwnd = TkWinGetHWND (w);
  RECT rc;
  HDC dc = GetDC (hwnd);
  TkWindow *winPtr;
  unsigned newWidth;
  unsigned newHeight;

  palette = TkWinGetPalette (display->screens[0].cmap);
  oldPalette = SelectPalette (dc, palette, FALSE);

  winPtr = TkWinGetWinPtr (w);
  brush = CreateSolidBrush (winPtr->atts.background_pixel);
  GetWindowRect (hwnd, &rc);
  newWidth = rc.right - rc.left;
  newHeight = rc.bottom - rc.top;

  if (newWidth > oldWidth)
    {
      rc.right = newWidth;
      rc.left = oldWidth;
      rc.bottom = rc.bottom - rc.top;
      rc.top = 0;
      FillRect (dc, &rc, brush);
    }
  if (newHeight > oldHeight)
    {
      rc.right = oldWidth;
      rc.left = 0;
      rc.bottom = newHeight;
      rc.top = oldHeight;
      FillRect (dc, &rc, brush);
    }
  DeleteObject (brush);
  SelectPalette (dc, oldPalette, TRUE);
  ReleaseDC (hwnd, dc);
#endif
}

void
tkobjc_raster_savePixmap (Raster *raster)
{
  raster->oldpm = raster->pm;
}

void
tkobjc_raster_copy (Raster *raster, unsigned oldWidth, unsigned oldHeight)
{
  unsigned minWidth = raster->width < oldWidth ? raster->width : oldWidth;
  unsigned minHeight = raster->height < oldHeight ? raster->height : oldHeight;
#ifndef _WIN32
  Display *display = Tk_Display (raster->tkwin);

  XCopyArea (display, raster->oldpm, raster->pm, raster->gc,
             0, 0, minWidth, minHeight, 0, 0);
  XFreePixmap (display, raster->oldpm);
#else
  if (!dib_copy ((dib_t *)raster->oldpm, (dib_t *)raster->pm,
                 0, 0,
                 minWidth, minHeight))
    abort ();
#endif
}

#ifdef _WIN32
static unsigned
offset_for_object (dib_t *raster_dib, void *object)
{
  unsigned i;
  
  for (i = 0; i < raster_dib->colorMapBlocks; i++)
    if (raster_dib->colorMapObjects[i] == object)
      return raster_dib->colorMapOffsets[i];
  abort ();
}
#endif

void
tkobjc_pixmap_create (Pixmap *pixmap,
                      png_bytep *row_pointers,
                      unsigned bit_depth,
                      png_colorp palette, unsigned palette_size,
		      Raster *raster)
{
#ifndef _WIN32
  XpmColor *colors = xmalloc (sizeof (XpmColor) * palette_size);
  XpmImage image;
  
  image.width = pixmap->width;
  image.height = pixmap->height;
  image.cpp = 7;
  {
    int i;
    
    for (i = 0; i < palette_size; i++)
      {
        XpmColor *color = &colors[i];
        char *str = xmalloc (1 + 2 * 3 + 1);
        
        sprintf (str, "#%02x%02x%02x",
                 palette[i].red, palette[i].green, palette[i].blue);
        color->string = NULL;
        color->symbolic = str;
        color->m_color = NULL;
        color->g4_color = NULL;
        color->g_color = NULL;
        color->c_color = str;
      }
    image.ncolors = palette_size;
    image.colorTable = colors;
  }
  {
    unsigned *data = xmalloc (sizeof (int) * image.width * image.height);
    unsigned *out_pos = data;
    unsigned ri;
    
    
    for (ri = 0; ri < image.height; ri++)
      {
        unsigned ci;
        png_bytep in_row = row_pointers[ri];
        
        for (ci = 0; ci < image.width; ci++)
          {
            int bit_pos = bit_depth * ci;
            int byte_offset = bit_pos >> 3;
            int bit_shift = bit_pos & 0x7;
            int bit_mask = ((1 << bit_depth) - 1);
            
            *out_pos++ = (in_row[byte_offset] >> bit_shift) & bit_mask;
          }
      }
    image.data = data;
  }
  {
    XpmAttributes xpmattrs;
    Tk_Window tkwin = tkobjc_nameToWindow (".");
    Display *display = Tk_Display (tkwin);
    int rc;
    
    xpmattrs.valuemask = 0;
    
    rc = XpmCreatePixmapFromXpmImage (display,
                                      XDefaultRootWindow (display),
                                      &image,
                                      &pixmap->pixmap,
                                      &pixmap->mask,
                                      &xpmattrs);
    if (rc != 0)
      {
        const char *error = NULL;
        const char *warning = NULL;
        
        switch (rc)
          {
          case XpmSuccess:
            break;
          case XpmColorError:
            warning = "Could not parse or alloc requested color";
            break;
          case XpmOpenFailed:
            error = "Cannot open file";
            break;
          case XpmFileInvalid:
            error = "Invalid XPM file";
            break;
          case XpmNoMemory:
            error = "Not enough memory";
            break;
          case XpmColorFailed:
            error = "Failed to parse or alloc some color";
            break;
          }
        if (warning)
          [Warning raiseEvent: "Creating pixmap: %s\n", warning];
        if (error)
          [WindowCreation raiseEvent: "Creating pixmap: %s\n", error];
      }
  }
#else
  {
    dib_t *dib = dib_create ();

    dib_createBitmap (dib, NULL, pixmap->width, pixmap->height);
    {
      unsigned long map[palette_size];
      int i;
      
      for (i = 0; i < palette_size; i++)
        map[i] = palette[i].red
          | (palette[i].green << 8)
          | (palette[i].blue << 16);
      
      {
        // (Apparently, there isn't a need to merge the pixmap and raster
        // pixmaps by hand, but without doing so, somehow the black
        // gets lost. -mgd)
        dib_t *raster_dib = raster->pm;

        dib_augmentPalette (raster_dib, pixmap, palette_size, map);
      }

      dib_augmentPalette (dib, pixmap, palette_size, map);
      {
        unsigned ri;
        BYTE *out_pos = dib_lock (dib);
        
        for (ri = 0; ri < pixmap->height; ri++)
          {
            unsigned ci;
            png_bytep in_row = row_pointers[ri];
            
            for (ci = 0; ci < pixmap->width; ci++)
              {
                int bit_pos = bit_depth * ci;
                int byte_offset = bit_pos >> 3;
                int bit_shift = bit_pos & 0x7;
                int bit_mask = ((1 << bit_depth) - 1);
		BYTE val = ((in_row[byte_offset] >> bit_shift) & bit_mask);
                
                *out_pos++ = val;
                
              }
          }
        dib_unlock (dib);
      }
    }
    pixmap->pixmap = dib;
  }
#endif
}

void
tkobjc_pixmap_draw (Pixmap *pixmap, int x, int y, Raster *raster)
{
#ifndef _WIN32
  Tk_Window tkwin = tkobjc_nameToWindow (".");
  Display *display = Tk_Display (tkwin);
  GC gc = raster->gc;
  Drawable w = raster->pm;
  X11Pixmap mask = pixmap->mask;
  
  if (mask == 0)
    XCopyArea (display, pixmap->pixmap, w, gc, 0, 0,
               pixmap->width, pixmap->height,
               x, y);
  else
    {
      XSetClipMask (display, gc, mask);
      XCopyArea (display, pixmap->pixmap, w, gc, 0, 0,
                 pixmap->width, pixmap->height,
                 x, y);
      XSetClipMask (display, gc, None);
    }
#else
  dib_copy (pixmap->pixmap, raster->pm, x, y, pixmap->width, pixmap->height);
#endif
}

