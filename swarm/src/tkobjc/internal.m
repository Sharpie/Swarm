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
#include "tk/tkInt.h"
#undef BOOL

#define Arguments Win32Arguments
#define Colormap X11Colormap
#include "tk/tkWinInt.h"
#undef Colormap
#undef Arguments
#endif

#import <tkobjc/TkExtra.h>
#import <objectbase/Arguments.h>
#import <tkobjc/Pixmap.h> // PixmapError

#include <misc.h>

typedef struct raster_private {
  GC gc;
  Tk_Window tkwin;
#ifndef _WIN32
  X11Pixmap pm;
  X11Pixmap oldpm;
#else
  dib_t *oldpm;
  dib_t *pm;
#endif
} raster_private_t;

extern TkExtra *globalTkInterp;

Tk_Window 
tkobjc_nameToWindow (const char *widgetName)
{
  return Tk_NameToWindow ([globalTkInterp interp],
                          (char *)widgetName,
                          [globalTkInterp mainWindow]);
}

void
tkobjc_setName (id widget, const char *name)
{
  const char *prefix = "Swarm";
  const char *appName = [arguments getAppName];
  const char *appModeString = [arguments getAppModeString];
  char buf[(strlen (prefix) + 1 + strlen (appName) + 1 +
            strlen (appModeString) + 1)];
  
  Tk_Window tkwin = tkobjc_nameToWindow ([[widget getTopLevel] getWidgetName]);
  
  if (name)
    Tk_Name (tkwin) = (char *)name;
  stpcpy (stpcpy (stpcpy
                  (stpcpy (stpcpy (buf, prefix), "-"),
                   appName), "-"),
          appModeString);

  Tk_SetClass (tkwin, buf);
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
tkobjc_raster_create (Raster *raster)
{
  Tk_Window tkwin = tkobjc_nameToWindow ([raster getWidgetName]);

  if (tkwin == NULL)
    [WindowCreation raiseEvent: "Error creating tkwin!\n%s",
                    [globalTkInterp result]];
  else
    {
      raster_private_t *private = xmalloc (sizeof (raster_private_t));

      Tk_MakeWindowExist (tkwin);
      private->tkwin = tkwin;
      raster->private = private;
    }
}

void
tkobjc_raster_erase (Raster *raster)
{
  raster_private_t *private = raster->private;
#ifndef _WIN32
  Display *display = Tk_Display (private->tkwin);
  
  Xfill (display, private->gc, private->pm,
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
  raster_private_t *private = raster->private;

#ifndef _WIN32
  PixelValue *map = ((Colormap *)raster->colormap)->map;
  
  Xfill (Tk_Display (private->tkwin), private->gc, private->pm,
         x, y, width, height,
         map[color]);
#else
  dib_fill (private->pm, x, y, width, height, color);
#endif
}

void
tkobjc_raster_ellipse (Raster *raster,
		       int x, int y,
		       unsigned width, unsigned height,
		       unsigned pixels,
		       Color color)
{
  raster_private_t *private = raster->private;
#ifndef _WIN32
  PixelValue *map = ((Colormap *)raster->colormap)->map;
  Display *display = Tk_Display (private->tkwin);
  GC gc = private->gc;

  XSetForeground (display, gc, map[color]);
  XDrawArc (display, private->pm, gc, x, y, width, height, 0, 23040);
#else
  dib_ellipse (private->pm, x, y, width, height, pixels, color);
#endif
}

void
tkobjc_raster_line (Raster *raster,
                    int x0, int y0,
                    int x1, int y1,
                    unsigned pixels,
                    Color color)
{
  raster_private_t *private = raster->private;
#ifndef _WIN32
  PixelValue *map = ((Colormap *)raster->colormap)->map;
  Display *display = Tk_Display (private->tkwin);
  GC gc = private->gc;

  XSetForeground (display, gc, map[color]);
  XSetLineAttributes (display, gc, pixels, LineSolid, CapButt, JoinRound);
  XDrawLine (display, private->pm, gc, x0, y0, x1, y1);
#else
  dib_line (private->pm, x0, y0, x1, y1, pixels, color);
#endif
}

void
tkobjc_raster_rectangle (Raster *raster,
                         int x, int y,
                         unsigned width, unsigned height,
                         unsigned pixels,
                         Color color)
{
  raster_private_t *private = raster->private;
#ifndef _WIN32
  PixelValue *map = ((Colormap *)raster->colormap)->map;
  Display *display = Tk_Display (private->tkwin);
  GC gc = private->gc;

  XSetForeground (display, gc, map[color]);
  XSetLineAttributes (display, gc, pixels, LineSolid, CapButt, JoinRound);
  XDrawRectangle (display, private->pm, gc, x, y, width, height);
#else
  dib_rectangle (private->pm, x, y, width, height, pixels, color);
#endif
}

void
tkobjc_raster_drawPoint (Raster *raster, int x, int y, Color c)
{
  raster_private_t *private = raster->private;
#ifndef _WIN32
  PixelValue *map = ((Colormap *)raster->colormap)->map;
  X11Pixmap pm = private->pm;
  Display *display = Tk_Display (private->tkwin);
  GC gc = private->gc;

  XSetForeground (display, gc, map[c]);    // no checking on map.

  XDrawPoint (display, pm, gc, x, y);
#else
  dib_t *dib = private->pm;
  int frameWidth = dib->dibInfo->bmiHead.biWidth;
  int frameHeight = (dib->dibInfo->bmiHead.biHeight < 0
		     ? -dib->dibInfo->bmiHead.biHeight
		     : dib->dibInfo->bmiHead.biHeight);

  if (x >= 0 && x < frameWidth
      && y >= 0 && y < frameHeight)
    ((BYTE *)dib->bits)[x + y * frameWidth] = c;
#endif
}

void
tkobjc_raster_createContext (Raster *raster)
{
  raster_private_t *private = raster->private;
  XGCValues gcv;
  Display *display = Tk_Display (private->tkwin);
  Window xwin = Tk_WindowId (private->tkwin);
  GC gc;

  // now create a GC
  gcv.background = BlackPixel (display, DefaultScreen (display));
  gcv.graphics_exposures = False;
  gc = XCreateGC (display, xwin, GCBackground | GCGraphicsExposures, &gcv);
  XSetFillStyle (display, gc, FillSolid);
  XSetClipOrigin (display, gc, 0, 0);
  private->gc = gc;
}

void
tkobjc_raster_setColormap (Raster *raster)
{
  raster_private_t *private = raster->private;
#ifdef _WIN32
  Colormap *colormap = raster->colormap;
  dib_t *dib = private->pm;
  
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
  raster_private_t *private = raster->private;
  Tk_Window tkwin = private->tkwin;
#ifndef _WIN32
  // and create a local Pixmap for drawing
  private->pm = XCreatePixmap (Tk_Display (tkwin),
                              Tk_WindowId (tkwin),
                              raster->width,
                              raster->height,
                              Tk_Depth (tkwin));
#else
  dib_t *dib = dib_create ();
  
  private->pm = dib;

  dib_createBitmap (dib, TkWinGetHWND (Tk_WindowId (tkwin)),
		    raster->width, raster->height);
  tkobjc_raster_setColormap (raster);
#endif
}

void
tkobjc_raster_setBackground (Raster *raster, PixelValue color)
{
  XGCValues gcv;
  raster_private_t *private = raster->private;

  gcv.background = color;
  XChangeGC (Tk_Display (private->tkwin), private->gc, GCBackground, &gcv);
}

void
tkobjc_raster_flush (Raster *raster)
{
  raster_private_t *private = raster->private;
  Tk_Window tkwin = private->tkwin;
#ifndef _WIN32
  Display *display = Tk_Display (tkwin);

  XCopyArea (display, private->pm, Tk_WindowId (tkwin), private->gc,
             0, 0, raster->width, raster->height, 0, 0);
  XFlush (display); 
#else
  TkWinDrawable *twdPtr = (TkWinDrawable *)Tk_WindowId (tkwin);
  dib_t *dib = (dib_t *)private->pm;
  
  dib_paintBlit (dib, GetDC (twdPtr->window.handle),
		 0, 0, 0, 0, raster->width, raster->height);
#endif
}

void
tkobjc_raster_clear (Raster *raster, unsigned oldWidth, unsigned oldHeight)
{
  raster_private_t *private = raster->private;
#ifdef _WIN32
  Tk_Window tkwin = private->tkwin;
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
  raster_private_t *private = raster->private;

  private->oldpm = private->pm;
}

void
tkobjc_raster_copy (Raster *raster, unsigned oldWidth, unsigned oldHeight)
{
  raster_private_t *private = raster->private;
  unsigned minWidth = raster->width < oldWidth ? raster->width : oldWidth;
  unsigned minHeight = raster->height < oldHeight ? raster->height : oldHeight;

#ifndef _WIN32
  Display *display = Tk_Display (private->tkwin);

  XCopyArea (display, private->oldpm, private->pm, private->gc,
             0, 0, minWidth, minHeight, 0, 0);
  XFreePixmap (display, private->oldpm);
#else
  if (!dib_copy (private->oldpm, private->pm,
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

#ifndef _WIN32
static void
xpmerrcheck (int xpmerr, const char *what)
{
  const char *error = NULL;
  const char *warning = NULL;
  
  switch (xpmerr)
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
    [Warning raiseEvent: "Creating pixmap: %s (%s)\n", warning, what];
  if (error)
    [PixmapError raiseEvent: "Creating pixmap: %s (%s)\n", error, what];
}
#endif

#ifndef _WIN32
static void
x_pixmap_create_from_window (Pixmap *pixmap, Window window)
{
  int x, y;
  unsigned w, h, bw, depth;
  XImage *ximage;
  Window root;
  
  if (!XGetGeometry (pixmap->display, window, &root,
                     &x, &y, &w, &h,
                     &bw, &depth))
    [PixmapError raiseEvent: "Cannot get geometry for root window"];
  ximage = XGetImage (pixmap->display, window, x, y, w, h, AllPlanes, ZPixmap);
  if (ximage == NULL)
    [PixmapError raiseEvent: "Cannot get XImage of window"];
  
  xpmerrcheck (XpmCreateXpmImageFromImage (pixmap->display, ximage, NULL,
                                           &pixmap->xpmimage, 
                                           NULL),
               "x_pixmap_create_from_window / XpmImage");
  
  xpmerrcheck (XpmCreatePixmapFromXpmImage (pixmap->display,
                                            window,
                                            &pixmap->xpmimage,
                                            &pixmap->pixmap,
                                            &pixmap->mask,
                                            NULL),
               "x_pixmap_create_from_window / Pixmap");
  XDestroyImage (ximage);
}
#else
static void
win32_pixmap_create_from_window (Pixmap *pixmap, HWND window)
{
  dib_t *dib = dib_create ();
  dib->window = window;
  pixmap->pixmap = dib;

  {
    RECT rect;

    GetWindowRect (dib->window, &rect);
    pixmap->height = rect.bottom - rect.top;
    pixmap->width = rect.right - rect.left;
  }
  dib_snapshot (dib);
}
#endif

static void
pixmap_create_from_root_window (Pixmap *pixmap)
{
#ifndef _WIN32
  Tk_Window tkwin = tkobjc_nameToWindow (".");
  Window root;
  
  pixmap->display = Tk_Display (tkwin);
  root = RootWindow (pixmap->display, DefaultScreen (pixmap->display));
  x_pixmap_create_from_window (pixmap, root);
#else
  win32_pixmap_create_from_window (pixmap, GetDesktopWindow ());
#endif
}

void
tkobjc_pixmap_create_from_widget (Pixmap *pixmap, id widget)
{

  if (widget == nil)
    pixmap_create_from_root_window (pixmap);
  else
    {
      Tk_Window tkwin = tkobjc_nameToWindow ([widget getWidgetName]);
      Window window = Tk_WindowId (tkwin);

#ifndef _WIN32
      pixmap->display = Tk_Display (tkwin);
      x_pixmap_create_from_window (pixmap, window);
#else
      win32_pixmap_create_from_window (pixmap, Tk_GetHWND (window));
#endif
    }
}

void
tkobjc_pixmap_create (Pixmap *pixmap,
                      png_bytep *row_pointers,
                      unsigned bit_depth,
                      png_colorp palette, unsigned palette_size,
		      Raster *raster)
{
  raster_private_t *private = raster->private;

#ifndef _WIN32
  XpmColor *colors = xmalloc (sizeof (XpmColor) * palette_size);
  
  pixmap->xpmimage.width = pixmap->width;
  pixmap->xpmimage.height = pixmap->height;
  pixmap->xpmimage.cpp = 7;
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
    pixmap->xpmimage.ncolors = palette_size;
    pixmap->xpmimage.colorTable = colors;
  }
  {
    unsigned *data = xmalloc (sizeof (int)
                              * pixmap->xpmimage.width
                              * pixmap->xpmimage.height);
    unsigned *out_pos = data;
    unsigned ri;
    
    for (ri = 0; ri < pixmap->xpmimage.height; ri++)
      {
        unsigned ci;
        png_bytep in_row = row_pointers[ri];
        
        for (ci = 0; ci < pixmap->xpmimage.width; ci++)
          {
            int bit_pos = bit_depth * ci;
            int byte_offset = bit_pos >> 3;
            int bit_shift = bit_pos & 0x7;
            int bit_mask = ((1 << bit_depth) - 1);
            
            *out_pos++ = (in_row[byte_offset] >> bit_shift) & bit_mask;
          }
      }
    pixmap->xpmimage.data = data;
  }
  {
    XpmAttributes xpmattrs;
    Tk_Window tkwin = tkobjc_nameToWindow (".");
    Display *display = Tk_Display (tkwin);
    int rc;
    
    xpmattrs.valuemask = 0;
    
    xpmerrcheck (XpmCreatePixmapFromXpmImage (display,
                                              XDefaultRootWindow (display),
                                              &pixmap->xpmimage,
                                              &pixmap->pixmap,
                                              &pixmap->mask,
                                              &xpmattrs),
                 "tkobjc_pixmap_create");
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
        dib_t *raster_dib = private->pm;

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
  raster_private_t *private = raster->private;
#ifndef _WIN32
  Tk_Window tkwin = tkobjc_nameToWindow (".");
  Display *display = Tk_Display (tkwin);
  GC gc = private->gc;
  Drawable w = private->pm;
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
  dib_copy (pixmap->pixmap, private->pm, x, y, pixmap->width, pixmap->height);
#endif
}

void
tkobjc_pixmap_save (Pixmap *pixmap, const char *filename)
{
  FILE *fp = fopen (filename, "wb");
  png_structp png_ptr;
  png_infop info_ptr;
  unsigned width = pixmap->width;
  unsigned height = pixmap->height;
#ifndef _WIN32
  unsigned ncolors = pixmap->xpmimage.ncolors;
#else
  dib_t *dib = pixmap->pixmap;
  unsigned ncolors = dib->colorMapSize;
#endif

  if (fp == NULL)
    [PixmapError raiseEvent: "Cannot open output pixmap file: %s\n", filename];
  
  png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (png_ptr == NULL)
    {
      fclose (fp);
      [PixmapError raiseEvent: "Could not create PNG write struct\n"];
    }
  
  info_ptr = png_create_info_struct (png_ptr);
  if (info_ptr == NULL)
    {
      png_destroy_write_struct (&png_ptr, (png_infopp)NULL);
      fclose (fp);
      [PixmapError raiseEvent: "Could not create PNG info struct\n"];
    }
  
  if (setjmp (png_ptr->jmpbuf))
    {
      png_destroy_write_struct (&png_ptr, &info_ptr);
      fclose (fp);
      [PixmapError raiseEvent: "Error during PNG write of %s\n", filename];
    }
  
  png_init_io (png_ptr, fp);

  png_set_IHDR (png_ptr, info_ptr,
		width, height,
                8, PNG_COLOR_TYPE_PALETTE,
                PNG_INTERLACE_NONE,
                PNG_COMPRESSION_TYPE_DEFAULT,
                PNG_FILTER_TYPE_DEFAULT);
  {
    png_color palette[ncolors];
    int i;
    
    for (i = 0; i < ncolors; i++)
      {
#ifndef _WIN32
        unsigned red, green, blue;

        sscanf (pixmap->xpmimage.colorTable[i].c_color, "#%4x%4x%4x", 
                &red, &green, &blue);
        palette[i].red = red >> 8;
        palette[i].green = green >> 8;
        palette[i].blue = blue >> 8;
#else
	palette[i].red = dib->dibInfo->rgb[i].rgbRed;
	palette[i].green = dib->dibInfo->rgb[i].rgbGreen;
	palette[i].blue = dib->dibInfo->rgb[i].rgbBlue;
#endif
      }
    png_set_PLTE (png_ptr, info_ptr, palette, ncolors);
    png_write_info (png_ptr, info_ptr);
  }
  {
    png_byte buf[height][width];
    png_bytep row_pointers[height];
#ifndef _WIN32
    unsigned *data = pixmap->xpmimage.data;
#else
    BYTE *data = dib->bits;
#endif
    int yi, xi;
    
    for (yi = 0; yi < height; yi++)
      for (xi = 0; xi < width; xi++)
        buf[yi][xi] = (png_byte)*data++;
    
    for (yi = 0; yi < height; yi++)
      row_pointers[yi] = &buf[yi][0];
    png_write_image (png_ptr, row_pointers);
  }
  png_write_end (png_ptr, info_ptr);
  png_destroy_write_struct (&png_ptr, &info_ptr);
  fclose (fp);
}


void
tkobjc_pixmap_drop (Pixmap *pixmap)
{
#ifndef _WIN32
  XFreePixmap (pixmap->display, pixmap->pixmap);
  if (pixmap->mask)
    XFreePixmap (pixmap->display, pixmap->mask);
  XpmFreeXpmImage (&pixmap->xpmimage);
#else
  dib_destroy (pixmap->pixmap);
#endif
}
