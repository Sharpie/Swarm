// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "internal.h"

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

#include <stdlib.h> // getenv

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
setSecondaryPath (void)
{
  const char *swarmHome = getenv ("SWARMHOME");

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
  setSecondaryPath ();
  [globalTkInterp initWithArgc: [arguments getArgc]
                  argv: [arguments getArgv]];
  registerInterp ();
}

void
tkobjc_initTkInterp (id arguments)
{
  globalTkInterp = [TkExtra alloc];
  setSecondaryPath ();
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
Xfill (Display *display, GC gc, Pixmap pm,
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
  id colorMap = raster->colormap;

  if (![colorMap colorIsSet: 255])
    [colorMap setColor: 255 ToName: "black"];
  tkobjc_raster_fillRectangle (raster, 0, 0, 
                               raster->width, raster->height,
                               255);
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
tkobjc_raster_drawPoint (Raster *raster, int x, int y, Color c)
{
  Pixmap pm = raster->pm;
  PixelValue *map = ((Colormap *)raster->colormap)->map;
#ifndef _WIN32
  Display *display = Tk_Display (raster->tkwin);
  GC gc = raster->gc;

  XSetForeground (display, gc, map[c]);    // no checking on map.

  XDrawPoint (display, pm, gc, x, y);
#else
  dib_t *dib = (dib_t *)pm;
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
    {
      TkWinColormap *wColormap = (TkWinColormap *)colormap->cmap;
      dib_setPalette (dib, wColormap, colormap->map);
    }
  else
    {
      Display *display = Tk_Display (raster->tkwin);      
      TkWinColormap *wColormap =
	(TkWinColormap *)DefaultColormap (display, DefaultScreen (display));
      
      dib_setPalette (dib, wColormap, NULL);
    }
  dib_realizePalette (dib);
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
  
  raster->pm = (Pixmap)dib;
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
tkobjc_raster_copy (Raster *raster, Pixmap oldpm,
                    unsigned oldWidth, unsigned oldHeight)
{
  unsigned minWidth = raster->width < oldWidth ? raster->width : oldWidth;
  unsigned minHeight = raster->height < oldHeight ? raster->height : oldHeight;
#ifndef _WIN32
  Display *display = Tk_Display (raster->tkwin);

  XCopyArea (display, oldpm, raster->pm, raster->gc,
             0, 0, minWidth, minHeight, 0, 0);
  XFreePixmap (display, oldpm);
#else
  if (!dib_copy ((dib_t *)oldpm, (dib_t *)raster->pm, minWidth, minHeight))
    abort ();
#endif
}

