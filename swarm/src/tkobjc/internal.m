// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "internal.h"
#import <tkobjc/TkExtra.h>
#import <objectbase/Arguments.h>

#include <tk.h>
#define BOOL BOOL_
#include "tkInt.h"

#ifdef __WIN32__
#include "tkWinInt.h"
#endif

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

void
tkobjc_initTclInterp (id arguments)
{
  globalTkInterp = [TclInterp alloc];  // misnomer
  [globalTkInterp initWithArgc: [arguments getArgc]
                  argv: [arguments getArgv]];
  registerInterp ();
}

void
tkobjc_initTkInterp (id arguments)
{
  globalTkInterp = [TkExtra alloc];
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

void
tkobjc_raster_fillRectangle (Raster *raster,
                             int x, int y, int width, int height,
                             PixelValue color)
{
  Tk_Window tkwin = raster->tkwin;
  Display *display = Tk_Display (tkwin);
  Pixmap pm = raster->pm;
  GC gc = raster->gc;

#if 1
  {
#if 0
    XGCValues oldgcv;

    XGetGCValues (display, gc, GCForeground, &oldgcv);  // save old colour
#endif

    XSetForeground (display, gc, color);	        // no checking on map.
    XFillRectangle (display, pm, gc, x, y, width, height);

#if 0
    XChangeGC (display, gc, GCForeground, &oldgcv);
#endif
  }
#else
  {
    TkWinDrawable *twdPtr = (TkWinDrawable *)pm;
    
    TkWinFillRect (GetDC (twdPtr->window.handle), x, y, width, height, color);
  }
#endif
}

void
tkobjc_raster_drawPoint (Raster *raster, int x, int y, PixelValue c)
{
  Display *display = Tk_Display (raster->tkwin);
  Pixmap pm = raster->pm;
  GC gc = raster->gc;

  XSetForeground (display, gc, c);    // no checking on map.

  XDrawPoint (display, pm, gc, x, y);
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
tkobjc_raster_createPixmap (Raster *raster)
{
  Tk_Window tkwin = raster->tkwin;

  // and create a local Pixmap for drawing
  raster->pm = XCreatePixmap (Tk_Display (tkwin),
                              Tk_WindowId (tkwin),
                              raster->width,
                              raster->height,
                              Tk_Depth (tkwin));
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
  Display *display = Tk_Display (tkwin);

  XCopyArea (display, raster->pm, Tk_WindowId (tkwin), raster->gc,
             0, 0, raster->width, raster->height, 0, 0);
  XFlush (display);
}

void
tkobjc_raster_copy (Raster *raster, Pixmap oldpm, int oldWidth, int oldHeight)
{
  Display *display = Tk_Display (raster->tkwin);
  unsigned minWidth = raster->width < oldWidth ? raster->width : oldWidth;
  unsigned minHeight = raster->height < oldHeight ? raster->height : oldHeight;
  
  XCopyArea (display, oldpm, raster->pm, raster->gc,
             0, 0, minWidth, minHeight, 0, 0);
  XFreePixmap (display, oldpm);
}

