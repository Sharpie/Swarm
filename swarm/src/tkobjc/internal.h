// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <gui.h>

#import <tkobjc/Raster.h>
@class Raster;

#define Colormap X11Colormap
#include <tk.h>
#include <X11/Xlib.h>    // XColormap
#include <X11/Xutil.h>  // ZoomRaster
#import <X11/xpm.h>
#undef Colormap

Tk_Window tkobjc_nameToWindow (const char *widgetName);

void tkobjc_unlinkVar (const char *variableName);
void tkobjc_linkVar (const char *variableName, void *p, int type);
void tkobjc_initTclInterp (id arguments);
void tkobjc_initTkInterp (id arguments);
void tkobjc_createEventHandler (id widget, Tk_EventProc proc);
void tkobjc_deleteEventHandler (id widget, Tk_EventProc proc);

void tkobjc_raster_fillRectangle (Raster *raster,
                                  int x, int y, int width, int height, 
                                  PixelValue c);

void tkobjc_raster_drawPoint (Raster *raster, int x, int y, PixelValue c);
void tkobjc_raster_createContext (Raster *raster);
void tkobjc_raster_createPixmap (Raster *raster);
void tkobjc_raster_setBackground (Raster *raster, PixelValue c);
void tkobjc_raster_copy (Raster *raster, Pixmap oldpm, int w, int h);
void tkobjc_raster_flush (Raster *raster);
