// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <gui.h>

#define Colormap X11Colormap
#include <tk.h>
#include <X11/Xlib.h>    // XColormap
#include <X11/Xutil.h>  // ZoomRaster
#ifndef __WIN32__
#include <X11/xpm.h>
#endif
#undef Colormap

#import <tkobjc/Raster.h>
@class Raster;

#import <tkobjc/Colormap.h>
@class Colormap;

Tk_Window tkobjc_nameToWindow (const char *widgetName);

void tkobjc_unlinkVar (const char *variableName);
void tkobjc_linkVar (const char *variableName, void *p, int type);
void tkobjc_initTclInterp (id arguments);
void tkobjc_initTkInterp (id arguments);
void tkobjc_createEventHandler (id widget, Tk_EventProc proc);
void tkobjc_deleteEventHandler (id widget, Tk_EventProc proc);

void tkobjc_raster_erase (Raster *raster);
void tkobjc_raster_clear (Raster *raster, unsigned oldWidth, unsigned oldHeight);
void tkobjc_raster_setColormap (Raster *raster);

void tkobjc_raster_fillRectangle (Raster *raster,
                                  int x, int y, 
                                  unsigned width, unsigned height, 
				  Color color);
void tkobjc_raster_ellipse (Raster *raster,
			    int x, int y,
			    unsigned width, unsigned height,
			    unsigned pixels,
			    Color color);

void tkobjc_raster_drawPoint (Raster *raster, int x, int y, Color color);
void tkobjc_raster_createContext (Raster *raster);
void tkobjc_raster_createPixmap (Raster *raster);
void tkobjc_raster_setBackground (Raster *raster, PixelValue c);
void tkobjc_raster_copy (Raster *raster, Pixmap opm, unsigned ow, unsigned oh);
void tkobjc_raster_flush (Raster *raster);
