// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <gui.h>

#define Colormap X11Colormap
#define Pixmap X11Pixmap
#define String X11String
#define Widget X11Widget
#define Object X11Object

#include <tk.h>
#include <X11/Xlib.h>    // XColormap
#include <X11/Xutil.h>  // ZoomRaster

#ifndef _WIN32
#include <swarmconfig.h>
#ifdef HAVE_XPM_H
#include <xpm.h>
#elif defined(HAVE_X11_XPM_H)
#include <X11/xpm.h>
#else
#error Need xpm.h or X11/xpm.h.
#endif
#endif

#undef Pixmap
#undef Colormap
#undef String
#undef Widget
#undef Object

#include <misc.h> // header problems might occur unprotected in png.h
#include <png.h>

#import <tkobjc/Raster.h>
@class Raster;

#import <tkobjc/Colormap.h>
@class Colormap;

#import <tkobjc/Pixmap.h>
@class Pixmap;

Tk_Window tkobjc_nameToWindow (const char *widgetName);

void tkobjc_setName (id widget, const char *name);

void tkobjc_unlinkVar (const char *variableName);
void tkobjc_linkVar (const char *variableName, void *p, int type);
void tkobjc_initTclInterp (id arguments);
void tkobjc_initTkInterp (id arguments);
void tkobjc_createEventHandler (id widget, Tk_EventProc proc);
void tkobjc_deleteEventHandler (id widget, Tk_EventProc proc);

void tkobjc_animate_message (id srcWidget, id destWidget, int sx, int sy, int dx, int dy, BOOL triggerFlag, unsigned sleepTime);
void tkobjc_move (id toplevel, int x, int y);

BOOL tkobjc_setColor (Colormap *colormap, const char *colorName, PixelValue *pvptr);
void tkobjc_raster_create (Raster *raster);
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
void tkobjc_raster_line (Raster *raster,
                         int x0, int y0,
                         int x1, int y1,
                         unsigned pixels,
                         Color color);
void tkobjc_raster_rectangle (Raster *raster,
                               int x0, int y0,
                               unsigned width, unsigned height,
                               unsigned pixels,
                               Color color);
void tkobjc_raster_drawPoint (Raster *raster, int x, int y, Color color);
void tkobjc_raster_createContext (Raster *raster);
void tkobjc_raster_setBackground (Raster *raster, PixelValue c);
void tkobjc_raster_createPixmap (Raster *raster);
void tkobjc_raster_savePixmap (Raster *raster);
void tkobjc_raster_dropOldPixmap (Raster *raster);
void tkobjc_raster_copy (Raster *raster, unsigned ow, unsigned oh);
void tkobjc_raster_flush (Raster *raster);
void tkobjc_pixmap_create (Pixmap *pixmap, png_bytep *row_pointers, 
                           unsigned bit_depth);
void tkobjc_pixmap_update_raster (Pixmap *pixmap, Raster *raster);
void tkobjc_pixmap_draw (Pixmap *pixmap, int x, int y, Raster *raster);
void tkobjc_pixmap_create_from_widget (Pixmap *pixmap, id <Widget> widget, BOOL parentFlag);
void tkobjc_pixmap_save (Pixmap *pixmap, const char *filename);
void tkobjc_pixmap_drop (Pixmap *pixmap);
