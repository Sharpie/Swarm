// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define Colormap__ Colormap
#undef Colormap
#define Colormap Colormap_
#include <tk.h>
#include <X11/Xutil.h>  // ZoomRaster
#import <X11/xpm.h>
#undef Colormap
#define Colormap Colormap__

Tk_Window tkobjc_nameToWindow (const char *widgetName);

void tkobjc_unlinkVar (const char *variableName);
void tkobjc_linkVar (const char *variableName, void *p, int type);
void tkobjc_initTclInterp (int argc, const char **argv);
void tkobjc_initTkInterp (int argc, const char **argv);
