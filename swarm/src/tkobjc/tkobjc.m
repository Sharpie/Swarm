// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <defobj.h>

#include "internal.h"
#import <gui.h>
#import <tkobjc/global.h>
#import <defobj.h> // Arguments

externvardef id <TkExtra> globalTkInterp;

#import "simtools_tcl.x"
#import "analysis_tcl.x"

id <Error> WindowCreation, WindowUsage, MissingFiles,
  PaletteError, PixmapError,
  WindowGeometryRecordError;

void
initTkObjc (id arguments)
{
  if ([arguments getBatchModeFlag])
    tkobjc_initTclInterp (arguments);
  else
    {
      deferror (WindowCreation, NULL);
      deferror (WindowUsage, NULL);
      deferror (MissingFiles, NULL);
      deferror (PaletteError, NULL);
      deferror (PixmapError, NULL);
      deferror (WindowGeometryRecordError, NULL);
      
      tkobjc_initTkInterp (arguments);

      [globalTkInterp eval: "proc fmtx {sigfigures widget val} {\n"
                      "format \"%%.${sigfigures}g\" $val\n"
                      "}\n"];
      [globalTkInterp eval: simtools_tcl];

      if ([globalTkInterp newBLTp])
        [globalTkInterp eval: "uplevel #0 {"
                        "set hideOption -hide\n"
                        "set hideYes yes\n"
                        "set hideNo no\n"
                        "}\n"];
      else
        [globalTkInterp eval: "uplevel #0 {"
                        "set hideOption -mapped\n"
                        "set hideYes 0\n"
                        "set hideNo 1\n"
                      "}\n"];

      [globalTkInterp eval: analysis_tcl];
      [globalTkInterp eval: "proc send {id args} { }"];
    }
}

