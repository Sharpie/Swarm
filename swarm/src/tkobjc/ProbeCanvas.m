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

#import <tkobjc/ProbeCanvas.h>
#import <tkobjc/global.h>

@implementation ProbeCanvas

PHASE(Creating)

- setHorizontalScrollbarFlag: (BOOL)theHorizontalScrollbarFlag
{
  horizontalScrollbarFlag = theHorizontalScrollbarFlag;
  return self;
}

- createEnd
{
  const char *canvasName, *frameName;

  [super createEnd];

  canvasName = [self getWidgetName];
  frameName = [parent getWidgetName];

  if (horizontalScrollbarFlag)
    [globalTkInterp
      eval: "%s configure "
      "-xscrollcommand {%s.xscroll set} "
      "-yscrollcommand {%s.yscroll set};"
      "scrollbar %s.xscroll -orient horizontal -command {%s xview} ;"
      "scrollbar %s.yscroll -orient vertical -command {%s yview} ;"
      "pack %s.xscroll -side bottom -fill x ;"
      "pack %s.yscroll -side right -fill y ;"
      "pack %s -side left -fill both -expand true ;"
      "pack %s -side top -fill both -expand true",
      canvasName,
      frameName, frameName,   // [x,y]scrollcommand
      frameName, canvasName,  // scrollbar x, xview
      frameName, canvasName,  // scrollbar y, yview
      frameName, frameName,   // pack xscroll, yscroll
      canvasName,             // pack canvas
      frameName               // pack frame
    ];
  else
    [globalTkInterp
      eval:
        "%s configure -width 10 -height 10 -yscrollcommand {%s.yscroll set} ;"
      "scrollbar %s.yscroll -orient vertical -command {%s yview} ;"
      "pack %s.yscroll -side right -fill y ;"
      "pack %s -side left -fill both -expand true",
      canvasName,
      frameName,
      frameName,
      canvasName,
      frameName,
      canvasName];
  
  return self;
}

PHASE(Using)

@end
