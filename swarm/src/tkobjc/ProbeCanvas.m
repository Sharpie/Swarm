// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

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

@end
