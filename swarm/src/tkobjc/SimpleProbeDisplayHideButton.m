// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/SimpleProbeDisplayHideButton.h>

#import <tkobjc/control.h>
#import <tkobjc/global.h>

@implementation SimpleProbeDisplayHideButton

- setFrame: theFrame
{
  frame = theFrame;
  return self;
}

- setProbeDisplay: theProbeDisplay
{
  probeDisplay = theProbeDisplay;
  return self;
}

- createEnd
{
  [super createEnd];

  [globalTkInterp 
    eval: "%s configure -command {%s markForDrop}",
    [self getWidgetName],
    tclObjc_objectToName (probeDisplay)];
  
  tkobjc_configureSpecialBitmap (self);
  
  [globalTkInterp eval: "pack %s -side right -fill both -expand 0",
		  [self getWidgetName]];
  
  [globalTkInterp eval: "pack %s -before %s -side left -fill both -expand 0",
		  [frame getWidgetName],
		  [self getWidgetName]];
  return self;
}

@end

