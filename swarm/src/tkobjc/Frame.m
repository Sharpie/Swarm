// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// TopLevels and Frames are about the same, so we put these in one class.

#import <tkobjc/Frame.h>
#import <tkobjc/global.h>
#import <tkobjc/common.h>

@implementation Frame

PHASE(Creating)

- setBorderWidth: (unsigned)theBorderWidth
{
  borderWidth = borderWidth;
  return self;
}

- setReliefFlag: (BOOL)theReliefFlag
{
  reliefFlag = theReliefFlag;
  return self;
}

// make a new top level frame. Can't use Widget default createEnd, because
// this is where the toplevel is actually built.
- createEnd
{
  if (parent == nil)
    {
      [self setWidgetNameFromParentName: "."];
      [globalTkInterp eval: "toplevel %s; wm minsize %s 1 1",
                      widgetName, widgetName];
      [self registerAndLoad];
    }
  else
    {
      [super createEnd];
      tkobjc_makeFrame (self);
    }

  if (reliefFlag)
    [globalTkInterp eval: "%s configure -relief ridge -borderwidth 3",
                    widgetName];

  if (borderWidth > 0)
    [globalTkInterp eval: "%s configure -bd %d",
                    widgetName, borderWidth];

  return self;
}

PHASE(Using)

- (void)withdraw
{
  [globalTkInterp eval: "wm withdraw %s", [self getWidgetName]];
}

- (void)deiconify
{
  [globalTkInterp eval: "wm deiconify %s", [self getWidgetName]];
}

@end

