// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// TopLevels and Frames are about the same, so we put these in one class.

#import <tkobjc/Frame.h>
#import <tkobjc/global.h>
#import <tkobjc/common.h>

@implementation Frame

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
  return self;
}

- assertPosition
{
  [globalTkInterp eval: "%s create window 0 0 -anchor nw -window %s",
                  [[self getParent] getWidgetName],
                  [self getWidgetName]];
  return self;
}

- assertGeometry
{
  id canvas = [self getParent];
  const char *canvasName = [canvas getWidgetName];
  
  [globalTkInterp eval:
                    "tkwait visibility %s ;"
                  "set width [winfo width %s] ;"
                  "set height [winfo height %s] ;"
                  "%s configure -scrollregion [list 0 0 $width $height] ;"
                  "if {$height > 500} {set height 500} ;"
                  "if {$width > 809} {set width 809} ;"
                  "%s configure -width $width -height $height",
                  widgetName, widgetName, widgetName,
                  canvasName, canvasName];
  return self;
}

- withdraw
{
  [globalTkInterp eval: "wm withdraw %s", [self getWidgetName]];
  return self;
}

- deiconify
{
  [globalTkInterp eval: "wm deiconify %s", [self getWidgetName]];
  return self;
}

- (void)drop
{
  void archiverUnregister (id client);
  
  archiverUnregister (self);

  if (parent == nil && !destroyedFlag)
    [globalTkInterp eval: "destroy %s", [self getWidgetName]]; 
}

@end

