// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/VarProbeEntry.h>

#import <tkobjc/common.h>
#import <tkobjc/global.h>

#import <objc/objc-api.h>

static void
tkobjc_bindReturnToSetValue (id widget, id self)
{
  const char *widgetName = [widget getWidgetName];
  
  [globalTkInterp
    eval:
      "bind %s <Return> {%s configure -highlightcolor red ;"
    "update ;"
    "%s setValue}",
    widgetName, widgetName,
    [self getObjectName]];
}

static void
tkobjc_bindButton3ToBeUnhelpful (id widget)
{
  const char *widgetName = [widget getWidgetName];

  [globalTkInterp
    eval:
      "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;"
    "update ;"
    "bell ; update ; "
    "%s configure -highlightcolor black ;"
    "update} ;",
    widgetName, widgetName, widgetName, widgetName];
}

static void
tkobjc_bindKeyReleaseReturnToResetColorAndUpdate (id widget)
{
  const char *widgetName = [widget getWidgetName];

  [globalTkInterp
    eval: "bind %s <KeyRelease-Return> {%s configure -highlightcolor black ;"
    "update };", 
    widgetName,
    widgetName];
}

static void
tkobjc_bindFocusOutToClearSelection (id widget)
{
  const char *widgetName = [widget getWidgetName];

  [globalTkInterp
    eval: "bind %s <FocusOut> {%s selection clear}",
    widgetName,
    widgetName];
}

static void
tkobjc_bindFocusInToSetSelection (id widget)
{
  const char *widgetName = [widget getWidgetName];

  [globalTkInterp
    eval: "bind %s <FocusIn> {%s selection range 0 end}",
    widgetName,
    widgetName];
}

@implementation VarProbeEntry

PHASE(Creating)

- setProbeType: (char)theProbeType
{
  probeType = theProbeType;
  return self;
}

- setInteractiveFlag: (BOOL)theInteractiveFlag
{
  interactiveFlag = theInteractiveFlag;
  return self;
}

- setOwner: theOwner
{
  owner = theOwner;
  return self;
}

- createEnd
{
  [super createEnd];

  if (interactiveFlag)
    {
      tkobjc_bindReturnToSetValue (self, owner);
      tkobjc_bindKeyReleaseReturnToResetColorAndUpdate (self);
      tkobjc_bindFocusInToSetSelection (self);
      tkobjc_bindFocusOutToClearSelection (self);
    }
  else
    [self setActiveFlag: NO];

  if (probeType == _C_ID)
    {
      tkobjc_bindButton3ToSpawn (self, owner, 0);
      tkobjc_dragAndDropTarget (self, owner);
      tkobjc_dragAndDrop (self, owner);
    }
  else
    tkobjc_bindButton3ToBeUnhelpful (self);
  return self;
}

@end

