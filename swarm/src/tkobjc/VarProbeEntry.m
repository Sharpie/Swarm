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

#import <tkobjc/VarProbeEntry.h>

#import <tkobjc/common.h>
#import <tkobjc/global.h>

#import <defobj/swarm-objc-api.h>

static void
tkobjc_bindReturnToSetValue (id widget, id self)
{
  const char *widgetName = [widget getWidgetName];
  
  [globalTkInterp
    eval:
      "bind %s <Return> {%s configure -highlightcolor red ;"
    "update ;"
    "%s setVariableValue: %%W}",
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

- setVarProbe: aVarProbe
{
  varProbe = aVarProbe;

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

  {
    const char *probedType = [varProbe getProbedType];
    
    if (!probedType || probedType[0] == _C_ID)
      {
        tkobjc_bindButton3ToSpawn (self, owner, 0);
        tkobjc_dragAndDropTarget (self, owner);
        tkobjc_dragAndDrop (self, owner);
      }
    else
      tkobjc_bindButton3ToBeUnhelpful (self);
  }
  return self;
}

PHASE(Using)

- (id)getVarProbe
{
  return varProbe;
}

@end

