// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/ClassDisplayHideButton.h>

#include <stdlib.h>
#include <string.h>

#include <tclObjc.h>
#include <tkobjc/global.h>
#include <tkobjc/common.h>

@implementation ClassDisplayHideButton

static void
tkobjc_packForgetArmSuperAndResize (id hideB, id user, id subWidget, id owner)
{
  const char *subWidgetName = strdup (tclObjc_objectToName (subWidget));
  const char *ownerName = tclObjc_objectToName (owner);

  [globalTkInterp 
    eval: 
      "%s configure -command {pack forget %s; %s armSuperButton; %s do_resize}",
    [hideB getWidgetName],
    [user getWidgetName],
    subWidgetName,
    ownerName];

  free ((void *)subWidgetName);
}

static void
tkobjc_configureWidgetToDrop (id widget, id owner)
{
  [globalTkInterp 
    eval:
      "%s configure -command {%s markForDrop}",
    [widget getWidgetName],
    tclObjc_objectToName (owner)];
}

static void
tkobjc_configureHideBitmap (id widget)
{
  [globalTkInterp
    eval: "%s configure -bitmap hide -activeforeground red -foreground red", 
    [widget getWidgetName]];
}

- createEnd
{
  [super createEnd];

  if (subWidget)
    {
      tkobjc_packForgetArmSuperAndResize (self, user, subWidget, owner);
      tkobjc_configureHideBitmap (self);
    }
  else
    {
      tkobjc_configureWidgetToDrop (self, owner);
      tkobjc_configureSpecialBitmap (self);
    }

  return self;
}

- setSubWidget: theSubWidget
{
  subWidget = theSubWidget;
  return self;
}

- setOwner: theOwner
{
  owner = theOwner;
  return self;
}

- setUser: theUser
{
  user = theUser;
  return self;
}

@end

