// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/SuperButton.h>

#include <stdlib.h>
#include <string.h>

#include <tclObjc.h>
#include <tkobjc/global.h>
#include <tkobjc/common.h>

@implementation SuperButton

static void
tkobjc_configureWidgetToPackBeforeAndFillLeftThenDisableAndResize
(id superB,
 id superWidget,
 id user,
 id owner)
{
  [globalTkInterp 
    eval:
      "%s configure -command { pack %s -before %s -fill both -expand 1; %s configure -state disabled; %s do_resize}",
    [superB getWidgetName],
    [superWidget getWidgetName],
    [user getWidgetName],
    [superB getWidgetName],
    tclObjc_objectToName (owner)];
}

static void
tkobjc_configureSuperBitmap (id widget)
{
  [globalTkInterp
    eval:
      "%s configure -bitmap super -activeforeground forestgreen -foreground forestgreen", 
    [widget getWidgetName]];
}

static void
tkobjc_configureWidgetToBeep (id widget)
{
  [globalTkInterp eval: "%s configure -command { bell }",
                  [widget getWidgetName]];
}


- createEnd
{
  [super createEnd];

  tkobjc_configureSuperBitmap (self);
  if (superWidget)
    tkobjc_configureWidgetToPackBeforeAndFillLeftThenDisableAndResize 
      (self,
       superWidget,
       user,
       owner);
  else
    {
      tkobjc_configureWidgetToBeep (self);
      [self setActiveFlag: NO];
    }

  return self;
}

- setSuperWidget: theSuperWidget
{
  superWidget = theSuperWidget;
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
