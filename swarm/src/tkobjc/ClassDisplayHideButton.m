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

#import <tkobjc/ClassDisplayHideButton.h>
#import <defobj.h> // OSTRDUP, OFREEBLOCK
#include <tkobjc/global.h>
#include <tkobjc/common.h>

@implementation ClassDisplayHideButton

static void
tkobjc_packForgetArmSuperAndResize (id hideB, id user, id subWidget, id owner)
{
  const char *subWidgetName = OSTRDUP (subWidget, [subWidget getObjectName]);
  const char *ownerName = [owner getObjectName];

  [globalTkInterp 
    eval: 
      "%s configure -command {pack forget %s; %s armSuperButton; %s do_resize}",
    [hideB getWidgetName],
    [user getWidgetName],
    subWidgetName,
    ownerName];

  OFREEBLOCK (subWidget, subWidgetName);
}

static void
tkobjc_configureWidgetToDrop (id widget, id owner)
{
  [globalTkInterp 
    eval:
      "%s configure -command {%s markForDrop}",
    [widget getWidgetName],
    [owner getObjectName]];
}

static void
tkobjc_configureHideBitmap (id widget)
{
  [globalTkInterp
    eval: "%s configure -bitmap hide -activeforeground red -foreground red", 
    [widget getWidgetName]];
}

PHASE(Creating)

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

PHASE(Using)

@end

