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

#import <tkobjc/SuperButton.h>
#import <tkobjc/global.h>

@implementation SuperButton

static void
tkobjc_configureWidgetToPackBeforeAndFillLeftThenDisableAndResize (id superB,
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
    [owner getObjectName]];
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

PHASE(Creating)

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

PHASE(Using)

@end
