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

#import <tkobjc/MessageProbeEntry.h>

#import <tkobjc/common.h>
#import <tkobjc/global.h>

static void
tkobjc_bindButton3ToBeUnhelpfulAndRefocus (id widget, id self)
{
  const char *widgetName = [widget getWidgetName];
  [globalTkInterp
    eval:
      "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;"
    "update ; bell ; update ; %s configure -highlightcolor black ;"
    "update ; focus %s ; update} ;",
    widgetName, widgetName, widgetName, widgetName,
    [self getWidgetName]];
}

@implementation MessageProbeEntry

PHASE(Creating)

- setArg: (int)theArg
{
  arg = theArg;
  return self;
}

- setIdFlag: (BOOL)theIdFlag
{
  idFlag = theIdFlag;
  return self;
}

+ createBegin: aZone
{
  MessageProbeEntry *obj = [super createBegin: aZone];
  
  obj->arg = -1;
  obj->idFlag = NO;
  return obj;
}
  
- createEnd
{
  BOOL argFlag = arg != -1;

  [super createEnd];
  if (idFlag)
    {
      [self setActiveFlag: NO];
      if (argFlag)
        {
          tkobjc_bindButton3ToArgSpawn (self, parent, arg);
          tkobjc_dragAndDropArg (self, parent, arg);
        }
      else
        {
          tkobjc_bindButton3ToSpawn (self, parent, 1);
          tkobjc_dragAndDrop (self, parent);
        }
    }
  else
    tkobjc_bindButton3ToBeUnhelpfulAndRefocus (self, parent);
  
  [self packFillLeft: argFlag];

  return self;
}

PHASE(Using)

@end

