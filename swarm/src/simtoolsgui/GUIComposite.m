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

#import <simtoolsgui/GUIComposite.h>
#import <simtoolsgui.h> // buildWindowGeometryRecordName
#import <collections.h>

static int
compareFunc (id obj1, id obj2)
{
  return strcmp ((const char *)obj1, (const char *)obj2);
}

@implementation GUIComposite

PHASE(Creating)

+ createBegin: aZone
{
  GUIComposite *obj = [super createBegin: aZone];

  obj->componentList = [Map createBegin: aZone];
  [obj->componentList setCompareFunction: compareFunc];
  obj->componentList = [obj->componentList createEnd];

  return obj;
}

- setWindowGeometryRecordName: (const char *)windowGeometryRecordName
{
  baseWindowGeometryRecordName = (windowGeometryRecordName
				  ? STRDUP (windowGeometryRecordName)
				  : NULL);
  return self;
}

- setSaveSizeFlag: (BOOL)theSaveSizeFlag
{
  saveSizeFlag = theSaveSizeFlag;
  return self;
}

- setWindowGeometryRecordNameForComponent: (const char *)componentName
                                   widget: widget
{
  if ([componentList at: (id)componentName])
    [componentList at: (id)componentName replace: widget];
  else
    [componentList at: (id)componentName insert: widget];

  [widget setWindowGeometryRecordName: 
            buildWindowGeometryRecordName (baseWindowGeometryRecordName, 
                                           componentName)];
  return self;
}

PHASE(Using)

- (void)enableDestroyNotification: theNotificationTarget
               notificationMethod: (SEL)theNotificationMethod
{
  [componentList forEach: 
                   @selector (enableDestroyNotification:notificationMethod:)
                 : theNotificationTarget
                 : (id)theNotificationMethod];
}

- (void)disableDestroyNotification
{
  [componentList forEach: @selector (disableDestroyNotification)];
}

- (void)drop
{
  if (baseWindowGeometryRecordName)
    FREEBLOCK (baseWindowGeometryRecordName);
  [super drop];
}
@end
