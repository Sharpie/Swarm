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

#import <simtoolsgui/CommonProbeDisplay.h>
#import <simtoolsgui.h> // probeDisplayManager
#import <gui.h>
#include <defobj.h> // STRDUP

@implementation CommonProbeDisplay

PHASE(Creating)

- setWindowGeometryRecordName: (const char *)theName
{
  windowGeometryRecordName = theName ? STRDUP (theName) : NULL;
  return self;
}

- setSaveSizeFlag: (BOOL)theSaveSizeFlag
{
  saveSizeFlag = theSaveSizeFlag;
  return self;
}

- createEnd
{
  id <Frame> c_Frame;
  
  topLevel = [Frame createBegin: [self getZone]];
  [topLevel setWindowGeometryRecordName: windowGeometryRecordName];
  [topLevel setSaveSizeFlag: saveSizeFlag];
  topLevel = [topLevel createEnd];
  [topLevel enableDestroyNotification: self
            notificationMethod: @selector (markForDrop)];
  [topLevel withdraw];
  c_Frame =  [Frame createParent: topLevel]; 

  canvas = [ProbeCanvas createBegin: [self getZone]];
  [canvas setParent: c_Frame];
  [canvas setHorizontalScrollbarFlag: horizontalScrollbarFlag];
  canvas = [canvas createEnd];
  
  [c_Frame pack];

  topFrame =  [Frame createParent: canvas];
  [topFrame setBorderWidth: 0];
  [canvas addWidget: topFrame X: 0 Y: 0 centerFlag: NO];

  markedForDropFlag = NO;
  return self;
}

PHASE(Using)

- (void)markForDrop
{
  [topLevel disableDestroyNotification];
  if ([probeDisplayManager getDropImmediatelyFlag])
    [self drop];
  else
    markedForDropFlag = YES;
}

- (BOOL)getMarkedForDropFlag
{
  return markedForDropFlag;
}

- getTopLevel
{
  return topLevel;
}

- install
{
  [topLevel deiconify];
  [canvas checkGeometry: topFrame];

  [probeDisplayManager addProbeDisplay: self];

  return self;
}

- (void)drop
{
  [topFrame drop];
  [canvas drop];
  [topLevel drop];
  if (windowGeometryRecordName)
    FREEBLOCK (windowGeometryRecordName);
  [super drop];
}

- (void)update
{
  raiseEvent (SubclassMustImplement, "");
}

@end
