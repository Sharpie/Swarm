// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

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

- update
{
  raiseEvent (SubclassMustImplement, "");
  return self;
}

@end
