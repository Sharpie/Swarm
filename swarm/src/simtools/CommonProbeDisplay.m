// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/CommonProbeDisplay.h>
#import <simtools/global.h> // probeDisplayManager

#import <tkobjc.h>
#import <tkobjc/control.h>

@implementation CommonProbeDisplay

- createEnd
{
  id c_Frame;

  topLevel = [Frame createBegin: [self getZone]];
  [topLevel setWindowGeometryRecordName: windowGeometryRecordName];
  topLevel = [topLevel createEnd];
  [topLevel enableDestroyNotification: self
            notificationMethod: @selector (markForDrop)];
  [topLevel setWindowTitle: tkobjc_getId (probedObject)];
  tkobjc_withdrawWindow (topLevel);
  c_Frame =  [Frame createParent: topLevel]; 

  canvas = [Canvas createParent: c_Frame];
  tkobjc_configureProbeCanvas (canvas);

  [c_Frame pack];

  topFrame =  [Frame createParent: canvas];
  tkobjc_setBorderWidth (topFrame, 0);

  tkobjc_createWindow (topFrame);

  markedForDropFlag = NO;
  return self;
}

- setWindowGeometryRecordName: (const char *)theName
{
  windowGeometryRecordName = theName;
  return self;
}

- setProbedObject: (id)anObject
{
  probedObject = anObject;
  return self;
}

- getProbedObject
{
  return probedObject;
}

- (void)setRemoveRef: (BOOL) theRemoveRef
{
  removeRef = theRemoveRef;
}

- (void)setObjectRef: (ref_t)theObjectRef
{
  objectRef = theObjectRef;
}

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

- install
{
  tkobjc_deiconify (topLevel);
  tkobjc_assertGeometry (topFrame);

  [probeDisplayManager addProbeDisplay: self];
  return self;
}
  

@end
