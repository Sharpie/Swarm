// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
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
  [topLevel setWindowGeometryRecordName : windowGeometryRecordName];
  topLevel = [topLevel createEnd];
  [topLevel setWindowTitle: getId (probedObject)];
  withdrawWindow (topLevel);
  c_Frame =  [Frame createParent: topLevel]; 

  canvas = [Canvas createParent: c_Frame];
  configureProbeCanvas (canvas);

  [c_Frame pack];

  topFrame =  [Frame createParent: canvas];
  setBorderWidth (topFrame, 0);

  createWindow (topFrame);

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
  deiconify (topLevel);
  assertGeometry (topFrame);

  [probeDisplayManager addProbeDisplay: self];
  return self;
}
  

@end
