// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h>
#import <gui.h>

@interface CommonProbeDisplay: SwarmObject
{
  id probedObject;  
  id <Frame> topLevel, topFrame;
  id <ProbeCanvas> canvas;

  BOOL horizontalScrollbarFlag;
  BOOL removeRef;
  BOOL markedForDropFlag;
  ref_t objectRef;
  const char *windowGeometryRecordName;
}

- createEnd;
- install;
- setProbedObject: anObject;
- getProbedObject;

- setWindowGeometryRecordName : (const char *)windowGeometryRecordName;
- (void)setRemoveRef: (BOOL)removeRef;
- (void)setObjectRef: (ref_t)objectRef;
- (void)markForDrop;
- (BOOL)getMarkedForDropFlag;

- (const char *)package;
- (const char *)getId;

@end
