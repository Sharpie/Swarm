// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <gui.h>

@interface CommonProbeDisplay: SwarmObject
{
  id <Frame> topLevel, topFrame;
  id <ProbeCanvas> canvas;

  BOOL horizontalScrollbarFlag;
  BOOL markedForDropFlag;
  const char *windowGeometryRecordName;
}

- createEnd;
- install;

- setWindowGeometryRecordName : (const char *)windowGeometryRecordName;
- (void)markForDrop;
- (BOOL)getMarkedForDropFlag;
- getTopLevel;
- (void)drop;

@end
