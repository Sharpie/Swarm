// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <gui.h>
#import <simtoolsgui.h> // CommonProbeDisplay

@interface CommonProbeDisplay: SwarmObject <CommonProbeDisplay>
{
  id <Frame> topLevel, topFrame;
  id <ProbeCanvas> canvas;

  BOOL horizontalScrollbarFlag;
  BOOL markedForDropFlag;
  const char *windowGeometryRecordName;
  BOOL saveSizeFlag;
}

- createEnd;
- install;

- setWindowGeometryRecordName : (const char *)windowGeometryRecordName;
- setSaveSizeFlag: (BOOL)saveSizeFlag;
- (void)markForDrop;
- (BOOL)getMarkedForDropFlag;
- getTopLevel;
- (void)drop;
- update;

@end
