// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/CommonProbeDisplay.h>
#import <simtoolsgui.h>
#import <collections.h>

@interface MultiVarProbeDisplay: CommonProbeDisplay
{
  id <List> objectList;
  id <ProbeMap> probeMap;
  id <MultiVarProbeWidget> widget;
  SEL objectNameSelector;
  BOOL fieldLabelingFlag;

  id <Frame> top_top_Frame, middleFrame, raisedFrame;
  id <CompleteProbeDisplayLabel> title;
  id <Button> hideB;
}

- setObjectList: (id <List>)objectList;
- setProbeMap: (id <ProbeMap>)probeMap;
- setObjectNameSelector: (SEL)aSel;
- setFieldLabelingFlag: (BOOL)labelingFlag;
- createEnd;
- update;
- (void)drop;

- (const char *)package: (const char *)windowName;
- (const char *)getId: (const char *)windowName;   
@end
