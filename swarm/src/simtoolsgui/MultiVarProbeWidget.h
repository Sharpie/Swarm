// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui.h> // MultiVarProbeWidget
#import <objectbase/SwarmObject.h>
#import <collections.h>
#import <gui.h>

@interface MultiVarProbeWidget: SwarmObject
{
  id <List> objectList;
  id <ProbeMap> probeMap;

  BOOL fieldLabelingFlag;
  SEL objectNameSelector;
  
  id <Frame> parent;
  id <VarProbeLabel> objectsTitleLabel;
  id <Frame> objectsLabelFrame;

  id <Map> labelMap;
  id <Map> multiProbeMap;
}

+ createBegin: aZone;
- createEnd;
- setObjectList: (id <List>)objectList;
- setParent: parent;
- setProbeMap: (id <ProbeMap>)probeMap;
- setFieldLabelingFlag: (BOOL)labelingFlag;
- setObjectNameSelector: (SEL)agentNameSelector;

- update;
- pack;
- (void)drop;
@end
