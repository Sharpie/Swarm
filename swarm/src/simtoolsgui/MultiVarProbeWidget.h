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
