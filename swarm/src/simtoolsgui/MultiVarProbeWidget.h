#import <objectbase/SwarmObject.h>
#import <collections.h>
#import <gui.h>

@interface MultiVarProbeWidget: SwarmObject
{
  BOOL labelingFlag;
  id <List> objectList, probeList;

  SEL agentNameSelector;
  
  id <Frame> parent;
  id <VarProbeLabel> objectsTitleLabel;
  id <Frame> objectsLabelFrame;

  id <Map> labelMap;
  id <Map> multiProbeMap;
}

- createEnd;
- setLabelingFlag: (BOOL)labelingFlag;
- setObjectList: (id <List>)objectList;
- setProbeList: (id <List>)probeList;
- setAgentNameSelector: (SEL)agentNameSelector;
- update;
- pack;
- (void)drop;
@end
