#import <simtoolsgui/MultiVarProbeWidget.h>
#import <objectbase/SwarmObject.h>
#import <gui.h>

#include <misc.h>

@interface MultiProbe: SwarmObject
{
  id <Frame> parent;

  id <VarProbe> varProbe;
  id <Frame> frame;
  id <Label> label;
  id <Map> entryMap;
  compare_t compareFunction;
  BOOL interactiveFlag;
}
- setParent: parent;
- (void)setCompareFunction: (compare_t)aFunction;
- setVarProbe: (id <VarProbe>)probe;
- createEnd;
- addEntries: (id <List>)objectList;
- Spawn: (const char *)widgetName;
- (const char *)package: (const char *)window;
@end

@implementation MultiProbe

- setParent: (id <Frame>)parentFrame
{
  parent = parentFrame;

  return self;
}

- setVarProbe: (id <VarProbe>)probe
{
  varProbe = probe;
 
  return self;
}

- (void)setCompareFunction: (compare_t)aFunction
{
  compareFunction = aFunction;
}

- createEnd
{
  frame = [Frame createParent: parent];

  label = [VarProbeLabel createParent: frame];
  [label setText: [varProbe getProbedVariable]];

  entryMap = [Map createBegin: [self getZone]];
  [entryMap setCompareFunction: compareFunction];
  entryMap = [entryMap createEnd];

  return self;
}

- addEntryFor: obj
{
  id <VarProbeEntry> entry = [VarProbeEntry createBegin: [self getZone]];
  
  [entry setParent: frame];
  [entry setProbeType: ([varProbe getProbedType])[0]];
  [entry setInteractiveFlag: [varProbe getInteractiveFlag]];
  [entry setOwner: self];
  entry = [entry createEnd];
  if (![varProbe getInteractiveFlag])
    {
      [entry setActiveFlag: YES];
      [entry setValue: "Blah"];
      [entry setActiveFlag: NO];
    }
  else
    [entry setActiveFlag: YES];
  
  [entryMap at: obj insert: entry];
  
  return self;
}

- addEntries: (id <List>)objectList
{
  id <ListIndex> li = [objectList begin: [self getZone]];
  id obj;

  while ((obj = [li next]) != nil)
    [self addEntryFor: obj];

  [li drop];

  return self;
}

static BOOL
findObject (id entryMap, const char *windowName,
            id *obj_ptr, id *entry_ptr)
{
  id <MapIndex> emi = [entryMap begin: [entryMap getZone]];
  id <VarProbeEntry> entry;
  id obj;

  while ((entry = [emi next: &obj]) != nil)
    {
      if (strcmp (windowName, [entry getWidgetName]) == 0)
        {
          *obj_ptr = obj;
          if (entry_ptr)
            *entry_ptr = entry;
          [emi drop];
          return YES;
        }
    }
  *obj_ptr = nil;
  if (entry_ptr)
    *entry_ptr = nil;
  [emi drop];
  return NO;
}

- setVariableValue: (const char *)windowName
{
  id obj;
  id <VarProbeEntry> entry;

  if (findObject (entryMap, windowName, &obj, &entry))
    [varProbe setData: obj ToString: [entry getValue]];
  return self;
}

- update
{
  id <MapIndex> emi = [entryMap begin: [self getZone]];
  char buffer[512];
  id <VarProbeEntry> entry;
  id obj;

  while ((entry = [emi next: &obj]) != nil)
    {
      const char *originalValue = strdup ([entry getValue]);
      const char *newValue =
        strdup ([varProbe probeAsString: obj Buffer: buffer]);

      if (strcmp (newValue, originalValue) != 0)
        {
          if ([varProbe getInteractiveFlag])
            [entry setValue: newValue];
          else
            {
              [entry setActiveFlag: YES];
              [entry setValue: newValue];
              [entry setActiveFlag: NO];
            }
        }
    }
  [emi drop];

  return self;
}

- packFillLeft
{
  [frame packFillLeft: NO];

  [label pack];
  [entryMap forEach: M(pack)];
  
  return self;
}

- (void)drop
{
  [frame drop];
  [label drop];
  [entryMap forEach: M(drop)];
  [entryMap drop];
}

- Spawn: (const char *)widgetName
{
  return self;
} 

- (const char *)package: (const char *)windowName
{
  id obj;

  if (findObject (entryMap, windowName, &obj, NULL))
    return [obj getObjectName];
  return "";
}

- (const char *)getId: (const char *)windowName
{
  id obj;
  id <VarProbeEntry> entry;
  
  if (findObject (entryMap, windowName, &obj, &entry))
    {
      const char *str = [entry getValue];
      
      return str;
    }
  return "";
}

@end

@implementation MultiVarProbeWidget

PHASE(Creating)

- setObjectList: (id <List>)l
{
  objectList = l;

  return self;
}

- setProbeList: (id <List>)l
{
  probeList = l;

  return self;
}

- setLabelingFlag: (BOOL)flag
{
  labelingFlag = flag;

  return self;
}

- setAgentNameSelector: (SEL)sel
{
  agentNameSelector = sel;

  return self;
}

- setParent: (id <Frame>)frame
{
  parent = frame;
  
  return self;
}

- createEnd
{
  id <Zone> aZone = [self getZone];

  int findPosition (id <List> l, id obj)
    {
      id <ListIndex> li;
      id lobj;
      
      li = [l begin: aZone];

      while ((lobj = [li next]) != nil)
        if (lobj == obj)
          {
            int offset = [li getOffset];

            [li drop];
            return offset;
          }
      abort ();
    }

  int compareObjects (id a, id b)
    {
      return findPosition (objectList, a) - findPosition (objectList, b);
    }

  int compareProbes (id a, id b)
    {
      return findPosition (probeList, a) - findPosition (probeList, b);
    }

  objectsLabelFrame = [Frame createParent: parent];
  
  objectsTitleLabel = [VarProbeLabel createParent: objectsLabelFrame];
  [objectsTitleLabel setText: [[objectList getFirst] name]];

  labelMap = [Map createBegin: aZone];
  [labelMap setCompareFunction: compareObjects];
  labelMap = [labelMap createEnd];

  {
    id <ListIndex> oli = [objectList begin: aZone];
    id obj;
    
    while ((obj = [oli next]) != nil)
      {
        id <VarProbeLabel> label =
          [VarProbeLabel createParent: objectsLabelFrame];
        
        [label setText: (const char *)[obj perform: agentNameSelector]];
        
        [labelMap at: obj insert: label];
      }
    [oli drop];
  }

  multiProbeMap = [Map createBegin: aZone];
  [multiProbeMap setCompareFunction: compareProbes];
  multiProbeMap = [multiProbeMap createEnd];

  {
    id <ListIndex> pli;
    id <VarProbe> probe;

    pli = [probeList begin: aZone];

    while ((probe = [pli next]) != nil)
      {
        id multiProbe = [MultiProbe createBegin: aZone];

        [multiProbe setParent: parent];
        [multiProbe setCompareFunction: compareObjects];
        [multiProbe setVarProbe: probe];
        multiProbe = [multiProbe createEnd];
        
        [multiProbe addEntries: objectList];
        
        [multiProbeMap at: probe insert: multiProbe];
      }
    [pli drop];
  }
  [self update];
  
  return self;
} 

- update
{
  [multiProbeMap forEach: M(update)];
  GUI_UPDATE ();
  return self;
}

- pack 
{
  [objectsLabelFrame packFillLeft: NO];
  [multiProbeMap forEach: M(packFillLeft)];

  [objectsTitleLabel pack];
  [labelMap forEach: M(pack)];
  
  return self;
}

- (void)drop
{
  [objectsTitleLabel drop];

  [labelMap forEach: M(drop)];
  [multiProbeMap forEach: M(drop)];

  [objectsLabelFrame drop];
}  

@end
