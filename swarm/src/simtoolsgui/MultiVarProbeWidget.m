#import <simtoolsgui/MultiVarProbeWidget.h>
#import <objectbase/SwarmObject.h>
#import <objectbase.h>
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
  BOOL labelingFlag;
}
- setParent: parent;
- setLabelingFlag: (BOOL)flag;
- (void)setCompareFunction: (compare_t)aFunction;
- setVarProbe: (id <VarProbe>)probe;
- createEnd;
- addEntries: (id <List>)objectList;
- Spawn: (const char *)windowName;
- idReceive: (const char *)windowName;
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

- setLabelingFlag: (BOOL)flag
{
  labelingFlag = flag;

  return self;
}

- createEnd
{
  frame = [Frame createParent: parent];

  if (labelingFlag)
    {
      label = [VarProbeLabel createParent: frame];
      [label setText: [varProbe getProbedVariable]];
    }

  entryMap = [Map createBegin: [self getZone]];
  [entryMap setCompareFunction: compareFunction];
  entryMap = [entryMap createEnd];

  return self;
}

- addEntryFor: obj
{
  id <VarProbeEntry> entry = [VarProbeEntry createBegin: [self getZone]];
  
  [entry setParent: frame];
  [entry setVarProbe: varProbe];
  [entry setInteractiveFlag: [varProbe getInteractiveFlag]];
  [entry setOwner: self];
  entry = [entry createEnd];
  if (![varProbe getInteractiveFlag])
    [entry setActiveFlag: NO];
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

- Spawn: (const char *)windowName
{
  return self;
} 

- idReceive: (const char *)windowName
{
  id resObj = GUI_DRAG_AND_DROP_OBJECT ();
  id obj;
  id <VarProbeEntry> entry;

  if (findObject (entryMap, windowName, &obj, &entry))
    {
      [[entry getVarProbe] setData: obj To: &resObj];
      
      [self update];
    }

  return self;
}      

- (const char *)package: (const char *)windowName
{
  id obj;
  id <VarProbeEntry> entry;

  if (findObject (entryMap, windowName, &obj, &entry))
    {
      id *content = [[entry getVarProbe] probeRaw: obj];
      
      if (*content == nil)
        {
          GUI_BEEP ();
          GUI_UPDATE ();
          return "";
        }
      return [*content getObjectName];         
    }
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

+ createBegin: aZone
{
  MultiVarProbeWidget *obj = [super createBegin: aZone];

  obj->fieldLabelingFlag = NO;

  return obj;
}

- setObjectList: (id <List>)l
{
  objectList = l;

  return self;
}

- setProbeMap: (id <ProbeMap>)aProbeMap
{
  probeMap = aProbeMap;

  return self;
}

- setFieldLabelingFlag: (BOOL)flag
{
  fieldLabelingFlag = flag;

  return self;
}

- setObjectNameSelector: (SEL)sel
{
  objectNameSelector = sel;

  return self;
}

- setParent: frame
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
      BOOL aIsVarProbe = [a conformsTo: @protocol(_VarProbe)];
      BOOL bIsVarProbe = [b conformsTo: @protocol(_VarProbe)];

      int typediff = aIsVarProbe - bIsVarProbe;

      if (typediff == 0)
        {
          if (aIsVarProbe)
            return strcmp ([a getProbedVariable], [b getProbedVariable]);
          else
            return strcmp ([a getProbedMessage], [b getProbedMessage]);
        }
      return typediff;
    }

  objectsLabelFrame = [Frame createParent: parent];

  labelMap = [Map createBegin: aZone];
  [labelMap setCompareFunction: compareObjects];
  labelMap = [labelMap createEnd];

  if (objectNameSelector)
    {
      id oli = [objectList begin: aZone];
      id obj;

      objectsTitleLabel = [VarProbeLabel createParent: objectsLabelFrame];
      [objectsTitleLabel setText: [[objectList getFirst] name]];
      
      while ((obj = [oli next]) != nil)
        {
          id <Label> label = [Label createParent: objectsLabelFrame];
        
          [label setText: (const char *)[obj perform: objectNameSelector]];
          
        [labelMap at: obj insert: label];
        }
      [oli drop];
    }
  
  multiProbeMap = [Map createBegin: aZone];
  [multiProbeMap setCompareFunction: compareProbes];
  multiProbeMap = [multiProbeMap createEnd];

  {
    id pmi = [probeMap begin: aZone];
    id probe;
    
    while ((probe = [pmi next]) != nil)
      {
        if ([probe conformsTo: @protocol(_VarProbe)])
          {
            id multiProbe = [MultiProbe createBegin: aZone];

            [multiProbe setLabelingFlag: fieldLabelingFlag];
            [multiProbe setParent: parent];
            [multiProbe setCompareFunction: compareObjects];
            [multiProbe setVarProbe: probe];
            multiProbe = [multiProbe createEnd];
            
            [multiProbe addEntries: objectList];
            
            [multiProbeMap at: probe insert: multiProbe];
          }
      }
    [pmi drop];
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
