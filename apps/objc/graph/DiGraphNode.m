// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <graph.h> 
#import <random.h>
#import <simtoolsgui.h> // CREATE_PROBE_DISPLAY

@implementation DiGraphNode

- setRandPosFunc: rp
{
  uRandPosition = rp;
  return self;
}

- setCanvas: aCanvas 
{
  [self setCanvas: aCanvas withRandPosFunc: uRandPosition];
  return self;
}

- setCanvas: aCanvas withRandPosFunc: posFunc
{
  canvas = aCanvas;
  
  if (aCanvas == nil)
    return self;
  
  if (nodeType == OvalNode)
    nodeItem = [OvalNodeItem createBegin: [self getZone]]; 
  else if (nodeType == RectangleNode)
    nodeItem = [RectangleNodeItem createBegin: [self getZone]]; 
  else
    {
      fprintf (stderr,
               "Invalid nodeType in DiGraphNode. Defaulting to OvalNode.\n");
      nodeItem = [OvalNodeItem createBegin: [self getZone]]; 
    }
  
  nodeItem = [[[[nodeItem setX: 
                            [posFunc getIntegerWithMin: 0L
                                            withMax: [canvas getWidth]]
                          Y: 
                            [posFunc getIntegerWithMin: 0L
                                            withMax: [canvas getHeight]]] 
                 setString: label]
                setCanvas: canvas] createEnd];
  
  [nodeItem setTargetId: self];
  [nodeItem setClickSel: M(showContent)];
  [nodeItem setMoveSel: M(agreeX:Y:)];
  [nodeItem setPostMoveSel: M(updateLinks)];

  return self;
}

- setNodeLabel: (const char *)aLabel
{
  label = aLabel;
  return self;
}

+ createBegin: aZone
{
  DiGraphNode *obj;

  // Uses the default distribution for the 
  // random node positions from simtools
  // not recommended - use only for backwards 
  // compatitibility.

  obj = [super createBegin: aZone];
  obj->nodeType = OvalNode;
  obj->uRandPosition = uniformIntRand;

  return obj;
}

- createEnd
{  
  fromList = [List create: [self getZone]];
  toList = [List create: [self getZone]];

  return self;
}

- getNodeItem
{
  return nodeItem;
}

- getToLinks
{
   return fromList;
}

- getFromLinks
{
   return toList;
}

- makeLinkTo: aNode
{
  id aLink;

  if (canvas)
     aLink = [[[[DiGraphLink createBegin: [self getZone]] 
                  setFrom: self To: aNode] 
                 setCanvas: canvas]                        
                createEnd];  
  else
      aLink = [[[DiGraphLink createBegin: [self getZone]] 
                  setFrom: self To: aNode]   
                 createEnd];  
  return aLink;
}

- makeLinkFrom: aNode
{
  id aLink;

  if (canvas)    
     aLink = [[[[DiGraphLink createBegin: [self getZone]] 
               setFrom: aNode To: self]              
              setCanvas: canvas]
             createEnd];
  else  
     aLink = [[[DiGraphLink createBegin: [self getZone]]   
                    setFrom: aNode To: self]   
                   createEnd]; 
  return aLink;
}

- addFrom: aLink
{
  [fromList addFirst: aLink];
  return self;
}

- addTo: aLink
{
  [toList addFirst: aLink];
  return self;
}

- (int)linkedTo: anObj
{
  id index, link;

  index = [toList begin: globalZone];
  while ((link = [index next]))
    if ([link getTo] == anObj)
      {
        [index drop]; 
        return 1;
      }
  [index drop];
  return 0;
}

- (int)linkedFrom: anObj
{
  id index, link;

  index = [fromList begin: globalZone];
  while ((link = [index next]))
    if ([link getFrom] == anObj)
      {
        [index drop]; 
        return 1;
      }
  [index drop];
  return 0;
}

- removeFrom: which
{
  [fromList remove: which];
  return self;
}

- removeTo: which
{
  [toList remove: which];
  return self;
}

- hideNode
{
  canvas = nil;
  [nodeItem drop];      
  return self;
}

- (void)drop
{
  while ([fromList getCount])
    [[fromList getFirst] drop];
  [fromList drop];

  while ([toList getCount])
    [[toList atOffset:0] drop];
  [toList drop];

  if (canvas)
    [self hideNode];
  
  [super drop];
}

// Callbacks...

- (int)agreeX: (int)x Y: (int)y
{
  return 1;
}

- updateLinks
{
  [fromList forEach: M(update)];
  [toList forEach: M(update)];
  return self;
}

- showContent
{
  CREATE_PROBE_DISPLAY (self);
  return self;
}

@end
