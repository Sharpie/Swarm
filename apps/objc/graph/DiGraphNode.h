// Copyright � 1995-2000 Swarm Development Group.
// No warranty implied, see LICENSE for terms.

#import <objectbase/SwarmObject.h>
#import <gui.h>

@interface DiGraphNode: SwarmObject
{
  id fromList;
  id toList;
  id <Canvas> canvas;
  id <NodeItem> nodeItem;
  id <Symbol> nodeType;
  const char *label;
  id uRandPosition;
}

- setRandPosFunc: rp;
- setCanvas: aCanvas;
- setCanvas: aCanvas withRandPosFunc: posFunc;
- createEnd;
- getNodeItem;
- getToLinks;
- getFromLinks;
- makeLinkTo: aNode;
- makeLinkFrom: aNode;
- addFrom: aLink;
- addTo: aLink;
- removeFrom: aLink;
- removeTo: aLink;
- (int)linkedTo: anObj;
- (int)linkedFrom: anObj;
- (int)agreeX: (int)x Y: (int)y;
- updateLinks;
- hideNode;
- (void)drop;
- setNodeLabel: (const char *)aLabel;
- (const char *) getNodeLabel;

@end
