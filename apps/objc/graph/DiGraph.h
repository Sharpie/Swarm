// Copyright (C) 1995-1998 The Santa Fe Institute.
// No warranty implied, see LICENSE for terms.

#import <objectbase/SwarmObject.h>
#import <gui.h>

@interface DiGraph: SwarmObject
{
  id nodeList;
  id <Canvas> canvas;

  int randPosSeed;
  id randGPosition, uRandPosition;
  // For BoingDistribute...
  float springLength;
}

- setRandPosSeed: (int)seed;
- setCanvas: aCanvas;
- setCanvas: aCanvas withRandPosSeed: (int)seed;
- showCanvas: aCanvas;
- hideCanvas;
- getCanvas;
- getNodeList;
- addNode: aNode;
- dropNode: which;
- addLinkFrom: this To: that;
- removeLink: aLink;
- update;

// Node placement techniques...

- redistribute;

- setSpringLength: (float) aLength;
- boingDistribute: (int) iterations;
- boingDistribute;
- (double) boingStep;

@end
