// Copyright (C) 1995 The Santa Fe Institute.
// No warranty implied, see LICENSE for terms.

#import <swarmobject.h>

@interface DiGraph: SwarmObject {
  id nodeList ;
  id canvas ;
}

-setCanvas: aCanvas ;
-createEnd ;
-getCanvas ;
-getNodeList ;
-addNode: aNode ;
-removeNode: which ;
-addLinkFrom: this To: that ;
-removeLink: aLink ;
-redistribute ;
-update ;
@end
