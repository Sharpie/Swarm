// Copyright (C) 1995 The Santa Fe Institute.
// No warranty implied, see LICENSE for terms.

#import <objectbase.h>

@interface DiGraph: SwarmObject {
  id nodeList ;
  id canvas ;

  // For BoingDistribute...
  float springLength ;
}

-setCanvas: aCanvas ;
-createEnd ;
-getCanvas ;
-getNodeList ;
-addNode: aNode ;
-dropNode: which ;
-addLinkFrom: this To: that ;
-removeLink: aLink ;
-update ;

// Node placement techniques...

-redistribute ;

-setSpringLength: (float) aLength ;
-boingDistribute: (int) iterations ;
-boingDistribute ;
-(double) boingStep ;

@end
