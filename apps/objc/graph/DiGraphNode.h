// Copyright (C) 1995 The Santa Fe Institute.
// No warranty implied, see LICENSE for terms.

#import <swarmobject.h>

@interface DiGraphNode: SwarmObject {
  id fromList ;
  id toList ;
  id canvas ;
  id nodeItem ;
  id nodeType ;
  char *name ;
}

-setCanvas: aCanvas ;
-createEnd ;
-getNodeItem ;
-makeLinkTo: aNode ;
-makeLinkFrom: aNode ;
-addFrom: aLink ;
-addTo: aLink ;
-removeFrom: aLink ;
-removeTo: aLink ;
-(int)linkedTo: anObj ;
-(int)linkedFrom: anObj ;
-(int) agreeX: (int) x Y: (int) y ;
-updateLinks ;
-(void) drop ;
@end
