#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc.h>
#import <simtools.h>

#import <objc/objc-api.h>
#import <graph.h> 

@implementation DiGraphNode

-setCanvas: the_canvas {
  canvas = the_canvas ;

  if(nodeType == OvalNode)
    nodeItem = [OvalNodeItem createBegin: [self getZone]] ; 
  else if(nodeType == RectangleNode)
    nodeItem = [RectangleNodeItem createBegin: [self getZone]] ; 
  else {
    fprintf(stderr,
      "Invalid nodeType in DiGraphNode. Defaulting to OvalNode.\n") ;
    nodeItem = [OvalNodeItem createBegin: [self getZone]] ; 
  }
  
  nodeItem = [[[[nodeItem setX: 
                     [uniformIntRand getIntegerWithMin: 0
                                               withMax: [canvas getWidth]]
                             Y: 
                     [uniformIntRand getIntegerWithMin: 0
                                               withMax: [canvas getHeight]]] 
                          setString: label]
                          setCanvas: canvas] createEnd] ;

  [nodeItem setTargetId: self] ;
  [nodeItem setClickSel: M(showContent)] ;
  [nodeItem setMoveSel: M(agreeX:Y:)] ;
  [nodeItem setPostMoveSel: M(updateLinks)] ;

  return self ;
}

-setNodeLabel: (char *) aLabel {
  label = aLabel ;
  return self ;
}

+createBegin: aZone {
  DiGraphNode * obj ;

  obj = [super createBegin: aZone] ;
  obj->nodeType = OvalNode ;

  return obj ;
}

-createEnd {  

  fromList = [List create: [self getZone]] ;
  toList = [List create: [self getZone]] ;

  return self;
}

-getNodeItem {
  return nodeItem ;
}

-makeLinkTo: aNode {
  id aLink ;

  aLink = [[[[DiGraphLink createBegin: [self getZone]] 
                          setCanvas: canvas]
                          setFrom: self To: aNode] 
                          createEnd] ;

  return aLink ;
}

-makeLinkFrom: aNode {
  id aLink ;

  aLink = [[[[DiGraphLink createBegin: [self getZone]] 
                          setCanvas: canvas]
                          setFrom: aNode To: self] 
                          createEnd] ;

  return aLink ;
}

-addFrom: aLink {
  [fromList addFirst: aLink] ;
  return self ;
}

-addTo: aLink {
  [toList addFirst: aLink] ;
  return self ;
}

-(int)linkedTo: anObj {
  id index, link ;

  index = [toList begin: globalZone] ;
  while( (link = [index next]) )
    if([link getTo] == anObj){
      [index drop] ; 
      return 1 ;
    }
  [index drop] ;
  return 0 ;
}

-(int)linkedFrom: anObj {
  id index, link ;

  index = [fromList begin: globalZone] ;
  while( (link = [index next]) )
    if([link getFrom] == anObj){
      [index drop] ; 
      return 1 ;
    }
  [index drop] ;
  return 0 ;
}

-removeFrom: which {
  [fromList remove: which] ;
  return self ;
}

-removeTo: which {
  [toList remove: which] ;
  return self ;
}

-(void) drop {

  while([fromList getCount]){
    [[fromList getFirst] drop] ;
  }
  [fromList drop] ;

  while([toList getCount]){
    [[toList atOffset:0] drop] ;
  }
  [toList drop] ;

  if(canvas)
    [nodeItem drop] ;

  [super drop] ;
}

//Callbacks...

-(int) agreeX: (int) x Y: (int) y {
  return 1 ;
}

-updateLinks {
  [fromList forEach: M(update)] ;
  [toList forEach: M(update)] ;
  return self ;
}

-showContent {
  [probeDisplayManager createProbeDisplayFor: self] ;
  return self ;
}

@end
