// Copyright (C) 1995 The Santa Fe Institute.
// No warranty implied, see LICENSE for terms.
#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <tkobjc.h>
#include <math.h>

#import "DiGraphLink.h"
#import "DiGraphNode.h"
#import "DiGraph.h"

@implementation DiGraph

-setCanvas: the_canvas {
  canvas = the_canvas ;
  return self ;
}

-createEnd {  
  nodeList = [List create: [self getZone]] ;
  return self;
}

-getCanvas {
  return canvas ;
}

-getNodeList {
  return nodeList ;
}

-addNode: aNode {
  [nodeList addFirst: aNode] ;
  return self ;
}

-removeNode: which {
  [nodeList remove: which] ;
  [which drop] ;
  return self ;
}

-addLinkFrom: this To: that {
  id aLink ;

  aLink = [[[[DiGraphLink createBegin: [self getZone]] 
                          setCanvas: canvas]
                          setFrom: this To: that] 
                          createEnd] ;

  [this addTo: aLink] ;
  [that addFrom: aLink] ;

  return self ;
}

-removeLink: aLink {
  [aLink drop] ;
  return self ;
}

-redistribute {
  int i, n, h, w, r, x, y, bx, by ;
  double phase ;
  id obj ;

  if(canvas){

    h = [canvas getHeight] ;
    w = [canvas getWidth] ;

    r = ( (h > w) ? w : h ) ;
    r /= 3 ;

    bx = w / 2 ;
    by = h / 2 ;

    n = [nodeList getCount] ;

    for(i = 0 ; i < n ; i++){
      phase = 6.2831853 * ((double) i) / ((double) n) ;
      x = bx + ((int) (((double) r) * cos(phase))) ;
      y = by + ((int) (((double) r) * sin(phase))) ;
      obj = [[nodeList atOffset: i] getNodeItem] ; 
      [obj initiateMoveX: x - [obj getX] Y: y - [obj getY]] ;
    }

  }

  return self ;
}

-update {
  if(canvas)
    [globalTkInterp eval: "update idletasks"] ;
  return self ;
}

@end
