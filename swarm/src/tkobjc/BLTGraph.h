// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C interface to BltGraph, for use with libtclobjc

#import <tkobjc/Widget.h>

@class GraphElement;
@class BLTVector;

@interface BLTGraph : Widget {
  id elementList;
}

-(GraphElement *) createElement;		  // create a dataset to draw
-destroyElement: (GraphElement *) g;		  // remove element, free it.
-title: (char *) t;				  // title the graph
-axisLabelsX: (char *) xl Y: (char *) yl;	  // change labels here.
-setScaleModeX: (int) xs Y: (int) ys;		  // 0 smooth, 1 jump
-setRangesXMin: (double) minX Max: (double) maxX YMin: (double) minY Max: (double) maxY;        // bltgraph will autoscale, but you can also fix scale.
@end

// object to describe one element (dataset) in a graph.
@interface GraphElement : CreateDrop {
  char * name;
  BLTGraph * ownerGraph;
  BLTVector *xData, *yData;
}

-setOwnerGraph: (BLTGraph *) og;
-createEnd;
+createOwnerGraph: (BLTGraph *) og;		  // alternate creation.

-(char *)getName;
-(BLTVector *) getXData;
-(BLTVector *) getYData;

-addX: (double) x Y: (double) y;
-resetData;

// user configurable options.
-setLabel: (char *) label;
-setColor: (char *) color;
-setWidth: (unsigned) w;
-setSymbol: (char *)s;   // line square circle diamond plus cross splus scross
-setDashes: (int) i;				  // only if we're a line.
@end

// wrapper for BLT 2.x vectors, most important functions..
@interface BLTVector : CreateDrop {
  char * name;
}

-createEnd;
-(char *)getName;
-(unsigned) getLength;

-setLength: (unsigned) n;
-append: (double) v;
-resetData;
-delete: (int) n;
@end
