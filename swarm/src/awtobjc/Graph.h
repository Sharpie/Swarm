// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <jni.h>

#import <awtobjc/ArchivedGeometryWidget.h>

@class GraphElement;
@class GraphVector;

@interface Graph: ArchivedGeometryWidget
{
  id elementList;
  
  jmethodID _setRanges, _setAxisLabels, _setScaleMode, _setElemLen,
    _createElem, _addElem, _dropElem, _setColor, _update;
}

- (GraphElement *)createElement;		  // create a dataset to draw
- destroyElement: (GraphElement *)g;		  // remove element, free it.
- setTitle: (const char *)t;			  // title the graph
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;	  // change labels here.
- setScaleModeX: (int)xs Y: (int)ys;		  // 0 smooth, 1 jump
- setRangesXMin: (double)minX Max: (double)maxX YMin: (double)minY Max: (double)maxY;  // Graph will autoscale, but you can also fix scale.
- setRangesYMin: (double)minY Max: (double)maxY;
- createJavaElement: (const char *)name;
- addElem: (const char *)name X: (double)x Y: (double)y;
- setElem: (const char *)name Len: (int)n;
- dropElem: (const char *)name;
- setColor: (const char *)n Color: (const char *)color;
- update;
@end

// object to describe one element (dataset) in a graph.
@interface GraphElement : CreateDrop
{
  const char *name;
  Graph *ownerGraph;
  GraphVector *xData, *yData;
}

- setOwnerGraph: (Graph *)og;
- createEnd;
+ createOwnerGraph: (Graph *)og;		  // alternate creation.

- (const char *)getName;
- (GraphVector *)getXData;
- (GraphVector *)getYData;

- addX: (double)x Y: (double)y;
- resetData;

// user configurable options.
- setLabel: (const char *)label;
- setColor: (const char *)color;
- setWidth: (unsigned)w;
// line square circle diamond plus cross splus scross
- setSymbol: (const char *)s;
- setDashes: (int) i;				  // only if we're a line.
@end

@interface GraphVector: CreateDrop
{
  const char * name;
  double 	 *val;		// shadow data vector
  unsigned	 max_val;	// maximum possible values in "val" 
  unsigned	 cur_val;	// number of valid values in "val"
  #define GRAPHV_CHUNK	1010
}

- createEnd;
- (const char *)getName;
- (unsigned)getLength;

- setLength: (unsigned)n;
- append: (double)v;
- resetData;
- delete: (int)n;
@end
