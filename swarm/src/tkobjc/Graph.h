// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C interface to BltGraph, for use with libtclobjc

#import <tkobjc/ArchivedGeometryWidget.h>
#import <gui.h>

@class GraphElement;
@class BLTVector;

@interface Graph: ArchivedGeometryWidget <_Graph>
{
  id elementList;
}

- (id <GraphElement>)createElement;		   // create dataset to draw
- destroyElement: (id <GraphElement>)g;            // remove element, free it
- setTitle: (const char *)title;                   // title the graph
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;  // change labels here
- setScaleModeX: (BOOL)xs Y: (BOOL)ys;               // 0 smooth, 1 jump
// bltgraph will autoscale, but you can also fix scale.

- setRangesXMin: (double)minX
            Max: (double)maxX
           YMin: (double)minY
            Max: (double)maxY;

- setRangesYMin: (double)minY Max: (double)maxY;
- (void)drop;
@end

// object to describe one element (dataset) in a graph.
@interface GraphElement: CreateDrop <_GraphElement>
{
  const char *name;
  id <Graph> ownerGraph;
  BLTVector *xData, *yData;
}

- setOwnerGraph: (id <Graph>)og;
- createEnd;
+ createOwnerGraph: (id <Graph>)og;  // alternate creation.

- (const char *)getName;
- (BLTVector *)getXData;
- (BLTVector *)getYData;

- addX: (double)x Y: (double)y;
- resetData;

// user configurable options.
- setLabel: (const char *)label;
- setColor: (const char *)color;
- setWidth: (unsigned)w;
// line square circle diamond plus cross splus scross
- setSymbol: (const char *)s; 
// only if we're a line.
- setDashes: (int)i;
@end

// wrapper for BLT 2.x vectors, most important functions..
@interface BLTVector : CreateDrop
{
  const char *name;
}

- createEnd;
- (const char *)getName;
- (unsigned)getLength;

- setLength: (unsigned)n;
- append: (double)v;
- resetData;
- delete: (int)n;
@end
