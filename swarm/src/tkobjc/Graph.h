// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

// Objective C interface to BltGraph, for use with libtclobjc

#import <tkobjc/ArchivedGeometryWidget.h>
#import <gui.h>

@class GraphElement;
@class BLTVector;

@interface Graph: ArchivedGeometryWidget <Graph>
{
  id elementList;
}

- (id <GraphElement>)createElement;		   // create dataset to draw
- (void)destroyElement: (id <GraphElement>)g;      // remove element, free it
- setTitle: (const char *)title;                   // title the graph
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;   // change labels here
- (void)setScaleModeX: (BOOL)xs Y: (BOOL)ys;              // 0 smooth, 1 jump
// bltgraph will autoscale, but you can also fix scale.

- (void)setRangesXMin: (double)minX
                  Max: (double)maxX
                 YMin: (double)minY
                  Max: (double)maxY;

- (void)setRangesXMin: (double)minX Max: (double)maxX;
- (void)setRangesYMin: (double)minY Max: (double)maxY;
- (void)drop;
@end

// object to describe one element (dataset) in a graph.
@interface GraphElement: CreateDrop <GraphElement>
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

- (void)addX: (double)x Y: (double)y;
- (void)resetData;

// user configurable options.
- (void)setLabel: (const char *)label;
- (void)setColor: (const char *)color;
- setWidth: (unsigned)w;
// line square circle diamond plus cross splus scross
- (void)setSymbol: (const char *)s; 
// symbol size in pixels
- (void)setSymbolSize: (unsigned)size;
// only if we're a line.
- (void)setDashes: (int)i;
@end

// wrapper for BLT 2.x vectors, most important functions..
@interface BLTVector : CreateDrop
{
  const char *name;
}

- createEnd;
- (const char *)getName;
- (unsigned)getLength;

- (void)setLength: (unsigned)n;
- (void)append: (double)v;
- (void)resetData;
- (void)delete: (int)n;
@end
