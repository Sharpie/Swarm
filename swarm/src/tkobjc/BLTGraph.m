// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// interface to BLT Graphs. Legends, etc need to be handled more
// gracefully. Bindings to mouse (see BLT demos), autoscrolling, etc
// should be added.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tclObjc.h>
#import <Tk.h>
#import <tkobjc/BLTGraph.h>
#import <collections.h>

@implementation BLTGraph

-createEnd
{
  [super createEnd];
  
  [globalTkInterp eval: "graph %s;", widgetName];
  [self setWidth: 400 Height: 247];  // golden ratio
  // lots of features!
  [globalTkInterp eval: "Blt_ZoomStack %s; Blt_Crosshairs %s; Blt_ActiveLegend %s; Blt_ClosestPoint %s",
		  widgetName, widgetName, widgetName, widgetName];
  
  elementList = [List create: [self getZone]];
  
  return self;
}

- setRangesXMin: (double)minx 
            Max: (double)maxx
           YMin: (double)miny
            Max: (double)maxy 
{
  [globalTkInterp 
    eval:
      "%s xaxis configure -min %f -max %f; %s yaxis configure -min %f -max %f",
    widgetName, minx, maxx, widgetName, miny, maxy];
  return self;
}


- setScaleModeX: (int)xs Y: (int)ys
{
  [globalTkInterp
    eval:
      "%s xaxis configure -loose %d; %s yaxis configure -loose %d",
    widgetName, xs, widgetName, ys];
  return self;
}

- (GraphElement *)createElement
{
  GraphElement *newElement;
  
  newElement = [GraphElement createOwnerGraph: self];
  [elementList addLast: newElement];
  
  return newElement;
}

- destroyElement: (GraphElement *)g
{
  [elementList remove: g];
  [g drop];
  return self;
}

- title: (const char *)t
{
  [globalTkInterp eval: "%s configure -title \"%s\";", widgetName, t];
  [self setWindowTitle: t];
  return self;
}

- axisLabelsX: (const char *)xl Y: (const char *)yl
{
  [globalTkInterp
    eval:
      "%s xaxis configure -title \"%s\"; %s yaxis configure -title \"%s\";",
    widgetName, xl, widgetName, yl];
  return self;
}

- pack
{
  [globalTkInterp eval: "pack %s -fill both -expand true;", widgetName];
  return self;
}

// first destroy all the elements, then ourselves.
- (void) drop
{
  while ([elementList getCount] > 0)
    [self destroyElement: [elementList getFirst]];
  
  [globalTkInterp eval: "destroy %s", [parent getWidgetName]]; 
  [super drop];
}

@end


@implementation GraphElement

- setOwnerGraph: (BLTGraph *)og
{
  ownerGraph = og;
  return self;
}

// create ourselves and two vectors.
- createEnd
{
  if (ownerGraph == nil)
    [InvalidCombination raiseEvent: "This element has no owner graph!\n"];
  name = strdup (tclObjc_objectToName(self));
  xData = [BLTVector create: [self getZone]];
  yData = [BLTVector create: [self getZone]];

  if ([globalTkInterp newBLTp])
    {
#if 0
      // Create a pen for a small, hollow circle.
      [globalTkInterp
        eval: "%s pen create %s_line -symbol circle -outlinewidth 1 -fill \"\" -pixels 0.05i",
        [ownerGraph getWidgetName],
        [self getName]];
#else
      // Create a pen for a wider line.
      [globalTkInterp
        eval: "%s pen create %s_line -symbol none -linewidth 3",
        [ownerGraph getWidgetName],
        [self getName]];
#endif
      
      // When inactive, use no symbol.  When active, use the new pen. 
      [globalTkInterp
        eval: "%s element create %s -xdata %s -ydata %s -symbol none -activepen %s_line",
        [ownerGraph getWidgetName],
        [self getName],
        [xData getName],
        [yData getName],
        [self getName]];
    }
  else
    // If we are using old BLT, only change line width.
    [globalTkInterp
      eval: "%s element create %s -xdata %s -ydata %s -symbol none -activelinewidth 3",
      [ownerGraph getWidgetName],
      [self getName],
      [xData getName],
      [yData getName],
      [self getName]];

  return self;
}

+ createOwnerGraph: (BLTGraph *)og
{
  return [[[self createBegin: [og getZone]] setOwnerGraph: og] createEnd];
}

- (void)drop
{
  [globalTkInterp eval: "%s element delete %s",
		  [ownerGraph getWidgetName],
                  [self getName]];
  [xData drop];
  [yData drop];
  [super drop];
}

- (const char *)getName
{
  return name;
}

- (BLTVector *)getXData
{
  return xData;
}

- (BLTVector *)getYData
{
  return yData;
}

- addX: (double)x Y: (double)y
{
  [xData append: x];
  [yData append: y];
  return self;
}

- resetData
{
  [xData resetData];
  [yData resetData];
  return self;
}

- setLabel: (const char *)label
{
  [globalTkInterp eval: "%s element configure %s -label \"%s\"",
                  [ownerGraph getWidgetName],
                  name,
                  label];
  return self;
}

- setColor: (const char *)color
{
  if ([globalTkInterp newBLTp])
    {
      [globalTkInterp eval: "%s element configure %s -color %s",
                      [ownerGraph getWidgetName],
                      name,
                      color];
      [globalTkInterp
        eval: "%s pen configure %s_line -color %s -outline %s",
        [ownerGraph getWidgetName],
        [self getName],
        color, color];
    }
  else
    [globalTkInterp
      eval: "%s element configure %s -color %s -activecolor %s",
      [ownerGraph getWidgetName],
      name,
      color, color];
  
  return self;
}

- setWidth: (unsigned)w
{
  [globalTkInterp eval: "%s element configure %s -linewidth %u",
		  [ownerGraph getWidgetName], name, w];
  return self;
}

- setSymbol: (const char *)s
{
  [globalTkInterp eval: "%s element configure %s -symbol %s",
		  [ownerGraph getWidgetName],
                  name,
                  s];
  return self;
}

// set the dash style - 0 means solid.
- setDashes: (int) d
{
  [globalTkInterp eval: "%s element configure %s -dashes %d",
		  [ownerGraph getWidgetName],
                  name,
                  d];
  return self;
}

@end


// replace this stuff with the BLT C API - should be much faster.
// also optimize append so it doesn't regrow the vector for every single
// value - regrow in chunks.
@implementation BLTVector

-createEnd
{
  name = strdup (tclObjc_objectToName (self));
  [globalTkInterp eval: "vector %s", name];
  return self;
}

- (const char *)getName
{
  return name;
}

- (unsigned)getLength
{
  [globalTkInterp eval: "%s length", name];
  return atoi ([globalTkInterp result]);
}

- setLength: (unsigned)n
{
  [globalTkInterp eval: "%s length %u", name, n];
  return self;
}

- append: (double)v
{
  [globalTkInterp eval: "%s append %g", name, v];
  return self;
}

// vector ranges - ":" is like the range "5:7", but it assumes you
// mean from min to max.
- resetData
{
  [globalTkInterp eval: "%s delete :", name];
  return self;
}

- delete: (int)n
{
  [globalTkInterp eval: "%s delete %d", name, n];
  return self;
}

- (void)drop
{
  [globalTkInterp eval: "unset %s", name];
}

@end
