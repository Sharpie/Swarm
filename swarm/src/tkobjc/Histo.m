// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// quickie interface to Blt_Graph. Elements, legends, etc need to be handled
// more gracefully. Bindings to mouse (seem BLT demos), autoscrolling,
// etc should be added.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <Tk.h>
#import <tkobjc/Histo.h>

@implementation Histo

-createEnd {
  [super createEnd];

  // create the graph with one element
  [globalTkInterp eval: "barchart %s;", widgetName];
  [self setWidth: 400 Height: 247];		  // golden ratio

  numPoints = 0;
  
  return self;
}

// you can only call this once. Fix it.
-setNumPoints: (int) n Labels: (char **) l Colors: (char **) c{
  int i;

  numPoints = n;

  elements = [[self getZone] alloc: (sizeof(*elements) * n)];
  for (i = 0; i < n; i++) {
    char strBuffer[256];
    sprintf(strBuffer, "%d", i);
    elements[i] = strdup(strBuffer);
    [globalTkInterp eval: "%s element create %s; %s element configure %s -relief flat", widgetName, strBuffer, widgetName, elements[i]];
    if (l && l[i])
      [globalTkInterp eval: "%s element configure %s -label \"%s\"",
		      widgetName, elements[i], l[i]];
    if (c && c[i])
      [globalTkInterp eval: "%s element configure %s -foreground \"%s\"",
		      widgetName, elements[i], c[i]];
  }    

  return self;
}

-drawHistoWithDouble: (double *) points {
  int i;
  for (i = 0; i < numPoints; i++)
    [globalTkInterp eval: "%s element configure %s -data { %d %f }",
		    widgetName, elements[i], i, points[i]];
  return self;
}

// ick. How to do two data formats right?
-drawHistoWithInt: (int *) points {
  int i;
  for (i = 0; i < numPoints; i++)
    [globalTkInterp eval: "%s element configure %s -data { %d %d }",
		    widgetName, elements[i], i, points[i]];
  return self;
}

-drawHistoWithInt: (int *) points atLocations: (double *) locations {
  int i;
  for (i = 0; i < numPoints; i++)
    [globalTkInterp eval: "%s element configure %s -data { %g %d }",
		    widgetName, elements[i], locations[i], points[i]];
  return self;
}

-drawHistoWithDouble: (double *) points atLocations: (double *) locations {
  int i;
  for (i = 0; i < numPoints; i++)
    [globalTkInterp eval: "%s element configure %s -data { %g %g }",
		    widgetName, elements[i], locations[i], points[i]];
  return self;
}

// this code is in common with BLTGraph
-title: (char *) t {
  [globalTkInterp eval: "%s configure -title \"%s\";", widgetName, t];
  [self setWindowTitle: t];
  return self;
}

-axisLabelsX: (char *) xl Y: (char *) yl {
  [globalTkInterp eval: "%s xaxis configure -title \"%s\"; %s yaxis configure -title \"%s\";",
		  widgetName, xl, widgetName, yl];
  return self;
}

-pack {
  [globalTkInterp eval: "pack %s -fill both -expand true;", widgetName];
  return self;
}

@end

