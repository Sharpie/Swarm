// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// quickie interface to Blt_Graph. Elements, legends, etc need to be handled
// more gracefully. Bindings to mouse (seem BLT demos), autoscrolling,
// etc should be added.

#import <tkobjc/global.h>
#import <tkobjc/Histogram.h>
#include <misc.h> // strdup

@implementation Histogram

PHASE(Creating)

+ createBegin: aZone
{
  Histogram *histo;

  histo = [super createBegin: aZone];
  histo->numBins = -1;

  return histo;
}

// This method can only be called once!
-setNumBins: (unsigned)n 
{
  int i;

  numBins = n;
  elements = [[self getZone] alloc: (sizeof (*elements) * n)];

  return self;
}

- createEnd
{
  int i;

  [super createEnd];

  if (numBins < 1)
    [InvalidCombination raiseEvent:
    "Histogram: creation error: number of bins not specified\n"];

  // create the graph with one element
  [globalTkInterp eval: "barchart %s;", widgetName];
  [self setWidth: 400 Height: 247];		  // golden ratio

  // configure the elements
  for (i = 0; i < numBins; i++)
    {
      char strBuffer[256];

      sprintf (strBuffer, "%d", i);
      elements[i] = strdup (strBuffer);
      [globalTkInterp
        eval: "%s element create %s; %s element configure %s -relief flat",
          widgetName, strBuffer, widgetName, elements[i]];
    }

  return self;
}

PHASE(Using)

- setLabels: (const char * const *)l
{
  int i;

  if (l == NULL) return self;	// nothing to be done

  if (numBins < 1)
    [InvalidCombination raiseEvent:
    "Histogram: cannot set labels -- number of bins not set\n"];

  for (i=0; i<numBins; i++)
    if (l && l[i])
      [globalTkInterp eval: "%s element configure %s -label \"%s\"",
        widgetName, elements[i], l[i]];

  return self;
}

- setColors: (const char * const *)c
{
  int i;

  if (c == NULL) return self;	// nothing to be done

  if (numBins < 1)
    [InvalidCombination raiseEvent:
    "Histogram: cannot set colors -- number of bins not set\n"];

  for (i=0; i<numBins; i++)
    if (c && c[i])
      [globalTkInterp eval: "%s element configure %s -foreground \"%s\"",
        widgetName, elements[i], c[i]];

  // Note: caller needs to supply enough colors.
  // If not, excess bars remain colored blue.

  return self;
}

- drawHistogramWithDouble: (double *) points
{
  int i;

  for (i = 0; i < numBins; i++)
    [globalTkInterp eval: "%s element configure %s -data { %d %f }",
		    widgetName, elements[i], i, points[i]];
  return self;
}

// ick. How to do two data formats right?
- drawHistogramWithInt: (int *) points
{
  int i;

  for (i = 0; i < numBins; i++)
    [globalTkInterp eval: "%s element configure %s -data { %d %d }",
		    widgetName, elements[i], i, points[i]];
  return self;
}

- drawHistogramWithInt: (int *) points atLocations: (double *) locations
{
  int i;

  for (i = 0; i < numBins; i++)
    [globalTkInterp eval: "%s element configure %s -data { %g %d }",
		    widgetName, elements[i], locations[i], points[i]];
  return self;
}

- drawHistogramWithDouble: (double *) points atLocations: (double *) locations
{
  int i;

  for (i = 0; i < numBins; i++)
    [globalTkInterp eval: "%s element configure %s -data { %g %g }",
		    widgetName, elements[i], locations[i], points[i]];
  return self;
}

// this code is in common with BLTGraph
- setTitle: (const char *)t
{
  [globalTkInterp eval: "%s configure -title \"%s\";", widgetName, t];
  [self setWindowTitle: t];
  return self;
}

- setAxisLabelsX: (const char *)xl Y: (const char *)yl
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

- setBarWidth: (double)step
{
  [globalTkInterp eval: 
                    "%s configure -barwidth %g",
                  [self getWidgetName],
                  step];
  return self;
}

- setXaxisMin: (double)min max: (double)max step: (double)step precision: (unsigned)precision
{
  [globalTkInterp eval: 
                    "%s xaxis configure -min %g -max %g -stepsize %g -command \"fmtx %d\"",
                  [self getWidgetName],
                  min,
                  max,
                  step,
                  precision];
  return self;
}

- setXaxisMin: (double)min max: (double)max step: (double)step
{
  [self setXaxisMin: min max: max step: step precision: 3];
  return self;
}

- setActiveOutlierText: (int)outliers count: (int)count
{
  [globalTkInterp
    eval: 
      "%s marker configure active_outlier_marker -text \"outliers: %d (%g)\" ",
    [self getWidgetName], 
    outliers, 
    ((double)outliers / ((double)outliers + (double)count))];

  return self;
}

- hideLegend
{
  [globalTkInterp eval: "%s legend configure $hideOption $hideYes",
                  [self getWidgetName]];
  
  return self;
}

- setupActiveItemInfo
{
  [globalTkInterp eval: "active_item_info %s",
                  [self getWidgetName]];

  return self;
}

- setupActiveOutlierMarker
{
  [globalTkInterp
    eval: 
      "%s marker create text -coords { -Inf +Inf } "
    "-name active_outlier_marker "
    "-anchor nw -justify right "
    "-bg {} $hideOption $hideNo",
    [self getWidgetName]];

  return self;
}

- setupZoomStack
{
  [globalTkInterp eval: "Blt_ZoomStack %s",
                  [self getWidgetName]];
  return self;
}

@end

