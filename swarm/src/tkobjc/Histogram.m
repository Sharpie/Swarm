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

// quickie interface to Blt_Graph. Elements, legends, etc need to be handled
// more gracefully. Bindings to mouse (seem BLT demos), autoscrolling,
// etc should be added.

#import <tkobjc/global.h>
#import <tkobjc/Histogram.h>
#import <defobj.h> // STRDUP
#import <defobj/defalloc.h> // getZone

@implementation Histogram

PHASE(Creating)

+ createBegin: aZone
{
  Histogram *histo;

  histo = [super createBegin: aZone];
  histo->binCount = 0;

  return histo;
}

// This method can only be called once!
- setBinCount: (unsigned)n 
{
  binCount = n;
  elements = [getZone (self) alloc: (sizeof (*elements) * n)];

  return self;
}

- createEnd
{
  unsigned i;

  [super createEnd];

  if (binCount < 1)
    raiseEvent (InvalidCombination, 
                "Histogram: creation error: number of bins not specified\n");

  // create the graph with one element
  [globalTkInterp eval: "barchart %s;", widgetName];
  [self setWidth: 400 Height: 247];		  // golden ratio

  // configure the elements
  for (i = 0; i < binCount; i++)
    {
      char strBuffer[256];

      sprintf (strBuffer, "%d", i);
      elements[i] = STRDUP (strBuffer);
      [globalTkInterp
        eval: "%s element create %s; %s element configure %s -relief flat",
          widgetName, strBuffer, widgetName, elements[i]];
    }
  [self updateSize];
  return self;
}

PHASE(Using)

- (void)setLabels: (const char * const *)l count: (unsigned)labelCount
{
  unsigned i;

  if (l == NULL)
    return;

  if (binCount < 1)
    raiseEvent (InvalidCombination,
                "Histogram: cannot set labels -- number of bins not set\n");

  if (l)
    for (i = 0; i < binCount; i++)
      [globalTkInterp eval: "%s element configure %s -label {%s}",
		      widgetName, elements[i], l[i % labelCount]];
}

- (void)setColors: (const char * const *)c count: (unsigned)colorCount
{
  unsigned i;

  if (c == NULL)
    return;

  if (binCount < 1)
    raiseEvent (InvalidCombination,
                "Histogram: cannot set colors -- number of bins not set\n");

  if (c)
    for (i = 0; i < binCount; i++)
      [globalTkInterp eval: "%s element configure %s -foreground {%s}",
                      widgetName, elements[i], c[i % colorCount]];
  
  // Note: caller needs to supply enough colors.
  // If not, excess bars remain colored blue.
}

- (void)drawHistogramWithDouble: (double *)points
{
  unsigned i;

  for (i = 0; i < binCount; i++)
    [globalTkInterp eval: "%s element configure %s -data { %d %f }",
		    widgetName, elements[i], i, points[i]];
}

// ick. How to do two data formats right?
- (void)drawHistogramWithInt: (int *)points
{
  unsigned i;

  for (i = 0; i < binCount; i++)
    [globalTkInterp eval: "%s element configure %s -data { %d %d }",
		    widgetName, elements[i], i, points[i]];
}

- (void)drawHistogramWithInt: (int *)points atLocations: (double *)locations
{
  unsigned i;

  for (i = 0; i < binCount; i++)
    [globalTkInterp eval: "%s element configure %s -data { %g %d }",
		    widgetName, elements[i], locations[i], points[i]];
}

- (void)drawHistogramWithDouble: (double *)points atLocations: (double *)locations
{
  unsigned i;

  for (i = 0; i < binCount; i++)
    [globalTkInterp eval: "%s element configure %s -data { %g %g }",
		    widgetName, elements[i], locations[i], points[i]];
}

// this code is in common with BLTGraph
- setTitle: (const char *)t
{
  [globalTkInterp eval: "%s configure -title {%s};", widgetName, t];
  [self setWindowTitle: t];
  return self;
}

- setAxisLabelsX: (const char *)xl Y: (const char *)yl
{
  [globalTkInterp
    eval:
      "%s xaxis configure -title {%s}; %s yaxis configure -title {%s};",
    widgetName, xl, widgetName, yl];
  return self;
}

- (void)pack
{
  [globalTkInterp eval: "pack %s -fill both -expand true;", widgetName];
}

- (void)setBarWidth: (double)step
{
  [globalTkInterp eval: 
                    "%s configure -barwidth %g",
                  [self getWidgetName],
                  step];
}

- (void)setXaxisMin: (double)min max: (double)max step: (double)step precision: (unsigned)precision
{
  [globalTkInterp eval: 
                    "%s xaxis configure -min %g -max %g -stepsize %g -command {fmtx %d}",
                  [self getWidgetName],
                  min,
                  max,
                  step,
                  precision];
}

- (void)setXaxisMin: (double)min max: (double)max step: (double)step
{
  [self setXaxisMin: min max: max step: step precision: 3];
}

- (void)setActiveOutlierText: (unsigned)outliers count: (unsigned)count
{
  [globalTkInterp
    eval: 
      "%s marker configure active_outlier_marker -text {outliers: %d (%g)} ",
    [self getWidgetName], 
    outliers, 
    ((double)outliers / ((double)outliers + (double)count))];
}

- (void)hideLegend
{
  [globalTkInterp eval: "%s legend configure $hideOption $hideYes",
                  [self getWidgetName]];
}

- (void)setupActiveItemInfo
{
  [globalTkInterp eval: "active_item_info %s",
                  [self getWidgetName]];
}

- (void)setupActiveOutlierMarker
{
  [globalTkInterp
    eval: 
      "%s marker create text -coords { -Inf +Inf } "
    "-name active_outlier_marker "
    "-anchor nw -justify right "
    "-bg {} $hideOption $hideNo",
    [self getWidgetName]];
}

- (void)setupZoomStack
{
  [globalTkInterp eval: "Blt_ZoomStack %s",
                  [self getWidgetName]];
}

@end
