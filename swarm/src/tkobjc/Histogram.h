// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C interface to BltGraph, for use with libtclobjc

#import <tkobjc/ArchivedGeometryWidget.h>
#import <gui.h>

@interface Histogram: ArchivedGeometryWidget <_Histogram>
{
  const char **elements;
  int numBins;  			// should be dynamic
}
+ createBegin: aZone;
- setNumBins: (unsigned)n;		// how many bins to use (bars to draw)
- createEnd;

- setLabels: (const char * const *)l;	// labels for the bars
- setColors: (const char * const *)c;	// colors for the bars

- drawHistogramWithDouble: (double *) points;	  // data format hack
- drawHistogramWithInt: (int *) points;

// This is used by EZBin to avoid the usual integer tagging of elements...
- drawHistogramWithInt: (int *)points atLocations: (double *)locations;
- drawHistogramWithDouble: (double *)points atLocations: (double *)locations;

- setTitle: (const char *)t;                             // title the graph
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;  // change labels here.
- setBarWidth: (double)step;
- setXaxisMin: (double)min max: (double)max step: (double)step;
- setXaxisMin: (double)min max: (double)max step: (double)step precision: (unsigned)precision;
- setActiveOutlierText: (int)outlierCount count: (int)count;

- hideLegend;

- setupActiveItemInfo;
- setupActiveOutlierMarker;
- setupZoomStack;
@end
