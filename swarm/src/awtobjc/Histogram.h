// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/ArchivedGeometryWidget.h>

@interface Histogram: ArchivedGeometryWidget
{
  const char **elements;
  int numPoints;				  // should be dynamic
  const char **labels;
  const char **colors;

  jmethodID _reset, _addBar, _update;
}

- setNumPoints: (int)n Labels: (char **)l Colors: (char **)c;  // how many points to draw.
- drawHistoWithDouble: (double *)points;	  // data format hack
- drawHistoWithInt: (int *)points;

// This is used by EZBin to avoid the usual integer tagging of elements...
- drawHistoWithInt: (int *)points atLocations: (double *)locations;
- drawHistoWithDouble: (double *)points atLocations: (double *)locations;

- setTitle: (const char *)t;                             // title the graph
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;	 // change labels here.
@end
