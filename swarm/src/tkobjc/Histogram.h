// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C interface to BltGraph, for use with libtclobjc

#import <tkobjc/ArchivedGeometryWidget.h>
#import <gui.h>

@interface Histogram: ArchivedGeometryWidget <Histogram>
{
  const char **elements;
  unsigned binCount; // should be dynamic
}
+ createBegin: aZone;
- setBinCount: (unsigned)n; // how many bins to use (bars to draw)
- createEnd;

- (void)setLabels: (const char * const *)l count: (unsigned)labelCount;
- (void)setColors: (const char * const *)c count: (unsigned)colorCount;

- (void)drawHistogramWithDouble: (double *)points; // data format hack
- (void)drawHistogramWithInt: (int *)points;

// This is used by EZBin to avoid the usual integer tagging of elements...
- (void)drawHistogramWithInt: (int *)points atLocations: (double *)locations;
- (void)drawHistogramWithDouble: (double *)points atLocations: (double *)locations;

- setTitle: (const char *)t;
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;
- (void)setBarWidth: (double)step;
- (void)setXaxisMin: (double)min max: (double)max step: (double)step;
- (void)setXaxisMin: (double)min max: (double)max step: (double)step precision: (unsigned)precision;
- (void)setActiveOutlierText: (unsigned)outlierCount count: (unsigned)count;

- (void)hideLegend;

- (void)setupActiveItemInfo;
- (void)setupActiveOutlierMarker;
- (void)setupZoomStack;
@end
