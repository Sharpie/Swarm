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
