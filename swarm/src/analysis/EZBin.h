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

#import <Swarm/analysis.h> // EZBin
#import <Swarm/GUIComposite.h>
#import <Swarm/gui.h>

// EZBin object: used to generate histograms.

@interface EZBin: GUIComposite <EZBin>
{
  BOOL graphics;
  id <Histogram> aHisto;

  BOOL fileOutput;
  id anOutFile;

  BOOL monoColorBars;

  const char * const *binColors;
  unsigned binColorCount;

  const char *fileName;
  const char *title;
  const char *xLabel;
  const char *yLabel;

  unsigned *distribution;
  double *locations;
  double *cachedLimits;
  double min, max;
  BOOL clean;
  unsigned binCount;
  unsigned count;
  unsigned outliers;
  id collection;
  SEL probedSelector;

  double minval, maxval, average, average2, std;
  unsigned precision;
}

- setFileName: (const char *)aFileName;
- setTitle: (const char *)aTitle; 
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;

- setGraphics: (BOOL)state;
- setFileOutput: (BOOL)state;
- setMonoColorBars: (BOOL)mcb;

- setColors: (const char * const *)colors count: (unsigned)nc;

- setBinCount: (unsigned)binCount;
- setLowerBound: (double)min;
- setUpperBound: (double)max;
- setCollection: aCollection;
- setProbedSelector: (SEL)aSel;
- (void)setPrecision: (unsigned)precision;

- (void)reset;
- (void)update;
- (void)output;
- (void)outputGraph;
- (void)outputToFile;

- (unsigned *)getDistribution;

- (unsigned)getCount;
- (unsigned)getOutliers;
- (unsigned)getBinCount;
- (unsigned)getBinColorCount;
- (double)getUpperBound;
- (double)getLowerBound;
- (const char *)getFileName;
- (const char *)getTitle;

- (double)getMin;
- (double)getMax;
- (double)getAverage;
- (double)getStdDev;

- (id <Histogram>)getHistogram;
@end
