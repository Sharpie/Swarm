// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/GUIComposite.h>
#import <gui.h>

// EZBin object: used to generate histograms.

@interface EZBin: GUIComposite
{
  int graphics;
  id <Histogram> aHisto;

  int fileOutput;
  id anOutFile;

  const char *theTitle;
  const char *xLabel;
  const char *yLabel;

  int *distribution;
  double *locations;
  double *cachedLimits;
  double min, max;
  int clean;
  int binNum;
  int count;
  int outliers;
  id collection;
  SEL probedSelector;

  double minval, maxval, average, average2, std;
}

- setTitle: (const char *)aTitle; 
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;

- setGraphics: (int)state;
- setFileOutput: (int)state;

- setBinNum: (int)theBinNum;
- setLowerBound: (double)theMin;
- setUpperBound: (double)theMax;
- setCollection: (id)aCollection;
- setProbedSelector: (SEL)aSel;

- reset;
- update;
- output;

- (int *)getDistribution;

- (int)getCount;
- (int)getOutliers;
- (int)getBinNum;
- (double)getUpperBound;
- (double) getLowerBound;

- (double)getMin;
- (double)getMax;
- (double)getAverage;
- (double)getStd;
@end
